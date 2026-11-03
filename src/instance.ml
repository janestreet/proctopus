open! Core
open Async

module Running = struct
  type t =
    | Process of { pid : Pid.t }
    | Interactive of
        { tmux : Tmux.t
        ; pane_pid : Pid.t option
        }
end

module State = struct
  type t =
    { commands : Protocol.Register_commands.Entry.command Command_id.Table.t
    ; running : Running.t Command_id.Table.t
    ; process_manager : Process_manager.t
    ; socket_path : string
    ; log_config : Log_config.t option
    ; mutable connection : Rpc.Connection.t option
    }
end

type t =
  { connection : Rpc.Connection.t
  ; state : State.t
  ; cleanup_finished : unit Deferred.t
  }

let report_log (state : State.t) ~level ?id message =
  match state.connection with
  | Some connection ->
    Protocol.Client_log.dispatch connection { level; id; message }
    |> (ignore : unit Or_error.t -> unit)
  | None -> ()
;;

let report_error state ?id message = report_log state ~level:Error ?id message

let report_group_status (state : State.t) ~id update =
  match state.connection with
  | Some connection ->
    Protocol.Group_status.dispatch connection { id; update }
    |> (ignore : unit Or_error.t -> unit)
  | None -> ()
;;

let spawn_group_processes ~(state : State.t) ~entries ~env_prefix_id ~socket_path =
  let groups =
    List.filter_map entries ~f:(fun entry ->
      match entry with
      | Protocol.Register_commands.Entry.Group { id; bash_code } -> Some (id, bash_code)
      | Command _ -> None)
  in
  Deferred.List.filter_map groups ~how:`Parallel ~f:(fun (id, bash_code) ->
    let child_prefix = Command_id.to_string (Command_id.concat env_prefix_id id) in
    let full_id = Command_id.concat env_prefix_id id in
    let log_env =
      match state.log_config with
      | None -> []
      | Some log_config -> Log_config.to_env_vars log_config
    in
    let env =
      `Extend
        ([ "PROCTOPUS_PREFIX", child_prefix; "PROCTOPUS_SOCKET", socket_path ] @ log_env)
    in
    let%bind.Deferred process_result =
      Process.create ~env ~prog:"/bin/bash" ~args:[ "-c"; bash_code ] ()
    in
    match process_result with
    | Ok process ->
      (* Stream stdout/stderr via Group_status and report exit status *)
      don't_wait_for
        (let%bind () =
           Deferred.all_unit
             [ Reader.lines (Process.stdout process)
               |> Pipe.iter_without_pushback ~f:(fun line ->
                 report_group_status state ~id:full_id (Output line))
             ; Reader.lines (Process.stderr process)
               |> Pipe.iter_without_pushback ~f:(fun line ->
                 report_group_status state ~id:full_id (Output line))
             ]
         in
         let%bind exit_status = Process.wait process in
         (match exit_status with
          | Ok () -> report_group_status state ~id:full_id Exited_ok
          | Error (`Exit_non_zero code) ->
            report_group_status
              state
              ~id:full_id
              (Exited_error [%string "exited with code %{code#Int}"])
          | Error (`Signal signal) ->
            report_group_status
              state
              ~id:full_id
              (Exited_error [%string "killed by signal %{Signal.to_string signal}"]));
         return ());
      return (Some process)
    | Error err ->
      report_group_status
        state
        ~id:full_id
        (Exited_error [%string "Failed to spawn: %{Error.to_string_hum err}"]);
      return None)
;;

let report_status_update (state : State.t) ~id update =
  match state.connection with
  | Some connection ->
    Protocol.Status_update.dispatch connection { id; update }
    |> (ignore : unit Or_error.t -> unit)
  | None -> ()
;;

let report_interactive_session (state : State.t) ~id tmux =
  let session_id = Tmux.session_id tmux in
  let session_id_sexp = Sexp.to_string_mach (Tmux.Session_id.sexp_of_t session_id) in
  match state.connection with
  | Some connection ->
    let msg : Protocol.Interactive_session.Msg.t = { id; session_id_sexp } in
    Protocol.Interactive_session.dispatch connection msg
    |> (ignore : unit Or_error.t -> unit)
  | None -> ()
;;

let watch_interactive_exit (state : State.t) ~id ~tmux =
  don't_wait_for
    (let%bind () = Tmux.closed tmux in
     Hashtbl.remove state.running id;
     report_status_update state ~id (Exited { code = None; signal = None });
     return ())
;;

(* Handle start_process RPC for interactive commands - creates a new tmux session *)
let handle_start_interactive (state : State.t) ~id ~bash_code =
  let env : Core_unix.env =
    `Override [ "PROCTOPUS_PREFIX", None; "PROCTOPUS_SOCKET", Some state.socket_path ]
  in
  let command = [%string "/bin/bash -c %{Sys.concat_quoted [ bash_code ]}"] in
  match%bind Tmux.create ~don't_wait_for_ui:() ~env ~command () with
  | Ok tmux ->
    report_interactive_session state ~id tmux;
    let%bind pane_pid =
      match%map Tmux.pane_pid tmux with
      | Ok pid -> Some pid
      | Error err ->
        report_log
          state
          ~level:Debug
          ~id
          [%string "Failed to get pane PID: %{Error.to_string_hum err}"];
        None
    in
    Hashtbl.set state.running ~key:id ~data:(Interactive { tmux; pane_pid });
    let pid = Option.value pane_pid ~default:(Core_unix.getpid ()) in
    report_status_update state ~id (Started { pid });
    watch_interactive_exit state ~id ~tmux;
    return (Ok ())
  | Error err ->
    report_log
      state
      ~level:Debug
      ~id
      [%string "Failed to create tmux session: %{Error.to_string_hum err}"];
    report_status_update state ~id (Exited { code = Some 1; signal = None });
    return (Error err)
;;

(* Handle start_process RPC from server *)
let handle_start_process (state : State.t) (query : Protocol.Start_process.Query.t) =
  let { Protocol.Start_process.Query.id; output_fifo } = query in
  match Hashtbl.find state.commands id with
  | None -> return (Or_error.error_s [%sexp "Unknown command", (id : Command_id.t)])
  | Some { bash_code; is_interactive = true; _ } ->
    (* Interactive command - create a new tmux session (output_fifo is unused and
       immediately closed so as not to leak it) *)
    don't_wait_for (Writer.open_file output_fifo >>= Writer.close);
    handle_start_interactive state ~id ~bash_code
  | Some { bash_code; is_interactive = false; _ } ->
    (* Open FIFO for writing - server already started reading, so this won't block *)
    let%bind fifo_writer = Writer.open_file output_fifo in
    (* Unset PROCTOPUS_PREFIX so that this process is not mistaken for a group, and
       explicitly set PROCTOPUS_SOCKET so that a nested proctopus can detect it is running
       inside another instance and suggest using the -g flag instead. *)
    let env : Core_unix.env =
      `Override [ "PROCTOPUS_PREFIX", None; "PROCTOPUS_SOCKET", Some state.socket_path ]
    in
    let%bind result =
      Process_manager.start_process state.process_manager ~env ~bash_code
    in
    (match result with
     | Ok (pid, ~stdout, ~stderr, exited) ->
       let output, log_cleanup =
         Log_config.forward_output state.log_config ~id ~stdout ~stderr
       in
       don't_wait_for
         (let%bind () =
            Writer.transfer fifo_writer output (fun line ->
              if not (Writer.is_closed fifo_writer)
              then Writer.write_line fifo_writer line)
          in
          Writer.close fifo_writer);
       don't_wait_for
         (let%bind exit_status = exited in
          let%bind () = log_cleanup () in
          Hashtbl.remove state.running id;
          let code, signal =
            match exit_status with
            | Ok () -> Some 0, None
            | Error (`Exit_non_zero code) -> Some code, None
            | Error (`Signal signal) -> None, Some (Signal.to_string signal)
          in
          report_status_update state ~id (Exited { code; signal });
          return ());
       Hashtbl.set state.running ~key:id ~data:(Process { pid });
       (* We send the [Started] event back to the channel via a status update rather than
          a response to this RPC because our synchronous call to [dispatch] ensures that
          it gets enqueued before either a [Stats] update or an [Exited] update. *)
       report_status_update state ~id (Started { pid });
       return (Ok ())
     | Error err ->
       report_error state ~id (Error.to_string_hum err);
       report_status_update state ~id (Exited { code = Some 1; signal = None });
       don't_wait_for (Writer.close fifo_writer);
       return (Error err))
;;

(* Handle signal_process RPC from server *)
let handle_signal_process (state : State.t) (query : Protocol.Signal_process.Query.t) =
  let { Protocol.Signal_process.Query.id; signal } = query in
  (match Hashtbl.find state.running id with
   | Some (Interactive { tmux; pane_pid = _ }) ->
     (* Interactive commands: kill closes the tmux session; pause/resume send signals
        directly to the pane PID (not the process group, since tmux owns the group). *)
     (match signal with
      | Term ->
        don't_wait_for
          (let%bind (_ : unit Or_error.t) = Tmux.close tmux in
           return ())
      | Stop | Cont ->
        report_error
          state
          ~id
          "Pause/resume is not supported for interactive commands due to a tmux \
           limitation")
   | Some (Process { pid }) ->
     (match signal with
      | Term -> Process_manager.kill_process state.process_manager pid
      | Stop -> Process_manager.pause_process state.process_manager pid
      | Cont -> Process_manager.resume_process state.process_manager pid)
   | None -> ());
  return (Ok ())
;;

(* Periodically sample running processes and send stats updates to server *)
let start_stats_loop ~(state : State.t) ~connection =
  don't_wait_for
    (Deferred.repeat_until_finished () (fun () ->
       if Rpc.Connection.is_closed connection
       then return (`Finished ())
       else (
         let%bind () =
           Deferred.List.iter
             (Hashtbl.to_alist state.running)
             ~how:`Parallel
             ~f:(fun (id, running) ->
               let pid =
                 match running with
                 | Running.Process { pid } -> Some pid
                 | Running.Interactive { pane_pid; _ } -> pane_pid
               in
               match pid with
               | None -> return ()
               | Some pid ->
                 let%bind stats_opt =
                   Process_manager.sample_process_group state.process_manager pid
                 in
                 (* Check if process is still tracked - it may have exited during sampling *)
                 if Hashtbl.mem state.running id
                 then (
                   match stats_opt with
                   | Some stats ->
                     Protocol.Status_update.dispatch
                       connection
                       { id; update = Stats { stats } }
                     |> (ignore : unit Or_error.t -> unit)
                   | None -> ());
                 return ())
         in
         let%bind () = Clock.after (sec 1.0) in
         return (`Repeat ()))))
;;

let closed t =
  let%bind () = Rpc.Connection.close_finished t.connection in
  t.cleanup_finished
;;

let close t =
  let%bind () = Rpc.Connection.close t.connection in
  t.cleanup_finished
;;

(* Connect to server and register commands *)
let create ~socket_path ~(entries : Protocol.Register_commands.Entry.t list) ~log_config =
  let prefix =
    Sys.getenv "PROCTOPUS_PREFIX" |> Option.value_map ~default:[] ~f:Command_id.of_string
  in
  (* Environment takes precedence: when nested, the parent's config wins over CLI args *)
  let log_config =
    match Log_config.of_env () with
    | Some _ as env_config -> env_config
    | None -> log_config
  in
  let commands =
    List.filter_map entries ~f:(fun entry ->
      match entry with
      | Protocol.Register_commands.Entry.Command cmd ->
        Some (Command_id.concat prefix cmd.id, cmd)
      | Group _ -> None)
    |> Command_id.Table.of_alist_exn
  in
  let running = Command_id.Table.create () in
  let process_manager = Process_manager.create () in
  let state : State.t =
    { commands; running; process_manager; socket_path; log_config; connection = None }
  in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        (Protocol.Start_process.implement (fun state query ->
           handle_start_process state query)
         @ Protocol.Signal_process.implement (fun state query ->
           handle_signal_process state query))
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  match%bind Sys.file_exists socket_path with
  | `No | `Unknown ->
    return
      (Or_error.error_string
         [%string "Socket not found: %{socket_path}. Is proctopus running?"])
  | `Yes ->
    (match%bind
       Monitor.try_with_or_error (fun () ->
         let%bind _socket, reader, writer =
           Tcp.connect (Tcp.Where_to_connect.of_file socket_path)
         in
         let%bind connection =
           Rpc.Connection.create
             reader
             writer
             ~implementations
             ~connection_state:(fun _ -> state)
         in
         match connection with
         | Error exn ->
           return (Or_error.error_s [%sexp "RPC connection failed", (exn : Exn.t)])
         | Ok connection ->
           state.connection <- Some connection;
           (* Clean up process manager and tmux sessions when connection closes *)
           let cleanup_finished =
             let%bind () = Rpc.Connection.close_finished connection in
             let%bind () =
               Deferred.List.iter
                 (Hashtbl.data state.running)
                 ~how:`Parallel
                 ~f:(fun running ->
                   match running with
                   | Running.Interactive { tmux; _ } ->
                     let%bind (_ : unit Or_error.t) = Tmux.close tmux in
                     return ()
                   | Running.Process _ -> return ())
             in
             Process_manager.close state.process_manager
           in
           let t = { connection; state; cleanup_finished } in
           let query : Protocol.Register_commands.Query.t = { entries; prefix } in
           (match%bind Protocol.Register_commands.dispatch connection query with
            | Error err ->
              let%bind () = Rpc.Connection.close connection in
              return (Or_error.error_s [%sexp "RPC dispatch failed", (err : Error.t)])
            | Ok (Error err) ->
              let%bind () = Rpc.Connection.close connection in
              return (Error err)
            | Ok (Ok ()) ->
              start_stats_loop ~state ~connection;
              (* Spawn group processes - they will connect back as nested instances *)
              don't_wait_for
                (let%bind (_ : Process.t list) =
                   spawn_group_processes
                     ~state
                     ~entries
                     ~env_prefix_id:prefix
                     ~socket_path
                 in
                 return ());
              return (Ok t)))
     with
     | Ok result -> return result
     | Error err ->
       return
         (Or_error.error_string
            [%string "Failed to connect to proctopus: %{Error.to_string_hum err}"]))
;;

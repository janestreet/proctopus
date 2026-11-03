open! Core
open Async

module Command_update = struct
  type t =
    | Status of Protocol.Status_update.Update.t
    | Output of string list
    | Session of Tmux.Session_id.t
end

module Entry = struct
  type t =
    | Command of
        { entry : Protocol.Register_commands.Entry.command
        ; updates : Command_update.t Pipe.Reader.t
        }
    | Group of
        { entry : Protocol.Register_commands.Entry.group
        ; updates : Protocol.Group_status.Update.t Pipe.Reader.t
        }
end

(** Per-command state tracked by the channel. Created when a command is registered and
    persists for the lifetime of the channel. *)
module Command_state = struct
  type t =
    { connection : Rpc.Connection.t
    ; writer : Command_update.t Pipe.Writer.t
    ; mutable process_output_finished : unit Deferred.t option
    (** Resolves when the current run's output FIFO is fully drained. Reset on each new
        process start. *)
    }
end

(** Per-group state tracked by the channel. *)
module Group_state = struct
  type t = { writer : Protocol.Group_status.Update.t Pipe.Writer.t }
end

type t =
  { server : (Socket.Address.Unix.t, string) Tcp.Server.t Ivar.t
  ; socket_path : string
  ; entries_ready : (prefix:Command_id.t * entries:Entry.t list) Pipe.Reader.t
  ; entries_ready_write : (prefix:Command_id.t * entries:Entry.t list) Pipe.Writer.t
  ; logs : Protocol.Client_log.Msg.t Pipe.Reader.t
  ; logs_write : Protocol.Client_log.Msg.t Pipe.Writer.t
  ; commands : Command_state.t Command_id.Table.t
  ; groups : Group_state.t Command_id.Table.t
  }

let socket_path t = t.socket_path
let entries_ready t = t.entries_ready
let logs t = t.logs

let debug t message =
  Pipe.write_without_pushback_if_open
    t.logs_write
    { Protocol.Client_log.Msg.level = Debug; id = None; message }
;;

let proctopus_runtime_dir () =
  let xdg_runtime_dir =
    match Sys.getenv "XDG_RUNTIME_DIR" with
    | Some dir -> dir
    | None -> [%string "/run/user/%{Core_unix.getuid ()#Int}"]
  in
  let dir = xdg_runtime_dir ^/ "proctopus" in
  Core_unix.mkdir_p dir;
  dir
;;

let create_socket_path () =
  let pid = Core_unix.getpid () |> Pid.to_int in
  proctopus_runtime_dir () ^/ sprintf "%d.sock" pid
;;

let register_command
  t
  ~connection
  ~prefix
  (cmd : Protocol.Register_commands.Entry.command)
  =
  let full_id = Command_id.concat prefix cmd.id in
  let reader, writer = Pipe.create () in
  Hashtbl.set
    t.commands
    ~key:full_id
    ~data:{ Command_state.connection; writer; process_output_finished = None };
  Entry.Command { entry = cmd; updates = reader }
;;

let register_group t ~prefix (grp : Protocol.Register_commands.Entry.group) =
  let full_id = Command_id.concat prefix grp.id in
  let reader, writer = Pipe.create () in
  Hashtbl.set t.groups ~key:full_id ~data:{ Group_state.writer };
  Entry.Group { entry = grp; updates = reader }
;;

(* Create a FIFO in the proctopus runtime directory and return its path *)
let create_output_fifo () : Filename.t =
  let fifo =
    proctopus_runtime_dir ()
    ^/ [%string
         "output-%{Pid.to_int (Core_unix.getpid ())#Int}-%{Random.int 1000000#Int}.fifo"]
  in
  Core_unix.mkfifo fifo ~perm:0o600;
  fifo
;;

(* Dispatch start_process to the client that owns the given id. The server creates a FIFO
   for output streaming; the client writes to it and the server reads from it. All updates
   (started, output, stats, exit) arrive through the per-command pipe. *)
let dispatch_start_process t ~id =
  match Hashtbl.find t.commands id with
  | None ->
    return (Or_error.error_s [%sexp "No command registered for", (id : Command_id.t)])
  | Some command ->
    let output_fifo = create_output_fifo () in
    (* Open the FIFO with O_NONBLOCK so it returns immediately without consuming a
       thread-pool thread. This avoids exhausting the Async thread pool when many
       processes start concurrently (each blocking [Reader.open_file] would otherwise hold
       a thread until the write side opens). *)
    let reader =
      let fd =
        let raw_fd = Core_unix.openfile output_fifo ~mode:[ O_RDONLY; O_NONBLOCK ] in
        Fd.create Fifo raw_fd (Info.of_string output_fifo)
      in
      Reader.create fd
    in
    let query : Protocol.Start_process.Query.t = { id; output_fifo } in
    let%map.Deferred result =
      match%bind
        Protocol.Start_process.dispatch command.connection query >>| Or_error.join
      with
      | Error err ->
        don't_wait_for (Reader.close reader);
        let error_text = [%string "Failed to start: %{Error.to_string_hum err}"] in
        let lines = String.split_lines error_text in
        Pipe.write_without_pushback_if_open command.writer (Command_update.Output lines);
        return (Error err)
      | Ok () ->
        let output_finished =
          Pipe.transfer' (Reader.lines reader) command.writer ~f:(fun lines ->
            return (Queue.singleton (Command_update.Output (Queue.to_list lines))))
        in
        command.process_output_finished <- Some output_finished;
        return (Ok ())
    in
    (* Once we received a result from the dispatch, we can clean up the temporary path. *)
    Core_unix.unlink output_fifo;
    result
;;

(* Dispatch signal_process to the client that owns the given id *)
let dispatch_signal_process t ~id ~signal =
  match Hashtbl.find t.commands id with
  | None ->
    return (Or_error.error_s [%sexp "No command registered for", (id : Command_id.t)])
  | Some command ->
    let query : Protocol.Signal_process.Query.t = { id; signal } in
    (match%bind Protocol.Signal_process.dispatch command.connection query with
     | Error err ->
       return (Or_error.error_s [%sexp "RPC dispatch failed", (err : Error.t)])
     | Ok response -> return response)
;;

let close t =
  let%bind () =
    match%bind Sys.file_exists t.socket_path with
    | `Yes -> Unix.unlink t.socket_path
    | `No | `Unknown -> return ()
  and () =
    let%bind server = Ivar.read t.server in
    Tcp.Server.close server ~close_existing_connections:true
  in
  return ()
;;

let on_register_commands t ~connection (query : Protocol.Register_commands.Query.t) =
  let { Protocol.Register_commands.Query.entries; prefix } = query in
  let channel_entries =
    List.map entries ~f:(fun entry ->
      match entry with
      | Protocol.Register_commands.Entry.Command cmd ->
        register_command t ~connection ~prefix cmd
      | Protocol.Register_commands.Entry.Group grp -> register_group t ~prefix grp)
  in
  Pipe.write_without_pushback_if_open
    t.entries_ready_write
    (~prefix, ~entries:channel_entries);
  return (Ok ())
;;

let on_status_update t (msg : Protocol.Status_update.Msg.t) =
  match Hashtbl.find t.commands msg.id with
  | Some command ->
    (match msg.update with
     | Exited _ ->
       (* Wait for the output FIFO to drain before surfacing the exit event so that
          trailing output is not reordered with respect to the separator. Because all
          updates for a given command go through this single pipe, ordering is guaranteed. *)
       don't_wait_for
         (let%bind () =
            match command.process_output_finished with
            | Some output_finished -> output_finished
            | None -> return ()
          in
          Pipe.write_if_open command.writer (Command_update.Status msg.update))
     | Started _ | Stats _ ->
       Pipe.write_without_pushback_if_open
         command.writer
         (Command_update.Status msg.update))
  | None ->
    debug
      t
      [%string
        "Received status update for unknown command %{Command_id.to_string msg.id}"]
;;

let on_client_log t (msg : Protocol.Client_log.Msg.t) =
  Pipe.write_without_pushback_if_open t.logs_write msg
;;

let on_group_status t (msg : Protocol.Group_status.Msg.t) =
  match Hashtbl.find t.groups msg.id with
  | None ->
    debug
      t
      [%string "Received group status for unknown group %{Command_id.to_string msg.id}"]
  | Some group -> Pipe.write_without_pushback_if_open group.writer msg.update
;;

let on_interactive_session t (msg : Protocol.Interactive_session.Msg.t) =
  match Hashtbl.find t.commands msg.id with
  | Some command ->
    let session_id = Tmux.Session_id.t_of_sexp (Sexp.of_string msg.session_id_sexp) in
    Pipe.write_without_pushback_if_open command.writer (Command_update.Session session_id)
  | None ->
    debug
      t
      [%string
        "Received interactive session for unknown command %{Command_id.to_string msg.id}"]
;;

let start () =
  let socket_path = create_socket_path () in
  let entries_ready, entries_ready_write = Pipe.create () in
  let logs, logs_write = Pipe.create () in
  let t =
    { server = Ivar.create ()
    ; socket_path
    ; entries_ready
    ; entries_ready_write
    ; logs
    ; logs_write
    ; commands = Command_id.Table.create ()
    ; groups = Command_id.Table.create ()
    }
  in
  let%bind () =
    match%bind Sys.file_exists socket_path with
    | `Yes -> Unix.unlink socket_path
    | `No | `Unknown -> return ()
  in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        (Protocol.Register_commands.implement (fun connection query ->
           on_register_commands t ~connection query)
         @ Protocol.Status_update.implement (fun _connection msg ->
           on_status_update t msg)
         @ Protocol.Client_log.implement (fun _connection msg -> on_client_log t msg)
         @ Protocol.Group_status.implement (fun _connection msg -> on_group_status t msg)
         @ Protocol.Interactive_session.implement (fun _connection msg ->
           on_interactive_session t msg))
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%bind server =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_file socket_path)
      (fun _addr reader writer ->
         Rpc.Connection.server_with_close
           reader
           writer
           ~implementations
           ~connection_state:(fun connection -> connection)
           ~on_handshake_error:`Raise)
  in
  Ivar.fill_exn t.server server;
  return t
;;

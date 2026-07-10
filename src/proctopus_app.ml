open! Core
open Async
open Bonsai_term

(* Build tree from channel entries. Groups that haven't registered their children yet
   become Initializing_group leaves. *)
(* Collapse branches in a tree whose section path matches one of the given paths. The
   [prefix] is stripped from each collapse path so matching is relative to this subtree.
   Section paths are accumulated by traversing Section-valued branch nodes, skipping the
   root. *)
let collapse_matching_sections tree ~prefix ~collapse_paths =
  if List.is_empty collapse_paths
  then tree
  else (
    (* Strip [prefix] from each collapse path to get paths relative to this subtree *)
    let relative_paths =
      List.filter_map collapse_paths ~f:(fun collapse_path ->
        let rec strip p pref =
          match p, pref with
          | rest, [] -> Some rest
          | [], _ :: _ -> None
          | ph :: pt, xh :: xt -> if String.equal ph xh then strip pt xt else None
        in
        strip collapse_path prefix)
    in
    if List.is_empty relative_paths
    then tree
    else (
      let rec go ~section_path node =
        match node with
        | Navigable_tree.Node.Leaf _ -> node
        | Navigable_tree.Node.Branch { value; children; expanded } ->
          let current_section_path =
            match (value : Tree_item.t) with
            | Section name -> section_path @ [ name ]
            | Command _ | Initializing_group _ -> section_path
          in
          let children = List.map children ~f:(go ~section_path:current_section_path) in
          let should_collapse =
            List.exists relative_paths ~f:(fun rpath ->
              [%equal: string list] current_section_path rpath)
          in
          let expanded = if should_collapse then false else expanded in
          Navigable_tree.Node.Branch { value; children; expanded }
      in
      (* Check if the root itself should be collapsed (when a relative path is []) *)
      match tree with
      | Navigable_tree.Node.Leaf _ -> tree
      | Navigable_tree.Node.Branch { value; children; expanded } ->
        let should_collapse_root = List.exists relative_paths ~f:List.is_empty in
        let expanded = if should_collapse_root then false else expanded in
        let children = List.map children ~f:(go ~section_path:[]) in
        Navigable_tree.Node.Branch { value; children; expanded }))
;;

let build_tree_from_entries entries ~prefix ~collapse_paths =
  let root = Command_id.display_name prefix in
  let items =
    List.map entries ~f:(fun entry ->
      match (entry : Channel.Entry.t) with
      | Command { entry = { id = path; bash_code; run_on_start; is_interactive }; _ } ->
        let id = Command_id.concat prefix path in
        path, Tree_item.Command { id; bash_code; run_on_start; is_interactive }
      | Group { entry = { id = path; bash_code }; _ } ->
        let id = Command_id.concat prefix path in
        path, Tree_item.Initializing_group { id; bash_code })
  in
  let tree = Commands.of_items ~root items in
  collapse_matching_sections tree ~prefix ~collapse_paths
;;

let run_tui ~title ~channel ~start_fullscreen ~initial_select ~initial_collapse ~flavor =
  Bonsai_term.start_with_exit (fun ~exit ~dimensions graph ->
    let open Bonsai.Let_syntax in
    let debug_messages = Bonsai.Expert.Var.create [ "Starting proctopus..." ] in
    let debug msg =
      Bonsai.Expert.Var.update debug_messages ~f:(fun msgs -> msgs @ [ msg ])
    in
    (* Error state for displaying errors in a red bar *)
    let current_error = Bonsai.Expert.Var.create None in
    let set_error msg = Bonsai.Expert.Var.set current_error (Some msg) in
    let dismiss_error () = Bonsai.Expert.Var.set current_error None in
    (* Capture inject function for use in event handlers *)
    let inject_ref = ref (fun _ -> Effect.Ignore) in
    let log_or_error context result =
      Or_error.iter_error result ~f:(fun err ->
        debug [%string "%{context}: %{Error.to_string_hum err}"])
    in
    (* Track pending restarts: when a process exits and has a pending restart, we
       automatically start it again. This avoids a race where the exit event could arrive
       before the restart handler sets up its ivar. *)
    let pending_restarts : Command_id.Hash_set.t = Command_id.Hash_set.create () in
    let start_process ~id =
      debug [%string "Dispatching Start_process for %{Command_id.to_string id}"];
      let%map.Deferred result = Channel.dispatch_start_process channel ~id in
      log_or_error "Start_process" result
    in
    let handle_event (event : App.App_event.t) =
      Effect.of_deferred_thunk (fun () ->
        match event with
        | Start_process { id } -> start_process ~id
        | Kill_process { id } ->
          debug [%string "Dispatching Kill_process for %{Command_id.to_string id}"];
          let%map.Deferred result =
            Channel.dispatch_signal_process channel ~id ~signal:Term
          in
          log_or_error "Kill_process" result
        | Restart_process { id } ->
          debug [%string "Restart requested for %{Command_id.to_string id}"];
          Hash_set.add pending_restarts id;
          let%map.Deferred result =
            Channel.dispatch_signal_process channel ~id ~signal:Term
          in
          log_or_error "Restart/Kill" result
        | Pause_process { id } ->
          debug [%string "Dispatching Pause_process for %{Command_id.to_string id}"];
          let%map.Deferred result =
            Channel.dispatch_signal_process channel ~id ~signal:Stop
          in
          log_or_error "Pause_process" result
        | Resume_process { id } ->
          debug [%string "Dispatching Resume_process for %{Command_id.to_string id}"];
          let%map.Deferred result =
            Channel.dispatch_signal_process channel ~id ~signal:Cont
          in
          log_or_error "Resume_process" result)
    in
    let%sub ~view, ~handler, ~inject, ~nav_inject, ~nav_model =
      Bonsai_term_color_scheme.set_flavor_within
        (Bonsai.return flavor)
        (fun graph ->
          App.component
            ~title
            ~debug_messages:(Bonsai.Expert.Var.value debug_messages)
            ~debug:(Bonsai.return debug)
            ~current_error:(Bonsai.Expert.Var.value current_error)
            ~dismiss_error:(Bonsai.return dismiss_error)
            ~dimensions
            ~on_event:(Bonsai.return handle_event)
            ~exit:(Bonsai.return exit)
            ~start_fullscreen
            graph)
        graph
    in
    let nav_inject_ref = ref (fun _ -> Effect.Ignore) in
    (* Track whether selection is explicit by observing the model. This ref gets updated
       whenever the model changes, so we can check it from outside the Bonsai computation. *)
    let selection_is_explicit_ref = ref false in
    let () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr inject and nav_inject in
           inject_ref := inject;
           nav_inject_ref := nav_inject;
           Effect.Ignore)
        graph
    in
    let () =
      Bonsai.Edge.on_change
        nav_model
        ~equal:[%equal: Tree_item.t Navigable_tree.Model.t]
        ~trigger:`After_display
        ~callback:
          (Bonsai.return (fun model ->
             selection_is_explicit_ref := Navigable_tree.Model.is_selection_explicit model;
             Effect.Ignore))
        graph
    in
    (let on_exn exn = debug [%string "Effect error: %{Exn.to_string exn}"] in
     (* Handle incoming entries from child instances *)
     don't_wait_for
       (Pipe.iter_without_pushback
          (Channel.entries_ready channel)
          ~f:(fun (~prefix, ~entries) ->
            let collapse_paths = List.map initial_collapse ~f:Command_id.of_string in
            let tree = build_tree_from_entries entries ~prefix ~collapse_paths in
            debug
              [%string
                "Received entries for prefix %{[%sexp (prefix : Command_id.t)]#Sexp} \
                 with %{List.length entries#Int} nodes"];
            let start_effects =
              List.filter_map entries ~f:(function
                | Channel.Entry.Command { entry = { run_on_start = true; id; _ }; _ } ->
                  let id = Command_id.concat prefix id in
                  Some (handle_event (App.App_event.Start_process { id }))
                | _ -> None)
            in
            let update_tree_effect =
              (* Replace the Initializing_group placeholder matching this prefix *)
              !nav_inject_ref
                (Navigable_tree.Action.Replace_leaf
                   (function
                     | Tree_item.Initializing_group { id; _ }
                       when Command_id.equal id prefix -> Some tree
                     | _ -> None))
            in
            (* When starting fullscreen with a single auto command, select it instead of
               the root. Only do this for the initial tree load (empty prefix). *)
            let select_first_child_effect =
              if start_fullscreen && List.is_empty prefix
              then !nav_inject_ref Navigable_tree.Action.Move_down
              else Effect.Ignore
            in
            (* If -select was specified and selection is still implicit (not yet
               explicitly set by user navigation or previous auto-selection), try to
               select the specified path. *)
            let initial_select_effect =
              match initial_select with
              | None -> Effect.Ignore
              | Some _ when !selection_is_explicit_ref -> Effect.Ignore
              | Some path ->
                let target_id = Command_id.of_string path in
                let predicate item =
                  match (item : Tree_item.t) with
                  | Command { id; _ } | Initializing_group { id; _ } ->
                    [%equal: Command_id.t] id target_id
                  | Section _ -> false
                in
                (* Check if any entry matches the target path *)
                let path_exists =
                  List.exists entries ~f:(fun entry ->
                    let id =
                      match entry with
                      | Channel.Entry.Command { entry; _ } -> entry.id
                      | Channel.Entry.Group { entry; _ } -> entry.id
                    in
                    let full_id = Command_id.concat prefix id in
                    [%equal: Command_id.t] full_id target_id)
                in
                if path_exists
                then !nav_inject_ref (Navigable_tree.Action.Select_path predicate)
                else Effect.Ignore
            in
            (* Start consuming per-entry update pipes *)
            List.iter entries ~f:(fun entry ->
              match entry with
              | Channel.Entry.Command { entry; updates } ->
                let id = Command_id.concat prefix entry.id in
                don't_wait_for
                  (Pipe.iter_without_pushback updates ~f:(fun update ->
                     match (update : Channel.Command_update.t) with
                     | Session session_id ->
                       debug
                         [%string
                           "Registered interactive session for %{Command_id.to_string id}"];
                       Effect.Expert.handle
                         ~on_exn
                         (!inject_ref (App.Action.Set_tmux_session (id, session_id)))
                     | Status (Started { pid }) ->
                       debug
                         [%string
                           "Process started: %{Command_id.to_string id} pid=%{pid#Pid}"];
                       Effect.Expert.handle
                         ~on_exn
                         (!inject_ref (App.Action.Start_process (id, pid)))
                     | Status (Stats { stats }) ->
                       Effect.Expert.handle
                         ~on_exn
                         (!inject_ref (App.Action.Update_stats (id, stats)))
                     | Status (Exited { code; signal }) ->
                       debug [%string "Process exited: %{Command_id.to_string id}"];
                       let exit_status =
                         match code, signal with
                         | Some 0, None -> Ok ()
                         | Some code, None -> Error (`Exit_non_zero code)
                         | None, Some signal -> Error (`Signal (Signal.of_string signal))
                         | _ -> Error (`Exit_non_zero 1)
                       in
                       if Hash_set.mem pending_restarts id
                       then (
                         Hash_set.remove pending_restarts id;
                         Effect.Expert.handle
                           ~on_exn
                           (!inject_ref (App.Action.Start_requested id));
                         don't_wait_for (start_process ~id));
                       Effect.Expert.handle
                         ~on_exn
                         (!inject_ref (App.Action.Process_exited (id, exit_status)))
                     | Output lines ->
                       Effect.Expert.handle
                         ~on_exn
                         (!inject_ref (App.Action.Add_output (id, lines)))))
              | Channel.Entry.Group { entry; updates } ->
                let id = Command_id.concat prefix entry.id in
                don't_wait_for
                  (Pipe.iter_without_pushback updates ~f:(fun update ->
                     let action =
                       match (update : Protocol.Group_status.Update.t) with
                       | Output line -> App.Action.Group_output (id, line)
                       | Exited_ok ->
                         App.Action.Group_finished
                           (id, App.Group_status.Failed "exited unexpectedly")
                       | Exited_error error ->
                         App.Action.Group_finished (id, App.Group_status.Failed error)
                     in
                     Effect.Expert.handle ~on_exn (!inject_ref action))));
            Effect.Expert.handle
              ~on_exn
              (Effect.all_parallel_unit
                 (update_tree_effect
                  :: select_first_child_effect
                  :: initial_select_effect
                  :: start_effects))));
     (* Handle log messages from instances - errors display as a red bar, others go to
        debug *)
     don't_wait_for
       (Pipe.iter_without_pushback (Channel.logs channel) ~f:(fun msg ->
          match msg.level with
          | Protocol.Client_log.Level.Error -> set_error msg.message
          | Protocol.Client_log.Level.Debug -> debug msg.message)));
    ~view, ~handler)
;;

(* Parse CLI arguments into protocol entries, preserving CLI position order *)
let parse_entries ~auto_args ~no_auto_args ~groups ~interactive_args ~all_args =
  let parse_command_arg arg =
    match String.lsplit2 arg ~on:':' with
    | Some (label, bash_code) -> ~id:(Command_id.of_string label), ~bash_code
    | None -> ~id:(Command_id.of_string arg), ~bash_code:arg
  in
  List.filter_map all_args ~f:(fun arg ->
    if List.mem auto_args arg ~equal:String.equal
    then (
      let ~id, ~bash_code = parse_command_arg arg in
      Some
        (Protocol.Register_commands.Entry.Command
           { id; bash_code; run_on_start = true; is_interactive = false }))
    else if List.mem no_auto_args arg ~equal:String.equal
    then (
      let ~id, ~bash_code = parse_command_arg arg in
      Some
        (Protocol.Register_commands.Entry.Command
           { id; bash_code; run_on_start = false; is_interactive = false }))
    else if List.mem groups arg ~equal:String.equal
    then (
      let ~id, ~bash_code = parse_command_arg arg in
      Some (Protocol.Register_commands.Entry.Group { id; bash_code }))
    else if List.mem interactive_args arg ~equal:String.equal
    then (
      let ~id, ~bash_code = parse_command_arg arg in
      Some
        (Protocol.Register_commands.Entry.Command
           { id; bash_code; run_on_start = true; is_interactive = true }))
    else None)
;;

let shutdown () =
  (* The default [force] is 10s, but allow extra time in case child processes are slow to
     shut down. *)
  let force = after (sec 30.) in
  Shutdown.shutdown ~force 0
;;

let run
  ~title
  ~initial_select
  ~initial_collapse
  ~flavor
  ~log_config
  (commands : (name:string * command:string * kind:Command_kind.t) list)
  =
  let open Deferred.Let_syntax in
  let entries =
    List.map commands ~f:(fun (~name, ~command, ~kind) ->
      let id = Command_id.of_string name in
      let bash_code = command in
      match kind with
      | `Auto ->
        Protocol.Register_commands.Entry.Command
          { id; bash_code; run_on_start = true; is_interactive = false }
      | `No_auto ->
        Protocol.Register_commands.Entry.Command
          { id; bash_code; run_on_start = false; is_interactive = false }
      | `Group -> Protocol.Register_commands.Entry.Group { id; bash_code }
      | `Interactive ->
        Protocol.Register_commands.Entry.Command
          { id; bash_code; run_on_start = true; is_interactive = true })
  in
  let%bind.Deferred.Or_error () =
    if List.is_empty entries
    then Deferred.Or_error.error_string "No commands or groups specified"
    else Deferred.Or_error.return ()
  in
  let%bind.Deferred.Or_error () =
    match Sys.getenv "PROCTOPUS_PREFIX", Sys.getenv "PROCTOPUS_SOCKET" with
    | None, Some _ ->
      Deferred.Or_error.error_string
        "This proctopus instance is nested inside another proctopus but was not \
         registered as a group. If you want to nest proctopus, use the -g flag to \
         register this command as a group."
    | _ -> Deferred.Or_error.return ()
  in
  (* Start fullscreen if there's exactly one auto command *)
  let start_fullscreen =
    match commands with
    | [ (~name:_, ~command:_, ~kind:`Auto) ] -> true
    | _ -> false
  in
  (* Install signal handlers for graceful shutdown *)
  Signal.handle Signal.terminating ~f:(fun (_ : Signal.t) -> shutdown ());
  (* Start the channel if none exists *)
  let%bind channel_opt, socket_path =
    match Sys.getenv "PROCTOPUS_SOCKET" with
    | Some socket_path -> return (None, socket_path)
    | None ->
      let%map channel = Channel.start () in
      Some channel, Channel.socket_path channel
  in
  (* Register entries and spawn any group processes *)
  let%bind.Deferred.Or_error instance =
    Instance.create ~socket_path ~entries ~log_config
  in
  (* Run TUI if we're the root, then wait for instance to close *)
  let%bind.Deferred.Or_error () =
    match channel_opt with
    | Some channel ->
      run_tui ~title ~channel ~start_fullscreen ~initial_select ~initial_collapse ~flavor
    | None -> Deferred.Or_error.return ()
  in
  let%bind () = Instance.closed instance
  and () =
    match channel_opt with
    | Some channel -> Channel.close channel
    | None -> Deferred.return ()
  in
  Deferred.Or_error.return ()
;;

let entries_to_run_format entries
  : (name:string * command:string * kind:Command_kind.t) list
  =
  List.map entries ~f:(function
    | Protocol.Register_commands.Entry.Command { id; bash_code; is_interactive = true; _ }
      -> ~name:(Command_id.to_string id), ~command:bash_code, ~kind:`Interactive
    | Protocol.Register_commands.Entry.Command { id; bash_code; run_on_start = true; _ }
      -> ~name:(Command_id.to_string id), ~command:bash_code, ~kind:`Auto
    | Protocol.Register_commands.Entry.Command { id; bash_code; run_on_start = false; _ }
      -> ~name:(Command_id.to_string id), ~command:bash_code, ~kind:`No_auto
    | Protocol.Register_commands.Entry.Group { id; bash_code } ->
      ~name:(Command_id.to_string id), ~command:bash_code, ~kind:`Group)
;;

let command =
  Command.async_or_error
    ~summary:
      {|A task runner for developing multi-process apps.

Examples:
  proctopus 'ping:ping localhost' -n 'seq:seq 100'
  proctopus 'servers/web:./start-web.sh' 'servers/api:./start-api.sh' -n 'tests/unit:pytest'
  proctopus -g 'group:./group.sh' 'foo:echo foo' 'bar:echo bar'
  proctopus -i 'editor:nvim' 'build:make'
  proctopus -- my-complex-command -with "quoted args"

Use -group to invoke a script that spawns a nested proctopus instance.
Use -interactive for commands that need an embedded terminal (e.g. nvim, htop).
Use -- to pass all remaining arguments as a single auto command.|}
    (let%map_open.Command auto_args = anon (sequence ("NAME[:CMD]" %: string))
     and no_auto_args =
       flag
         "-no-auto"
         (listed string)
         ~doc:"NAME[:CMD] commands that don't run on startup"
     and groups =
       flag
         "-group"
         (listed string)
         ~doc:"NAME[:CMD] nest commands under group, optionally running CMD first"
     and interactive_args =
       flag
         "-interactive"
         (listed string)
         ~doc:
           "NAME[:CMD] interactive commands embedded via Bonsai_term_tmux (e.g. nvim, \
            htop)"
     and title =
       flag
         "-title"
         (optional_with_default "proctopus" string)
         ~doc:"TITLE title displayed at the top of the tree (default: proctopus)"
     and flavor =
       flag_optional_with_default_doc_sexp
         "-theme"
         (Command.Arg_type.enumerated_sexpable
            ~list_values_in_help:true
            (module Bonsai_term_color_scheme.Flavor_name))
         [%sexp_of: Bonsai_term_color_scheme.Flavor_name.t]
         ~default:(Bonsai_term_color_scheme.Flavor_name.Catppuccin Mocha)
         ~doc:"THEME color scheme"
     and initial_select =
       flag
         "-select"
         (optional string)
         ~doc:"PATH initially select the item at PATH (e.g., foo/bar)"
     and initial_collapse =
       flag
         "-collapse"
         (listed string)
         ~doc:"PATH start with the group at PATH collapsed (repeatable)"
     and log_dir =
       flag
         "-log"
         (optional string)
         ~doc:
           "DIR tee all process output to log files under DIR, mirroring the command \
            tree structure. Ignored when running as a nested instance (the parent's log \
            config is inherited instead)."
     and log_append =
       flag
         "-log-append"
         no_arg
         ~doc:
           " append to log files instead of truncating on process restart. Ignored when \
            running as a nested instance."
     and log_split =
       flag
         "-log-split"
         no_arg
         ~doc:
           " write stdout and stderr to separate .stdout and .stderr files. Ignored when \
            running as a nested instance."
     and escape_args = flag "--" escape ~doc:"CMD run CMD as a single auto command"
     and all_args = args in
     fun () ->
       let commands =
         match escape_args with
         | Some (first_arg :: _ as escaped) ->
           let bash_code = Sys.concat_quoted escaped in
           [ ~name:first_arg, ~command:bash_code, ~kind:`Auto ]
         | Some [] | None ->
           let entries =
             parse_entries ~auto_args ~no_auto_args ~groups ~interactive_args ~all_args
           in
           entries_to_run_format entries
       in
       let flavor = Bonsai_term_color_scheme.Flavor_name.to_flavor flavor in
       let log_config =
         Option.map log_dir ~f:(fun log_dir ->
           { Log_config.log_dir; append = log_append; split = log_split })
       in
       run ~title ~initial_select ~initial_collapse ~flavor ~log_config commands)
;;

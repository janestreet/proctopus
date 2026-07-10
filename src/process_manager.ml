open! Core
open Async

type process_info =
  { stdout : Reader.t
  ; stderr : Reader.t
  }

type t =
  { running_processes : (Pid.t, process_info) Hashtbl.t
  ; prev_cpu_times : (Pid.t, Int64.t * Time_ns.t) Hashtbl.t
  ; stopped_pids : Pid.Hash_set.t
  ; mutable closed : bool
  }

let create () =
  { running_processes = Hashtbl.create (module Pid)
  ; prev_cpu_times = Hashtbl.create (module Pid)
  ; stopped_pids = Pid.Hash_set.create ()
  ; closed = false
  }
;;

let page_size = 4096 (* Standard page size on Linux *)

let state_of_procfs_state (state : Procfs.Process.Stat.State.t)
  : Process_stats.Sample.State.t
  =
  match state with
  | Running -> Running
  | Interruptible_sleep | Uninterruptible_disk_sleep -> Sleeping
  | Stopped | Tracing_stop -> Stopped
  | Zombie -> Zombie
  | Dead | Unknown _ -> Other
;;

let get_thread_ids pid : int list =
  let task_dir = [%string "/proc/%{pid#Pid}/task"] in
  try
    Stdlib.Sys.readdir task_dir
    |> Array.filter_map ~f:(fun entry ->
      let entry = String.strip entry in
      if String.is_empty entry
      then None
      else Option.try_with (fun () -> Int.of_string entry))
    |> Array.to_list
  with
  | _ -> []
;;

(** Compute CPU percent for a single thread, tracking its previous time in
    [prev_cpu_times] keyed by a synthetic [Pid.t] derived from the thread's tid. *)
let cpu_percent_of_thread t ~now ~clk_tck ~pid tid : float =
  let stat_path = [%string "/proc/%{pid#Pid}/task/%{tid#Int}/stat"] in
  try
    let contents = In_channel.read_all stat_path in
    let stat = Procfs.Process.Stat.of_string contents in
    let total_time =
      Int64.( + )
        (Big_int.int64_of_big_int stat.utime)
        (Big_int.int64_of_big_int stat.stime)
    in
    let key = Pid.of_int tid in
    match Hashtbl.find t.prev_cpu_times key with
    | None ->
      Hashtbl.set t.prev_cpu_times ~key ~data:(total_time, now);
      0.0
    | Some (prev_time, prev_timestamp) ->
      Hashtbl.set t.prev_cpu_times ~key ~data:(total_time, now);
      let time_diff = Int64.( - ) total_time prev_time in
      let elapsed = Time_ns.diff now prev_timestamp |> Time_ns.Span.to_sec in
      if Float.( > ) elapsed 0.0
      then (
        let cpu_seconds = Int64.to_float time_diff /. clk_tck in
        Float.min 100.0 (cpu_seconds /. elapsed *. 100.0))
      else 0.0
  with
  | _ -> 0.0
;;

let sample_single_process t ~now ~clk_tck pid : Process_stats.Sample.t option =
  try
    let stat = Procfs.Process.Stat.load pid |> Or_error.ok_exn in
    let thread_ids = get_thread_ids pid in
    let cpu_percent =
      List.sum (module Float) thread_ids ~f:(cpu_percent_of_thread t ~now ~clk_tck ~pid)
    in
    let rss_pages = Big_int.int64_of_big_int stat.rss in
    let memory_rss_bytes = Int64.( * ) rss_pages (Int64.of_int page_size) in
    let memory_virt_bytes = Big_int.int64_of_big_int stat.vsize in
    let num_threads = Big_int.int_of_big_int stat.num_threads in
    let state = state_of_procfs_state stat.state in
    Some
      { Process_stats.Sample.timestamp = now
      ; cpu_percent
      ; memory_rss_bytes
      ; memory_virt_bytes
      ; num_threads
      ; state
      }
  with
  | _ -> None
;;

let get_process_command pid : string =
  try
    let stat = Procfs.Process.Stat.load pid |> Or_error.ok_exn in
    stat.comm
  with
  | _ -> "?"
;;

let get_children_pids_of_thread ~pid tid : Pid.t list =
  let children_file = [%string "/proc/%{pid#Pid}/task/%{tid#Int}/children"] in
  try
    let contents = In_channel.read_all children_file in
    String.split contents ~on:' '
    |> List.filter_map ~f:(fun s ->
      let s = String.strip s in
      if String.is_empty s then None else Option.try_with (fun () -> Pid.of_string s))
  with
  | _ -> []
;;

let get_children_pids pid : Pid.t list =
  get_thread_ids pid
  |> List.concat_map ~f:(get_children_pids_of_thread ~pid)
  |> List.dedup_and_sort ~compare:Pid.compare
;;

let rec get_all_descendants pid : Pid.t list =
  let direct_children = get_children_pids pid in
  let descendants =
    List.concat_map direct_children ~f:(fun child -> child :: get_all_descendants child)
  in
  descendants
;;

let sample_process_group t pid =
  Deferred.return
    (let clk_tck =
       Core_unix.sysconf Core_unix.CLK_TCK
       |> Option.value_exn ~message:"Failed to get CLK_TCK"
       |> Int64.to_float
     in
     let now = Time_ns.now () in
     match sample_single_process t ~now ~clk_tck pid with
     | None -> None
     | Some root_sample ->
       let command = get_process_command pid in
       let root = { Process_stats.Tree_sample.pid; command; sample = root_sample } in
       let children_pids = get_all_descendants pid in
       let children =
         List.filter_map children_pids ~f:(fun child_pid ->
           match sample_single_process t ~now ~clk_tck child_pid with
           | None -> None
           | Some sample ->
             let command = get_process_command child_pid in
             Some { Process_stats.Tree_sample.pid = child_pid; command; sample })
       in
       let total_cpu_percent =
         root_sample.cpu_percent
         +. List.sum (module Float) children ~f:(fun c -> c.sample.cpu_percent)
       in
       let total_memory_rss_bytes =
         Int64.( + )
           root_sample.memory_rss_bytes
           (List.fold children ~init:0L ~f:(fun acc c ->
              Int64.( + ) acc c.sample.memory_rss_bytes))
       in
       Some
         { Process_stats.Tree_sample.root
         ; children
         ; total_cpu_percent
         ; total_memory_rss_bytes
         })
;;

let kill_process t pid =
  (* Kill all processes in the process group with SIGTERM. If the group was stopped via
     SIGSTOP, we send SIGTERM first (which gets queued) then SIGCONT so the process wakes
     up and immediately receives the pending SIGTERM. SIGSTOP cannot be caught, so the
     process can't handle SIGTERM while stopped. *)
  Signal_unix.send_i Signal.term (`Group pid);
  if Hash_set.mem t.stopped_pids pid
  then (
    Signal_unix.send_i Signal.cont (`Group pid);
    Hash_set.remove t.stopped_pids pid)
;;

let pause_process t pid =
  (* Send SIGSTOP to the process group to pause all processes. *)
  Signal_unix.send_i Signal.stop (`Group pid);
  Hash_set.add t.stopped_pids pid
;;

let resume_process t pid =
  (* Send SIGCONT to the process group to resume all processes. *)
  Signal_unix.send_i Signal.cont (`Group pid);
  Hash_set.remove t.stopped_pids pid
;;

let kill_all_running_processes t =
  (* Send SIGTERM to all process groups, return PIDs so caller can wait. Processes will be
     untracked when they exit via start_process. kill_process handles SIGCONT for any
     stopped processes. *)
  let pids = Hashtbl.keys t.running_processes in
  List.iter pids ~f:(fun pid -> kill_process t pid);
  pids
;;

let track_process t ~pid ~stdout ~stderr =
  Hashtbl.set t.running_processes ~key:pid ~data:{ stdout; stderr }
;;

let untrack_process t pid =
  Hashtbl.remove t.running_processes pid;
  Hash_set.remove t.stopped_pids pid
;;

let start_process t ~env ~bash_code =
  if t.closed
  then Deferred.Or_error.error_string "Process manager is closed"
  else (
    let%bind process_result =
      (* Put the process in its own process group so we can kill the entire group *)
      Process.create
        ~env
        ~setpgid:Core_unix.Pgid.new_process_group
        ~prog:"/bin/bash"
        ~args:[ "-c"; bash_code ]
        ()
    in
    match process_result with
    | Error err -> Deferred.return (Error err)
    | Ok process ->
      let pid = Process.pid process in
      let stdout_reader = Process.stdout process in
      let stderr_reader = Process.stderr process in
      track_process t ~pid ~stdout:stdout_reader ~stderr:stderr_reader;
      let stdout = Reader.lines stdout_reader in
      let stderr = Reader.lines stderr_reader in
      let exited =
        let%bind exit_status = Process.wait process in
        untrack_process t pid;
        return exit_status
      in
      Deferred.Or_error.return (pid, ~stdout, ~stderr, exited))
;;

let process_group_has_processes pid =
  let pid_int = Pid.to_int pid in
  match Core_unix.system [%string "pgrep -g %{pid_int#Int} > /dev/null 2>&1"] with
  | Ok () -> true
  | Error _ -> false
;;

let close t =
  t.closed <- true;
  let pids = kill_all_running_processes t in
  let now = Time_ns.now () in
  let warn_every = Time_ns.Span.of_int_sec 2 in
  let warn_at = ref (Time_ns.add now warn_every) in
  let final_deadline = Time_ns.add now (Time_ns.Span.of_sec 10.0) in
  Deferred.repeat_until_finished pids (fun pids ->
    let now = Time_ns.now () in
    let remaining = List.filter pids ~f:process_group_has_processes in
    if List.is_empty remaining
    then Deferred.return (`Finished ())
    else if Time_ns.( >= ) now final_deadline
    then (
      let remaining_pids_str =
        List.map remaining ~f:Pid.to_string |> String.concat ~sep:" "
      in
      Core.eprintf
        "Timed out, but some processes are still running: %s\n%!"
        remaining_pids_str;
      return (`Finished ()))
    else (
      if Time_ns.( >= ) now !warn_at
      then (
        warn_at := Time_ns.add now warn_every;
        let remaining_pids_str =
          List.map remaining ~f:Pid.to_string |> String.concat ~sep:" "
        in
        Core.eprintf "Still running: %s\n%!" remaining_pids_str);
      let%bind () = Clock.after (sec 0.05) in
      Deferred.return (`Repeat remaining)))
;;

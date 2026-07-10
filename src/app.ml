open! Core
open! Async
open Bonsai_term
open Bonsai.Let_syntax

module Focus_mode = struct
  module Output_kind = struct
    type t =
      | Logs
      | Interactive
    [@@deriving sexp, equal, variants]
  end

  type t =
    | Tree
    | Output of Output_kind.t
    | Filter_input of { prev : t }
    | Debug of { prev : t }
    | Modal of
        { which : [ `Help | `Output_menu | `Command_search ]
        ; prev : t
        }
  [@@deriving sexp, equal, variants]
end

(** Common styled text helpers for consistent rendering with Catppuccin theme. These
    helpers assume a crust background. For selection-dependent backgrounds (like in
    make_render_item), use View.text directly with the appropriate attrs.

    Most of the time, you can inherit the default [fg] and [bg] color from the view
    context, but otherwise you need to override. *)

module Styled = struct
  type t =
    { text : string -> View.t
    ; muted : string -> View.t
    ; label : string -> View.t
    ; colored : Attr.Color.t -> string -> View.t
    ; pad : string -> View.t
    ; blank : View.t
    ; fg : Attr.Color.t
    ; bg : Attr.Color.t
    ; highlight : View.t -> View.t
    ; modal_bg : Attr.Color.t
    ; modal_active_border : Attr.Color.t
    ; selection_bg : Attr.Color.t
    }

  let create graph =
    let%map flavor = Bonsai_term_color_scheme.flavor graph in
    let text_color = Bonsai_term_color_scheme.color ~flavor Text in
    let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
    let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
    let surface0 = Bonsai_term_color_scheme.color ~flavor Surface0 in
    let lavender = Bonsai_term_color_scheme.color ~flavor Lavender in
    { text = (fun s -> View.text s)
    ; muted = (fun s -> View.text ~attrs:[ Attr.fg subtext_color ] s)
    ; label = (fun s -> View.text ~attrs:[ Attr.fg mauve; Attr.bold ] s)
    ; colored = (fun color s -> View.text ~attrs:[ Attr.fg color ] s)
    ; pad = (fun s -> View.text s)
    ; blank = View.text ""
    ; fg = text_color
    ; bg = Bonsai_term_color_scheme.color ~flavor Crust
    ; highlight = (fun v -> View.with_colors' ~bg:surface0 v)
    ; modal_bg = surface0
    ; modal_active_border = lavender
    ; selection_bg = surface0
    }
  ;;
end

(** Handle common scrolling keys: page up/down (half screen), g/G (top/bottom), and mouse
    scroll (×3 lines). Returns [Effect.Ignore] for unrecognized keys. *)
let common_scroll_handler scroll_inject (event : Event.t) : unit Effect.t =
  match event with
  | Key_press { key = Page `Up; mods = [] }
  | Key_press { key = ASCII 'u'; mods = [ Ctrl ] }
  | Key_press { key = ASCII 'u'; mods = [] } ->
    scroll_inject Bonsai_term_scroller.Action.Up_half_screen
  | Key_press { key = Page `Down; mods = [] }
  | Key_press { key = ASCII 'd'; mods = [ Ctrl ] }
  | Key_press { key = ASCII 'd'; mods = [] } ->
    scroll_inject Bonsai_term_scroller.Action.Down_half_screen
  | Key_press { key = ASCII 'g'; mods = [] } ->
    scroll_inject Bonsai_term_scroller.Action.Top
  | Key_press { key = ASCII 'G'; mods = [] } ->
    scroll_inject Bonsai_term_scroller.Action.Bottom
  | Mouse { kind = Scroll `Up; _ } ->
    Effect.Many (List.init 3 ~f:(fun _ -> scroll_inject Up))
  | Mouse { kind = Scroll `Down; _ } ->
    Effect.Many (List.init 3 ~f:(fun _ -> scroll_inject Down))
  | _ -> Effect.Ignore
;;

let page_scroll ~on_up ~on_down (event : Event.t) : unit Effect.t =
  match event with
  | Key_press { key = Page `Up; mods = [] }
  | Key_press { key = ASCII 'u'; mods = [ Ctrl ] }
  | Key_press { key = ASCII 'u'; mods = [] } -> on_up
  | Key_press { key = Page `Down; mods = [] }
  | Key_press { key = ASCII 'd'; mods = [ Ctrl ] }
  | Key_press { key = ASCII 'd'; mods = [] } -> on_down
  | _ -> Effect.Ignore
;;

module Process_status = struct
  type t =
    | Not_started
    | Starting (* Start requested but process hasn't started yet *)
    | Running
    | Stopping (* Stop requested but process hasn't exited yet *)
    | Exited of
        { exit_status : Unix.Exit_or_signal.t
        ; message : string
        }
  [@@deriving sexp]

  let is_alive = function
    | Starting | Running | Stopping -> true
    | Not_started | Exited _ -> false
  ;;

  let status_color t ~is_paused ~subtext ~yellow ~green ~red ~blue =
    match t with
    | Not_started -> subtext
    | Starting -> yellow
    | Running -> if is_paused then blue else green
    | Stopping -> if is_paused then blue else yellow
    | Exited { exit_status = Ok (); _ } -> green
    | Exited { exit_status = Error _; _ } -> red
  ;;

  (** Icon for the tree sidebar *)
  let icon ~is_paused = function
    | Not_started -> "○"
    | Starting -> "○"
    | Running -> if is_paused then "■" else "●"
    | Stopping -> if is_paused then "■" else "◎"
    | Exited _ -> "◎"
  ;;

  (** Short prefix for process tree display (empty when running) *)
  let tree_prefix ~is_paused = function
    | Running -> if is_paused then "[paused] " else ""
    | Starting -> "[starting] "
    | Stopping -> if is_paused then "[stopping/paused] " else "[stopping] "
    | Exited _ -> "[exited] "
    | Not_started -> "[not started] "
  ;;

  let render ~is_paused t =
    match t with
    | Not_started -> "not started"
    | Starting -> "starting..."
    | Running -> if is_paused then "paused" else "running"
    | Stopping -> if is_paused then "stopping (paused)" else "stopping..."
    | Exited { exit_status = Ok (); _ } -> "exited (0)"
    | Exited { exit_status = Error (`Exit_non_zero code); _ } ->
      [%string "exited (%{code#Int})"]
    | Exited { exit_status = Error (`Signal signal); _ } ->
      [%string "killed (%{Signal.to_string signal})"]
  ;;
end

module Output_line_content = struct
  type t =
    | Text of string
    | Separator of string
  [@@deriving compare, sexp_of]
end

module Process_state = struct
  type t =
    { status : Process_status.t
    ; output_lines : (int * Output_line_content.t) Circular_buffer.t
    ; pid : Pid.t option
    ; started_at : Time_ns.t option
    ; exited_at : Time_ns.t option
    ; stats_history : Process_stats.History.t
    ; tmux_session_id : Tmux.Session_id.t option
    }
  [@@deriving sexp_of]

  let max_lines = 10_000

  let create () =
    { status = Not_started
    ; output_lines = Circular_buffer.create ~capacity:max_lines
    ; pid = None
    ; started_at = None
    ; exited_at = None
    ; stats_history = Process_stats.History.create ~max_samples:60
    ; tmux_session_id = None
    }
  ;;

  let exit_message t =
    match t.status with
    | Exited { message; _ } -> Some message
    | _ -> None
  ;;

  let add_lines t ~next_seq lines =
    let output_lines, next_seq =
      List.fold lines ~init:(t.output_lines, next_seq) ~f:(fun (buf, seq) line ->
        Circular_buffer.push buf (seq, Output_line_content.Text line), seq + 1)
    in
    { t with output_lines }, next_seq
  ;;

  let add_separator t ~seq ~message =
    { t with
      output_lines =
        Circular_buffer.push t.output_lines (seq, Output_line_content.Separator message)
    }
  ;;

  let add_stats t stats =
    { t with stats_history = Process_stats.History.add t.stats_history stats }
  ;;

  (** Whether the process is currently paused (SIGSTOP'd), derived from the latest stats
      report showing the root process in Stopped state. *)
  let is_paused t =
    match
      Process_status.is_alive t.status, Process_stats.History.latest t.stats_history
    with
    | true, Some stats ->
      (match stats.root.sample.state with
       | Stopped -> true
       | Running | Sleeping | Zombie | Other -> false)
    | _, _ -> false
  ;;

  (** Iterate lines with sequence numbers, oldest to newest *)
  let iter_lines_with_seq t ~f = Circular_buffer.iter t.output_lines ~f

  (** Build a [(path, content)] list oldest to newest, stripping seq numbers *)
  let lines_with_path t ~path =
    Circular_buffer.to_list_map t.output_lines ~f:(fun (_, content) -> path, content)
  ;;
end

module Group_status = struct
  type t =
    | Initializing
    | Failed of string
  [@@deriving sexp]
end

module Group_state = struct
  type t =
    { status : Group_status.t
    ; output_lines : string Circular_buffer.t
    }
  [@@deriving sexp_of]

  let create () =
    { status = Initializing; output_lines = Circular_buffer.create ~capacity:100 }
  ;;

  let add_line t line = { t with output_lines = Circular_buffer.push t.output_lines line }

  let lines_with_path t ~path =
    Circular_buffer.to_list_map t.output_lines ~f:(fun line ->
      path, Output_line_content.Text line)
  ;;
end

module Action = struct
  type t =
    | Start_requested of Command_id.t
    | Start_process of Command_id.t * Pid.t
    | Add_output of Command_id.t * string list
    | Process_exited of Command_id.t * Unix.Exit_or_signal.t
    | Update_stats of Command_id.t * Process_stats.Tree_sample.t
    | Kill_requested of Command_id.t
    | Set_tmux_session of Command_id.t * Tmux.Session_id.t
    | Group_output of Command_id.t * string
    | Group_finished of Command_id.t * Group_status.t
  [@@deriving sexp_of]
end

module App_event = struct
  type t =
    | Start_process of { id : Command_id.t }
    | Kill_process of { id : Command_id.t }
    | Restart_process of { id : Command_id.t }
    | Pause_process of { id : Command_id.t }
    | Resume_process of { id : Command_id.t }
end

let selected_item ~nav_model =
  Option.map (Navigable_tree.Model.selected_row nav_model) ~f:(fun row -> row.value)
;;

(* Build render_item function for the navigable tree *)
let make_render_item
  ~title
  ~(process_states : Process_state.t Command_id.Map.t)
  ~(group_states : Group_state.t Command_id.Map.t)
  ~(flavor : Bonsai_term_color_scheme.Flavor.t)
  : int -> Tree_item.t Navigable_tree.Row.t -> Navigable_tree.Item_display.t
  =
  let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
  let green = Bonsai_term_color_scheme.color ~flavor Green in
  let red = Bonsai_term_color_scheme.color ~flavor Red in
  let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
  let blue = Bonsai_term_color_scheme.color ~flavor Blue in
  fun i row ->
    match i with
    | 0 ->
      let label = View.text ~attrs:[ Attr.fg yellow; Attr.bold ] title in
      { Navigable_tree.Item_display.label; status = View.none }
    | _ ->
      let label_text = Tree_item.name row.value in
      let label = View.text label_text in
      let status =
        match row.value with
        | Section _ -> View.none
        | Initializing_group { id; _ } ->
          let group_state =
            Map.find group_states id |> Option.value_or_thunk ~default:Group_state.create
          in
          let status_color, status_char =
            match group_state.status with
            | Initializing -> yellow, "…"
            | Failed _ -> red, "✕"
          in
          View.text ~attrs:[ Attr.fg status_color ] status_char
        | Command { id; _ } ->
          let process_state =
            Map.find process_states id
            |> Option.value_or_thunk ~default:Process_state.create
          in
          let is_paused = Process_state.is_paused process_state in
          let status_color =
            Process_status.status_color
              process_state.status
              ~is_paused
              ~subtext:subtext_color
              ~yellow
              ~green
              ~red
              ~blue
          in
          let status_char = Process_status.icon ~is_paused process_state.status in
          View.text ~attrs:[ Attr.fg status_color ] status_char
      in
      { Navigable_tree.Item_display.label; status }
;;

let selected_tree_item ~(nav_model : Tree_item.t Navigable_tree.Model.t)
  : Tree_item.t option
  =
  Navigable_tree.Model.selected_row nav_model
  |> Option.map ~f:(fun row -> row.Navigable_tree.Row.value)
;;

let wrap_text ~width text =
  Styled_text.wrap ~width (Styled_text.of_string text)
  |> List.map ~f:Styled_text.to_string
;;

let format_bytes bytes = Byte_units.of_bytes_int64_exn bytes |> Byte_units.to_string_hum

(** Render process tree lines for a stats sample with a given status prefix. Returns
    string lines like " [exited] 1234 server (CPU:5.0% Mem:47.7M)". *)
let render_process_tree_strings
  ~(status : Process_status.t)
  ~is_paused
  (stats : Process_stats.Tree_sample.t)
  =
  let status_prefix = Process_status.tree_prefix ~is_paused status in
  let root_info =
    sprintf
      "  %s%s %s (CPU:%.1f%% Mem:%s)"
      status_prefix
      (Pid.to_string stats.root.pid)
      stats.root.command
      stats.root.sample.cpu_percent
      (format_bytes stats.root.sample.memory_rss_bytes)
  in
  let children_info =
    List.map stats.children ~f:(fun child ->
      sprintf
        "  └─ %s %s (CPU:%.1f%% Mem:%s)"
        (Pid.to_string child.pid)
        child.command
        child.sample.cpu_percent
        (format_bytes child.sample.memory_rss_bytes))
  in
  root_info :: children_info
;;

(** Render CPU and Mem sparkline View.t lines. Takes pre-computed sparkline strings and
    optional current values. *)
let render_stats_lines
  ~(styled : Styled.t)
  ~peach
  ~teal
  ~sparkline_bg
  ~cpu_sparkline
  ~mem_sparkline
  ~latest_cpu
  ~latest_mem
  =
  let cpu_line =
    View.hcat
      ([ styled.label "CPU: "
       ; View.text ~attrs:[ Attr.fg peach; Attr.bg sparkline_bg ] cpu_sparkline
       ]
       @
       match latest_cpu with
       | Some cpu -> [ styled.text (sprintf " %5.1f%%" cpu) ]
       | None -> [])
  in
  let mem_line =
    View.hcat
      ([ styled.label "Mem: "
       ; View.text ~attrs:[ Attr.fg teal; Attr.bg sparkline_bg ] mem_sparkline
       ]
       @
       match latest_mem with
       | Some mem -> [ styled.text (" " ^ format_bytes mem) ]
       | None -> [])
  in
  [ cpu_line; mem_line ]
;;

(** Render a "Label: " prefix followed by wrapped text, with continuation lines indented
    to align with the first line's content. *)
let render_wrapped_label ~(styled : Styled.t) ~width ~label text =
  let label_width = String.length label in
  let content_width = width - label_width in
  let wrapped = wrap_text ~width:content_width text in
  List.mapi wrapped ~f:(fun i line ->
    if i = 0
    then View.hcat [ styled.label label; styled.text line ]
    else View.hcat [ styled.pad (String.make label_width ' '); styled.text line ])
;;

let render_preview_header
  ~(nav_model : Tree_item.t Navigable_tree.Model.t Bonsai.t)
  ~(process_states : Process_state.t Command_id.Map.t Bonsai.t)
  ~(group_states : Group_state.t Command_id.Map.t Bonsai.t)
  ~(width : int Bonsai.t)
  ~(show_process_tree : bool Bonsai.t)
  (local_ graph)
  =
  let%arr nav_model
  and process_states
  and group_states
  and width
  and show_process_tree
  and flavor = Bonsai_term_color_scheme.flavor graph
  and styled = Styled.create graph in
  let blue = Bonsai_term_color_scheme.color ~flavor Blue in
  let teal = Bonsai_term_color_scheme.color ~flavor Teal in
  let green = Bonsai_term_color_scheme.color ~flavor Green in
  let red = Bonsai_term_color_scheme.color ~flavor Red in
  let peach = Bonsai_term_color_scheme.color ~flavor Peach in
  let sparkline_bg = Bonsai_term_color_scheme.color ~flavor Surface0 in
  match selected_tree_item ~nav_model with
  | None -> styled.muted "(no selection)"
  | Some (Section name) ->
    (* Get all descendant commands for aggregate stats *)
    let descendants = Navigable_tree.Model.selected_descendants nav_model in
    let command_ids =
      List.filter_map descendants ~f:(function
        | Tree_item.Command { id; is_interactive = false; _ } -> Some id
        | Tree_item.Command { is_interactive = true; _ }
        | Tree_item.Section _ | Tree_item.Initializing_group _ -> None)
    in
    let sparkline_width = 20 in
    (* Collect all histories from child processes *)
    let all_histories =
      List.filter_map command_ids ~f:(fun id ->
        let process_state =
          Map.find process_states id
          |> Option.value_or_thunk ~default:Process_state.create
        in
        if Process_stats.History.is_empty process_state.stats_history
        then None
        else Some (id, process_state.stats_history))
    in
    (* Compute aggregated sparklines by summing CPU and memory across all processes *)
    let has_any_stats = not (List.is_empty all_histories) in
    let aggregated_cpu_percents, aggregated_mem_bytes =
      if not has_any_stats
      then [], []
      else (
        (* Find the minimum history length to align samples *)
        let min_len =
          List.fold all_histories ~init:Int.max_value ~f:(fun acc (_, h) ->
            Int.min acc (List.length (Process_stats.History.to_list h)))
        in
        (* Sum up values at each time point *)
        let cpu_sums = Array.create ~len:min_len 0.0 in
        let mem_sums = Array.create ~len:min_len 0L in
        List.iter all_histories ~f:(fun (_, h) ->
          let cpu_list = Process_stats.History.cpu_percents h in
          let mem_list = Process_stats.History.memory_rss_bytes h in
          (* Take only the most recent min_len samples *)
          let cpu_recent = List.rev cpu_list |> List.take _ min_len |> List.rev in
          let mem_recent = List.rev mem_list |> List.take _ min_len |> List.rev in
          List.iteri cpu_recent ~f:(fun i v -> cpu_sums.(i) <- cpu_sums.(i) +. v);
          List.iteri mem_recent ~f:(fun i v -> mem_sums.(i) <- Int64.( + ) mem_sums.(i) v));
        Array.to_list cpu_sums, Array.to_list mem_sums)
    in
    let cpu_sparkline =
      Process_stats.render_sparkline
        aggregated_cpu_percents
        ~max_width:sparkline_width
        ~min_bound:0.0
        ~max_bound:100.0
        ~right_align:true
        ()
    in
    let max_mem =
      List.fold aggregated_mem_bytes ~init:0L ~f:(fun acc m ->
        if Int64.( > ) m acc then m else acc)
      |> Int64.to_float
    in
    let mem_sparkline =
      Process_stats.render_sparkline
        (List.map aggregated_mem_bytes ~f:Int64.to_float)
        ~max_width:sparkline_width
        ~min_bound:0.0
        ~max_bound:(Float.max max_mem 1.0)
        ~right_align:true
        ()
    in
    let latest_cpu =
      if has_any_stats
      then Some (List.last aggregated_cpu_percents |> Option.value ~default:0.0)
      else None
    in
    let latest_mem =
      if has_any_stats
      then Some (List.last aggregated_mem_bytes |> Option.value ~default:0L)
      else None
    in
    let stats_lines =
      render_stats_lines
        ~styled
        ~peach
        ~teal
        ~sparkline_bg
        ~cpu_sparkline
        ~mem_sparkline
        ~latest_cpu
        ~latest_mem
    in
    (* Collect stats with status for tree display *)
    let histories_with_status =
      List.filter_map command_ids ~f:(fun id ->
        let process_state =
          Map.find process_states id
          |> Option.value_or_thunk ~default:Process_state.create
        in
        match Process_stats.History.latest process_state.stats_history with
        | Some stats ->
          let is_paused = Process_state.is_paused process_state in
          Some (stats, process_state.status, is_paused)
        | None -> None)
    in
    (* Count total processes across alive children only *)
    let total_procs =
      List.fold histories_with_status ~init:0 ~f:(fun acc (stats, status, _is_paused) ->
        if Process_status.is_alive status
        then acc + 1 + List.length stats.children
        else acc)
    in
    let procs_text = if total_procs > 0 then sprintf " (%d procs)" total_procs else "" in
    (* Process tree display for sections - annotate exited processes *)
    let tree_lines =
      if show_process_tree && not (List.is_empty histories_with_status)
      then
        List.concat_map histories_with_status ~f:(fun (stats, status, is_paused) ->
          List.map (render_process_tree_strings ~status ~is_paused stats) ~f:styled.muted)
      else []
    in
    View.vcat
      ([ View.hcat
           [ styled.label "Section: "; styled.colored blue name; styled.muted procs_text ]
       ]
       @ stats_lines
       @ tree_lines
       @ [ styled.blank ])
  | Some (Command { id; bash_code; is_interactive; _ }) ->
    let process_state =
      Map.find process_states id |> Option.value_or_thunk ~default:Process_state.create
    in
    let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
    let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
    let is_paused = Process_state.is_paused process_state in
    let status_color =
      Process_status.status_color
        process_state.status
        ~is_paused
        ~subtext:subtext_color
        ~yellow
        ~green
        ~red
        ~blue
    in
    let status_text =
      let base_status = Process_status.render ~is_paused process_state.status in
      match process_state.status, process_state.pid with
      | Running, Some pid -> [%string "%{base_status} (PID %{pid#Pid})"]
      | _ -> base_status
    in
    let command_lines =
      render_wrapped_label ~styled ~width ~label:"Command: " bash_code
    in
    (* Stats display - always show CPU/Mem lines, empty sparkline if no samples *)
    let sparkline_width = 20 in
    let cpu_sparkline =
      Process_stats.render_sparkline
        (Process_stats.History.cpu_percents process_state.stats_history)
        ~max_width:sparkline_width
        ~min_bound:0.0
        ~max_bound:100.0
        ~right_align:true
        ()
    in
    let max_mem =
      Process_stats.History.max_memory_rss_bytes process_state.stats_history
      |> Int64.to_float
    in
    let mem_sparkline =
      Process_stats.render_sparkline
        (Process_stats.History.memory_rss_bytes process_state.stats_history
         |> List.map ~f:Int64.to_float)
        ~max_width:sparkline_width
        ~min_bound:0.0
        ~max_bound:(Float.max max_mem 1.0)
        ~right_align:true
        ()
    in
    let latest = Process_stats.History.latest process_state.stats_history in
    let stats_lines =
      render_stats_lines
        ~styled
        ~peach
        ~teal
        ~sparkline_bg
        ~cpu_sparkline
        ~mem_sparkline
        ~latest_cpu:(Option.map latest ~f:(fun s -> s.total_cpu_percent))
        ~latest_mem:(Option.map latest ~f:(fun s -> s.total_memory_rss_bytes))
    in
    let is_alive = Process_status.is_alive process_state.status in
    (* Process tree display - annotate exited processes *)
    let tree_lines =
      match Process_stats.History.latest process_state.stats_history with
      | Some stats when show_process_tree ->
        List.map
          (render_process_tree_strings ~status:process_state.status ~is_paused stats)
          ~f:styled.muted
      | Some _ | None -> []
    in
    (* Status line - combine with process count only when alive *)
    let status_line =
      match Process_stats.History.latest process_state.stats_history with
      | Some stats when is_alive ->
        let num_procs = 1 + List.length stats.children in
        let procs_text = if num_procs > 1 then sprintf " (%d procs)" num_procs else "" in
        View.hcat
          [ styled.label "Status: "
          ; styled.colored status_color status_text
          ; styled.muted procs_text
          ]
      | Some _ | None ->
        View.hcat [ styled.label "Status: "; styled.colored status_color status_text ]
    in
    View.vcat
      (command_lines
       @ stats_lines
       @ [ status_line ]
       @ tree_lines
       @
       if is_interactive && Option.is_some process_state.tmux_session_id
       then []
       else [ styled.blank ])
  | Some (Initializing_group { id; bash_code }) ->
    let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
    let group_state =
      Map.find group_states id |> Option.value_or_thunk ~default:Group_state.create
    in
    let status_color, status_text =
      match group_state.status with
      | Initializing -> yellow, "Initializing..."
      | Failed msg -> red, [%string "Failed: %{msg}"]
    in
    let command_lines =
      if String.is_empty bash_code
      then []
      else render_wrapped_label ~styled ~width ~label:"Command: " bash_code
    in
    let status_line = styled.colored status_color status_text in
    View.vcat (command_lines @ [ status_line ])
;;

(** Information about a single output line for selection, copying, and scrolling *)
module Output_line = struct
  type t =
    { view : View.t
    ; raw_line : string (* The original unformatted line *)
    ; formatted_message : string (* The formatted message (without timestamp/level) *)
    ; rendered_height : int (* Number of terminal rows this line takes when displayed *)
    ; start_row : int (* Starting row (0-based) in the rendered output *)
    ; original_index : int
        (* Index in the unfiltered list; equals display index when no filter *)
    ; is_separator : bool
    }
  [@@deriving fields ~getters]
end

(** Result from render_output_content with selection support. Each [Output_line.t]
    contains its pre-rendered view (without selection highlighting) so that selection can
    be applied cheaply in a separate step without re-rendering all content. *)
module Output_render_result = struct
  type t =
    | No_output
    | Filtered_out
    | Lines of { lines : Output_line.t Iarray.t }

  let empty = No_output

  let lines = function
    | No_output | Filtered_out -> Iarray.empty
    | Lines { lines } -> lines
  ;;

  let view t ~selected_line ~selection_bg ~width ~muted =
    match t with
    | No_output -> muted "(no output)"
    | Filtered_out -> muted "(no matching output)"
    | Lines { lines } ->
      let views =
        Iarray.mapi lines ~f:(fun i line ->
          let view = Output_line.view line in
          if [%equal: int option] selected_line (Some i)
          then (
            let view_width = View.width view in
            let padding = max 0 (width - view_width) in
            let padded_view =
              if padding > 0
              then View.hcat [ view; View.text (String.make padding ' ') ]
              else view
            in
            View.with_colors' ~fill_backdrop:true ~bg:selection_bg padded_view)
          else view)
      in
      View.vcat (Iarray.to_list views)
  ;;
end

let render_output_content
  ~(nav_model : Tree_item.t Navigable_tree.Model.t Bonsai.t)
  ~(process_states : Process_state.t Command_id.Map.t Bonsai.t)
  ~(group_states : Group_state.t Command_id.Map.t Bonsai.t)
  ~(wrap_output : bool Bonsai.t)
  ~(parse_logs : bool Bonsai.t)
  ~(timestamp_mode : Parsed_log_line.Timestamp_mode.t Bonsai.t)
  ~(message_mode : Parsed_log_line.Message_mode.t Bonsai.t)
  ~(filter_tokens : Filter_query.Token.t list Bonsai.t)
  ~(width : int Bonsai.t)
  (local_ graph)
  =
  let%arr nav_model
  and process_states
  and group_states
  and wrap_output
  and parse_logs
  and timestamp_mode
  and message_mode
  and filter_tokens
  and width
  and flavor = Bonsai_term_color_scheme.flavor graph in
  let matches_filter ~path line =
    let level =
      if not parse_logs
      then ""
      else (
        match Parsed_log_line.parse line with
        | None -> ""
        | Some { level = None; _ } -> ""
        | Some { level = Some level; _ } -> Log.Level.to_string level)
    in
    Filter_query.matches filter_tokens ~path ~level ~line
  in
  let red = Bonsai_term_color_scheme.color ~flavor Red in
  let green = Bonsai_term_color_scheme.color ~flavor Green in
  let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
  let cyan = Bonsai_term_color_scheme.color ~flavor Teal in
  let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
  let muted_attrs = [ Attr.fg subtext_color ] in
  let zone = force Time_ns.Zone.local in
  (* style_line returns (styled_text list, formatted_message, new_previous_time) where
     new_previous_time is the timestamp from this line if parsed, or the input
     previous_time otherwise. Returns multiple styled lines when the formatted message
     contains newlines. The formatted_message is just the message part (no
     timestamp/level) for copying. *)
  let style_line ~start_time ~previous_time line =
    if not parse_logs
    then [ Styled_text.of_string line ], line, previous_time
    else (
      match Parsed_log_line.parse line with
      | None -> [ Styled_text.of_string line ], line, previous_time
      | Some { timestamp; level; message } ->
        let level_fragment =
          match level with
          | None -> Styled_text.of_string " "
          | Some level ->
            let level_str, level_color =
              match level with
              | `Debug -> "Dbg", cyan
              | `Info -> "Inf", green
              | `Warn -> "War", yellow
              | `Error -> "Err", red
            in
            Styled_text.of_string
              ~attrs:[ Attr.fg level_color ]
              [%string " %{level_str} "]
        in
        let timestamp_str =
          Parsed_log_line.format_timestamp
            timestamp
            ~mode:timestamp_mode
            ~start_time
            ~previous_time
            ~zone
        in
        let formatted_message = Parsed_log_line.format_message ~message_mode message in
        let message_lines = String.split_lines formatted_message in
        let styled_lines =
          List.mapi message_lines ~f:(fun i msg_line ->
            if i = 0
            then
              Styled_text.concat
                [ Styled_text.of_string ~attrs:muted_attrs timestamp_str
                ; level_fragment
                ; Styled_text.of_string msg_line
                ]
            else (
              (* Continuation lines: indent to align with first line's message *)
              let indent =
                String.length timestamp_str
                + String.length (Styled_text.to_string level_fragment)
              in
              Styled_text.concat
                [ Styled_text.of_string (String.make indent ' ')
                ; Styled_text.of_string msg_line
                ]))
        in
        styled_lines, formatted_message, Some timestamp)
  in
  (* Render a styled line to a view, calculating its height *)
  let render_styled_line_with_height styled_line =
    let view =
      if wrap_output
      then Styled_text.to_view_wrapped ~width styled_line
      else Styled_text.to_view styled_line
    in
    view, View.height view
  in
  (* Render a separator as a yellow horizontal line with centered message *)
  let render_separator message =
    let yellow_attrs = [ Attr.fg yellow ] in
    let line =
      View.text ~attrs:yellow_attrs (String.concat (List.init width ~f:(fun _ -> "─")))
    in
    if String.is_empty message
    then line
    else (
      let label =
        View.text ~attrs:yellow_attrs [%string " %{message} "]
        |> View.center ~within:{ width; height = 1 }
      in
      View.zcat [ label; line ])
  in
  (* Render lines, returning Output_render_result.t. Applies filter first so delta times
     are computed between visible lines only. Separators always pass through the filter. *)
  let render_lines ~start_time ~prefix_fn output_lines_with_paths =
    (* Apply filter first, tracking original indices and paths. Separators always pass
       through. *)
    let filtered_with_indices =
      List.filter_mapi output_lines_with_paths ~f:(fun orig_idx (path, content) ->
        match (content : Output_line_content.t) with
        | Separator _ -> Some (orig_idx, path, content)
        | Text line ->
          if matches_filter ~path line then Some (orig_idx, path, content) else None)
    in
    if List.is_empty filtered_with_indices
    then
      if List.is_empty output_lines_with_paths
      then Output_render_result.No_output
      else Output_render_result.Filtered_out
    else (
      let _, _current_row, lines_rev =
        List.foldi
          filtered_with_indices
          ~init:(start_time, 0, [])
          ~f:(fun idx (prev_time, row, lines) (orig_idx, path, content) ->
            match (content : Output_line_content.t) with
            | Separator message ->
              let prefix = prefix_fn idx path "" in
              let full_message =
                if String.is_empty prefix
                then message
                else (
                  let trimmed_prefix = String.rstrip prefix in
                  [%string "%{trimmed_prefix} %{message}"])
              in
              let view = render_separator full_message in
              let height = View.height view in
              let line_data =
                { Output_line.view
                ; raw_line = ""
                ; formatted_message = ""
                ; rendered_height = height
                ; start_row = row
                ; original_index = orig_idx
                ; is_separator = true
                }
              in
              prev_time, row + height, line_data :: lines
            | Text line ->
              let prefix = prefix_fn idx path line in
              let styled_lines, formatted_message, new_prev_time =
                style_line ~start_time ~previous_time:prev_time line
              in
              let styled_with_prefix =
                List.mapi styled_lines ~f:(fun i styled_line ->
                  if i = 0 && not (String.is_empty prefix)
                  then
                    Styled_text.concat
                      [ Styled_text.of_string ~attrs:muted_attrs prefix; styled_line ]
                  else if i > 0 && not (String.is_empty prefix)
                  then (
                    let indent = String.length prefix in
                    Styled_text.concat
                      [ Styled_text.of_string (String.make indent ' '); styled_line ])
                  else styled_line)
              in
              (* Render all styled lines for this logical line *)
              let line_views_with_heights =
                List.map styled_with_prefix ~f:render_styled_line_with_height
              in
              let total_height =
                List.fold line_views_with_heights ~init:0 ~f:(fun acc (_, h) -> acc + h)
              in
              let view = List.map line_views_with_heights ~f:fst |> View.vcat in
              let line_data =
                { Output_line.view
                ; raw_line = line
                ; formatted_message
                ; rendered_height = total_height
                ; start_row = row
                ; original_index = orig_idx
                ; is_separator = false
                }
              in
              new_prev_time, row + total_height, line_data :: lines)
      in
      Output_render_result.Lines { lines = Iarray.of_list_rev lines_rev })
  in
  match selected_tree_item ~nav_model with
  | None -> Output_render_result.empty
  | Some (Section _) ->
    (* Get all descendant commands with their relative paths *)
    let descendants_with_paths =
      Navigable_tree.Model.selected_descendants' nav_model ~get_name:Tree_item.name
    in
    let commands_with_paths =
      List.filter_map descendants_with_paths ~f:(fun (item, ~path) ->
        match item with
        | Tree_item.Command { id; is_interactive = false; _ } -> Some (path, id)
        | Tree_item.Command { is_interactive = true; _ }
        | Tree_item.Section _ | Tree_item.Initializing_group _ -> None)
    in
    (* Find the earliest start time among all descendant processes *)
    let start_time =
      List.filter_map commands_with_paths ~f:(fun (_, id) ->
        let process_state =
          Map.find process_states id
          |> Option.value_or_thunk ~default:Process_state.create
        in
        process_state.started_at)
      |> List.min_elt ~compare:Time_ns.compare
    in
    (* Collect all lines with sequence numbers and relative paths from all processes *)
    let all_lines_with_seq = Vec.create () in
    List.iter commands_with_paths ~f:(fun (path, id) ->
      let process_state =
        Map.find process_states id |> Option.value_or_thunk ~default:Process_state.create
      in
      let relative_path =
        match path with
        | [] -> Command_id.display_name id
        | _ -> String.concat path ~sep:"/"
      in
      Process_state.iter_lines_with_seq process_state ~f:(fun (seq, content) ->
        Vec.push_back all_lines_with_seq (seq, relative_path, content)));
    (* Sort by sequence number (ascending) to get chronological order *)
    Vec.sort all_lines_with_seq ~compare:[%compare: int * _ * _];
    (* Render lines with dimmed path labels - filtering is done in render_lines. We pass
       output_lines with their paths, building prefix_fn to look up paths. *)
    let lines_with_path = Vec.to_list all_lines_with_seq in
    let output_lines_with_paths =
      List.map lines_with_path ~f:(fun (_, path, content) -> path, content)
    in
    let prefix_fn _idx path _line = [%string "[%{path}] "] in
    render_lines ~start_time ~prefix_fn output_lines_with_paths
  | Some (Command { id; is_interactive = true; _ }) ->
    (* Interactive commands render their tmux session separately, but if the session is
       inactive, we render any output lines here (e.g. error messages from failed starts
       sent by the channel) *)
    let process_state =
      Map.find process_states id |> Option.value_or_thunk ~default:Process_state.create
    in
    let path = Command_id.display_name id in
    let output_lines_with_paths = Process_state.lines_with_path process_state ~path in
    let prefix_fn _ _ _ = "" in
    render_lines ~start_time:process_state.started_at ~prefix_fn output_lines_with_paths
  | Some (Command { id; _ }) ->
    let process_state =
      Map.find process_states id |> Option.value_or_thunk ~default:Process_state.create
    in
    let path = Command_id.display_name id in
    let output_lines_with_paths = Process_state.lines_with_path process_state ~path in
    let prefix_fn _ _ _ = "" in
    render_lines ~start_time:process_state.started_at ~prefix_fn output_lines_with_paths
  | Some (Initializing_group { id; _ }) ->
    let group_state =
      Map.find group_states id |> Option.value_or_thunk ~default:Group_state.create
    in
    let path = Command_id.display_name id in
    let output_lines_with_paths = Group_state.lines_with_path group_state ~path in
    let prefix_fn _ _ _ = "" in
    render_lines ~start_time:None ~prefix_fn output_lines_with_paths
;;

(* Render output for interactive items using Bonsai_term_tmux. Attaches to a tmux session
   owned by the instance process. *)
let render_output_interactive
  ~(session_id : Tmux.Session_id.t Bonsai.t)
  ~(exit_message : string option Bonsai.t)
  ~(header : View.t Bonsai.t)
  ~(tmux_dimensions : Dimensions.t Bonsai.t)
  ~(focus_mode : Focus_mode.t Bonsai.t)
  ~(show_tree : bool Bonsai.t)
  (local_ graph)
  =
  (* Scope on the session ID so that a restart (which produces a new session) creates a
     fresh attach_component instance and resets all per-session state. *)
  Bonsai.scope_model
    (module Sexp)
    ~on:
      (let%arr session_id in
       Tmux.Session_id.sexp_of_t session_id)
    graph
    ~for_:(fun graph ->
      (* When not fullscreen, we render a border around the interactive app. Shrink the
         inner tmux dimensions by 2 in each direction to account for the border
         characters. *)
      let show_border = show_tree in
      let inner_tmux_dimensions =
        let%arr { Dimensions.width; height } = tmux_dimensions
        and show_border in
        if show_border
        then { Dimensions.width = max 1 (width - 2); height = max 1 (height - 2) }
        else { Dimensions.width; height }
      in
      let tmux =
        Bonsai_term_tmux.attach_component
          ~session_id
          ~dimensions:inner_tmux_dimensions
          graph
      in
      (* Track the last successfully rendered tmux view so we can show it after the
         session closes (when polling starts returning errors). *)
      let last_ok_view, set_last_ok_view = Bonsai.state View.none graph in
      let () =
        Bonsai.Edge.on_change
          ~trigger:`After_display
          ~equal:phys_equal
          (let%arr { Bonsai_term_tmux.Attached.last_view; _ } = tmux in
           last_view)
          ~callback:
            (let%arr set_last_ok_view in
             fun (last_view : View.t Pending_or_error.t) ->
               match last_view with
               | Ok view -> set_last_ok_view view
               | Pending | Error _ -> Effect.Ignore)
          graph
      in
      let view =
        let%arr header
        and { Bonsai_term_tmux.Attached.is_closed; _ } = tmux
        and last_ok_view
        and exit_message
        and { Dimensions.width = inner_width; height = inner_height } =
          inner_tmux_dimensions
        and { Dimensions.width = outer_width; height = _ } = tmux_dimensions
        and show_border
        and focus_mode
        and flavor = Bonsai_term_color_scheme.flavor graph in
        let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
        let tmux_area =
          if is_closed
          then (
            let yellow_attrs = [ Attr.fg yellow ] in
            let text = Option.value exit_message ~default:"process exited" in
            let message = View.text ~attrs:yellow_attrs [%string " %{text} "] in
            let overlay =
              Bonsai_term_border_box.view
                ~line_type:Round_corners
                ~attrs:yellow_attrs
                ~left_padding:1
                ~right_padding:1
                message
              |> View.center ~within:{ width = inner_width; height = inner_height }
            in
            View.zcat [ overlay; last_ok_view ])
          else last_ok_view
        in
        let is_focused =
          [%equal: Focus_mode.t] focus_mode (Focus_mode.Output Interactive)
        in
        let hint =
          let muted = Bonsai_term_color_scheme.color ~flavor Overlay0 in
          if is_focused
          then View.text ~attrs:[ Attr.fg muted ] " Ctrl-Q to unfocus "
          else if not is_closed
          then View.text ~attrs:[ Attr.fg muted ] " Tab to focus "
          else View.none
        in
        if show_border
        then (
          let border_color =
            if is_focused
            then Bonsai_term_color_scheme.color ~flavor Lavender
            else Bonsai_term_color_scheme.color ~flavor Surface0
          in
          let box =
            Bonsai_term_border_box.view
              ~line_type:Round_corners
              ~attrs:[ Attr.fg border_color ]
              tmux_area
          in
          let hint_width = View.width hint in
          let box_width = View.width box in
          let padded_hint = View.pad ~l:(Int.max 0 (box_width - hint_width - 1)) hint in
          View.vcat [ header; View.zcat [ padded_hint; box ] ])
        else (
          (* Fullscreen: float hint at the right edge on the last line of the header *)
          let base = View.vcat [ header; tmux_area ] in
          let hint_width = View.width hint in
          if hint_width = 0
          then base
          else (
            let header_height = View.height header in
            let left_pad = max 0 (outer_width - hint_width) in
            let hint_overlay = View.pad ~t:(header_height - 1) ~l:left_pad hint in
            View.zcat [ hint_overlay; base ]))
      in
      let handler =
        let%arr { Bonsai_term_tmux.Attached.handler; _ } = tmux in
        handler
      in
      let%arr view and handler in
      view, handler)
;;

let render_preview
  ~(nav_model : Tree_item.t Navigable_tree.Model.t Bonsai.t)
  ~(process_states : Process_state.t Command_id.Map.t Bonsai.t)
  ~(group_states : Group_state.t Command_id.Map.t Bonsai.t)
  ~(wrap_output : bool Bonsai.t)
  ~(parse_logs : bool Bonsai.t)
  ~(timestamp_mode : Parsed_log_line.Timestamp_mode.t Bonsai.t)
  ~(message_mode : Parsed_log_line.Message_mode.t Bonsai.t)
  ~(filter_text : string Bonsai.t)
  ~(filter_tokens : Filter_query.Token.t list Bonsai.t)
  ~(filter_textbox_view : View.t Bonsai.t)
  ~(filter_input_active : bool Bonsai.t)
  ~(show_process_tree : bool Bonsai.t)
  ~(focus_mode : Focus_mode.t Bonsai.t)
  ~(selected_line : int option Bonsai.t)
  ~(set_selected_line : (int option -> unit Effect.t) Bonsai.t)
  ~(show_tree : bool Bonsai.t)
  ~dimensions
  (local_ graph)
  =
  (* Compute whether filter bar should be visible *)
  let selected_is_interactive =
    let%arr nav_model in
    match selected_item ~nav_model with
    | Some (Command { is_interactive = true; _ }) -> true
    | _ -> false
  in
  let filter_visible =
    let%arr filter_text and filter_input_active and selected_is_interactive in
    (not selected_is_interactive)
    && (filter_input_active || not (String.is_empty filter_text))
  in
  let width =
    let%arr { Dimensions.width; _ } = dimensions in
    width
  in
  let header =
    render_preview_header
      ~nav_model
      ~process_states
      ~group_states
      ~width
      ~show_process_tree
      graph
  in
  let output_result =
    render_output_content
      ~nav_model
      ~process_states
      ~group_states
      ~wrap_output
      ~parse_logs
      ~timestamp_mode
      ~message_mode
      ~filter_tokens
      ~width
      graph
  in
  let output_view =
    let%arr output_result
    and selected_line
    and styled = Styled.create graph
    and width in
    Output_render_result.view
      output_result
      ~selected_line
      ~selection_bg:styled.selection_bg
      ~width
      ~muted:styled.muted
  in
  let scroller_dimensions =
    let%arr { Dimensions.height; width } = dimensions
    and header
    and filter_visible in
    let header_height = View.height header in
    (* Reserve 1 line for filter bar when visible *)
    let filter_height = if filter_visible then 1 else 0 in
    { Dimensions.height = max 1 (height - header_height - filter_height); width }
  in
  (* Extract the tmux session ID for the selected interactive command, if any *)
  let selected_interactive_session_id =
    let%arr nav_model and process_states in
    match selected_item ~nav_model with
    | Some (Command { id; is_interactive = true; _ }) ->
      let process_state = Map.find process_states id in
      Option.bind process_state ~f:(fun s -> s.tmux_session_id)
    | _ -> None
  in
  let selected_interactive_exit_message =
    let%arr nav_model and process_states in
    match selected_item ~nav_model with
    | Some (Command { id; is_interactive = true; _ }) ->
      let process_state = Map.find process_states id in
      Option.bind process_state ~f:Process_state.exit_message
    | _ -> None
  in
  (* Use tree index to scope both scroller and interactive preview - this ensures each
     item gets its own state *)
  let selected_tree_index =
    let%arr nav_model in
    Navigable_tree.Model.selected_index nav_model
  in
  Bonsai.scope_model (module Int) ~on:selected_tree_index graph ~for_:(fun graph ->
    (* Render the scroller view (for non-interactive items) *)
    let%sub { view = scrolled_output
            ; inject = scroll_inject
            ; stuck_to_bottom
            ; scroll_position
            ; _
            }
      =
      Bonsai_term_scroller.component
        ~crop_width_if_too_big:`No
        ~default_stuck_to_bottom:true
        ~dimensions:scroller_dimensions
        output_view
        graph
    in
    let is_at_bottom =
      let%arr scroll_position in
      match scroll_position with
      | Bonsai_term_scroller.Scroll_position.Bottom | All_visible -> true
      | Top | Percentage _ -> false
    in
    (* Autoscroll to selected line when selection changes *)
    let () =
      Bonsai.Edge.on_change
        ~trigger:`After_display
        ~equal:[%equal: int option]
        selected_line
        ~callback:
          (let%arr scroll_inject and output_result in
           fun selected_line ->
             match selected_line with
             | None -> Effect.Ignore
             | Some idx ->
               (match Iarray.get_opt (Output_render_result.lines output_result) idx with
                | Some line ->
                  (* Scroll so the selected line is visible. The scroller expects
                     top/bottom to be the row indices of the element to show. *)
                  let bottom_row =
                    Output_line.start_row line + Output_line.rendered_height line - 1
                  in
                  scroll_inject
                    (Bonsai_term_scroller.Action.Scroll_to
                       { top = Output_line.start_row line; bottom = bottom_row })
                | None -> Effect.Ignore))
        graph
    in
    let scroller_handler =
      let%arr scroll_inject
      and focus_mode
      and selected_line
      and set_selected_line
      and output_result
      and is_at_bottom in
      let lines = Output_render_result.lines output_result in
      let line_count = Iarray.length lines in
      (* Find the next non-separator line index in the given direction, or None if there
         are no selectable lines remaining. *)
      let next_selectable ~from ~direction =
        let rec loop i =
          if i < 0 || i >= line_count
          then None
          else (
            match Iarray.get_opt lines i with
            | Some line when not (Output_line.is_separator line) -> Some i
            | _ -> loop (i + direction))
        in
        loop from
      in
      let first_selectable = next_selectable ~from:0 ~direction:1 in
      let last_selectable = next_selectable ~from:(line_count - 1) ~direction:(-1) in
      fun (event : Event.t) ->
        match focus_mode with
        | Focus_mode.Tree ->
          (* When tree is focused, use page up/down for scrolling *)
          let%bind.Effect () =
            page_scroll
              ~on_up:(scroll_inject Bonsai_term_scroller.Action.Up_half_screen)
              ~on_down:
                (if is_at_bottom
                 then scroll_inject Stick_to_bottom
                 else scroll_inject Down_half_screen)
              event
          in
          (match event with
           | Key_press { key = ASCII 'g'; mods = [] } ->
             scroll_inject Bonsai_term_scroller.Action.Top
           | Key_press { key = ASCII 'G'; mods = [] } ->
             scroll_inject Bonsai_term_scroller.Action.Stick_to_bottom
           | Mouse { kind = Scroll `Up; _ } ->
             Effect.Many (List.init 3 ~f:(fun _ -> scroll_inject Up))
           | Mouse { kind = Scroll `Down; _ } ->
             if is_at_bottom
             then scroll_inject Stick_to_bottom
             else Effect.Many (List.init 3 ~f:(fun _ -> scroll_inject Down))
           | _ -> Effect.Ignore)
        | Focus_mode.Output Logs ->
          (* When output is focused, j/k move selection, skipping separators *)
          let%bind.Effect () =
            page_scroll
              ~on_up:(scroll_inject Bonsai_term_scroller.Action.Up_half_screen)
              ~on_down:
                (if is_at_bottom
                 then scroll_inject Stick_to_bottom
                 else scroll_inject Down_half_screen)
              event
          in
          (match event with
           | Key_press { key = ASCII 'j'; mods = [] }
           | Key_press { key = Arrow `Down; mods = [] } ->
             let target =
               match selected_line with
               | None -> first_selectable
               | Some idx -> next_selectable ~from:(idx + 1) ~direction:1
             in
             (match target with
              | Some new_idx -> set_selected_line (Some new_idx)
              | None -> Effect.Ignore)
           | Key_press { key = ASCII 'k'; mods = [] }
           | Key_press { key = Arrow `Up; mods = [] } ->
             let target =
               match selected_line with
               | None -> last_selectable
               | Some idx -> next_selectable ~from:(idx - 1) ~direction:(-1)
             in
             (match target with
              | Some new_idx -> set_selected_line (Some new_idx)
              | None -> Effect.Ignore)
           | Key_press { key = ASCII 'g'; mods = [] } ->
             (* Go to first selectable line *)
             (match first_selectable with
              | Some idx -> set_selected_line (Some idx)
              | None -> Effect.Ignore)
           | Key_press { key = ASCII 'G'; mods = [] } ->
             (* Go to last selectable line and re-enable autoscroll *)
             (match last_selectable with
              | Some idx ->
                Effect.Many
                  [ set_selected_line (Some idx)
                  ; scroll_inject Bonsai_term_scroller.Action.Stick_to_bottom
                  ]
              | None -> Effect.Ignore)
           | Mouse { kind = Scroll `Up; _ } ->
             Effect.Many (List.init 3 ~f:(fun _ -> scroll_inject Up))
           | Mouse { kind = Scroll `Down; _ } ->
             if is_at_bottom
             then scroll_inject Stick_to_bottom
             else Effect.Many (List.init 3 ~f:(fun _ -> scroll_inject Down))
           | _ -> Effect.Ignore)
        | _ -> Effect.Ignore
    in
    let%sub interactive_view, interactive_handler =
      match%sub selected_interactive_session_id with
      | Some session_id ->
        render_output_interactive
          ~session_id
          ~exit_message:selected_interactive_exit_message
          ~header
          ~tmux_dimensions:scroller_dimensions
          ~focus_mode
          ~show_tree
          graph
      | None ->
        (* No tmux session -- show the regular output (which may contain error messages
           from failed starts) *)
        let view =
          let%arr header and scrolled_output in
          View.vcat [ header; scrolled_output ]
        in
        Bonsai.both view (Bonsai.return (fun (_ : Event.t) -> Effect.Ignore))
    in
    let scroll_warning =
      let%arr stuck_to_bottom
      and { Dimensions.height; width } = scroller_dimensions
      and flavor = Bonsai_term_color_scheme.flavor graph in
      if stuck_to_bottom
      then View.text ""
      else (
        let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
        let surface0 = Bonsai_term_color_scheme.color ~flavor Surface0 in
        let banner_text = " G to autoscroll " in
        let banner = View.text ~attrs:[ Attr.fg yellow; Attr.bg surface0 ] banner_text in
        (* Position at the bottom of the scroller area *)
        let left_pad = max 0 (width - String.length banner_text) in
        View.pad ~t:(height - 1) ~l:left_pad banner)
    in
    (* Render filter bar inline at bottom of preview *)
    let filter_bar_view =
      let%arr filter_textbox_view
      and filter_visible
      and { Dimensions.width; _ } = dimensions
      and flavor = Bonsai_term_color_scheme.flavor graph in
      if not filter_visible
      then View.none
      else (
        let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
        let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
        let surface0 = Bonsai_term_color_scheme.color ~flavor Surface0 in
        let prefix_view =
          View.text ~attrs:[ Attr.fg mauve; Attr.bold; Attr.bg surface0 ] "/"
        in
        let hint =
          View.text ~attrs:[ Attr.fg subtext_color; Attr.bg surface0 ] "(Esc to clear)"
        in
        (* Layout: [/][textbox] ... padding ... [hint] *)
        let left_content = View.hcat [ prefix_view; filter_textbox_view ] in
        let left_width = View.width left_content in
        let hint_width = View.width hint in
        let padding = max 0 (width - left_width - hint_width) in
        View.hcat
          [ left_content
          ; View.text ~attrs:[ Attr.bg surface0 ] (String.make padding ' ')
          ; hint
          ])
    in
    (* Choose which view to display based on selection *)
    let scroller_view =
      let%arr header
      and scrolled_output
      and scroll_warning
      and filter_bar_view
      and filter_visible
      and { Dimensions.height; _ } = dimensions in
      let output_with_warning = View.zcat [ scroll_warning; scrolled_output ] in
      let main_content = View.vcat [ header; output_with_warning ] in
      (* Position filter bar at the very bottom of the panel *)
      if not filter_visible
      then main_content
      else (
        let filter_top = height - 1 in
        View.zcat [ View.pad ~t:filter_top filter_bar_view; main_content ])
    in
    let view =
      let%arr selected_is_interactive and scroller_view and interactive_view in
      if selected_is_interactive then interactive_view else scroller_view
    in
    let output_handler =
      let%arr focus_mode and scroller_handler and interactive_handler in
      fun event ->
        match focus_mode with
        | Tree | Output Logs -> scroller_handler event
        | Output Interactive -> interactive_handler event
        | _ -> Effect.Ignore
    in
    let header_height =
      let%arr header in
      View.height header
    in
    let%arr view
    and output_handler
    and interactive_handler
    and scroll_inject
    and output_result
    and header_height in
    view, output_handler, interactive_handler, scroll_inject, output_result, header_height)
;;

let render_instructions ~(width : int Bonsai.t) (local_ graph) =
  let%arr width
  and flavor = Bonsai_term_color_scheme.flavor graph in
  (* Inverse color scheme: dark text on muted light background *)
  let key_color = Bonsai_term_color_scheme.color ~flavor Subtext1 in
  let action_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
  let instruction key action =
    View.hcat
      [ View.text ~attrs:[ Attr.bold; Attr.fg key_color ] key
      ; View.text " "
      ; View.text ~attrs:[ Attr.fg action_color ] action
      ]
  in
  let content =
    View.hcat
      (List.intersperse
         ~sep:(View.text "  ")
         [ instruction "?" "Help"
         ; instruction "Enter" "Run"
         ; instruction "x" "Kill"
         ; instruction "R" "Restart"
         ; instruction "PgUp/Dn" "Scroll"
         ; instruction "o" "Options"
         ; instruction "f" "Fullscreen"
         ; instruction "/" "Filter"
         ; instruction "^K" "Jump"
         ; instruction "Tab" "Focus"
         ])
  in
  (* Pad the instruction bar to fill the full width *)
  let content_width = View.width content in
  let padding = max 0 (width - content_width) in
  View.hcat [ content; View.text (String.make padding ' ') ]
;;

let help_content =
  [ "? / F1", "Toggle help", "Show or hide this help modal"
  ; "Escape", "Close/Deselect", "Close modals or clear selected output line"
  ; "", "", ""
  ; "Navigation", "", ""
  ; ( "j / Down"
    , "Move down"
    , "Move selection down in tree view, or scroll down in output view" )
  ; "k / Up", "Move up", "Move selection up in tree view, or scroll up in output view"
  ; "l / Right", "Expand/Enter", "Expand a collapsed section, or move to first child"
  ; "h / Left", "Collapse/Parent", "Collapse an expanded section, or move to parent"
  ; "Ctrl-K", "Jump to", "Search and jump to a command by name"
  ; "", "", ""
  ; "Process Control", "", ""
  ; "Enter / S", "Start", "Run the selected command or all commands in a section"
  ; "x / K", "Kill", "Kill the selected process or all processes in a section"
  ; "R", "Restart", "Kill and restart; if already stopped, just start"
  ; "z", "Pause/Resume", "Toggle pause (SIGSTOP) and resume (SIGCONT) for the process"
  ; "", "", ""
  ; "Output View", "", ""
  ; "Tab / m", "Toggle focus", "Switch focus between tree and output panel"
  ; "u / PgUp", "Scroll up", "Scroll output up by half a screen"
  ; "d / PgDn", "Scroll down", "Scroll output down by half a screen"
  ; "g / G", "Top / Bottom", "Jump to beginning or end of output (G autoscrolls)"
  ; "/", "Filter", "Filter output: word, !exclude, @path, \"exact\""
  ; "Escape", "Clear filter", "Clear the current filter"
  ; "c / C", "Copy", "Copy selected line to clipboard (c: formatted, C: raw)"
  ; "w", "Toggle wrap", "Toggle line wrapping for long output lines"
  ; "Alt-r", "Toggle raw", "Toggle between raw output and log syntax highlighting"
  ; "t / T", "Cycle time", "Cycle timestamp: time only, relative to start, delta"
  ; "v / V", "Cycle format", "Cycle message format (shift to go backwards)"
  ; "", "", ""
  ; "Display", "", ""
  ; "o", "Output options", "Open output options menu for wrap, raw, and timestamp"
  ; "f", "Fullscreen", "Show or hide the left tree panel"
  ; "p", "Toggle procs", "Show or hide the process tree with child processes"
  ; "`", "Toggle debug", "Show or hide the debug panel with log messages"
  ]
;;

(* Calculate column widths based on actual content, excluding headers and empty rows *)
let help_column_widths =
  let key_width, action_width =
    List.fold help_content ~init:(0, 0) ~f:(fun (max_key, max_action) (key, action, _) ->
      (* Skip empty lines and section headers (where action is empty) *)
      if String.is_empty action
      then max_key, max_action
      else Int.max max_key (String.length key), Int.max max_action (String.length action))
  in
  (* Add 2 spaces of padding after each column *)
  key_width + 2, action_width + 2
;;

(** Center content in a bordered modal with an opaque background. *)
let render_centered_modal ~screen_height ~screen_width ~border_color ~bg content =
  let padded_content = View.pad ~l:1 ~r:1 content in
  let bordered_content =
    Bonsai_term_border_box.view
      ~line_type:Round_corners
      ~attrs:[ Attr.fg border_color ]
      padded_content
  in
  let modal_height = View.height bordered_content in
  let modal_width = View.width bordered_content in
  let top_pad = max 0 ((screen_height - modal_height) / 2) in
  let left_pad = max 0 ((screen_width - modal_width) / 2) in
  let background = View.rectangle ~height:modal_height ~width:modal_width () in
  let modal_with_bg = View.zcat [ bordered_content; background ] in
  View.pad ~t:top_pad ~l:left_pad modal_with_bg |> View.with_colors' ~bg
;;

let render_help_modal ~dimensions (local_ graph) =
  (* Calculate modal dimensions first so we can size content appropriately *)
  let modal_dimensions =
    let%arr { Dimensions.height; width } = dimensions in
    (* Take up most of the screen *)
    let modal_height = max 5 (height - 4) in
    let modal_width = max 30 (width - 6) in
    { Dimensions.height = modal_height; width = modal_width }
  in
  let help_view =
    let%arr flavor = Bonsai_term_color_scheme.flavor graph
    and { Dimensions.width = content_width; _ } = modal_dimensions in
    let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
    let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
    let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
    let key_width, action_width = help_column_widths in
    let desc_width = max 10 (content_width - key_width - action_width) in
    (* Wrap text to fit within a given width, splitting on word boundaries *)
    let wrap_text text width =
      if String.length text <= width
      then [ text ]
      else (
        let words = String.split text ~on:' ' in
        let rec build_lines current_line lines = function
          | [] ->
            let final_lines =
              if String.is_empty current_line then lines else current_line :: lines
            in
            List.rev final_lines
          | word :: rest ->
            let new_line =
              if String.is_empty current_line then word else current_line ^ " " ^ word
            in
            if String.length new_line <= width
            then build_lines new_line lines rest
            else if String.is_empty current_line
            then
              (* Word itself is too long, just use it *)
              build_lines "" (word :: lines) rest
            else build_lines word (current_line :: lines) rest
        in
        build_lines "" [] words)
    in
    let pad_line line_view =
      let line_width = View.width line_view in
      let padding = max 0 (content_width - line_width) in
      View.hcat [ line_view; View.text (String.make padding ' ') ]
    in
    let render_entry (key, action, description) =
      if String.is_empty key && String.is_empty action
      then [ pad_line (View.text "") ]
      else if (not (String.is_empty key)) && String.is_empty action
      then [ pad_line (View.text ~attrs:[ Attr.fg yellow; Attr.bold ] key) ]
      else (
        let pad_to_width s width =
          let len = String.length s in
          if len >= width then s else s ^ String.make (width - len) ' '
        in
        let desc_lines = wrap_text description desc_width in
        List.mapi desc_lines ~f:(fun i desc_line ->
          let key_text = if i = 0 then key else "" in
          let action_text = if i = 0 then action else "" in
          let key_view =
            View.text
              ~attrs:[ Attr.fg mauve; Attr.bold ]
              (pad_to_width key_text key_width)
          in
          let action_view = View.text (pad_to_width action_text action_width) in
          let desc_view = View.text ~attrs:[ Attr.fg subtext_color ] desc_line in
          pad_line (View.hcat [ key_view; action_view; desc_view ])))
    in
    View.vcat (List.concat_map help_content ~f:render_entry)
  in
  (* Scroller dimensions - same as modal minus border *)
  let scroller_dimensions =
    let%arr { Dimensions.height; width } = modal_dimensions in
    { Dimensions.height = height - 2; width = width - 2 }
  in
  let%sub { view = scrolled_help; inject = scroll_inject; _ } =
    Bonsai_term_scroller.component
      ~crop_width_if_too_big:`No
      ~default_stuck_to_bottom:false
      ~dimensions:scroller_dimensions
      help_view
      graph
  in
  let view =
    let%arr scrolled_help
    and { Dimensions.height; width } = dimensions
    and styled = Styled.create graph in
    render_centered_modal
      ~screen_height:height
      ~screen_width:width
      ~border_color:styled.modal_active_border
      ~bg:styled.modal_bg
      scrolled_help
  in
  let handler =
    let%arr scroll_inject in
    fun (event : Event.t) ->
      let%bind.Effect () = common_scroll_handler scroll_inject event in
      match event with
      | Key_press { key = Arrow `Up; mods = [] }
      | Key_press { key = ASCII 'k'; mods = [] } ->
        scroll_inject Bonsai_term_scroller.Action.Up
      | Key_press { key = Arrow `Down; mods = [] }
      | Key_press { key = ASCII 'j'; mods = [] } ->
        scroll_inject Bonsai_term_scroller.Action.Down
      | _ -> Effect.Ignore
  in
  let%arr view and handler in
  view, handler
;;

let render_output_menu
  ~dimensions
  ~(wrap_output : bool Bonsai.t)
  ~(parse_logs : bool Bonsai.t)
  ~(timestamp_mode : Parsed_log_line.Timestamp_mode.t Bonsai.t)
  ~(message_mode : Parsed_log_line.Message_mode.t Bonsai.t)
  (local_ graph)
  =
  let%arr flavor = Bonsai_term_color_scheme.flavor graph
  and { Dimensions.height; width } = dimensions
  and wrap_output
  and parse_logs
  and timestamp_mode
  and message_mode
  and styled = Styled.create graph in
  let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
  let green = Bonsai_term_color_scheme.color ~flavor Green in
  let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
  let title = View.text ~attrs:[ Attr.fg mauve; Attr.bold ] "Output Options" in
  let space = View.text " " in
  let subtitle =
    View.text ~attrs:[ Attr.fg subtext_color ] "These are global keybindings"
  in
  let blank = View.text "" in
  let checkbox ~checked ~key ~label =
    let check_char = if checked then "▣" else "□" in
    let check_view = View.text ~attrs:[ Attr.fg green ] check_char in
    let key_view = View.text ~attrs:[ Attr.fg mauve ] key in
    let label_view = View.text label in
    View.hcat [ key_view; space; check_view; space; label_view ]
  in
  let radio ~selected ~label =
    let check_char = if selected then "◉" else "○" in
    let check_view = View.text ~attrs:[ Attr.fg green ] check_char in
    let label_view = View.text label in
    View.hcat [ check_view; space; label_view ]
  in
  let wrap_line = checkbox ~checked:wrap_output ~key:"    w" ~label:"Wrap lines" in
  let raw_line =
    checkbox ~checked:(not parse_logs) ~key:"Alt-r" ~label:"Raw log output"
  in
  let timestamp_header =
    View.hcat [ View.text ~attrs:[ Attr.fg mauve ] "t"; View.text " Timestamp format" ]
  in
  let timestamp_view =
    View.vcat
      (timestamp_header
       :: List.map Parsed_log_line.Timestamp_mode.all ~f:(fun mode ->
         let label =
           match mode with
           | Time_only -> "Time"
           | Relative -> "Since start"
           | Delta -> "Since last"
         in
         radio
           ~selected:([%equal: Parsed_log_line.Timestamp_mode.t] timestamp_mode mode)
           ~label))
  in
  let pretty_header =
    View.hcat [ View.text ~attrs:[ Attr.fg mauve ] "v"; View.text " Message format" ]
  in
  let pretty_view =
    View.vcat
      (pretty_header
       :: List.map Parsed_log_line.Message_mode.all ~f:(fun mode ->
         let label =
           match mode with
           | Normal -> "Normal"
           | Pretty -> "Pretty"
           | Expectree -> "Expectree"
           | Expectable -> "Expectable"
         in
         radio
           ~selected:([%equal: Parsed_log_line.Message_mode.t] message_mode mode)
           ~label))
  in
  let raw_disclaimer =
    if parse_logs
    then View.text ""
    else View.vcat [ View.text ~attrs:[ Attr.fg subtext_color ] "No effect in raw mode:" ]
  in
  let format_options = View.hcat [ timestamp_view; View.text " "; pretty_view ] in
  let content =
    View.vcat
      [ title; subtitle; blank; wrap_line; raw_line; raw_disclaimer; format_options ]
  in
  render_centered_modal
    ~screen_height:height
    ~screen_width:width
    ~border_color:styled.modal_active_border
    ~bg:styled.modal_bg
    content
;;

let render_command_search_modal
  ~(title : string)
  ~(nav_model : Tree_item.t Navigable_tree.Model.t Bonsai.t)
  ~(nav_inject : (Tree_item.t Navigable_tree.Action.t -> unit Effect.t) Bonsai.t)
  ~(set_focus_mode : (Focus_mode.t -> unit Effect.t) Bonsai.t)
  ~dimensions
  (local_ graph)
  =
  let items =
    let%arr nav_model in
    Navigable_tree.Model.visible_values nav_model
    |> List.filter_map ~f:(fun item ->
      let name =
        match item with
        | Tree_item.Command { id; _ } -> Command_id.to_string id
        | Tree_item.Section name -> name
        | Tree_item.Initializing_group { id; _ } -> Command_id.to_string id
      in
      let name = if String.is_empty name then title else name in
      Some (name, item))
  in
  let render_item =
    let%arr flavor = Bonsai_term_color_scheme.flavor graph in
    let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
    let green = Bonsai_term_color_scheme.color ~flavor Green in
    let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
    fun name (item : Tree_item.t) ~is_selected ->
      let icon_text =
        match item with
        | Command _ -> "◆ "
        | Section _ -> "▸ "
        | Initializing_group _ -> "… "
      in
      let icon =
        View.text
          ~attrs:[ Attr.fg (if is_selected then green else subtext_color) ]
          icon_text
      in
      let label =
        match
          String.lsplit2 name ~on:'/'
          |> Option.map ~f:(fun _ -> String.rsplit2_exn name ~on:'/')
        with
        | None ->
          let attrs =
            if is_selected then [ Attr.fg yellow; Attr.bold ] else [ Attr.fg yellow ]
          in
          View.text ~attrs name
        | Some (prefix, basename) ->
          let prefix_view = View.text ~attrs:[ Attr.fg subtext_color ] (prefix ^ "/") in
          let basename_attrs =
            if is_selected then [ Attr.fg yellow; Attr.bold ] else [ Attr.fg yellow ]
          in
          let basename_view = View.text ~attrs:basename_attrs basename in
          View.hcat [ prefix_view; basename_view ]
      in
      { Selection_modal.Item_view.icon; label }
  in
  let on_select =
    let%arr nav_inject and set_focus_mode in
    fun item ->
      let select_action =
        Navigable_tree.Action.Select_path (fun value -> [%equal: Tree_item.t] value item)
      in
      Effect.Many [ nav_inject select_action; set_focus_mode Focus_mode.Tree ]
  in
  let%sub { Selection_modal.view; handler; reset } =
    Selection_modal.component ~items ~render_item ~on_select ~dimensions graph
  in
  let%arr view and handler and reset in
  view, handler, reset
;;

let handle_output_option_key
  ~wrap_output
  ~set_wrap_output
  ~parse_logs
  ~set_parse_logs
  ~timestamp_mode
  ~set_timestamp_mode
  ~message_mode
  ~set_message_mode
  (event : Event.t)
  =
  match event with
  | Key_press { key = ASCII 'w'; mods = [] } -> set_wrap_output (not wrap_output)
  | Key_press { key = ASCII 'r'; mods = [ Meta ] } -> set_parse_logs (not parse_logs)
  | Key_press { key = ASCII 't'; mods = [] } ->
    Effect.Many
      [ set_timestamp_mode (Parsed_log_line.Timestamp_mode.cycle timestamp_mode)
      ; set_parse_logs true
      ]
  | Key_press { key = ASCII 'T'; mods = [] } ->
    Effect.Many
      [ set_timestamp_mode (Parsed_log_line.Timestamp_mode.cycle_back timestamp_mode)
      ; set_parse_logs true
      ]
  | Key_press { key = ASCII 'v'; mods = [] } ->
    Effect.Many
      [ set_message_mode (Parsed_log_line.Message_mode.cycle message_mode)
      ; set_parse_logs true
      ]
  | Key_press { key = ASCII 'V'; mods = [] } ->
    Effect.Many
      [ set_message_mode (Parsed_log_line.Message_mode.cycle_back message_mode)
      ; set_parse_logs true
      ]
  | _ -> Effect.Ignore
;;

let global_handler
  ~(nav_model : Tree_item.t Navigable_tree.Model.t Bonsai.t)
  ~(process_states : Process_state.t Command_id.Map.t Bonsai.t)
  ~nav_inject
  ~inject
  ~output_option_handler
  ~(show_tree : bool Bonsai.t)
  ~set_fullscreen
  ~(show_process_tree : bool Bonsai.t)
  ~set_show_process_tree
  ~(focus_mode : Focus_mode.t Bonsai.t)
  ~set_focus_mode
  ~(output_result : Output_render_result.t Bonsai.t)
  ~(selected_line : int option Bonsai.t)
  ~set_selected_line
  ~set_pending_interactive_focus
  ~copy_to_clipboard
  ~on_event
  (local_ graph)
  =
  let output_kind =
    let%arr nav_model and process_states in
    let (kind : Focus_mode.Output_kind.t) =
      match selected_item ~nav_model with
      | Some (Command { id; is_interactive = true; _ }) ->
        (match Map.find process_states id with
         | Some state ->
           if Option.is_some state.tmux_session_id && Process_status.is_alive state.status
           then Interactive
           else Logs
         | None -> Logs)
      | _ -> Logs
    in
    kind
  in
  let output_kind_peek = Bonsai.peek output_kind graph in
  let nav_model_peek = Bonsai.peek nav_model graph in
  let process_states_peek = Bonsai.peek process_states graph in
  let output_result_peek = Bonsai.peek output_result graph in
  let selected_line_peek = Bonsai.peek selected_line graph in
  let%arr nav_inject
  and nav_model_peek
  and process_states_peek
  and inject
  and output_option_handler
  and show_tree
  and set_fullscreen
  and show_process_tree
  and set_show_process_tree
  and focus_mode
  and set_focus_mode
  and output_result_peek
  and selected_line_peek
  and set_selected_line
  and set_pending_interactive_focus
  and output_kind_peek
  and copy_to_clipboard
  and on_event in
  let for_selected_commands ~f =
    let%bind.Effect nav_model_status = nav_model_peek in
    let%bind.Effect process_states_status = process_states_peek in
    match nav_model_status, process_states_status with
    | Active nav_model, Active process_states ->
      let descendants = Navigable_tree.Model.selected_descendants nav_model in
      let command_ids =
        List.filter_map descendants ~f:(fun item ->
          match item with
          | Tree_item.Command { id; _ } -> Some id
          | Tree_item.Section _ | Tree_item.Initializing_group _ -> None)
      in
      let effects =
        List.filter_map command_ids ~f:(fun id ->
          let process_state =
            Map.find process_states id
            |> Option.value_or_thunk ~default:Process_state.create
          in
          f id process_state)
      in
      Effect.Many effects
    | Inactive, _ | _, Inactive -> Effect.Ignore
  in
  fun (event : Event.t) ->
    let%bind.Effect () =
      match event with
      | Key_press { key = ASCII 'f'; mods = [] } ->
        (* Toggle fullscreen mode *)
        if show_tree
        then (
          (* When entering fullscreen, ensure focus is on output *)
          let%bind.Effect output_kind_status = output_kind_peek in
          let output_kind =
            match output_kind_status with
            | Active output_kind -> output_kind
            | Inactive -> Logs
          in
          Effect.Many
            [ set_fullscreen true; set_focus_mode (Focus_mode.Output output_kind) ])
        else
          (* When exiting fullscreen, deselect and return focus to tree *)
          Effect.Many
            [ set_fullscreen false
            ; set_focus_mode Focus_mode.Tree
            ; set_selected_line None
            ]
      | Key_press { key = ASCII 'p'; mods = [] } ->
        set_show_process_tree (not show_process_tree)
      | _ -> Effect.Ignore
    in
    let%bind.Effect () = output_option_handler event in
    (* Process interaction hotkeys (kill, restart, pause/resume) run before the
       focus-mode-specific handlers so they work in both Tree and Output modes. This keeps
       them usable in fullscreen mode where the tree sidebar is hidden and focus is on
       Output. *)
    let%bind.Effect () =
      match event with
      | Key_press { key = ASCII 'x'; mods = [] }
      | Key_press { key = ASCII 'K'; mods = [] } ->
        for_selected_commands ~f:(fun id process_state ->
          match process_state.status with
          | Running ->
            Some
              (Effect.Many
                 [ inject (Action.Kill_requested id)
                 ; on_event (App_event.Kill_process { id })
                 ])
          | Not_started | Starting | Stopping | Exited _ -> None)
      | Key_press { key = ASCII 'R'; mods = [] } ->
        for_selected_commands ~f:(fun id process_state ->
          match process_state.status with
          | Starting -> None
          | Not_started | Exited _ ->
            Some
              (Effect.Many
                 [ inject (Action.Start_requested id)
                 ; on_event (App_event.Start_process { id })
                 ])
          | Running ->
            Some
              (Effect.Many
                 [ inject (Action.Kill_requested id)
                 ; on_event (App_event.Restart_process { id })
                 ])
          | Stopping -> Some (on_event (App_event.Restart_process { id })))
      | Key_press { key = ASCII 'z'; mods = [] } ->
        for_selected_commands ~f:(fun id process_state ->
          let is_paused = Process_state.is_paused process_state in
          match process_state.status with
          | Running ->
            if is_paused
            then Some (on_event (App_event.Resume_process { id }))
            else Some (on_event (App_event.Pause_process { id }))
          | Stopping when is_paused -> Some (on_event (App_event.Resume_process { id }))
          | Not_started | Starting | Stopping | Exited _ -> None)
      | _ -> Effect.Ignore
    in
    match focus_mode with
    | Tree ->
      (match event with
       | Key_press { key = Tab; mods = [] } | Key_press { key = ASCII 'm'; mods = [] } ->
         let%bind.Effect output_kind_status = output_kind_peek in
         let output_kind =
           match output_kind_status with
           | Active output_kind -> output_kind
           | Inactive -> Logs
         in
         set_focus_mode (Focus_mode.Output output_kind)
       | Key_press { key = ASCII 'j'; mods = [] }
       | Key_press { key = Arrow `Down; mods = [] } ->
         nav_inject Navigable_tree.Action.Move_down
       | Key_press { key = ASCII 'k'; mods = [] }
       | Key_press { key = Arrow `Up; mods = [] } ->
         nav_inject Navigable_tree.Action.Move_up
       | Key_press { key = ASCII 'l'; mods = [] }
       | Key_press { key = Arrow `Right; mods = [] } ->
         nav_inject Navigable_tree.Action.Expand_or_select_next_at_lower_depth
       | Key_press { key = Enter; mods = [] } | Key_press { key = ASCII 'S'; mods = [] }
         ->
         let%bind.Effect output_kind_status = output_kind_peek
         and nav_model_status = nav_model_peek in
         (match output_kind_status, nav_model_status with
          | Active output_kind, Active nav_model ->
            (match output_kind with
             | Interactive -> set_focus_mode (Focus_mode.Output Interactive)
             | Logs ->
               let pending_interactive_focus =
                 match selected_item ~nav_model with
                 | Some (Command { is_interactive; _ }) -> is_interactive
                 | _ -> false
               in
               Effect.Many
                 [ set_pending_interactive_focus pending_interactive_focus
                 ; for_selected_commands ~f:(fun id process_state ->
                     if Process_status.is_alive process_state.status
                     then None
                     else
                       Some
                         (Effect.Many
                            [ inject (Action.Start_requested id)
                            ; on_event (App_event.Start_process { id })
                            ]))
                 ])
          | _ -> Effect.Ignore)
       | Key_press { key = ASCII 'h'; mods = [] }
       | Key_press { key = Arrow `Left; mods = [] } ->
         let%bind.Effect nav_model_status = nav_model_peek in
         (match nav_model_status with
          | Inactive -> Effect.Ignore
          | Active nav_model ->
            (match Navigable_tree.Model.selected_row nav_model with
             | Some row when row.is_branch && row.is_expanded ->
               nav_inject Navigable_tree.Action.Toggle_expand
             | Some _ | None -> nav_inject Navigable_tree.Action.Select_parent))
       | _ -> Effect.Ignore)
    | Output _ ->
      (match event with
       | Key_press { key = Tab; mods = [] } | Key_press { key = ASCII 'm'; mods = [] } ->
         (* Tab/m switches focus back to tree and deselects *)
         Effect.Many [ set_focus_mode Focus_mode.Tree; set_selected_line None ]
       | Key_press { key = ASCII 'c'; mods = [] } ->
         (* Copy formatted message of selected line *)
         let%bind.Effect output_result_status = output_result_peek in
         let%bind.Effect selected_line_status = selected_line_peek in
         (match output_result_status, selected_line_status with
          | Active output_result, Active (Some idx) ->
            (match Iarray.get_opt (Output_render_result.lines output_result) idx with
             | Some line -> copy_to_clipboard (Output_line.formatted_message line)
             | None -> Effect.Ignore)
          | _, _ -> Effect.Ignore)
       | Key_press { key = ASCII 'C'; mods = [] } ->
         (* Copy raw line of selected line *)
         let%bind.Effect output_result_status = output_result_peek in
         let%bind.Effect selected_line_status = selected_line_peek in
         (match output_result_status, selected_line_status with
          | Active output_result, Active (Some idx) ->
            (match Iarray.get_opt (Output_render_result.lines output_result) idx with
             | Some line -> copy_to_clipboard (Output_line.raw_line line)
             | None -> Effect.Ignore)
          | _, _ -> Effect.Ignore)
       | _ -> Effect.Ignore)
    | _ -> Effect.Ignore
;;

let render_debug_panel
  ~(debug_messages : string list Bonsai.t)
  ~(dimensions : Dimensions.t Bonsai.t)
  (local_ graph)
  =
  let debug_content =
    let%arr debug_messages
    and styled = Styled.create graph in
    if List.is_empty debug_messages
    then styled.muted "(no debug messages)"
    else List.map debug_messages ~f:styled.text |> View.vcat
  in
  (* Scroller dimensions account for the header line *)
  let scroller_dimensions =
    let%arr { Dimensions.height; width } = dimensions in
    { Dimensions.height = max 1 (height - 1); width }
  in
  let%sub { view = scrolled_debug; inject = scroll_inject; _ } =
    Bonsai_term_scroller.component
      ~crop_width_if_too_big:`No
      ~default_stuck_to_bottom:true
      ~dimensions:scroller_dimensions
      debug_content
      graph
  in
  let handler =
    let%arr scroll_inject in
    common_scroll_handler scroll_inject
  in
  let view =
    let%arr scrolled_debug
    and { Dimensions.height; width } = dimensions
    and flavor = Bonsai_term_color_scheme.flavor graph in
    let surface0 = Bonsai_term_color_scheme.color ~flavor Surface0 in
    let yellow = Bonsai_term_color_scheme.color ~flavor Yellow in
    let debug_header =
      View.text
        ~attrs:[ Attr.fg yellow; Attr.bg surface0; Attr.bold ]
        " Debug (press ` to toggle) "
    in
    let header_width = View.width debug_header in
    let line_width = max 0 (width - header_width) in
    let horizontal_line =
      View.text
        ~attrs:[ Attr.fg surface0 ]
        (String.concat (List.init line_width ~f:(fun _ -> "─")))
    in
    let content =
      View.vcat [ View.hcat [ debug_header; horizontal_line ]; scrolled_debug ]
    in
    (* Add opaque background behind debug panel *)
    let bg = View.rectangle ~height ~width () in
    View.zcat [ content; bg ]
    |> View.with_colors' ~bg:(Bonsai_term_color_scheme.color ~flavor Mantle)
  in
  let%arr view and handler in
  view, handler
;;

let backdrop ~dimensions (local_ _graph) =
  let%arr { Dimensions.height; width } = dimensions in
  View.rectangle ~height ~width ()
;;

let vertical_divider ~height ~(focus_mode : Focus_mode.t Bonsai.t) (local_ graph) =
  let%arr height
  and focus_mode
  and flavor = Bonsai_term_color_scheme.flavor graph in
  let color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
  let top_char =
    match focus_mode with
    | Tree | Filter_input _ | Debug _ | Modal _ -> "┐"
    | Output _ -> "┌"
  in
  if height <= 0
  then View.none
  else
    View.vcat
      (View.text ~attrs:[ Attr.fg color ] top_char
       :: List.init (height - 1) ~f:(fun _ -> View.text ~attrs:[ Attr.fg color ] "│"))
;;

let render_error_bar
  ~(current_error : string option Bonsai.t)
  ~(width : int Bonsai.t)
  graph
  =
  let%arr current_error
  and width
  and flavor = Bonsai_term_color_scheme.flavor graph in
  match current_error with
  | None -> None
  | Some error_msg ->
    let red = Bonsai_term_color_scheme.color ~flavor Red in
    let crust = Bonsai_term_color_scheme.color ~flavor Crust in
    let prefix = " ⚠ " in
    let suffix = " [Space to dismiss] " in
    let prefix_len = String.length prefix in
    let suffix_len = String.length suffix in
    let available_width = max 0 (width - prefix_len - suffix_len) in
    let truncated_msg =
      if String.length error_msg <= available_width
      then error_msg
      else String.prefix error_msg (max 0 (available_width - 3)) ^ "..."
    in
    let msg_len = String.length truncated_msg in
    let padding = max 0 (available_width - msg_len) in
    let error_view =
      View.hcat
        [ View.text ~attrs:[ Attr.fg crust; Attr.bg red; Attr.bold ] prefix
        ; View.text ~attrs:[ Attr.fg crust; Attr.bg red ] truncated_msg
        ; View.text ~attrs:[ Attr.bg red ] (String.make padding ' ')
        ; View.text ~attrs:[ Attr.fg crust; Attr.bg red ] suffix
        ]
    in
    Some error_view
;;

let component
  ~(title : string)
  ~(debug_messages : string list Bonsai.t)
  ~(debug : (string -> unit) Bonsai.t)
  ~(current_error : string option Bonsai.t)
  ~(dismiss_error : (unit -> unit) Bonsai.t)
  ~dimensions
  ~on_event
  ~(exit : (unit -> unit Effect.t) Bonsai.t)
  ~start_fullscreen
  (local_ graph)
  =
  (* Wrap output toggle state *)
  let wrap_output, set_wrap_output = Bonsai.state false graph in
  (* Fullscreen mode hides the tree. Derived [show_tree] makes the tree visible whenever
     focus is on Tree, even in fullscreen. *)
  let fullscreen, set_fullscreen = Bonsai.state start_fullscreen graph in
  (* Focus mode state - Output when in fullscreen, Tree otherwise *)
  let focus_mode, set_focus_mode =
    let initial_focus =
      if start_fullscreen then Focus_mode.Output Logs else Focus_mode.Tree
    in
    Bonsai.state initial_focus graph
  in
  let show_tree =
    let%arr fullscreen and focus_mode in
    match focus_mode with
    | Tree -> true
    | _ -> not fullscreen
  in
  (* When set, the selected interactive command was just started via Enter. Once its tmux
     session becomes available we auto-focus it. *)
  let pending_interactive_focus, set_pending_interactive_focus =
    Bonsai.state false graph
  in
  (* Process tree visibility toggle state *)
  let show_process_tree, set_show_process_tree = Bonsai.state false graph in
  (* Log parsing/highlighting toggle state *)
  let parse_logs, set_parse_logs = Bonsai.state true graph in
  (* Timestamp display mode state *)
  let timestamp_mode, set_timestamp_mode =
    Bonsai.state Parsed_log_line.Timestamp_mode.default graph
  in
  (* Pretty mode state *)
  let message_mode, set_message_mode =
    Bonsai.state Parsed_log_line.Message_mode.default graph
  in
  let output_option_handler =
    let%arr wrap_output
    and set_wrap_output
    and parse_logs
    and set_parse_logs
    and timestamp_mode
    and set_timestamp_mode
    and message_mode
    and set_message_mode in
    handle_output_option_key
      ~wrap_output
      ~set_wrap_output
      ~parse_logs
      ~set_parse_logs
      ~timestamp_mode
      ~set_timestamp_mode
      ~message_mode
      ~set_message_mode
  in
  (* Whether the debug panel is visible, derived from focus_mode *)
  let show_debug =
    let%arr focus_mode in
    match focus_mode with
    | Focus_mode.Debug _ -> true
    | Tree | Output _ | Filter_input _ | Modal _ -> false
  in
  (* Whether the filter input textbox has focus, derived from focus_mode *)
  let filter_input_active =
    let%arr focus_mode in
    match focus_mode with
    | Focus_mode.Filter_input _ -> true
    | Tree | Output _ | Debug _ | Modal _ -> false
  in
  let filter_input =
    let cursor_attrs =
      let%arr flavor = Bonsai_term_color_scheme.flavor graph in
      let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
      [ Attr.bg mauve ]
    in
    let text_attrs =
      let%arr flavor = Bonsai_term_color_scheme.flavor graph in
      let surface0 = Bonsai_term_color_scheme.color ~flavor Surface0 in
      [ Attr.bg surface0 ]
    in
    Filter_input.component ~cursor_attrs ~text_attrs ~is_focused:filter_input_active graph
  in
  let filter_text =
    let%arr { Filter_input.text; _ } = filter_input in
    text
  in
  let set_filter_text =
    let%arr { Filter_input.set_text; _ } = filter_input in
    set_text
  in
  (* Original index to re-select after clearing filter *)
  let pending_reselect_index, set_pending_reselect_index =
    Bonsai.state (None : int option) graph
  in
  (* Toast notification component *)
  let toaster = Toaster.create graph in
  (* App state machine - handles process and group lifecycle. Entries are created on
     demand when actions are dispatched. *)
  let states, inject =
    Bonsai.state_machine
      ~default_model:
        (Map.empty (module Command_id), Map.empty (module Command_id), 0 (* next_seq *))
      ~apply_action:(fun _ (process_states, group_states, next_seq) action ->
        match action with
        | Action.Start_requested id ->
          let process_states =
            Map.update process_states id ~f:(fun prev ->
              let state = Option.value_or_thunk prev ~default:Process_state.create in
              match state.status with
              | Not_started | Exited _ ->
                { state with status = Starting; tmux_session_id = None }
              | Starting | Running | Stopping -> state)
          in
          process_states, group_states, next_seq
        | Action.Start_process (id, pid) ->
          let process_states =
            Map.update process_states id ~f:(fun prev ->
              let state = Option.value_or_thunk prev ~default:Process_state.create in
              { state with
                Process_state.status = Running
              ; pid = Some pid
              ; started_at = Some (Time_ns.now ())
              ; exited_at = None
              })
          in
          process_states, group_states, next_seq
        | Action.Add_output (id, lines) ->
          let process_states, next_seq =
            let state =
              Map.find process_states id
              |> Option.value_or_thunk ~default:Process_state.create
            in
            let state, next_seq = Process_state.add_lines state ~next_seq lines in
            Map.set process_states ~key:id ~data:state, next_seq
          in
          process_states, group_states, next_seq
        | Action.Process_exited (id, exit_status) ->
          let now = Time_ns.now () in
          let process_states =
            Map.update process_states id ~f:(fun state ->
              let state = Option.value_or_thunk state ~default:Process_state.create in
              let exit_info =
                match exit_status with
                | Ok () -> "exited (0)"
                | Error (`Exit_non_zero code) -> [%string "exited (%{code#Int})"]
                | Error (`Signal signal) ->
                  [%string "exited (%{Signal.to_string signal})"]
              in
              let pid_part =
                match state.pid with
                | Some pid -> [%string "process %{pid#Pid} %{exit_info}"]
                | None -> exit_info
              in
              let time_part =
                if am_running_test
                then ""
                else (
                  let zone = force Time_ns.Zone.local in
                  let ofday = Time_ns.to_ofday now ~zone in
                  [%string " at %{Time_ns.Ofday.to_sec_string ofday}"])
              in
              let message = [%string "%{pid_part}%{time_part}"] in
              let state = Process_state.add_separator state ~seq:next_seq ~message in
              { state with
                status = Exited { exit_status; message }
              ; exited_at = Some now
              })
          in
          process_states, group_states, next_seq + 1
        | Action.Update_stats (id, stats) ->
          let process_states =
            Map.update process_states id ~f:(fun state ->
              let state = Option.value_or_thunk state ~default:Process_state.create in
              Process_state.add_stats state stats)
          in
          process_states, group_states, next_seq
        | Action.Kill_requested id ->
          let process_states =
            Map.update process_states id ~f:(fun state ->
              let state = Option.value_or_thunk state ~default:Process_state.create in
              match state.status with
              | Running -> { state with status = Stopping }
              | Not_started | Starting | Stopping | Exited _ -> state)
          in
          process_states, group_states, next_seq
        | Action.Set_tmux_session (id, session_id) ->
          let process_states =
            Map.update process_states id ~f:(fun state ->
              let state = Option.value_or_thunk state ~default:Process_state.create in
              { state with tmux_session_id = Some session_id })
          in
          process_states, group_states, next_seq
        | Action.Group_output (id, line) ->
          let group_states =
            Map.update group_states id ~f:(fun state ->
              let state = Option.value_or_thunk state ~default:Group_state.create in
              Group_state.add_line state line)
          in
          process_states, group_states, next_seq
        | Action.Group_finished (id, status) ->
          let group_states =
            Map.update group_states id ~f:(fun state ->
              let state = Option.value_or_thunk state ~default:Group_state.create in
              { state with status })
          in
          process_states, group_states, next_seq)
      graph
  in
  let process_states =
    let%arr process_states, _, _ = states in
    process_states
  in
  let group_states =
    let%arr _, group_states, _ = states in
    group_states
  in
  (* Build render_item function that captures process_states and group_states *)
  let render_item =
    let%arr process_states
    and group_states
    and flavor = Bonsai_term_color_scheme.flavor graph in
    make_render_item ~title ~process_states ~group_states ~flavor
  in
  (* Calculate tree dimensions (left half of content area) *)
  let tree_dimensions =
    let%arr { Dimensions.height; width } = dimensions in
    (* height - 1 for instructions header, width / 2 for left panel *)
    { Dimensions.height = height - 1; width = max 1 (width / 2) }
  in
  (* Navigable tree component - owns tree state and handles rendering *)
  let ~view:tree_view, ~model:nav_model, ~inject:nav_inject =
    let highlight =
      let%arr { highlight; _ } = Styled.create graph in
      highlight
    in
    (* Start with a Leaf placeholder that will be replaced when entries are registered.
       Replace_leaf will turn this into a Branch with commands as direct children. *)
    let initial_tree =
      Navigable_tree.Node.Leaf (Tree_item.Initializing_group { id = []; bash_code = "" })
    in
    Navigable_tree.component
      ~initial_tree
      ~render_item
      ~highlight
      ~dimensions:tree_dimensions
      ~strip_depth:1
      graph
  in
  (* Selected output line state - scoped per tree selection *)
  let selected_tree_index =
    let%arr nav_model in
    Navigable_tree.Model.selected_index nav_model
  in
  let%sub selected_line, set_selected_line =
    Bonsai.scope_model (module Int) ~on:selected_tree_index graph ~for_:(fun graph ->
      let selected_line, set_selected_line = Bonsai.state None graph in
      Bonsai.both selected_line set_selected_line)
  in
  (* Auto-unfocus when the interactive session dies *)
  let selected_interactive_is_alive =
    let%arr nav_model and process_states in
    match selected_item ~nav_model with
    | Some (Command { id; is_interactive = true; _ }) ->
      (match Map.find process_states id with
       | Some state ->
         Option.is_some state.tmux_session_id && Process_status.is_alive state.status
       | None -> false)
    | _ -> false
  in
  let () =
    Bonsai.Edge.on_change
      ~trigger:`After_display
      ~equal:[%equal: bool]
      selected_interactive_is_alive
      ~callback:
        (let%arr focus_mode
         and set_focus_mode
         and pending_interactive_focus
         and set_pending_interactive_focus in
         fun is_alive ->
           if is_alive && pending_interactive_focus
           then
             Effect.Many
               [ set_pending_interactive_focus false
               ; set_focus_mode (Focus_mode.Output Interactive)
               ]
           else if (not is_alive)
                   && [%equal: Focus_mode.t] focus_mode (Output Interactive)
           then set_focus_mode Focus_mode.Tree
           else if not is_alive
           then set_pending_interactive_focus false
           else Effect.Ignore)
      graph
  in
  let width =
    let%arr { Dimensions.width; _ } = dimensions in
    width
  in
  let instructions = render_instructions ~width graph in
  let content_height =
    let%arr { Dimensions.height; _ } = dimensions in
    height - 1
  in
  (* Calculate preview width based on actual tree width *)
  let preview_dimensions =
    let%arr { Dimensions.height; width } = dimensions
    and tree_view
    and show_tree in
    let preview_width =
      if show_tree
      then (
        let tree_width = View.width tree_view in
        (* total width - left padding (1) - tree - divider padding (1) - divider (1) -
           preview padding (1) *)
        max 1 (width - 1 - tree_width - 1 - 1 - 1))
      else (* full width minus left padding *)
        max 1 (width - 1)
    in
    { Dimensions.height = height - 1; width = preview_width }
  in
  (* Parse filter once when filter text changes, not on every render *)
  let filter_tokens =
    let%arr filter_text in
    Filter_query.parse_tokens filter_text
  in
  let selected_is_interactive =
    let%arr nav_model in
    match selected_item ~nav_model with
    | Some (Command { is_interactive = true; _ }) -> true
    | _ -> false
  in
  let%sub ( preview_view
          , output_handler
          , interactive_handler
          , scroll_inject
          , output_result
          , (_ : int) )
    =
    render_preview
      ~nav_model
      ~process_states
      ~group_states
      ~wrap_output
      ~parse_logs
      ~timestamp_mode
      ~message_mode
      ~filter_text
      ~filter_tokens
      ~filter_textbox_view:
        (let%arr { Filter_input.view; _ } = filter_input in
         view)
      ~filter_input_active
      ~show_process_tree
      ~focus_mode
      ~selected_line
      ~set_selected_line
      ~show_tree
      ~dimensions:preview_dimensions
      graph
  in
  (* Re-select line after filter is cleared: use the stored original index to restore
     selection and scroll to center it *)
  let () =
    Bonsai.Edge.on_change
      ~trigger:`After_display
      ~equal:[%equal: int option * string]
      (Bonsai.both pending_reselect_index filter_text)
      ~callback:
        (let%arr set_pending_reselect_index
         and set_selected_line
         and scroll_inject
         and output_result
         and preview_dimensions in
         fun (pending_idx, filter_text) ->
           match pending_idx with
           | None -> Effect.Ignore
           | Some target_idx ->
             (* Only act when filter is cleared (empty) and we have a pending index *)
             if not (String.is_empty filter_text)
             then Effect.Ignore
             else (
               (* Set selection to the original index and scroll to center it *)
               match
                 Iarray.get_opt (Output_render_result.lines output_result) target_idx
               with
               | None ->
                 (* Index out of bounds, just clear pending *)
                 set_pending_reselect_index None
               | Some line ->
                 let viewport_height = preview_dimensions.Dimensions.height in
                 (* Calculate scroll position to center the line *)
                 let center_offset =
                   max 0 (Output_line.start_row line - (viewport_height / 2))
                 in
                 Effect.Many
                   [ set_selected_line (Some target_idx)
                   ; scroll_inject
                       (Bonsai_term_scroller.Action.Scroll_to
                          { top = center_offset
                          ; bottom = center_offset + Output_line.rendered_height line - 1
                          })
                   ; set_pending_reselect_index None
                   ]))
      graph
  in
  (* Copy to clipboard effect using bonsai_term_clipboard library *)
  let clipboard_copy = Bonsai_term_clipboard.copy_to_clipboard graph in
  let copy_to_clipboard =
    let%arr clipboard_copy
    and debug
    and show_toast = toaster.show
    and flavor = Bonsai_term_color_scheme.flavor graph in
    fun text ->
      let%bind.Effect result =
        clipboard_copy Bonsai_term_clipboard.Selection.Clipboard text
      in
      match result with
      | Ok () ->
        let green = Bonsai_term_color_scheme.color ~flavor Green in
        show_toast ~bg:green "Copied!"
      | Error err ->
        debug [%string "Failed to copy: %{Error.to_string_hum err}"];
        Effect.Ignore
  in
  (* Keyboard handler - created after scroll_inject is available *)
  let global_handler =
    global_handler
      ~nav_model
      ~process_states
      ~nav_inject
      ~inject
      ~output_option_handler
      ~show_tree
      ~set_fullscreen
      ~show_process_tree
      ~set_show_process_tree
      ~focus_mode
      ~set_focus_mode
      ~output_result
      ~selected_line
      ~set_selected_line
      ~set_pending_interactive_focus
      ~copy_to_clipboard
      ~on_event
      graph
  in
  let divider = vertical_divider ~height:content_height ~focus_mode graph in
  (* Help modal component *)
  let%sub help_modal_view, help_modal_handler = render_help_modal ~dimensions graph in
  (* Output menu component *)
  let output_menu_view =
    render_output_menu
      ~dimensions
      ~wrap_output
      ~parse_logs
      ~timestamp_mode
      ~message_mode
      graph
  in
  (* Command search modal component *)
  let%sub command_search_view, command_search_handler, command_search_reset =
    render_command_search_modal
      ~title
      ~nav_model
      ~nav_inject
      ~set_focus_mode
      ~dimensions
      graph
  in
  (* Debug panel component - takes up bottom third of screen *)
  let debug_dimensions =
    let%arr { Dimensions.height; width } = dimensions in
    { Dimensions.height = max 3 (height / 3); width }
  in
  let%sub debug_panel_view, debug_panel_handler =
    render_debug_panel ~debug_messages ~dimensions:debug_dimensions graph
  in
  (* Error bar at the bottom *)
  let width =
    let%arr { Dimensions.width; _ } = dimensions in
    width
  in
  let error_bar = render_error_bar ~current_error ~width graph in
  let view =
    let%arr backdrop = backdrop ~dimensions graph
    and instructions
    and tree_view
    and preview_view
    and divider
    and show_tree
    and focus_mode
    and show_debug
    and toast_view = toaster.view
    and help_modal_view
    and output_menu_view
    and command_search_view
    and debug_panel_view
    and error_bar
    and styled = Styled.create graph
    and { Dimensions.height; width } = dimensions in
    let main_content =
      if show_tree
      then View.hcat [ tree_view; View.pad ~l:1 divider; View.pad ~l:1 preview_view ]
      else preview_view
    in
    let full_view = View.vcat [ instructions; main_content ] in
    let base_view = View.zcat [ View.pad ~l:1 full_view; backdrop ] in
    (* Error bar overlays the bottom of the screen *)
    let with_error =
      match error_bar with
      | None -> base_view
      | Some error_view ->
        let top_padding = max 0 (height - 1) in
        View.zcat [ View.pad ~t:top_padding error_view; base_view ]
    in
    (* Debug panel overlays the bottom of the screen *)
    let with_debug =
      if show_debug
      then (
        let debug_panel_height = View.height debug_panel_view in
        let top_padding = max 0 (height - debug_panel_height) in
        View.zcat [ View.pad ~t:top_padding debug_panel_view; with_error ])
      else with_error
    in
    (* Toast notification at top right (View.none when not visible) *)
    let toast_width = View.width toast_view in
    let toast_left_padding = max 0 (width - toast_width) in
    let with_toast =
      View.zcat [ View.pad ~l:toast_left_padding toast_view; with_debug ]
    in
    let view =
      match focus_mode with
      | Modal { which = `Help; _ } -> View.zcat [ help_modal_view; with_toast ]
      | Modal { which = `Output_menu; _ } -> View.zcat [ output_menu_view; with_toast ]
      | Modal { which = `Command_search; _ } ->
        View.zcat [ command_search_view; with_toast ]
      | Tree | Output _ | Filter_input _ | Debug _ -> with_toast
    in
    View.with_colors view ~fg:styled.fg ~bg:styled.bg
  in
  let filter_input_handler =
    let%arr { Filter_input.handler; _ } = filter_input in
    handler
  in
  let handler =
    let%arr global_handler
    and output_handler
    and interactive_handler
    and help_modal_handler
    and command_search_handler
    and command_search_reset
    and debug_panel_handler
    and filter_text
    and set_filter_text
    and filter_input_handler
    and current_error
    and dismiss_error
    and selected_line
    and set_selected_line
    and output_result
    and set_pending_reselect_index
    and focus_mode
    and set_focus_mode
    and exit
    and selected_is_interactive
    and selected_interactive_is_alive
    and output_option_handler
    and (_ : string -> unit) = debug in
    (* This top-level handler will:

       - handle special escape sequences
       - handle keys that change focus
       - dispatch events to the handler with focus *)
    fun (event : Event.t) : unit Effect.t ->
      match event with
      (* Ctrl+C exits unless an interactive terminal is focused *)
      | Key_press { key = ASCII ('C' | 'c'); mods = [ Ctrl ] }
        when not ([%equal: Focus_mode.t] focus_mode (Output Interactive)) -> exit ()
      | _ ->
        (match focus_mode with
         | Filter_input { prev } ->
           (match event with
            | Key_press { key = Escape; mods = [] } ->
              Effect.Many [ set_filter_text ""; set_focus_mode prev ]
            | Key_press { key = Enter; mods = [] } ->
              let lines = Output_render_result.lines output_result in
              let last_selectable =
                let rec loop i =
                  match Iarray.get_opt lines i with
                  | None -> None
                  | Some line ->
                    if not (Output_line.is_separator line) then Some i else loop (i - 1)
                in
                loop (Iarray.length lines - 1)
              in
              let select_effect =
                match last_selectable with
                | Some idx -> set_selected_line (Some idx)
                | None -> Effect.Ignore
              in
              Effect.Many [ set_focus_mode (Focus_mode.Output Logs); select_effect ]
            | _ -> filter_input_handler event)
         | Debug { prev } ->
           (match event with
            | Key_press { key = ASCII ('`' | '~'); mods = [] }
            | Key_press { key = Escape; mods = [] } -> set_focus_mode prev
            | _ -> debug_panel_handler event)
         | Modal { which = `Help; prev } ->
           (match event with
            | Key_press { key = Function 1; mods = [] }
            | Key_press { key = ASCII '?'; mods = [] }
            | Key_press { key = Escape; mods = [] } -> set_focus_mode prev
            | _ -> help_modal_handler event)
         | Modal { which = `Output_menu; prev } ->
           (match event with
            | Key_press { key = Function 1; mods = [] }
            | Key_press { key = ASCII '?'; mods = [] } ->
              set_focus_mode (Modal { which = `Help; prev })
            | Key_press { key = Escape; mods = [] }
            | Key_press { key = ASCII 'o'; mods = [] } -> set_focus_mode prev
            | _ -> output_option_handler event)
         | Modal { which = `Command_search; prev } ->
           (match event with
            | Key_press { key = Escape; mods = [] }
            | Key_press { key = ASCII ('k' | 'K'); mods = [ Ctrl ] } ->
              Effect.Many [ command_search_reset; set_focus_mode prev ]
            | _ -> command_search_handler event)
         | Output Interactive ->
           (* All events route to the embedded terminal, except Ctrl-Q which unfocuses. *)
           (match event with
            | Key_press { key = ASCII ('Q' | 'q'); mods = [ Ctrl ] } ->
              set_focus_mode Focus_mode.Tree
            | _ -> output_handler event)
         | (Tree | Output Logs) as prev ->
           (match event with
            | Key_press { key = Function 1; mods = [] }
            | Key_press { key = ASCII '?'; mods = [] } ->
              set_focus_mode (Modal { which = `Help; prev })
            | Key_press { key = ASCII 'o'; mods = [] } ->
              set_focus_mode (Modal { which = `Output_menu; prev })
            | Key_press { key = ASCII ('k' | 'K'); mods = [ Ctrl ] } ->
              set_focus_mode (Modal { which = `Command_search; prev })
            | Key_press { key = ASCII ('`' | '~'); mods = [] } ->
              set_focus_mode (Focus_mode.Debug { prev })
            | Key_press { key = ASCII ' '; mods = [] } when Option.is_some current_error
              ->
              dismiss_error ();
              Effect.Ignore
            | Key_press { key = ASCII '/'; mods = [] } when not selected_is_interactive ->
              set_focus_mode (Focus_mode.Filter_input { prev })
            | Key_press { key = Escape; mods = [] } when not (String.is_empty filter_text)
              ->
              let save_selection_effect =
                match selected_line with
                | None -> Effect.Ignore
                | Some idx ->
                  (match
                     Iarray.get_opt (Output_render_result.lines output_result) idx
                   with
                   | Some line ->
                     set_pending_reselect_index (Some (Output_line.original_index line))
                   | None -> Effect.Ignore)
              in
              Effect.Many [ save_selection_effect; set_filter_text "" ]
            | Key_press { key = Escape; mods = [] } when Option.is_some selected_line ->
              set_selected_line None
            | Key_press { key = Escape; mods = [] } when Focus_mode.is_output focus_mode
              -> set_focus_mode Focus_mode.Tree
            (* Pressing Ctrl-Q when the tree has focus sends a literal Ctrl-Q to the
               interactive pane and moves focus into it, so that pressing Ctrl-Q Ctrl-Q
               when an interactive pane has focus sends a literal Ctrl-Q to the running
               process and doesn't lose its focus. *)
            | Key_press { key = ASCII ('Q' | 'q'); mods = [ Ctrl ] }
              when selected_interactive_is_alive && Focus_mode.is_tree focus_mode ->
              Effect.Many
                [ interactive_handler event
                ; set_focus_mode (Focus_mode.Output Interactive)
                ]
            | _ ->
              let%bind.Effect () = global_handler event in
              output_handler event))
  in
  let%arr view and handler and inject and nav_inject and nav_model in
  ~view, ~handler, ~inject, ~nav_inject, ~nav_model
;;

open! Core
open Bonsai_term
open Bonsai.Let_syntax

module Item_view = struct
  type t =
    { icon : View.t
    ; label : View.t
    }
end

type 'a t =
  { view : View.t
  ; handler : Event.t -> unit Effect.t
  ; reset : unit Effect.t
  }

let component
  (type a)
  ~(items : (string * a) list Bonsai.t)
  ~(render_item : (string -> a -> is_selected:bool -> Item_view.t) Bonsai.t)
  ~(on_select : (a -> unit Effect.t) Bonsai.t)
  ~(dimensions : Dimensions.t Bonsai.t)
  (local_ graph)
  =
  (* Text input for the search query *)
  let is_focused = Bonsai.return true in
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
  let search_input = Filter_input.component ~cursor_attrs ~text_attrs ~is_focused graph in
  let search_text =
    let%arr { Filter_input.text; _ } = search_input in
    text
  in
  (* Selected index within filtered results *)
  let selected_idx, set_selected_idx = Bonsai.state 0 graph in
  (* Reset selection when search text changes *)
  let () =
    Bonsai.Edge.on_change
      ~trigger:`After_display
      ~equal:[%equal: string]
      search_text
      ~callback:
        (let%arr set_selected_idx in
         fun (_ : string) -> set_selected_idx 0)
      graph
  in
  (* Filter items by substring match (case-insensitive) *)
  let filtered_items =
    let%arr items and search_text in
    let query = String.lowercase search_text in
    if String.is_empty query
    then items
    else
      List.filter items ~f:(fun (name, _) ->
        String.is_substring (String.lowercase name) ~substring:query)
  in
  (* Clamp selected index to valid range *)
  let clamped_selected_idx =
    let%arr selected_idx and filtered_items in
    let len = List.length filtered_items in
    if len = 0 then 0 else Int.min selected_idx (len - 1)
  in
  let view =
    let%arr flavor = Bonsai_term_color_scheme.flavor graph
    and { Dimensions.height; width } = dimensions
    and { Filter_input.view = search_input_view; _ } = search_input
    and filtered_items
    and clamped_selected_idx
    and render_item in
    let mauve = Bonsai_term_color_scheme.color ~flavor Mauve in
    let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
    let surface0 = Bonsai_term_color_scheme.color ~flavor Surface0 in
    let lavender = Bonsai_term_color_scheme.color ~flavor Lavender in
    let selection_bg = surface0 in
    let modal_bg = surface0 in
    let modal_width = Int.min (width - 6) 60 |> Int.max 30 in
    let modal_height = Int.min (height - 4) 20 |> Int.max 5 in
    let content_width = modal_width - 2 in
    (* Search input line *)
    let search_prefix =
      View.text ~attrs:[ Attr.fg mauve; Attr.bold; Attr.bg surface0 ] "❯ "
    in
    let search_line = View.hcat [ search_prefix; search_input_view ] in
    let search_line_width = View.width search_line in
    let search_padding = Int.max 0 (content_width - search_line_width) in
    let search_line_padded =
      View.hcat
        [ search_line
        ; View.text ~attrs:[ Attr.bg surface0 ] (String.make search_padding ' ')
        ]
    in
    (* Separator *)
    let separator =
      View.text
        ~attrs:[ Attr.fg subtext_color ]
        (String.concat (List.init content_width ~f:(fun _ -> "─")))
    in
    (* Results list *)
    let max_visible_results = modal_height - 4 in
    let num_results = List.length filtered_items in
    (* Calculate scroll window to keep selection visible *)
    let scroll_offset =
      if clamped_selected_idx < max_visible_results / 2
      then 0
      else if clamped_selected_idx > num_results - (max_visible_results / 2)
      then Int.max 0 (num_results - max_visible_results)
      else Int.max 0 (clamped_selected_idx - (max_visible_results / 2))
    in
    let visible_results =
      List.drop filtered_items scroll_offset |> List.take _ max_visible_results
    in
    let result_views =
      if List.is_empty visible_results
      then [ View.text ~attrs:[ Attr.fg subtext_color ] "(no matching items)" ]
      else
        List.mapi visible_results ~f:(fun display_idx (name, value) ->
          let actual_idx = display_idx + scroll_offset in
          let is_selected = actual_idx = clamped_selected_idx in
          let { Item_view.icon; label } = render_item name value ~is_selected in
          let line = View.hcat [ icon; label ] in
          let line_width = View.width line in
          let padding = Int.max 0 (content_width - line_width) in
          let padded_line = View.hcat [ line; View.text (String.make padding ' ') ] in
          if is_selected
          then View.with_colors' ~bg:selection_bg padded_line
          else padded_line)
    in
    (* Count display *)
    let count_text =
      if num_results = List.length filtered_items
      then sprintf "%d items" num_results
      else sprintf "%d / %d" num_results (List.length filtered_items)
    in
    let count_view = View.text ~attrs:[ Attr.fg subtext_color ] count_text in
    let content =
      View.vcat ([ search_line_padded; separator ] @ result_views @ [ count_view ])
    in
    let padded_content = View.pad ~l:1 ~r:1 content in
    let bordered_content =
      Bonsai_term_border_box.view
        ~line_type:Round_corners
        ~attrs:[ Attr.fg lavender ]
        padded_content
    in
    let bw = View.width bordered_content in
    let bh = View.height bordered_content in
    let top_pad = Int.max 0 ((height - bh) / 2) in
    let left_pad = Int.max 0 ((width - bw) / 2) in
    let background = View.rectangle ~height:bh ~width:bw () in
    let modal_with_bg = View.zcat [ bordered_content; background ] in
    View.pad ~t:top_pad ~l:left_pad modal_with_bg |> View.with_colors' ~bg:modal_bg
  in
  let handler =
    let%arr { Filter_input.handler = input_handler; set_text; _ } = search_input
    and set_selected_idx
    and clamped_selected_idx
    and filtered_items
    and on_select in
    let reset = Effect.Many [ set_text ""; set_selected_idx 0 ] in
    let handler (event : Event.t) =
      match event with
      | Key_press { key = Arrow `Up; mods = [] }
      | Key_press { key = ASCII ('p' | 'P'); mods = [ Ctrl ] } ->
        let new_idx = Int.max 0 (clamped_selected_idx - 1) in
        set_selected_idx new_idx
      | Key_press { key = Arrow `Down; mods = [] }
      | Key_press { key = ASCII ('n' | 'N'); mods = [ Ctrl ] } ->
        let len = List.length filtered_items in
        let new_idx = Int.min (len - 1) (clamped_selected_idx + 1) in
        set_selected_idx new_idx
      | Key_press { key = Enter; mods = [] } ->
        (match List.nth filtered_items clamped_selected_idx with
         | None -> Effect.Ignore
         | Some (_, value) -> Effect.Many [ reset; on_select value ])
      | _ -> input_handler event
    in
    handler, reset
  in
  let%arr view and handler in
  let handler, reset = handler in
  { view; handler; reset }
;;

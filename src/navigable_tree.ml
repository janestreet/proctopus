open! Core
open Bonsai.Let_syntax

(* A generic navigable tree component that renders all nodes visible at once with
   tree-style indentation, and allows navigating through all items. *)

(* A tree node can be a branch (has children) or a leaf *)
module Node = struct
  type 'a t =
    | Branch of
        { value : 'a
        ; children : 'a t list
        ; expanded : bool
        }
    | Leaf of 'a
  [@@deriving sexp, equal]

  let value = function
    | Branch { value; _ } -> value
    | Leaf value -> value
  ;;

  let is_branch = function
    | Branch _ -> true
    | Leaf _ -> false
  ;;

  let children = function
    | Branch { children; expanded; _ } -> if expanded then children else []
    | Leaf _ -> []
  ;;

  let is_expanded = function
    | Branch { expanded; _ } -> expanded
    | Leaf _ -> false
  ;;
end

(* A tree is a single root node. The root is typically a Branch containing children. *)
type 'a t = 'a Node.t [@@deriving sexp, equal]

(* A flattened row for rendering, with depth information *)
module Row = struct
  type 'a t =
    { value : 'a
    ; depth : int
    ; is_branch : bool
    ; is_expanded : bool
    ; is_last_sibling : bool
    ; ancestor_is_last : bool list (* from root to parent, whether each was last *)
    }
end

(* Flatten the tree into a list of rows with depth info for rendering. The root node is
   included at index 0 with depth 0, and its children are displayed starting at depth 1. *)
let flatten (tree : 'a t) : 'a Row.t list =
  let rec go_children ~depth ~ancestor_is_last nodes =
    let num_nodes = List.length nodes in
    List.concat_mapi nodes ~f:(fun i node ->
      let is_last_sibling = i = num_nodes - 1 in
      let row =
        { Row.value = Node.value node
        ; depth
        ; is_branch = Node.is_branch node
        ; is_expanded = Node.is_expanded node
        ; is_last_sibling
        ; ancestor_is_last
        }
      in
      let children_rows =
        go_children
          ~depth:(depth + 1)
          ~ancestor_is_last:(ancestor_is_last @ [ is_last_sibling ])
          (Node.children node)
      in
      row :: children_rows)
  in
  go_children ~depth:0 ~ancestor_is_last:[] [ tree ]
;;

(* Toggle expansion of a branch at the given flat index. Index 0 is the root node. *)
let toggle_at_index (tree : 'a t) ~(index : int) : 'a t =
  let current_index = ref 0 in
  let rec go_node node =
    let this_index = !current_index in
    incr current_index;
    if this_index = index
    then (
      match node with
      | Node.Branch { value; children; expanded } ->
        if expanded
        then Node.Branch { value; children; expanded = false }
        else (
          let new_node = Node.Branch { value; children; expanded = true } in
          ignore (go_children children : 'a Node.t list);
          new_node)
      | Node.Leaf _ -> node)
    else (
      match node with
      | Node.Branch { value; children; expanded } ->
        if expanded
        then (
          let new_children = go_children children in
          Node.Branch { value; children = new_children; expanded })
        else node
      | Node.Leaf _ -> node)
  and go_children nodes = List.map nodes ~f:go_node in
  (* Start from the root itself (index 0) *)
  go_node tree
;;

let count_visible (tree : 'a t) : int = List.length (flatten tree)

(* Render the tree prefix characters (lines and corners) *)
let render_prefix ~(row : _ Row.t) ~strip_depth : string =
  if row.depth <= strip_depth
  then ""
  else (
    let prefix_parts =
      List.drop row.ancestor_is_last (strip_depth + 1)
      |> List.map ~f:(fun is_last -> if is_last then "   " else "│  ")
    in
    let final =
      (* Connection column: draw corner based on whether THIS node is last *)
      if row.is_last_sibling then "└─ " else "├─ "
    in
    String.concat (prefix_parts @ [ final ]))
;;

(* Actions for the tree component *)
module Action = struct
  type 'a t =
    | Move_up
    | Move_down
    | Toggle_expand
    | Select_parent
    | Expand_or_select_next_at_lower_depth
    | Replace_leaf of ('a -> 'a Node.t option)
    (** Replace matching leaf nodes (including root) with the returned node *)
    | Select_path of ('a -> bool)
    (** Select the first visible node whose value matches the predicate *)
  [@@deriving sexp_of]
end

(* Model for the navigable tree component *)
(* Collect all leaf values from a node recursively (including collapsed children) *)
let rec collect_all_leaves_from_node (node : 'a Node.t) : 'a list =
  match node with
  | Node.Leaf value -> [ value ]
  | Node.Branch { children; _ } ->
    List.concat_map children ~f:collect_all_leaves_from_node
;;

(* Collect all leaf values with their relative paths from a node. The path is built up as
   we traverse, using the provided [get_name] function to extract the name from each
   node's value. *)
let collect_all_leaves_with_path_from_node (node : 'a Node.t) ~(get_name : 'a -> 'name)
  : ('a * path:'name list) list
  =
  let rec go ~path node =
    match node with
    | Node.Leaf value -> [ value, ~path:(List.rev path) ]
    | Node.Branch { children; _ } ->
      List.concat_map children ~f:(fun child ->
        let child_name = get_name (Node.value child) in
        go ~path:(child_name :: path) child)
  in
  go ~path:[] node
;;

(* Find the node at a given visible index. Index 0 is the root node. *)
let find_node_at_index (tree : 'a t) ~(index : int) : 'a Node.t option =
  let rec walk nodes current_index =
    match nodes with
    | [] -> None, current_index
    | node :: rest ->
      if current_index = index
      then Some node, current_index
      else (
        let children = Node.children node in
        let result, after_node = walk children (current_index + 1) in
        match result with
        | Some _ -> result, after_node
        | None -> walk rest after_node)
  in
  fst (walk [ tree ] 0)
;;

module Model = struct
  type 'a t =
    { tree : 'a Node.t
    ; selected_index : int option (* None = implicit default, Some i = explicit *)
    ; strip_depth : int
    }
  [@@deriving sexp, equal]

  let create tree ~strip_depth = { tree; selected_index = None; strip_depth }
  let visible_rows t = flatten t.tree
  let num_visible t = count_visible t.tree

  (* The effective selected index, defaulting to 0 if implicit *)
  let selected_index t = Option.value t.selected_index ~default:0
  let is_selection_explicit t = Option.is_some t.selected_index

  let selected_row t =
    let rows = visible_rows t in
    List.nth rows (selected_index t)
  ;;

  (* Get all leaf values under the currently selected node (recursively). If the selected
     node is a leaf, returns just that leaf's value. If the selected node is a branch,
     returns all leaf values in its subtree. *)
  let selected_descendants t =
    match find_node_at_index t.tree ~index:(selected_index t) with
    | None -> []
    | Some node -> collect_all_leaves_from_node node
  ;;

  (* Like [selected_descendants], but also returns the relative path from the selected
     node to each descendant. The path is a list of names, computed using [get_name]. *)
  let selected_descendants' t ~get_name =
    match find_node_at_index t.tree ~index:(selected_index t) with
    | None -> []
    | Some node -> collect_all_leaves_with_path_from_node node ~get_name
  ;;

  let visible_values t = visible_rows t |> List.map ~f:(fun (row : _ Row.t) -> row.value)
end

(* Apply an action to the model. Navigation actions make the selection explicit. *)
let apply_action (model : 'a Model.t) (action : 'a Action.t) : 'a Model.t =
  let current_index = Model.selected_index model in
  match action with
  | Move_down ->
    let num_visible = Model.num_visible model in
    if num_visible = 0 || current_index >= num_visible - 1
    then model
    else { model with selected_index = Some (current_index + 1) }
  | Move_up ->
    if current_index <= 0
    then model
    else { model with selected_index = Some (current_index - 1) }
  | Toggle_expand ->
    let tree = toggle_at_index model.tree ~index:current_index in
    let num_visible = count_visible tree in
    let new_index = Int.min current_index (num_visible - 1) in
    { model with tree; selected_index = Some new_index }
  | Select_parent ->
    let rows = Model.visible_rows model in
    (match List.nth rows current_index with
     | None -> model
     | Some current_row ->
       if current_row.depth = 0
       then (* Already at root *) model
       else (
         let parent_depth = current_row.depth - 1 in
         let parent_index =
           List.foldi rows ~init:None ~f:(fun i acc row ->
             if i < current_index && row.is_branch && row.depth = parent_depth
             then Some i
             else acc)
         in
         match parent_index with
         | Some idx -> { model with selected_index = Some idx }
         | None -> model))
  | Expand_or_select_next_at_lower_depth ->
    let rows = Model.visible_rows model in
    (match List.nth rows current_index with
     | None -> model
     | Some current_row ->
       if current_row.is_branch && not current_row.is_expanded
       then (
         let tree = toggle_at_index model.tree ~index:current_index in
         { model with tree; selected_index = Some current_index })
       else if current_row.is_branch && current_row.is_expanded
       then (
         let num_visible = Model.num_visible model in
         if current_index + 1 < num_visible
         then { model with selected_index = Some (current_index + 1) }
         else model)
       else (
         let current_depth = current_row.depth in
         let next_lower_depth_index =
           List.findi rows ~f:(fun i row ->
             i > current_index && row.depth < current_depth)
           |> Option.map ~f:fst
         in
         match next_lower_depth_index with
         | Some idx -> { model with selected_index = Some idx }
         | None -> model))
  | Replace_leaf f ->
    let rec replace_in_node node =
      match node with
      | Node.Leaf value -> Option.value (f value) ~default:node
      | Node.Branch { value; children; expanded } ->
        Node.Branch { value; children = List.map children ~f:replace_in_node; expanded }
    in
    let new_tree = replace_in_node model.tree in
    let num_visible = count_visible new_tree in
    (* Preserve explicitness: if it was explicit, clamp to valid range; if implicit, stay
       implicit *)
    let selected_index =
      Option.map model.selected_index ~f:(fun idx ->
        Int.min idx (Int.max 0 (num_visible - 1)))
    in
    { model with tree = new_tree; selected_index }
  | Select_path predicate ->
    let rows = Model.visible_rows model in
    (match List.findi rows ~f:(fun _ row -> predicate row.value) with
     | Some (index, _) -> { model with selected_index = Some index }
     | None -> model)
;;

(** Per-item display information for rendering. The views should include styling. *)
module Item_display = struct
  type t =
    { label : Bonsai_term.View.t
    ; status : Bonsai_term.View.t
    }
end

(* Render the tree view with proper column alignment and line highlighting. The
   render_item function provides styled View.t values for label and status. The
   navigable_tree handles tree chrome with appropriate background. Rows at depth <=
   strip_depth will not have tree prefix characters. *)
let render_view
  ~(render_item : int -> 'a Row.t -> Item_display.t)
  ~(highlight : Bonsai_term.View.t -> Bonsai_term.View.t)
  ~(flavor : Bonsai_term_color_scheme.Flavor.t)
  (model : 'a Model.t)
  : Bonsai_term.View.t
  =
  let open Bonsai_term in
  let subtext_color = Bonsai_term_color_scheme.color ~flavor Subtext0 in
  let content_rows = Model.visible_rows model in
  let selected_index = Model.selected_index model in
  let items, statuses =
    List.mapi content_rows ~f:(fun i row ->
      let is_selected = selected_index = i in
      let display = render_item i row in
      let tree_prefix = render_prefix ~row ~strip_depth:model.strip_depth in
      (* Only collapsed branches show "+ " indicator; everything else has no indicator *)
      let expand_indicator = if row.is_branch && not row.is_expanded then "+ " else "" in
      let prefix_attrs = [ Attr.fg subtext_color ] in
      let item =
        View.hcat
          [ View.text ~attrs:prefix_attrs tree_prefix
          ; View.text ~attrs:prefix_attrs expand_indicator
          ; display.label
          ]
      in
      let status =
        (* Transparent rectangle here prevents a [View.none] status from collapsing when
           we [View.vcat] *)
        View.zcat [ display.status; View.transparent_rectangle ~width:1 ~height:1 ]
      in
      let maybe_highlight v = if is_selected then highlight v else v in
      maybe_highlight item, maybe_highlight status)
    |> List.unzip
  in
  let view = View.hcat [ View.vcat items |> View.pad ~r:1; View.vcat statuses ] in
  let selection =
    View.vcat
      [ View.transparent_rectangle ~width:(View.width view) ~height:selected_index
      ; highlight (View.rectangle ~width:(View.width view) ~height:1 ())
      ]
  in
  View.zcat [ view; selection ]
;;

(* Calculate the scroll offset to keep the selected item visible with padding *)
let calculate_scroll_offset ~selected_index ~visible_height ~total_rows ~current_offset =
  let padding = 1 in
  if total_rows <= visible_height
  then 0
  else if selected_index < current_offset + padding
  then Int.max 0 (selected_index - padding)
  else if selected_index >= current_offset + visible_height - padding
  then
    Int.min (total_rows - visible_height) (selected_index - visible_height + padding + 1)
  else current_offset
;;

let component
  (type a)
  ~(initial_tree : a t)
  ~(render_item : (int -> a Row.t -> Item_display.t) Bonsai.t)
  ~(highlight : (Bonsai_term.View.t -> Bonsai_term.View.t) Bonsai.t)
  ~(dimensions : Bonsai_term.Dimensions.t Bonsai.t)
  ~strip_depth
  (local_ graph)
  =
  let model, inject =
    Bonsai.state_machine
      ~default_model:(Model.create initial_tree ~strip_depth)
      ~apply_action:(fun _ model action -> apply_action model action)
      graph
  in
  (* Track scroll offset *)
  let scroll_offset, set_scroll_offset = Bonsai.state 0 graph in
  (* Update scroll offset when selection changes *)
  let () =
    let selected_index =
      let%arr model in
      Model.selected_index model
    in
    Bonsai.Edge.on_change
      selected_index
      ~trigger:`After_display
      ~equal:[%equal: int]
      ~callback:
        (let%arr set_scroll_offset and dimensions and model and scroll_offset in
         fun selected_index ->
           let visible_height = dimensions.Bonsai_term.Dimensions.height in
           let total_rows = Model.num_visible model in
           let new_offset =
             calculate_scroll_offset
               ~selected_index
               ~visible_height
               ~total_rows
               ~current_offset:scroll_offset
           in
           if new_offset <> scroll_offset
           then set_scroll_offset new_offset
           else Bonsai_term.Effect.Ignore)
      graph
  in
  let view =
    let%arr model
    and render_item
    and flavor = Bonsai_term_color_scheme.flavor graph
    and dimensions
    and scroll_offset
    and highlight in
    let full_view = render_view ~render_item ~flavor ~highlight model in
    (* Crop to show only visible portion *)
    let visible_height = dimensions.Bonsai_term.Dimensions.height in
    let total_rows = Bonsai_term.View.height full_view in
    if total_rows <= visible_height
    then full_view
    else (
      let crop_top = scroll_offset in
      let crop_bottom = Int.max 0 (total_rows - scroll_offset - visible_height) in
      Bonsai_term.View.crop ~t:crop_top ~b:crop_bottom full_view)
  in
  ~view, ~model, ~inject
;;

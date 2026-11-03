(** A navigable tree with keyboard-driven selection and expand/collapse. *)

open! Core

module Node : sig
  type 'a t =
    | Branch of
        { value : 'a
        ; children : 'a t list
        ; expanded : bool
        }
    | Leaf of 'a
  [@@deriving sexp, equal]
end

(** A tree is a single root node. The root is typically a Branch containing children. *)
type 'a t = 'a Node.t [@@deriving sexp, equal]

module Row : sig
  (** A flattened row from the tree, with metadata for rendering. *)
  type 'a t =
    { value : 'a
    ; depth : int
    ; is_branch : bool
    ; is_expanded : bool
    ; is_last_sibling : bool
    ; ancestor_is_last : bool list
    }
end

val flatten : 'a t -> 'a Row.t list
val render_prefix : row:'a Row.t -> strip_depth:int -> string

module Action : sig
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

module Model : sig
  type 'a t [@@deriving sexp, equal]

  val selected_row : 'a t -> 'a Row.t option
  val selected_index : 'a t -> int

  (** Returns true if the selection has been explicitly set (by user navigation or
      programmatic selection), false if it's still the implicit default. *)
  val is_selection_explicit : 'a t -> bool

  (** Get all leaf values under the currently selected node (recursively). If the selected
      node is a leaf, returns just that leaf's value. If the selected node is a branch,
      returns all leaf values in its subtree. *)
  val selected_descendants : 'a t -> 'a list

  (** Like [selected_descendants], but also returns the relative path from the selected
      node to each descendant. The path is a list of names, computed using [get_name]. *)
  val selected_descendants'
    :  'a t
    -> get_name:('a -> 'name)
    -> ('a * path:'name list) list

  (** Returns all values from visible (expanded) rows in order. *)
  val visible_values : 'a t -> 'a list
end

(** Per-item display information for rendering. The views should include styling. *)
module Item_display : sig
  type t =
    { label : Bonsai_term.View.t
    ; status : Bonsai_term.View.t
    }
end

(** Bonsai component for a navigable tree with rendering and auto-scrolling.

    [render_item] is called for each visible row to produce display information. It
    receives the row index, the row, and can use external state (like process states) to
    determine the label and status views.

    [highlight] applies styling via [View.with_color] to the selected row.

    [dimensions] specifies the visible area. The tree will scroll to keep the selected
    item visible.

    [strip_depth] controls tree chrome rendering. Rows at depth <= strip_depth will not
    have tree prefix characters (lines/corners). This is useful when the root node serves
    as a title row that shouldn't have tree chrome.

    Returns the rendered view, the model (for querying selection), and inject for
    dispatching actions (including tree updates via [Replace_leaf]). *)
val component
  :  initial_tree:'a t
  -> render_item:(int -> 'a Row.t -> Item_display.t) Bonsai.t
  -> highlight:(Bonsai_term.View.t -> Bonsai_term.View.t) Bonsai.t
  -> dimensions:Bonsai_term.Dimensions.t Bonsai.t
  -> strip_depth:int
  -> Bonsai.graph @ local
  -> view:Bonsai_term.View.t Bonsai.t
     * model:'a Model.t Bonsai.t
     * inject:('a Action.t -> unit Bonsai.Effect.t) Bonsai.t

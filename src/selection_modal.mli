(** A generic modal with a text input for filtering and a selectable list of results.

    The modal handles keyboard input (typing to filter, arrow keys to navigate, Enter to
    select, Escape to cancel), rendering (centered bordered modal with scrolling results),
    and state management (clearing input on open, clamping selection). *)

open! Core
open Bonsai_term

(** How to render a single item in the results list. *)
module Item_view : sig
  type t =
    { icon : View.t
    ; label : View.t
    }
end

(** The result of the selection modal component. *)
type 'a t =
  { view : View.t
  ; handler : Event.t -> unit Effect.t
  ; reset : unit Effect.t
  }

(** Create a selection modal component.

    [items] is the list of selectable items. Each item has a display name (used for
    filtering) and a value (returned on selection).

    [render_item] controls how each item appears in the list. It receives the item name,
    value, and whether the item is currently highlighted.

    [on_select] is called when the user presses Enter on a highlighted item.

    [on_cancel] is called when the user presses Escape.

    The component resets its search text and selection on select. Callers should fire
    [reset] when closing the modal via other means (e.g. Escape). *)
val component
  :  items:(string * 'a) list Bonsai.t
  -> render_item:(string -> 'a -> is_selected:bool -> Item_view.t) Bonsai.t
  -> on_select:('a -> unit Effect.t) Bonsai.t
  -> dimensions:Dimensions.t Bonsai.t
  -> Bonsai.graph @ local
  -> 'a t Bonsai.t

open! Core
open Bonsai_term
open Bonsai.Let_syntax

type t =
  { view : View.t Bonsai.t
  ; show : (bg:Attr.Color.t -> string -> unit Effect.t) Bonsai.t
  }

let create (local_ graph) =
  (* Toast state: stores the time until which to show the toast, plus its content *)
  let toast_state, set_toast_state =
    Bonsai.state_opt ~equal:[%equal: Time_ns.t * Attr.Color.t * string] graph
  in
  (* Effect to get current time from Bonsai clock *)
  let get_current_time = Bonsai.Clock.get_current_time graph in
  (* Check if we're before the toast expiry time *)
  let toast_visible_until =
    let%arr toast_state in
    match toast_state with
    | None -> Time_ns.epoch
    | Some (visible_until, _, _) -> visible_until
  in
  let is_visible =
    let before_or_after = Bonsai.Clock.at toast_visible_until graph in
    let%arr before_or_after in
    match before_or_after with
    | Before -> true
    | After -> false
  in
  let view =
    let%arr is_visible
    and toast_state
    and flavor = Bonsai_term_color_scheme.flavor graph in
    match is_visible, toast_state with
    | true, Some (_, bg, text) ->
      let crust = Bonsai_term_color_scheme.color ~flavor Crust in
      let toast_text = [%string " %{text} "] in
      View.text ~attrs:[ Attr.fg crust; Attr.bg bg ] toast_text
    | _ -> View.none
  in
  let show =
    let%arr set_toast_state and get_current_time in
    fun ~bg text ->
      let%bind.Effect now = get_current_time in
      let visible_until = Time_ns.add now (Time_ns.Span.of_sec 1.0) in
      set_toast_state (Some (visible_until, bg, text))
  in
  { view; show }
;;

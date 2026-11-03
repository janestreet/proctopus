(** A simple single-line text input component for filtering. *)

open! Core
open Bonsai_term

type t =
  { view : View.t
  ; text : string
  ; handler : Event.t -> unit Effect.t
  ; set_text : string -> unit Effect.t
  }

val component
  :  cursor_attrs:Attr.t list Bonsai.t
  -> text_attrs:Attr.t list Bonsai.t
  -> is_focused:bool Bonsai.t
  -> Bonsai.graph @ local
  -> t Bonsai.t

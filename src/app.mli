open! Core
open Async

module Group_status : sig
  (* Groups don't have an "ok" status because they don't expect to exit, only to be
     replaced with a subtree *)
  type t =
    | Initializing
    | Failed of string
  [@@deriving sexp]
end

(* Actions are injected from the outside world *)
module Action : sig
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

(* Events are like effects that need to be handled by the outside world. *)
module App_event : sig
  type t =
    | Start_process of { id : Command_id.t }
    | Kill_process of { id : Command_id.t }
    | Restart_process of { id : Command_id.t }
    | Pause_process of { id : Command_id.t }
    | Resume_process of { id : Command_id.t }
end

(** The main app component for running the process manager *)
val component
  :  title:string
  -> debug_messages:string list Bonsai.t
  -> debug:(string -> unit) Bonsai.t
  -> current_error:string option Bonsai.t
  -> dismiss_error:(unit -> unit) Bonsai.t
  -> dimensions:Bonsai_term.Dimensions.t Bonsai.t
  -> on_event:(App_event.t -> unit Bonsai.Effect.t) Bonsai.t
  -> exit:(unit -> unit Bonsai.Effect.t) Bonsai.t
  -> start_fullscreen:bool
  -> Bonsai.graph @ local
  -> (view:Bonsai_term.View.t
     * handler:(Bonsai_term.Event.t -> unit Bonsai.Effect.t)
     * inject:(Action.t -> unit Bonsai.Effect.t)
     * nav_inject:(Tree_item.t Navigable_tree.Action.t -> unit Bonsai.Effect.t)
     * nav_model:Tree_item.t Navigable_tree.Model.t)
       Bonsai.t

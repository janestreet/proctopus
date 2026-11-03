(** An [Instance.t] is a proctopus process that is responsible for managing a configured
    set of processes when requested by the channel and relaying updates back to the
    channel. *)

open! Core
open Async

type t

(** Connect to the channel and register commands. The instance handles start/kill/status
    RPCs by calling Process_manager directly. Command bash codes are extracted from
    entries. Group entries trigger spawning of nested instances that connect back to the
    channel. *)
val create
  :  socket_path:string
  -> entries:Protocol.Register_commands.Entry.t list
  -> log_config:Log_config.t option
  -> t Or_error.t Deferred.t

(** Returns a deferred that becomes determined when the connection closes. The process
    manager is automatically cleaned up when the connection closes. *)
val closed : t -> unit Deferred.t

(** Close the connection to the channel and wait for cleanup to finish: all managed
    processes are terminated and interactive sessions are closed. *)
val close : t -> unit Deferred.t

(** Operations on file systems *)
open Core.Std
open Async.Std
open Phat_pure.Std
open Path

val exists : (abs, _) Path.t -> [ `Yes | `Unknown | `No ] Deferred.t

val lstat : (abs, _) Path.t -> Unix.Stats.t Or_error.t Deferred.t

val mkdir : (abs, dir) Path.t -> unit Or_error.t Deferred.t

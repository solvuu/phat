(** Operations on file systems *)
open Core.Std
open Phat_pure
open Path

val exists : (abs, _) Path.t -> [ `Yes | `Unknown | `No ]

val lstat : (abs, _) Path.t -> Unix.stats Or_error.t

val mkdir : (abs, dir) Path.t -> unit Or_error.t

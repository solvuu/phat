(** Operations on file systems *)
open Core.Std
open Async.Std
open Phat_path

val exists : (abs, _) t -> [ `Yes | `Unknown | `No ] Deferred.t

val lstat : (abs, _) t -> Unix.Stats.t Or_error.t Deferred.t

val mkdir : abs_dir -> unit Or_error.t Deferred.t

(** [find item path] searches for [item] in [path], returning the full
    path of the first directory it is found in or None if it is not
    found. *)
val find_item
  :  (rel,'typ) item
  -> abs_dir list
  -> (abs,'typ) t option Deferred.t

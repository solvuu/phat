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

val reify :
  (abs, dir) t ->
  (abs, dir) t Deferred.Or_error.t

val fold :
  (abs, dir) t ->
  f:('a -> [ `File of (rel, file) t
           | `Dir of (rel, dir) t
           | `Broken_link of (rel, link) t ] -> 'a Deferred.t) ->
  init:'a ->
  'a Deferred.Or_error.t

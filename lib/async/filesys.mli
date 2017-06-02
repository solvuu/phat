(** Operations on file systems *)
open Core
open Async
open Phat_path

(** [exists p] is either:
   - [`Yes] if [p] exists verbatim on the file system
   - [`Yes_modulo_links] if [p] makes sense on the file system (that
     is [p] corresponds to an object of the correct type on the file system)
   - [`Yes_as_other_object] if [p] exists but corresponds to an
     incompatible object (file instead of dir, link with a different
     target, ...)
   - [`No] if [p] does not correspond to any object on the file system
   - [`Unknown] if permissions are not enough to test the existence of
     [p] *)
val exists : (abs, _) t -> [ `Yes | `Yes_modulo_links | `Yes_as_other_object | `Unknown | `No ] Deferred.t

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

val fold_follows_links :
  (abs, dir) t ->
  f:('a -> [ `File of (abs, file) t * (abs, file) t
           | `Dir  of (abs, dir) t  * (abs, dir) t
           | `Broken_link of (abs, link) t * (abs, link) t ] -> 'a Deferred.t) ->
  init:'a ->
  'a Deferred.Or_error.t

val iter :
  (abs, dir) t
  -> f :
  (
    [
    | `File of (rel, file) t
    | `Dir of (rel, dir) t
    | `Broken_link of (rel, link) t
    ] ->
    unit Deferred.t
  )
  -> unit Deferred.Or_error.t

val iter_follows_links :
  (abs, dir) t
  -> f :
  (
    [
    | `File of (abs, file) t * (abs, file) t
    | `Dir  of (abs, dir) t  * (abs, dir) t
    | `Broken_link of (abs, link) t * (abs, link) t
    ] ->
    unit Deferred.t
  )
  -> unit Deferred.Or_error.t

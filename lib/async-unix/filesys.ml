open Core.Std
open Phat_pure.Core2
open Async.Std
module Path = Phat_path

let and_check f x e =
  match e with
  | `Yes -> f x
  | `No -> return `No
  | `Unknown -> return `Unknown

let and_check2 f (seen, e) =
  match e with
  | `Yes -> f seen
  | `No -> return (seen, `No)
  | `Unknown -> return (seen, `Unknown)


(** Async's [file_exists] has a
    {{:https://github.com/janestreet/async_unix/issues/6}bug} that
    doesn't allow setting follow_symlinks to false. *)
let file_exists x =
  In_thread.run (fun () -> Core.Std.Sys.file_exists ~follow_symlinks:false x)

let is_file = Sys.is_file ~follow_symlinks:false
let is_directory = Sys.is_directory ~follow_symlinks:false

let is_link p =
  file_exists p >>= function
  | `No
  | `Unknown as x ->
    return x
  | `Yes ->
    Unix.lstat p >>| fun {Unix.Stats.kind; _} ->
    match kind with
    | `Link ->
      `Yes
    | `File | `Directory | `Char | `Block | `Fifo | `Socket ->
      `No


(* Represents a position in a path. This is useful when building or
   checking a path, which is a step by step operation: each step is
   performed by a recursive call on a (p_abs, p_rel) pair, with the
   invariant that:
   - the path that we want to build/check is concat p_abs p_rel
   - p_abs has already been built/checked
   - p_abs is resolved

   In general, we'd like to avoid calling the recursive function twice
   with the same arguments (to avoid duplicating work on shared
   subtrees, or to be cycle-tolerant) and so the build/check function
   will generally carry a set of cursors to remember which calls have
   already been made.
 *)
module Path_cursor : sig
  type t
  val make : (_,_) Path.t -> (_,_) Path.t -> t
  val compare : t -> t -> int
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end
= struct
  type t = P : (_,_) Path.t * (_,_) Path.t -> t
  let make p q = P (p, q)
  let compare = compare
  let t_of_sexp _ = assert false
  let sexp_of_t _ = assert false
end

module Cursor_set = struct
  include Set.Make(Path_cursor)
  let add set p q = add set (Path_cursor.make p q)
  let mem set p q = mem set (Path_cursor.make p q)
end

let rec exists_main
  : type typ. Cursor_set.t -> (Path.abs, typ) Path.t -> (Cursor_set.t * [ `Yes | `Unknown | `No ]) Deferred.t
  =
  let open Path in
  fun seen -> function
    | Item Root -> return (seen, `Yes)
    | Cons (Root, p_rel) -> exists_rel_path seen (Item Root) p_rel

and exists_item
    : type typ. Cursor_set.t -> Path.abs_dir -> (Path.rel,typ) Path.item -> (Cursor_set.t * [ `Yes | `Unknown | `No ]) Deferred.t
    =
    fun seen p_abs item ->
      match item with
      | Path.Dot -> return (seen, `Yes)
      | Path.Dotdot -> return (seen, `Yes)
      | Path.File _ ->
        let p_abs' = Path.to_string (Path.concat p_abs (Path.Item item)) in
        file_exists p_abs' >>= and_check is_file p_abs' >>| fun file_exists ->
        seen, file_exists
      | Path.Dir _ ->
        let p_abs' = Path.to_string (Path.concat p_abs (Path.Item item)) in
        file_exists p_abs' >>= and_check is_directory p_abs' >>| fun dir_exists ->
        seen, dir_exists
      | Path.Link (_, target) ->
        let target_exists seen =
          match Path.kind_of target with
          | `Abs p -> exists_main seen p
          | `Rel p -> exists_rel_path seen p_abs p
        in
        let p_abs' = Path.to_string (Path.concat p_abs (Path.Item item)) in
        file_exists p_abs' >>= and_check is_link p_abs' >>= fun link_exists ->
        (seen, link_exists) |> and_check2 target_exists

and exists_rel_path
    : type typ. Cursor_set.t -> Path.abs_dir -> (Path.rel,typ) Path.t -> (Cursor_set.t * [ `Yes | `Unknown | `No ]) Deferred.t
    =
    fun seen p_abs p_rel ->
      if Cursor_set.mem seen p_abs p_rel then return (seen, `Yes)
      else (
        let seen' = Cursor_set.add seen p_abs p_rel in
        match p_rel with
        | Path.Item x -> exists_item seen' p_abs x
        | Path.Cons (x, y) ->
          exists_item seen' p_abs x
          >>= and_check2 (fun seen -> exists_rel_path seen (Path.concat p_abs (Path.Item x)) y)
      )

and exists p =
  exists_main Cursor_set.empty p >>| snd


let lstat p : Unix.Stats.t Or_error.t Deferred.t =
  try_with (fun () -> Unix.lstat (Path.to_string p)) >>|
  Or_error.of_exn_result >>|
  Or_error.tag_loc _here_

let wrap_unix loc f =
  try_with f >>|
  Or_error.of_exn_result >>|
  Or_error.tag_loc loc

let unix_mkdir p =
  wrap_unix _here_ (fun () -> Unix.mkdir (Path.to_string p))

let unix_symlink link_path ~targets:link_target =
  wrap_unix _here_ (fun () ->
      Unix.symlink ~dst:(Path.to_string link_path) ~src:(Path.to_string link_target)
    )

let rec mkdir_main
  : Cursor_set.t -> Path.abs_dir -> Cursor_set.t Or_error.t Deferred.t
  = fun seen p ->
    match p with
    | Path.Item Path.Root -> return (Ok seen)
    | Path.Cons (Path.Root, rel_p) ->
      mkdir_aux seen Path.root rel_p

and mkdir_aux
  : Cursor_set.t -> Path.abs_dir -> Path.rel_dir -> Cursor_set.t Or_error.t Deferred.t
  = fun seen p_abs p_rel ->
    if Cursor_set.mem seen p_abs p_rel then return (Ok seen)
    else
      let seen' = Cursor_set.add seen p_abs p_rel in
      match p_rel with
      | Path.Item (Path.Dir _) -> (
          let p = Path.concat p_abs p_rel in
          exists p >>= (fun x -> match x with
            | `Yes -> return (Ok ())
            | `No | `Unknown -> unix_mkdir p
          ) >>|? fun () ->
          seen'
        )
      | Path.Item Path.Dot -> return (Ok seen')
      | Path.Item Path.Dotdot -> return (Ok seen')
      | Path.Item (Path.Link (_, dir)) -> (
          let p = Path.concat p_abs p_rel in
          unix_symlink p ~targets:dir >>=? fun () ->
          match Path.kind_of dir with
          | `Rel dir -> mkdir_aux seen' p_abs dir
          | `Abs dir -> mkdir_main seen' dir
        )
      | Path.Cons (Path.Dir n, p_rel') -> (
          let p_abs' = Path.concat p_abs (Path.Item (Path.Dir n)) in
          exists p_abs' >>= (fun x -> match x with
            | `Yes -> return (Ok ())
            | `No | `Unknown -> unix_mkdir p_abs'
          ) >>=? fun () ->
          mkdir_aux seen' p_abs' p_rel'
        )
      | Path.Cons (Path.Link (_, dir) as l, p_rel') -> (
          unix_symlink (Path.concat p_abs (Path.Item l)) ~targets:dir >>=? fun () ->
          match Path.kind_of dir with
          | `Rel dir ->
            mkdir_aux seen' p_abs (Path.concat dir p_rel')
          | `Abs dir ->
            mkdir_main seen' (Path.concat dir p_rel')
        )
      | Path.Cons (Path.Dot, p_rel') -> mkdir_aux seen' p_abs p_rel'
      | Path.Cons (Path.Dotdot, p_rel') ->
        mkdir_aux seen' (Path.parent p_abs) p_rel'

and mkdir p =
  mkdir_main Cursor_set.empty p >>| fun _ ->
  Ok ()

let rec find_item item path =
  match path with
  | [] -> return None
  | dir::path ->
     let x = Path.concat dir (Path.Item item) in
     exists x >>= function
     | `Yes -> return (Some x)
     | `Unknown | `No -> find_item item path

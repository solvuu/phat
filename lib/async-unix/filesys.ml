open Core.Std
open Async.Std
open Phat_pure.Std
open Path

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

(* FIXME: exists is not immune to cyclic paths, use same procedure than
   mkdir *)
let rec exists
  : type o. (abs, o) Path.t -> [ `Yes | `Unknown | `No ] Deferred.t
  =
  function
  | Item Root -> return `Yes
  | Cons (Root, p_rel) -> exists_rel_path "/" p_rel

and exists_item
    : type o. string -> (rel, o) item -> [ `Yes | `Unknown | `No ] Deferred.t
    =
    fun p_abs p_rel ->
      match p_rel with
      | Dot -> return `Yes
      | Dotdot -> return `Yes
      | File f -> (
          let p_abs' = Filename.concat p_abs (f :> string) in
          file_exists p_abs' >>= function
          | `No | `Unknown as x -> return x
          | `Yes -> is_file p_abs'
        )
      | Dir d -> (
          let p_abs' = Filename.concat p_abs (d :> string) in
          file_exists p_abs' >>= function
          | `No | `Unknown as x -> return x
          | `Yes -> is_directory p_abs'
        )
      | Link (l, target) -> (
          let target_exists () =
            match kind target with
            | Abs_path p -> exists p
            | Rel_path p -> exists_rel_path p_abs p
          in
          let p_abs' = Filename.concat p_abs (l :> string) in
          file_exists p_abs' >>= function
          | `No | `Unknown as x -> return x
          | `Yes ->
            is_link p_abs' >>= function
            | `No | `Unknown as x -> return x
            | `Yes -> target_exists ()
        )

and exists_rel_path
    : type o. string -> (rel, o) Path.t -> [ `Yes | `Unknown | `No ] Deferred.t
    =
    fun p_abs p_rel ->
      match p_rel with
      | Item x -> exists_item p_abs x
      | Cons (x, y) -> (
          exists_item p_abs x >>= function
          | `No | `Unknown as z -> return z
          | `Yes ->
            exists_rel_path
              (Filename.concat p_abs (string_of_item x :> string))
              y
        )

let sexp_of_unix_error =
  Tuple.T3.sexp_of_t
    Unix.sexp_of_error
    sexp_of_string
    sexp_of_string

let lstat p : Unix.Stats.t Or_error.t Deferred.t =
  try_with (fun () -> Unix.lstat (to_string p)) >>|
  Or_error.of_exn_result >>| fun x ->
  Or_error.tag x "Phat_unix.Filesys.lstat"

let wrap_unix title f =
  try_with f >>|
  Or_error.of_exn_result >>| fun x ->
  Or_error.tag x title

let unix_mkdir p =
  wrap_unix "Phat_unix.Filesys.mkdir" (fun () -> Unix.mkdir (to_string p))

let unix_symlink link_path ~targets:link_target =
  wrap_unix "Phat_unix.Filesys.mkdir" (fun () ->
      Unix.symlink ~dst:(to_string link_path) ~src:(to_string link_target)
    )

module State : sig
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

module Path_set = struct
  include Set.Make(State)
  let add set p q = add set (State.make p q)
  let mem set p q = mem set (State.make p q)
end


let rec mkdir_main
  : Path_set.t -> (abs, dir) Path.t -> Path_set.t Or_error.t Deferred.t
  = fun seen p ->
    match p with
    | Item Root -> return (Ok seen)
    | Cons (Root, rel_p) ->
      mkdir_aux seen root rel_p

and mkdir_aux
  : Path_set.t -> (abs, dir) Path.t -> (rel, dir) Path.t -> Path_set.t Or_error.t Deferred.t
  = fun seen p_abs p_rel ->
    if Path_set.mem seen p_abs p_rel then return (Ok seen)
    else
      let seen' = Path_set.add seen p_abs p_rel in
      match p_rel with
      | Item (Dir _) -> (
          let p = concat p_abs p_rel in
          exists p >>= (fun x -> match x with
            | `Yes -> return (Ok ())
            | `No | `Unknown -> unix_mkdir p
          ) >>|? fun () ->
          seen'
        )
      | Item Dot -> return (Ok seen')
      | Item Dotdot -> return (Ok seen')
      | Item (Link (_, dir)) -> (
          let p = concat p_abs p_rel in
          unix_symlink p ~targets:dir >>=? fun () ->
          match kind dir with
          | Rel_path dir -> mkdir_aux seen' p_abs dir
          | Abs_path dir -> mkdir_main seen' dir
        )
    | Cons (Dir n, p_rel') -> (
        let p_abs' = concat p_abs (Item (Dir n)) in
        exists p_abs' >>= (fun x -> match x with
            | `Yes -> return (Ok ())
            | `No | `Unknown -> unix_mkdir p_abs'
        ) >>=? fun () ->
        mkdir_aux seen' p_abs' p_rel'
      )
    | Cons (Link (_, dir) as l, p_rel') -> (
      unix_symlink (concat p_abs (Item l)) ~targets:dir >>=? fun () ->
      match kind dir with
      | Rel_path dir ->
        mkdir_aux seen' p_abs (concat dir p_rel')
      | Abs_path dir ->
        mkdir_main seen' (concat dir p_rel')
      )
    | Cons (Dot, p_rel') -> mkdir_aux seen' p_abs p_rel'
    | Cons (Dotdot, p_rel') ->
      mkdir_aux seen' (parent p_abs) p_rel'

and mkdir p =
  mkdir_main Path_set.empty p >>| fun _ ->
  Ok ()

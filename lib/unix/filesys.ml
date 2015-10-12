open Core.Std
open Phat_pure
open Path

let file_exists = Sys.file_exists ~follow_symlinks:false
let is_file = Sys.is_file ~follow_symlinks:false
let is_directory = Sys.is_directory ~follow_symlinks:false

let and_check f x e =
  match e with
  | `Yes -> f x
  | `No -> `No
  | `Unknown -> `Unknown

let is_link p =
  let p_is_a_link () =
    Unix.(if (lstat p).st_kind = S_LNK then `Yes else `No)
  in
  file_exists p
  |> and_check p_is_a_link ()

let rec exists
  : type o. (abs, o) Path.t -> [ `Yes | `Unknown | `No ]
  =
  function
  | Item Root -> `Yes
  | Cons (Root, p_rel) -> exists_rel_path "/" p_rel

and exists_item
    : type o. string -> (rel, o) item -> [ `Yes | `Unknown | `No ]
    =
    fun p_abs p_rel ->
      match p_rel with
      | Dot -> `Yes
      | Dotdot -> `Yes
      | File f ->
        let p_abs' = Filename.concat p_abs (f :> string) in
        file_exists p_abs'
        |> and_check is_file p_abs'
      | Dir d ->
        let p_abs' = Filename.concat p_abs (d :> string) in
        file_exists p_abs'
        |> and_check is_directory p_abs'
      | Link (l, target) ->
        let target_exists () =
          match kind target with
          | Abs_path p -> exists p
          | Rel_path p -> exists_rel_path p_abs p
        in
        let p_abs' = Filename.concat p_abs (l :> string) in
        file_exists p_abs'
        |> and_check is_link p_abs'
        |> and_check target_exists ()

and exists_rel_path
    : type o. string -> (rel, o) Path.t -> [ `Yes | `Unknown | `No ]
    =
    fun p_abs p_rel ->
      match p_rel with
      | Item x -> exists_item p_abs x
      | Cons (x, y) ->
        exists_item p_abs x
        |> and_check (fun (x,y) ->
            exists_rel_path
              (Filename.concat p_abs (string_of_item x :> string))
              y) (x, y)

let sexp_of_unix_error =
  Tuple.T3.sexp_of_t
    Unix.sexp_of_error
    sexp_of_string
    sexp_of_string

let lstat p =
  try Ok (Unix.lstat (to_string p))
  with Unix.Unix_error (e, fn, msg) ->
    error "Phat_unix.Filesys.lstat" (e, fn, msg) sexp_of_unix_error


let wrap_unix fn f =
  try Ok (f ())
  with
  | Unix.Unix_error (e, fn, msg) ->
    error fn (e, fn, msg) sexp_of_unix_error

let unix_mkdir p =
  wrap_unix "Phat_unix.Filesys.mkdir" (fun () -> Unix.mkdir (to_string p))

let unix_symlink link_path ~targets:link_target =
  wrap_unix "Phat_unix.Filesys.mkdir" (fun () ->
      Unix.symlink ~dst:(to_string link_path) ~src:(to_string link_target)
    )

let rec mkdir
  : (abs, dir) Path.t -> unit Or_error.t
  = function
    | Item Root -> Ok ()
    | Cons (Root, rel_p) ->
      mkdir_aux root rel_p

and mkdir_aux
  : (abs, dir) Path.t -> (rel, dir) Path.t -> unit Or_error.t
  = fun p_abs p_rel ->
    let open Or_error.Monad_infix in
    match p_rel with
    | Item (Dir n) ->
      let p = concat p_abs p_rel in
      if exists p = `Yes then Ok () else unix_mkdir p
    | Item Dot -> Ok ()
    | Item Dotdot -> Ok ()
    | Item (Link (n, dir)) -> (
        unix_symlink (concat p_abs p_rel) ~targets:dir >>= fun () ->
        match kind dir with
        | Rel_path dir -> mkdir_aux p_abs dir
        | Abs_path dir -> mkdir dir
      )
    | Cons (Dir n, p_rel') ->
      let p_abs' = concat p_abs (Item (Dir n)) in
      (if exists p_abs' = `Yes then Ok () else unix_mkdir p_abs') >>= fun () ->
      mkdir_aux p_abs' p_rel'
    | Cons (Link (n, dir) as l, p_rel') -> (
      unix_symlink (concat p_abs (Item l)) dir >>= fun () ->
      match kind dir with
      | Rel_path dir ->
        mkdir_aux p_abs (concat dir p_rel')
      | Abs_path dir ->
        mkdir (concat dir p_rel')
      )
    | Cons (Dot, p_rel') -> mkdir_aux p_abs p_rel'
    | Cons (Dotdot, p_rel') ->
      mkdir_aux (parent p_abs) p_rel'

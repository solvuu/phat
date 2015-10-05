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
    Unix.(if (stat p).st_kind = S_LNK then `Yes else `No)
  in
  file_exists p
  |> and_check p_is_a_link ()

let some_kind_of_path
  : type k o. (k, o) Path.t -> o Path.of_some_kind
  = fun path ->
    match path with
    | Item Root -> Abs_path path
    | Item Dot -> Rel_path path
    | Item Dotdot -> Rel_path path
    | Item (File _) -> Rel_path path
    | Item (Dir _) -> Rel_path path
    | Item (Link _) -> Rel_path path
    | Cons (Root, _) -> Abs_path path
    | Cons (Dot, _) -> Rel_path path
    | Cons (Dotdot, _) -> Rel_path path
    | Cons (Dir _, _) -> Rel_path path
    | Cons (Link _, _) -> Rel_path path

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
          match some_kind_of_path target with
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

let stat p =
  try Ok (Unix.stat (to_string p))
  with Unix.Unix_error (e, fn, msg) ->
    error "Phat.stat" (e, fn, msg) sexp_of_unix_error

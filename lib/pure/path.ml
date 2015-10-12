open Core_kernel.Std
open Core2
open Result.Monad_infix

(* Many functions not tail-recursive. Considered okay because paths
   are unlikely to be very long.
*)

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
type name = string with sexp

type abs = [`abs] with sexp
type rel = [`rel] with sexp

type dir = [`dir] with sexp
type file = [`file] with sexp

type ('kind,'obj) item =
  | Root : (abs,dir) item
  | File : name -> (rel,file) item
  | Dir : name -> (rel,dir) item
  | Link : name * (_,'obj) t -> (rel,'obj) item
  | Dot : (rel,dir) item
  | Dotdot : (rel,dir) item

and ('kind,'obj) t =
  | Item : ('kind,'obj) item -> ('kind,'obj) t
  | Cons : ('a,dir) item * (rel,'obj) t -> ('a,'obj) t

with sexp_of

type 'a of_some_kind =
  | Abs_path of (abs,'a) t
  | Rel_path of (rel,'a) t

type file_path = (abs,file) t
type dir_path = (abs,dir) t


(******************************************************************************)
(* Names                                                                      *)
(******************************************************************************)
let name s =
  if String.mem s '/' then
    error "slash not allowed in file or directory name" s sexp_of_string
  else if s = "." || s = ".." || s = "" then
    error "invalid file or directory name" s sexp_of_string
  else
    Ok s

let name_exn s = name s |> ok_exn

(******************************************************************************)
(* Visitors                                                                   *)
(******************************************************************************)

let rec is_normalized
  : type k o. (k, o) t -> bool
  = function
    | Item _ -> true
    | p -> is_normalized_no_dot p

and is_normalized_no_dot
  : type k o. (k, o) t -> bool
  = function
    | Item Dot -> false
    | Cons (Dot, _) -> false
    | Item _ -> true
    | Cons (Dotdot, p') -> is_normalized_no_dot p'
    | Cons (_, p') -> is_normalized_no_dot_or_dotdot p'

and is_normalized_no_dot_or_dotdot
  : type k o. (k, o) t -> bool
  = function
    | Item Dot -> false
    | Item Dotdot -> false
    | Cons (Dot, _) -> false
    | Cons (Dotdot, _) -> false
    | Item _ -> true
    | Cons (_, p') -> is_normalized_no_dot_or_dotdot p'

let rec has_link
  : type k o. (k, o) t -> bool
  = function
    | Item (Link _) -> true
    | Cons (Link _, _) -> true
    | Cons (_, p) -> has_link p
    | _ -> false


type ('o, 'a) mapper = { map : 'k. ('k, 'o) t -> 'a }

let map_any_kind x mapper =
  match x with
  | Abs_path p -> mapper.map p
  | Rel_path p -> mapper.map p

let kind
  : type k o. (k, o) t -> o of_some_kind
  = fun p ->
    match p with
    | Item Root -> Abs_path p
    | Item (File _) -> Rel_path p
    | Item (Dir _) -> Rel_path p
    | Item Dot -> Rel_path p
    | Item Dotdot -> Rel_path p
    | Item (Link _) -> Rel_path p
    | Cons (Root, _) -> Abs_path p
    | Cons (Dir _, _) -> Rel_path p
    | Cons (Dotdot, _) -> Rel_path p
    | Cons (Dot, _) -> Rel_path p
    | Cons (Link _, _) -> Rel_path p

(******************************************************************************)
(* Operators                                                                  *)
(******************************************************************************)

let rec concat
  : type a kind. (a,dir) t -> (rel, kind) t -> (a,kind) t
  = fun x y ->
    match x with
    | Item x -> Cons (x,y)
    | Cons (x1,x2) -> Cons (x1, concat x2 y)

let rec resolve_any : type k o. (k,o) t -> o of_some_kind =
  function
  | Item x -> resolve_item x
  | Cons (item, path) ->
    match resolve_item item, resolve_any path with
    | Rel_path x, Rel_path y -> Rel_path (concat x y)
    | _, Abs_path y -> Abs_path y
    | Abs_path x, Rel_path y -> Abs_path (concat x y)

and resolve_item : type k o . (k,o) item -> o of_some_kind = fun x ->
  match x with
  | Root -> Abs_path (Item x)
  | File _ -> Rel_path (Item x)
  | Dir _ -> Rel_path (Item x)
  | Link (_, target) -> resolve_any target
  | Dot -> Rel_path (Item x)
  | Dotdot -> Rel_path (Item x)

and resolve : type o. (abs,o) t -> (abs, o) t =
  function
  | Item Root as x -> x
  | Cons (Root as x, path) ->
    match resolve_any path with
    | Abs_path y -> y
    | Rel_path y -> Cons (x, y)

let rec parent : type a b . (a,b) t -> (a,dir) t =
  fun path -> match path with
  | Item Root -> path
  | Item (File _) -> Item Dot
  | Item (Dir _) -> Item Dot
  | Item (Link _) -> Item Dot
  | Item Dot -> Item Dotdot
  | Item Dotdot -> Cons (Dotdot, path)
  | Cons (item, path) -> Cons(item, parent path)

let rec normalize : type a b . (a,b) t -> (a,b) t =
  fun path -> match path with
    | Item _ -> path
    | Cons (dir, path_tail) ->
      let path_tail_norm = normalize path_tail in
      match dir, path_tail_norm with
      | _, Item Dot -> Item dir
      | Dot, _ -> path_tail_norm
      | Root, Item Dotdot -> Item Root
      | Root, Cons (Dotdot, path') -> normalize (Cons (Root, path'))
      | Dotdot, Item Dotdot -> Cons (Dotdot, path_tail_norm)
      | Dotdot, Cons (Dotdot, _) -> Cons (Dotdot, path_tail_norm)
      | Dir _, Item Dotdot -> Item Dot
      | Dir _, Cons (Dotdot, path') -> path'
      | Link _, Item Dotdot -> Item Dot
      | Link _, Cons (Dotdot, path') -> path'
      | _, _ -> Cons (dir, path_tail_norm)


let rec equal
  : type a b c d. (a,b) t -> (c,d) t -> bool
  = fun p q -> equal_normalized (normalize p) (normalize q)

and equal_normalized : type a b c d. (a,b) t -> (c,d) t -> bool =
    fun p q -> match p,q with
    | Item p, Item q -> equal_item p q
    | Cons(p_dir,p_path), Cons(q_dir,q_path) ->
      (equal_item p_dir q_dir) && (equal_normalized p_path q_path)
    | _, _ -> false

and equal_item : type a b c d. (a,b) item -> (c,d) item -> bool =
    fun p q -> match p,q with
      | Root, Root -> true
      | Root, _ -> false
      | _, Root -> false
      | File p, File q -> String.equal p q
      | File _, _ -> false
      | _, File _ -> false
      | Dir p, Dir q -> String.equal p q
      | Dir _, _ -> false
      | _, Dir _ -> false
      | Link (np, p'), Link (nq, q') ->
        String.equal np nq && equal p' q'
      | Link _, _ -> false
      | _, Link _ -> false
      | Dot, Dot -> true
      | Dot, _ -> false
      | _, Dot -> false
      | Dotdot, Dotdot -> true


(******************************************************************************)
(* Elems - internal use only                                             *)
(******************************************************************************)
module Elem : sig

  (** Like [item], an [elem] represents a single component of a path
      but is less strongly typed. It is a string guaranteed to be
      either a [name] or ".", "", "..", or "/". *)
  type elem = private string

  (** List guaranteed to be non-empty. *)
  type elems = private elem list

(*  val elem : string -> elem Or_error.t  # Avoid unused warning *)
  val elems : string -> elems Or_error.t

  val item_to_elem : (_,_) item -> elem

  val rel_dir_of_elems : elems -> (rel,dir) t Or_error.t
  val dir_of_elems : elems -> (abs,dir) t Or_error.t
  val rel_file_of_elems : elems -> (rel,file) t Or_error.t
  val file_of_elems : elems -> (abs,file) t Or_error.t

end = struct
  type elem = string with sexp
  type elems = elem list with sexp

  let elem s = match s with
    | "/" | "" | "." | ".." -> Ok s
    | _ -> name s

  let elems = function
    | "" -> Or_error.error_string "invalid empty path"
    | "/" -> Ok ["/"]
    | s ->
      let s,leading_slash = match String.chop_prefix s ~prefix:"/" with
        | None -> s,false
        | Some s -> s,true
      in
      Result.List.map ~f:elem (String.split s ~on:'/')
      >>| fun l ->
      if leading_slash then "/"::l else l

  let item_to_elem : type a b . (a,b) item -> elem = function
    | Root -> "/"
    | Dir x -> x
    | File x -> x
    | Link (x,_) -> x
    | Dot -> "."
    | Dotdot -> ".."

  let rel_dir_of_elems elems : (rel,dir) t Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::_ ->
      error "relative path cannot begin with root directory"
        elems sexp_of_elems
    | _::tl ->
      if List.mem tl "/" then
        error "root directory can only occur as first item in path"
          elems sexp_of_elems
      else
        let item = function
          | "/" -> assert false
          | "" | "." -> Dot
          | ".." -> Dotdot
          | x -> (Dir x)
        in
        let rec loop = function
          | [] -> assert false
          | x::[] -> Item (item x)
          | x::elems -> Cons (item x, loop elems)
        in
        Ok (loop elems)

  let dir_of_elems elems : (abs,dir) t Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::[] -> Ok (Item Root)
    | "/"::tl -> (
      if List.mem tl "/" then
        error "root directory can only occur as first item in path"
          elems sexp_of_elems
      else (
        rel_dir_of_elems tl >>| fun reldir ->
        Cons (Root, reldir)
      )
    )
    | _ ->
      error "absolute path must begin with root directory"
        elems sexp_of_elems

  let rel_file_of_elems elems : (rel,file) t Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::_ ->
      error "relative path cannot begin with root directory"
        elems sexp_of_elems
    | _ -> (
      let elems_rev = List.rev elems in
      let last = List.hd_exn elems_rev in
      let elems = List.rev (List.tl_exn elems_rev) in
      match last with
      | "." | "" | ".." ->
        error "path cannot be treated as file" elems sexp_of_elems
      | _ ->
        match elems with
        | [] -> Ok (Item (File last))
        | _ ->
          rel_dir_of_elems elems >>| fun dir ->
          concat dir (Item (File last))
    )

  let file_of_elems elems : (abs,file) t Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::[] ->
      error "root directory cannot be treated as file"
        elems sexp_of_elems
    | "/"::rest-> (
      let rest_rev = List.rev rest in
      let last = List.hd_exn rest_rev in
      let rest = List.rev (List.tl_exn rest_rev) in
      match last with
      | "." | "" | ".." ->
        error "path cannot be treated as file" elems sexp_of_elems
      | _ ->
        dir_of_elems ("/"::rest) >>| fun dir ->
        concat dir (Item (File last))
    )
    | _ ->
      error "absolute path must begin with root directory"
        elems sexp_of_elems

end
open Elem


(******************************************************************************)
(* Constructors                                                               *)
(******************************************************************************)
let root = Item Root
let rel_dir_path s = elems s >>= rel_dir_of_elems
let dir_path s = elems s >>= dir_of_elems
let rel_file_path s = elems s >>= rel_file_of_elems
let file_path s = elems s >>= file_of_elems


(******************************************************************************)
(* Deconstructors                                                             *)
(******************************************************************************)
let rec to_elem_list : type a b . (a,b) t -> elem list = function
  | Item x -> [item_to_elem x]
  | Cons (item, path) -> (item_to_elem item) :: (to_elem_list path)

let to_list path = (to_elem_list path :> string list)

let to_string t =
  to_list t |> function
  | "/"::path -> "/" ^ (String.concat ~sep:"/" path)
  | path -> String.concat ~sep:"/" path

(* since 'k and 'o are phantom types in ('k, 'o) t, we don't need
   conversion functions for them, but sexp is not smart enough to see
   that. *)
let sexp_of_t p = sexp_of_t (fun _ -> assert false) (fun _ -> assert false) p

let string_of_item x = (Elem.item_to_elem x :> string)

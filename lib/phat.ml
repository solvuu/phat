open Core.Std
open Phat_core2
open Result.Monad_infix

(* Many functions not tail-recursive. Considered okay because paths
   are unlikely to be very long.
*)

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
type name = string with sexp

type abs
type rel

type dir
type file

type (_,_,_) cons =
  | RR : (rel,rel,rel) cons
  | RA : (rel,abs,abs) cons
  | AR : (abs,rel,abs) cons
  | AA : (abs,abs,abs) cons

type ('kind,'obj) item =
| Root : (abs,dir) item
| File : name -> (rel,file) item
| Dir : name -> (rel,dir) item
| Link : name * (_,'obj) path -> (rel,'obj) item
| Dot : (rel,dir) item
| Dotdot : (rel,dir) item

and ('kind,'obj) path =
| Item : ('kind,'obj) item -> ('kind,'obj) path
| Cons : ('a,'b,'c) cons * ('a,dir) item * ('b,'obj) path -> ('c,'obj) path

type _ some_kind_of_path =
  | Abs_path : (abs,'a) path -> 'a some_kind_of_path
  | Rel_path : (rel,'a) path -> 'a some_kind_of_path

type file_path = (abs,file) path
type dir_path = (abs,dir) path


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


(******************************************************************************)
(* Operators                                                                  *)
(******************************************************************************)

(* let rec concat : type a b c kind . *)
(*   (a,b,c) cons -> (a,dir) path -> (b,kind) path -> (c,kind) path *)
(*   = fun cons x y -> *)
(*     match cons, x with *)
(*     | _, Item x -> Cons (cons,x,y) *)
(*     | RR, Cons(RR,x1,x2) -> Cons (RR, x1, concat RR x2 y) *)
(*     | AR, Cons(AR,x1,x2) -> Cons (AR, x1, concat RR x2 y) *)
(*     | AR, Cons(RA,x1,x2) -> concat AR x2 y *)
(*     | AR, Cons(AA,x1,x2) -> concat AR x2 y *)
(*     | RA, _ -> y *)
(*     | AA, _ -> y *)

let rec concat : type a b c kind .
  (a,b,c) cons -> (a,dir) path -> (b,kind) path -> (c,kind) path
  = fun cons x y ->
    match cons, x with
    | _, Item x -> Cons (cons,x,y)
    | RR, Cons (RR,x1,x2) -> Cons (RR, x1, concat RR x2 y)
    | AR, Cons (AR,x1,x2) -> Cons (AR, x1, concat RR x2 y)
    | AR, Cons (RA,x1,x2) -> Cons (RA, x1, concat AR x2 y)
    | AR, Cons (AA,x1,x2) -> Cons (AA, x1, concat AR x2 y)
    | RA, Cons (RR,x1,x2) -> Cons (RA, x1, concat RA x2 y)
    | AA, Cons (AA,x1,x2) -> Cons (AA, x1, concat AA x2 y)
    | AA, Cons (AR,x1,x2) -> Cons (AA, x1, concat RA x2 y)
    | AA, Cons (RA,x1,x2) -> Cons (RA, x1, concat AA x2 y)

let rec resolve : type k o. (k,o) path -> o some_kind_of_path =
  function
  | Item x -> resolve_item x
  | Cons (_, item, path) ->
    match resolve_item item, resolve path with
    | Rel_path x, Rel_path y -> Rel_path (concat RR x y)
    | Rel_path x, Abs_path y -> Abs_path (concat RA x y)
    | Abs_path x, Rel_path y -> Abs_path (concat AR x y)
    | Abs_path x, Abs_path y -> Abs_path (concat AA x y)

and resolve_item : type k o . (k,o) item -> o some_kind_of_path = fun x ->
  match x with
  | Root -> Abs_path (Item x)
  | File _ -> Rel_path (Item x)
  | Dir _ -> Rel_path (Item x)
  | Link (_, target) -> resolve target
  | Dot -> Rel_path (Item x)
  | Dotdot -> Rel_path (Item x)

let rec parent : type a b . (a,b) path -> (a,dir) path =
  fun path -> match path with
  | Item Root -> path
  | Item (File _) -> Item Dot
  | Item (Dir _) -> Item Dot
  | Item (Link _) -> Item Dot
  | Item Dot -> Item Dotdot
  | Item Dotdot -> Cons (RR, Dotdot, path)
  | Cons (cons, item, path) -> Cons(cons, item, parent path)

let rec normalize : type a b . (a,b) path -> (a,b) path =
  fun path -> match path with
    | Item _ -> path
    | Cons (cons, dir, path) ->
      let path = normalize path in
      match cons, dir, path with
      | RA, _, (Item Root as p) -> p
      | AA, _, (Item Root as p) -> p
      | _, Root, Item (File _) -> Cons (cons, dir, path)
      | _, Root, Item (Link _) -> Cons (cons, dir, path)
      | _, Root, Item (Dir _)  -> Cons (cons, dir, path)
      | _, Dir _, Item (File _) -> Cons(cons, dir, path)
      | _, Dir _, Item (Link _) -> Cons(cons, dir, path)
      | _, Dir _, Item (Dir _) -> Cons (cons, dir, path)
      | RR, Dir _, Item Dot -> Item dir
      | RR, Dir _, Item Dotdot -> Item Dot
      | _,  Dir _, Cons(_, Dir _, _) -> Cons (cons, dir, path)
      | _,  Dir _, Cons (_, Link _, _) -> Cons (cons, dir, path)
      | RA, Dir _, Cons(_, Root, _) -> path
      | _, _, Cons (_, Dot, _) -> assert false
      | RR, Dir _, Cons (RR, Dotdot, path) -> path
      | RA, Dir _, Cons (RA, Dotdot, path) -> path
      | _, Link(name,p), _ -> Cons (cons, Link (name, normalize p), path)
      | RR, Dot, _ -> path
      | RA, Dot, _ -> path
      | _, Dotdot, Cons _ -> Cons (cons, dir, path)
      | _, Dotdot, Item (File _) -> Cons (cons, dir, path)
      | _, Dotdot, Item (Link _) -> Cons (cons, dir, path)
      | _, Dotdot, Item (Dir _) -> Cons (cons, dir, path)
      | RR, Dotdot, Item Dot -> Item Dotdot
      | _, Dotdot, Item Dotdot -> Cons (cons, dir, path)
      | AA, Root, Cons (_, Root, _) -> path
      | _, Root, Cons (_, Dir _, _) -> Cons (cons, dir, path)
      | _, Root, Cons (_, Link _, _) -> Cons (cons, dir, path)
      | AA, Root, Cons (RA, Dotdot, path) -> normalize (Cons (cons, Root, path))
      | AR, Root, Cons (RR, Dotdot, path) -> normalize (Cons (cons, Root, path))
      | AR, Root, Item Dot -> Item Root
      | AR, Root, Item Dotdot -> Item Root

let equal p q =
  let equal_item : type a b . (a,b) item -> (a,b) item -> bool =
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
      | Link _, _ -> assert false
      | _, Link _ -> assert false
      | Dot, Dot -> true
      | Dot, _ -> false
      | _, Dot -> false
      | Dotdot, Dotdot -> true
  in
  let rec equal_normalized : type a b . (a,b) path -> (a,b) path -> bool =
    fun p q -> match p,q with
    | Item p, Item q -> equal_item p q
    | Cons(RR, p_dir,p_path), Cons(RR, q_dir,q_path) ->
      (equal_item p_dir q_dir) && (equal_normalized p_path q_path)
    | Cons(AR, p_dir,p_path), Cons(AR, q_dir,q_path) ->
      (equal_item p_dir q_dir) && (equal_normalized p_path q_path)
    | Cons(RA, p_dir,p_path), Cons(RA, q_dir,q_path) ->
      (equal_item p_dir q_dir) && (equal_normalized p_path q_path)
    | Cons(AA, p_dir,p_path), Cons(AA, q_dir,q_path) ->
      (equal_item p_dir q_dir) && (equal_normalized p_path q_path)
    | _, _ -> false
  in
  equal_normalized (normalize p) (normalize q)


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

  val rel_dir_of_elems : elems -> (rel,dir) path Or_error.t
  val dir_of_elems : elems -> (abs,dir) path Or_error.t
  val rel_file_of_elems : elems -> (rel,file) path Or_error.t
  val file_of_elems : elems -> (abs,file) path Or_error.t

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

  let rel_dir_of_elems elems : (rel,dir) path Or_error.t =
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
          | x::elems -> Cons (RR, item x, loop elems)
        in
        Ok (loop elems)

  let dir_of_elems elems : (abs,dir) path Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::[] -> Ok (Item Root)
    | "/"::tl -> (
      if List.mem tl "/" then
        error "root directory can only occur as first item in path"
          elems sexp_of_elems
      else (
        rel_dir_of_elems tl >>| fun reldir ->
        Cons (AR, Root, reldir)
      )
    )
    | _ ->
      error "absolute path must begin with root directory"
        elems sexp_of_elems

  let rel_file_of_elems elems : (rel,file) path Or_error.t =
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
          concat RR dir (Item (File last))
    )

  let file_of_elems elems : (abs,file) path Or_error.t =
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
        concat AR dir (Item (File last))
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
let rec to_elem_list : type a b . (a,b) path -> elem list = function
  | Item x -> [item_to_elem x]
  | Cons (_, item, path) -> (item_to_elem item) :: (to_elem_list path)

let to_list path = (to_elem_list path :> string list)

let to_string t =
  to_list t |> function
  | "/"::path -> "/" ^ (String.concat ~sep:"/" path)
  | path -> String.concat ~sep:"/" path

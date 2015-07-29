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

type ('absrel,'kind) item =
| Root : (abs,dir) item
| File : name -> (rel,file) item
| Dir : name -> (rel,dir) item
| Link : name * ('absrel,'kind) path -> ('absrel,'kind) item
| Dot : (rel,dir) item
| Dotdot : (rel,dir) item

and ('absrel,'kind) path =
| Item : ('absrel,'kind) item -> ('absrel,'kind) path
| Cons : ('absrel,dir) item * (rel,'kind) path -> ('absrel,'kind) path

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
(* Path manipulation                                                          *)
(******************************************************************************)
let rec concat : type absrel kind .
  (absrel,dir) path -> (rel,kind) path -> (absrel,kind) path
  = fun x y ->
    match x with
    | Item x -> Cons (x,y)
    | Cons (x1,x2) -> Cons (x1, concat x2 y)


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
          | x::elems -> Cons (item x, loop elems)
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
        Cons (Root, reldir)
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
          concat dir (Item (File last))
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
        concat dir (Item (File last))
    )
    | _ ->
      error "absolute path must begin with root directory"
        elems sexp_of_elems

end
open Elem


(******************************************************************************)
(* Path constructors                                                          *)
(******************************************************************************)
let root = Item Root
let rel_dir_path s = elems s >>= rel_dir_of_elems
let dir_path s = elems s >>= dir_of_elems
let rel_file_path s = elems s >>= rel_file_of_elems
let file_path s = elems s >>= file_of_elems


(******************************************************************************)
(* Path converters                                                            *)
(******************************************************************************)
let rec to_elem_list : type a b . (a,b) path -> elem list = function
  | Item x -> [item_to_elem x]
  | Cons (item,path) -> (item_to_elem item)::(to_elem_list path)

let to_list path = (to_elem_list path :> string list)

let to_string t = to_list t |> String.concat ~sep:"/"

(* TODO: check for infinite loops *)
let rec resolve_links : type a b . (a,b) path -> (a,b) path =
  let resolve_item_links : type a b . (a,b) item -> (a,b) path = fun x ->
    match x with
    | Root -> Item x
    | File _ -> Item x
    | Dir _ -> Item x
    | Link (_, target) -> resolve_links target
    | Dot -> Item x
    | Dotdot -> Item x
  in
  function
  | Item x -> resolve_item_links x
  | Cons (item,path) ->
    concat (resolve_item_links item) (resolve_links path)

let rec parent : type a b . (a,b) path -> (a,dir) path =
  fun path -> match path with
  | Item Root -> path
  | Item (File _) -> Item Dot
  | Item (Dir _) -> Item Dot
  | Item (Link (_,path)) -> parent path
  | Item Dot -> Item Dotdot
  | Item Dotdot -> Cons (Dotdot, path)
  | Cons (item,path) -> Cons(item, parent path)

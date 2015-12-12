open Core_kernel.Std
open Core2
open Result.Monad_infix

(* Many functions not tail-recursive. Considered okay because paths
   are unlikely to be very long.
*)

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)
type name = string [@@deriving sexp]

type abs = [`Abs] [@@deriving sexp_of]
type rel = [`Rel] [@@deriving sexp_of]

type dir = [`Dir] [@@deriving sexp_of]
type file = [`File] [@@deriving sexp_of]
type link = [`Link]

type ('kind,'typ) item =
  | Root : (abs,dir) item
  | File : name -> (rel,file) item
  | Dir : name -> (rel,dir) item
  | Link : name * (_,'typ) t -> (rel,'typ) item
  | Broken_link : name * string list -> (rel, link) item
  | Dot : (rel,dir) item
  | Dotdot : (rel,dir) item

and ('kind,'typ) t =
  | Item : ('kind,'typ) item -> ('kind,'typ) t
  | Cons : ('a,dir) item * (rel,'typ) t -> ('a,'typ) t

[@@deriving sexp_of]

type 'typ of_any_kind = [
  | `Abs of (abs,'typ) t
  | `Rel of (rel,'typ) t
]

type 'kind of_any_typ = [
  | `File of ('kind, file) t
  | `Link of ('kind, link) t
  | `Dir of ('kind, dir) t
]

type abs_file = (abs,file) t [@@deriving sexp_of]
type rel_file = (rel,file) t [@@deriving sexp_of]
type abs_dir = (abs,dir) t [@@deriving sexp_of]
type rel_dir = (rel,dir) t [@@deriving sexp_of]

type ('typ, 'a) map_any_kind = { map : 'kind. ('kind, 'typ) t -> 'a }

let map_any_kind x mapper =
  match x with
  | `Abs p -> mapper.map p
  | `Rel p -> mapper.map p


(******************************************************************************)
(* Deserializers - those that ppx_sexp_conv can't derive                      *)
(******************************************************************************)
let rec rel_dir_item_of_sexp sexp : (rel,dir) item =
  match sexp with
  | Sexp.List [Sexp.Atom "Dir"; name] ->
    Dir (name_of_sexp name)
  | Sexp.List [Sexp.Atom "Link"; name; x] ->
    let name = name_of_sexp name in
    map_any_kind (dir_of_any_kind_of_sexp x) { map = fun p -> Link (name, p) }
  | Sexp.Atom "Dot" ->
    Dot
  | Sexp.Atom "Dotdot" ->
    Dotdot
  | _ ->
    failwithf "invalid sexp for (rel,dir) item: %s"
      (Sexp.to_string sexp) ()

and abs_dir_item_of_sexp sexp : (abs,dir) item =
  match sexp with
  | Sexp.Atom "Root" ->
    Root
  | _ ->
    failwithf "invalid sexp for (abs,dir) item: %s"
      (Sexp.to_string sexp) ()

and rel_file_item_of_sexp sexp : (rel,file) item =
  match sexp with
  | Sexp.List [Sexp.Atom "File"; name] ->
    File (name_of_sexp name)
  | Sexp.List [Sexp.Atom "Link"; name; x] ->
    let name = name_of_sexp name in
    map_any_kind (file_of_any_kind_of_sexp x) { map = fun p -> Link (name, p) }
  | _ ->
    failwithf "invalid sexp for (rel,file) item: %s"
      (Sexp.to_string sexp) ()

and rel_file_of_sexp sexp : rel_file =
  match sexp with
  | Sexp.List [Sexp.Atom "Item"; item] ->
    Item (rel_file_item_of_sexp item)
  | Sexp.List [Sexp.Atom "Cons"; item; x] ->
    Cons (rel_dir_item_of_sexp item, rel_file_of_sexp x)
  | _ ->
    failwithf "invalid sexp for (rel,file) t: %s"
      (Sexp.to_string sexp) ()

and abs_file_of_sexp sexp : abs_file =
  match sexp with
  | Sexp.List [Sexp.Atom "Cons"; item; x] ->
    Cons (abs_dir_item_of_sexp item, rel_file_of_sexp x)
  | _ ->
    failwithf "invalid sexp for (abs,file) t: %s"
      (Sexp.to_string sexp) ()

and rel_dir_of_sexp sexp : (rel,dir) t =
  match sexp with
  | Sexp.List [Sexp.Atom "Item"; item] ->
    Item (rel_dir_item_of_sexp item)
  | Sexp.List [Sexp.Atom "Cons"; item; x] ->
    Cons (rel_dir_item_of_sexp item, rel_dir_of_sexp x)
  | _ ->
    failwithf "invalid sexp for (rel,dir) t: %s"
      (Sexp.to_string sexp) ()

and abs_dir_of_sexp sexp : abs_dir =
  match sexp with
  | Sexp.List [Sexp.Atom "Item"; item] ->
    Item (abs_dir_item_of_sexp item)
  | Sexp.List [Sexp.Atom "Cons"; item; x] ->
    Cons (abs_dir_item_of_sexp item, rel_dir_of_sexp x)
  | _ ->
    failwithf "invalid sexp for (abs,dir) t: %s"
      (Sexp.to_string sexp) ()

and dir_of_any_kind_of_sexp sexp : dir of_any_kind =
  try `Rel (rel_dir_of_sexp sexp)
  with Failure _ -> `Abs (abs_dir_of_sexp sexp)

and file_of_any_kind_of_sexp sexp : file of_any_kind =
  try `Rel (rel_file_of_sexp sexp)
  with Failure _ -> `Abs (abs_file_of_sexp sexp)

(******************************************************************************)
(* Names                                                                      *)
(******************************************************************************)
let name s =
  if String.mem s '/' then
    errorh _here_ "slash not allowed in file or directory name" s sexp_of_string
  else if s = "." || s = ".." || s = "" then
    errorh _here_ "invalid file or directory name" s sexp_of_string
  else
    Ok s

let name_exn s = name s |> ok_exn


(******************************************************************************)
(* Visitors                                                                   *)
(******************************************************************************)

let rec is_normalized
  : type a b. (a,b) t -> bool
  = function
    | Item _ -> true
    | p -> is_normalized_no_dot p

and is_normalized_no_dot
  : type a b. (a,b) t -> bool
  = function
    | Item Dot -> false
    | Cons (Dot, _) -> false
    | Item _ -> true
    | Cons (Dotdot, p') -> is_normalized_no_dot p'
    | Cons (_, p') -> is_normalized_no_dot_or_dotdot p'

and is_normalized_no_dot_or_dotdot
  : type a b. (a,b) t -> bool
  = function
    | Item Dot -> false
    | Item Dotdot -> false
    | Cons (Dot, _) -> false
    | Cons (Dotdot, _) -> false
    | Item _ -> true
    | Cons (_, p') -> is_normalized_no_dot_or_dotdot p'

let rec has_link
  : type a b. (a,b) t -> bool
  = function
    | Item (Link _) -> true
    | Cons (Link _, _) -> true
    | Cons (_, p) -> has_link p
    | _ -> false


let kind_of
  : type a b. (a,b) t -> b of_any_kind
  = fun p ->
    match p with
    | Item Root -> `Abs p
    | Item (File _) -> `Rel p
    | Item (Dir _) -> `Rel p
    | Item Dot -> `Rel p
    | Item Dotdot -> `Rel p
    | Item (Link _) -> `Rel p
    | Item (Broken_link _) -> `Rel p
    | Cons (Root, _) -> `Abs p
    | Cons (Dir _, _) -> `Rel p
    | Cons (Dotdot, _) -> `Rel p
    | Cons (Dot, _) -> `Rel p
    | Cons (Link _, _) -> `Rel p

let rec obj_aux
  : type k k' b. (k, b) t -> (k', b) t -> k of_any_typ
  = fun p suffix_p ->
    match suffix_p with
    | Item (File _) -> `File p
    | Item (Link (_, p')) -> obj_aux p p'
    | Item (Broken_link _) -> `Link p
    | Item (Dir _) -> `Dir p
    | Item Dot -> `Dir p
    | Item Dotdot -> `Dir p
    | Item Root -> `Dir p
    | Cons (_, p') -> obj_aux p p'

let typ_of
  : type k. (k, _) t -> k of_any_typ
  = fun p -> obj_aux p p

(******************************************************************************)
(* Operators                                                                  *)
(******************************************************************************)

let rec concat
  : type a b. (a,dir) t -> (rel,b) t -> (a,b) t
  = fun x y ->
    match x with
    | Item x -> Cons (x,y)
    | Cons (x1,x2) -> Cons (x1, concat x2 y)

let rec resolve_any_kind : type a b. (a,b) t -> b of_any_kind =
  function
  | Item x -> resolve_item x
  | Cons (item, path) ->
    match resolve_item item, resolve_any_kind path with
    | `Rel x, `Rel y -> `Rel (concat x y)
    | _, `Abs y -> `Abs y
    | `Abs x, `Rel y -> `Abs (concat x y)

and resolve_item : type a b. (a,b) item -> b of_any_kind = fun x ->
  match x with
  | Root -> `Abs (Item x)
  | File _ -> `Rel (Item x)
  | Broken_link _ -> `Rel (Item x)
  | Dir _ -> `Rel (Item x)
  | Link (_, target) -> resolve_any_kind target
  | Dot -> `Rel (Item x)
  | Dotdot -> `Rel (Item x)

and resolve : type b. (abs,b) t -> (abs,b) t =
  function
  | Item Root as x -> x
  | Cons (Root as x, path) ->
    match resolve_any_kind path with
    | `Abs y -> y
    | `Rel y -> Cons (x, y)

let rec parent : type a b. (a,b) t -> (a,dir) t =
  fun path -> match path with
  | Item Root -> path
  | Item (File _) -> Item Dot
  | Item (Broken_link _) -> Item Dot
  | Item (Dir _) -> Item Dot
  | Item (Link _) -> Item Dot
  | Item Dot -> Item Dotdot
  | Item Dotdot -> Cons (Dotdot, path)
  | Cons (item, path) -> Cons(item, parent path)

let rec normalize : type a b. (a,b) t -> (a,b) t =
  fun path -> match path with
    | Item i -> Item (normalize_item i)
    | Cons (dir, path_tail) ->
      let dir_norm = normalize_item dir in
      let path_tail_norm = normalize path_tail in
      match dir_norm, path_tail_norm with
      | _, Item Dot -> Item dir_norm
      | Dot, _ -> path_tail_norm
      | Root, Item Dotdot -> Item Root
      | Root, Cons (Dotdot, path') -> normalize (Cons (Root, path'))
      | Dotdot, Item Dotdot -> Cons (Dotdot, path_tail_norm)
      | Dotdot, Cons (Dotdot, _) -> Cons (Dotdot, path_tail_norm)
      | Dir _, Item Dotdot -> Item Dot
      | Dir _, Cons (Dotdot, path') -> path'
      | Link _, Item Dotdot -> Item Dot
      | Link _, Cons (Dotdot, path') -> path'
      | _, _ -> Cons (dir_norm, path_tail_norm)

and normalize_item
  : type a b. (a,b) item -> (a,b) item
  = fun item ->
    match item with
    | File _ -> item
    | Dir _ -> item
    | Dot -> item
    | Dotdot -> item
    | Root -> item
    | Broken_link _ -> item
    | Link (n, p) -> Link (n, normalize p)

let equal
  : type a b. (a,b) t -> (a,b) t -> bool
  = fun p q -> normalize p = normalize q

let compare
  : type a b. (a,b) t -> (a,b) t -> int
  = fun p q -> compare (normalize p) (normalize q)

module Infix = struct
  let ( / ) = concat
end

module Make_relative = struct

  let rec skip_common_prefix
    : type typ.
        (rel, dir) t ->
        (rel, dir) t ->
        (rel, dir) t option * (rel, dir) t option
    = fun p1 p2 ->
      match p1, p2 with
      | Item i1, Item i2 ->
        if i1 = i2 then None, None else Some p1, Some p2
      | Item i1, Cons (i2, q2) ->
        if i1 = i2 then None, Some q2 else Some p1, Some p2
      | Cons (i1, q1), Item i2 ->
        if i1 = i2 then Some q1, None else Some p1, Some p2
      | Cons (i1, q1), Cons (i2, q2) ->
        if i1 = i2 then skip_common_prefix q1 q2 else Some p1, Some p2

  let rec backwards
    : type k. (k, dir) t -> (rel, dir) t option
    =
    let open Option.Monad_infix in
    function
    | Item Root -> Some (Item Dot)
    | Item (Dir _) -> Some (Item Dotdot)
    | Item (Link _) -> Some (Item Dotdot)
    | Item Dot -> Some (Item Dot)
    | Item Dotdot -> None
    | Cons (Root, p_rel) -> backwards p_rel
    | Cons (Dir _, p_rel) ->
      backwards p_rel >>| fun q -> Cons(Dotdot, q)
    | Cons (Link _, p_rel) ->
      backwards p_rel >>| fun q -> Cons(Dotdot, q)
    | Cons (Dot, p_rel) -> backwards p_rel
    | Cons (Dotdot, _) -> None

  let rec last_item
    : type o. (rel, o) t -> (rel, o) t
    = function
        Item i as p -> p
      | Cons (_, q) -> last_item q

  type _ ty =
    | TyFile : file ty
    | TyDir  : dir ty
    | TyLink : link ty

  let rec ty
    : type k o. (k, o) t -> o ty
    = function
      | Item i -> item_ty i
      | Cons (_, p) -> ty p
  and item_ty
    : type k o. (k, o) item -> o ty
    = function
      | Root -> TyDir
      | Dir _ -> TyDir
      | Dot -> TyDir
      | Dotdot -> TyDir
      | File _ -> TyFile
      | Broken_link _ -> TyLink
      | Link (_, target) -> ty target

  let make_relative
    : type o. (abs, o) t -> from:(abs, dir) t -> (rel, o) t
    = fun p ~from:origin ->
      match normalize origin, normalize p with
      | Item Root, Item Root -> Item Dot
      | Item Root, Cons (Root, p_rel) -> p_rel
      | Cons (Root, p_rel), Item Root ->
        Option.value_exn (backwards (normalize origin))
        (* backwards provides a result if the path is absolute and normalized *)
      | Cons (Root, q_rel), Cons (Root, p_rel) ->
        let rec k
          : type o. o ty -> (rel, o) t -> (rel, o) t
          = fun p_rel_ty p_rel ->
            match p_rel_ty  with
            | TyDir -> (
                match skip_common_prefix q_rel p_rel with
                | None, None -> Item Dot
                | Some q_rel, None -> (
                    Option.value_exn (backwards q_rel)
                  )
                | None, Some p_rel -> p_rel
                | Some q_rel, Some p_rel ->
                  let backwards_q_rel = Option.value_exn (backwards q_rel) (* q_rel has no Dotdot *) in
                  normalize (concat backwards_q_rel p_rel)
              )
            | TyFile -> (
                let p_rel_parent = parent p_rel in
                normalize (concat (k (ty p_rel_parent) p_rel_parent) (last_item p_rel))
              )
            | TyLink -> (
                let p_rel_parent = parent p_rel in
                normalize (concat (k (ty p_rel_parent) p_rel_parent) (last_item p_rel))
              )
        in
        k (ty p_rel) p_rel
end

let make_relative = Make_relative.make_relative

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
  val path_to_elems : _ t -> elems

  val rel_dir_of_elems : elems -> rel_dir Or_error.t
  val abs_dir_of_elems : elems -> abs_dir Or_error.t
  val rel_file_of_elems : elems -> rel_file Or_error.t
  val abs_file_of_elems : elems -> abs_file Or_error.t

end = struct
  type elem = string [@@deriving sexp_of]
  type elems = elem list [@@deriving sexp_of]

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
    | Broken_link (x, _) -> x
    | Link (x,_) -> x
    | Dot -> "."
    | Dotdot -> ".."

  let rec path_to_elems : type a b . (a,b) t -> elems = function
    | Item i -> [ item_to_elem i ]
    | Cons (x, y) -> (item_to_elem x) :: (path_to_elems y)

  let rel_dir_of_elems elems : rel_dir Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::_ ->
      errorh _here_ "relative path cannot begin with root directory"
        elems sexp_of_elems
    | _::tl ->
      if List.mem tl "/" then
        errorh _here_ "root directory can only occur as first item in path"
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

  let abs_dir_of_elems elems : abs_dir Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::[] -> Ok (Item Root)
    | "/"::tl -> (
      if List.mem tl "/" then
        errorh _here_ "root directory can only occur as first item in path"
          elems sexp_of_elems
      else (
        rel_dir_of_elems tl >>| fun reldir ->
        Cons (Root, reldir)
      )
    )
    | _ ->
      errorh _here_ "absolute path must begin with root directory"
        elems sexp_of_elems

  let rel_file_of_elems elems : rel_file Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::_ ->
      errorh _here_ "relative path cannot begin with root directory"
        elems sexp_of_elems
    | _ -> (
      let elems_rev = List.rev elems in
      let last = List.hd_exn elems_rev in
      let elems = List.rev (List.tl_exn elems_rev) in
      match last with
      | "." | "" | ".." ->
        errorh _here_ "path cannot be treated as file" elems sexp_of_elems
      | _ ->
        match elems with
        | [] -> Ok (Item (File last))
        | _ ->
          rel_dir_of_elems elems >>| fun dir ->
          concat dir (Item (File last))
    )

  let abs_file_of_elems elems : abs_file Or_error.t =
    match elems with
    | [] -> assert false
    | "/"::[] ->
      errorh _here_ "root directory cannot be treated as file"
        elems sexp_of_elems
    | "/"::rest-> (
      let rest_rev = List.rev rest in
      let last = List.hd_exn rest_rev in
      let rest = List.rev (List.tl_exn rest_rev) in
      match last with
      | "." | "" | ".." ->
        errorh _here_ "path cannot be treated as file" elems sexp_of_elems
      | _ ->
        abs_dir_of_elems ("/"::rest) >>| fun dir ->
        concat dir (Item (File last))
    )
    | _ ->
      errorh _here_ "absolute path must begin with root directory"
        elems sexp_of_elems

end
open Elem


(******************************************************************************)
(* Constructors                                                               *)
(******************************************************************************)

let root = Item Root
let file n = Item (File n)
let file_exn s = file (name_exn s)
let dir n = Item (Dir n)
let dir_exn s = dir (name_exn s)
let link n p = Item (Link (n, p))
let link_exn s p = link (name_exn s) p
let dot = Item Dot
let dotdot = Item Dotdot
let broken_link n xs = Item (Broken_link (n, xs))
let broken_link_exn s xs = broken_link (name_exn s) xs

let cons x y = concat x (Item y)

let rel_dir s = elems s >>= rel_dir_of_elems
let abs_dir s = elems s >>= abs_dir_of_elems
let rel_file s = elems s >>= rel_file_of_elems
let abs_file s = elems s >>= abs_file_of_elems

let file_of_any_kind s =
  match rel_file s with
  | Ok x -> Ok (`Rel x)
  | Error e1 ->
    match abs_file s with
    | Ok x -> Ok (`Abs x)
    | Error e2 -> Error (Error.of_list [e1;e2])

let dir_of_any_kind s =
  match rel_dir s with
  | Ok x -> Ok (`Rel x)
  | Error e1 ->
    match abs_dir s with
    | Ok x -> Ok (`Abs x)
    | Error e2 -> Error (Error.of_list [e1;e2])


(******************************************************************************)
(* Deconstructors                                                             *)
(******************************************************************************)
let rec to_elem_list : type a b. (a,b) t -> elem list = function
  | Item x -> [item_to_elem x]
  | Cons (item, path) -> (item_to_elem item) :: (to_elem_list path)

let to_list path = (to_elem_list path :> string list)

let to_string x =
  to_list x |> function
  | "/"::path -> "/" ^ (String.concat ~sep:"/" path)
  | path -> String.concat ~sep:"/" path

(* since 'k and 't are unused types in ('k, 't) path, we don't need
   conversion functions for them, but ppx_sexp_conv is not smart enough to see
   that. *)
let sexp_of_t p =
  sexp_of_t (fun _ -> assert false) (fun _ -> assert false) p

let string_of_item x = (Elem.item_to_elem x :> string)

let rec last_item
  : type o. (rel, o) t -> (rel, o) item
  = function
    | Item i -> i
    | Cons (_, rest) -> last_item rest

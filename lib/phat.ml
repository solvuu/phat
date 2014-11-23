open Core.Std
open Phat_core2
open Result.Monad_infix

(******************************************************************************)
(* Names                                                                      *)
(******************************************************************************)
type name = string with sexp

let name s =
  if String.mem s '/' then
    error "slash not allowed in file or directory name" s sexp_of_string
  else if s = "." || s = ".." || s = "" then
    error "invalid file or directory name" s sexp_of_string
  else
    Ok s


(******************************************************************************)
(* Items                                                                      *)
(******************************************************************************)
type item = string with sexp
type items = item list with sexp

let item s =
  match s with
  | "/" | "" | "." | ".." -> Ok s
  | _ -> name s

let items (s:string) : items Or_error.t =
  match s with
  | "" -> Or_error.error_string "invalid empty path"
  | "/" -> Ok ["/"]
  | _ ->
    let s,leading_slash = match String.chop_prefix s ~prefix:"/" with
      | None -> s,false
      | Some s -> s,true
    in
    Result.List.map ~f:item (String.split s ~on:'/')
    >>| fun l ->
    if leading_slash then "/"::l else l


(******************************************************************************)
(* GADT and constructors                                                      *)
(******************************************************************************)
type abs
type rel

type dir
type file

type ('absrel,'kind) path =
| Root : (abs,dir) path
| File : name -> (rel,file) path
| Dir : name -> (rel,dir) path
| Link : name * ('absrel,'kind) path -> ('absrel,'kind) path
| Dot : (rel,dir) path
| Dotdot : (rel,dir) path
| Concat : ('absrel,dir) path * (rel,'kind) path -> ('absrel,'kind) path

let root = Root

(** Internal helper function used in other parsers below. Returned
    list will not be empty. *)
let rec to_items : type a b . (a,b) path -> items = function
  | Root -> ["/"]
  | Dir x -> [x]
  | File x -> [x]
  | Link (x,_) -> [x]
  | Dot -> ["."]
  | Dotdot -> [".."]
  | Concat (dir,relpath) -> (to_items dir)@(to_items relpath)

let rel_dir_of_items items : (rel,dir) path Or_error.t =
  match items with
  | [] -> assert false
  | hd::tl -> (
    (
      match hd with
      | "/" ->
        error "relative path cannot begin with root directory"
          items sexp_of_items
      | "" | "." -> Ok Dot
      | ".." -> Ok Dotdot
      | _ -> Ok (Dir hd)
    ) >>= fun init ->
    Result.List.fold tl ~init ~f:(fun accum item ->
      match item with
      | "/" ->
        error "root directory can only occur as first item in path"
          items sexp_of_items
      | "" | "." -> Ok (Concat (accum, Dot))
      | ".." -> Ok (Concat (accum, Dotdot))
      | _ -> Ok (Concat (accum, Dir item))
    )
  )

let dir_of_items items : (abs,dir) path Or_error.t =
  match items with
  | [] -> assert false
  | "/"::items ->
    Result.List.fold items ~init:root ~f:(fun accum item ->
      match item with
      | "/" ->
        error "root directory can only occur as first item in path"
          items sexp_of_items
      | "" | "." -> Ok (Concat (accum, Dot))
      | ".." -> Ok (Concat (accum, Dotdot))
      | _ -> Ok (Concat (accum, Dir item))
    )
  | _ ->
    error "absolute path must begin with root directory"
      items sexp_of_items

let rel_file_of_items items : (rel,file) path Or_error.t =
  match items with
  | [] -> assert false
  | "/"::_ ->
    error "relative path cannot begin with root directory"
      items sexp_of_items
  | _ -> (
    let items_rev = List.rev items in
    let last = List.hd_exn items_rev in
    let items = List.rev (List.tl_exn items_rev) in
    match last with
    | "." | "" | ".." ->
      error "path cannot be treated as file" items sexp_of_items
    | _ ->
      match items with
      | [] -> Ok (File last)
      | _ ->
        rel_dir_of_items items >>| fun dir ->
        Concat (dir, File last)
  )

let file_of_items items : (abs,file) path Or_error.t =
  match items with
  | [] -> assert false
  | "/"::[] ->
    error "root directory cannot be treated as file" items sexp_of_items
  | "/"::rest-> (
    let rest_rev = List.rev rest in
    let last = List.hd_exn rest_rev in
    let rest = List.rev (List.tl_exn rest_rev) in
    match last with
    | "." | "" | ".." ->
      error "path cannot be treated as file" items sexp_of_items
    | _ ->
      dir_of_items ("/"::rest) >>| fun dir ->
      Concat (dir, File last)
  )
  | _ ->
    error "absolute path must begin with root directory"
      items sexp_of_items

let rel_dir_path s = items s >>= rel_dir_of_items
let dir_path s = items s >>= dir_of_items
let rel_file_path s = items s >>= rel_file_of_items
let file_path s = items s >>= file_of_items

let to_list = to_items

let to_string t = to_items t |> String.concat ~sep:"/"

(* TODO: check for infinite loops *)
let rec resolve_links : type a b . (a,b) path -> (a,b) path = fun x ->
  match x with
  | Root -> x
  | File _ -> x
  | Dir _ -> x
  | Link (_, target) -> resolve_links target
  | Dot -> x
  | Dotdot -> x
  | Concat (dir,relpath) -> Concat (resolve_links dir, resolve_links relpath)

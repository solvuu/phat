(** Types useful in clients' interfaces. Purpose of this module is to
    allow doing [open Phat_pure.Types] in the mli files of client
    code.

    The main [Phat_pure.Std] interface is meant to be used in
    implementations as [module Phat = Phat_pure.Std] and is not
    compatible with the goal of this module. Doing [open
    Phat_pure.Std] in an mli file isn't advisable as it brings into
    scope types [t] and [item], which are likely to conflict with
    other types.
*)

type abs = Phat_path.abs
type rel = Phat_path.rel

type file = Phat_path.file
type dir = Phat_path.dir
type link = Phat_path.link

module Phat : sig
  type name = Phat_path.name

  type ('kind,'typ) item = ('kind,'typ) Phat_path.item =
    | Root : (abs,dir) item
    | File : name -> (rel,file) item
    | Dir : name -> (rel,dir) item
    | Link : name * (_,'typ) t -> (rel,'typ) item
    | Broken_link : name * string list -> (rel, link) item
    | Dot : (rel,dir) item
    | Dotdot : (rel,dir) item

  and ('kind,'typ) t = ('kind,'typ) Phat_path.t =
    | Item : ('kind,'typ) item -> ('kind,'typ) t
    | Cons : ('kind,dir) item * (rel,'typ) t -> ('kind,'typ) t

  type 'typ of_any_kind = 'typ Phat_path.of_any_kind
  type 'typ of_any_typ = 'typ Phat_path.of_any_typ

end

type abs_file = Phat_path.abs_file
type rel_file = Phat_path.rel_file
type abs_dir = Phat_path.abs_dir
type rel_dir = Phat_path.rel_dir

(** User chosen file or directory name. By "user chosen" we mean to
    exclude reserved names such as ".", "", "..", and "/". *)
type name = private string [@@deriving sexp]

(** Indicate whether a path is absolute or relative. *)
type abs = [ `Abs ]

type rel = [ `Rel ]

(** Indicate the type of file, i.e. a regular file or directory. *)
type file = [ `File ]

type dir = [ `Dir ]
type link = [ `Link ]

type ('kind, 'typ) item =
  | Root : (abs, dir) item
  | File : name -> (rel, file) item
  | Dir : name -> (rel, dir) item
  | Link : name * (_, 'typ) t -> (rel, 'typ) item
  | Broken_link : name * string list -> (rel, link) item
  | Dot : (rel, dir) item
  | Dotdot : (rel, dir) item

and ('kind, 'typ) t =
  | Item : ('kind, 'typ) item -> ('kind, 'typ) t
  | Cons : ('kind, dir) item * (rel, 'typ) t -> ('kind, 'typ) t

type 'typ item_of_any_kind =
  [ `Abs of (abs, 'typ) item
  | `Rel of (rel, 'typ) item
  ]

type 'typ of_any_kind =
  [ `Abs of (abs, 'typ) t
  | `Rel of (rel, 'typ) t
  ]

type 'kind of_any_typ =
  [ `File of ('kind, file) t
  | `Link of ('kind, link) t
  | `Dir of ('kind, dir) t
  ]

type abs_file = (abs, file) t [@@deriving sexp, compare, equal]
type rel_file = (rel, file) t [@@deriving sexp, compare, equal]
type abs_dir = (abs, dir) t [@@deriving sexp, compare, equal]
type rel_dir = (rel, dir) t [@@deriving sexp, compare, equal]

val equal : ('kind, 'typ) t -> ('kind, 'typ) t -> bool
val compare : ('kind, 'typ) t -> ('kind, 'typ) t -> int

(** {2 Constructors} *)

(** Unix root directory "/". *)
val root : abs_dir

val file : name -> rel_file
val file_exn : string -> rel_file
val dir : name -> rel_dir
val dir_exn : string -> rel_dir
val link : name -> (_, 'o) t -> (rel, 'o) t
val link_exn : string -> (_, 'o) t -> (rel, 'o) t
val dot : rel_dir
val dotdot : rel_dir
val broken_link : name -> string list -> (rel, link) t
val broken_link_exn : string -> string list -> (rel, link) t

(** Parse a name. *)
val name : string -> name Or_error.t

val name_exn : string -> name

(** Parse an absolute directory path. *)
val abs_dir : string -> abs_dir Or_error.t

(** Parse an absolute file path. *)
val abs_file : string -> abs_file Or_error.t

(** Parse a relative directory path. *)
val rel_dir : string -> rel_dir Or_error.t

(** Parse a relative file path. *)
val rel_file : string -> rel_file Or_error.t

(** Parse an absolute or relative file path. *)
val file_of_any_kind : string -> file of_any_kind Or_error.t

(** Parse an absolute or relative dir path. *)
val dir_of_any_kind : string -> dir of_any_kind Or_error.t

(** {2 Deconstructors} *)

val last_item : (rel, 'o) t -> (rel, 'o) item
val to_list : (_, _) t -> string list
val to_string : (_, _) t -> string
val sexp_of_t : (_, _) t -> Sexp.t
val string_of_item : (_, _) item -> string

(** {2 Visitors} *)

val is_normalized : (_, _) t -> bool

(** Return true if given path has a link. Only [Link], not
    [Broken_link], is treated as a link here under the assumption that
    callers of this function will likely be checking for valid links. *)
val has_link : (_, _) t -> bool

type ('typ, 'a) map_any_kind = { map : 'kind. ('kind, 'typ) t -> 'a }

val map_any_kind : 'typ of_any_kind -> ('typ, 'a) map_any_kind -> 'a
val kind_of : (_, 'typ) t -> 'typ of_any_kind
val typ_of : ('kind, _) t -> 'kind of_any_typ

(** {2 Operators} *)

val normalize : ('kind, 'typ) t -> ('kind, 'typ) t
val concat : ('kind, dir) t -> (rel, 'typ) t -> ('kind, 'typ) t
val cons : ('k, dir) t -> (rel, 't) item -> ('k, 't) t

(** Follow all links. Returned value guaranteed not to contain any
    instance of [Link]. *)
val resolve : (abs, 'typ) t -> (abs, 'typ) t

val resolve_any_kind : ('kind, 'typ) t -> 'typ of_any_kind
val parent : ('kind, _) t -> ('kind, dir) t

module Infix : sig
  val ( / ) : ('kind, dir) t -> (rel, 'typ) t -> ('kind, 'typ) t
end

(**
This function is useful when you have two absolute paths and want to
have a relative path from one to the other.

In particular, the following property holds (at least it should):

[equal p (concat q (make_relative p ~from:q))]

The trivial solution is to go back to Root, but the algorithm
implemented tries to be (only slightly) more clever by detecting a
common prefix. This can be pretty involved because of link
indirections, so the algorithm only improves on the trivial approach
on a few number of cases.

Note that this is a function on paths only, so it doesn't try to
detect impossible cases where arguments correspond to different
objects at the same location in the filesystem.
*)
val make_relative : (abs, 'typ) t -> from:(abs, dir) t -> (rel, 'typ) t

(** Return last item in path. If path is [Item Root], the returned
    item is absolute, else it is relative. See also {!last_of_rel}.
*)
val last : (_, 'typ) t -> 'typ item_of_any_kind

(** Return last item in a relative path. *)
val last_of_rel : (rel, 'typ) t -> (rel, 'typ) item

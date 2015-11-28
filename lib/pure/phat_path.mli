(** File paths.

    Conceptually a path is a non-empty list of reserved items such as
    [Root] "/", [Dot] ".", or [Dotdot] "..", or any user chosen
    {!name} where certain characters such as slash are disallowed. The
    empty string "" is equivalent to ".", and both are parsed to
    [Dot].

    We represent such a list with a GADT {!t}, which has 2 type
    parameters:

    - 'kind: is either [abs] or [rel]. The first item in an absolute
    path is "/", the root directory. The first item in a relative path
    is guaranteed not to be "/".

    - 'typ: can be [file], [link] or [dir] and indicates the type of
    file system item identified by the path after symbolic link
    resolution. This property is held by the last item in a path,
    while every non-last item must be a directory.

    These type parameters restrict operations in sensible ways. For
    example, you can request the items under a directory but not under
    a regular file.

    Only absolute paths can resolve to an actual file or directory in
    your filesystem. A relative path cannot; it is incomplete in the
    sense that it is unknown what (absolute) directory the path is
    relative to. Said another way, an absolute path is truly a path,
    but a relative path is an abstraction, which, when given an
    absolute directory prefix, will yield a (absolute) path.

    A normalized path contains the minimum number of [Dot]s and
    [Dotdot]s possible. For an absolute path, this means there are no
    [Dot]s or [Dotdot]s. For a relative path, it means [Dot]s and
    [Dotdot]s occur only in cases where the path is: a lone [Dot], or
    a consecutive sequence of [Dotdot]s at the beginning followed by
    only named items.

    A resolved path does not contain any links. Resolving a path
    replaces links by its target. The kind of the path may change in
    the process: a relative path may become absolute after resolution
    (but not vice versa). However, resolving a path will not change
    the type of the object it identifies. If a path has the [link]
    type, then this means it represents a broken link: by convention
    broken link resolve as themselves, like regular files or
    directories.

    We have defined [equal p q] to mean [p] and [q] are the same after
    normalization, i.e. disregarding [Dot]s and [Dotdot]s that don't
    affect the final path being referred to. This is not the only
    possible choice; one might also want to disregard links and say
    paths are equal if they resolve to the same path. However, we
    believe that users of this library will often care about the link
    structure of a path, so we did not choose this definition. One can
    check this alternate definition of equality by manually calling
    [resolve] first, so there is no limitation. On the other hand, one
    might care even about the [Dot]s and [Dotdot]s, in which case you
    can call OCaml's polymorphic equality [Pervasives.equal]. Only
    paths of the same kind and type can be tested for equality, even
    though the string representations could be equal for paths not
    satisfying these criteria.

    Windows paths are not supported, but that would be a simple
    extension if anyone requests it.
*)
open Core_kernel.Std

(** User chosen file or directory name. By "user chosen" we mean to
    exclude reserved names such as ".", "", "..", and "/". *)
type name = private string [@@deriving sexp]

(** Indicate whether a path is absolute or relative. *)
type abs = [`Abs]
type rel = [`Rel]

(** Indicate the type of file, i.e. a regular file or directory. *)
type file = [`File]
type dir  = [`Dir]
type link = [`Link]

type ('kind,'typ) item =
  | Root : (abs,dir) item
  | File : name -> (rel,file) item
  | Dir : name -> (rel,dir) item
  | Link : name * (_,'typ) t -> (rel,'typ) item
  | Broken_link : name * name list -> (rel, link) item
  | Dot : (rel,dir) item
  | Dotdot : (rel,dir) item

and ('kind,'typ) t =
  | Item : ('kind,'typ) item -> ('kind,'typ) t
  | Cons : ('kind,dir) item * (rel,'typ) t -> ('kind,'typ) t

type 'typ of_any_kind = [
  | `Abs of (abs,'typ) t
  | `Rel of (rel,'typ) t
]

type 'kind of_any_typ = [
  | `File of ('kind, file) t
  | `Link of ('kind, link) t
  | `Dir of ('kind, dir) t
]

type abs_file = (abs,file) t [@@deriving sexp]
type rel_file = (rel,file) t [@@deriving sexp]
type abs_dir = (abs,dir) t [@@deriving sexp]
type rel_dir = (rel,dir) t [@@deriving sexp]

val equal : ('kind,'typ) t -> ('kind,'typ) t -> bool
val compare : ('kind,'typ) t -> ('kind,'typ) t -> int


(** {2 Constructors} *)

(** Unix root directory "/". *)
val root : abs_dir

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

val to_list : (_, _) t -> string list
val to_string : (_, _) t -> string
val sexp_of_t : (_, _) t -> Sexp.t
val string_of_item : (_, _) item -> string


(** {2 Visitors} *)

val is_normalized : (_, _) t -> bool

val has_link : (_, _) t -> bool

type ('typ, 'a) map_any_kind = { map : 'kind. ('kind, 'typ) t -> 'a }

val map_any_kind : 'typ of_any_kind -> ('typ,'a) map_any_kind -> 'a

val kind_of : (_, 'typ) t -> 'typ of_any_kind

val typ_of : ('kind, _) t -> 'kind of_any_typ


(** {2 Operators} *)

val normalize : ('kind, 'typ) t -> ('kind, 'typ) t

val concat : ('kind, dir) t -> (rel, 'typ) t -> ('kind, 'typ) t

(** Follow all links. Returned value guaranteed not to contain any
    instance of [Link]. *)
val resolve : (abs, 'typ) t -> (abs, 'typ) t

val resolve_any_kind : ('kind, 'typ) t -> 'typ of_any_kind

val parent : ('kind, _) t -> ('kind, dir) t

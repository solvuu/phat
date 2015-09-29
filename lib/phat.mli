(** File paths.

    Conceptually a path is a non-empty list of reserved items such as
    [Root] "/", [Dot] ".", or [Dotdot] "..", or any user chosen
    {!name} where certain characters such as slash are disallowed. The
    empty string "" is equivalent to ".", and both are parsed to
    [Dot].

    We represent such a list with type {!path}, which has 2 phantom
    type parameters:

    - 'kind: is either [abs] or [rel]. The first item in an absolute
    path is "/", the root directory. The first item in a relative path
    is guaranteed not to be "/".

    - 'obj: is either [file] or [dir] and indicates the type of the
    object identified by the path. This is a property of the last item
    in a path. Every non-last item must be a directory.

    These phantom types restrict operations in sensible ways. For
    example, you can request the items under a directory but not under
    a file.

    Only absolute paths can resolve to an actual file or directory in
    your filesystem. A relative path cannot; it is incomplete in the
    sense that it is unknown what (absolute) directory the path is
    relative to. Said another way, an absolute path is truly a path,
    but a relative path is an abstraction, which, when given an
    absolute directory prefix, will yield a (absolute) path. Thus,
    naming choices often exclude "abs"; it is implied that "path"
    means "absolute path".

    A normalized path contains the minimum number of [Dot]s and
    [Dotdot]s possible. For an absolute path, this means there are no
    [Dot]s or [Dotdot]s. For a relative path, it means [Dot]s and
    [Dotdot]s occur only in cases where the path is: a lone [Dot], or
    a consecutive sequence of [Dotdot]s at the beginning followed by
    only named items.

    A resolved path does not contain any links. Resolving a path means
    to replace occuring links by the path they represent. The kind of
    the path may change in the process: a relative path may become
    absolute after resolution (converse not true). However, resolving
    a path will not change the type of the object it identifies.

    Paths are semantically [equal] if they point to the same location,
    even after resolution. Only paths of the same type can be tested
    for equality. It is assumed that a relative path is always
    different from an absolute path, and a file path is always
    different from a directory path even though their string
    representations might be equal.

    Windows paths are not supported, but that would be a simple
    extension if anyone requests it.
*)
open Core.Std

(** User chosen file or directory name. By "user chosen" we mean to
    exclude reserved names such as ".", "", "..", and "/". *)
type name = private string

type abs (** absolute path, phantom type *)
type rel (** relative path, phantom type *)

type file (** regular file, phantom type *)
type dir (** directory, phantom type *)

type ('kind,'obj) item =
  | Root : (abs,dir) item
  | File : name -> (rel,file) item
  | Dir : name -> (rel,dir) item
  | Link : name * (_,'obj) path -> (rel,'obj) item
  | Dot : (rel,dir) item
  | Dotdot : (rel,dir) item

and ('kind,'obj) path =
  | Item : ('kind,'obj) item -> ('kind,'obj) path
  | Cons : ('a,dir) item * (rel,'obj) path -> ('a,'obj) path

type 'a some_kind_of_path =
  | Abs_path of (abs,'a) path
  | Rel_path of (rel,'a) path

type file_path = (abs,file) path
type dir_path = (abs,dir) path

val equal : ('absrel,'kind) path -> ('absrel,'kind) path -> bool


(** {2 Constructors} *)

(** Unix root directory "/". *)
val root : (abs, dir) path

(** Parse a name. *)
val name : string -> name Or_error.t

(** Parse an absolute directory path. *)
val dir_path : string -> (abs, dir) path Or_error.t

(** Parse an absolute file path. *)
val file_path : string -> (abs, file) path Or_error.t

(** Parse a relative directory path. *)
val rel_dir_path : string -> (rel, dir) path Or_error.t

(** Parse a relative file path. *)
val rel_file_path : string -> (rel, file) path Or_error.t


(** {2 Deconstructors} *)

val to_list : (_, _) path -> string list
val to_string : (_, _) path -> string


(** {2 Operators} *)

val normalize : ('absrel, 'kind) path -> ('absrel, 'kind) path

val concat : ('a, dir) path -> (rel, 'kind) path -> ('a, 'kind) path

(** Follow all links. Returned value guaranteed not to contain any
    instance of [Link]. *)
val resolve : ('k, 'o) path -> 'o some_kind_of_path

val parent : ('absrel, _) path -> ('absrel, dir) path

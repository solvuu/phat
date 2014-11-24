(** File paths.

    Conceptually a path is a non-empty list of reserved items such as
    [Root] "/", [Dot] ".", or [Dotdot] "..", or any user chosen
    {!name} where certain characters such as slash are disallowed. The
    empty string "" is equivalent to ".", and both are parsed to
    [Dot].

    We represent such a list with type {!path}, which has 2 phantom
    type parameters:

    - `absrel: is either [abs] or [rel]. The first item in an absolute
    path is "/", the root directory. The first item in a relative path
    is guaranteed not to be "/".

    - 'kind: is either [file] or [dir] and indicates the type of the
    path. This is a property of the last item in a path. Every
    non-last item must be a directory.

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
    [Dotdot]s occur only in cases where the path is: a lone [Dot],
    only one or more [Dotdot]s, or the first item is a directory
    followed only by one or more [Dotdot]s.

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


(** {2 Constructors, Converters} *)

(** Unix root directory "/". *)
val root : (abs,dir) path

(** Parse a name. *)
val name : string -> name Or_error.t

(** Parse an absolute directory path. *)
val dir_path : string -> (abs,dir) path Or_error.t

(** Parse an absolute file path. *)
val file_path : string -> (abs,file) path Or_error.t

(** Parse a relative directory path. *)
val rel_dir_path : string -> (rel,dir) path Or_error.t

(** Parse a relative file path. *)
val rel_file_path : string -> (rel,file) path Or_error.t

val to_list : (_,_) path -> string list

val to_string : (_,_) path -> string


(** {2 Path Manipulation} *)

val concat : ('absrel,dir) path -> (rel,'kind) path -> ('absrel,'kind) path

(** Follow all links. Returned value guaranteed not to contain any
    instance of [Link]. *)
val resolve_links : ('absrel, 'kind) path -> ('absrel,'kind) path

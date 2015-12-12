open Core.Std
open Phat_pure.Core2
open Async.Std
module Path = Phat_path

module DOR = Deferred.Or_error

type exists_answer = [
  | `Yes
  | `Yes_modulo_links
  | `Yes_as_other_object
  | `Unknown
  | `No
]

let exists_answer (x : [`Yes | `No | `Unknown]) = (x :> exists_answer)

let conj (x : exists_answer) y =
  match x, y with
  | `Yes, _ -> y
  | _, `Yes -> x
  | `Yes_modulo_links, _ -> y
  | _, `Yes_modulo_links -> x
  | `Yes_as_other_object, _ -> y
  | _, `Yes_as_other_object -> x
  | _, `Unknown -> `Unknown
  | `Unknown, _ -> `Unknown
  | _, `No -> `No


let and_check f x e =
  match e with
  | `Yes
  | `Yes_modulo_links
  | `Yes_as_other_object ->
    f x >>| conj e
  | `No -> return `No
  | `Unknown -> return `Unknown


(** Async's [file_exists] has a
    {{:https://github.com/janestreet/async_unix/issues/6}bug} that
    doesn't allow setting follow_symlinks to false. *)
let file_exists x =
  In_thread.run (fun () -> Core.Std.Sys.file_exists ~follow_symlinks:false x)

let is_file x = Sys.is_file ~follow_symlinks:false x
let is_directory x = Sys.is_directory ~follow_symlinks:false x

let is_link p =
  file_exists p >>= function
  | `No
  | `Unknown as x ->
    return x
  | `Yes ->
    Unix.lstat p >>| fun {Unix.Stats.kind; _} ->
    match kind with
    | `Link ->
      `Yes
    | `File | `Directory | `Char | `Block | `Fifo | `Socket ->
      `No


let exists_as_file p =
  file_exists p >>= function
  | `No | `Unknown as x -> return x
  | `Yes -> (
      is_file p >>= function
      | `Yes | `Unknown as x -> return x
      | `No -> (
          is_directory p >>= function
          | `Yes -> return `Yes_as_other_object
          | `Unknown -> return `Unknown
          | `No -> (
              is_link p >>= function
              | `No -> return `Yes_as_other_object
              | `Unknown -> return `Unknown
              | `Yes -> (
                  Unix.stat p >>| function
                  | { Unix.Stats.kind = `File } -> `Yes_modulo_links
                  | _ -> `Yes_as_other_object
                )
            )
        )
    )

let exists_as_directory p =
  file_exists p >>= function
  | `No | `Unknown as x -> return x
  | `Yes -> (
      is_directory p >>= function
      | `Yes | `Unknown as x -> return x
      | `No -> (
          is_file p >>= function
          | `Yes -> return `Yes_as_other_object
          | `Unknown -> return `Unknown
          | `No -> (
              is_link p >>= function
              | `No -> return `Yes_as_other_object
              | `Unknown -> return `Unknown
              | `Yes -> (
                  Unix.stat p >>| function
                  | { Unix.Stats.kind = `Directory } -> `Yes_modulo_links
                  | _ -> `Yes_as_other_object
                )
            )
        )
    )


let exists_as_link p target =
  file_exists p >>= function
  | `No | `Unknown as x -> return x
  | `Yes -> (
      is_link p >>= function
      | `No -> return `Yes_as_other_object
      | `Unknown -> return `Unknown
      | `Yes ->
        Unix.readlink p >>| fun target_on_fs ->
        if target = target_on_fs then `Yes
        else `Yes_as_other_object
    )

let rec exists
  : type typ. (Path.abs, typ) Path.t -> exists_answer Deferred.t
  =
  let open Path in
  function
  | Item Root -> (file_exists "/" :> exists_answer Deferred.t)
  | Cons (Root, p_rel) ->
    file_exists "/"
    >>| exists_answer
    >>= and_check (exists_rel_path root) p_rel

and exists_item
  : type typ. Path.abs_dir -> (Path.rel,typ) Path.item -> exists_answer Deferred.t
  =
  fun p_abs item ->
    match item with
    | Path.Dot -> return `Yes
    | Path.Dotdot -> return `Yes
    | Path.File _ ->
      exists_as_file (Path.to_string (Path.cons p_abs item))
    | Path.Dir _ ->
      exists_as_directory (Path.to_string (Path.cons p_abs item))
    | Path.Broken_link (_, target) ->
      let target_does_not_exist target =
        let target_as_str = Filename.of_parts target in
        file_exists (
          match target with
          | "/" :: _ -> target_as_str
          | _ -> Filename.concat (Path.to_string p_abs) target_as_str
        )
        >>| (function
            | `Yes -> `No
            | `No -> `Yes
            | `Unknown -> `Unknown)
        >>| exists_answer
      in
      Path.(exists_as_link (to_string (cons p_abs item)) (Filename.of_parts target))
      >>= and_check target_does_not_exist target
    | Path.Link (_, target) ->
      let target_exists target =
        match Path.kind_of target with
        | `Abs p -> exists p
        | `Rel p -> exists_rel_path p_abs p
      in
      Path.(exists_as_link (to_string (cons p_abs item)) (to_string target))
      >>= and_check target_exists target

and exists_rel_path
  : type typ. Path.abs_dir -> (Path.rel,typ) Path.t -> exists_answer Deferred.t
  = fun p_abs p_rel ->
    match p_rel with
    | Path.Item x -> exists_item p_abs x
    | Path.Cons (x, y) ->
      exists_item p_abs x
      >>= and_check (exists_rel_path (Path.cons p_abs x)) y


let lstat p : Unix.Stats.t Or_error.t Deferred.t =
  try_with (fun () -> Unix.lstat (Path.to_string p)) >>|
  Or_error.of_exn_result >>|
  Or_error.tag_loc _here_

let wrap_unix loc f =
  try_with f >>|
  Or_error.of_exn_result >>|
  Or_error.tag_loc loc

let unix_mkdir p =
  wrap_unix _here_ (fun () -> Unix.mkdir (Path.to_string p))

let unix_symlink link_path ~targets:link_target =
  wrap_unix _here_ (fun () ->
      Unix.symlink ~dst:(Path.to_string link_path) ~src:(Path.to_string link_target)
    )

let rec mkdir
  : Path.abs_dir -> unit Or_error.t Deferred.t
  = fun p ->
    match p with
    | Path.Item Path.Root -> return (Ok ())
    | Path.Cons (Path.Root, rel_p) ->
      mkdir_aux Path.root rel_p

and mkdir_aux
  : Path.abs_dir -> Path.rel_dir -> unit Or_error.t Deferred.t
  = fun p_abs p_rel ->
    let maybe_build p = function
      | `Yes | `Yes_modulo_links ->
        DOR.return ()
      | `Yes_as_other_object ->
        let msg = sprintf "Path %s already exists and is not a directory" (Path.to_string p) in
        DOR.error_string msg
      | `No -> unix_mkdir p
      | `Unknown ->
        let msg = sprintf "Insufficient permissions to create %s" (Path.to_string p) in
        DOR.error_string msg
    in
    match p_rel with
    | Path.Item (Path.Dir _) -> (
        let p = Path.concat p_abs p_rel in
        exists p >>= maybe_build p
      )
    | Path.Item Path.Dot -> return (Ok ())
    | Path.Item Path.Dotdot -> return (Ok ())
    | Path.Item (Path.Link (_, dir)) -> (
        let p = Path.concat p_abs p_rel in
        unix_symlink p ~targets:dir >>=? fun () ->
        match Path.kind_of dir with
        | `Rel dir -> mkdir_aux p_abs dir
        | `Abs dir -> mkdir dir
      )
    | Path.Cons (Path.Dir n, p_rel') -> (
        let p_abs' = Path.cons p_abs (Path.Dir n) in
        exists p_abs' >>= maybe_build p_abs' >>=? fun () ->
        mkdir_aux p_abs' p_rel'
      )
    | Path.Cons (Path.Link (_, dir) as l, p_rel') -> (
        unix_symlink (Path.cons p_abs l) ~targets:dir >>=? fun () ->
        match Path.kind_of dir with
        | `Rel dir ->
          mkdir_aux p_abs (Path.concat dir p_rel')
        | `Abs dir ->
          mkdir (Path.concat dir p_rel')
      )
    | Path.Cons (Path.Dot, p_rel') -> mkdir_aux p_abs p_rel'
    | Path.Cons (Path.Dotdot, p_rel') ->
      mkdir_aux (Path.parent p_abs) p_rel'

let rec find_item item path =
  match path with
  | [] -> return None
  | dir::path ->
     let x = Path.cons dir item in
     exists x >>= function
     | `Yes | `Yes_modulo_links -> return (Some x)
     | `Unknown | `No | `Yes_as_other_object -> find_item item path

let rec reify
  : type o.
    (Path.abs, o) Path.t ->
    (Path.abs, o) Path.t Deferred.Or_error.t
  = fun p ->
    exists p >>= function
    | `No | `Unknown ->
      Deferred.Or_error.errorf "No such file or directory %s" (Path.to_string p)
    | `Yes_as_other_object ->
      Deferred.Or_error.errorf "Path %s is not of the expected type" (Path.to_string p)
    | `Yes | `Yes_modulo_links ->
      match p with
      | Path.Item Path.Root -> Deferred.Or_error.return p
      | Path.Cons (Path.Root, p_rel) ->
        reify_aux Path.root p_rel

and reify_aux
  : type o.
    (Path.abs, Path.dir) Path.t ->
    (Path.rel, o) Path.t ->
    (Path.abs, o) Path.t Deferred.Or_error.t
  = fun p_abs p_rel ->
    match p_rel with
    | Path.Item i ->
      reify_aux_item p_abs i
    | Path.Cons (h, t) ->
      reify_aux_item p_abs h >>=? fun p_abs' ->
      reify_aux p_abs' t

and reify_aux_item
  : type o.
    (Path.abs, Path.dir) Path.t ->
    (Path.rel, o) Path.item ->
    (Path.abs, o) Path.t Deferred.Or_error.t
  = fun p_abs item ->
    let p = Path.cons p_abs item in
    let p_str = Path.to_string p in
    match item with
    | Path.File n -> (
        is_link p_str >>= function
        | `Yes -> reify_link Path.file_of_any_kind p_abs n p_str
        | `No ->  Deferred.Or_error.return p
        | `Unknown -> assert false (* [EXIST] *)
      )
    | Path.Dir n -> (
        is_link p_str >>= function
        | `Yes -> reify_link Path.dir_of_any_kind p_abs n p_str
        | `No ->  Deferred.Or_error.return p
        | `Unknown -> assert false (* [EXIST] *)
      )
    | Path.Link (n, target) -> (
        let k f p =
          reify p >>=? fun p ->
          Path.cons p_abs (Path.Link (n, f p))
          |> Deferred.Or_error.return
        in
        match Path.kind_of target with
          | `Abs p -> k ident p
          | `Rel p_rel ->
            k
              (Path.make_relative ~from:p_abs)
              Path.(concat p_abs p_rel)
      )
    | Path.Broken_link _ -> Deferred.Or_error.return p
    | Path.Dot -> Deferred.Or_error.return p
    | Path.Dotdot -> Deferred.Or_error.return p


and reify_link
  : type o.
    (string -> o Path.of_any_kind Or_error.t) ->
    (Path.abs, Path.dir) Path.t ->
    Path.name ->
    string ->
    (Path.abs, o) Path.t Deferred.Or_error.t
  = fun parse p_abs n p_str ->
    Unix.readlink p_str >>= fun target_str ->
    try_with Unix.(fun () -> stat p_str) >>= function
    | Ok stats -> ( (* hence the target exists! *)
        let target =
          parse target_str
          |> ok_exn (* safe because the target exists, meaning the
                       target path is syntactically valid *)
        in
        let abs_target = match target with
          | `Abs p -> p
          | `Rel p ->
            Path.concat p_abs p
        in
        reify abs_target >>=? fun reified_abs_target ->
        let link = match target with
          | `Abs _ -> Path.Link (n, reified_abs_target)
          | `Rel _ ->
            Path.(Link (n, make_relative reified_abs_target ~from:p_abs))
        in
        Deferred.Or_error.return (Path.cons p_abs link)
      )
    | Error _ ->
      Deferred.Or_error.errorf
        "Expected a valid target for link %s but it is broken"
        p_str



(* NB: the uses of normalize in the function on the relative paths
   that are recursively built are safe wrt links. This is true because
   relative paths created by [fold] never have ".." in it. This is
   easy because we don't follow links. *)
let rec fold_aux p_abs p_rel obj ~f ~init =
  let dir = Path.(concat p_abs p_rel) in
  match obj with
  | `File file -> f init (`File (Path.cons p_rel file |> Path.normalize))
  | `Broken_link bl ->  f init (`Broken_link Path.(cons p_rel bl |> normalize))
  | `Dir subdir_item ->
    let subdir_rel = Path.(cons p_rel subdir_item |> normalize) in
    let subdir = Path.cons dir subdir_item in
    let subdir_as_str = Path.to_string subdir in
    f init (`Dir subdir_rel) >>= fun accu -> (* prefix traversal *)
    Sys.readdir subdir_as_str >>= fun dir_contents ->
    Deferred.Array.fold dir_contents ~init:accu ~f:(fun accu obj ->
        let obj_as_str = Filename.concat subdir_as_str obj in
        let n = Path.name_exn obj in
        Unix.(lstat obj_as_str) >>= fun stats ->
        (
          match stats.Unix.Stats.kind with
          | `File | `Block | `Char | `Fifo | `Socket ->
            return (`File (Path.File n))

          | `Directory ->
            return (`Dir (Path.Dir n))

          | `Link ->
            discover_link subdir subdir_as_str n
        )
        >>= fun item ->
        fold_aux p_abs (Path.cons p_rel subdir_item) item ~f ~init:accu
      )

and discover_link dir dir_as_str n =
  let link_as_str = Filename.concat dir_as_str (n : Path.name :> string) in
  try_with Unix.(fun () -> stat link_as_str) >>= function
  | Ok stats -> (
      let make_link item cons =
        reify (Path.cons dir item) >>| ok_exn (* We could do a stat on the
                                                 link, so reify has no reason to fail *)
        >>| function
        | Path.Item _ -> assert false
        | Path.Cons (_, p_rel) -> cons (Path.last_item p_rel)
      in
      match stats.Unix.Stats.kind with
      | `File | `Block | `Char | `Fifo | `Socket ->
        make_link (Path.File n) (fun x -> `File x)

      | `Directory ->
        make_link (Path.Dir  n) (fun x -> `Dir x)

      | `Link ->
        (* should not happen: Unix.stat resolves to a
                     link-free path *)
        assert false
    )
  | Error _ ->
    Unix.readlink link_as_str >>| fun target ->
    let bl = Path.Broken_link (n, String.split ~on:'/' target) in
    `Broken_link bl

let fold start ~f ~init =
  exists start >>= function
  | `Yes | `Yes_modulo_links ->
    fold_aux start Path.(Item (Path.Dot)) (`Dir Path.Dot) ~f ~init >>| fun r ->
    Ok r

  | `No | `Unknown | `Yes_as_other_object ->
    errorh _here_ "Directory does not exist" () sexp_of_unit
    |> return

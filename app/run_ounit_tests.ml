open Core.Std
open Async.Std
open OUnit2
open Phat_async_unix.Std

(** Like OUnit.(>::), but the test function returns a Deferred. *)
let (>::=) test_name (f : test_ctxt -> unit Deferred.t) : test =
  test_name >:: (
    fun test_ctxt ->
    Thread_safe.block_on_async_exn (fun () -> f test_ctxt)
  )

let new_name =
  let k = ref (- 1) in
  fun () ->
    incr k ;
    "foo" ^ (string_of_int !k)
    |> Path.name
    |> ok_exn

let rec random_rel_dir_item ?(no_link = false) ?root ?level () =
  let bottom = match level with
    | Some l -> l <= 0
    | None -> false
  in
  match Random.bool () || bottom, Random.float 1. > 0.2 with
  | true, b ->
    if no_link || b then
      Path.Dir (new_name ())
    else (
      Path.map_any_kind (random_dir_path ~no_link ?root ?level ()) { Path.map = fun p ->
          Path.Link (new_name (), p)
        }
    )
  | false, true -> Path.Dot
  | false, false -> Path.Dotdot

and random_rel_file_item ?(no_link = false) ?root ?level () =
  if no_link || Random.float 1. > 0.1 then
    Path.File (new_name ())
  else (
    Path.map_any_kind (random_file_path ~no_link ?root ?level ()) { Path.map = fun p ->
        Path.Link (new_name (), p)
      }
  )

and random_dir_path ?no_link ?root ?level () =
  match Random.bool () with
  | true -> Path.Abs_path (random_abs_dir_path ?no_link ?root ?level ())
  | false -> Path.Rel_path (random_rel_dir_path ?no_link ?root ?level ())

and random_file_path ?no_link ?root ?level () =
  match Random.bool () with
  | true -> Path.Abs_path (random_abs_file_path ?no_link ?root ?level ())
  | false -> Path.Rel_path (random_rel_file_path ?no_link ?root ?level ())

and random_abs_dir_path ?no_link ?(root = Path.root) ?level () =
  Path.concat root (random_rel_dir_path ?no_link ~root ?level ())

and random_rel_dir_path ?no_link ?root ?level () =
  let d = random_rel_dir_item ?no_link ?root ?level () in
  if Random.bool () then
    let level = Option.map level ~f:(fun i -> i - 1) in
    Path.Cons (d, random_rel_dir_path ?no_link ?root ?level ())
  else
    Path.Item d

and random_abs_file_path ?no_link ?(root = Path.root) ?level () =
  Path.concat root (random_rel_file_path ?no_link ~root ?level ())

and random_rel_file_path ?no_link ?root ?level () =
  Path.concat
    (random_rel_dir_path ?no_link ?root ?level:(Option.map level ~f:(fun x -> x - 1)) ())
    (Path.Item (random_rel_file_item ?no_link ?root ?level ()))

(* [random_path_resolving_to p] generates a path that resolves to what
   [p] resolves (the name is a tiny bit misleading) *)
and random_path_resolving_to ?root ?level p () =
  let link = Path.map_any_kind p { Path.map = fun p -> Path.Link (new_name (), p) } in
  if Random.float 1. < 0.1 then
    Path.Rel_path (Path.Item link)
  else
    match random_dir_path ?root ?level () with
    | Path.Abs_path dir -> Path.(Abs_path (concat dir (Item link)))
    | Path.Rel_path dir -> Path.(Rel_path (concat dir (Item link)))

and random_path_with_link ?root ?level () =
  random_path_resolving_to (random_dir_path ?root ?level ())

let not_names = [
  "foo/" ;
  "/foo" ;
  "foo/bar" ;
  "." ;
  ".." ;
  "" ;
]

let name_constructor _ =
  List.iter not_names ~f:(fun n ->
      match Path.name n with
      | Ok _ ->
        let msg = sprintf "String %s was accepted as a valid name" n in
        assert_failure msg
      | Error _ -> ()
    )

let sexp_serialization _ =
  let check path_of_sexp p =
    try
      let p' = path_of_sexp (Path.sexp_of_t p) in
      assert_equal ~printer:Path.to_string p p'
    with Failure msg ->
      let msg =
        sprintf
          "Deserialization failed\nPath:\n%s\nSexp:\n%s\nBacktrace:\n%s\nError:\n%s"
          (Path.to_string p)
          (Sexp.to_string_hum (Path.sexp_of_t p))
          (Printexc.get_backtrace ())
          msg
      in
      assert_failure msg
  in
  for _ = 1 to 1000 do
    check Path.dir_path_of_sexp (random_abs_dir_path ()) ;
    check Path.file_path_of_sexp (random_abs_file_path ())
  done

let normalization _ =
  let check p =
    let p_norm = Path.normalize p in
    let msg =
      sprintf
        "Path %s was normalized into %s which is not normalized"
        (Path.to_string p)
        (Path.to_string p_norm)
    in
    assert_bool msg (Path.is_normalized p_norm)
  in
  for _ = 1 to 1000 do
    match random_dir_path () with
    | Path.Abs_path dir -> check dir
    | Path.Rel_path dir -> check dir
  done

let normalization_is_idempotent _ =
  let check p =
    let p_norm = Path.normalize p in
    let p_norm_norm = Path.normalize p_norm in
    let msg =
      sprintf
        "Path %s was normalized into %s which was normalized into %s"
        (Path.to_string p)
        (Path.to_string p_norm)
        (Path.to_string p_norm_norm)
    in
    assert_bool msg (p_norm = p_norm_norm)
  in
  for _ = 1 to 1000 do
    Path.map_any_kind (random_dir_path ()) { Path.map = check }
  done

let resolution_for_eventually_abs_paths _ =
  let failure p_ref p p_res =
    let msg =
      sprintf
        "Path %s was resolved into %s instead of %s"
        (Sexp.to_string_hum (Path.sexp_of_t p))
        (Sexp.to_string_hum (Path.sexp_of_t p_res))
        (Sexp.to_string_hum (Path.sexp_of_t p_ref))
    in
    assert_failure msg
  in
  let check p_ref p p_res =
    if p_ref <> p_res then failure p_ref p p_res
  in
  for _ = 1 to 1000 do
    let p_ref = random_abs_dir_path ~no_link:true () in
    match random_path_resolving_to (Path.Abs_path p_ref) () with
    | Path.Abs_path p -> check p_ref p (Path.resolve p)
    | Path.Rel_path p ->
      match Path.resolve_any p with
      | Path.Abs_path p_res -> check p_ref p p_res
      | Path.Rel_path p_res -> failure p_ref p p_res
  done

let resolution_is_identity_for_paths_without_links _ =
  let failure dir dir' =
    let msg =
      sprintf
        "Path %s was resolved into %s"
        (Sexp.to_string_hum (Path.sexp_of_t dir))
        (Sexp.to_string_hum (Path.sexp_of_t dir'))
    in
    assert_failure msg
  in
  let check dir dir' =
    if dir <> dir' then failure dir dir'
  in
  for _ = 1 to 1000 do
    match random_dir_path ~no_link:true () with
    | Path.Abs_path dir -> check dir (Path.resolve dir)
    | Path.Rel_path dir ->
      match Path.resolve_any dir with
      | Path.Abs_path dir' -> failure dir dir'
      | Path.Rel_path dir' -> check dir dir'
  done

let resolution_eliminates_links _ =
  let check_aux p p_res =
    let msg =
      sprintf
        "Path %s was resolved into %s"
        (Sexp.to_string_hum (Path.sexp_of_t p))
        (Sexp.to_string_hum (Path.sexp_of_t p_res))
    in
    assert_bool msg (not (Path.has_link p_res))
  in
  let check p = Path.map_any_kind (Path.resolve_any p) { Path.map = fun p_res -> check_aux p p_res } in
  for _ = 1 to 1000 do
    Path.map_any_kind (random_dir_path ()) { Path.map = check }
  done

let create_test_directory path =
  let f x = Filename.concat path x in
  Unix.mkdir (f "foo") >>= fun () ->
  Unix.mkdir (f "foo/bar") >>= fun () ->
  Writer.save (f "foo/bar/baz") ~contents:"baz" >>= fun () ->
  Unix.symlink ~src:"foo/bar/baz" ~dst:(f "qux")

let filesys_exists ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Path.dir_path tmpdir) in
  let check p =
    Filesys.exists (Path.concat tmpdir_path p) >>| function
    | `Yes -> ()
    | `Unknown
    | `No ->
       let msg =
         sprintf
           "Failed to detect path %s"
           (Sexp.to_string_hum (Path.sexp_of_t p))
       in
       assert_failure msg
  in
  let foo = Path.(Item (Dir (name_exn "foo"))) in
  let foo_bar = Path.(concat foo (Item (Dir (name_exn "bar")))) in
  let foo_bar_baz = Path.(concat foo_bar (Item (File (name_exn "baz")))) in
  let qux = Path.(Item (Link (name_exn "qux", foo_bar_baz))) in
  create_test_directory tmpdir >>= fun () ->
  check foo >>= fun () ->
  check foo_bar >>= fun () ->
  check foo_bar_baz >>= fun () ->
  check qux


let filesys_mkdir ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Path.dir_path tmpdir) in
  List.init 1000 ~f:(fun _ -> ()) |>
  Deferred.List.iter ~f:(fun () ->
    let p = Path.concat tmpdir_path (random_rel_dir_path ~root:tmpdir_path ~level:0 ()) in
    (
      Filesys.mkdir p >>= function
      | Ok () -> (
          Filesys.exists p >>| function
          | `Yes -> ()
          | `No
          | `Unknown ->
            let msg =
              sprintf
                "Filesys.mkdir failed to create path %s"
                (Sexp.to_string_hum (Path.sexp_of_t p))
            in
            assert_failure msg
        )
      | Error e ->
        let msg =
          sprintf
            "Filesys.mkdir failed to create path %s: %s"
            (Sexp.to_string_hum (Path.sexp_of_t p))
            (Sexp.to_string_hum (Error.sexp_of_t e))
        in
        return (ignore (assert_failure msg))
    ) >>= fun () ->
    Sys.command_exn (sprintf "rm -rf %s ; mkdir -p %s" tmpdir tmpdir)
  )

let filesys_mkdir_cycles ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Path.dir_path tmpdir) in
  let rec foo_bar = Path.(Item (Link (name_exn "bar", baz_qux)))
  and baz_qux = Path.(Item (Link (name_exn "qux", foo_bar)))
  in
  let p = Path.concat tmpdir_path foo_bar in
  Filesys.mkdir p >>= function
  | Ok () ->
    Filesys.exists p >>| fun file_exists ->
    if file_exists <> `Yes then (
      assert_failure "Filesys.mkdir failed to create the cyclic path correctly."
    )
  | Error e ->
    let msg =
      sprintf
        "Filesys.mkdir failed to create cyclic path: %s"
        (Sexp.to_string_hum (Error.sexp_of_t e))
    in
    assert_failure msg

let suite = "Phat test suite" >::: [
    "Name constructor" >:: name_constructor ;
    "Sexp serialization" >:: sexp_serialization ;
    "Normalization" >:: normalization ;
    "Normalization is idempotent" >:: normalization_is_idempotent ;
    "Resolution eliminates links" >:: resolution_eliminates_links ;
    "Resolution is the identity for paths without links" >:: resolution_is_identity_for_paths_without_links ;
    "Resolution for eventually abs paths" >:: resolution_for_eventually_abs_paths ;
    "Exists test" >::= filesys_exists ;
    "Create dir paths" >::= filesys_mkdir ;
    "Create dirs with cycles" >::= filesys_mkdir_cycles ;
  ]


let () = run_test_tt_main suite

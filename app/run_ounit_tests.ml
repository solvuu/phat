open Core.Std
open OUnit2
open Phat_unix.Std
open Path

let new_name =
  let k = ref (- 1) in
  fun () ->
    incr k ;
    "foo" ^ (string_of_int !k)
    |> name
    |> ok_exn

let rec random_rel_dir_item ?(no_link = false) ?root ?level () =
  let bottom = match level with
    | Some l -> l <= 0
    | None -> false
  in
  match Random.bool () || bottom, Random.float 1. > 0.2 with
  | true, b ->
    if no_link || b then
      Dir (new_name ())
    else (
      map_any_kind (random_dir_path ~no_link ?root ?level ()) { map = fun p ->
          Link (new_name (), p)
        }
    )
  | false, true -> Dot
  | false, false -> Dotdot

and random_dir_path ?no_link ?root ?level () =
  match Random.bool () with
  | true -> Abs_path (random_abs_dir_path ?no_link ?root ?level ())
  | false -> Rel_path (random_rel_dir_path ?no_link ?root ?level ())

and random_abs_dir_path ?no_link ?(root = root) ?level () =
  concat root (random_rel_dir_path ?no_link ~root ?level ())

and random_rel_dir_path ?no_link ?root ?level () =
  let d = random_rel_dir_item ?no_link ?root ?level () in
  if Random.bool () then
    let level = Option.map level ~f:(fun i -> i - 1) in
    Cons (d, random_rel_dir_path ?no_link ?root ?level ())
  else
    Item d

(* [random_path_resolving_to p] generates a path that resolves to what
   [p] resolves (the name is a tiny bit misleading) *)
and random_path_resolving_to ?root ?level p () =
  let link = map_any_kind p { map = fun p -> Link (new_name (), p) } in
  if Random.float 1. < 0.1 then
    Rel_path (Item link)
  else
    match random_dir_path ?root ?level () with
    | Abs_path dir -> Abs_path (concat dir (Item link))
    | Rel_path dir -> Rel_path (concat dir (Item link))

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

let normalization _ =
  let check p =
    let p_norm = normalize p in
    let msg =
      sprintf
        "Path %s was normalized into %s which is not normalized"
        (Path.to_string p)
        (Path.to_string p_norm)
    in
    assert_bool msg (is_normalized p_norm)
  in
  for _ = 1 to 1000 do
    match random_dir_path () with
    | Abs_path dir -> check dir
    | Rel_path dir -> check dir
  done

let normalization_is_idempotent _ =
  let check p =
    let p_norm = normalize p in
    let p_norm_norm = normalize p_norm in
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
    map_any_kind (random_dir_path ()) { map = check }
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
    match random_path_resolving_to (Abs_path p_ref) () with
    | Abs_path p -> check p_ref p (resolve p)
    | Rel_path p ->
      match resolve_any p with
      | Abs_path p_res -> check p_ref p p_res
      | Rel_path p_res -> failure p_ref p p_res
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
    | Abs_path dir -> check dir (resolve dir)
    | Rel_path dir ->
      match resolve_any dir with
      | Abs_path dir' -> failure dir dir'
      | Rel_path dir' -> check dir dir'
  done

let resolution_eliminates_links _ =
  let check_aux p p_res =
    let msg =
      sprintf
        "Path %s was resolved into %s"
        (Sexp.to_string_hum (Path.sexp_of_t p))
        (Sexp.to_string_hum (Path.sexp_of_t p_res))
    in
    assert_bool msg (not (has_link p_res))
  in
  let check p = map_any_kind (resolve_any p) { map = fun p_res -> check_aux p p_res } in
  for _ = 1 to 1000 do
    map_any_kind (random_dir_path ()) { map = check }
  done

let create_test_directory path =
  let f x = Filename.concat path x in
  Unix.mkdir (f "foo") ;
  Unix.mkdir (f "foo/bar") ;
  Out_channel.write_all (f "foo/bar/baz") ~data:"baz" ;
  Unix.symlink ~src:"foo/bar/baz" ~dst:(f "qux")

let filesys_exists ctx =
  let open Path in
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (dir_path tmpdir) in
  let check p =
    if not (Filesys.exists (concat tmpdir_path p) = `Yes) then (
      let msg =
        sprintf
          "Failed to detect path %s"
          (Sexp.to_string_hum (Path.sexp_of_t p))
      in
      assert_failure msg
    )
  in
  let foo = Item (Dir (name_exn "foo")) in
  let foo_bar = concat foo (Item (Dir (name_exn "bar"))) in
  let foo_bar_baz = concat foo_bar (Item (File (name_exn "baz"))) in
  let qux = Item (Link (name_exn "qux", foo_bar_baz)) in
  create_test_directory tmpdir ;
  check foo ;
  check foo_bar ;
  check foo_bar_baz ;
  check qux


let filesys_mkdir ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx ^ "_delme" in
  let tmpdir_path = ok_exn (Path.dir_path tmpdir) in
  for _ = 1 to 1000 do
    let p = concat tmpdir_path (random_rel_dir_path ~root:tmpdir_path ~level:0 ()) in
    (
      match Filesys.mkdir p with
      | Ok () ->
        if Filesys.exists p <> `Yes then (
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
        assert_failure msg
    ) ;
    Sys.command_exn (sprintf "tree %s >> delme ; rm -rf %s ; mkdir -p %s" tmpdir tmpdir tmpdir)
  done

let suite = "Phat test suite" >::: [
    "Name constructor" >:: name_constructor ;
    "Normalization" >:: normalization ;
    "Normalization is idempotent" >:: normalization_is_idempotent ;
    "Resolution eliminates links" >:: resolution_eliminates_links ;
    "Resolution is the identity for paths without links" >:: resolution_is_identity_for_paths_without_links ;
    "Resolution for eventually abs paths" >:: resolution_for_eventually_abs_paths ;
    "Exists test" >:: filesys_exists ;
    "Create dir paths" >:: filesys_mkdir ;
  ]


let () = run_test_tt_main suite

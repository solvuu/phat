open Core.Std
open Async.Std
open OUnit2
module Phat = Phat_async_unix.Std

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
    |> Phat.name
    |> ok_exn

let rec random_rel_dir_item ?(no_link = false) ?root ?level () =
  let bottom = match level with
    | Some l -> l <= 0
    | None -> false
  in
  match Random.bool () || bottom, Random.float 1. > 0.2 with
  | true, b ->
    if no_link || b then
      Phat.Dir (new_name ())
    else (
      Phat.map_any_kind (random_dir_path ~no_link ?root ?level ()) { Phat.map = fun p ->
          Phat.Link (new_name (), p)
        }
    )
  | false, true -> Phat.Dot
  | false, false -> Phat.Dotdot

and random_rel_file_item ?(no_link = false) ?root ?level () =
  if no_link || Random.float 1. > 0.1 then
    Phat.File (new_name ())
  else (
    Phat.map_any_kind (random_file_path ~no_link ?root ?level ()) { Phat.map = fun p ->
        Phat.Link (new_name (), p)
      }
  )

and random_dir_path ?no_link ?root ?level () =
  match Random.bool () with
  | true -> Phat.Abs (random_abs_dir_path ?no_link ?root ?level ())
  | false -> Phat.Rel (random_rel_dir_path ?no_link ?root ?level ())

and random_file_path ?no_link ?root ?level () =
  match Random.bool () with
  | true -> Phat.Abs (random_abs_file_path ?no_link ?root ?level ())
  | false -> Phat.Rel (random_rel_file_path ?no_link ?root ?level ())

and random_abs_dir_path ?no_link ?(root = Phat.root) ?level () =
  Phat.concat root (random_rel_dir_path ?no_link ~root ?level ())

and random_rel_dir_path ?no_link ?root ?level () =
  let d = random_rel_dir_item ?no_link ?root ?level () in
  if Random.bool () then
    let level = Option.map level ~f:(fun i -> i - 1) in
    Phat.Cons (d, random_rel_dir_path ?no_link ?root ?level ())
  else
    Phat.Item d

and random_abs_file_path ?no_link ?(root = Phat.root) ?level () =
  Phat.concat root (random_rel_file_path ?no_link ~root ?level ())

and random_rel_file_path ?no_link ?root ?level () =
  Phat.concat
    (random_rel_dir_path ?no_link ?root ?level:(Option.map level ~f:(fun x -> x - 1)) ())
    (Phat.Item (random_rel_file_item ?no_link ?root ?level ()))

(* [random_path_resolving_to p] generates a path that resolves to what
   [p] resolves (the name is a tiny bit misleading) *)
and random_path_resolving_to ?root ?level p () =
  let link = Phat.map_any_kind p { Phat.map = fun p -> Phat.Link (new_name (), p) } in
  if Random.float 1. < 0.1 then
    Phat.Rel (Phat.Item link)
  else
    match random_dir_path ?root ?level () with
    | Phat.Abs dir -> Phat.(Abs (concat dir (Item link)))
    | Phat.Rel dir -> Phat.(Rel (concat dir (Item link)))

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
      match Phat.name n with
      | Ok _ ->
        let msg = sprintf "String %s was accepted as a valid name" n in
        assert_failure msg
      | Error _ -> ()
    )

let sexp_serialization _ =
  let check path_of_sexp p =
    try
      let p' = path_of_sexp (Phat.sexp_of_t p) in
      assert_equal ~printer:Phat.to_string p p'
    with Failure msg ->
      let msg =
        sprintf
          "Deserialization failed\nPath:\n%s\nSexp:\n%s\nBacktrace:\n%s\nError:\n%s"
          (Phat.to_string p)
          (Sexp.to_string_hum (Phat.sexp_of_t p))
          (Printexc.get_backtrace ())
          msg
      in
      assert_failure msg
  in
  for _ = 1 to 1000 do
    check Phat.abs_dir_of_sexp (random_abs_dir_path ()) ;
    check Phat.abs_file_of_sexp (random_abs_file_path ())
  done

let normalization _ =
  let check p =
    let p_norm = Phat.normalize p in
    let msg =
      sprintf
        "Path %s was normalized into %s which is not normalized"
        (Phat.to_string p)
        (Phat.to_string p_norm)
    in
    assert_bool msg (Phat.is_normalized p_norm)
  in
  for _ = 1 to 1000 do
    match random_dir_path () with
    | Phat.Abs dir -> check dir
    | Phat.Rel dir -> check dir
  done

let normalization_is_idempotent _ =
  let check p =
    let p_norm = Phat.normalize p in
    let p_norm_norm = Phat.normalize p_norm in
    let msg =
      sprintf
        "Path %s was normalized into %s which was normalized into %s"
        (Phat.to_string p)
        (Phat.to_string p_norm)
        (Phat.to_string p_norm_norm)
    in
    assert_bool msg (p_norm = p_norm_norm)
  in
  for _ = 1 to 1000 do
    Phat.map_any_kind (random_dir_path ()) { Phat.map = check }
  done

let resolution_for_eventually_abs_paths _ =
  let failure p_ref p p_res =
    let msg =
      sprintf
        "Path %s was resolved into %s instead of %s"
        (Sexp.to_string_hum (Phat.sexp_of_t p))
        (Sexp.to_string_hum (Phat.sexp_of_t p_res))
        (Sexp.to_string_hum (Phat.sexp_of_t p_ref))
    in
    assert_failure msg
  in
  let check p_ref p p_res =
    if p_ref <> p_res then failure p_ref p p_res
  in
  for _ = 1 to 1000 do
    let p_ref = random_abs_dir_path ~no_link:true () in
    match random_path_resolving_to (Phat.Abs p_ref) () with
    | Phat.Abs p -> check p_ref p (Phat.resolve p)
    | Phat.Rel p ->
      match Phat.resolve_any_kind p with
      | Phat.Abs p_res -> check p_ref p p_res
      | Phat.Rel p_res -> failure p_ref p p_res
  done

let resolution_is_identity_for_paths_without_links _ =
  let failure dir dir' =
    let msg =
      sprintf
        "Path %s was resolved into %s"
        (Sexp.to_string_hum (Phat.sexp_of_t dir))
        (Sexp.to_string_hum (Phat.sexp_of_t dir'))
    in
    assert_failure msg
  in
  let check dir dir' =
    if dir <> dir' then failure dir dir'
  in
  for _ = 1 to 1000 do
    match random_dir_path ~no_link:true () with
    | Phat.Abs dir -> check dir (Phat.resolve dir)
    | Phat.Rel dir ->
      match Phat.resolve_any_kind dir with
      | Phat.Abs dir' -> failure dir dir'
      | Phat.Rel dir' -> check dir dir'
  done

let resolution_eliminates_links _ =
  let check_aux p p_res =
    let msg =
      sprintf
        "Path %s was resolved into %s"
        (Sexp.to_string_hum (Phat.sexp_of_t p))
        (Sexp.to_string_hum (Phat.sexp_of_t p_res))
    in
    assert_bool msg (not (Phat.has_link p_res))
  in
  let check p = Phat.map_any_kind (Phat.resolve_any_kind p) { Phat.map = fun p_res -> check_aux p p_res } in
  for _ = 1 to 1000 do
    Phat.map_any_kind (random_dir_path ()) { Phat.map = check }
  done

let create_test_directory path =
  let f x = Filename.concat path x in
  Unix.mkdir (f "foo") >>= fun () ->
  Unix.mkdir (f "foo/bar") >>= fun () ->
  Writer.save (f "foo/bar/baz") ~contents:"baz" >>= fun () ->
  Unix.symlink ~src:"foo/bar/baz" ~dst:(f "qux")

let filesys_exists ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  let check p =
    Phat.exists (Phat.concat tmpdir_path p) >>| function
    | `Yes -> ()
    | `Unknown
    | `No ->
       let msg =
         sprintf
           "Failed to detect path %s"
           (Sexp.to_string_hum (Phat.sexp_of_t p))
       in
       assert_failure msg
  in
  let foo = Phat.(Item (Dir (name_exn "foo"))) in
  let foo_bar = Phat.(concat foo (Item (Dir (name_exn "bar")))) in
  let foo_bar_baz = Phat.(concat foo_bar (Item (File (name_exn "baz")))) in
  let qux = Phat.(Item (Link (name_exn "qux", foo_bar_baz))) in
  create_test_directory tmpdir >>= fun () ->
  check foo >>= fun () ->
  check foo_bar >>= fun () ->
  check foo_bar_baz >>= fun () ->
  check qux


let filesys_mkdir ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  List.init 1000 ~f:(fun _ -> ()) |>
  Deferred.List.iter ~f:(fun () ->
    let p = Phat.concat tmpdir_path (random_rel_dir_path ~root:tmpdir_path ~level:0 ()) in
    (
      Phat.mkdir p >>= function
      | Ok () -> (
          Phat.exists p >>| function
          | `Yes -> ()
          | `No
          | `Unknown ->
            let msg =
              sprintf
                "mkdir failed to create path %s"
                (Sexp.to_string_hum (Phat.sexp_of_t p))
            in
            assert_failure msg
        )
      | Error e ->
        let msg =
          sprintf
            "mkdir failed to create path %s: %s"
            (Sexp.to_string_hum (Phat.sexp_of_t p))
            (Sexp.to_string_hum (Error.sexp_of_t e))
        in
        return (ignore (assert_failure msg))
    ) >>= fun () ->
    Sys.command_exn (sprintf "rm -rf %s ; mkdir -p %s" tmpdir tmpdir)
  )

let filesys_mkdir_cycles ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  let rec foo_bar = Phat.(Item (Link (name_exn "bar", baz_qux)))
  and baz_qux = Phat.(Item (Link (name_exn "qux", foo_bar)))
  in
  let p = Phat.concat tmpdir_path foo_bar in
  Phat.mkdir p >>= function
  | Ok () ->
    Phat.exists p >>| fun file_exists ->
    if file_exists <> `Yes then (
      assert_failure "mkdir failed to create the cyclic path correctly."
    )
  | Error e ->
    let msg =
      sprintf
        "mkdir failed to create cyclic path: %s"
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

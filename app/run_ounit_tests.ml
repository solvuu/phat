open Core.Std
open Async.Std
open OUnit2
module Phat = Phat_async_unix.Std

(** Like OUnit.(>::), but the test function returns a Deferred. *)
let (>::=) test_name (f : test_ctxt -> unit Deferred.t) : test =
  test_name >:: (
    fun test_ctxt ->
      try Thread_safe.block_on_async_exn (fun () -> f test_ctxt)
      with exn -> raise (Monitor.extract_exn exn)
  )

let deferred_repeat n ~f =
  List.init 1000 ~f:(fun _ -> ())
  |> Deferred.List.iteri ~how:`Sequential ~f:(fun i () -> f i)

let string_of_path x = Sexp.to_string (Phat.sexp_of_t x)
let string_hum_of_path x = Sexp.to_string_hum (Phat.sexp_of_t x)

let rec update_level
  : type k o. root:(Phat.abs, Phat.dir) Phat.t -> int -> (k, o) Phat.t -> int
  = fun ~root l -> function
    | Phat.Item i -> update_level_item ~root l i
    | Phat.Cons (h, t) ->
      let l' = update_level_item ~root l h in
      update_level ~root l' t

and update_level_item
  : type k o. root:(Phat.abs, Phat.dir) Phat.t -> int -> (k, o) Phat.item -> int
  = fun ~root i -> function
    | Phat.Root -> 0
    | Phat.Dir _ -> i + 1
    | Phat.File _ -> i + 1
    | Phat.Dot -> i
    | Phat.Dotdot -> i - 1
    | Phat.Link (_, p) -> (
        match Phat.kind_of p with
        | `Abs p ->
          update_level ~root i p - update_level ~root:Phat.root 0 root
        | `Rel p ->  update_level ~root i p
      )
    | Phat.Broken_link _ -> i + 1

let new_name =
  let k = ref (- 1) in
  fun () ->
    incr k ;
    "foo" ^ (string_of_int !k)
    |> Phat.name
    |> ok_exn

(* The level argument is used to make sure we don't add too many
   Dotdot in the path. This is useful if generated paths must lie in
   some directory The link_level argument is there to limit the number
   of link indirections. *)
let rec random_rel_dir_item ~link_level ~root ~level () =
  let bottom = level <= 0 in
  let no_link = link_level <= 0 in
  if Random.bool () then
    Phat.Dir (new_name ())
  else if Random.bool () && not no_link then
    Phat.map_any_kind
      (random_dir_path ~link_level:(link_level - 1) ~root ~level ())
      { Phat.map = fun p -> Phat.Link (new_name (), p) }
  else if Random.bool () && not bottom then
    Phat.Dotdot
  else
    Phat.Dot

and random_rel_file_item ~link_level ~root ~level () =
  let no_link = link_level <= 0 in
  if no_link || Random.float 1. > 0.1 then
    Phat.File (new_name ())
  else (
    Phat.map_any_kind
      (random_file_path ~link_level:(link_level - 1) ~root ~level ())
      { Phat.map = fun p -> Phat.Link (new_name (), p) }
  )

and random_dir_path ~link_level ~root ~level () =
  match Random.bool () with
  | true -> `Abs (random_abs_dir_path ~link_level ~root ())
  | false -> `Rel (random_rel_dir_path ~link_level ~root ~level ())

and random_file_path ~link_level ~root ~level () =
  match Random.bool () with
  | true -> `Abs (random_abs_file_path ~link_level ~root ())
  | false -> `Rel (random_rel_file_path ~link_level ~root ~level ())

and random_abs_dir_path ~link_level ?(root = Phat.root) () =
  Phat.concat root (random_rel_dir_path ~link_level ~root ~level:0 ())

and random_rel_dir_path ~link_level ~root ~level () =
  let d = random_rel_dir_item ~link_level ~root ~level () in
  if Random.float 1. < 0.6 then
    let level = update_level_item ~root level d in
    Phat.Cons (d, random_rel_dir_path ~link_level ~root ~level ())
  else
    Phat.Item d

and random_abs_file_path ~link_level ?(root = Phat.root) () =
  Phat.concat root (random_rel_file_path ~link_level ~root ~level:0 ())

and random_rel_file_path ~link_level ~root ~level () =
  Phat.concat
    (random_rel_dir_path ~link_level ~root ~level ())
    (Phat.Item (random_rel_file_item ~link_level ~root ~level ()))

(* [random_path_resolving_to p] generates a path that resolves to what
   [p] resolves to (the name is a tiny bit misleading) *)
and random_path_resolving_to ~link_level ~root ~level p () =
  let link = Phat.map_any_kind p { Phat.map = fun p ->
      Phat.Link (new_name (), p)
    } in
  if Random.float 1. < 0.1 then
    `Rel (Phat.Item link)
  else
    match random_dir_path ~link_level ~root ~level () with
    | `Abs dir -> `Abs (Phat.concat dir (Phat.Item link))
    | `Rel dir -> `Rel (Phat.concat dir (Phat.Item link))

and random_path_with_link ~link_level ~root ~level () =
  random_path_resolving_to (random_dir_path ~link_level ~root ~level ())

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
    check Phat.abs_dir_of_sexp (random_abs_dir_path ~link_level:4 ()) ;
    check Phat.abs_file_of_sexp (random_abs_file_path ~link_level:4 ())
  done

let normalization _ =
  let check p =
    let p_norm = Phat.normalize p in
    let msg =
      sprintf
        "Path %s was normalized into %s which is not normalized"
        (string_of_path p)
        (string_of_path p_norm)
    in
    assert_bool msg (Phat.is_normalized p_norm)
  in
  for _ = 1 to 1000 do
    match random_dir_path ~link_level:4 ~root:Phat.root ~level:0 () with
    | `Abs dir -> check dir
    | `Rel dir -> check dir
  done

let normalization_is_idempotent _ =
  let check p =
    let p_norm = Phat.normalize p in
    let p_norm_norm = Phat.normalize p_norm in
    let msg =
      sprintf
        "Path %s was normalized into %s which was normalized into %s"
        (string_of_path p)
        (string_of_path p_norm)
        (string_of_path p_norm_norm)
    in
    assert_bool msg (p_norm = p_norm_norm)
  in
  for _ = 1 to 1000 do
    Phat.map_any_kind (random_dir_path ~link_level:4 ~root:Phat.root ~level:0 ()) { Phat.map = check }
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
    let p_ref = random_abs_dir_path ~link_level:0 () in
    match random_path_resolving_to ~link_level:4 ~root:Phat.root ~level:0 (`Abs p_ref) () with
    | `Abs p -> check p_ref p (Phat.resolve p)
    | `Rel p ->
      match Phat.resolve_any_kind p with
      | `Abs p_res -> check p_ref p p_res
      | `Rel p_res -> failure p_ref p p_res
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
    match random_dir_path ~root:Phat.root ~level:0 ~link_level:0 () with
    | `Abs dir -> check dir (Phat.resolve dir)
    | `Rel dir ->
      match Phat.resolve_any_kind dir with
      | `Abs dir' -> failure dir dir'
      | `Rel dir' -> check dir dir'
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
    Phat.map_any_kind (random_dir_path ~link_level:4 ~root:Phat.root ~level:0 ()) { Phat.map = check }
  done

let create_test_directory path =
  let f x = Filename.concat path x in
  Unix.mkdir (f "foo") >>= fun () ->
  Unix.mkdir (f "foo/bar") >>= fun () ->
  Writer.save (f "foo/bar/baz") ~contents:"baz" >>= fun () ->
  Unix.symlink ~src:"foo/bar/baz" ~dst:(f "qux") >>= fun () ->
  Unix.symlink ~src:"foo/bar/booz" ~dst:(f "broken")

let filesys_exists ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  let check p =
    Phat.exists (Phat.concat tmpdir_path p) >>| function
    | `Yes -> ()
    | `Yes_modulo_links
    | `Yes_as_other_object
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
  let foo_bar = Phat.(cons foo (Dir (name_exn "bar"))) in
  let foo_bar_baz = Phat.(cons foo_bar (File (name_exn "baz"))) in
  let qux = Phat.(Item (Link (name_exn "qux", foo_bar_baz))) in
  let broken = Phat.(Item (Broken_link (name_exn "broken", ["foo" ; "bar" ; "booz" ]))) in
  create_test_directory tmpdir >>= fun () ->
  check foo >>= fun () ->
  check foo_bar >>= fun () ->
  check foo_bar_baz >>= fun () ->
  check qux >>= fun () ->
  check broken

let filesys_exists_modulo_links ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  deferred_repeat 1000 ~f:(fun _ ->
      let p = random_abs_dir_path ~link_level:4 ~root:tmpdir_path () in
      (
        Phat.mkdir p >>= function
        | Ok () -> (
            let q = Phat.abs_dir (Phat.to_string p) |> ok_exn in
            Phat.exists q >>| function
            | `Yes_modulo_links -> ()
            | `Yes ->
              if Phat.has_link p then
                let msg =
                  sprintf
                    "exists sees:\n\n%s\n\nas:\n\n%s\n"
                    (string_hum_of_path p)
                    (string_hum_of_path q)
                in
                assert_failure msg
              else ()
            | `Yes_as_other_object ->
              let msg =
                sprintf
                  "exists sees:\n\n%s\n\nas another object when given:\n\n%s\n"
                  (string_hum_of_path p)
                  (string_hum_of_path q)
              in
              assert_failure msg
            | `No
            | `Unknown ->
              let msg =
                sprintf
                  "exists does not see:\n\n%s"
                  (string_hum_of_path p)
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

let filesys_exists_as_other_object ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  List.init 1000 ~f:(fun _ -> ()) |>
  Deferred.List.iter ~f:(fun () ->
      let p = (* This is needed to be sure the string representation of the dir can be parsed as a file *)
        Phat.(
          cons
            (random_abs_dir_path ~link_level:4 ~root:tmpdir_path ())
            (Dir (name_exn "foo"))
        )
      in
      (
        Phat.mkdir p >>= function
        | Ok () -> (
            let q = Phat.abs_file (Phat.to_string p) |> ok_exn in
            Phat.exists q >>| function
            | `Yes_modulo_links
            | `Yes ->
              let msg =
                sprintf
                  "exists does not see that:\n\n%s\n\nand:\n\n%s\n\nhave different types\n"
                  (string_hum_of_path p)
                  (string_hum_of_path q)
              in
              assert_failure msg
            | `Yes_as_other_object -> ()
            | `No
            | `Unknown ->
              let msg =
                sprintf
                  "exists does not see:\n\n%s\n\nwhen given:\n\n%s\n"
                  (string_hum_of_path p)
                  (string_hum_of_path q)
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

let filesys_mkdir ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  List.init 1000 ~f:(fun _ -> ()) |>
  Deferred.List.iter ~f:(fun () ->
      Sys.command_exn (sprintf "rm -rf %s ; mkdir -p %s" tmpdir tmpdir) >>= fun () ->
      let p = random_abs_dir_path ~root:tmpdir_path ~link_level:4 () in
      Phat.mkdir p >>= function
      | Ok () -> (
          Phat.exists p >>| function
          | `Yes -> ()
          | `Yes_modulo_links
          | `Yes_as_other_object
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
            "mkdir failed to create path:\n\n%s\n\nError:\n\n%s\n\n%s\n"
            (Sexp.to_string_hum (Phat.sexp_of_t p))
            (Sexp.to_string_hum (Error.sexp_of_t e))
            (Sexp.to_string_hum (Phat.sexp_of_t (Phat.resolve p)))
        in
        return (ignore (assert_failure msg))
    )

let fold_works_on_test_directory ctx =
  let expected = List.sort ~cmp:compare [
    "(Cons Dot(Item Dot))" ;
    "(Cons Dot(Cons Dot(Item(Broken_link broken(foo bar booz)))))" ;
    "(Cons Dot(Cons Dot(Item(Dir foo))))" ;
    "(Cons Dot(Cons Dot(Cons(Dir foo)(Item(Dir bar)))))" ;
    "(Cons Dot(Cons Dot(Cons(Dir foo)(Cons(Dir bar)(Item(File baz))))))" ;
    "(Cons Dot(Cons Dot(Item(Link qux(Cons(Dir foo)(Cons(Dir bar)(Item(File baz))))))))" ;
  ]
  in
  let tmpdir_as_str = OUnit2.bracket_tmpdir ctx in
  let tmpdir = ok_exn (Phat.abs_dir tmpdir_as_str) in
  let str_of_elt =
    let f x = Sexp.to_string (Phat.sexp_of_t x) in
    function
    | `File file -> f file
    | `Dir d -> f d
    | `Broken_link bl -> f bl
  in
  create_test_directory tmpdir_as_str >>= fun () ->
  Phat.fold tmpdir ~init:[] ~f:(fun accu elt ->
      return ((str_of_elt elt) :: accu)
    )
  >>| function
  | Ok l -> assert_equal ~printer:(List.to_string ~f:ident) expected (List.sort ~cmp:compare l)
  | Error _ -> assert_failure "Fold failed on test directory"


let reify_directory ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  deferred_repeat 1000 ~f:(fun i ->
      Process.run ~prog:"rm" ~args:[ "-rf" ; tmpdir ] () >>| ok_exn >>= fun _ ->
      let p =
        random_rel_dir_path ~link_level:4 ~root:tmpdir_path ~level:0 ()
        |> Phat.concat tmpdir_path
      in
      let p_str = Phat.to_string p in
      Phat.mkdir p >>= function
      | Ok () -> (
          Phat.abs_dir p_str |> ok_exn |> Phat.reify >>= function
          | Ok q -> (
              Phat.exists q >>= function
              | `Yes -> return ()
              | `Yes_modulo_links | `Yes_as_other_object | `No | `Unknown ->
                Process.run ~prog:"tree" ~args:[ tmpdir ] () >>| ok_exn >>| fun stdout ->
                let msg = sprintf "Tree:\n%s\n\nOriginal:\n%s\n\nReified:\n\n%s\n" stdout (string_hum_of_path p) (string_hum_of_path q) in
                assert_failure msg
            )
          | Error e ->
            Process.run ~prog:"tree" ~args:[ tmpdir ] () >>| ok_exn >>= fun stdout ->
            let msg = sprintf "Tree:\n%s\n\nOriginal:\n%s\n\nError:\n%s\n" stdout (string_hum_of_path p) (Error.to_string_hum e) in
            assert_failure msg
        )
      | Error e ->
        let msg = sprintf "mkdir failure:\n\n%s\n" (Error.to_string_hum e) in
        assert_failure msg
    )

let suite = "Phat test suite" >::: [
    "Name constructor" >:: name_constructor ;
    "Sexp serialization" >:: sexp_serialization ;
    "Normalization" >:: normalization ;
    "Normalization is idempotent" >:: normalization_is_idempotent ;
    "Resolution eliminates links" >:: resolution_eliminates_links ;
    "Resolution is the identity for paths without links" >:: resolution_is_identity_for_paths_without_links ;
    "Resolution for eventually abs paths" >:: resolution_for_eventually_abs_paths ;
    "Exists test" >::= filesys_exists ;
    "Exists modulo link" >::= filesys_exists_modulo_links ;
    "Exists as other object" >::= filesys_exists_as_other_object ;
    "Create dir paths" >::= filesys_mkdir ;
    "Fold works on test dir" >::= fold_works_on_test_directory ;
    "Reify directory" >::= reify_directory ;
  ]

let () = Random.init 420
let () = run_test_tt_main suite

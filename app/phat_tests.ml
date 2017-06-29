open Core
open Async
open OUnit2
module Phat = Phat_async

(** Like OUnit.(>::), but the test function returns a Deferred. *)
let (>::=) test_name (f : test_ctxt -> unit Deferred.t) : test =
  test_name >:: (
    fun test_ctxt ->
      try Thread_safe.block_on_async_exn (fun () -> f test_ctxt)
      with exn -> raise (Monitor.extract_exn exn)
  )

let deferred_repeat n ~f =
  List.init n ~f:(fun _ -> ())
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


(* A module to generate normalized random paths *)
module NR = struct
  let rec rel_dir_item ?(no_dot = false) ~link_level ~root ~level () =
    let bottom = level <= 0 in
    let no_link = link_level <= 0 in
    if Random.bool () then
      Phat.Dir (new_name ())
    else if Random.bool () && not no_link then
      Phat.map_any_kind
        (dir_path ~link_level:(link_level - 1) ~root ~level ())
        { Phat.map = fun p -> Phat.Link (new_name (), p) }
    else if not bottom && not no_dot && Random.bool () then
      Phat.Dotdot
    else
      rel_dir_item ~no_dot ~link_level ~root ~level ()

  and rel_file_item ~link_level ~root ~level () =
    let no_link = link_level <= 0 in
    if no_link || Random.float 1. > 0.1 then
      Phat.File (new_name ())
    else (
      Phat.map_any_kind
        (file_path ~link_level:(link_level - 1) ~root ~level ())
        { Phat.map = fun p -> Phat.Link (new_name (), p) }
    )

  and dir_path ~link_level ~root ~level () =
    match Random.bool () with
    | true -> `Abs (abs_dir_path ~link_level ~root ())
    | false -> `Rel (rel_dir_path ~link_level ~root ~level ())

  and file_path ~link_level ~root ~level () =
    match Random.bool () with
    | true -> `Abs (abs_file_path ~link_level ~root ())
    | false -> `Rel (rel_file_path ~link_level ~root ~level ())

  and abs_dir_path ~link_level ?(root = Phat.root) () =
    Phat.concat root (rel_dir_path ~link_level ~root ~level:0 ())

  and rel_dir_path ?(no_dot = false) ~link_level ~root ~level () =
    let d = rel_dir_item ~no_dot ~link_level ~root ~level () in
    if Random.float 1. < 0.6 then
      let no_dot = no_dot || d <> Phat.Dotdot in
      let level = update_level_item ~root level d in
      Phat.Cons (d, rel_dir_path ~no_dot ~link_level ~root ~level ())
    else
      Phat.Item d

  and abs_file_path ~link_level ?(root = Phat.root) () =
    Phat.concat root (rel_file_path ~link_level ~root ~level:0 ())

  and rel_file_path ~link_level ~root ~level () =
    Phat.concat
      (rel_dir_path ~link_level ~root ~level ())
      (Phat.Item (rel_file_item ~link_level ~root ~level ()))
end


module R = struct
  let rec rel_dir_item ~link_level ~root ~level () =
    let bottom = level <= 0 in
    let no_link = link_level <= 0 in
    if Random.bool () then
      Phat.Dir (new_name ())
    else if Random.bool () && not no_link then
      Phat.map_any_kind
        (dir_path ~link_level:(link_level - 1) ~root ~level ())
        { Phat.map = fun p -> Phat.Link (new_name (), p) }
    else if Random.bool () && not bottom then
      Phat.Dotdot
    else
      Phat.Dot

  and rel_file_item ~link_level ~root ~level () =
    let no_link = link_level <= 0 in
    if no_link || Random.float 1. > 0.1 then
      Phat.File (new_name ())
    else (
      Phat.map_any_kind
        (file_path ~link_level:(link_level - 1) ~root ~level ())
        { Phat.map = fun p -> Phat.Link (new_name (), p) }
    )

  and dir_path ~link_level ~root ~level () =
    match Random.bool () with
    | true -> `Abs (abs_dir_path ~link_level ~root ())
    | false -> `Rel (rel_dir_path ~link_level ~root ~level ())

  and file_path ~link_level ~root ~level () =
    match Random.bool () with
    | true -> `Abs (abs_file_path ~link_level ~root ())
    | false -> `Rel (rel_file_path ~link_level ~root ~level ())

  and abs_dir_path ~link_level ?(root = Phat.root) () =
    Phat.concat root (rel_dir_path ~link_level ~root ~level:0 ())

  and rel_dir_path ~link_level ~root ~level () =
    let d = rel_dir_item ~link_level ~root ~level () in
    if Random.float 1. < 0.6 then
      let level = update_level_item ~root level d in
      Phat.Cons (d, rel_dir_path ~link_level ~root ~level ())
    else
      Phat.Item d

  and abs_file_path ~link_level ?(root = Phat.root) () =
    Phat.concat root (rel_file_path ~link_level ~root ~level:0 ())

  and rel_file_path ~link_level ~root ~level () =
    Phat.concat
      (rel_dir_path ~link_level ~root ~level ())
      (Phat.Item (rel_file_item ~link_level ~root ~level ()))

  (* [path_resolving_to p] generates a path that resolves to what
     [p] resolves to (the name is a tiny bit misleading) *)
  and path_resolving_to ~link_level ~root ~level p () =
    let link = Phat.map_any_kind p { Phat.map = fun p ->
        Phat.Link (new_name (), p)
      } in
    if Random.float 1. < 0.1 then
      `Rel (Phat.Item link)
    else
      match dir_path ~link_level ~root ~level () with
      | `Abs dir -> `Abs (Phat.concat dir (Phat.Item link))
      | `Rel dir -> `Rel (Phat.concat dir (Phat.Item link))

  and path_with_link ~link_level ~root ~level () =
    path_resolving_to (dir_path ~link_level ~root ~level ())
end


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
    check Phat.abs_dir_of_sexp (R.abs_dir_path ~link_level:4 ()) ;
    check Phat.abs_file_of_sexp (R.abs_file_path ~link_level:4 ())
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
    match R.dir_path ~link_level:4 ~root:Phat.root ~level:0 () with
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
    Phat.map_any_kind (R.dir_path ~link_level:4 ~root:Phat.root ~level:0 ()) { Phat.map = check }
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
    let p_ref = R.abs_dir_path ~link_level:0 () in
    match R.path_resolving_to ~link_level:4 ~root:Phat.root ~level:0 (`Abs p_ref) () with
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
    match R.dir_path ~root:Phat.root ~level:0 ~link_level:0 () with
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
    Phat.map_any_kind (R.dir_path ~link_level:4 ~root:Phat.root ~level:0 ()) { Phat.map = check }
  done

let create_test_directory path =
  let f x = Filename.concat path x in
  Unix.mkdir (f "foo") >>= fun () ->
  Unix.mkdir (f "foo/bar") >>= fun () ->
  Writer.save (f "foo/bar/baz") ~contents:"baz" >>= fun () ->
  Unix.symlink ~src:"foo/bar/baz" ~dst:(f "qux") >>= fun () ->
  Unix.symlink ~src:".." ~dst:(f "foo/bar/norf") >>= fun () ->
  Unix.symlink ~src:"foo/bar/booz" ~dst:(f "broken")

type path_wrapper = PW : (Phat.rel,_) Phat.t -> path_wrapper

let test_directory_description =
  let open Phat in
  let open Phat.Infix in
  let foo = dir_exn "foo" in
  let foo_bar = foo / dir_exn "bar" in
  let foo_bar_baz = foo_bar / file_exn "baz" in
  let qux = link_exn "qux" foo_bar_baz in
  let norf = foo_bar / link_exn "norf" dotdot in
  let broken = broken_link_exn "broken" ["foo" ; "bar" ; "booz" ]
  in
  [ PW foo ; PW foo_bar ; PW foo_bar_baz ; PW qux ; PW broken ; PW norf ]


let filesys_exists ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  let check
    : type o. (Phat.rel, o) Phat.t -> unit Deferred.t
    = fun p ->
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
  create_test_directory tmpdir >>= fun () ->
  Deferred.List.iter test_directory_description ~f:(function PW p -> check p)

let filesys_exists_modulo_links ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  deferred_repeat 100 ~f:(fun _ ->
      Sys.command_exn (sprintf "rm -rf %s ; mkdir -p %s" tmpdir tmpdir) >>= fun () ->
      let p = R.abs_dir_path ~link_level:4 ~root:tmpdir_path () in
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
  deferred_repeat 100 ~f:(fun _ ->
      Sys.command_exn (sprintf "rm -rf %s ; mkdir -p %s" tmpdir tmpdir) >>= fun () ->
      let p = (* This is needed to be sure the string representation of the dir can be parsed as a file *)
        Phat.(
          cons
            (R.abs_dir_path ~link_level:4 ~root:tmpdir_path ())
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
  deferred_repeat 100 ~f:(fun _ ->
      Sys.command_exn (sprintf "rm -rf %s ; mkdir -p %s" tmpdir tmpdir) >>= fun () ->
      let p = R.abs_dir_path ~root:tmpdir_path ~link_level:4 () in
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
  let expected = List.sort ~cmp:compare (PW Phat.dot :: test_directory_description) in
  let tmpdir_as_str = OUnit2.bracket_tmpdir ctx in
  let tmpdir = ok_exn (Phat.abs_dir tmpdir_as_str) in
  let pw_of_elt =
    function
    | `File file -> PW file
    | `Dir d -> PW d
    | `Broken_link bl -> PW bl
  in
  let to_string (PW p) = Phat.to_string p in
  create_test_directory tmpdir_as_str >>= fun () ->
  Phat.fold tmpdir ~init:[] ~f:(fun accu elt ->
      return ((pw_of_elt elt) :: accu)
    )
  >>| function
  | Ok l -> assert_equal ~printer:(List.to_string ~f:to_string) expected (List.sort ~cmp:compare l)
  | Error _ -> assert_failure "Fold failed on test directory"


let reify_directory ctx =
  let tmpdir = OUnit2.bracket_tmpdir ctx in
  let tmpdir_path = ok_exn (Phat.abs_dir tmpdir) in
  deferred_repeat 100 ~f:(fun i ->
      Process.run ~prog:"rm" ~args:[ "-rf" ; tmpdir ] () >>| ok_exn >>= fun _ ->
      let p =
        NR.abs_dir_path ~link_level:4 ~root:tmpdir_path ()
      in
      let p_str = Phat.to_string p in
      Phat.mkdir p >>= function
      | Ok () -> (
          Phat.abs_dir p_str |> ok_exn |> Phat.reify >>= function
          | Ok q -> (
              if p <> q then (
                Process.run ~prog:"tree" ~args:[ tmpdir ] () >>| ok_exn >>| fun stdout ->
                let msg = sprintf "Tree:\n%s\n\nOriginal:\n%s\n\nReified:\n\n%s\n" stdout (string_hum_of_path p) (string_hum_of_path q) in
                assert_failure msg
              )
              else return ()
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


let fold_follows_links_works_on_test_directory ctx =
  let tmpdir_as_str = OUnit2.bracket_tmpdir ctx in
  let tmpdir = ok_exn (Phat.abs_dir tmpdir_as_str) in
  let description =
    let h x = Phat.concat tmpdir x in
    let f = function PW p ->
    match Phat.typ_of p with
    | `File file -> `File (h file)
    | `Dir dir -> `Dir (h dir)
    | `Link bl -> `Broken_link (h bl)
    in
    (`Dir tmpdir) :: List.map test_directory_description ~f
  in
  let expected = List.sort ~cmp:compare description in
  let map_elts = function
    | `File (_,f) -> `File f
    | `Dir (_, d) -> `Dir d
    | `Broken_link (_, bl) -> `Broken_link bl
  in
  let to_string = function
    | `File x -> sprintf "(File %s)" (Phat.to_string x)
    | `Dir x -> sprintf "(Dir  %s)" (Phat.to_string x)
    | `Broken_link x -> sprintf "(Broken_link %s)" (Phat.to_string x)
  in
  create_test_directory tmpdir_as_str >>= fun () ->
  Phat.fold_follows_links tmpdir ~init:[] ~f:(fun accu elt ->
      return (map_elts elt :: accu)
    )
  >>| function
  | Ok l -> assert_equal ~printer:(fun xs -> String.concat ~sep:"\n" (List.map xs ~f:to_string)) expected (List.sort ~cmp:compare l)
  | Error _ -> assert_failure "Fold failed on test directory"

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
    "Fold (when following link) works on test dir" >::= fold_follows_links_works_on_test_directory ;
  ]

let () = Random.init 42
let () = run_test_tt_main suite

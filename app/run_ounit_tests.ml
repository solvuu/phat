open Core.Std
open OUnit2
open Phat_unix.Std
open Path

let random_char () =
  let a = Char.to_int 'a' in
  let z = Char.to_int 'z' in
  Char.of_int_exn (Random.int (z - a) + a)

let random_name () =
  String.init (Random.int 4 + 1) ~f:(fun _ -> random_char ())
  |> name
  |> ok_exn

let rec random_rel_dir_item ?(no_link = false) () =
  match Random.bool (), Random.bool () with
  | true, b ->
    if no_link || b then
      Dir (random_name ())
    else (
      let f p = Link (random_name (), p) in
      match random_dir_path () with
      | Abs_path p -> f p
      | Rel_path p -> f p
    )
  | false, true -> Dot
  | false, false -> Dotdot

and random_dir_path ?no_link () =
  match Random.bool () with
  | true -> Abs_path (random_abs_dir_path ?no_link ())
  | false -> Rel_path (random_rel_dir_path ?no_link ())

and random_abs_dir_path ?no_link () =
  concat root (random_rel_dir_path ?no_link ())

and random_rel_dir_path ?no_link () =
  let d = random_rel_dir_item ?no_link () in
  if Random.bool () then
    Cons (d, random_rel_dir_path ?no_link ())
  else
    Item d

(* [random_path_resolving_to p] generates a path that resolves to what
   [p] resolves (the name is a tiny bit misleading) *)
and random_path_resolving_to p () =
  let link = match p with
    | Abs_path p -> Link (random_name (), p)
    | Rel_path p -> Link (random_name (), p)
  in
  if Random.float 1. < 0.1 then
    Rel_path (Item link)
  else
    match random_dir_path () with
    | Abs_path dir -> Abs_path (concat dir (Item link))
    | Rel_path dir -> Rel_path (concat dir (Item link))

and random_path_with_link () =
  random_path_resolving_to (random_dir_path ())

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
    match random_dir_path () with
    | Abs_path dir -> check dir
    | Rel_path dir -> check dir
  done

let resolution_for_eventually_abs_paths _ =
  let failure p_ref p p_res =
    let msg =
      sprintf
        "Path %s was resolved into %s instead of %s"
        (Sexp.to_string (Path.sexp_of_t p))
        (Sexp.to_string (Path.sexp_of_t p_res))
        (Sexp.to_string (Path.sexp_of_t p_ref))
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
        (Sexp.to_string (Path.sexp_of_t dir))
        (Sexp.to_string (Path.sexp_of_t dir'))
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
        (Sexp.to_string (Path.sexp_of_t p))
        (Sexp.to_string (Path.sexp_of_t p_res))
    in
    assert_bool msg (not (has_link p_res))
  in
  let check p =
    match resolve_any p with
    | Abs_path p_res -> check_aux p p_res
    | Rel_path p_res -> check_aux p p_res
  in
  for _ = 1 to 1000 do
    match random_dir_path () with
    | Abs_path dir -> check dir
    | Rel_path dir -> check dir
  done


let suite = "Phat test suite" >::: [
    "Name constructor" >:: name_constructor ;
    "Normalization" >:: normalization ;
    "Normalization is idempotent" >:: normalization_is_idempotent ;
    "Resolution eliminates links" >:: resolution_eliminates_links ;
    "Resolution is the identity for paths without links" >:: resolution_is_identity_for_paths_without_links ;
    "Resolution for eventually abs paths" >:: resolution_for_eventually_abs_paths ;
  ]


let () = run_test_tt_main suite

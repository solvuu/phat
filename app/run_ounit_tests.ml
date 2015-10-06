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

let rec random_rel_dir_item () =
  match Random.bool (), Random.bool () with
  | true, true -> Dir (random_name ())
  | true, false -> (
      let f p = Link (random_name (), p) in
      match random_dir_path () with
      | Abs_path p -> f p
      | Rel_path p -> f p
    )
  | false, true -> Dot
  | false, false -> Dotdot

and random_dir_path () =
  match Random.bool () with
  | true -> Abs_path (random_abs_dir_path ())
  | false -> Rel_path (random_rel_dir_path ())

and random_abs_dir_path () =
  concat root (random_rel_dir_path ())

and random_rel_dir_path () =
  let d = random_rel_dir_item () in
  if Random.bool () then
    Cons (d, random_rel_dir_path ())
  else
    Item d

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

let suite = "Phat test suite" >::: [
    "Name constructor" >:: name_constructor ;
    "Normalization" >:: normalization ;
    "Normalization is idempotent" >:: normalization_is_idempotent ;
  ]


let () = run_test_tt_main suite

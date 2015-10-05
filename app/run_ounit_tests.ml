open Core.Std
open OUnit2
open Phat

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

let suite = "Phat test suite" >::: [
    "Name constructor" >:: name_constructor
  ]


let () = run_test_tt_main suite

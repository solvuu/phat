open Printf
open Solvuu_build.Std

let project_name = "phat"
let version = "dev"

let make_lib ?findlib_deps ?internal_deps ?build_if short_name : Item.t =
  Item.lib (sprintf "%s_%s" project_name short_name)
    ~pkg:(sprintf "%s.%s" project_name short_name)
    ~dir:(sprintf "lib/%s" short_name)
    ~pack_name:(sprintf "%s_%s" project_name short_name)
    ?findlib_deps
    ?internal_deps
    ?build_if

let pure = make_lib "pure"
    ~findlib_deps:["core_kernel"; "ppx_sexp_conv"; "ppx_here"]

let async = make_lib "async"
    ~internal_deps:[pure]
    ~findlib_deps:["async"]
    ~build_if:[`Pkgs_installed]

let phat_tests = Item.app "phat_tests"
    ~file:"app/phat_tests.ml"
    ~internal_deps:[async]
    ~findlib_deps:["oUnit"]
    ~build_if:[`Pkgs_installed]

let phat = Item.app "phat"
    ~file:"app/phat.ml"
    ~internal_deps:[async]
    ~build_if:[`Pkgs_installed]

let ocamlinit_postfix = [
  "open Core.Std";
  "open Async.Std";
  "open Phat_async.Std";
]

let project = Project.make ~name:project_name ~version ~ocamlinit_postfix
    [pure;async;phat_tests;phat]
;;

let () = Project.dispatch project

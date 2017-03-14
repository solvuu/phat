open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "phat"
let version = "dev"

let make_lib ?findlib_deps ?internal_deps ?ml_files short_name : Project.item =
  Project.lib (sprintf "%s_%s" project_name short_name)
    ~install:(`Findlib (sprintf "%s.%s" project_name short_name))
    ~dir:(sprintf "lib/%s" short_name)
    ~style:(`Pack (sprintf "%s_%s" project_name short_name))
    ?findlib_deps
    ?internal_deps
    ?ml_files

let make_app ?internal_deps ?findlib_deps short_name : Project.item =
  Project.app short_name
    ~file:(sprintf "app/%s.ml" short_name)
    ?internal_deps
    ?findlib_deps

let pure = make_lib "pure"
    ~findlib_deps:["core_kernel"; "ppx_jane"]
    ~ml_files:(`Add ["about.ml"])

let async = make_lib "async"
    ~internal_deps:[pure]
    ~findlib_deps:["async"]

let phat_tests = make_app "phat_tests"
    ~internal_deps:[async]
    ~findlib_deps:["oUnit"]

let phat = make_app "phat"
    ~internal_deps:[async]

let ocamlinit_postfix = [
  "open Core.Std";
  "open Async.Std";
  "open Phat_async.Std";
]

let optional_pkgs = ["async"; "lwt"; "oUnit"]

let items =
  [pure;async;phat_tests;phat] |>
  List.filter ~f:(fun x -> Project.dep_opts_sat x optional_pkgs)

;;
let () = Project.solvuu1 ~project_name ~version ~ocamlinit_postfix items

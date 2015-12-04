let version = "dev"

open Printf
open Ocamlbuild_plugin

let () =
  Options.use_ocamlfind := true


let git_commit =
  if Sys.file_exists ".git" then
    let git_output = Ocamlbuild_pack.My_unix.run_and_read "git rev-parse HEAD" in
    let commit = String.sub git_output 0 (String.length git_output - 1) in
    sprintf "Some \"%s\"" commit
  else "None"

let () =
  dispatch (function
      | After_rules ->
        rule "m4 generation"
          ~prod:"%.ml"
          ~dep:"%.ml.m4"
          (fun env build ->
             let ml_m4 = env "%.ml.m4" in
             let ml = env "%.ml" in
             let vars =
               S [ A"-D" ; A ("VERSION=" ^ version) ;
                   A"-D" ; A ("GIT_COMMIT=" ^ git_commit) ] in
             let cmd = Cmd (S [ A"m4" ; vars ; P ml_m4 ; Sh">" ; P ml ]) in
             Seq [ cmd ]
          )
      | _ -> ()
    )


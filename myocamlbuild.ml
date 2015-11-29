let version = "dev"

let packages = [
  "pure", [ "core_kernel" ] ;
]


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

let rec list_find_opt f = function
  | [] -> None
  | h :: t ->
    match f h with
    | Some _ as r -> r
    | None -> list_find_opt f t

let dir_exists p =
  Sys.file_exists p && Sys.is_directory p

let readdir p = Array.to_list (Sys.readdir p)

let libdir = "lib"

let libs = readdir libdir

let modules =
  let f lib =
    readdir (Filename.concat libdir lib)
    |> List.filter_opt (fun x ->
        list_find_opt
          (fun ext ->
            if Filename.check_suffix x ext
            then Some (Filename.chop_suffix x ext)
            else None)
          [ ".ml" ; ".ml.m4" ]
      )
    |> List.map Filename.basename
  in
  List.map (fun x -> x, f x) libs

let rule_for_lib_mlpack lib =
  let prod = "lib/" ^ lib ^ ".mlpack" in
  rule ("mlpack generation for library " ^ lib)
    ~prod
    (fun env build ->
       let mlpack = env prod in
       let lines =
         List.map
           (fun m -> libdir ^ "/" ^ lib ^ "/" ^ (String.capitalize m) ^ "\n")
           (List.assoc lib modules)
       in
       Echo (lines, mlpack))

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
          ) ;

        List.iter rule_for_lib_mlpack libs

      | _ -> ()
    )

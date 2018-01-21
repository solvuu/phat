open Core
open Async

(******************************************************************************)
(* Argument Types                                                             *)
(******************************************************************************)
let abs_file = Command.Arg_type.create (fun x -> Path.abs_file x |> ok_exn)
let rel_file = Command.Arg_type.create (fun x -> Path.rel_file x |> ok_exn)
let abs_dir = Command.Arg_type.create (fun x -> Path.abs_dir x |> ok_exn)
let rel_dir = Command.Arg_type.create (fun x -> Path.rel_dir x |> ok_exn)
let file_of_any_kind = Command.Arg_type.create (fun x ->
  Path.file_of_any_kind x |> ok_exn)
let dir_of_any_kind = Command.Arg_type.create (fun x ->
  Path.dir_of_any_kind x |> ok_exn)

(******************************************************************************)
(* Common app parameters                                                      *)
(******************************************************************************)
module Param = struct
  include Command.Param

  let log_level =
    flag "-log" (optional_with_default `Info Log.Level.arg)
      ~doc:(sprintf "level Log level can be debug, info, or error. \
                     Default is info.")

end

(******************************************************************************)
(* tree                                                                       *)
(******************************************************************************)
let tree : Command.t =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"print a directory tree"
    [%map_open
      let log_level = Param.log_level
      and dir = Param.(anon ("DIR" %: string)) in
      fun () ->
        Log.Global.set_level log_level;
        (
          Deferred.return (Path.dir_of_any_kind dir) >>=? function
          | `Abs abs_dir -> Deferred.return (Ok abs_dir)
          | `Rel rel_dir -> (
              (Unix.getcwd() >>| Path.abs_dir) >>|? fun cwd ->
              Path.concat cwd rel_dir
            )
        ) >>=?
        Filesys.fold ~init:[] ~f:(fun accum x ->
          let x = match x with
            | `File x -> Path.to_string x
            | `Dir x -> Path.to_string x
            | `Broken_link x -> Path.to_string x
          in
          Deferred.return (x::accum)
        ) >>|?
        List.rev >>|? fun l ->
        List.iter l ~f:(fun x -> Writer.write_line (force Writer.stdout) x);
    ]

let main = Command.group ~summary:"file system operations"
  [
    "tree", tree;
  ]

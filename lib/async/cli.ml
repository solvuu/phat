open Core.Std
open Async.Std
module Phat = struct
  include Phat_path
  include Filesys
end

(** Async_extra.Command is missing this function. See pull request:
    - https://github.com/janestreet/async_extra/pull/6
*)
let async_or_error' ~summary ?readme params main =
  Command.async_or_error ~summary ?readme (Command.Spec.of_params params) main

(******************************************************************************)
(* Argument Types                                                             *)
(******************************************************************************)
let abs_file = Command.Arg_type.create (fun x -> Phat.abs_file x |> ok_exn)
let rel_file = Command.Arg_type.create (fun x -> Phat.rel_file x |> ok_exn)
let abs_dir = Command.Arg_type.create (fun x -> Phat.abs_dir x |> ok_exn)
let rel_dir = Command.Arg_type.create (fun x -> Phat.rel_dir x |> ok_exn)
let file_of_any_kind = Command.Arg_type.create (fun x ->
  Phat.file_of_any_kind x |> ok_exn)
let dir_of_any_kind = Command.Arg_type.create (fun x ->
  Phat.dir_of_any_kind x |> ok_exn)

(******************************************************************************)
(* Common app parameters                                                      *)
(******************************************************************************)
module Param = struct
  include Command.Param

  let log_level =
    flag "-log" (optional_with_default `Info Log.Level.arg)
      ~doc:(sprintf "level Log level can be debug, info, or error. \
                     Default is info.")

  let common_args remaining_args =
    log_level @> remaining_args

end

(** Handle [common_args] in a common way. Returns path to config
    file. *)
let common_handler log_level : unit Or_error.t Deferred.t =
  Log.Global.set_level log_level;
  return (Ok ())


(******************************************************************************)
(* tree                                                                       *)
(******************************************************************************)
let tree : Command.t = async_or_error'
  ~summary:"print a directory tree"
  Param.(common_args @@ anon ("DIR" %: string) @> nil)
  (fun a dir () ->
    common_handler a >>=? fun () ->
    (
      return (Phat.dir_of_any_kind dir) >>=? function
      | `Abs abs_dir -> return (Ok abs_dir)
      | `Rel rel_dir -> (
	(Unix.getcwd() >>| Phat.abs_dir) >>|? fun cwd ->
	Phat.concat cwd rel_dir
      )
    ) >>=?
    Phat.fold ~init:[] ~f:(fun accum x ->
      let x = match x with
	| `File x -> Phat.to_string x
	| `Dir x -> Phat.to_string x
	| `Broken_link x -> Phat.to_string x
      in
      return (x::accum)
    ) >>|?
    List.rev >>|? fun l ->
    List.iter l ~f:(fun x -> Writer.write_line (force Writer.stdout) x);
  )


let main = Command.group ~summary:"file system operations"
  [
    "tree", tree;
  ]

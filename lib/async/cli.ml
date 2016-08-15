open Core.Std
open Async.Std
module Phat = struct
  include Phat_path
  include Filesys
end

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

end

(******************************************************************************)
(* tree                                                                       *)
(******************************************************************************)
let tree : Command.t = Command.async_or_error'
  ~summary:"print a directory tree"
  (
  let open Command.Let_syntax in
  [%map_open
  let log_level = Param.log_level
  and dir = Param.(anon ("DIR" %: string)) in
  fun () ->
    Log.Global.set_level log_level;
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
  ]
  )


let main = Command.group ~summary:"file system operations"
  [
    "tree", tree;
  ]

open Core.Std
open Async.Std

include Phat_pure.Std.Path

let abs_file_of_any ?base_dir x =
  match file_path_of_some_kind x with
  | Error _ as e -> return e
  | Ok (Abs_path x) -> return (Ok x)
  | Ok (Rel_path x) ->
    (match base_dir with
     | Some x -> return (Ok x)
     | None -> Unix.getcwd() >>| dir_path
    ) >>|? fun base_dir ->
    concat base_dir x

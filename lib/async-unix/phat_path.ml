open Core.Std
open Async.Std

include Phat_pure.Phat_path

let abs_file_of_any ?base_dir x =
  match file_of_any_kind x with
  | Error _ as e -> return e
  | Ok (Abs x) -> return (Ok x)
  | Ok (Rel x) ->
    (match base_dir with
     | Some x -> return (Ok x)
     | None -> Unix.getcwd() >>| abs_dir
    ) >>|? fun base_dir ->
    concat base_dir x

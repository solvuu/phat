(* We don't provide a corresponding mli because of
   {{:https://sympa.inria.fr/sympa/arc/caml-list/2015-11/msg00002.html}this}.
   The workaround would require manually copying too much code. *)
open Core
open Async

include Phat_pure.Phat_path

let abs_file_of_any ?(base_dir:abs_dir option) (x:string)
  : abs_file Or_error.t Deferred.t
  =
  match file_of_any_kind x with
  | Error _ as e -> return e
  | Ok (`Abs x) -> return (Ok x)
  | Ok (`Rel x) ->
    (match base_dir with
     | Some x -> return (Ok x)
     | None -> Unix.getcwd() >>| abs_dir
    ) >>|? fun base_dir ->
    concat base_dir x

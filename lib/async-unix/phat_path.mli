open Core.Std
open Async.Std

include module type of Phat_pure.Phat_path

val abs_file_of_any
  :  ?base_dir:abs_dir
  -> string
  -> abs_file Or_error.t Deferred.t

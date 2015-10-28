open Core.Std
open Async.Std

include module type of Phat_pure.Std.Path

val abs_file_of_any
  :  ?base_dir:dir_path
  -> string
  -> file_path Or_error.t Deferred.t

open Core.Std
open Async.Std
module Phat = Phat_async_unix.Std

let () =
  try Command.run ~version:Phat.version Phat.Cli.main
  with e -> eprintf "%s\n" (Exn.to_string e)

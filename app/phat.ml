open Core
open Async
module Phat = Phat_async.Std

let () =
  try Command.run ~version:Phat.version Phat.Cli.main
  with e -> eprintf "%s\n" (Exn.to_string e)

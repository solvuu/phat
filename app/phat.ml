open Core
open Async
module Phat = Phat_async

let () =
  try Command_unix.run ~version:Phat.About.version Phat.Cli.main with
  | e -> eprintf "%s\n" (Exn.to_string e)
;;

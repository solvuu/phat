(** General information about this code. *)

(** Version: [VERSION] *)
let version = String.trim "include(../VERSION)"

(** Git commit if known: [GIT_COMMIT] *)
let git_commit = include(GIT_COMMIT)dnl

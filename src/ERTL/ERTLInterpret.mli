
(** This module provides an interpreter for the [ERTL] language. *)

val interpret : bool -> ERTL.program -> AST.trace

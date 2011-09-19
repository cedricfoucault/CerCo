
(** This module provides a function to interpret a [RTL] program
    and return the trace of cost labels encountered. *)

val interpret : bool -> RTL.program -> AST.trace

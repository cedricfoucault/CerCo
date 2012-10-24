
(** This module provides a function to interpret a [RTLabs] program
    and return the trace of cost labels encountered. *)

val interpret : bool -> RTLabs.program -> AST.trace

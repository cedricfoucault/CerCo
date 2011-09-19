
(** This module provides a function to interpret a [LIN] program and
    return the trace of cost labels encountered. *)

val interpret: bool -> LIN.program -> AST.trace

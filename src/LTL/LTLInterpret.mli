
(** This module provides a function to interpret a [LTL] program and
    return the trace of cost labels encountered. *)

val interpret: bool -> LTL.program -> AST.trace

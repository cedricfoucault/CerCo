
(** This module provides a function to interpret a [MIPS] program and
    return the trace of cost labels encountered. *)

val interpret : bool -> ASM.program -> AST.trace

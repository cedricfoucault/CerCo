(** This module provides a function to interpret a [Clight] program
    and return the trace of cost labels encountered. This function
    can also print debug informations.  *)

(** [interpret debug p] returns the trace of execution of program [p]. *)

val interpret: bool -> Clight.program -> AST.trace

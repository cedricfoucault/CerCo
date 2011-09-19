
(** This module provides a function to interpret a [Arch] program and
    return the trace of cost labels encountered. *)

module Make (A : Arch.ARCH) : sig

  val interpret : bool -> A.register Arch.generic_program -> AST.trace

end

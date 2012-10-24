(** This module provides a function to interpret a [Cminor] program and
    return the trace of cost labels encountered. This function can
    also print debug informations.  *)

module Eval_op (M : Memory.S) : sig
  val concrete_stacksize : AST.abstract_size -> int
  val cst :
    'a M.memory -> M.Value.address -> AST.sig_type -> AST.cst -> M.Value.t
  val op1 :
    AST.sig_type (* returned type *) -> AST.sig_type -> AST.op1 -> M.Value.t ->
    M.Value.t
  val op2 :
    AST.sig_type (* returned type *) -> AST.sig_type -> AST.sig_type ->
    AST.op2 -> M.Value.t -> M.Value.t -> M.Value.t
end

val interpret : bool -> Cminor.program -> AST.trace

(** This module offers functions for dynamic verification of invariants. *)

(** [same_traces ts] checks that the collected execution traces are 
    identical (modulo permutation). *)
val same_traces : (Languages.ast * AST.trace) list -> unit

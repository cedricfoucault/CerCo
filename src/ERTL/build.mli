(* Pasted from Pottier's PP compiler *)

(** This module builds an interference graph for an [ERTL] function.
    This is done by running a liveness analysis and exploiting its
    result. [build] returns both the result of the liveness analysis
    and the interference graph. *)

val build: ERTL.internal_function -> Liveness.valuation * Interference.graph


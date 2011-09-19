(* Pasted from Pottier's PP compiler *)

(** This module offers functions that count how many times each
    pseudo-register is used within a piece of [ERTL] code. This is used
    in [Coloring] to drive the spilling heuristics. *)

(* [examine_procedure int_fun] counts how many times each pseudo-register
   is used within procedure [int_fun]. It returns a function that maps
   pseudo-registers to integer use counts. *)

val examine_internal: ERTL.internal_function -> (Register.t -> int)


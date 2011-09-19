
(** This module performs a transformation of a [Clight] program with potentially
    32 and 16 bits integers to an equivalent [Clight] program that only uses 8
    bits integers. 

    The main changes are: defining two types that represent 32 and 16 bits
    integers with a structure of 8 bits integers, making the substitution,
    replacing primitive integer operations on 32 and 16 bits integers with new
    functions emulating them on the new types, and finally defining a global
    variable for each 32 and 16 bits integer constant, which is then replaced by
    its associated variable. *)

val translate : Clight.program -> Clight.program

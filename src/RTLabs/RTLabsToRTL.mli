
(** This module provides a translation of [RTLabs] programs to [RTL]
    programs. *)

(** Most of the work consists in the instruction selection. We target the 8051
    processor, which has addresses coded on two machine words. *)

val translate : RTLabs.program -> RTL.program

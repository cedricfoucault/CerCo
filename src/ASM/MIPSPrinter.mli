
(** This module provides a function to print [MIPS] programs in the SPIM
    syntax. *)

(* Adapted from Pottier's PP compiler *)

(* This module prints [MIPS] programs. It is slightly more than just a printer:
   it also inserts code and directives that reserve space for global variables,
   initialize [$gp], and provide code for the primitive operations. One might
   say that this is really a translation to MIPS assembly language. *)

open Print
open PrintPottier

val print_program: MIPS.program printer


(** This module translates a [Cminor] program into a [RTLabs] program. *)

(** The main part of the translation is transforming a Cminor program into a
    control flow graph. This is done from the end of the program and up to the
    beginning. *)

val translate : Cminor.program -> RTLabs.program

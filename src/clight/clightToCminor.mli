(** This module translates a {!Clight} program into a {!Cminor}
    program. *)

val sizeof_ctype : Clight.ctype -> AST.abstract_size

(** [translate cp] compiles a Clight program into a Cminor program. *)
(* Translation simplifies control structures and explicits memory operations *)

val translate : Clight.program -> Cminor.program

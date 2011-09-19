
(** This module performs a switch simplification: they are replaced by
    equivalent if-then-else statements. This is a temporary hack before
    implementing switch tables. *)

val simplify : Clight.program -> Clight.program

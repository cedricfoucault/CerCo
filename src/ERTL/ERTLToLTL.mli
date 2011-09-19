
(** This module is the external part of the translation of [ERTL]
    programs into [LTL] programs. *)

(** The translation consists in the following operations:
    - Build an interference graph and color it. This process relies an a
      liveness analysis and allows to associate a physical location to each
      pseudo-register.
    - Do the actual translation by removing the statements whose written
      register is dead (using the results of the liveness analysis). *)

val translate : ERTL.program -> LTL.program

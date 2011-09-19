
(** This module is the external part of the translation of [LTL]
    programs into [LIN] programs. *)

(** The translation mainly consists in compressing the graph (remove all gotos
    statements) and then transform the graph structure in a sequence of
    statements. *)

val translate : LTL.program -> LIN.program

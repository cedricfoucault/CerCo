
(** This module optimizes [LTL] code by suppressing all empty
    statements. *)

(* Pasted from Pottier's PP compiler *)

(* This module optimizes [LTL] code by suppressing all [St_skip] statements. In
   short, every statement whose successor is a [St_skip] statement is modified
   so that its successor is the successor of the [St_skip] statement, and this
   is repeated until no reachable [St_skip] statements remain. Unreachable
   [St_skip] statements remain in the graph, but will be implicitly eliminated
   in the translation of [LTL] to [LIN]. *)

val compress: Label.t -> LTL.graph -> (Label.t * LTL.graph)

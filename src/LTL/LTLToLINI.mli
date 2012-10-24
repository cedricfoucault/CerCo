
(** This module is the central part of the translation of [LTL]
    programs into [LIN] programs. *)

(* Adapted from Pottier's PP compiler *)

(* The functor [Visit] implements the core of the translation of
   [LTL] to [LIN]. *)

module Visit (S : sig

  (* [fetch l] is the statement found at label [l] in the source
     program. *)

  val fetch: Label.t -> LTL.statement

  (* [translate_statement stmt] translates the [LTL] statement [stmt] to
     a [LIN] statement. [LTL] statements that have one explicit
     successor are turned into [LIN] statements with an implicit
     successor. [LTL] statements that have two explicit successors
     are turned into [LIN] statements where the first successor is
     explicit and the second successor is implicit. *)

  val translate_statement: LTL.statement -> LIN.statement

  (* [generate stmt] generates statement [stmt]. Statements are
     generated sequentially. *)

  val generate: LIN.statement -> unit

  (* [require l] records the fact that the label [l] should explicitly
     exist in the [LIN] program. It must be used whenever a [LIN]
     branch statement is issued. *)

  val require: Label.t -> unit

  (* [mark l] marks the label [l]. [marked l] tells whether [l] is
     marked. *)

  val mark: Label.t -> unit
  val marked: Label.t -> bool

end) : sig

  (* [visit] implements a depth-first traversal of the control flow graph,
     generating statements as new nodes are being discovered.

     If label [l] has already been discovered, then [visit l] issues
     an [St_goto] statement towards [l]. If [l] has not been
     discovered yet, [visit l] marks [l] as discovered, issues an
     [St_label] statement, translates the statement found at [l] in
     the source program, and visits its successors. *)

  val visit: Label.t -> unit

end


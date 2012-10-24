
(** This module is the central part of the translation of [ERTL]
    programs into [LTL] programs. *)

(* Pasted from Pottier's PP compiler *)

(* This module translates [ERTL] statements into [LTL] statements. It is
   parameterized over a module [Env], whose signature appears below, which
   provides support for mapping pseudo-registers to stack slots or hardware
   registers and for generating instructions (which requires allocating fresh
   control flow graph labels). *)

type decision =
  | Spill of AST.immediate
  | Color of Driver.TargetArch.register

module Make (Env : sig

  val lookup: Register.t -> decision

  (* [generate instruction] returns a fresh instruction label, which
     it associates with [instruction] in the control flow graph. *)

  val generate: LTL.statement -> Label.t

  val fresh_label: unit -> Label.t

  val add_graph: Label.t -> LTL.statement -> unit

  val locals: int

  val stacksize: int

	     end) : sig

  (* [translate_statement] turns a [ERTL] statement into a [LTL] statement, or
     sequence of statements, that transfers control to the same label(s).

     Existing statement labels are preserved, that is, the labels in
     the new control flow graph form a superset of the labels in the
     existing control flow graph. *)

  val translate_statement: ERTL.statement -> LTL.statement

end


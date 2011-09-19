(* Adapted from Pottier's PP compiler *)

(* ------------------------------------------------------------------------- *)

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

end) = struct

  open S

  let rec visit l =

    if marked l then begin

      (* Label [l] has been visited before. This implies that an [St_label l]
	 statement has been issued already. We must now generate an
	 [St_skip] statement that transfers control to this place. Because
	 [l] is the target of a branch statement, we require it to exist
	 in the [LIN] code. *)

      require l;
      generate (LIN.St_skip l)

    end
    else begin

      (* Label [l] has never been visited before. First, record that it
	 now has been visited, so as to avoid looping. *)

      mark l;

      (* Then, generate an [St_label l] statement. This statement
	 will be useless if [l] turns out not to be the target of a
	 branch: this is taken care of later. *)

      generate (LIN.St_label l);

      (* Fetch the statement found at label [l] in the source program. *)

      let statement = fetch l in

      (* Translate the statement. *)

      generate (translate_statement statement);

      (* Note that [translate_statement] never produces an [St_skip]
	 statement. As a result, the code above never generates an [St_label]
	 statement immediately followed with an [St_skip] statement. This
	 proves that we never generate a (conditional or unconditional) branch
	 towards an [St_skip] statement. *)

      (* There now remains to visit the statement's successors. *)

      match statement with

	(* Sequential statements. There is only one successor, with implicit
	   fallthrough. *)

	| LTL.St_skip l
	| LTL.St_comment (_, l)
	| LTL.St_cost (_, l)
	| LTL.St_int (_, _, l)
	| LTL.St_addr (_, _, l)
	| LTL.St_unop (_, _, _, l)
	| LTL.St_binop (_, _, _, _, l)
	| LTL.St_load (_, _, _, l)
	| LTL.St_store (_, _, _, l)
	| LTL.St_call (_, l) ->

	  visit l

	(* Conditional branch statement. The label that is reached by
	   falling through in [LIN] is [l2], which means that it must be
	   translated first, so that its statements are contiguous with the
	   [LIN] branch statement. The label [l1], on the other hand,
	   becomes the target of a jump, so it is required to exist in the
	   [LIN] code.

	   Code for [l1] is generated, if necessary, after we are done dealing
	   with [l2]. If [l1] has already been visited at this point, no code
	   needs be produced, so the second call to visit is made only if [l1]
	   has not been visited yet. *)

	| LTL.St_cond (_, l1, l2) ->

	  visit l2;
	  require l1;
	  if not (marked l1) then
	    visit l1

	(* Statement without a successor. *)

	(* We would prefer to duplicate, rather than share, these
	   statements. Indeed, it is inefficient to generate a jump towards
	   one of these statements. Unfortunately, it is not easy to achieve
	   this, for two reasons. First, frame deletion is in the way. Second,
	   and worse, we must not generate duplicate labels. Maybe I will find
	   a fix in the future. *)

	| LTL.St_tailcall _
	| LTL.St_return ->

	  ()

    end

end


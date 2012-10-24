
(* Pasted from Pottier's PP compiler *)

open Interference

let build (int_fun : ERTL.internal_function) =

  (* Perform liveness analysis. *)

  let liveafter = Liveness.analyze int_fun in

  (* Create an interference graph whose vertices are the procedure's
     pseudo-registers. This graph initially has no edges. *)

  let graph = create int_fun.ERTL.f_locals in

  (* Every pseudo register interferes with special forbidden registers. *)

  let graph = mkiph graph int_fun.ERTL.f_locals Driver.TargetArch.forbidden in

  (* Iterate over all statements in the control flow graph and populate the
     interference graph with interference and preference edges. *)

  let graph =
    Label.Map.fold (fun label stmt graph ->
      let live = liveafter label in
      match Liveness.eliminable live stmt with

      | Some _ ->

	  (* This statement is eliminable and should be ignored. Eliminable
	     statements have not been eliminated yet, because we are still
	     in between ERTL and LTL. They *will* be eliminated soon, though,
	     so there is no reason to take them into account while building
	     the interference graph. *)

	  graph

      | None ->

	  (* Create interference edges. The general rule is, every
	     pseudo-register or hardware register that is defined (written) by
	     a statement interferes with every pseudo-register or hardware
	     register (other than itself) that is live immediately after the
	     statement executes.

	     An exception to the general rule can be made for move
	     statements. In a move statement, we do not need the source
	     and destination pseudo-registers to be assigned distinct hardware
	     registers, since they contain the same value -- in fact, we would
	     like them to be assigned the same hardware register. So, for a
	     move statement, we let the register that is defined (written)
	     interfere with every pseudo-register, other than itself *and
	     other than the source pseudo-register*, that is live immediately
	     after the statement executes. This optimization is explained in
	     Chapter 10 of Appel's book (p. 221).

	     This special case is only a hack that works in many cases. There
	     are cases where it doesn't suffice. For instance, if two
	     successive move statements have the same source [r0], then
	     their destinations [r1] and [r2] *will* be considered as
	     interfering, even though it would in fact be correct and
	     desirable to map both of them to the same hardware register. A
	     more general solution would be to perform a static analysis that
	     determines, for every program point, which pseudo-registers
	     definitely hold the same value, and to exploit this information
	     to build fewer interference edges. *)

	  let defined = Liveness.defined stmt in
	  let exceptions =
	    match stmt with
	    | ERTL.St_unop (Arch.UOpAddi 0, _, sourcer, _)
	    | ERTL.St_move (_, sourcer, _)
	    | ERTL.St_set_hdw (_, sourcer, _) ->
	      Liveness.L.psingleton sourcer
	    | ERTL.St_get_hdw (_, sourcehwr, _) ->
	      Liveness.L.hsingleton sourcehwr
	    | _ ->
	      Liveness.L.bottom
	  in
	  let graph =
	    mki graph (Liveness.L.diff live exceptions) defined
	  in

(*
	  (* Two registers written at the same time are interfering (they
	     obviously should not be associated the same address).
	     Only happens with St_addr. *)

	  let graph =
	    match stmt with
	      | St_addr (r1, r2, _, _) ->
		mki graph (Liveness.L.psingleton r1) (Liveness.L.psingleton r2)
	      | _ ->
		graph
	  in
*)

	  (* Create preference edges between pseudo-registers. Two registers
	     should preferably be assigned the same color when they are
	     related by a move statement, so that the move statement can
	     be eliminated. *)

	  let graph =
	    match stmt with
	    | ERTL.St_unop (Arch.UOpAddi 0, r1, r2, _)
	    | ERTL.St_move (r1, r2, _) ->
		mkppp graph r1 r2
	    | ERTL.St_get_hdw (r, hwr, _)
	    | ERTL.St_set_hdw (hwr, r, _) ->
		mkpph graph r hwr
	    | _ ->
		graph
	  in

	  (* Add interference edges between the hardware register [$zero]
	     and every pseudo-register that the statement renders
	     nonzeroable. See [Zero] for an explanation. *)

	  let graph =
	    mkiph graph (Zero.nonzeroable stmt)
	      (Driver.TargetArch.RegisterSet.singleton Driver.TargetArch.zero)
	  in
	  graph

    ) int_fun.ERTL.f_graph graph
  in

  (* Done. *)

  liveafter, graph

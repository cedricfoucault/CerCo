
(* This module provides a translation of LTL programs to LIN programs. *)

(* Adapted from Pottier's PP compiler *)

(* ------------------------------------------------------------------------- *)

(* [translate_statement] translates an [LTL] statement into a [LIN]
   statement. *)

let translate_statement = function

  (* Because [Branch.compress] has been called before, no [St_skip]
     statement can be reached. *)

  | LTL.St_skip lbl ->
    LIN.St_skip lbl

  (* Sequential statements. *)

  | LTL.St_comment (s, _) ->
    LIN.St_comment s
  | LTL.St_cost (lbl, _) ->
    LIN.St_cost lbl
  | LTL.St_int (r, i, _) ->
    LIN.St_int (r, i)
  | LTL.St_addr (addr, x, _) ->
    LIN.St_addr (addr, x)
  | LTL.St_unop (unop, destr, srcr, _) ->
    LIN.St_unop (unop, destr, srcr)
  | LTL.St_binop (binop, destr, srcr1, srcr2, _) ->
    LIN.St_binop (binop, destr, srcr1, srcr2)
  | LTL.St_load (size, destr, addr, _) ->
    LIN.St_load (size, destr, addr)
  | LTL.St_store (size, addr, srcr, _) ->
    LIN.St_store (size, addr, srcr)
  | LTL.St_call (f, _) ->
    LIN.St_call f
  | LTL.St_tailcall f ->
    LIN.St_tailcall f

  (* Conditional branch statement. In [LIN], control implicitly
     falls through to the second successor, so only the first
     successor is explicitly mentioned in the statement. *)

  | LTL.St_cond (r, lbl_true, _) ->
    LIN.St_cond (r, lbl_true)

  (* Statement without a successor. *)

  | LTL.St_return ->
    LIN.St_return

(* ------------------------------------------------------------------------- *)

(* [translate entry graph] turns an [LTL] control flow graph into
   a [LIN] sequence of statements. *)

let translate_graph entry graph =

  (* Keep track of the labels that have been visited (initially none), of the
     labels that must exist within the [LIN] code (initially only the graph's
     entry point) and of the list of [LIN] statements that are being
     generated (initially empty). Statements are held in the list in reverse
     order, for efficiency. The list is reversed once generation is over. *)

  let visited, required, statements =
    ref Label.Set.empty, ref (Label.Set.singleton entry), ref []
  in

  (* Instantiate the functor. *)

  let module V = LTLToLINI.Visit (struct
    let fetch label =
      Label.Map.find label graph
    let translate_statement =
      translate_statement
    let generate statement =
      statements := statement :: !statements
    let require label =
      required := Label.Set.add label !required
    let mark label =
      visited := Label.Set.add label !visited
    let marked label =
      Label.Set.mem label !visited
  end) in

  (* Traverse the control flow graph. *)

  V.visit entry;

  (* Now, get rid of the labels that do not need to exist. Also,
     reverse the list to re-establish the correct order. *)

  List.filter (function
    | LIN.St_label l ->
	Label.Set.mem l !required
    | _ ->
	true
  ) (List.rev !statements)

(* ------------------------------------------------------------------------- *)

(* Extend the translation to procedures and programs. *)

let translate_internal int_fun =
  (* Compress the graph to eliminate gotos (St_skip) statements. *)
  let entry, graph = Branch.compress int_fun.LTL.f_entry int_fun.LTL.f_graph in
  translate_graph entry graph

let translate_funct (name, def) =
  let def' = match def with
    | LTL.F_int def -> LIN.F_int (translate_internal def)
    | LTL.F_ext def -> LIN.F_ext def in
  (name, def')

let translate (p : LTL.program) : LIN.program =
  { LIN.globals = p.LTL.globals ;
    LIN.functs = List.map translate_funct p.LTL.functs ;
    LIN.main = p.LTL.main }

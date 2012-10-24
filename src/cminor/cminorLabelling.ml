
(** This module defines the labelling of a [Cminor] program. *)


let prefix = "_cost"


(* Add a cost label in front of a statement. *)

let add_starting_cost_label cost_universe stmt =
  Cminor.St_cost (CostLabel.Gen.fresh cost_universe, stmt)

(* Add a cost label at the end of a statement. *)

let add_ending_cost_label cost_universe stmt =
  Cminor.St_seq (stmt, add_starting_cost_label cost_universe Cminor.St_skip)


(* This function adds cost labels to an expression, given the result on its
   sub-expressions. *)

let f_add_cost_labels_exp cost_universe e subexp_res = match e, subexp_res with
  | Cminor.Id _, _ | Cminor.Cst _, _ -> e
  | Cminor.Op1 (op1, _), [e] -> Cminor.Op1 (op1, e)
  | Cminor.Op2 (op2, _, _), [e1 ; e2] -> Cminor.Op2 (op2, e1, e2)
  | Cminor.Mem (chunk, _), [e] -> Cminor.Mem (chunk, e)
  | Cminor.Cond _, [e1 ; e2 ; e3] ->
    let e2 = Cminor.Exp_cost (CostLabel.Gen.fresh cost_universe, e2) in
    let e3 = Cminor.Exp_cost (CostLabel.Gen.fresh cost_universe, e3) in
    Cminor.Cond (e1, e2, e3)
  | Cminor.Exp_cost (lab, _), [e] -> Cminor.Exp_cost (lab, e)
  | _ -> assert false (* wrong number of arguments *)

(* This function adds cost labels to a statement, given the result on its
   sub-expressions and sub-statements. *)

let f_add_cost_labels_body cost_universe stmt subexp_res substmt_res =
  match stmt, subexp_res, substmt_res with
    | Cminor.St_skip, _, _ | Cminor.St_exit _, _, _
    | Cminor.St_goto _, _, _ | Cminor.St_return None, _, _ ->
      stmt
    | Cminor.St_assign (x, _), [e], _ ->
      Cminor.St_assign (x, e)
    | Cminor.St_store (chunk, _, _), [e1 ; e2], _ ->
      Cminor.St_store (chunk, e1, e2)
    | Cminor.St_call (x, _, _, sg), f :: args, _ ->
      Cminor.St_call (x, f, args, sg)
    | Cminor.St_tailcall (_, _, sg), f :: args, _ ->
      Cminor.St_tailcall (f, args, sg)
    | Cminor.St_seq _, _, [stmt1 ; stmt2] ->
      Cminor.St_seq (stmt1, stmt2)
    | Cminor.St_ifthenelse _, [e], [stmt1 ; stmt2] ->
      let stmt1 = add_starting_cost_label cost_universe stmt1 in
      let stmt2 = add_starting_cost_label cost_universe stmt2 in
      Cminor.St_ifthenelse (e, stmt1, stmt2)
    | Cminor.St_loop _, _, [stmt] ->
      let stmt = add_starting_cost_label cost_universe stmt in
      add_ending_cost_label cost_universe (Cminor.St_loop stmt)
    | Cminor.St_block _, _, [stmt] ->
      Cminor.St_block stmt
    | Cminor.St_switch (_, tbl, dflt), [e], _ ->
      add_ending_cost_label cost_universe (Cminor.St_switch (e, tbl, dflt))
    | Cminor.St_return _, [e], _ ->
      Cminor.St_return (Some e)
    | Cminor.St_label (lab, _), _, [stmt] ->
      let stmt = add_starting_cost_label cost_universe stmt in
      Cminor.St_label (lab, stmt)
    | Cminor.St_cost (lab, _), _, [stmt] ->
      Cminor.St_cost (lab, stmt)
    | _ -> assert false (* wrong number of arguments *)

(* Add cost labels to a statement. *)

let add_cost_labels_body cost_universe stmt =
  CminorFold.statement
    (f_add_cost_labels_exp cost_universe)
    (f_add_cost_labels_body cost_universe)
    stmt

(* Add cost labels to a function definition. *)

let add_cost_labels_functs cost_universe functs (f_id, f_def) =
  match f_def with
    | Cminor.F_int def ->
	let body = add_cost_labels_body cost_universe def.Cminor.f_body in
	let body = add_starting_cost_label cost_universe body in
	let def' = { def with Cminor.f_body = body } in
	functs @ [(f_id, Cminor.F_int def')]
    | Cminor.F_ext _ -> functs @ [(f_id, f_def)]

(** [add_cost_labels prog] inserts some labels to enable cost annotation. *)

let add_cost_labels prog =
  let labs = CminorAnnotator.all_labels prog in
  let labs = StringTools.Set.fold CostLabel.Set.add labs CostLabel.Set.empty in
  let cost_prefix = CostLabel.Gen.fresh_prefix labs prefix in
  let cost_universe = CostLabel.Gen.new_universe cost_prefix in
  let functs =
    List.fold_left (add_cost_labels_functs cost_universe) []
      prog.Cminor.functs in
  { prog with Cminor.functs = functs }

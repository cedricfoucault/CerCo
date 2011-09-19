
let int_size = Driver.CminorMemory.int_size

let funct_vars (id, fun_def) = match fun_def with
  | Cminor.F_int def ->
    id :: (List.map fst (def.Cminor.f_params @ def.Cminor.f_vars))
  | _ -> [id]

let prog_idents p =
  let vars = List.map (fun (x, _, _) -> x) p.Cminor.vars in
  let f vars funct = vars @ (funct_vars funct) in
  let vars = List.fold_left f vars p.Cminor.functs in
  let f vars var = StringTools.Set.add var vars in
  List.fold_left f StringTools.Set.empty vars

let fresh_cost_id prefix p =
  let vars = prog_idents p in
  StringTools.Gen.fresh_prefix vars prefix


(*
let increment cost_id incr =
  let cost =
    Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol cost_id), AST.Sig_ptr) in
  let load = Cminor.Expr (Cminor.Mem (Memory.QInt 4, cost), AST.Sig_int 4) in
  let incr = Cminor.Expr (Cminor.Cst (AST.Cst_int incr), AST.Sig_int 4) in
  let incr = Cminor.Expr (Cminor.Op2 (AST.Op_add, load, incr), AST.Sig_int 4) in
  Cminor.St_store (Memory.QInt 4, cost, incr)


let f_remove_cost_labels_exp e subexp_res = match e, subexp_res with
  | Cminor.Id _, _ | Cminor.Cst _, _ -> e
  | Cminor.Op1 (op, _), [e1] -> Cminor.Op1 (op, e1)
  | Cminor.Op2 (op, _, _), [e1 ; e2] -> Cminor.Op2 (op, e1, e2)
  | Cminor.Mem (chunk, _), [e] -> Cminor.Mem (chunk, e)
  | Cminor.Cond (_, _, _), [e1 ; e2 ; e3] -> Cminor.Cond (e1, e2, e3)
  | Cminor.Exp_cost _, [e] -> e
  | _ -> assert false (* wrong number of arguments *)

let remove_cost_labels_exp e =
  CminorFold.expression f_remove_cost_labels_exp e

let remove_cost_labels_exps exps =
  List.map remove_cost_labels_exp exps


let list_seq l =
  let f res stmt = Cminor.St_seq (res, stmt) in
  List.fold_left f Cminor.St_skip l

let f_update_cost_exp costs_mapping cost_id e subexp_res =
  match e, subexp_res with
    | Cminor.Cond (e1, _, _), [stmt1 ; stmt2 ; stmt3] ->
      Cminor.St_seq (stmt1, Cminor.St_ifthenelse (e1, stmt2, stmt3))
    | Cminor.Exp_cost (lbl, _), [stmt2] ->
      let incr =
	if CostLabel.Map.mem lbl costs_mapping then
	  CostLabel.Map.find lbl costs_mapping
	else 0 in
      let stmt1 = increment cost_id incr in
      Cminor.St_seq (stmt1, stmt2)
    | _ -> list_seq subexp_res

let update_cost_exp costs_mapping cost_id e =
  CminorFold.expression (f_update_cost_exp costs_mapping cost_id) e

let update_cost_exps costs_mapping cost_id exps =
  let l = List.map (update_cost_exp costs_mapping cost_id) exps in
  let f stmt upd = Cminor.St_seq (stmt, upd) in
  List.fold_left f Cminor.St_skip l


let rec instrument_stmt costs_mapping cost_id body = match body with
  | Cminor.St_skip | Cminor.St_exit _ | Cminor.St_return None -> body
  | Cminor.St_assign (x, e) ->
      let upd = update_cost_exp costs_mapping cost_id e in
      let e = remove_cost_labels_exp e in
      Cminor.St_seq (upd, Cminor.St_assign (x, e))
  | Cminor.St_store (chunk, e1, e2) ->
      let upd = update_cost_exps costs_mapping cost_id [e1 ; e2] in
      let e1 = remove_cost_labels_exp e1 in
      let e2 = remove_cost_labels_exp e2 in
      Cminor.St_seq (upd, Cminor.St_store (chunk, e1, e2))
  | Cminor.St_call (ox, f, args, sg) ->
      let upd = update_cost_exps costs_mapping cost_id (f :: args) in
      let f = remove_cost_labels_exp f in
      let args = remove_cost_labels_exps args in
      Cminor.St_seq (upd, Cminor.St_call (ox, f, args, sg))
  | Cminor.St_tailcall (f, args, sg) ->
      let upd = update_cost_exps costs_mapping cost_id (f :: args) in
      let f = remove_cost_labels_exp f in
      let args = remove_cost_labels_exps args in
      Cminor.St_seq (upd, Cminor.St_tailcall (f, args, sg))
  | Cminor.St_seq (stmt1, stmt2) ->
      let stmt1 = instrument_stmt costs_mapping cost_id stmt1 in
      let stmt2 = instrument_stmt costs_mapping cost_id stmt2 in
      Cminor.St_seq (stmt1, stmt2)
  | Cminor.St_ifthenelse (e, stmt1, stmt2) ->
      let upd = update_cost_exp costs_mapping cost_id e in
      let e = remove_cost_labels_exp e in
      let stmt1 = instrument_stmt costs_mapping cost_id stmt1 in
      let stmt2 = instrument_stmt costs_mapping cost_id stmt2 in
      Cminor.St_seq (upd, Cminor.St_ifthenelse (e, stmt1, stmt2))
  | Cminor.St_loop stmt ->
      Cminor.St_loop (instrument_stmt costs_mapping cost_id stmt)
  | Cminor.St_block stmt ->
      Cminor.St_block (instrument_stmt costs_mapping cost_id stmt)
  | Cminor.St_switch (e, tbl, dflt) ->
      let upd = update_cost_exp costs_mapping cost_id e in
      let e = remove_cost_labels_exp e in
      Cminor.St_seq (upd, Cminor.St_switch (e, tbl, dflt))
  | Cminor.St_label (lbl, stmt) ->
      Cminor.St_label (lbl, instrument_stmt costs_mapping cost_id stmt)
  | Cminor.St_goto lbl -> body
  | Cminor.St_return (Some e) ->
      let upd = update_cost_exp costs_mapping cost_id e in
      let e = remove_cost_labels_exp e in
      Cminor.St_seq (upd, Cminor.St_return (Some e))
  | Cminor.St_cost (lbl, stmt) ->
      let incr =
	if CostLabel.Map.mem lbl costs_mapping then
	  CostLabel.Map.find lbl costs_mapping
	else 0 in
      let stmt = instrument_stmt costs_mapping cost_id stmt in
      if incr = 0 then stmt
      else Cminor.St_seq (increment cost_id incr, stmt)


let instrument_function costs_mapping cost_id = function
  | Cminor.F_int int_def ->
      let body = instrument_stmt costs_mapping cost_id int_def.Cminor.f_body in
      let def = { int_def with Cminor.f_body = body} in
      Cminor.F_int def
  | def -> def


(** [instrument prog cost_map] instruments the program [prog]. First a fresh
    global variable --- the so-called cost variable --- is added to the program.
    Then, each cost label in the program is replaced by an increment of the cost
    variable, following the mapping [cost_map]. The function also returns the
    name of the cost variable and the name of the cost increment function. *)

let instrument p costs_mapping =
  let cost_id = fresh_cost_id "_cost" p in
  let vars = (cost_id, [AST.Data_int32 0]) :: p.Cminor.vars in
  let f (f_name, f_def) =
    (f_name, instrument_function costs_mapping cost_id f_def) in
  let functs = List.map f p.Cminor.functs in
  ({ Cminor.vars   = vars ;
     Cminor.functs = functs ;
     Cminor.main   = p.Cminor.main },
   "" (* TODO *),
   "" (* TODO *))
*)


(* Program cost labels and labels *)

let labels_empty = (CostLabel.Set.empty, Label.Set.empty)

let add_cost_label lbl (cost_label, user_label) =
  (CostLabel.Set.add lbl cost_label, user_label)

let add_label lbl (cost_label, user_label) =
  (cost_label, Label.Set.add lbl user_label)

let labels_union (cost_labels1, user_labels1) (cost_labels2, user_labels2) =
  (CostLabel.Set.union cost_labels1 cost_labels2,
   Label.Set.union user_labels1 user_labels2)

let labels_union_list l =
  let f res labels = labels_union res labels in
  List.fold_left f labels_empty l

let f_exp_labels (Cminor.Expr (ed, _)) subexp_res =
  let labels1 = labels_union_list subexp_res in
  let labels2 = match ed with
    | Cminor.Exp_cost (lbl, _) -> add_cost_label lbl labels_empty
    | _ -> labels_empty in
  labels_union labels1 labels2

let f_body_labels stmt subexp_res substmt_res =
  let labels1 = labels_union_list subexp_res in
  let labels2 = labels_union_list substmt_res in
  let labels = labels_union labels1 labels2 in
  let labels3 = match stmt with
    | Cminor.St_label (lbl, _) -> add_label lbl labels_empty
    | Cminor.St_cost (lbl, _) -> add_cost_label lbl labels_empty
    | _ -> labels_empty in
  labels_union labels labels3

let body_labels stmt = CminorFold.statement f_exp_labels f_body_labels stmt

let prog_labels p =
  let f lbls (_, f_def) = match f_def with
    | Cminor.F_int def ->
	labels_union lbls (body_labels def.Cminor.f_body)
    | _ -> lbls in
  List.fold_left f (CostLabel.Set.empty, Label.Set.empty) p.Cminor.functs

let cost_labels p = fst (prog_labels p)
let user_labels p = snd (prog_labels p)

let all_labels p =
  let (cost_labels, user_labels) = prog_labels p in
  let all =
    CostLabel.Set.fold (fun lbl lbls -> StringTools.Set.add lbl lbls)
      cost_labels StringTools.Set.empty in
  Label.Set.fold (fun lbl lbls -> StringTools.Set.add lbl lbls) user_labels all

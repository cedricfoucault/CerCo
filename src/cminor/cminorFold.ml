
(** This module provides folding functions over the constructors of the
    [Cminor]'s AST. *)


let expression_subs (Cminor.Expr (ed, _)) = match ed with
  | Cminor.Id _ | Cminor.Cst _ -> []
  | Cminor.Op1 (_, e) | Cminor.Mem (_, e) | Cminor.Exp_cost (_, e) -> [e]
  | Cminor.Op2 (_, e1, e2) -> [e1 ; e2]
  | Cminor.Cond (e1, e2, e3) -> [e1 ; e2 ; e3]

let expression_fill_subs (Cminor.Expr (ed, t)) subs =
  let ed = match ed, subs with
    | Cminor.Id _, _ | Cminor.Cst _, _ -> ed
    | Cminor.Op1 (op1, _), e :: _ -> Cminor.Op1 (op1, e)
    | Cminor.Op2 (op2, _, _), e1 :: e2 :: _ -> Cminor.Op2 (op2, e1, e2)
    | Cminor.Mem (size, _), e :: _ -> Cminor.Mem (size, e)
    | Cminor.Cond _, e1 :: e2 :: e3 :: _ -> Cminor.Cond (e1, e2, e3)
    | Cminor.Exp_cost (lbl, _), e :: _ -> Cminor.Exp_cost (lbl, e)
    | _ -> assert false (* wrong parameter size *) in
  Cminor.Expr (ed, t)


(* In [expression f e], [f]'s second argument is the list of
   [expression]'s results on [e]'s sub-expressions. *)

let rec expression f_expr e =
  let sub_es_res = List.map (expression f_expr) (expression_subs e) in
  f_expr e sub_es_res


let statement_subs = function
  | Cminor.St_skip | Cminor.St_exit _ | Cminor.St_return None
  | Cminor.St_goto _ -> ([], [])
  | Cminor.St_assign (_, e) | Cminor.St_switch (e, _, _)
  | Cminor.St_return (Some e) -> ([e], [])
  | Cminor.St_store (_, e1, e2) ->
    ([e1 ; e2], [])
  | Cminor.St_call (_, f, args, _) | Cminor.St_tailcall (f, args, _) ->
    (f :: args, [])
  | Cminor.St_seq (stmt1, stmt2) ->
    ([], [stmt1 ; stmt2])
  | Cminor.St_ifthenelse (e, stmt1, stmt2) ->
    ([e], [stmt1 ; stmt2])
  | Cminor.St_loop stmt | Cminor.St_block stmt
  | Cminor.St_label (_, stmt) | Cminor.St_cost (_, stmt) ->
    ([], [stmt])

let statement_fill_subs stmt sub_es sub_stmts =
  match stmt, sub_es, sub_stmts with
    | (  Cminor.St_skip | Cminor.St_exit _ | Cminor.St_return None
       | Cminor.St_goto _), _, _ -> stmt
    | Cminor.St_assign (x, _), e :: _, _ ->
      Cminor.St_assign (x, e)
    | Cminor.St_switch (_, cases, dflt), e :: _, _ ->
      Cminor.St_switch (e, cases, dflt)
    | Cminor.St_return _, e :: _, _ ->
      Cminor.St_return (Some e)
    | Cminor.St_store (size, _, _), e1 :: e2 :: _, _ ->
      Cminor.St_store (size, e1, e2)
    | Cminor.St_call (x_opt, _, _, sg), f :: args, _ ->
      Cminor.St_call (x_opt, f, args, sg)
    | Cminor.St_tailcall (_, _, sg), f :: args, _ ->
      Cminor.St_tailcall (f, args, sg)
    | Cminor.St_seq _, _, stmt1 :: stmt2 :: _ ->
      Cminor.St_seq (stmt1, stmt2)
    | Cminor.St_ifthenelse _, e :: _, stmt1 :: stmt2 :: _ ->
      Cminor.St_ifthenelse (e, stmt1, stmt2)
    | Cminor.St_loop _, _, stmt :: _ ->
      Cminor.St_loop stmt
    | Cminor.St_block _, _, stmt :: _ ->
      Cminor.St_block stmt
    | Cminor.St_label (lbl, _), _, stmt :: _ ->
      Cminor.St_label (lbl, stmt)
    | Cminor.St_cost (lbl, _), _, stmt :: _ ->
      Cminor.St_cost (lbl, stmt)
    | _ -> assert false (* do not use on these arguments *)

(* In [statement f_expr f_stmt stmt], [f_stmt]'s second argument is the
   list of [expression f_expr]'s results on [stmt]'s sub-expressions, and
   [f_stmt]'s third argument is the list of [statement]'s results
   on [stmt]'s sub-statements. *)

let rec statement f_expr f_stmt stmt =
  let (sub_es, sub_stmts) = statement_subs stmt in
  let sub_es_res = List.map (expression f_expr) sub_es in
  let sub_stmts_res = List.map (statement f_expr f_stmt) sub_stmts in
  f_stmt stmt sub_es_res sub_stmts_res

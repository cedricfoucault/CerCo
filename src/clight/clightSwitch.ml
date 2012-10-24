
(** This module performs a switch simplification: they are replaced by
    equivalent if-then-else statements. This is a temporary hack before
    implementing switch tables. *)

let type_of (Clight.Expr (_, t)) = t


let f_expr e _ = e

let f_stmt lbl stmt sub_exprs_res sub_stmts_res =
  match stmt, sub_stmts_res with
    | Clight.Sbreak, _ -> Clight.Sgoto lbl
    | Clight.Swhile _, _ | Clight.Sdowhile _, _
    | Clight.Sfor _, _ | Clight.Sswitch _, _ -> stmt
    | _ -> ClightFold.statement_fill_subs stmt sub_exprs_res sub_stmts_res

let replace_undeep_break lbl = ClightFold.statement2 f_expr (f_stmt lbl)


let add_starting_lbl fresh stmt =
  let lbl = fresh () in
  (lbl, Clight.Slabel (lbl, stmt))

let add_starting_lbl_list fresh stmts = List.map (add_starting_lbl fresh) stmts

let add_ending_goto lbl stmt =
  Clight.Ssequence (stmt, Clight.Slabel (lbl, Clight.Sskip))

let make_sequence stmts =
  let f sequence stmt = Clight.Ssequence (sequence, stmt) in
  List.fold_left f Clight.Sskip stmts

let simplify_switch fresh e cases stmts =
  let exit_lbl = fresh () in
  let (lbls, stmts) = List.split (add_starting_lbl_list fresh stmts) in
  let stmts = List.map (replace_undeep_break exit_lbl) stmts in
  let rec aux cases lbls = match cases, lbls with
    | Clight.LSdefault _, lbl :: _ -> [Clight.Sgoto lbl]
    | Clight.LScase (i, _, cases), lbl :: lbls ->
      let next_cases = aux cases lbls in
      let ret_type = Clight.Tint (Clight.I32, AST.Signed) in
      let cst_i = Clight.Expr (Clight.Econst_int i, type_of e) in
      let test = Clight.Expr (Clight.Ebinop (Clight.Oeq, e, cst_i), ret_type) in
      Clight.Sifthenelse (test, Clight.Sgoto lbl, Clight.Sskip) :: next_cases
    | _ ->
      (* Do not use on these arguments: wrong list size. *)
      assert false in
  add_ending_goto exit_lbl (make_sequence ((aux cases lbls) @ stmts))

let f_expr e _ = e

let f_stmt fresh stmt sub_exprs_res sub_stmts_res =
  match stmt, sub_stmts_res with
    | Clight.Sswitch (e, cases), sub_stmts ->
      simplify_switch fresh e cases sub_stmts
    | _ -> ClightFold.statement_fill_subs stmt sub_exprs_res sub_stmts_res

let simplify_statement fresh = ClightFold.statement2 f_expr (f_stmt fresh)

let simplify_fundef fresh = function
  | Clight.Internal cfun ->
    let fn_body = simplify_statement fresh cfun.Clight.fn_body in
    Clight.Internal { cfun with Clight.fn_body = fn_body }
  | fundef -> fundef

let simplify p =
  let labels = ClightAnnotator.all_labels p in
  let fresh = StringTools.make_fresh labels "_tmp_switch" in
  let f (id, fundef) = (id, simplify_fundef fresh fundef) in
  { p with Clight.prog_funct = List.map f p.Clight.prog_funct }

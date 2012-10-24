
(** This module provides folding functions over the constructors of the
    [Clight]'s AST. *)


let ctype_subs = function
  | Clight.Tvoid | Clight.Tint _ | Clight.Tfloat _ | Clight.Tcomp_ptr _ -> []
  | Clight.Tpointer ctype | Clight.Tarray (ctype, _) -> [ctype]
  | Clight.Tfunction (args, res) -> args @ [res]
  | Clight.Tstruct (_, fields) | Clight.Tunion (_, fields) ->
    List.map snd fields

let ctype_fill_subs ctype sub_ctypes = match ctype, sub_ctypes with
  | Clight.Tvoid, _ | Clight.Tint _, _ | Clight.Tfloat _, _
  | Clight.Tcomp_ptr _, _ -> ctype
  | Clight.Tpointer _, ctype :: _ -> Clight.Tpointer ctype
  | Clight.Tarray (_, size), ctype :: _ -> Clight.Tarray (ctype, size)
  | Clight.Tfunction _, _ ->
    let (args, res) = MiscPottier.split_last sub_ctypes in
    Clight.Tfunction (args, res)
  | Clight.Tstruct (name, fields), _ ->
    let fields = List.map2 (fun (x, _) ctype -> (x, ctype)) fields sub_ctypes in
    Clight.Tstruct (name, fields)
  | Clight.Tunion (name, fields), _ ->
    let fields = List.map2 (fun (x, _) ctype -> (x, ctype)) fields sub_ctypes in
    Clight.Tunion (name, fields)
  | _ -> assert false (* wrong arguments, do not use on these values *)

let rec ctype f t =
  let sub_ctypes_res = List.map (ctype f) (ctype_subs t) in
  f t sub_ctypes_res


let expr_subs = function
  | Clight.Expr (expr_descr, ctype) -> ([ctype], [expr_descr])

let expr_descr_subs = function
  | Clight.Econst_int _ | Clight.Econst_float _ | Clight.Evar _ -> ([], [])
  | Clight.Ederef e | Clight.Eaddrof e | Clight.Eunop (_, e)
  | Clight.Efield (e, _) -> ([], [e])
  | Clight.Ebinop (_, e1, e2) | Clight.Eandbool (e1, e2)
  | Clight.Eorbool (e1, e2) -> ([], [e1 ; e2])
  | Clight.Ecast (ctype, e) -> ([ctype], [e])
  | Clight.Econdition (e1, e2, e3) -> ([], [e1 ; e2 ; e3])
  | Clight.Esizeof ctype -> ([ctype], [])
  | Clight.Ecost (_, e) -> ([], [e])
  | Clight.Ecall (_, e1, e2) -> ([], [e1 ; e2])

let expr_fill_subs e sub_ctypes sub_expr_descrs =
  match e, sub_ctypes, sub_expr_descrs with
  | Clight.Expr _, ctype :: _, expr_descr :: _ ->
    Clight.Expr (expr_descr, ctype)
  | _ -> assert false (* wrong arguments, do not use on these values *)

let expr_descr_fill_subs e sub_ctypes sub_exprs = 
  match e, sub_ctypes, sub_exprs with
  | Clight.Econst_int _, _, _ | Clight.Econst_float _, _, _
  | Clight.Evar _, _, _ -> e
  | Clight.Ederef _, _, e :: _ -> Clight.Ederef e
  | Clight.Eaddrof _, _, e :: _ -> Clight.Eaddrof e
  | Clight.Eunop (unop, _), _, e :: _ -> Clight.Eunop (unop, e)
  | Clight.Ebinop (binop, _, _), _, e1 :: e2 :: _ ->
    Clight.Ebinop (binop, e1, e2)
  | Clight.Ecast _, ctype :: _, e :: _ -> Clight.Ecast (ctype, e)
  | Clight.Econdition _, _, e1 :: e2 :: e3 :: _ ->
    Clight.Econdition (e1, e2, e3)
  | Clight.Eandbool (_, _), _, e1 :: e2 :: _ ->
    Clight.Eandbool (e1, e2)
  | Clight.Eorbool (_, _), _, e1 :: e2 :: _ ->
    Clight.Eorbool (e1, e2)
  | Clight.Esizeof _, ctype :: _, _ -> Clight.Esizeof ctype
  | Clight.Efield (_, field_name), _, e :: _ -> Clight.Efield (e, field_name)
  | Clight.Ecost (lbl, _), _, e :: _ -> Clight.Ecost (lbl, e)
  | Clight.Ecall (id, _, _), _, e1 :: e2 :: _ -> Clight.Ecall (id, e1, e2)
  | _ -> assert false (* wrong arguments, do not use on these values *)

let expr_fill_exprs (Clight.Expr (ed, t)) exprs =
  let (sub_ctypes, _) = expr_descr_subs ed in
  let ed = expr_descr_fill_subs ed sub_ctypes exprs in
  Clight.Expr (ed, t)

let rec expr f_ctype f_expr f_expr_descr e =
  let (sub_ctypes, sub_expr_descrs) = expr_subs e in
  let sub_expr_descrs_res =
    List.map (expr_descr f_ctype f_expr f_expr_descr) sub_expr_descrs in
  let sub_ctypes_res = List.map (ctype f_ctype) sub_ctypes in
  f_expr e sub_ctypes_res sub_expr_descrs_res

and expr_descr f_ctype f_expr f_expr_descr e =
  let (sub_ctypes, sub_exprs) = expr_descr_subs e in
  let sub_exprs_res =
    List.map (expr f_ctype f_expr f_expr_descr) sub_exprs in
  let sub_ctypes_res = List.map (ctype f_ctype) sub_ctypes in
  f_expr_descr e sub_ctypes_res sub_exprs_res


let expr_subs2 e =
  let (_, expr_descrs) = expr_subs e in
  let l = List.map expr_descr_subs expr_descrs in
  List.flatten (List.map snd l)

let rec expr2 f e = f e (List.map (expr2 f) (expr_subs2 e))


let rec labeled_statements_subs = function
  | Clight.LSdefault stmt -> [stmt]
  | Clight.LScase (_, stmt, lbl_stmts) ->
    stmt :: (labeled_statements_subs lbl_stmts)

let statement_subs = function
  | Clight.Sskip | Clight.Sbreak | Clight.Scontinue | Clight.Sreturn None
  | Clight.Sgoto _ -> ([], [])
  | Clight.Sassign (e1, e2) -> ([e1 ; e2], [])
  | Clight.Scall (None, e, args) -> (e :: args, [])
  | Clight.Scall (Some e1, e2, args) -> (e1 :: e2 :: args, [])
  | Clight.Ssequence (stmt1, stmt2) -> ([], [stmt1 ; stmt2])
  | Clight.Sifthenelse (e, stmt1, stmt2) -> ([e], [stmt1 ; stmt2])
  | Clight.Swhile (e, stmt) | Clight.Sdowhile (e, stmt) -> ([e], [stmt])
  | Clight.Sfor (stmt1, e, stmt2, stmt3) -> ([e], [stmt1 ; stmt2 ; stmt3])
  | Clight.Sreturn (Some e) -> ([e], [])
  | Clight.Sswitch (e, lbl_stmts) -> ([e], labeled_statements_subs lbl_stmts)
  | Clight.Slabel (_, stmt) | Clight.Scost (_, stmt) -> ([], [stmt])

let statement_sub_exprs stmt = fst (statement_subs stmt)

let rec labeled_statements_fill_subs lbl_stmts sub_statements =
  match lbl_stmts, sub_statements with
    | Clight.LSdefault _, stmt :: _ -> Clight.LSdefault stmt
    | Clight.LScase (i, _, lbl_stmts), stmt :: sub_statements ->
      Clight.LScase (i, stmt,
		     labeled_statements_fill_subs lbl_stmts sub_statements)
    | _ -> assert false (* wrong arguments, do not use on these values *)

let statement_fill_subs statement sub_exprs sub_statements =
  match statement, sub_exprs, sub_statements with
    | Clight.Sskip, _, _ | Clight.Sbreak, _, _ | Clight.Scontinue, _, _
    | Clight.Sreturn None, _, _ | Clight.Sgoto _, _, _ -> statement
    | Clight.Sassign _, e1 :: e2 :: _, _ -> Clight.Sassign (e1, e2)
    | Clight.Scall (None, _, _), e :: args, _ ->
      Clight.Scall (None, e, args)
    | Clight.Scall (Some _, _, _), e1 :: e2 :: args, _ ->
      Clight.Scall (Some e1, e2, args)
    | Clight.Ssequence _, _, stmt1 :: stmt2 :: _ ->
      Clight.Ssequence (stmt1, stmt2)
    | Clight.Sifthenelse _, e :: _, stmt1 :: stmt2 :: _ ->
      Clight.Sifthenelse (e, stmt1, stmt2)
    | Clight.Swhile _, e :: _, stmt :: _ ->
      Clight.Swhile (e, stmt)
    | Clight.Sdowhile _, e :: _, stmt :: _ ->
      Clight.Sdowhile (e, stmt)
    | Clight.Sfor _, e :: _, stmt1 :: stmt2 :: stmt3 :: _ ->
      Clight.Sfor (stmt1, e, stmt2, stmt3)
    | Clight.Sreturn (Some _), e :: _, _ -> Clight.Sreturn (Some e)
    | Clight.Sswitch (_, lbl_stmts), e :: _, _ ->
      Clight.Sswitch (e, labeled_statements_fill_subs lbl_stmts sub_statements)
    | Clight.Slabel (lbl, _), _, stmt :: _ -> Clight.Slabel (lbl, stmt)
    | Clight.Scost (lbl, _), _, stmt :: _ -> Clight.Scost (lbl, stmt)
    | _ -> assert false (* wrong arguments, do not use on these values *)

let rec statement f_ctype f_expr f_expr_descr f_statement stmt =
  let (sub_exprs, sub_stmts) = statement_subs stmt in
  let sub_exprs_res = List.map (expr f_ctype f_expr f_expr_descr) sub_exprs in
  let sub_stmts_res =
    List.map (statement f_ctype f_expr f_expr_descr f_statement) sub_stmts in
  f_statement stmt sub_exprs_res sub_stmts_res

let rec statement2 f_expr f_statement stmt =
  let (sub_exprs, sub_stmts) = statement_subs stmt in
  let sub_exprs_res = List.map (expr2 f_expr) sub_exprs in
  let sub_stmts_res = List.map (statement2 f_expr f_statement) sub_stmts in
  f_statement stmt sub_exprs_res sub_stmts_res

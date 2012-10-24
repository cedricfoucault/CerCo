
(** This module performs a transformation of a [Clight] program with potentially
    32 and 16 bits integers to an equivalent [Clight] program that only uses 8
    bits integers. 

    The main changes are: defining two types that represent 32 and 16 bits
    integers with a structure of 8 bits integers, making the substitution,
    replacing primitive integer operations on 32 and 16 bits integers with new
    functions emulating them on the new types, and finally defining a global
    variable for each 32 and 16 bits integer constant, which is then replaced by
    its associated variable. *)


let error_prefix = "Clight32 to Clight8"
let error s = Error.global_error error_prefix s


let cint32s = Clight.Tint (Clight.I32, AST.Signed)
let cint32u = Clight.Tint (Clight.I32, AST.Unsigned)
let cint8s = Clight.Tint (Clight.I8, AST.Signed)
let cint8u = Clight.Tint (Clight.I8, AST.Unsigned)


(* Change the main so that it returns a 8 bits integer. Indeed, 32 bits integers
   will be replaced by structures, and returning a structure from the main is
   not Clight compliant. *)

let main_ret_type = function
  | Clight.Tint (_, AST.Signed) -> cint8s
  | Clight.Tint (_, AST.Unsigned) -> cint8u
  | _ -> error "The main should return an integer which is not the case."

let f_ctype ctype _ = ctype
let f_expr e _ _ = e
let f_expr_descr ed _ _ = ed

let f_stmt ret_type stmt sub_exprs_res sub_stmts_res =
  match stmt, sub_exprs_res with
    | Clight.Sreturn (Some _), e :: _ ->
      let e' = Clight.Expr (Clight.Ecast (ret_type, e), ret_type) in
      Clight.Sreturn (Some e')
    | _ -> ClightFold.statement_fill_subs stmt sub_exprs_res sub_stmts_res

let body_returns ret_type =
  ClightFold.statement f_ctype f_expr f_expr_descr (f_stmt ret_type)

let fundef_returns_char = function
  | Clight.Internal cfun ->
    let ret_type = main_ret_type cfun.Clight.fn_return in
    let body = body_returns ret_type cfun.Clight.fn_body in
    Clight.Internal {cfun with Clight.fn_return = ret_type ;
                               Clight.fn_body = body }
  | Clight.External _ as fundef -> fundef

let main_returns_char p = match p.Clight.prog_main with
  | None -> p
  | Some main ->
    let main_def = List.assoc main p.Clight.prog_funct in
    let main_def = fundef_returns_char main_def in
    let prog_funct =
      MiscPottier.update_list_assoc main main_def p.Clight.prog_funct in
    { p with Clight.prog_funct = prog_funct }


(* Replacement *)

let seq =
  List.fold_left
    (fun stmt1 stmt2 -> Clight.Ssequence (stmt1, stmt2))
    Clight.Sskip

let is_complex = function
  | Clight.Tstruct _ | Clight.Tunion _ -> true
  | _ -> false

let is_subst_complex type_substitutions res_type =
  if List.mem_assoc res_type type_substitutions then
    is_complex (List.assoc res_type type_substitutions)
  else false

let addrof_with_type e ctype =
  let ctype = Clight.Tpointer ctype in
  (Clight.Expr (Clight.Eaddrof e, ctype), ctype)

let address_if_subst_complex type_substitutions =
  let f l (e, ctype) =
    let arg_and_type =
      if is_subst_complex type_substitutions ctype then addrof_with_type e ctype
      else (e, ctype) in
    l @ [arg_and_type] in
  List.fold_left f []

let make_call_struct tmpe res_type f_var args args_types =
  let (res_e, res_type) = addrof_with_type tmpe res_type in
  let f_type = Clight.Tfunction (res_type :: args_types, Clight.Tvoid) in
  let f = Clight.Expr (f_var, f_type) in
  Clight.Scall (None, f, res_e :: args)

let make_call_wo_struct tmpe res_type f_var args args_types =
  let f_type = Clight.Tfunction (args_types, res_type) in
  let f = Clight.Expr (f_var, f_type) in
  Clight.Scall (Some tmpe, f, args)

let make_call type_substitutions tmp f_id args_with_types res_type =
  let tmpe = Clight.Expr (Clight.Evar tmp, res_type) in
  let args_with_types =
    address_if_subst_complex type_substitutions args_with_types in
  let (args, args_types) = List.split args_with_types in
  let f_var = Clight.Evar f_id in
  let call =
    if is_subst_complex type_substitutions res_type then make_call_struct
    else make_call_wo_struct in
  (tmpe, call tmpe res_type f_var args args_types)

let call fresh replaced type_substitutions replacement_assoc
    args added_stmt added_tmps ret_type =
  let tmp = fresh () in
  let replacement_fun_name = List.assoc replaced replacement_assoc in
  let (tmpe, call) =
    make_call type_substitutions tmp replacement_fun_name args ret_type in
  let stmt = seq (added_stmt @ [call]) in
  (tmpe, stmt, added_tmps @ [(tmp, ret_type)])

let replace_ident replacement_assoc x t =
  let new_name = match t with
    | Clight.Tfunction _
	when List.mem_assoc (Runtime.Fun x) replacement_assoc ->
      let replacement_fun_name = List.assoc (Runtime.Fun x) replacement_assoc in
      replacement_fun_name
    | _ -> x in
  (Clight.Expr (Clight.Evar new_name, t), Clight.Sskip, [])

let replace_expression fresh type_substitutions replacement_assoc e =

  let rec aux (Clight.Expr (ed, t) as e) =
    let expr ed = Clight.Expr (ed, t) in
    match ed with

      | Clight.Econst_int _ | Clight.Econst_float _ | Clight.Esizeof _ ->
	(e, Clight.Sskip, [])

      | Clight.Evar x -> replace_ident replacement_assoc x t

      | Clight.Ederef e' ->
	let (e', stmt, tmps) = aux e' in
	(expr (Clight.Ederef e'), stmt, tmps)

      | Clight.Eaddrof e' ->
	let (e', stmt, tmps) = aux e' in
	(expr (Clight.Eaddrof e'), stmt, tmps)

      | Clight.Eunop (unop, (Clight.Expr (ed', t') as e'))
	  when List.mem_assoc (Runtime.Unary (unop, t')) replacement_assoc ->
	let (e', stmt, tmps) = aux e' in
	call fresh (Runtime.Unary (unop, t'))
	  type_substitutions replacement_assoc [(e', t')]
	  [stmt] tmps t

      | Clight.Eunop (unop, e') ->
	let (e', stmt, tmps) = aux e' in
	(expr (Clight.Eunop (unop, e')), stmt, tmps)

      | Clight.Ebinop (binop,
		       (Clight.Expr (ed1, t1) as e1),
		       (Clight.Expr (ed2, t2) as e2))
	  when
	    List.mem_assoc (Runtime.Binary (binop, t1, t2)) replacement_assoc ->
	let (e1, stmt1, tmps1) = aux e1 in
	let (e2, stmt2, tmps2) = aux e2 in
	call fresh (Runtime.Binary (binop, t1, t2))
	  type_substitutions replacement_assoc
	  [(e1, t1) ; (e2, t2)] [stmt1 ; stmt2] (tmps1 @ tmps2) t

      | Clight.Ebinop (binop, e1, e2) ->
	let (e1, stmt1, tmps1) = aux e1 in
	let (e2, stmt2, tmps2) = aux e2 in
	let stmt = seq [stmt1 ; stmt2] in
	(expr (Clight.Ebinop (binop, e1, e2)), stmt, tmps1 @ tmps2)

      | Clight.Ecast (t, (Clight.Expr (_, t') as e'))
	  when List.mem_assoc (Runtime.Cast (t, t')) replacement_assoc ->
	let (e', stmt, tmps) = aux e' in
	call fresh (Runtime.Cast (t, t'))
	  type_substitutions replacement_assoc [(e', t')] [stmt]
	  tmps t

      | Clight.Ecast (t', e') ->
	let (e', stmt, tmps) = aux e' in
	(expr (Clight.Ecast (t', e')), stmt, tmps)

      | Clight.Econdition (e1, e2, e3) ->
	let (e1, stmt1, tmps1) = aux e1 in
	let (e2, stmt2, tmps2) = aux e2 in
	let (e3, stmt3, tmps3) = aux e3 in
	let stmt = seq [stmt1 ; stmt2 ; stmt3] in
	(expr (Clight.Econdition (e1, e2, e3)), stmt, tmps1 @ tmps2 @ tmps3)

      | Clight.Eandbool (e1, e2) ->
	let (e1, stmt1, tmps1) = aux e1 in
	let (e2, stmt2, tmps2) = aux e2 in
	let stmt = seq [stmt1 ; stmt2] in
	(expr (Clight.Eandbool (e1, e2)), stmt, tmps1 @ tmps2)

      | Clight.Eorbool (e1, e2) ->
	let (e1, stmt1, tmps1) = aux e1 in
	let (e2, stmt2, tmps2) = aux e2 in
	let stmt = seq [stmt1 ; stmt2] in
	(expr (Clight.Eorbool (e1, e2)), stmt, tmps1 @ tmps2)

      | Clight.Efield (e', field) ->
	let (e', stmt, tmps) = aux e' in
	(expr (Clight.Efield (e', field)), stmt, tmps)

      | Clight.Ecost (lbl, e') ->
	let (e', stmt, tmps) = aux e' in
	(expr (Clight.Ecost (lbl, e')), stmt, tmps)

      | Clight.Ecall (id, e1, e2) ->
	let (e1, stmt1, tmps1) = aux e1 in
	let (e2, stmt2, tmps2) = aux e2 in
	let stmt = seq [stmt1 ; stmt2] in
	(expr (Clight.Ecall (id, e1, e2)), stmt, tmps1 @ tmps2)

  in
  aux e


let replace_expression_list fresh type_substitutions  replacement_assoc =
  let f (l, stmt, tmps) e =
    let (e', stmt', tmps') =
      replace_expression fresh type_substitutions replacement_assoc e in
    (l @ [e'], seq [stmt ; stmt'], tmps @ tmps') in
  List.fold_left f ([], Clight.Sskip, [])


let replace_statement fresh type_substitutions replacement_assoc =
  let aux_exp =
    replace_expression fresh type_substitutions replacement_assoc in
  let aux_exp_list =
    replace_expression_list fresh type_substitutions replacement_assoc in

  let rec aux = function

    | Clight.Sskip | Clight.Sbreak | Clight.Scontinue | Clight.Sgoto _
    | Clight.Sreturn None
	as stmt -> (stmt, [])

    | Clight.Slabel (lbl, stmt) ->
      let (stmt', tmps) = aux stmt in
      (Clight.Slabel (lbl, stmt'), tmps)

    | Clight.Scost (lbl, stmt) ->
      let (stmt', tmps) = aux stmt in
      (Clight.Scost (lbl, stmt'), tmps)

    | Clight.Sassign (e1, e2) ->
      let (e1', stmt1, tmps1) = aux_exp e1 in
      let (e2', stmt2, tmps2) = aux_exp e2 in
      let stmt = seq [stmt1 ; stmt2 ; Clight.Sassign (e1', e2')] in
      (stmt, tmps1 @ tmps2)

    | Clight.Scall (None, f, args) ->
      let (f', stmt1, tmps1) = aux_exp f in
      let (args', stmt2, tmps2) = aux_exp_list args in
      let stmt = seq [stmt1 ; stmt2 ; Clight.Scall (None, f', args')] in
      (stmt, tmps1 @ tmps2)

    | Clight.Scall (Some e, f, args) ->
      let (e', stmt1, tmps1) = aux_exp e in
      let (f', stmt2, tmps2) = aux_exp f in
      let (args', stmt3, tmps3) = aux_exp_list args in
      let stmt = seq [stmt1 ; stmt2 ; stmt3 ;
		      Clight.Scall (Some e', f', args')] in
      (stmt, tmps1 @ tmps2 @ tmps3)

    | Clight.Sifthenelse (e, stmt1, stmt2) ->
      let (e', stmte, tmpse) = aux_exp e in
      let (stmt1', tmps1) = aux stmt1 in
      let (stmt2', tmps2) = aux stmt2 in
      let stmt = seq [stmte ; Clight.Sifthenelse (e', stmt1', stmt2')] in
      (stmt, tmpse @ tmps1 @ tmps2)

    | Clight.Swhile (e, stmt) ->
      let (e', stmte, tmpse) = aux_exp e in
      let (stmt', tmps) = aux stmt in
      let stmt = seq [stmte ; Clight.Swhile (e', seq [stmt' ; stmte])] in
      (stmt, tmpse @ tmps)

    | Clight.Sdowhile (e, stmt) ->
      let (e', stmte, tmpse) = aux_exp e in
      let (stmt', tmps) = aux stmt in
      let stmt = seq [Clight.Sdowhile (e', seq [stmt' ; stmte])] in
      (stmt, tmpse @ tmps)

    | Clight.Sfor (stmt1, e, stmt2, stmt3) ->
      let (e', stmte, tmpse) = aux_exp e in
      let (stmt1', tmps1) = aux stmt1 in
      let (stmt2', tmps2) = aux stmt2 in
      let (stmt3', tmps3) = aux stmt3 in
      let stmt = seq [stmt1' ; stmte ;
		      Clight.Swhile (e', seq [stmt3' ; stmt2' ; stmte])] in
      (stmt, tmpse @ tmps1 @ tmps2 @ tmps3)

    | Clight.Sswitch (e, lbl_stmts) ->
      let (e', stmte, tmpse) = aux_exp e in
      let (lbl_stmts', tmps) = aux_lbl_stmts lbl_stmts in
      let stmt = seq [stmte ; Clight.Sswitch (e', lbl_stmts')] in
      (stmt, tmpse @ tmps)

    | Clight.Sreturn (Some e) ->
      let (e', stmte, tmpse) = aux_exp e in
      let stmt = seq [stmte ; Clight.Sreturn (Some e')] in
      (stmt, tmpse)

    | Clight.Ssequence (stmt1, stmt2) ->
      let (stmt1', tmps1) = aux stmt1 in
      let (stmt2', tmps2) = aux stmt2 in
      let stmt = seq [stmt1' ; stmt2'] in
      (stmt, tmps1 @ tmps2)

  and aux_lbl_stmts = function

    | Clight.LSdefault stmt ->
      let (stmt', tmps) = aux stmt in
      (Clight.LSdefault stmt', tmps)

    | Clight.LScase (i, stmt, lbl_stmts) ->
      let (stmt', tmps1) = aux stmt in
      let (lbl_stmts', tmps2) = aux_lbl_stmts lbl_stmts in
      (Clight.LScase (i, stmt', lbl_stmts'), tmps1 @ tmps2)

  in

  aux


let f_ctype type_substitutions ctype sub_ctypes_res = match ctype with
  | _ when List.mem_assoc ctype type_substitutions ->
    List.assoc ctype type_substitutions
  | _ -> ClightFold.ctype_fill_subs ctype sub_ctypes_res

let replace_ctype type_substitutions =
  ClightFold.ctype (f_ctype type_substitutions)

let f_expr = ClightFold.expr_fill_subs

let f_expr_descr = ClightFold.expr_descr_fill_subs

let f_stmt = ClightFold.statement_fill_subs

let statement_replace_ctype type_substitutions =
  ClightFold.statement (f_ctype type_substitutions) f_expr f_expr_descr f_stmt


let replace_internal fresh type_substitutions replacement_assoc def =
  let (new_body, tmps) =
    replace_statement fresh type_substitutions replacement_assoc
      def.Clight.fn_body in
  let new_body = statement_replace_ctype type_substitutions new_body in
  let f (x, t) = (x, replace_ctype type_substitutions t) in
  let params = List.map f def.Clight.fn_params in
  let vars = List.map f (def.Clight.fn_vars @ tmps) in
  { Clight.fn_return = replace_ctype type_substitutions def.Clight.fn_return ;
    Clight.fn_params = params ;
    Clight.fn_vars = vars ;
    Clight.fn_body = new_body }

let replace_funct fresh type_substitutions replacement_assoc (id, fundef) =
  let fundef' = match fundef with
    | Clight.Internal def ->
      Clight.Internal
	(replace_internal fresh type_substitutions replacement_assoc def)
    | _ -> fundef in
  (id, fundef')

let replace fresh type_substitutions replacement_assoc p =
  let prog_funct =
    List.map (replace_funct fresh type_substitutions replacement_assoc)
      p.Clight.prog_funct in
  { p with Clight.prog_funct = prog_funct }


(* The constant replacement replaces each 32 bits and 16 bits integer constant
   by a global variable of the same value. They will be replaced by the
   appropriate structural value by the global replacement that comes
   afterwards. *)

module CstMap =
  Map.Make
    (struct
      type t = (int * Clight.intsize * Clight.ctype)
      let compare = Pervasives.compare
     end)

let f_subs fresh replace subs map =
  let f (l, map) x =
    let (x, map) = replace fresh map x in
    (l @ [x], map) in
  List.fold_left f ([], map) subs

let rec replace_constant_expr fresh map (Clight.Expr (ed, t) as e) =
  match ed, t with
    | Clight.Econst_int i, Clight.Tint (Clight.I8, _) ->
      (e, map)
    | Clight.Econst_int i, Clight.Tint (size, _)
      when CstMap.mem (i, size, t) map ->
      let id = CstMap.find (i, size, t) map in
      (Clight.Expr (Clight.Evar id, t), map)
    | Clight.Econst_int i, Clight.Tint (size, _) ->
      let id = fresh () in
      let map = CstMap.add (i, size, t) id map in
      let id = CstMap.find (i, size, t) map in
      (Clight.Expr (Clight.Evar id, t), map)
    | _ ->
      let (sub_ctypes, sub_exprs) = ClightFold.expr_descr_subs ed in
      let (sub_exprs, map) = f_subs fresh replace_constant_expr sub_exprs map in
      let ed = ClightFold.expr_descr_fill_subs ed sub_ctypes sub_exprs in
      (Clight.Expr (ed, t), map)

let rec replace_constant_stmt fresh map stmt =
  let (sub_exprs, sub_stmts) = ClightFold.statement_subs stmt in
  let (sub_exprs, map) =
    f_subs fresh replace_constant_expr sub_exprs map in
  let (sub_stmts, map) =
    f_subs fresh replace_constant_stmt sub_stmts map in
  (ClightFold.statement_fill_subs stmt sub_exprs sub_stmts, map)

let replace_constant_fundef fresh (functs, map) (id, fundef) =
  match fundef with
    | Clight.Internal cfun ->
      let (body, map) = replace_constant_stmt fresh map cfun.Clight.fn_body in
      let fundef = Clight.Internal { cfun with Clight.fn_body = body } in
      (functs @ [(id, fundef)], map)
    | Clight.External _ -> (functs @ [(id, fundef)], map)

let init_datas i size =
  [match size with
    | Clight.I8 -> Clight.Init_int8 i
    | Clight.I16 -> Clight.Init_int16 i
    | Clight.I32 -> Clight.Init_int32 i]

let globals_of_map map =
  let f (i, size, t) id l = l @ [((id, init_datas i size), t)] in
  CstMap.fold f map []

let replace_constant p =
  let tmp_universe = ClightAnnotator.fresh_universe "_cst" p in
  let fresh () = StringTools.Gen.fresh tmp_universe in
  let (functs, map) =
    List.fold_left (replace_constant_fundef fresh)
      ([], CstMap.empty) p.Clight.prog_funct in
  let csts = globals_of_map map in
  { p with
    Clight.prog_funct = functs ; Clight.prog_vars = p.Clight.prog_vars @ csts }


(* Globals replacement *)

let expand_int size i =
  let i = Big_int.big_int_of_int i in
  let i =
    if Big_int.ge_big_int i Big_int.zero_big_int then i
    else Big_int.add_big_int i (Big_int.power_int_positive_int 2 size) in
  let bound = Big_int.power_int_positive_int 2 8 in
  let rec aux n i =
    if n >= size then []
    else
      let (next, chunk) = Big_int.quomod_big_int i bound in
      chunk :: (aux (n+1) next) in
  List.map (fun i -> Clight.Init_int8 (Big_int.int_of_big_int i)) (aux 0 i)

let expand_init_data = function
  | Clight.Init_int16 i -> expand_int 2 i
  | Clight.Init_int32 i -> expand_int 4 i
  | init_data -> [init_data]

let expand_init_datas init_datas =
  List.flatten (List.map expand_init_data init_datas)

let replace_global type_substitutions ((id, init_datas), t) =
  let init_datas = expand_init_datas init_datas in
  ((id, init_datas), replace_ctype type_substitutions t)

let replace_globals type_substitutions p =
  let vars = List.map (replace_global type_substitutions) p.Clight.prog_vars in
  { p with Clight.prog_vars = vars }


(* Unsupported operations by the 8051. *)

(* 8 bits signed division *)

let divs_fun s _ =
  "signed char " ^ s ^ " (signed char x, signed char y) {\n" ^
  "  unsigned char x1 = (unsigned char) x;\n" ^
  "  unsigned char y1 = (unsigned char) y;\n" ^
  "  signed char sign = 1;\n" ^
  "  if (x < 0) { x1 = (unsigned char) (-x); sign = -sign; }\n" ^
  "  if (y < 0) { y1 = (unsigned char) (-y); sign = -sign; }\n" ^
  "  return (sign * ((signed char) (x1/y1)));\n" ^
  "}\n\n"

let divs =
  (Runtime.Binary (Clight.Odiv, cint8s, cint8s), "_divs", [], divs_fun, [])


(* 8 bits signed modulo *)

let mods_fun s _ =
  "signed char " ^ s ^ " (signed char x, signed char y) {\n" ^
  "  return (x - (x/y) * y);\n" ^
  "}\n\n"

let mods =
  (Runtime.Binary (Clight.Omod, cint8s, cint8s), "_mods", [], mods_fun,
   [Runtime.Binary (Clight.Odiv, cint8s, cint8s)])


(* Shifts *)

let sh_fun signedness op s _ =
  signedness ^ " char " ^ s ^ " (" ^ signedness ^ " char x, " ^
  signedness ^ " char y) {\n" ^
  "  " ^ signedness ^ " char res = x, i;\n" ^
  "  for (i = 0 ; i < y ; i++) res = res " ^ op ^ " 2;\n" ^
  "  return res;\n" ^
  "}\n\n"

(* 8 bits shifts left *)

let shls_fun = sh_fun "signed" "*"

let shls =
  (Runtime.Binary (Clight.Oshl, cint8s, cint8s), "_shls", [], shls_fun, [])

let shlu_fun s _ =
  "unsigned char " ^ s ^ " (unsigned char x, unsigned char y) {\n" ^
  "  return ((unsigned char) ((signed char) x << (signed char) y));\n" ^
  "}\n\n"

let shlu =
  (Runtime.Binary (Clight.Oshl, cint8u, cint8u), "_shlu", [], shlu_fun,
   [Runtime.Binary (Clight.Oshl, cint8s, cint8s)])

(* 8 bits shifts right *)

let shrs_fun s _ =
  "signed char " ^ s ^ " (signed char x, signed char y) {\n" ^
  "  signed char res = x, i;\n" ^
  "  for (i = 0 ; i < y ; i++) {\n" ^
  "    res = ((unsigned char) res) / 2;\n" ^
  "    res = res + ((signed char) 128);\n" ^
  "  }\n" ^
  "  return res;\n" ^
  "}\n\n"

let shrs =
  (Runtime.Binary (Clight.Oshr, cint8s, cint8s), "_shrs", [], shrs_fun, [])

let shru_fun = sh_fun "unsigned" "/"

let shru =
  (Runtime.Binary (Clight.Oshr, cint8u, cint8u), "_shru", [], shru_fun, [])


(* int32 *)

let struct_int32 name fields = match fields with
  | lolo :: lohi :: hilo :: hihi :: _ ->
    Clight.Tstruct
      (name,
       [(lolo, cint8u) ; (lohi, cint8u) ; (hilo, cint8u) ; (hihi, cint8u)])
  | _ -> error ("bad field names when defining type " ^ name ^ ".")

let struct_int32_name = "struct _int32_"

let new_int32 int32 =
  let lolo = "lolo" in
  let lohi = "lohi" in
  let hilo = "hilo" in
  let hihi = "hihi" in
  (int32, struct_int32_name, [lolo ; lohi ; hilo ; hihi], struct_int32)

let int32s = new_int32 (Clight.Tint (Clight.I32, AST.Signed))
let int32u = new_int32 (Clight.Tint (Clight.I32, AST.Unsigned))

(* 32 bits operations *)

(* 32 bits equality *)

let eq_int32s_fun s l =
  let (int32, lolo, lohi, hilo, hihi) = match l with
    | (int32, lolo :: lohi :: hilo :: hihi :: _) :: _ ->
      (int32, lolo, lohi, hilo, hihi)
    | _ -> error ("new type definition not coherent in function " ^ s ^ ".") in
  int32 ^ " " ^ s ^ " (" ^ int32 ^ " x, " ^ int32 ^ " y) {\n" ^
  "  " ^ int32 ^ " res;\n" ^
  "  res." ^ lolo ^ " = 1;\n" ^
  "  if (x." ^ lolo ^ " != y." ^ lolo ^ ") res." ^ lolo ^ " = 0;\n" ^
  "  if (x." ^ lohi ^ " != y." ^ lohi ^ ") res." ^ lolo ^ " = 0;\n" ^
  "  if (x." ^ hilo ^ " != y." ^ hilo ^ ") res." ^ lolo ^ " = 0;\n" ^
  "  if (x." ^ hihi ^ " != y." ^ hihi ^ ") res." ^ lolo ^ " = 0;\n" ^
  "  res." ^ lohi ^ " = 0;\n" ^
  "  res." ^ hilo ^ " = 0;\n" ^
  "  res." ^ hihi ^ " = 0;\n" ^
  "  return (res);\n" ^
  "}\n\n"

let eq32s =
  (Runtime.Binary (Clight.Oeq, cint32s, cint32s), "eq_int32s",
   [struct_int32_name], eq_int32s_fun, [])

(* 32 bits casts *)

let int32s_to_int8s_fun s l =
  let (int32, lolo, lohi, hilo, hihi) = match l with
    | (int32, lolo :: lohi :: hilo :: hihi :: _) :: _ ->
      (int32, lolo, lohi, hilo, hihi)
    | _ -> error ("new type definition not coherent in function " ^ s ^ ".") in
  "signed char " ^ s ^ " (" ^ int32 ^ " x) {\n" ^
  "  return ((signed char) x." ^ lolo ^ ");\n" ^
  "}\n\n"

let int32s_to_int8s =
  (Runtime.Cast (cint8s, cint32s), "int32s_to_int8s", [struct_int32_name],
   int32s_to_int8s_fun, [])


(* int16 *)

let struct_int16 name fields = match fields with
  | lo :: hi :: _ ->
    Clight.Tstruct (name, [(lo, cint8u) ; (hi, cint8u)])
  | _ -> error ("bad field names when defining type " ^ name ^ ".")

let struct_int16_name = "struct _int16_"

let new_int16 int16 =
  let lo = "lo" in
  let hi = "hi" in
  (int16, struct_int16_name, [lo ; hi], struct_int16)

let int16s = new_int16 (Clight.Tint (Clight.I16, AST.Signed))
let int16u = new_int16 (Clight.Tint (Clight.I16, AST.Unsigned))


(* int32 and int16 *)

let int32_and_int16_types = [int32s ; int32u ; int16s ; int16u]
let int32_and_int16_replacements = [eq32s ; int32s_to_int8s]


let unsupported = [divs ; mods ; shls ; shlu ; shrs ; shru]


let save_and_parse p =
  let tmp_file = Filename.temp_file "clight32toclight8" ".c" in
  try
    let oc = open_out tmp_file in
    output_string oc (ClightPrinter.print_program p) ;
    close_out oc ;
    let res = ClightParser.process tmp_file in
    Misc.SysExt.safe_remove tmp_file ;
    res
  with Sys_error _ -> failwith "Error reparsing Clight8 transformation."

let add_replacements p new_types replacements =
  let p = ClightCasts.simplify p in
  let (p, type_substitutions, replacement_assoc) =
    Runtime.add p new_types replacements in
  let p = ClightCasts.simplify p in
  let tmp_universe = ClightAnnotator.fresh_universe "_tmp" p in
  let fresh () = StringTools.Gen.fresh tmp_universe in
  let p = replace fresh type_substitutions replacement_assoc p in
  let p = replace_globals type_substitutions p in
  (* Printf.printf "%s\n%!" (ClightPrinter.print_program p) ; *)
  let p = save_and_parse p in
  ClightCasts.simplify p

let translate p =
  let p = main_returns_char p in
  let p = replace_constant p in
  let p =
    add_replacements p int32_and_int16_types int32_and_int16_replacements in
  add_replacements p [] unsupported

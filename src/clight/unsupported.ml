
(** This module adds runtime functions in a [Clight] program. These functions
    implement unsupported functions by the target architecture that introduce a
    branch. We need to define them at the [Clight] level in order to have a
    correct labelling. *)


let error_prefix = "Adding runtime functions"
let error = Error.global_error error_prefix


let add_program p s =
  let tmp_file = Filename.temp_file "add_runtime" ".c" in
  let cout = open_out tmp_file in
  let output = s ^ (ClightPrinter.print_program p) in
  output_string cout output ;
  flush cout ;
  close_out cout ;
  let p = ClightParser.process tmp_file in
  Misc.SysExt.safe_remove tmp_file ;
  p


module CtypeSet =
  Set.Make (struct type t = Clight.ctype let compare = Pervasives.compare end)


let deps op replacements =
  let f res (op', _, _, deps) = if op' = op then deps else res in
  List.fold_left f [] replacements


(* Filter used operations only *)

module OperationSet =
  Set.Make (struct type t = Runtime.operation
		   let compare = Pervasives.compare end)

let union_list l = List.fold_left OperationSet.union OperationSet.empty l

let f_ctype ctype _ = ctype

let f_expr e _ sub_expr_descrs_res =
  let res_e = match e with
    | Clight.Expr (Clight.Evar x, Clight.Tfunction _) ->
      OperationSet.singleton (Runtime.Fun x)
    | _ -> OperationSet.empty in
  union_list (res_e :: sub_expr_descrs_res)

let f_expr_descr ed _ sub_exprs_res =
  let res_ed = match ed with
    | Clight.Eunop (unop, Clight.Expr (_, t)) ->
      OperationSet.singleton (Runtime.Unary (unop, t))
    | Clight.Ebinop (binop, Clight.Expr (_, t1), Clight.Expr (_, t2)) ->
      OperationSet.singleton (Runtime.Binary (binop, t1, t2))
    | Clight.Ecast (t, Clight.Expr (_, t')) ->
      OperationSet.singleton (Runtime.Cast (t, t'))
    | _ -> OperationSet.empty in
  union_list (res_ed :: sub_exprs_res)

let f_stmt _ sub_exprs_res sub_stmts_res =
  OperationSet.union (union_list sub_exprs_res) (union_list sub_stmts_res)

let used_ops_stmt = ClightFold.statement f_ctype f_expr f_expr_descr f_stmt

let used_ops_fundef = function
  | Clight.Internal cfun -> used_ops_stmt cfun.Clight.fn_body
  | Clight.External _ -> OperationSet.empty

let used_ops p =
  let f ops (_, fundef) = OperationSet.union ops (used_ops_fundef fundef) in
  List.fold_left f OperationSet.empty p.Clight.prog_funct


let mem_replacements op =
  let f res (op', _, _, _) = res || (op' = op) in
  List.fold_left f false

let rec fix equal next x =
  let y = next x in
  if equal x y then x
  else fix equal next y

let needed_replacements used_ops replacements =
  let f op = mem_replacements op replacements in
  let reduced_used_ops = OperationSet.filter f used_ops in
  let next_ops ops =
    let add ops op = OperationSet.add op ops in
    let f op next_ops = List.fold_left add next_ops (deps op replacements) in
    OperationSet.fold f ops ops in
  let needed_ops = fix OperationSet.equal next_ops reduced_used_ops in
  let f (op, _, _, _) = OperationSet.mem op needed_ops in
  List.filter f replacements


let map_fresh_name unique map base_name =
  if StringTools.Map.mem base_name map then
    (map, StringTools.Map.find base_name map)
  else
    let fresh_name = unique base_name in
    (StringTools.Map.add base_name fresh_name map, fresh_name)

let fresh_replacements unique replacements =
  let f (map, l) (op, base_name, new_fun, deps) =
    let (map, fresh_name) = map_fresh_name unique map base_name in
    (map, l @ [(op, fresh_name, new_fun fresh_name, deps)]) in
  snd (List.fold_left f (StringTools.Map.empty, []) replacements)


let add_replacements replacements p =
  let f_replacements s (_, _, new_fun, _) = s ^ "\n" ^ new_fun in
  let added_string = List.fold_left f_replacements "" replacements in
  add_program p added_string


let make_replacement_assoc = List.map (fun (op, name, _, _) -> (op, name))


let add p replacements =
  let used_ops = used_ops p in
  let needed_replacements = needed_replacements used_ops replacements in
  let unique = StringTools.make_unique (ClightAnnotator.all_idents p) in
  let replacements = fresh_replacements unique needed_replacements in
  let p = add_replacements replacements p in
  (p, make_replacement_assoc replacements)


(* Replacement *)

let seq =
  List.fold_left
    (fun stmt1 stmt2 ->
      if stmt1 = Clight.Sskip then stmt2
      else
	if stmt2 = Clight.Sskip then stmt1
	else Clight.Ssequence (stmt1, stmt2))
    Clight.Sskip

let f_ctype ctype _ = ctype

let call fresh replaced replacement_assoc args ret_type =
  let tmp = fresh () in
  let replacement_fun_name = List.assoc replaced replacement_assoc in
  let tmpe = Clight.Expr (Clight.Evar tmp, ret_type) in
  let (args, args_types) = List.split args in
  let f_type = Clight.Tfunction (args_types, ret_type) in
  let f = Clight.Expr (Clight.Evar replacement_fun_name, f_type) in
  let call = Clight.Scall (Some tmpe, f, args) in
  (tmpe, call, [(tmp, ret_type)])

let replace_ident replacement_assoc x t =
  let new_name = match t with
    | Clight.Tfunction _
	when List.mem_assoc (Runtime.Fun x) replacement_assoc ->
      let replacement_fun_name = List.assoc (Runtime.Fun x) replacement_assoc in
      replacement_fun_name
    | _ -> x in
  (Clight.Expr (Clight.Evar new_name, t), Clight.Sskip, [])

let f_expr fresh replacement_assoc e _ _ =

  let f (Clight.Expr (ed, t) as e) sub_exprs_res =
    let f_sub_exprs (es, stmt, tmps) (e, stmt', tmps') =
      (es @ [e], seq [stmt ; stmt'], tmps @ tmps') in
    let (sub_exprs, stmt1, tmps1) =
      List.fold_left f_sub_exprs ([], Clight.Sskip, []) sub_exprs_res in
    let (e, stmt2, tmps2) = match ed, sub_exprs with
      | Clight.Evar x, _ -> replace_ident replacement_assoc x t
      | Clight.Eunop (unop, Clight.Expr (_, t')), e' :: _
	  when List.mem_assoc (Runtime.Unary (unop, t')) replacement_assoc ->
	call fresh (Runtime.Unary (unop, t')) replacement_assoc [(e', t')] t
      | Clight.Ebinop (binop, Clight.Expr (_, t1), Clight.Expr (_, t2)),
	e1 :: e2 :: _
	  when List.mem_assoc
	    (Runtime.Binary (binop, t1, t2)) replacement_assoc ->
	call fresh (Runtime.Binary (binop, t1, t2)) replacement_assoc
	  [(e1, t1) ; (e2, t2)] t
      | Clight.Ecast (t, Clight.Expr (_, t')), e' :: _
	  when List.mem_assoc (Runtime.Cast (t, t')) replacement_assoc ->
	call fresh (Runtime.Cast (t, t')) replacement_assoc [(e', t')] t
      | _ -> (e, Clight.Sskip, []) in
    (ClightFold.expr_fill_exprs e sub_exprs,
     seq [stmt1 ; stmt2],
     tmps1 @ tmps2) in

  ClightFold.expr2 f e

let f_expr_descr ed _ _ _ = ed

let f_stmt stmt sub_exprs_res sub_stmts_res =
  let f_sub_exprs (es, stmt, tmps) (e, stmt', tmps') =
    (es @ [e], seq [stmt ; stmt'], tmps @ tmps') in
  let (sub_exprs, added_stmt, tmps1) =
    List.fold_left f_sub_exprs ([], Clight.Sskip, []) sub_exprs_res in
  let f_sub_stmts (stmts, tmps) (stmt, tmps') =
    (stmts @ [stmt], tmps @ tmps') in
  let (sub_stmts, tmps2) = List.fold_left f_sub_stmts ([], []) sub_stmts_res in
  let stmt' = ClightFold.statement_fill_subs stmt sub_exprs sub_stmts in
  (seq [added_stmt ; stmt'], tmps1 @ tmps2)

let replace_statement fresh replacement_assoc =
  ClightFold.statement f_ctype (f_expr fresh replacement_assoc)
    f_expr_descr f_stmt

let replace_internal fresh replacement_assoc def =
  let (new_body, tmps) =
    replace_statement fresh replacement_assoc def.Clight.fn_body in
  { def with
    Clight.fn_vars = def.Clight.fn_vars @ tmps ; Clight.fn_body = new_body }

let replace_funct fresh replacement_assoc (id, fundef) =
  let fundef' = match fundef with
    | Clight.Internal def ->
      Clight.Internal (replace_internal fresh replacement_assoc def)
    | _ -> fundef in
  (id, fundef')

let replace fresh replacement_assoc p =
  let prog_funct =
    List.map (replace_funct fresh replacement_assoc) p.Clight.prog_funct in
  { p with Clight.prog_funct = prog_funct }


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

let add_replacements p replacements =
  let p = ClightCasts.simplify p in
  let (p, replacement_assoc) = add p replacements in
  let p = ClightCasts.simplify p in
  let tmp_universe = ClightAnnotator.fresh_universe "_tmp" p in
  let fresh () = StringTools.Gen.fresh tmp_universe in
  let p = replace fresh replacement_assoc p in
  let p = save_and_parse p in
  ClightCasts.simplify p


let replace p = add_replacements p Driver.TargetArch.unsupported

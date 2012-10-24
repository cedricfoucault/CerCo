

let f_expr set locals e sub_exprs_res =
  let e_res = match e with
    | Clight.Expr (Clight.Evar id, _) ->
      StringTools.Set.mem id set && not (List.mem id locals)
    | _ -> false in
  List.fold_left (||) false (e_res :: sub_exprs_res)

let f_stmt _ sub_exprs_res sub_stmts_res =
  List.fold_left (||) false (sub_exprs_res @ sub_stmts_res)

let references_stmt set locals stmt =
  ClightFold.statement2 (f_expr set locals) f_stmt stmt

let references_funct set (id, def) = match def with
  | Clight.Internal def ->
    let locals = List.map fst (def.Clight.fn_params @ def.Clight.fn_vars) in
    if references_stmt set locals def.Clight.fn_body then
      StringTools.Set.add id set
    else set
  | _ -> set

let references set p =
  List.fold_left references_funct set p.Clight.prog_funct


let unsupported_functions p =
  let rec fixpoint set =
    let set' = references set p in
    if StringTools.Set.equal set set' then set
    else fixpoint set' in
  fixpoint (StringTools.Set.of_list ["calloc" ; "memcpy"])


let remove_functions functions p =
  let f (id, _) = not (StringTools.Set.mem id functions) in
  let functs = List.filter f p.Clight.prog_funct in
  { p with Clight.prog_funct = functs }


let simplify p =
  let unsupported = unsupported_functions p in
  remove_functions unsupported p

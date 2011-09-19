(* This module provides an annotation function for Clight programs
   when given the cost associated to each cost labels of the
   program. *)


let error_prefix = "Clight Annotator"
let error = Error.global_error error_prefix


let cost_id_prefix = "_cost"
let cost_incr_prefix = "_cost_incr"


(* Program var and fun names, cost labels and labels *)

let string_set_of_list l =
  List.fold_left (fun res s -> StringTools.Set.add s res)
    StringTools.Set.empty l

let triple_union
    (names1, cost_labels1, user_labels1)
    (names2, cost_labels2, user_labels2) =
  (StringTools.Set.union names1 names2,
   CostLabel.Set.union cost_labels1 cost_labels2,
   Label.Set.union user_labels1 user_labels2)

let empty_triple = (StringTools.Set.empty, CostLabel.Set.empty, Label.Set.empty)

let name_singleton id =
  (StringTools.Set.singleton id, CostLabel.Set.empty, Label.Set.empty)

let cost_label_singleton cost_lbl =
  (StringTools.Set.empty, CostLabel.Set.singleton cost_lbl, Label.Set.empty)

let label_singleton lbl =
  (StringTools.Set.empty, CostLabel.Set.empty, Label.Set.singleton lbl)

let list_union l = List.fold_left triple_union empty_triple l

let f_ctype ctype sub_ctypes_res =
  let res = match ctype with
    | Clight.Tstruct (id, fields) | Clight.Tunion (id, fields) ->
      (string_set_of_list (id :: (List.map fst fields)),
       CostLabel.Set.empty, Label.Set.empty)
    | Clight.Tcomp_ptr id -> name_singleton id
    | _ -> empty_triple in
  list_union (res :: sub_ctypes_res)

let f_expr _ sub_ctypes_res sub_expr_descrs_res =
  list_union (sub_ctypes_res @ sub_expr_descrs_res)

let f_expr_descr ed sub_ctypes_res sub_exprs_res =
  let res = match ed with
    | Clight.Evar id | Clight.Efield (_, id) | Clight.Ecall (id, _, _) ->
      name_singleton id
    | Clight.Ecost (cost_lbl, _) -> cost_label_singleton cost_lbl
    | _ -> empty_triple in
  list_union (res :: (sub_ctypes_res @ sub_exprs_res))

let f_stmt stmt sub_exprs_res sub_stmts_res =
  let stmt_res = match stmt with
    | Clight.Slabel (lbl, _) | Clight.Sgoto lbl -> label_singleton lbl
    | Clight.Scost (cost_lbl, _) -> cost_label_singleton cost_lbl
    | _ -> empty_triple in
  list_union (stmt_res :: (sub_exprs_res @ sub_stmts_res))

let body_idents = ClightFold.statement f_ctype f_expr f_expr_descr f_stmt

let prog_idents p =
  let def_idents = function
    | Clight.Internal def ->
	let vars =
	  string_set_of_list
	    (List.map fst (def.Clight.fn_params @ def.Clight.fn_vars)) in
	let (names, cost_labels, user_labels) =
	  body_idents def.Clight.fn_body in
	(StringTools.Set.union vars names, cost_labels, user_labels)
    | Clight.External (id, _, _) ->
	(StringTools.Set.singleton id, CostLabel.Set.empty, Label.Set.empty) in
  let fun_idents (id, f_def) =
    let (names, cost_labels, user_labels) = def_idents f_def in
    (StringTools.Set.add id names, cost_labels, user_labels) in
  let f idents def = triple_union idents (fun_idents def) in
  List.fold_left f empty_triple p.Clight.prog_funct

let names p =
  let (names, _, _) = prog_idents p in names
let cost_labels p =
  let (_, cost_labels, _) = prog_idents p in cost_labels
let user_labels p =
  let (_, _, user_labels) = prog_idents p in user_labels

let all_labels p =
  let (_, cost_labels, user_labels) = prog_idents p in
  let all =
    CostLabel.Set.fold (fun lbl lbls -> StringTools.Set.add lbl lbls)
      cost_labels StringTools.Set.empty in
  Label.Set.fold (fun lbl lbls -> StringTools.Set.add lbl lbls) user_labels all

let all_idents p =
  let (names, cost_labels, user_labels) = prog_idents p in
  let to_string_set fold set =
    fold (fun lbl idents -> StringTools.Set.add lbl idents) set
      StringTools.Set.empty in
  let cost_labels = to_string_set CostLabel.Set.fold cost_labels in
  let user_labels = to_string_set Label.Set.fold user_labels in
  StringTools.Set.union names (StringTools.Set.union cost_labels user_labels)

let fresh_ident base p =
  StringTools.Gen.fresh_prefix (all_idents p) base

let fresh_universe base p =
  let universe = fresh_ident base p in
  StringTools.Gen.new_universe universe

let make_fresh base p =
  let universe = fresh_universe base p in
  (fun () -> StringTools.Gen.fresh universe)


(* Instrumentation *)

let int_typ = Clight.Tint (Clight.I32, AST.Signed)

let const_int i = Clight.Expr (Clight.Econst_int i, int_typ)

(* Instrument an expression. *)

let rec instrument_expr cost_mapping cost_incr e =
  let Clight.Expr (e, t) = e in
  match e with
    | Clight.Ecost (lbl, e)
	when CostLabel.Map.mem lbl cost_mapping &&
	     CostLabel.Map.find lbl cost_mapping = 0 ->
	e
    | _ ->
	let e' = instrument_expr_descr cost_mapping cost_incr e in
	Clight.Expr (e', t)
and instrument_expr_descr cost_mapping cost_incr e = match e with
  | Clight.Econst_int _ | Clight.Econst_float _ | Clight.Evar _
  | Clight.Esizeof _ -> e
  | Clight.Ederef e ->
      let e' = instrument_expr cost_mapping cost_incr e in
      Clight.Ederef e'
  | Clight.Eaddrof e ->
      let e' = instrument_expr cost_mapping cost_incr e in
      Clight.Eaddrof e'
  | Clight.Eunop (op, e) ->
      let e' = instrument_expr cost_mapping cost_incr e in
      Clight.Eunop (op, e')
  | Clight.Ebinop (op, e1, e2) ->
      let e1' = instrument_expr cost_mapping cost_incr e1 in
      let e2' = instrument_expr cost_mapping cost_incr e2 in
      Clight.Ebinop (op, e1', e2')
  | Clight.Ecast (t, e) ->
      let e' = instrument_expr cost_mapping cost_incr e in
      Clight.Ecast (t, e')
  | Clight.Econdition (e1, e2, e3) ->
      let e1' = instrument_expr cost_mapping cost_incr e1 in
      let e2' = instrument_expr cost_mapping cost_incr e2 in
      let e3' = instrument_expr cost_mapping cost_incr e3 in
      Clight.Econdition (e1', e2', e3')
  | Clight.Eandbool (e1, e2) ->
      let e1' = instrument_expr cost_mapping cost_incr e1 in
      let e2' = instrument_expr cost_mapping cost_incr e2 in
      Clight.Eandbool (e1', e2')
  | Clight.Eorbool (e1, e2) ->
      let e1' = instrument_expr cost_mapping cost_incr e1 in
      let e2' = instrument_expr cost_mapping cost_incr e2 in
      Clight.Eorbool (e1', e2')
  | Clight.Efield (e, x) ->
      let e' = instrument_expr cost_mapping cost_incr e in
      Clight.Efield (e', x)
  | Clight.Ecost (lbl, e) when CostLabel.Map.mem lbl cost_mapping ->
      let e' = instrument_expr cost_mapping cost_incr e in
      let incr = CostLabel.Map.find lbl cost_mapping in
      if incr = 0 then let Clight.Expr (e'', _) = e' in e''
      else Clight.Ecall (cost_incr, const_int incr, e')
  | Clight.Ecost (_, e) ->
    let Clight.Expr (e', _) = instrument_expr cost_mapping cost_incr e in
    e'
  | Clight.Ecall (x, e1, e2) -> assert false (* Should not happen. *)

(* Instrument a statement. *)

let rec instrument_body cost_mapping cost_incr stmt = match stmt with
  | Clight.Sskip | Clight.Sbreak | Clight.Scontinue | Clight.Sreturn None
  | Clight.Sgoto _ ->
    stmt
  | Clight.Sassign (e1, e2) ->
    let e1' = instrument_expr cost_mapping cost_incr e1 in
    let e2' = instrument_expr cost_mapping cost_incr e2 in
    Clight.Sassign (e1', e2')
  | Clight.Scall (eopt, f, args) ->
    let eopt' = match eopt with
      | None -> None
      | Some e -> Some (instrument_expr cost_mapping cost_incr e) in
    let f' = instrument_expr cost_mapping cost_incr f in
    let args' = List.map (instrument_expr cost_mapping cost_incr) args in
    Clight.Scall (eopt', f', args')
  | Clight.Ssequence (s1, s2) ->
    Clight.Ssequence (instrument_body cost_mapping cost_incr s1,
		      instrument_body cost_mapping cost_incr s2)
  | Clight.Sifthenelse (e, s1, s2) ->
    let e' = instrument_expr cost_mapping cost_incr e in
    let s1' = instrument_body cost_mapping cost_incr s1 in
    let s2' = instrument_body cost_mapping cost_incr s2 in
    Clight.Sifthenelse (e', s1', s2')
  | Clight.Swhile (e, s) ->
    let e' = instrument_expr cost_mapping cost_incr e in
    let s' = instrument_body cost_mapping cost_incr s in
    Clight.Swhile (e', s')
  | Clight.Sdowhile (e, s) ->
    let e' = instrument_expr cost_mapping cost_incr e in
    let s' = instrument_body cost_mapping cost_incr s in
    Clight.Sdowhile (e', s')
  | Clight.Sfor (s1, e, s2, s3) ->
    let s1' = instrument_body cost_mapping cost_incr s1 in
    let e' = instrument_expr cost_mapping cost_incr e in
    let s2' = instrument_body cost_mapping cost_incr s2 in
    let s3' = instrument_body cost_mapping cost_incr s3 in
    Clight.Sfor (s1', e', s2', s3')
  | Clight.Sreturn (Some e) ->
    let e' = instrument_expr cost_mapping cost_incr e in
    Clight.Sreturn (Some e')
  | Clight.Sswitch (e, ls) ->
    let e' = instrument_expr cost_mapping cost_incr e in
    let ls' = instrument_ls cost_mapping cost_incr ls in
    Clight.Sswitch (e', ls')
  | Clight.Slabel (lbl, s) ->
    let s' = instrument_body cost_mapping cost_incr s in
    Clight.Slabel (lbl, s')
  | Clight.Scost (lbl, s) when CostLabel.Map.mem lbl cost_mapping ->
    (* Keep the cost label in the code. *)
    let s' = instrument_body cost_mapping cost_incr s in
    let incr = CostLabel.Map.find lbl cost_mapping in
    let fun_typ = Clight.Tfunction ([int_typ], Clight.Tvoid) in
    let f = Clight.Expr (Clight.Evar cost_incr, fun_typ) in
    let args = [Clight.Expr (Clight.Econst_int incr, int_typ)] in
    Clight.Scost (lbl, Clight.Ssequence (Clight.Scall (None, f, args), s'))
  (*
    let s' = instrument_body cost_mapping cost_incr s in
    let incr = CostLabel.Map.find lbl cost_mapping in
    if incr = 0 then s'
    else
    let fun_typ = Clight.Tfunction ([int_typ], Clight.Tvoid) in
    let f = Clight.Expr (Clight.Evar cost_incr, fun_typ) in
    let args = [Clight.Expr (Clight.Econst_int incr, int_typ)] in
    Clight.Ssequence (Clight.Scall (None, f, args), s')
  *)
  | Clight.Scost (lbl, s) ->
    (* Keep the cost label in the code and show the increment of 0. *)
    let s' = instrument_body cost_mapping cost_incr s in
    let fun_typ = Clight.Tfunction ([int_typ], Clight.Tvoid) in
    let f = Clight.Expr (Clight.Evar cost_incr, fun_typ) in
    let args = [Clight.Expr (Clight.Econst_int 0, int_typ)] in
    Clight.Scost (lbl, Clight.Ssequence (Clight.Scall (None, f, args), s'))
  (*
    instrument_body cost_mapping cost_incr s
  *)
and instrument_ls cost_mapping cost_incr = function
  | Clight.LSdefault s ->
    let s' = instrument_body cost_mapping cost_incr s in
    Clight.LSdefault s'
  | Clight.LScase (i, s, ls) ->
    let s' = instrument_body cost_mapping cost_incr s in
    let ls' = instrument_ls cost_mapping cost_incr ls in
    Clight.LScase (i, s', ls')

(* Instrument a function. *)

let instrument_funct cost_mapping cost_incr (id, def) =
  let def = match def with
    | Clight.Internal def ->
	let body = instrument_body cost_mapping cost_incr def.Clight.fn_body in
	Clight.Internal { def with Clight.fn_body = body }
    | Clight.External _ -> def
  in
  (id, def)

(* This is the declaration of the cost variable. *)

let cost_decl cost_id =
  let init = [Clight.Init_int32 0] in
  ((cost_id, init), int_typ)

(* This is the definition of the increment cost function. *)

let cost_incr_def cost_id cost_incr =
  let int_var x = Clight.Expr (Clight.Evar x, int_typ) in
  let param = "incr" in
  let cost = int_var cost_id in
  let increment = int_var param in
  let cost_increment =
    Clight.Expr (Clight.Ebinop (Clight.Oadd, cost, increment), int_typ) in
  let stmt = Clight.Sassign (cost, cost_increment) in
  let cfun =
    { Clight.fn_return = Clight.Tvoid ;
      Clight.fn_params = [(param, int_typ)] ;
      Clight.fn_vars   = [] ;
      Clight.fn_body =   stmt } in
  (cost_incr, Clight.Internal cfun)

(* Create a fresh uninitialized cost variable for each external function. This
   will be used by the Cost plug-in of the Frama-C platform. *)

let extern_cost_variables make_unique functs =
  let prefix = "_cost_of_" in
  let f (decls, map) (_, def) = match def with
    | Clight.Internal _ -> (decls, map)
    | Clight.External (id, _, _) ->
      let new_var = make_unique (prefix ^ id) in
      (decls @ [cost_decl new_var], StringTools.Map.add id new_var map) in
  List.fold_left f ([], StringTools.Map.empty) functs

let save_tmp tmp_file s =
  let cout = open_out tmp_file in
  output_string cout s ;
  flush cout ;
  close_out cout

(** [instrument prog cost_map] instruments the program [prog]. First a fresh
    global variable --- the so-called cost variable --- is added to the program.
    Then, each cost label in the program is replaced by an increment of the cost
    variable, following the mapping [cost_map]. The function also returns the
    name of the cost variable and the name of the cost increment function. *)

let instrument p cost_mapping =

  (* Create a fresh 'cost' variable. *)
  let names = names p in
  let make_unique = StringTools.make_unique names in
  let cost_id = make_unique cost_id_prefix in
  let cost_decl = cost_decl cost_id in

  (* Create a fresh uninitialized global variable for each extern function. *)
  let (extern_cost_decls, extern_cost_variables) =
    extern_cost_variables make_unique p.Clight.prog_funct in

  (* Define an increment function for the cost variable. *)
  let cost_incr =
    StringTools.Gen.fresh_prefix (StringTools.Set.add cost_id names)
      cost_incr_prefix in
  let cost_incr_def = cost_incr_def cost_id cost_incr in

  (* Instrument each function *)
  let prog_funct =
    List.map (instrument_funct cost_mapping cost_incr) p.Clight.prog_funct in

  (* Glue all this together. *)
  let prog_vars = cost_decl :: extern_cost_decls @ p.Clight.prog_vars in
  let prog_funct = cost_incr_def :: prog_funct in
  let p' =
    { p with Clight.prog_vars = prog_vars ; Clight.prog_funct = prog_funct } in

  (* Save the resulted program. Then re-parse it.
     Indeed, the instrumentation may add side-effects in expressions, which is
     not Clight compliant. Re-parsing the result with CIL will remove these
     side-effects in expressions to obtain a Clight program. *)
  let tmp_file = Filename.temp_file "clight_instrument" ".c" in
  save_tmp tmp_file (ClightPrinter.print_program p') ;
  let res = ClightParser.process tmp_file in
  Misc.SysExt.safe_remove tmp_file ;
  (res, cost_id, cost_incr, extern_cost_variables)

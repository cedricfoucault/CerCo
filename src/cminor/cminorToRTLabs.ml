
(** This module translates a [Cminor] program into a [RTLabs] program. *)

open Driver


let error_prefix = "Cminor to RTLabs"
let error = Error.global_error error_prefix
let error_float () = error "float not supported."


(* Helper functions *)

let allocate (rtlabs_fun : RTLabs.internal_function) (sig_type : AST.sig_type)
    : RTLabs.internal_function * Register.t =
  let r = Register.fresh rtlabs_fun.RTLabs.f_runiverse in
  let locals = rtlabs_fun.RTLabs.f_locals @ [(r, sig_type)] in
  let rtlabs_fun =
    { rtlabs_fun with RTLabs.f_locals = locals } in
  (rtlabs_fun, r)

let type_of (Cminor.Expr (_, t)) = t

let allocate_expr
    (rtlabs_fun : RTLabs.internal_function)
    (e          : Cminor.expression)
    : (RTLabs.internal_function * Register.t) =
  allocate rtlabs_fun (type_of e)

type local_env = Register.t StringTools.Map.t

let find_local (lenv : local_env) (x : AST.ident) : Register.t =
  if StringTools.Map.mem x lenv then StringTools.Map.find x lenv
  else error ("Unknown local \"" ^ x ^ "\".")

let find_olocal (lenv : local_env) (ox : AST.ident option) : Register.t option =
  match ox with
    | None -> None
    | Some x -> Some (find_local lenv x)

let choose_destination
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (e          : Cminor.expression)
    : RTLabs.internal_function * Register.t =
  assert false (* TODO M1 *)

let choose_destinations
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (args       : Cminor.expression list)
    : RTLabs.internal_function * Register.t list =
  let f (rtlabs_fun, regs) e =
    let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
    (rtlabs_fun, regs @ [r]) in
  List.fold_left f (rtlabs_fun, []) args

let fresh_label (rtlabs_fun : RTLabs.internal_function) : Label.t =
  Label.Gen.fresh rtlabs_fun.RTLabs.f_luniverse

let change_entry
    (rtlabs_fun : RTLabs.internal_function)
    (new_entry : Label.t)
    : RTLabs.internal_function =
  { rtlabs_fun with RTLabs.f_entry = new_entry }


(* Add a label and its associated instruction at the beginning of a function's
   graph *)
let add_graph
    (rtlabs_fun : RTLabs.internal_function)
    (lbl        : Label.t)
    (stmt       : RTLabs.statement)
    : RTLabs.internal_function =
  assert false (* TODO M1 *)


let generate
    (rtlabs_fun : RTLabs.internal_function)
    (stmt       : RTLabs.statement)
    : RTLabs.internal_function =
  assert false (* TODO M1 *)


(*
(* [addressing e] returns the type of address represented by [e],
   along with its arguments *)

let addressing (Cminor.Expr (ed, t) : Cminor.expression)
    : (RTLabs.addressing * Cminor.expression list)  =
  match ed with
    | Cminor.Cst (AST.Cst_addrsymbol id) -> (RTLabs.Aglobal (id, 0), [])
    | Cminor.Cst (AST.Cst_stackoffset n) -> (RTLabs.Ainstack n, [])
    | Cminor.Op2 (AST.Op_addp _,
		  Cminor.Cst (AST.Cst_addrsymbol id),
		  Cminor.Cst (AST.Cst_int n)) ->
      (RTLabs.Aglobal (id, n), [])
    | Cminor.Op2 (AST.Op_addp _, e1, Cminor.Cst (AST.Cst_int n)) ->
      (RTLabs.Aindexed n, [e1])
    | Cminor.Op2 (AST.Op_addp _,
		  Cminor.Cst (AST.Cst_addrsymbol id),
		  e2) ->
      (RTLabs.Abased (id, 0), [e2])
    | Cminor.Op2 (AST.Op_addp _, e1, e2) -> (RTLabs.Aindexed2, [e1 ; e2])
    | _ -> (RTLabs.Aindexed 0, [e])
*)


(* Translating conditions *)

let rec translate_branch
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (e          : Cminor.expression)
    (lbl_true   : Label.t)
    (lbl_false  : Label.t)
    : RTLabs.internal_function =
  let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
  let stmt = RTLabs.St_cond (r, lbl_true, lbl_false) in
  let rtlabs_fun = generate rtlabs_fun stmt in
  translate_expr rtlabs_fun lenv r e

(*
  let Cminor.Expr (ed, t) = e in
  match ed with

    | Cminor.Id x ->
      let stmt =
	RTLabs.St_cond1 (AST.Op_id, find_local lenv x, lbl_true, lbl_false) in
      generate rtlabs_fun stmt

    | Cminor.Cst cst ->
      generate rtlabs_fun (RTLabs.St_condcst (cst, t, lbl_true, lbl_false))

    | Cminor.Op1 (op1, e) ->
      let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
      let stmt = RTLabs.St_cond1 (op1, r, lbl_true, lbl_false) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_expr rtlabs_fun lenv r e

    | Cminor.Op2 (op2, e1, e2) ->
      let (rtlabs_fun, r1) = choose_destination rtlabs_fun lenv e1 in
      let (rtlabs_fun, r2) = choose_destination rtlabs_fun lenv e2 in
      let stmt = RTLabs.St_cond2 (op2, r1, r2, lbl_true, lbl_false) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv [r1 ; r2] [e1 ; e2]

    | _ ->
      let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
      let stmt = RTLabs.St_cond1 (AST.Op_id, r, lbl_true, lbl_false) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_expr rtlabs_fun lenv r e
*)

(* Translating expressions *)

and translate_expr
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (destr      : Register.t)
    (e          : Cminor.expression)
    : RTLabs.internal_function =
  let Cminor.Expr (ed, t) = e in
  match ed with

    | Cminor.Id x ->
      assert false (* TODO M1 *)

    | Cminor.Cst cst ->
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_cst (destr, cst, old_entry) in
      generate rtlabs_fun stmt

    | Cminor.Op1 (op1, e) ->
      let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_op1 (op1, destr, r, old_entry) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_expr rtlabs_fun lenv r e

    | Cminor.Op2 (op2, e1, e2) ->
      let (rtlabs_fun, r1) = choose_destination rtlabs_fun lenv e1 in
      let (rtlabs_fun, r2) = choose_destination rtlabs_fun lenv e2 in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_op2 (op2, destr, r1, r2, old_entry) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv [r1 ; r2] [e1 ; e2]

    | Cminor.Mem (chunk, e) ->
      let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_load (chunk, r, destr, old_entry) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_expr rtlabs_fun lenv r e

    | Cminor.Cond (e1, e2, e3) ->
      assert false (* TODO M1 *)

    | Cminor.Exp_cost (lbl, e) ->
      let rtlabs_fun = translate_expr rtlabs_fun lenv destr e in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      generate rtlabs_fun (RTLabs.St_cost (lbl, old_entry))

and translate_exprs
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (regs       : Register.t list)
    (args       : Cminor.expression list)
    : RTLabs.internal_function =
  let f destr e rtlabs_fun = translate_expr rtlabs_fun lenv destr e in
  List.fold_right2 f regs args rtlabs_fun


(*
(* Switch transformation

   switch (e) {
   case c0: exit i0;
   case c1: exit i1;
   ...
   default: exit idfl; }

   is translated to

   if (e == c0) exit i0;
   if (e == c1) exit i1;
   ...
   exit idfl; *)

let transform_switch
    (e : Cminor.expression)
    (cases : (int * int) list)
    (dfl : int)
    : Cminor.statement =
  let rec aux = function
    | [] -> Cminor.St_skip
    | (case, exit) :: cases ->
      let c =
	Cminor.Op2 (AST.Op_cmp (AST.Cmp_eq, uint),
		    e, Cminor.Cst (AST.Cst_int case)) in
      let stmt =
	Cminor.St_ifthenelse (c, Cminor.St_exit exit, Cminor.St_skip) in
      Cminor.St_seq (stmt, aux cases)
  in
  Cminor.St_seq (aux cases, Cminor.St_exit dfl)
*)


(* Translating statements *)

let rec translate_stmt
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (exits      : Label.t list)
    (stmt       : Cminor.statement)
    : RTLabs.internal_function =
  match stmt with

    | Cminor.St_skip -> rtlabs_fun

    | Cminor.St_assign (x, e) ->
      assert false (* TODO M1 *)

    | Cminor.St_store (chunk, e1, e2) ->
      let (rtlabs_fun, addr) = choose_destination rtlabs_fun lenv e1 in
      let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e2 in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_store (chunk, addr, r, old_entry) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv [addr ; r] [e1 ; e2]

    | Cminor.St_call (oret,
		      Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol f), _),
		      args, sg) ->
      let (rtlabs_fun, regs) = choose_destinations rtlabs_fun lenv args in
      let oretr = find_olocal lenv oret in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_call_id (f, regs, oretr, sg, old_entry) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv regs args

    | Cminor.St_call (oret, f, args, sg) ->
      let (rtlabs_fun, fr) = choose_destination rtlabs_fun lenv f in
      let (rtlabs_fun, regs) = choose_destinations rtlabs_fun lenv args in
      let oretr = find_olocal lenv oret in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let stmt = RTLabs.St_call_ptr (fr, regs, oretr, sg, old_entry) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv (fr :: regs) (f :: args)

    | Cminor.St_tailcall (Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol f), _),
			  args, sg) ->
      let (rtlabs_fun, regs) = choose_destinations rtlabs_fun lenv args in
      let stmt = RTLabs.St_tailcall_id (f, regs, sg) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv regs args

    | Cminor.St_tailcall (f, args, sg) ->
      let (rtlabs_fun, fr) = choose_destination rtlabs_fun lenv f in
      let (rtlabs_fun, regs) = choose_destinations rtlabs_fun lenv args in
      let stmt = RTLabs.St_tailcall_ptr (fr, regs, sg) in
      let rtlabs_fun = generate rtlabs_fun stmt in
      translate_exprs rtlabs_fun lenv (fr :: regs) (f :: args)

    | Cminor.St_seq (s1, s2) ->
      assert false (* TODO M1 *)

    | Cminor.St_ifthenelse (e, s1, s2) ->
      assert false (* TODO M1 *)

    | Cminor.St_loop s ->
      assert false (* TODO M1 *)

    | Cminor.St_block s ->
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      translate_stmt rtlabs_fun lenv (old_entry :: exits) s

    | Cminor.St_exit n ->
      change_entry rtlabs_fun (List.nth exits n)

    | Cminor.St_return eopt ->
      let rtlabs_fun = change_entry rtlabs_fun rtlabs_fun.RTLabs.f_exit in
      (match eopt, rtlabs_fun.RTLabs.f_result with
	| None, None -> rtlabs_fun
	| Some e, Some (retr, _) -> translate_expr rtlabs_fun lenv retr e
	| _ -> assert false (* should be impossible *))

    | Cminor.St_switch (e, cases, dfl) ->
      assert false (* should have been simplified before *)
(*
      let stmt = transform_switch e cases dfl in
      translate_stmt rtlabs_fun lenv exits stmt
*)

    | Cminor.St_label (lbl, s) ->
      let rtlabs_fun = translate_stmt rtlabs_fun lenv exits s in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      add_graph rtlabs_fun lbl (RTLabs.St_skip old_entry)

    | Cminor.St_cost (lbl, s) ->
      let rtlabs_fun = translate_stmt rtlabs_fun lenv exits s in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      generate rtlabs_fun (RTLabs.St_cost (lbl, old_entry))

    | Cminor.St_goto lbl ->
      change_entry rtlabs_fun lbl


(* Translating function definitions *)

(* The translation consists in the following:
   - Create a universe of pseudo-register names
   - Create a universe of label names
   - Create a local environment; that is, a mapping from local
     variables to pseudo-registers
   - Extract the registers representing the formal variables
   - Extract the registers representing the local variables
   - Allocate a fresh register to hold the result of the function
   - Allocate a fresh label representing the exit point
   - Initialize the graph with a return instruction at the end
   - Complete the graph according to the function's body.
     Instructions will be added from end to start following the flow of the
     function. *)

let translate_internal lbl_prefix f_def =

  (* Register names *)
  let runiverse = Register.new_universe "%" in

  (* Labels of statements *)
  let luniverse = Label.Gen.new_universe lbl_prefix in

  (* Local environment *)
  let add_local lenv (x, _) =
    StringTools.Map.add x (Register.fresh runiverse) lenv in
  let lenv = StringTools.Map.empty in
  let lenv = List.fold_left add_local lenv f_def.Cminor.f_params in
  let lenv = List.fold_left add_local lenv f_def.Cminor.f_vars in

  let extract vars =
    let f l (x, t) = l @ [(find_local lenv x, t)] in
    List.fold_left f [] vars in

  (* Parameter registers *)
  let params = extract f_def.Cminor.f_params in
  
  (* Local registers *)
  let locals =  extract f_def.Cminor.f_vars in

  (* [result] is the result of the body, if any. *)
  let result = match f_def.Cminor.f_return with
    | AST.Type_void -> None
    | AST.Type_ret t -> Some (Register.fresh runiverse, t) in

  let locals =
    locals @ (match result with None -> [] | Some (r, t) -> [(r, t)]) in

  (* Exit label of the graph *)
  let exit = Label.Gen.fresh luniverse in

  (* The control flow graph: for now, it is only a return instruction at the
     end. *)
  let return = match result with
    | None -> None
    | Some (retr, _) -> Some retr in
  let graph = Label.Map.add exit (RTLabs.St_return return) Label.Map.empty in

  let rtlabs_fun =
    { RTLabs.f_luniverse = luniverse ;
      RTLabs.f_runiverse = runiverse ;
      RTLabs.f_result    = result ;
      RTLabs.f_params    = params ;
      RTLabs.f_locals    = locals ;
      RTLabs.f_stacksize = f_def.Cminor.f_stacksize ;
      RTLabs.f_graph     = graph ;
      RTLabs.f_entry     = exit ;
      RTLabs.f_exit      = exit } in

  (* Complete the graph *)
  translate_stmt rtlabs_fun lenv [] f_def.Cminor.f_body


let translate_functions lbls (f_id, f_def) = match f_def with
  | Cminor.F_int int_def ->
    let lbl_prefix = StringTools.Gen.fresh_prefix lbls f_id in
    let def = translate_internal lbl_prefix int_def in
    (f_id, RTLabs.F_int def)
  | Cminor.F_ext def -> (f_id, RTLabs.F_ext def)


(* Initialization of globals *)

let sum_offsets =
  let f res off =
    let cst_off =
      Cminor.Expr (Cminor.Cst (AST.Cst_offset off), AST.Sig_offset) in
    Cminor.Expr (Cminor.Op2 (AST.Op_add, res, cst_off), AST.Sig_offset) in
  List.fold_left f (Cminor.Expr (Cminor.Cst (AST.Cst_int 0), AST.Sig_offset))

let quantity_sig_of_data data =
  let i = match data with
    | AST.Data_int8 _ -> 1
    | AST.Data_int16 _ -> 2
    | AST.Data_int32 _ -> 4
    | _ -> assert false (* do not use on these arguments *) in
  (AST.QInt i, AST.Sig_int (i, AST.Unsigned))

let assign_data x stmt (offsets, data) =
  let off = sum_offsets offsets in
  let addr = Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol x), AST.Sig_ptr) in
  let e = Cminor.Expr (Cminor.Op2 (AST.Op_addp, addr, off), AST.Sig_ptr) in
  let stmt' = match data with
(*
    | AST.Data_reserve _ -> Cminor.St_skip
*)
    | AST.Data_int8 i | AST.Data_int16 i | AST.Data_int32 i ->
      let (quantity, etype) = quantity_sig_of_data data in
      let cst = Cminor.Expr (Cminor.Cst (AST.Cst_int i), etype) in
      Cminor.St_store (quantity, e, cst)
    | AST.Data_float32 f | AST.Data_float64 f -> error_float () in
  Cminor.St_seq (stmt, stmt')

let add_global_initializations_body vars body =
  assert false (* TODO M1 *)

let add_global_initializations_funct vars = function
  | Cminor.F_int def ->
    let f_body = add_global_initializations_body vars def.Cminor.f_body in
    Cminor.F_int { def with Cminor.f_body = f_body }
  | def -> def

(* [add_global_initializations p] moves the initializations of the globals of
   [p] to the beginning of the main function, if any. *)

let add_global_initializations p = match p.Cminor.main with
  | None -> p.Cminor.functs
  | Some main ->
    let main_def = List.assoc main p.Cminor.functs in
    let main_def = add_global_initializations_funct p.Cminor.vars main_def in
    MiscPottier.update_list_assoc main main_def p.Cminor.functs

(* Translation of a Cminor program to a RTLabs program. *)

let translate p =

  (* Fetch the labels already used in the program to create new ones. *)
  let lbls = CminorAnnotator.all_labels p in

  (* The initialization of globals are moved at the beginning of the main. *)
  let functs = add_global_initializations p in

  (* The globals are associated their size. *)
  let f (id, size, _) = (id, size) in

  (* Put all this together and translate each function. *)
  { RTLabs.vars = List.map f p.Cminor.vars ;
    RTLabs.functs = List.map (translate_functions lbls) functs ;
    RTLabs.main = p.Cminor.main }
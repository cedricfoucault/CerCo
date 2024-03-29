
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

(** [allocate_expr rtlabs_fun e] creates a fresh pseudo-register that is added
    to the locals of the [RTLabs] function [rtlabs_fun] according to the type of
    the [Cminor] expression [e], and then returns the [RTLabs] function obtained
    along with the pseudo-register created. *)
let allocate_expr
    (rtlabs_fun : RTLabs.internal_function)
    (e          : Cminor.expression)
    : (RTLabs.internal_function * Register.t) =
  allocate rtlabs_fun (type_of e)

type local_env = Register.t StringTools.Map.t

(** [find_local lenv x] returns the pseudo-register associated to the variable
    [x] in the local environment [local_env]. *)
let find_local (lenv : local_env) (x : AST.ident) : Register.t =
  if StringTools.Map.mem x lenv then StringTools.Map.find x lenv
  else error ("Unknown local \"" ^ x ^ "\".")

let find_olocal (lenv : local_env) (ox : AST.ident option) : Register.t option =
  match ox with
    | None -> None
    | Some x -> Some (find_local lenv x)

(** [choose_destination rtlabs_fun lenv e] returns a pseudo-register in the
    [RTLabs] function [rtlabs_fun] that can safely represent the [Cminor]
    expression [e], considering the local environment [lenv]. *)
let choose_destination
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (e          : Cminor.expression)
    : RTLabs.internal_function * Register.t =
  match e with
    | Cminor.Expr (Cminor.Id x, _) -> (rtlabs_fun, find_local lenv x)
    | _ -> allocate_expr rtlabs_fun e

let choose_destinations
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (args       : Cminor.expression list)
    : RTLabs.internal_function * Register.t list =
  let f (rtlabs_fun, regs) e =
    let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
    (rtlabs_fun, regs @ [r]) in
  List.fold_left f (rtlabs_fun, []) args

(** [fresh_label rtlabs_fun] returns a fresh label from the point of view of the
    graph of the [RTLabs] function [rtlabs_fun]. *)
let fresh_label (rtlabs_fun : RTLabs.internal_function) : Label.t =
  Label.Gen.fresh rtlabs_fun.RTLabs.f_luniverse

(** [change_entry rtlabs_fun new_entry] sets the label [new_entry] as the entry
    label of the graph of the [RTLabs] function [rtlabs_fun]. *)
let change_entry
    (rtlabs_fun : RTLabs.internal_function)
    (new_entry : Label.t)
    : RTLabs.internal_function =
  { rtlabs_fun with RTLabs.f_entry = new_entry }


(** [add_graph rtlabs_fun lbl stmt] adds the label [lbl] and its associated
    statement [stmt] at the beginning of the graph of the [RTLabs] function
    [rtlabs_fun]. *)
let add_graph
    (rtlabs_fun : RTLabs.internal_function)
    (lbl        : Label.t)
    (stmt       : RTLabs.statement)
    : RTLabs.internal_function =
  let graph = Label.Map.add lbl stmt rtlabs_fun.RTLabs.f_graph in
  let rtlabs_fun = { rtlabs_fun with RTLabs.f_graph = graph } in
  change_entry rtlabs_fun lbl


(** [generate rtlabs_fun stmt] adds the statement [stmt] at the beginning of the
    graph of the [RTLabs] function [rtlabs_fun]. *)
let generate
    (rtlabs_fun : RTLabs.internal_function)
    (stmt       : RTLabs.statement)
    : RTLabs.internal_function =
  let lbl = fresh_label rtlabs_fun in
  add_graph rtlabs_fun lbl stmt

exception TileFail

(** [instruction_selector tiles arg1 .. arg4] tries each tile on the given
  arguments and returns the best result (smallest graph size) *)
let instruction_selector tiles
  (rtlabs_fun (* : RTLabs.internal_function *))
  (lenv       (* : local_env *))
  (exits      (* : Label.t list *))
  (stmt       (* : Cminor.statement *))
  : RTLabs.internal_function =
  let try_tile tile acc =
    try
      (tile rtlabs_fun lenv exits stmt) :: acc
    with
    | TileFail -> acc
  in
  let size rtlabs_fun = Label.Map.cardinal rtlabs_fun.RTLabs.f_graph in
  let minsize x y = if size x < size y then x else y in
  match List.fold_right try_tile tiles [] with
  | hd :: tl -> List.fold_left minsize hd tl
  | _ -> failwith "cannot happen (tile_list always contain a fallback)"
(** same function for branch tiles (these tiles have a different 
    type signature) *)
let branch_instruction_selector tiles
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (e          : Cminor.expression)
    (lbl_true   : Label.t)
    (lbl_false  : Label.t)
    : RTLabs.internal_function =
  let try_tile tile acc =
    try
      (tile rtlabs_fun lenv e lbl_true lbl_false) :: acc
    with
    | TileFail -> acc
  in
  let size rtlabs_fun = Label.Map.cardinal rtlabs_fun.RTLabs.f_graph in
  let minsize x y = if size x < size y then x else y in
  match List.fold_right try_tile tiles [] with
  | hd :: tl -> List.fold_left minsize hd tl
  | _ -> failwith "cannot happen (tile_list always contain a fallback)"

(* Translating conditions *)

(** [translate_branch rtlabs_fun lenv e lbl_true lbl_false] adds instructions at
    the beginning of the graph of the [RTLabs] function [rtlabs_fun] such that
    the [Cminor] expression [e] is evaluated, and if it is different from 0 then
    the flow will fall to the [lbl_true] label of the graph, and if it is 0 then
    it will fall to the [lbl_false] label. *)

let rec translate_branch rtlabs_fun lenv e lbl_true lbl_false =
    branch_instruction_selector branch_tile_list
        rtlabs_fun lenv e lbl_true lbl_false
    
and branch_tile_list =
    let tile_cond_cmpz rtlabs_fun lenv e lbl_true lbl_false = match e with
    | Cminor.Expr (
        Cminor.Op2 (
            op2,
            el,
            er
        ),
        _
      ) -> begin
          (* get the comparison operator *)
          let cmp = match op2 with
          | AST.Op_cmp(c) -> c
          | AST.Op_cmpu(c) -> c
          | _ -> raise TileFail
          (* get the operand which is compared to zero *)
          and e = match (el, er) with
          | (Cminor.Expr(Cminor.Cst(AST.Cst_int(0)), _), _) -> er
          | (_, Cminor.Expr(Cminor.Cst(AST.Cst_int(0)), _)) -> el
          | _ -> raise TileFail
          in
          (* choose a destination register for the expression to compare *)
          let (rtlabs_fun, r) = choose_destination rtlabs_fun lenv e in
          (* generate the RTLabs cmpzero statement *)
          let stmt =
            if e = er then
                RTLabs.St_cond_cmpz (cmp, r, lbl_true, lbl_false)
            else (* reverse order if zero is the right operand *)
                RTLabs.St_cond_cmpz (cmp, r, lbl_false, lbl_true)
          in
          (* add the statement to the target graph *)
          let rtlabs_fun = generate rtlabs_fun stmt in
          (* evaluate the expression *)
          translate_expr rtlabs_fun lenv r e
      end
    | _ -> raise TileFail
    
    and tile_cond_cmp rtlabs_fun lenv e lbl_true lbl_false = match e with
    | Cminor.Expr (
        Cminor.Op2 (
            op2,
            el,
            er
        ),
        _
      ) -> begin
          (* get the comparison operator *)
          let cmp = match op2 with
          | AST.Op_cmp(c) -> c
          | AST.Op_cmpu(c) -> c
          | _ -> raise TileFail
          in
          (* choose a destination register for the 2 expressions to compare *)
          let (rtlabs_fun, rl) = choose_destination rtlabs_fun lenv el in
          let (rtlabs_fun, rr) = choose_destination rtlabs_fun lenv er in
          (* generate the RTLabs cmp statement *)
          let stmt = RTLabs.St_cond_cmp (cmp, rl, rr, lbl_true, lbl_false) in
          (* add the statement to the target graph *)
          let rtlabs_fun = generate rtlabs_fun stmt in
          (* evaluate the two expressions *)
          let rtlabs_fun = translate_expr rtlabs_fun lenv rl el in
          translate_expr rtlabs_fun lenv rr er
      end
    | _ -> raise TileFail
    
    in
    
    [tile_cond_cmpz; tile_cond_cmp; translate_branch_fallback]

and translate_branch_fallback
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

(* Translating expressions *)

(** [translate_expr rtlabs_fun lenv destr e] adds instructions at the beginning
    of the graph of the [RTLabs] function [rtlabs_fun] such that the translation
    of the [Cminor] expression [e] is assigned to the pseudo-register
    [destr]. *)

and translate_expr rtlabs_fun lenv destr e =
    instruction_selector tile_list rtlabs_fun lenv destr e

and tile_list = [translate_expr_fallback]

and translate_expr_fallback
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (destr      : Register.t)
    (e          : Cminor.expression)
    : RTLabs.internal_function =
  let Cminor.Expr (ed, t) = e in
  match ed with

    | Cminor.Id x ->
      let xr = find_local lenv x in
      (* If the destination and source are the same, just do nothing. *)
      if Register.equal destr xr then rtlabs_fun
      else
	let old_entry = rtlabs_fun.RTLabs.f_entry in
	let stmt = RTLabs.St_op1 (AST.Op_id, destr, xr, old_entry) in
	generate rtlabs_fun stmt

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
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let rtlabs_fun = translate_expr rtlabs_fun lenv destr e3 in
      let lbl_false = rtlabs_fun.RTLabs.f_entry in
      let rtlabs_fun = change_entry rtlabs_fun old_entry in
      let rtlabs_fun = translate_expr rtlabs_fun lenv destr e2 in
      let lbl_true = rtlabs_fun.RTLabs.f_entry in
      translate_branch rtlabs_fun lenv e1 lbl_true lbl_false

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


(* Translating statements *)

(** [translate_stmt rtlabs_fun lenv exits stmt] adds instructions at the
    beginning of the graph of the [RTLabs] function [rtlabs_fun] such that
    translates the [Cminor] statement [stmt]. [lenv] is the local environment
    associating a pseudo-register to each variable of the [Cminor] program, and
    [exits] are the labels associated to indices used in [exit] statements. *)
let rec translate_stmt rtlabs_fun lenv exits stmt =
  instruction_selector tile_list rtlabs_fun lenv exits stmt

and tile_list =
    let tile_addi rtlabs_fun lenv exits stmt = match stmt with
    | Cminor.St_assign (
        x1, 
        Cminor.Expr(
            Cminor.Op2(op2, 
                Cminor.Expr(Cminor.Id(x2), _),
                Cminor.Expr(Cminor.Cst(c), _)
            ),
            _
        )
    ) when x1 = x2 && (op2 = AST.Op_add || op2 = AST.Op_sub) ->
        let c = 
            if op2 = AST.Op_add then c 
            else 
                match c with 
                | AST.Cst_int(i) -> AST.Cst_int(- i)
                | _ -> raise TileFail
        in
        let old_entry = rtlabs_fun.RTLabs.f_entry in
        let reg = find_local lenv x1 in
        let stmt = RTLabs.St_addi (c, reg, reg, old_entry) in
        generate rtlabs_fun stmt
    | _ -> raise TileFail
    in
    
    [tile_addi; translate_stmt_fallback]

and translate_stmt_fallback
    (rtlabs_fun : RTLabs.internal_function)
    (lenv       : local_env)
    (exits      : Label.t list)
    (stmt       : Cminor.statement)
    : RTLabs.internal_function =
  match stmt with

    | Cminor.St_skip -> rtlabs_fun

    | Cminor.St_assign (x, e) ->
      translate_expr rtlabs_fun lenv (find_local lenv x) e

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
      let rtlabs_fun = translate_stmt rtlabs_fun lenv exits s2 in
      translate_stmt rtlabs_fun lenv exits s1

    | Cminor.St_ifthenelse (e, s1, s2) ->
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      let rtlabs_fun = translate_stmt rtlabs_fun lenv exits s2 in
      let lbl_false = rtlabs_fun.RTLabs.f_entry in
      let rtlabs_fun = change_entry rtlabs_fun old_entry in
      let rtlabs_fun = translate_stmt rtlabs_fun lenv exits s1 in
      let lbl_true = rtlabs_fun.RTLabs.f_entry in
      translate_branch rtlabs_fun lenv e lbl_true lbl_false

    | Cminor.St_loop s ->
      let loop_start = fresh_label rtlabs_fun in
      let rtlabs_fun = change_entry rtlabs_fun loop_start in
      let rtlabs_fun = translate_stmt rtlabs_fun lenv exits s in
      let old_entry = rtlabs_fun.RTLabs.f_entry in
      add_graph rtlabs_fun loop_start (RTLabs.St_skip old_entry)

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

(** [assign_data x stmt (offsets, data)] adds statements at the end of the
    [Cminor] statement [stmt] that assign the data [data] to the global pointer
    [x] shifted by [offsets] ([data] might be broken into multiple pieces).  *)
let assign_data
    (x               : AST.ident)
    (stmt            : Cminor.statement)
    ((offsets, data) : (AST.abstract_offset list * AST.data))
    : Cminor.statement =
  let off = sum_offsets offsets in
  let addr = Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol x), AST.Sig_ptr) in
  let e = Cminor.Expr (Cminor.Op2 (AST.Op_addp, addr, off), AST.Sig_ptr) in
  let stmt' = match data with
    | AST.Data_int8 i | AST.Data_int16 i | AST.Data_int32 i ->
      let (quantity, etype) = quantity_sig_of_data data in
      let cst = Cminor.Expr (Cminor.Cst (AST.Cst_int i), etype) in
      Cminor.St_store (quantity, e, cst)
    | AST.Data_float32 f | AST.Data_float64 f -> error_float () in
  Cminor.St_seq (stmt, stmt')

(** [add_global_initializations_body vars body] adds statements at the beginning
    of the statement [body] that make the initializations of the global
    variables [vars]. *)
let add_global_initializations_body
    (vars : (AST.ident * AST.abstract_size * AST.data list option) list)
    (body : Cminor.statement)
    : Cminor.statement =
  let f stmt (x, size, datas_opt) = match datas_opt with
    | None -> Cminor.St_skip
    | Some datas ->
      let offsets = Memory.all_offsets size in
      if List.length offsets <> List.length datas then
	error "bad global initialization style."
      else
	let offs_datas = List.combine offsets datas in
	List.fold_left (assign_data x) stmt offs_datas in
  Cminor.St_seq (List.fold_left f Cminor.St_skip vars, body)

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

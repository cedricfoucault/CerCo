
(** This module provides an interpreter for the RTLabs language. *)


let error_prefix = "RTLabs interpret"
let error s = Error.global_error error_prefix s


module Mem = Driver.RTLabsMemory
module Val = Mem.Value

let error_float () = error "float not supported"


type memory = RTLabs.function_def Mem.memory


(* Local environments. They associate a value and a type to the registers of the
   function being executed. *)

type local_env = (Val.t * AST.sig_type) Register.Map.t

(* Call frames. The execution state has a call stack, each element of the stack
   being composed of the return registers to store the result of the callee, the
   graph, the stack pointer, the node, the local environment and the typing
   environments to resume the execution of the caller. *)

type stack_frame =
    { ret_reg  : Register.t option ;
      graph    : RTLabs.graph ;
      sp       : Val.address ;
      pc       : Label.t ;
      lenv     : local_env }

(* Execution states. There are three possible states :
   - The constructor [State] represents a state when executing a function
   - The constructor [CallState] represents a state when calling a function
   - The constructor [ReturnState] represents a state when leaving a function *)

type state =
  | State of stack_frame list * RTLabs.graph * Val.address (* stack pointer *) *
             Label.t * local_env * memory * CostLabel.t list
  | CallState of stack_frame list * RTLabs.function_def *
                 Val.t list (* args *) * memory * CostLabel.t list
  | ReturnState of stack_frame list * Val.t (* return value *) *
                   memory * CostLabel.t list

let string_of_local_env lenv =
  let f x (v, _) s =
    s ^
      (if Val.eq v Val.undef then ""
       else (Register.print x) ^ " = " ^ (Val.to_string v) ^ "  ") in
  Register.Map.fold f lenv ""

let string_of_args args =
  let f s v = s ^ " " ^ (Val.to_string v) in
  List.fold_left f "" args

let print_state = function
  | State (_, _, sp, lbl, lenv, mem, _) ->
    Printf.printf "Stack pointer: %s\n\nLocal environment:\n%s\n\nMemory:%s\nRegular state: %s\n\n%!"
      (Val.string_of_address sp)
      (string_of_local_env lenv)
      (Mem.to_string mem)
      lbl
  | CallState (_, _, args, mem, _) ->
    Printf.printf "Memory:%s\nCall state: %s\n\n%!"
      (Mem.to_string mem)
      (string_of_args args)
  | ReturnState (_, v, mem, _) ->
    Printf.printf "Memory:%s\nReturn state: %s\n\n%!"
      (Mem.to_string mem)
      (Val.to_string v)


let find_function mem f =
  let addr = Mem.find_global mem f in
  Mem.find_fun_def mem addr

let get_local_env f lenv r =
  if Register.Map.mem r lenv then f (Register.Map.find r lenv)
  else error ("Unknown local register \"" ^ (Register.print r) ^ "\".")

let get_value = get_local_env fst
let get_args lenv args = List.map (get_value lenv) args

let get_type = get_local_env snd

let update_local r v lenv =
  let f (_, t) = Register.Map.add r (v, t) lenv in
  get_local_env f lenv r

let update_locals rs vs lenv =
  let f lenv r v = update_local r v lenv in
  List.fold_left2 f lenv rs vs

let value_of_address = List.hd
let address_of_value v = [v]


module Eval = CminorInterpret.Eval_op (Mem)

let concrete_stacksize = Eval.concrete_stacksize


(* Assign a value to some destinations registers. *)

let assign_state sfrs graph sp lbl lenv mem trace destr v =
  let lenv = update_local destr v lenv in
  State (sfrs, graph, sp, lbl, lenv, mem, trace)

(* Branch on a value. *)

let branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v =
  let next_lbl =
    if Val.is_true v then lbl_true
    else
      if Val.is_false v then lbl_false
      else error "Undefined conditional value." in
  State (sfrs, graph, sp, next_lbl, lenv, mem, trace)


(* Interpret statements. *)

let interpret_statement
    (sfrs  : stack_frame list)
    (graph : RTLabs.graph)
    (sp    : Val.address)
    (lenv  : local_env)
    (mem   : memory)
    (stmt  : RTLabs.statement)
    (trace : CostLabel.t list) :
    state = match stmt with

      | RTLabs.St_skip lbl ->
	State (sfrs, graph, sp, lbl, lenv, mem, trace)

      | RTLabs.St_cost (cost_lbl, lbl) ->
	State (sfrs, graph, sp, lbl, lenv, mem, cost_lbl :: trace)

      | RTLabs.St_cst (destr, cst, lbl) ->
	let v = Eval.cst mem sp (get_type lenv destr) cst in
	assign_state sfrs graph sp lbl lenv mem trace destr v

      | RTLabs.St_op1 (op1, destr, srcr, lbl) ->
	let v =
	  Eval.op1 (get_type lenv destr) (get_type lenv srcr) op1
	    (get_value lenv srcr) in
	assign_state sfrs graph sp lbl lenv mem trace destr v

      | RTLabs.St_op2 (op2, destr, srcr1, srcr2, lbl) ->
	let v =
	  Eval.op2
	    (get_type lenv destr) (get_type lenv srcr1) (get_type lenv srcr2)
	    op2
	    (get_value lenv srcr1)
	    (get_value lenv srcr2) in
	assign_state sfrs graph sp lbl lenv mem trace destr v

      | RTLabs.St_addi (i, srcr, destr, lbl) ->
        (* retrieve the integer value from constant i *)
        let i_val = match i with
        | AST.Cst_int v -> v
        | _ -> failwith "addi statement should be given an integer constant"
        in
        (* retrieve the signature of an integer *)
        let int_sig = AST.Sig_int (Val.int_size, AST.Signed) in
        (* evaluate the value (srcr + i) *)
        let v = Eval.op2
          (get_type lenv destr) (get_type lenv srcr) int_sig
          AST.Op_add
          (get_value lenv srcr)
          (Val.of_int i_val)
        in
        (* put this value in destr *)
        assign_state sfrs graph sp lbl lenv mem trace destr v
        (* failwith "not implemented yet" *)

      | RTLabs.St_load (q, addr, destr, lbl) ->
	let addr = address_of_value (get_value lenv addr) in
	let v = Mem.loadq mem q addr in
	assign_state sfrs graph sp lbl lenv mem trace destr v

      | RTLabs.St_loadi (q, i, addr, destr, lbl) ->
        (* retrieve the integer value from constant i *)
        let i_val = match i with
        | AST.Cst_int v -> v
        | _ -> failwith "addi statement should be given an integer constant"
        in
        (* retrieve the signature of an integer *)
        let int_sig = AST.Sig_int (Val.int_size, AST.Signed) in
        (* evaluate (addr + i) *)
        let v = Eval.op2
          (get_type lenv addr) (get_type lenv addr) int_sig
          AST.Op_add
          (get_value lenv addr)
          (Val.of_int i_val)
        in
        (* get the address from the calculated value *)
        let addr = address_of_value v in
        (* load the stored value from memory *)
        let v = Mem.loadq mem q addr in
        (* put this value in destr *)
        assign_state sfrs graph sp lbl lenv mem trace destr v
        (* failwith "not implemented yet" *)

      | RTLabs.St_store (q, addr, srcr, lbl) ->
	let addr = address_of_value (get_value lenv addr) in
	let v = get_value lenv srcr in
	let mem = Mem.storeq mem q addr v in
	State (sfrs, graph, sp, lbl, lenv, mem, trace)

      | RTLabs.St_storei (q, i, addr, srcr, lbl) ->
        (* retrieve the integer value from constant i *)
        let i_val = match i with
        | AST.Cst_int v -> v
        | _ -> failwith "addi statement should be given an integer constant"
        in
        (* retrieve the signature of an integer *)
        let int_sig = AST.Sig_int (Val.int_size, AST.Signed) in
        (* evaluate (addr + i) *)
        let v = Eval.op2
          (get_type lenv srcr) (get_type lenv srcr) int_sig
          AST.Op_add
          (get_value lenv addr)
          (Val.of_int i_val)
        in
        (* get the address from the calculated value *)
        let addr = address_of_value v in
        (* get the value to store *)
        let v = get_value lenv srcr in
        (* store this value at the address previously computed *)
        let mem = Mem.storeq mem q addr v in
        (* return the new state *)
        State (sfrs, graph, sp, lbl, lenv, mem, trace)
        (* failwith "not implemented yet" *)

      | RTLabs.St_call_id (f, args, destr, sg, lbl) ->
	let f_def = find_function mem f in
	let args = get_args lenv args in
	(* Save the stack frame. *)
	let sf =
	  { ret_reg = destr ; graph = graph ; sp = sp ; pc = lbl ; lenv = lenv }
	in
	CallState (sf :: sfrs, f_def, args, mem, trace)

      | RTLabs.St_call_ptr (r, args, destr, sg, lbl) ->
	let addr = get_value lenv r in
	let f_def = Mem.find_fun_def mem (address_of_value addr) in
	let args = get_args lenv args in
	(* Save the stack frame. *)
	let sf =
	  { ret_reg = destr ; graph = graph ; sp = sp ; pc = lbl ; lenv = lenv }
	in
	CallState (sf :: sfrs, f_def, args, mem, trace)

      | RTLabs.St_tailcall_id (f, args, sg) ->
	let f_def = find_function mem f in
	let args = get_args lenv args in
	(* No need to save the stack frame. But free the stack. *)
	let mem = Mem.free mem sp in
	CallState (sfrs, f_def, args, mem, trace)

      | RTLabs.St_tailcall_ptr (r, args, sg) ->
	let addr = get_value lenv r in
	let f_def = Mem.find_fun_def mem (address_of_value addr) in
	let args = get_args lenv args in
	(* No need to save the stack frame. But free the stack. *)
	let mem = Mem.free mem sp in
	CallState (sfrs, f_def, args, mem, trace)

      | RTLabs.St_cond (srcr, lbl_true, lbl_false) ->
	let v = get_value lenv srcr in
	branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v

      | RTLabs.St_cond_cmpz (cmp, srcr, lbl_true, lbl_false) ->
        (* retrieve the signature of the integer constant 0 *)
        let int_sig = AST.Sig_int (Val.int_size, AST.Signed) in
        (* retrieve the binary operator from cmp *)
        let op2_cmp = AST.Op_cmp cmp in
        (* evaluate the comparison with zero *)
        let v = Eval.op2
    	    (get_type lenv srcr) (get_type lenv srcr) int_sig
    	    op2_cmp
    	    (get_value lenv srcr)
    	    (Val.of_int 0)
    	  in
    	  (* do the conditional branch on the result *)
    	  branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v

    	| RTLabs.St_cond_cmp (cmp, srcr1, srcr2, lbl_true, lbl_false) ->
    	  (* retrieve the binary operator from cmp *)
        let op2_cmp = AST.Op_cmp cmp in
        (* evaluate the comparison between srcr1 and srcr2 *)
        let v = Eval.op2
    	    (get_type lenv srcr1) (get_type lenv srcr1) (get_type lenv srcr2)
    	    op2_cmp
    	    (get_value lenv srcr1)
    	    (get_value lenv srcr2)
    	  in
    	  (* do the conditional branch on the result *)
    	  branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v

(*
      | RTLabs.St_condcst (cst, t, lbl_true, lbl_false) ->
	let v = Eval.cst mem sp t cst in
	branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v

      | RTLabs.St_cond1 (op1, srcr, lbl_true, lbl_false) ->
	let v =
	  Eval.op1 (get_type lenv srcr) (get_type lenv srcr)
	    op1 (get_value lenv srcr) in
	branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v

      | RTLabs.St_cond2 (op2, srcr1, srcr2, lbl_true, lbl_false) ->
	let v =
	  Eval.op2 op2
	    (get_value lenv srcr1)
	    (get_value lenv srcr2) in
	branch_state sfrs graph sp lbl_true lbl_false lenv mem trace v
*)

      | RTLabs.St_jumptable (r, table) -> assert false (* TODO: jumptable *)
      (*
	let i = match get_value lenv r with
	| Val.Val_int i -> i
	| Val.Val_ptr _ -> error "Illegal cast from pointer to integer."
	| _ -> error "Typing error." in
	(try
	let next_lbl = List.nth table i in
	State (sfrs, graph, sp, next_lbl, lenv, mem, trace)
	with
	| Failure "nth" | Invalid_argument "List.nth" ->
	error "Index out of jumptable.")
      *)

      | RTLabs.St_return None ->
	let mem = Mem.free mem sp in
	ReturnState (sfrs, Val.undef, mem, trace)

      | RTLabs.St_return (Some r) ->
	let v = get_value lenv r in
	let mem = Mem.free mem sp in
	ReturnState (sfrs, v, mem, trace)


module InterpretExternal = Primitive.Interpret (Mem)

let interpret_external mem f args = match InterpretExternal.t mem f args with
  | (mem', InterpretExternal.V vs) ->
    let v = if List.length vs = 0 then Val.undef else List.hd vs in
    (mem', v)
  | (mem', InterpretExternal.A addr) -> (mem', value_of_address addr)


let init_locals
    (locals           : (Register.t * AST.sig_type) list)
    (params           : (Register.t * AST.sig_type) list)
    (args             : Val.t list) :
    local_env =
  let f_param lenv (r, t) v = Register.Map.add r (v, t) lenv in
  let f_local lenv (r, t) = Register.Map.add r (Val.undef, t) lenv in
  let lenv = List.fold_left2 f_param Register.Map.empty params args in
  List.fold_left f_local lenv locals

let state_after_call
    (sfrs  : stack_frame list)
    (f_def : RTLabs.function_def)
    (args  : Val.t list)
    (mem   : memory)
    (trace : CostLabel.t list) :
    state =
  match f_def with
    | RTLabs.F_int def ->
      let (mem', sp) =
	Mem.alloc mem (concrete_stacksize def.RTLabs.f_stacksize) in
      let lenv = init_locals def.RTLabs.f_locals def.RTLabs.f_params args in
      State (sfrs, def.RTLabs.f_graph, sp, def.RTLabs.f_entry, lenv, mem',
	     trace)
    | RTLabs.F_ext def ->
      let (mem', v) = interpret_external mem def.AST.ef_tag args in
      ReturnState (sfrs, v, mem', trace)


let state_after_return
    (sf      : stack_frame)
    (sfrs    : stack_frame list)
    (ret_val : Val.t)
    (mem     : memory)
    (trace   : CostLabel.t list) :
    state =
  let lenv = match sf.ret_reg with
    | None -> sf.lenv
    | Some ret_reg -> update_local ret_reg ret_val sf.lenv in
  State (sfrs, sf.graph, sf.sp, sf.pc, lenv, mem, trace)


let small_step (st : state) : state = match st with
  | State (sfrs, graph, sp, pc, lenv, mem, trace) ->
    let stmt = Label.Map.find pc graph in
    interpret_statement sfrs graph sp lenv mem stmt trace
  | CallState (sfrs, f_def, args, mem, trace) ->
    state_after_call sfrs f_def args mem trace
  | ReturnState ([], ret_val, mem, trace) ->
    assert false (* End of execution; handled in iter_small_step. *)
  | ReturnState (sf :: sfrs, ret_val, mem, trace) ->
    state_after_return sf sfrs ret_val mem trace


let compute_result v =
  if Val.is_int v then IntValue.Int32.cast (Val.to_int_repr v)
  else IntValue.Int32.zero

let rec iter_small_step debug st =
  let print_and_return_result (res, cost_labels) =
    if debug then Printf.printf "Result = %s\n%!"
      (IntValue.Int32.to_string res) ;
    (res, cost_labels) in
  if debug then print_state st ;
  match small_step st with
    | ReturnState ([], v, mem, trace) ->
      print_and_return_result (compute_result v, List.rev trace)
    | st' -> iter_small_step debug st'


let add_global_vars =
  List.fold_left (fun mem (id, size) -> Mem.add_var mem id size None)

let add_fun_defs =
  List.fold_left (fun mem (f_id, f_def) -> Mem.add_fun_def mem f_id f_def)


(* The memory is initialized by loading the code into it, and by reserving space
   for the global variables. *)

let init_mem (p : RTLabs.program) : memory =
  add_global_vars (add_fun_defs Mem.empty p.RTLabs.functs) p.RTLabs.vars


(* Interpret the program only if it has a main. *)

let interpret debug p =
  Printf.printf "*** RTLabs interpret ***\n%!" ;
  match p.RTLabs.main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let mem = init_mem p in
      let main_def = find_function mem main in
      let st = CallState ([], main_def, [], mem, []) in
      iter_small_step debug st

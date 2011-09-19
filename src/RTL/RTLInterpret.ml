
(** This module provides an interpreter for the RTL language. *)


let error_prefix = "RTL interpret"
let error s = Error.global_error error_prefix s


module Mem = Driver.TargetArchMemory
module Val = Mem.Value
module Eval = Arch.Eval (Val)


type memory = RTL.function_def Mem.memory


(* Local environments. They associate a value to the registers of the function
   being executed. *)

type local_env = Val.t Register.Map.t

(* Call frames. The execution state has a call stack, each element of the stack
   being composed of the return registers to store the result of the callee, the
   graph, the node, and the local environment to resume the execution of the
   caller. *)

type stack_frame =
    { ret_regs : Register.t list ;
      graph    : RTL.graph ;
      pc       : Label.t ;
      sp       : Val.address ;
      lenv     : local_env }

(* Execution states. There are three possible states :
   - The constructor [State] represents a state when executing a function
   - The constructor [CallState] represents a state when calling a function
   - The constructor [ReturnState] represents a state when leaving a function *)

type state =
  | State of stack_frame list * RTL.graph * Label.t * Val.address (* sp *) *
             local_env * memory * Val.address (* gp *) *
             CostLabel.t list
  | CallState of stack_frame list * RTL.function_def *
                 Val.t list (* args *) * memory * Val.address (* gp *) *
		 CostLabel.t list
  | ReturnState of stack_frame list * Val.t list (* return values *) *
                   memory * Val.address (* gp *) * CostLabel.t list

let string_of_local_env lenv =
  let f x v s =
    s ^
      (if Val.eq v Val.undef then ""
       else (Register.print x) ^ " = " ^ (Val.to_string v) ^ "  ") in
  Register.Map.fold f lenv ""

let string_of_args args =
  let f s v = s ^ " " ^ (Val.to_string v) in
  List.fold_left f "" args

let print_state = function
  | State (_, _, lbl, sp, lenv, mem, gp, _) ->
    Printf.printf "Global pointer: %s\n\nStack pointer: %s\n\nLocal environment:\n%s\n\nMemory:%s\nRegular state: %s\n\n%!"
      (Val.string_of_address gp)
      (Val.string_of_address sp)
      (string_of_local_env lenv)
      (Mem.to_string mem)
      lbl
  | CallState (_, _, args, mem, _, _) ->
    Printf.printf "Memory:%s\nCall state: %s\n\n%!"
      (Mem.to_string mem)
      (string_of_args args)
  | ReturnState (_, vs, mem, _, _) ->
    Printf.printf "Memory:%s\nReturn state: %s\n\n%!"
      (Mem.to_string mem)
      (string_of_args vs)


let find_function mem f =
  let addr = Mem.find_global mem f in
  Mem.find_fun_def mem addr

let get_local_value (lenv : local_env) (r : Register.t) : Val.t =
  if Register.Map.mem r lenv then Register.Map.find r lenv
  else error ("Unknown local register \"" ^ (Register.print r) ^ "\".")
let get_arg_values lenv args = List.map (get_local_value lenv) args

let get_local_addr lenv addr =
  List.map (get_local_value lenv) addr


let adds rs vs lenv =
  let f lenv r v = Register.Map.add r v lenv in
  List.fold_left2 f lenv rs vs


(* Assign a value to some destinations registers. *)

let assign_state sfrs graph lbl sp lenv mem gp trace destrs vs =
  let lenv = adds destrs vs lenv in
  State (sfrs, graph, lbl, sp, lenv, mem, gp, trace)

(* Branch on a value. *)

let branch_state sfrs graph lbl_true lbl_false sp lenv mem gp trace v =
  let next_lbl =
    if Val.is_true v then lbl_true
    else
      if Val.is_false v then lbl_false
      else error "Undefined conditional value." in
  State (sfrs, graph, next_lbl, sp, lenv, mem, gp, trace)


(* Interpret statements. *)

let interpret_statement
    (sfrs  : stack_frame list)
    (graph : RTL.graph)
    (sp    : Val.address)
    (lenv  : local_env)
    (mem   : memory)
    (gp    : Val.address)
    (stmt  : RTL.statement)
    (trace : CostLabel.t list) :
    state = match stmt with

      | RTL.St_skip lbl ->
	State (sfrs, graph, lbl, sp, lenv, mem, gp, trace)

      | RTL.St_cost (cost_lbl, lbl) ->
	State (sfrs, graph, lbl, sp, lenv, mem, gp, cost_lbl :: trace)

      | RTL.St_int (r, i, lbl) ->
	assign_state sfrs graph lbl sp lenv mem gp trace [r] [Val.of_int i]

      | RTL.St_move (destr, srcr, lbl) ->
	assign_state sfrs graph lbl sp lenv mem gp trace
	  [destr] [get_local_value lenv srcr]

      | RTL.St_unop (unop, destr, srcr, lbl) ->
	let v = Eval.unop unop (get_local_value lenv srcr) in
	assign_state sfrs graph lbl sp lenv mem gp trace [destr] [v]

      | RTL.St_binop (binop, destr, srcr1, srcr2, lbl) ->
	let v =
	  Eval.binop binop
	    (get_local_value lenv srcr1)
	    (get_local_value lenv srcr2) in
	assign_state sfrs graph lbl sp lenv mem gp trace [destr] [v]

      | RTL.St_funaddr (destrs, x, lbl) ->
	assign_state sfrs graph lbl sp lenv mem gp trace destrs
	  (Mem.find_global mem x)

      | RTL.St_stackaddr (destrs, lbl) ->
	assign_state sfrs graph lbl sp lenv mem gp trace destrs sp

      | RTL.St_globaladdr (destrs, lbl) ->
	assign_state sfrs graph lbl sp lenv mem gp trace destrs gp

      | RTL.St_load (size, destr, addr, lbl) ->
	let addr = get_local_addr lenv addr in
	let v = Mem.load mem size addr in
	assign_state sfrs graph lbl sp lenv mem gp trace [destr] [v]

      | RTL.St_store (size, addr, srcr, lbl) ->
	let addr = get_local_addr lenv addr in
	let mem = Mem.store mem size addr (get_local_value lenv srcr) in
	State (sfrs, graph, lbl, sp, lenv, mem, gp, trace)

      | RTL.St_cond (srcr, lbl_true, lbl_false) ->
	let v = get_local_value lenv srcr in
	branch_state sfrs graph lbl_true lbl_false sp lenv mem gp trace v

      | RTL.St_return rl ->
	let vl = List.map (get_local_value lenv) rl in
	let mem = Mem.free mem sp in
	ReturnState (sfrs, vl, mem, gp, trace)

      | RTL.St_call (addr, args, ret_regs, lbl) ->
	let addr = get_local_addr lenv addr in
	let f_def = Mem.find_fun_def mem addr in
	let args = get_arg_values lenv args in
	let sf = { ret_regs = ret_regs ; graph = graph ; pc = lbl ;
		   sp = sp ; lenv = lenv } in
	CallState (sf :: sfrs, f_def, args, mem, gp, trace)

      | RTL.St_tailcall (addr, args) ->
	let addr = get_local_addr lenv addr in
	let f_def = Mem.find_fun_def mem addr in
	let args = get_arg_values lenv args in
	let mem = Mem.free mem sp in
	CallState (sfrs, f_def, args, mem, gp, trace)


module InterpretExternal = Primitive.Interpret (Mem)

let interpret_external mem f args = match InterpretExternal.t mem f args with
  | (mem', InterpretExternal.V vs) -> (mem', vs)
  | (mem', InterpretExternal.A addr) -> (mem', addr)

let init_locals
    (locals : Register.Set.t)
    (params : Register.t list)
    (args   : Val.t list) :
    local_env =
  let f r lenv = Register.Map.add r Val.undef lenv in
  let lenv = Register.Set.fold f locals Register.Map.empty in
  let f lenv r v = Register.Map.add r v lenv in
  List.fold_left2 f lenv params args

let state_after_call
    (sfrs  : stack_frame list)
    (f_def : RTL.function_def)
    (args  : Val.t list)
    (mem   : memory)
    (gp    : Val.address)
    (trace : CostLabel.t list) :
    state =
  match f_def with
    | RTL.F_int def ->
      let (mem', sp) = Mem.alloc mem def.RTL.f_stacksize in
      State (sfrs, def.RTL.f_graph, def.RTL.f_entry, sp,
	     init_locals def.RTL.f_locals def.RTL.f_params args,
	     mem', gp, trace)
    | RTL.F_ext def ->
      let (mem', vs) = interpret_external mem def.AST.ef_tag args in
      ReturnState (sfrs, vs, mem', gp, trace)

let state_after_return
    (sf       : stack_frame)
    (sfrs     : stack_frame list)
    (ret_vals : Val.t list)
    (mem      : memory)
    (gp    : Val.address)
    (trace    : CostLabel.t list) :
    state =
  let f i lenv r = Register.Map.add r (List.nth ret_vals i) lenv in
  let lenv = MiscPottier.foldi f sf.lenv sf.ret_regs in
  State (sfrs, sf.graph, sf.pc, sf.sp, lenv, mem, gp, trace)


let small_step (st : state) : state = match st with
  | State (sfrs, graph, pc, sp, lenv, mem, gp, trace) ->
    let stmt = Label.Map.find pc graph in
    interpret_statement sfrs graph sp lenv mem gp stmt trace
  | CallState (sfrs, f_def, args, mem, gp, trace) ->
    state_after_call sfrs f_def args mem gp trace
  | ReturnState ([], _, _, _, _) ->
    assert false (* End of execution; handled in iter_small_step. *)
  | ReturnState (sf :: sfrs, ret_vals, mem, gp, trace) ->
    state_after_return sf sfrs ret_vals mem gp trace


let compute_result vs =
  let f res v = res && (Val.is_int v) in
  let is_int vs = (List.length vs > 0) && (List.fold_left f true vs) in
  if is_int vs then
    let chunks =
      List.map (fun v -> IntValue.Int32.cast (Val.to_int_repr v)) vs in
    IntValue.Int32.merge chunks
  else IntValue.Int32.zero

let rec iter_small_step debug st =
  let print_and_return_result (res, cost_labels) =
    if debug then Printf.printf "Result = %s\n%!"
      (IntValue.Int32.to_string res) ;
    (res, cost_labels) in
  if debug then print_state st ;
  match small_step st with
    | ReturnState ([], vs, _, _, trace) ->
      print_and_return_result (compute_result vs, List.rev trace)
    | st' -> iter_small_step debug st'


let add_global_vars =
  List.fold_left
    (fun mem (id, size) -> Mem.add_var mem id (AST.SQ (AST.QInt size)) None)

let add_fun_defs =
  List.fold_left (fun mem (f_id, f_def) -> Mem.add_fun_def mem f_id f_def)


(* The memory is initialized by loading the code into it, and by reserving space
   for the global variables. *)

let init_mem (p : RTL.program) : (memory * Val.address) =
  let mem = add_fun_defs Mem.empty p.RTL.functs in
  Mem.alloc mem p.RTL.globals


(* Interpret the program only if it has a main. *)

let interpret debug p =
  Printf.printf "*** RTL interpret ***\n%!" ;
  match p.RTL.main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let (mem, gp) = init_mem p in
      let main_def = find_function mem main in
      let st = CallState ([], main_def, [], mem, gp, []) in
      iter_small_step debug st

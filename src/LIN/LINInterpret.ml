
(** This module provides an interpreter for the LIN language. *)


let error_prefix = "LIN interpret"
let error s = Error.global_error error_prefix s


module Mem = Driver.TargetArchMemory
module Val = Mem.Value
module Eval = Arch.Eval (Val)


(* Memory *)

type memory = LIN.function_def Mem.memory

(* Hardware registers environments. They associate a value to each hardware
   register. *)

type hdw_reg_env = Val.t Driver.TargetArch.RegisterMap.t

(* Execution states. *)

type state =
    { pc     : Val.address ;
      exit   : Val.address ;
      renv   : hdw_reg_env ;
      mem    : memory ;
      trace  : CostLabel.t list }


(* Helpers *)

let change_pc st pc = { st with pc = pc }
let change_exit st exit = { st with exit = exit }
let change_renv st renv = { st with renv = renv }
let change_mem st mem = { st with mem = mem }
let change_trace st trace = { st with trace = trace }
let add_trace st cost_lbl = change_trace st (cost_lbl :: st.trace)

let empty_state =
  { pc     = Val.null ;
    exit   = Val.null ;
    renv   = Driver.TargetArch.RegisterMap.empty ;
    mem    = Mem.empty ;
    trace  = [] }


let int_fun_of_ptr mem ptr = match Mem.find_fun_def mem ptr with
  | LIN.F_int def -> def
  | _ -> error "Trying to fetch the definition of an external function."

let current_int_fun st = int_fun_of_ptr st.mem st.pc

let fetch_stmt st =
  let msg =
    Printf.sprintf "%s does not point to a statement."
      (Val.string_of_address st.pc) in
  if Val.is_mem_address st.pc then
    let off = Val.offset_of_address st.pc in
    let def = int_fun_of_ptr st.mem st.pc in
    List.nth def (Val.Offset.to_int off)
  else error msg

let init_fun_call st ptr =
  change_pc st (Val.change_address_offset ptr Val.Offset.zero)

let next_pc st =
  change_pc st (Val.add_address st.pc Val.Offset.one)

let add_reg r v st =
  let renv = Driver.TargetArch.RegisterMap.add r v st.renv in
  change_renv st renv

let add_regs rs vs st =
  assert (List.length rs = List.length vs) ;
  let f st r v = add_reg r v st in
  List.fold_left2 f st rs vs

let get_reg r st =
  if Driver.TargetArch.RegisterMap.mem r st.renv then
    Driver.TargetArch.RegisterMap.find r st.renv
  else error ("Unknown hardware register " ^
		 (Driver.TargetArch.print_register r) ^ ".")

let get_regs rs st = List.map (fun r -> get_reg r st) rs

let get_addr = get_regs

let get_ra = get_addr Driver.TargetArch.ra

let get_gp = get_addr Driver.TargetArch.gp

let get_sp = get_addr Driver.TargetArch.sp

let get_result = get_addr Driver.TargetArch.result

let set_ra = add_regs Driver.TargetArch.ra

let set_gp = add_regs Driver.TargetArch.gp

let set_sp = add_regs Driver.TargetArch.sp

let save_ra st =
  let ra = Val.add_address st.pc Val.Offset.one in
  set_ra ra st

let find_label lbl =
  let rec aux i = function
  | [] -> error (Printf.sprintf "Unknown label %s." lbl)
  | LIN.St_label lbl' :: _ when lbl' = lbl -> i
  | _ :: code -> aux (i+1) code
  in
  aux 0

let pointer_of_label st lbl =
  let code = current_int_fun st in
  let off = find_label lbl code in
  Val.change_address_offset st.pc (Val.Offset.of_int off)

let goto st lbl =
  change_pc st (pointer_of_label st lbl)


(* State pretty-printing *)

let print_renv renv =
  let f r v =
    if not (Val.eq v Val.undef) then
    Printf.printf "\n%s = %s%!"
      (Driver.TargetArch.print_register r) (Val.to_string v) in
  Driver.TargetArch.RegisterMap.iter f renv

let print_state st =
  Printf.printf "PC: %s\n%!" (Val.string_of_address st.pc) ;
  print_renv st.renv ;
  Mem.print st.mem ;
  Printf.printf "\n%!"


module InterpretExternal = Primitive.Interpret (Mem)

let interpret_external mem f args = match InterpretExternal.t mem f args with
  | (mem', InterpretExternal.V vs) -> (mem', vs)
  | (mem', InterpretExternal.A addr) -> (mem', addr)

let external_hdw_args size st =
  let params = MiscPottier.prefix size Driver.TargetArch.parameters in
  List.map (fun r -> get_reg r st) params

let external_stack_args size st =
  let sp = get_sp st in
  let arg_offset arg_number = (arg_number + 1) * Driver.TargetArch.int_size in
  let arg_addr arg_number =
    Val.add_address sp (Val.Offset.of_int (-(arg_offset arg_number))) in
  let fetch_arg arg_number =
    Mem.load st.mem Driver.TargetArch.int_size (arg_addr arg_number) in
  let args_number = MiscPottier.make_sequence ((+) 1) 0 size in
  List.map fetch_arg args_number

let fetch_external_args f st =
  let args_quantity = Primitive.args_quantity f in
  let arg_reg_size quantity =
    MiscPottier.div_up
      (Mem.size_of_quantity quantity) Driver.TargetArch.int_size in
  let size =
    List.fold_left (+) 0 (List.map arg_reg_size args_quantity) in
  let hdw_params = List.length Driver.TargetArch.parameters in
  let hdw_nb_params = min size hdw_params in
  let stack_nb_params = max 0 (size - hdw_params) in
  let params1 = external_hdw_args hdw_nb_params st in
  let params2 = external_stack_args stack_nb_params st in
  params1 @ params2

let set_result st vs =
  let f st (r, v) = add_reg r v st in
  List.fold_left f st (MiscPottier.combine Driver.TargetArch.result vs)

let interpret_external_call st f =
  let args = fetch_external_args f st in
  let (mem, vs) = interpret_external st.mem f args in
  let st = change_mem st mem in
  let st = set_result st vs in
  next_pc st

let interpret_call st f =
  let ptr = get_addr f st in
  match Mem.find_fun_def st.mem ptr with
    | LIN.F_int def ->
      let st = save_ra st in
      init_fun_call st ptr
    | LIN.F_ext def ->
      let st = next_pc st in
      interpret_external_call st def.AST.ef_tag

let interpret_tailcall st f =
  let ptr = get_addr f st in
  match Mem.find_fun_def st.mem ptr with
    | LIN.F_int def ->
      init_fun_call st ptr
    | LIN.F_ext def ->
      let st = change_pc st (get_ra st) in
      interpret_external_call st def.AST.ef_tag

let interpret_return st = change_pc st (get_ra st)

let interpret_stmt st stmt =
  match stmt with

    | LIN.St_skip lbl ->
      goto st lbl

    | LIN.St_label _ ->
      next_pc st

    | LIN.St_comment s ->
(*
      Printf.printf "*** %s ***\n\n%!" s ;
*)
      next_pc st

    | LIN.St_cost cost_lbl ->
      let st = add_trace st cost_lbl in
      next_pc st

    | LIN.St_int (r, i) ->
      let st = add_reg r (Val.of_int i) st in
      next_pc st

    | LIN.St_addr (addr, x) ->
      let vs = Mem.find_global st.mem x in
      let st = add_regs addr vs st in
      next_pc st

    | LIN.St_unop (unop, destr, srcr) ->
      let v = Eval.unop unop (get_reg srcr st) in
      let st = add_reg destr v st in
      next_pc st

    | LIN.St_binop (binop, destr, srcr1, srcr2) ->
      let v = Eval.binop binop (get_reg srcr1 st) (get_reg srcr2 st) in
      let st = add_reg destr v st in
      next_pc st

    | LIN.St_load (size, destr, addr) ->
      let v = Mem.load st.mem size (get_addr addr st) in
      let st = add_reg destr v st in
      next_pc st

    | LIN.St_store (size, addr, srcr) ->
      let mem = Mem.store st.mem size (get_addr addr st) (get_reg srcr st) in
      let st = change_mem st mem in
      next_pc st

    | LIN.St_call f -> interpret_call st f

    | LIN.St_tailcall f -> interpret_tailcall st f

    | LIN.St_cond (r, lbl_true) ->
      let v = get_reg r st in
      if Val.is_true v then goto st lbl_true
      else
	if Val.is_false v then next_pc st
	else error "Undecidable branchment."

    | LIN.St_return -> interpret_return st


let compute_result st =
  let vs = get_result st in
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
  match fetch_stmt st with
    | LIN.St_return when Val.eq_address (get_ra st) st.exit ->
      print_and_return_result (compute_result st, List.rev st.trace)
    | stmt ->
      let st' = interpret_stmt st stmt in
      iter_small_step debug st'


let add_fun_defs =
  List.fold_left (fun mem (f_id, f_def) -> Mem.add_fun_def mem f_id f_def)

let init_prog (st : state) (p : LIN.program)
    : state * Val.address (* ra *) * Val.address (* gp *) * Val.address (* sp *)
    =
  let mem = add_fun_defs st.mem p.LIN.functs in
  let (mem, ra) = Mem.alloc mem 1 in
  let (mem, gp) = Mem.alloc mem p.LIN.globals in
  let (mem, sp) = Mem.alloc mem Driver.TargetArch.ram_size in
  let sp =
    Val.change_address_offset sp
      (Val.Offset.of_int Driver.TargetArch.ram_size) in
  let st = change_mem st mem in
  let st = change_exit st ra in
  (st, ra, gp, sp)

let init_renv st ra gp sp =
  let f r renv = Driver.TargetArch.RegisterMap.add r Val.undef renv in
  let renv =
    Driver.TargetArch.RegisterSet.fold f
      Driver.TargetArch.registers Driver.TargetArch.RegisterMap.empty in
  let st = change_renv st renv in
  let st = set_ra ra st in
  let st = set_gp gp st in
  let st = set_sp sp st in
  add_reg Driver.TargetArch.zero (Val.of_int 0) st

let init_main_call st main =
  let ptr = Mem.find_global st.mem main in
  match Mem.find_fun_def st.mem ptr with
    | LIN.F_int def ->
      init_fun_call st ptr
    | _ -> error ("Cannot execute the main (\"" ^ main ^ "\"): it is external.")


(* Before interpreting, the environment is initialized:
   - Add function definitions to the memory.
   - Create a dummy return address for the whole program.
   - Reserve space for the globals and initialize the global pointer.
   - Allocate memory to emulate the stack and initialize the stack pointer.
   - Initialiaze the physical register environment. All are set to an undefinied
     value, except for the return address, stack pointer and global pointer
     registers (already initialized), and the zero register set to 0.
   - Initialize a call to the main (set the current program counter to the
     beginning of the function). *)

let interpret debug p =
  Printf.printf "*** LIN interpret ***\n%!" ;
  match p.LIN.main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let st = empty_state in
      let (st, ra, gp, sp) = init_prog st p in
      let st = init_renv st ra gp sp in
      let st = init_main_call st main in
      iter_small_step debug st

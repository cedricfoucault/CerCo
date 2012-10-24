
(** This module provides an interpreter for the LTL language. *)


let error_prefix = "LTL interpret"
let error s = Error.global_error error_prefix s


module Mem = Driver.TargetArchMemory
module Val = Mem.Value
module Eval = Arch.Eval (Val)


(* Memory *)

type memory = LTL.function_def Mem.memory

(* Hardware registers environments. They associate a value to the each hardware
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


(* Each label of each function is associated a pointer. The base of this pointer
   is the base of the function in memory. Inside a function, offsets are
   bijectively associated to labels. *)

module Labels_Offsets = Bijection.Make (Label) (Val.Offset)

let labels_offsets_internal int_fun =
  let f lbl _ (lbls_offs, i) =
    (Labels_Offsets.add1 lbl i lbls_offs, Val.Offset.succ i) in
  Label.Map.fold f int_fun.LTL.f_graph

(* [labels_offsets p] builds a bijection between the labels found in the
   functions of [p] and some offsets. *)

let labels_offsets p =
  let f (lbls_offs, i) (_, def) = match def with
    | LTL.F_int int_fun -> labels_offsets_internal int_fun (lbls_offs, i)
    | _ -> (lbls_offs, i) in
  fst (List.fold_left f (Labels_Offsets.empty, Val.Offset.zero) p.LTL.functs)

let fun_def_of_ptr mem ptr = match Mem.find_fun_def mem ptr with
  | LTL.F_int def -> def
  | _ -> error "Trying to fetch the definition of an external function."

let fetch_stmt lbls_offs st =
  let msg =
    Printf.sprintf "%s does not point to a statement."
      (Val.string_of_address st.pc) in
  if Val.is_mem_address st.pc then
    let off = Val.offset_of_address st.pc in
    let def = fun_def_of_ptr st.mem st.pc in
    let lbl = Labels_Offsets.find2 off lbls_offs in
    Label.Map.find lbl def.LTL.f_graph
  else error msg

let entry_pc lbls_offs ptr def =
  Val.change_address_offset ptr (Labels_Offsets.find1 def.LTL.f_entry lbls_offs)

let init_fun_call lbls_offs st ptr def =
  let pc = entry_pc lbls_offs ptr def in
  change_pc st pc

let next_pc lbls_offs st lbl =
  let off = Labels_Offsets.find1 lbl lbls_offs in
  change_pc st (Val.change_address_offset st.pc off)

let framesize st =
  if Val.is_mem_address st.pc then
    let def = fun_def_of_ptr st.mem st.pc in
    def.LTL.f_stacksize
  else error "Trying to load the stack size of an external function."

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

let save_ra lbls_offs st lbl =
  let ra =
    Val.change_address_offset st.pc (Labels_Offsets.find1 lbl lbls_offs) in
  set_ra ra st

let label_of_pointer lbls_offs ptr =
(*
  Printf.printf "Retrieving label of %s\n%!" (Val.to_string ptr) ;
*)
  let off = Val.offset_of_address ptr in
  Labels_Offsets.find2 off lbls_offs

let pointer_of_label lbls_offs ptr lbl =
  Val.change_address_offset ptr (Labels_Offsets.find1 lbl lbls_offs)


(* State pretty-printing *)

let current_label lbls_offs st =
  Labels_Offsets.find2 (Val.offset_of_address st.pc) lbls_offs

let print_renv renv =
  let f r v =
    if not (Val.eq v Val.undef) then
    Printf.printf "\n%s = %s%!"
      (Driver.TargetArch.print_register r) (Val.to_string v) in
  Driver.TargetArch.RegisterMap.iter f renv

let print_state lbls_offs st =
  Printf.printf "PC: %s (%s)\n%!"
    (Val.string_of_address st.pc) (current_label lbls_offs st) ;
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

let interpret_external_call st f next_pc =
  let args = fetch_external_args f st in
  let (mem, vs) = interpret_external st.mem f args in
  let st = change_mem st mem in
  let st = set_result st vs in
  change_pc st next_pc

let interpret_call lbls_offs st f rlbl =
  let ptr = get_addr f st in
  match Mem.find_fun_def st.mem ptr with
    | LTL.F_int def ->
      let st = save_ra lbls_offs st rlbl in
      init_fun_call lbls_offs st ptr def
    | LTL.F_ext def ->
      let next_pc = 
	Val.change_address_offset st.pc (Labels_Offsets.find1 rlbl lbls_offs) in
      interpret_external_call st def.AST.ef_tag next_pc

let interpret_tailcall lbls_offs st f =
  let ptr = get_addr f st in
  match Mem.find_fun_def st.mem ptr with
    | LTL.F_int def ->
      init_fun_call lbls_offs st ptr def
    | LTL.F_ext def ->
      let next_pc = get_ra st in
      interpret_external_call st def.AST.ef_tag next_pc

let interpret_return lbls_offs st =
  let pc = get_addr Driver.TargetArch.ra st in
  change_pc st pc


(* Interpret statements. *)

let interpret_stmt lbls_offs st stmt =
  let next_pc = next_pc lbls_offs in
  match stmt with

    | LTL.St_skip lbl ->
      next_pc st lbl

    | LTL.St_comment (s, lbl) ->
(*
      Printf.printf "*** %s ***\n\n%!" s ;
*)
      next_pc st lbl

    | LTL.St_cost (cost_lbl, lbl) ->
      let st = add_trace st cost_lbl in
      next_pc st lbl

    | LTL.St_int (r, i, lbl) ->
      let st = add_reg r (Val.of_int i) st in
      next_pc st lbl

    | LTL.St_addr (addr, x, lbl) ->
      let vs = Mem.find_global st.mem x in
      let st = add_regs addr vs st in
      next_pc st lbl

    | LTL.St_unop (unop, destr, srcr, lbl) ->
      let v = Eval.unop unop (get_reg srcr st) in
      let st = add_reg destr v st in
      next_pc st lbl

    | LTL.St_binop (binop, destr, srcr1, srcr2, lbl) ->
      let v = Eval.binop binop (get_reg srcr1 st) (get_reg srcr2 st) in
      let st = add_reg destr v st in
      next_pc st lbl

    | LTL.St_load (size, r, addr, lbl) ->
      let v = Mem.load st.mem size (get_addr addr st) in
      let st = add_reg r v st in
      next_pc st lbl

    | LTL.St_store (size, addr, r, lbl) ->
      let mem = Mem.store st.mem size (get_addr addr st) (get_reg r st) in
      let st = change_mem st mem in
      next_pc st lbl

    | LTL.St_call (f, lbl) ->
      interpret_call lbls_offs st f lbl

    | LTL.St_tailcall f ->
      interpret_tailcall lbls_offs st f

    | LTL.St_cond (r, lbl_true, lbl_false) ->
      let v = get_reg r st in
      let lbl =
	if Val.is_true v then lbl_true
	else
	  if Val.is_false v then lbl_false
	  else error "Undecidable branchment." in
      next_pc st lbl

    | LTL.St_return ->
      interpret_return lbls_offs st


let compute_result st =
  let vs = get_result st in
  let f res v = res && (Val.is_int v) in
  let is_int vs = (List.length vs > 0) && (List.fold_left f true vs) in
  if is_int vs then
    let chunks =
      List.map (fun v -> IntValue.Int32.cast (Val.to_int_repr v)) vs in
    IntValue.Int32.merge chunks
  else IntValue.Int32.zero

let rec iter_small_step debug lbls_offs st =
  let print_and_return_result (res, cost_labels) =
    if debug then Printf.printf "Result = %s\n%!"
      (IntValue.Int32.to_string res) ;
    (res, cost_labels) in
  if debug then print_state lbls_offs st ;
  match fetch_stmt lbls_offs st with
    | LTL.St_return when Val.eq_address (get_ra st) st.exit ->
      print_and_return_result (compute_result st, List.rev st.trace)
    | stmt ->
      let st' = interpret_stmt lbls_offs st stmt in
      iter_small_step debug lbls_offs st'


let add_fun_defs =
  List.fold_left (fun mem (f_id, f_def) -> Mem.add_fun_def mem f_id f_def)

let init_prog (st : state) (p : LTL.program)
    : state * Val.address (* ra *) * Val.address (* gp *) * Val.address (* sp *)
    =
  let mem = add_fun_defs st.mem p.LTL.functs in
  let (mem, ra) = Mem.alloc mem 1 in
  let (mem, gp) = Mem.alloc mem p.LTL.globals in
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
  let st = add_reg Driver.TargetArch.zero (Val.of_int 0) st in
  set_sp sp st

let init_main_call lbls_offs st main =
  let ptr = Mem.find_global st.mem main in
  match Mem.find_fun_def st.mem ptr with
    | LTL.F_int def ->
      init_fun_call lbls_offs st ptr def
    | _ -> error ("Cannot execute the main (\"" ^ main ^ "\"): it is external.")


(* Before interpreting, the environment is initialized:
   - Build a bijection between the labels in the program and some offset values.
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
  Printf.printf "*** LTL interpret ***\n%!" ;
  match p.LTL.main with
    | None -> (IntValue.Int8.zero, [])
    | Some main ->
      let lbls_offs = labels_offsets p in
      let st = empty_state in
      let (st, ra, gp, sp) = init_prog st p in
      let st = init_renv st ra gp sp in
      let st = init_main_call lbls_offs st main in
      iter_small_step debug lbls_offs st

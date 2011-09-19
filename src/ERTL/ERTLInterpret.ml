
(** This module provides an interpreter for the ERTL language. *)


let error_prefix = "ERTL interpret"
let error s = Error.global_error error_prefix s


module Mem = Driver.TargetArchMemory
module Val = Mem.Value
module Eval = Arch.Eval (Val)


(* Memory *)

type memory = ERTL.function_def Mem.memory

(* Local environments. They associate a value to the pseudo-registers of the
   function being executed. *)

type local_env = Val.t Register.Map.t

(* Hardware registers environments. They associate a value to each hardware
   register. *)

type hdw_reg_env = Val.t Driver.TargetArch.RegisterMap.t

(* Call frames. The execution state has a call stack, each element of the stack
   being composed of the local environment to resume the execution of the
   caller. *)

type stack_frame = local_env

(* Execution states. *)

type state =
    { st_frs : stack_frame list ;
      pc     : Val.address ;
      exit   : Val.address ;
      lenv   : local_env ;
      renv   : hdw_reg_env ;
      mem    : memory ;
      trace  : CostLabel.t list }


(* Helpers *)

let add_st_frs st frame = { st with st_frs = frame :: st.st_frs }
let pop_st_frs st = match st.st_frs with
  | [] -> error "Empty stack frames."
  | lenv :: st_frs -> { st with st_frs = st_frs ; lenv = lenv }
let change_pc st pc = { st with pc = pc }
let change_exit st exit = { st with exit = exit }
let change_lenv st lenv = { st with lenv = lenv }
let change_renv st renv = { st with renv = renv }
let change_mem st mem = { st with mem = mem }
let change_trace st trace = { st with trace = trace }
let add_trace st cost_lbl = change_trace st (cost_lbl :: st.trace)

let empty_state =
  { st_frs = [] ;
    pc     = Val.null ;
    exit   = Val.null ;
    lenv   = Register.Map.empty ;
    renv   = Driver.TargetArch.RegisterMap.empty ;
    mem    = Mem.empty ;
    trace  = [] }


(* Each label of each function is associated an address. The base of this
   address is the base of the function in memory. Inside a function, offsets are
   bijectively associated to labels. *)

module Labels_Offsets = Bijection.Make (Label) (Val.Offset)

let labels_offsets_internal int_fun =
  let f lbl _ (lbls_offs, i) =
    (Labels_Offsets.add1 lbl i lbls_offs, Val.Offset.succ i) in
  Label.Map.fold f int_fun.ERTL.f_graph

(* [labels_offsets p] builds a bijection between the labels found in the
   functions of [p] and some memory addresses. *)

let labels_offsets p =
  let f (lbls_offs, i) (_, def) = match def with
    | ERTL.F_int int_fun -> labels_offsets_internal int_fun (lbls_offs, i)
    | _ -> (lbls_offs, i) in
  fst (List.fold_left f (Labels_Offsets.empty, Val.Offset.zero) p.ERTL.functs)

let fun_def_of_ptr mem ptr = match Mem.find_fun_def mem ptr with
  | ERTL.F_int def -> def
  | _ -> error "Trying to fetch the definition of an external function."

let fetch_stmt lbls_offs st =
  let msg =
    Printf.sprintf "%s does not point to a statement."
      (Val.string_of_address st.pc) in
  if Val.is_mem_address st.pc then
    let off = Val.offset_of_address st.pc in
    let def = fun_def_of_ptr st.mem st.pc in
    let lbl = Labels_Offsets.find2 off lbls_offs in
    Label.Map.find lbl def.ERTL.f_graph
  else error msg

let entry_pc lbls_offs ptr def =
  let off = Labels_Offsets.find1 def.ERTL.f_entry lbls_offs in
  Val.change_address_offset ptr off

let init_fun_call lbls_offs st ptr def =
  let f r lenv = Register.Map.add r Val.undef lenv in
  let lenv = Register.Set.fold f def.ERTL.f_locals Register.Map.empty in
  let pc = entry_pc lbls_offs ptr def in
  change_lenv (change_pc st pc) lenv

let next_pc lbls_offs st lbl =
  let off = Labels_Offsets.find1 lbl lbls_offs in
  change_pc st (Val.change_address_offset st.pc off)

let framesize st =
  if Val.is_mem_address st.pc then
    let def = fun_def_of_ptr st.mem st.pc in
    def.ERTL.f_stacksize
  else error "No function at the given address."

type register = Hdw of Driver.TargetArch.register | Psd of Register.t

let add_reg r v st = match r with
  | Hdw r ->
    let renv = Driver.TargetArch.RegisterMap.add r v st.renv in
    change_renv st renv
  | Psd r ->
    let lenv = Register.Map.add r v st.lenv in
    change_lenv st lenv

let make_hdws l = List.map (fun r -> Hdw r) l

let make_psds l = List.map (fun r -> Psd r) l

let add_regs rs vs st =
  assert (List.length rs = List.length vs) ;
  let f st r v = add_reg r v st in
  List.fold_left2 f st rs vs

let add_hdw_regs rs vs st = add_regs (make_hdws rs) vs st

let get_reg r st = match r with
  | Hdw r ->
    if Driver.TargetArch.RegisterMap.mem r st.renv then
      Driver.TargetArch.RegisterMap.find r st.renv
    else
      error ("Unknown hardware register " ^
	     (Driver.TargetArch.print_register r) ^ ".")
  | Psd r ->
    if Register.Map.mem r st.lenv then Register.Map.find r st.lenv
    else error ("Unknown local register " ^ (Register.print r) ^ ".")

let save_frame st = add_st_frs st st.lenv

let label_of_pointer lbls_offs ptr =
(*
  Printf.printf "Retrieving label of %s\n%!" (Val.to_string ptr) ;
*)
  let off = Val.offset_of_address ptr in
  Labels_Offsets.find2 off lbls_offs

let pointer_of_label lbls_offs ptr lbl =
  Val.change_address_offset ptr (Labels_Offsets.find1 lbl lbls_offs)

let get_addr rs st = List.map (fun r -> get_reg r st) rs

let get_psd_addr rs = get_addr (make_psds rs)

let get_hdw_addr rs = get_addr (make_hdws rs)

let get_ra = get_hdw_addr Driver.TargetArch.ra

let get_gp = get_hdw_addr Driver.TargetArch.gp

let get_sp = get_hdw_addr Driver.TargetArch.sp

let get_result = get_hdw_addr Driver.TargetArch.result

let set_ra = add_hdw_regs Driver.TargetArch.ra

let set_gp = add_hdw_regs Driver.TargetArch.gp

let set_sp = add_hdw_regs Driver.TargetArch.sp

let save_ra lbls_offs st lbl =
  let ra =
    Val.change_address_offset st.pc (Labels_Offsets.find1 lbl lbls_offs) in
  set_ra ra st


module InterpretExternal = Primitive.Interpret (Mem)

let interpret_external mem f args = match InterpretExternal.t mem f args with
  | (mem', InterpretExternal.V vs) -> (mem', vs)
  | (mem', InterpretExternal.A addr) -> (mem', addr)

let external_hdw_args size st =
  let params = MiscPottier.prefix size Driver.TargetArch.parameters in
  List.map (fun r -> get_reg (Hdw r) st) params

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
  let f st (r, v) = add_reg (Hdw r) v st in
  List.fold_left f st (MiscPottier.combine Driver.TargetArch.result vs)

let interpret_external_call st f next_pc =
  let args = fetch_external_args f st in
  let (mem, vs) = interpret_external st.mem f args in
  let st = change_mem st mem in
  let st = set_result st vs in
  change_pc st next_pc

let interpret_call lbls_offs st f rlbl =
  let ptr = get_psd_addr f st in
  match Mem.find_fun_def st.mem ptr with
    | ERTL.F_int def ->
      let st = save_frame (save_ra lbls_offs st rlbl) in
      init_fun_call lbls_offs st ptr def
    | ERTL.F_ext def ->
      let next_pc = 
	Val.change_address_offset st.pc (Labels_Offsets.find1 rlbl lbls_offs) in
      interpret_external_call st def.AST.ef_tag next_pc

let interpret_tailcall lbls_offs st f =
  let ptr = get_psd_addr f st in
  let size = framesize st in
  let sp = get_sp st in
  let new_sp = Val.add_address sp (Val.Offset.of_int size) in
  let st = set_sp new_sp st in
  let st = pop_st_frs st in
  match Mem.find_fun_def st.mem ptr with
    | ERTL.F_int def ->
      init_fun_call lbls_offs st ptr def
    | ERTL.F_ext def ->
      let next_pc = get_ra st in
      interpret_external_call st def.AST.ef_tag next_pc

let interpret_return lbls_offs st =
  let st = pop_st_frs st in
  let pc = get_hdw_addr Driver.TargetArch.ra st in
  change_pc st pc


(* Interpret statements. *)

let interpret_stmt lbls_offs st stmt =
  let next_pc = next_pc lbls_offs in
  match stmt with

    | ERTL.St_skip lbl ->
      next_pc st lbl

    | ERTL.St_comment (s, lbl) ->
      (* Printf.printf "*** %s ***\n\n%!" s ; *)
      next_pc st lbl

    | ERTL.St_cost (cost_lbl, lbl) ->
      let st = add_trace st cost_lbl in
      next_pc st lbl

    | ERTL.St_newframe lbl ->
      let size = framesize st in
      let sp = get_sp st in
      let new_sp = Val.add_address sp (Val.Offset.of_int (-size)) in
      let st = set_sp new_sp st in
      next_pc st lbl

    | ERTL.St_delframe lbl ->
      let size = framesize st in
      let sp = get_sp st in
      let new_sp = Val.add_address sp (Val.Offset.of_int size) in
      let st = set_sp new_sp st in
      next_pc st lbl

    | ERTL.St_framesize (destr, lbl) ->
      let size = framesize st in
      let st = add_reg (Psd destr) (Val.of_int size) st in
      next_pc st lbl

    | ERTL.St_get_hdw (destr, srcr, lbl) ->
      let st = add_reg (Psd destr) (get_reg (Hdw srcr) st) st in
      next_pc st lbl

    | ERTL.St_set_hdw (destr, srcr, lbl) ->
      let st = add_reg (Hdw destr) (get_reg (Psd srcr) st) st in
      next_pc st lbl

    | ERTL.St_hdw_to_hdw (destr, srcr, lbl) ->
      let st = add_reg (Hdw destr) (get_reg (Hdw srcr) st) st in
      next_pc st lbl

    | ERTL.St_move (destr, srcr, lbl) ->
      let st = add_reg (Psd destr) (get_reg (Psd srcr) st) st in
      next_pc st lbl

    | ERTL.St_int (r, i, lbl) ->
      let st = add_reg (Psd r) (Val.of_int i) st in
      next_pc st lbl

    | ERTL.St_addrN (r, x, n, lbl) ->
      let vs = Mem.find_global st.mem x in
      assert (List.length vs > n) ;
      let st = add_reg (Psd r) (List.nth vs n) st in
      next_pc st lbl

    | ERTL.St_unop (unop, destr, srcr, lbl) ->
      let v = Eval.unop unop (get_reg (Psd srcr) st) in
      let st = add_reg (Psd destr) v st in
      next_pc st lbl

    | ERTL.St_binop (binop, destr, srcr1, srcr2, lbl) ->
      let v =
	Eval.binop binop (get_reg (Psd srcr1) st) (get_reg (Psd srcr2) st) in
      let st = add_reg (Psd destr) v st in
      next_pc st lbl

    | ERTL.St_load (size, destr, addr, lbl) ->
      let addr = get_psd_addr addr st in
      let v = Mem.load st.mem size addr in
      let st = add_reg (Psd destr) v st in
      next_pc st lbl

    | ERTL.St_store (size, addr, srcr, lbl) ->
      let addr = get_psd_addr addr st in
      let mem = Mem.store st.mem size addr (get_reg (Psd srcr) st) in
      let st = change_mem st mem in
      next_pc st lbl

    | ERTL.St_call (f, _, lbl) ->
      interpret_call lbls_offs st f lbl

    | ERTL.St_tailcall (f, _) ->
      interpret_tailcall lbls_offs st f

    | ERTL.St_cond (srcr, lbl_true, lbl_false) ->
      let v = get_reg (Psd srcr) st in
      let lbl =
	if Val.is_true v then lbl_true
	else
	  if Val.is_false v then lbl_false
	  else error "Undecidable branchment." in
      next_pc st lbl

    | ERTL.St_return _ ->
      interpret_return lbls_offs st


let print_lenv lenv =
  let f r v =
    if not (Val.eq v Val.undef) then
      Printf.printf "\n%s = %s%!" (Register.print r) (Val.to_string v) in
  Register.Map.iter f lenv

let print_renv renv =
  let f r v =
    if not (Val.eq v Val.undef) then
    Printf.printf "\n%s = %s%!"
      (Driver.TargetArch.print_register r)
      (Val.to_string v) in
  Driver.TargetArch.RegisterMap.iter f renv

let current_label lbls_offs st =
  Labels_Offsets.find2 (Val.offset_of_address st.pc) lbls_offs

let print_state lbls_offs st =
  Printf.printf "EXIT: %s\n%!" (Val.string_of_address st.exit) ;
  Printf.printf "PC: %s (%s)\n%!"
    (Val.string_of_address st.pc) (current_label lbls_offs st) ;
  print_lenv st.lenv ;
  print_renv st.renv ;
  Mem.print st.mem ;
  Printf.printf "\n%!"

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
    | ERTL.St_return _ when Val.eq_address (get_ra st) st.exit ->
      print_and_return_result (compute_result st, List.rev st.trace)
    | stmt ->
      let st' = interpret_stmt lbls_offs st stmt in
      iter_small_step debug lbls_offs st'


let add_fun_defs =
  List.fold_left (fun mem (f_id, f_def) -> Mem.add_fun_def mem f_id f_def)

let init_prog (st : state) (p : ERTL.program)
    : state * Val.address (* ra *) * Val.address (* gp *) * Val.address (* sp *)
    =
  let mem = add_fun_defs st.mem p.ERTL.functs in
  let (mem, ra) = Mem.alloc mem 1 in
  let (mem, gp) = Mem.alloc mem p.ERTL.globals in
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
  set_sp sp st

let init_main_call lbls_offs st main =
  let ptr = Mem.find_global st.mem main in
  match Mem.find_fun_def st.mem ptr with
    | ERTL.F_int def ->
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
     registers (already initialized).
   - Initialize a call to the main (set the current program counter to the
     beginning of the function). *)

let interpret debug p =
  Printf.printf "*** ERTL interpret ***\n%!" ;
  match p.ERTL.main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let lbls_offs = labels_offsets p in
      let st = empty_state in
      let (st, ra, gp, sp) = init_prog st p in
      let st = init_renv st ra gp sp in
      let st = init_main_call lbls_offs st main in
      iter_small_step debug lbls_offs st

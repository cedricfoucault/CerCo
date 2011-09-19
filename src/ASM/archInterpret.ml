
(** This module provides a function to interpret a [Arch] program and
    return the trace of cost labels encountered. *)


let error_prefix = "Arch interpret"
let error s = Error.global_error error_prefix s


module Make (A : Arch.ARCH) = struct

  module Mem = Memory.Make (A)
  module Val = Mem.Value
  module Eval = Arch.Eval (Val)

  (* Memory *)

  type memory = AST.external_function Mem.memory

  (* Hardware registers environments. They associate a value to each hardware
     register. *)

  type hdw_reg_env = Val.t A.RegisterMap.t

  (* Execution states. *)

  type state =
      { (* The PC is the offset of the instruction in the code (list of
	   instructions) *)
	pc     : Val.address ;
	exit   : Val.address ;
	renv   : hdw_reg_env ;
	mem    : memory ;
	code   : A.register Arch.generic_instructions ;
	trace  : CostLabel.t list }


  (* Helpers *)

  let set_pc st pc = { st with pc = pc }
  let set_exit st exit = { st with exit = exit }
  let set_renv st renv = { st with renv = renv }
  let set_mem st mem = { st with mem = mem }
  let set_code st code = { st with code = code }
  let set_trace st trace = { st with trace = trace }
  let add_trace st cost_lbl = set_trace st (cost_lbl :: st.trace)

  let empty_state =
    { pc     = Val.null ;
      exit   = Val.null ;
      renv   = A.RegisterMap.empty ;
      mem    = Mem.empty ;
      code   = [] ;
      trace  = [] }


  let get_current_pc st =
    let error_msg =
      Printf.sprintf "%s is not a valid instruction address."
	(Val.string_of_address st.pc) in
    if List.length st.pc = 1 && Val.is_int (List.hd st.pc) &&
      List.length st.code > Val.to_int (List.hd st.pc) then
      List.hd st.pc
    else error error_msg

  let fetch_instruction st = List.nth st.code (Val.to_int (get_current_pc st))

  let next_pc st = Val.add (get_current_pc st) (Val.of_int 1)

  let set_next_pc st = set_pc st [next_pc st]

  let add_reg r v st =
    let renv = A.RegisterMap.add r v st.renv in
    set_renv st renv

  let add_regs rs vs st =
    assert (List.length rs = List.length vs) ;
    let f st r v = add_reg r v st in
    List.fold_left2 f st rs vs

  let set_ra = add_regs A.ra

  let set_gp = add_regs A.gp

  let set_sp = add_regs A.sp

  let get_reg r st =
    if A.RegisterMap.mem r st.renv then A.RegisterMap.find r st.renv
    else error ("Unknown hardware register " ^ (A.print_register r) ^ ".")

  let get_regs rs st = List.map (fun r -> get_reg r st) rs

  let get_result = get_regs A.result

  let return_pc st = get_regs A.ra st

  let label_index lbl code =
    let rec aux i = function
      | [] -> error ("unknown label " ^ lbl)
      | Arch.ILabel lbl' :: _ when lbl' = lbl -> i
      | _ :: code -> aux (i+1) code in
    Val.of_int (aux 0 code)

  let goto st lbl = set_pc st [label_index lbl st.code]

  let branch_on_val st v lbl_true =
    if Val.is_true v then goto st lbl_true
    else
      if Val.is_false v then set_next_pc st
      else error "Undecidable branchment."

  let save_ra st = add_regs A.ra [next_pc st] st

  let get_sp st = get_regs A.sp st

  let label_address st lbl =
    if List.mem (Arch.ILabel lbl) st.code then
      [label_index lbl st.code]
    else
      if Mem.mem_global st.mem lbl then Mem.find_global st.mem lbl
      else error ("unknown label " ^ lbl)


  (* State pretty-printing *)

  let print_renv renv =
    let f r v =
      if not (Val.eq v Val.undef) then
	Printf.printf "\n%s = %s%!" (A.print_register r) (Val.to_string v) in
    A.RegisterMap.iter f renv

  let print_state st =
    Printf.printf "PC: %s\n%!" (Val.string_of_address st.pc) ;
    print_renv st.renv ;
    Mem.print st.mem ;
    Printf.printf "\n%!"


  (* Interpretation *)

  module InterpretExternal = Primitive.Interpret (Mem)

  let interpret_external mem f args = match InterpretExternal.t mem f args with
    | (mem', InterpretExternal.V vs) -> (mem', vs)
    | (mem', InterpretExternal.A addr) -> (mem', addr)

  let external_hdw_args size st =
    let params = MiscPottier.prefix size A.parameters in
    List.map (fun r -> get_reg r st) params

  let external_stack_args size st =
    let sp = get_sp st in
    let arg_offset arg_number = (arg_number + 1) * A.int_size in
    let arg_addr arg_number =
      Val.add_address sp (Val.Offset.of_int (-(arg_offset arg_number))) in
    let fetch_arg arg_number =
      Mem.load st.mem A.int_size (arg_addr arg_number) in
    let args_number = MiscPottier.make_sequence ((+) 1) 0 size in
    List.map fetch_arg args_number

  let fetch_external_args f st =
    let args_quantity = Primitive.args_quantity f in
    let arg_reg_size quantity =
      MiscPottier.div_up (Mem.size_of_quantity quantity) A.int_size in
    let size =
      List.fold_left (+) 0 (List.map arg_reg_size args_quantity) in
    let hdw_params = List.length A.parameters in
    let hdw_nb_params = min size hdw_params in
    let stack_nb_params = max 0 (size - hdw_params) in
    let params1 = external_hdw_args hdw_nb_params st in
    let params2 = external_stack_args stack_nb_params st in
    params1 @ params2

  let set_result st vs =
    let f st (r, v) = add_reg r v st in
    List.fold_left f st (MiscPottier.combine A.result vs)

  let interpret_external_call st f =
    let args = fetch_external_args f st in
    let (mem, vs) = interpret_external st.mem f args in
    let st = set_mem st mem in
    let st = set_result st vs in
    set_next_pc st

  let interpret_call st vs =
    if List.length vs = 1 && Val.is_int (List.hd vs) then
      (* internal call *)
      set_pc (save_ra st) vs
    else
      (* external call *)
      interpret_external_call st (Mem.find_fun_def st.mem vs).AST.ef_tag

  let interpret_instruction st = function

    | Arch.IComment (b, s) ->
      (* Printf.printf "%s*** %s ***\n\n" (if b then "\n" else "") s ; *)
      set_next_pc st

    | Arch.INop -> set_next_pc st
      
    | Arch.IConst (r, i) ->
      let st = add_reg r (Val.of_int i) st in
      set_next_pc st

    | Arch.IUnOp (unop, dst_reg, src_reg) ->
      let v = Eval.unop unop (get_reg src_reg st) in
      let st = add_reg dst_reg v st in
      set_next_pc st

    | Arch.IBinOp (binop, dst_reg, src_reg1, src_reg2) ->
      let v = Eval.binop binop (get_reg src_reg1 st) (get_reg src_reg2 st) in
      let st = add_reg dst_reg v st in
      set_next_pc st

    | Arch.ILoadAddr (rs, lbl) ->
      let st = add_regs rs (label_address st lbl) st in
      set_next_pc st

    | Arch.ICall rs -> interpret_call st (get_regs rs st)

    | Arch.ILoad (size, dst, addr) ->
      let addr = get_regs addr st in
      let v = Mem.load st.mem (Arch.byte_size_of_data_size size) addr in
      let st = add_reg dst v st in
      set_next_pc st

    | Arch.IStore (size, addr, src) ->
      let addr = get_regs addr st in
      let mem =
	Mem.store st.mem (Arch.byte_size_of_data_size size) addr
	  (get_reg src st) in
      let st = set_mem st mem in
      set_next_pc st

    | Arch.IGoto lbl ->
      goto st lbl

    | Arch.IGotor addr -> set_pc st (get_regs addr st)

    | Arch.IBranch (r, lbl_true) ->
      let v = get_reg r st in
      branch_on_val st v lbl_true

    | Arch.IReturn -> set_pc st (return_pc st)

    | Arch.ISyscall -> assert false (* should not happen *)

    | Arch.ILabel _ -> set_next_pc st

    | Arch.ICost cost_lbl ->
      let st = add_trace st cost_lbl in
      set_next_pc st


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
    match fetch_instruction st with
      | Arch.IReturn when Val.eq_address (return_pc st) st.exit ->
	print_and_return_result (compute_result st, List.rev st.trace)
      | instr ->
	let st' = interpret_instruction st instr in
	iter_small_step debug st'


    let init_externals mem p =
    let f mem ext = Mem.add_fun_def mem ext.AST.ef_tag ext in
    List.fold_left f mem p.Arch.externals

    let init_prog st p =
      let st = set_code st p.Arch.code in
      let mem = init_externals st.mem p in
      let (mem, ra) = Mem.alloc mem A.ptr_size in
      let (mem, gp) = Mem.alloc mem p.Arch.globals in
      let (mem, sp) = Mem.alloc mem A.ram_size in
      let sp = Val.change_address_offset sp (Val.Offset.of_int A.ram_size) in
      let st = set_mem st mem in
      let st = set_exit st ra in
      (st, ra, gp, sp)

    let init_renv st ra gp sp =
      let f r renv = A.RegisterMap.add r Val.undef renv in
      let renv = A.RegisterSet.fold f A.registers A.RegisterMap.empty in
      let st = set_renv st renv in
      let st = set_ra ra st in
      let st = set_gp gp st in
      let st = set_sp sp st in
      add_reg A.zero (Val.of_int 0) st

    let init_main_call st main = goto st main


  (* Before interpreting, the environment is initialized:
    - Allocate space for the globals and initialize the global pointer.
    - Allocate the stack and initialize the stack pointer.
    - Allocate a dummy return address of the whole program.
    - Initialiaze the physical register environment. All are set to an
      undefinied value, except for the return address, stack pointer and
      global pointer registers (already initialized), and the zero register set
      to 0.
    - Set the current program counter at the entry of the main function (if
      any.) *)

  let interpret debug p =
    Printf.printf "*** Arch interpret ***\n%!" ;
    match p.Arch.main with
      | None -> (IntValue.Int32.zero, [])
      | Some main ->
	let st = empty_state in
	let (st, ra, gp, sp) = init_prog st p in
	let st = init_renv st ra gp sp in
	let st = init_main_call st main in
	iter_small_step debug st

end

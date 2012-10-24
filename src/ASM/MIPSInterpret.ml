
(** This module provides a function to interpret a [MIPS] program and
    return the trace of cost labels encountered. *)


let error_prefix = "MIPS interpret"
let error s = Error.global_error error_prefix s


module Mem = Memory.Make (MIPS)
module Val = Mem.Value
module Eval = Arch.Eval (Val)


(* Memory *)

type memory = AST.external_function Mem.memory

(* Hardware registers environments. They associate a value to each hardware
   register. *)

type hdw_reg_env = Val.t MIPS.RegisterMap.t

(* Execution states. *)

type state =
    { (* The PC is the offset of the instruction in the code (list of
	 instructions) *)
      pc     : Val.address ;
      exit   : Val.address ;
      renv   : hdw_reg_env ;
      mem    : memory ;
      code   : (MIPS.register, MIPS.address) Arch.instructions ;
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
  { pc     = [Val.zero] ;
    exit   = Val.null ;
    renv   = MIPS.RegisterMap.empty ;
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
  let renv = MIPS.RegisterMap.add r v st.renv in
  set_renv st renv

let add_regs rs vs st =
  if List.length rs <> List.length vs then
    error "incompatible number of registers and associated values."
  else
    let f st r v = add_reg r v st in
    List.fold_left2 f st rs vs

let get_reg r st =
  if MIPS.RegisterMap.mem r st.renv then MIPS.RegisterMap.find r st.renv
  else error ("Unknown hardware register " ^ (MIPS.print_register r) ^ ".")

let get_regs rs st = List.map (fun r -> get_reg r st) rs

let return_pc st = get_regs MIPS.ra st

let label_index lbl code =
  let rec aux i = function
    | [] -> error ("unknown label " ^ lbl)
    | Arch.ILabel lbl' :: _ when lbl' = lbl -> i
    | _ :: code -> aux (i+1) code in
  Val.of_int (aux 0 code)

let goto st lbl = set_pc st [label_index lbl st.code]

let branch_on_reg st v lbl_true =
  if Val.is_true v then goto st lbl_true
  else
    if Val.is_false v then set_next_pc st
    else error "Undecidable branchment."

let save_ra st = add_regs MIPS.ra [next_pc st] st


(* State pretty-printing *)

let print_renv renv =
  let f r v =
    if not (Val.eq v Val.undef) then
    Printf.printf "\n%s = %s%!" (MIPS.print_register r) (Val.to_string v) in
  MIPS.RegisterMap.iter f renv

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

let fetch_external_args f st =
  let size = Primitive.nb_args f in
  let params = MiscPottier.prefix size MIPS.parameters in
  List.map (fun r -> get_reg r st) params

let set_result st vs =
  let f st (r, v) = add_reg r v st in
  List.fold_left f st (MiscPottier.combine MIPS.result vs)

let interpret_external_call st f =
  let args = fetch_external_args f st in
  let (mem, vs) = interpret_external st.mem f args in
  let st = set_mem st mem in
  set_result st vs

let interpret_call st vs =
  if List.length vs = 1 && Val.is_int (List.hd vs) then
    (* internal call *)
    set_pc (save_ra st) vs
  else
    (* external call *)
    interpret_external_call st (Mem.find_fun_def st.mem vs).AST.ef_tag

let interpret_instruction st = function

  | Arch.IComment (b, s) ->
    Printf.printf "%s*** %s ***\n\n" (if b then "\n" else "") s ;
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
    let v = label_index lbl st.code in
    let st = add_regs rs [v] st in
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
      Mem.store st.mem (Arch.byte_size_of_data_size size) addr (get_reg src st)
    in
    let st = set_mem st mem in
    set_next_pc st

  | Arch.IGoto lbl ->
    goto st lbl

  | Arch.IGotor r -> set_pc st (get_regs r st)

  | Arch.IBranch (r, lbl_true) ->
    let v = get_reg r st in
    branch_on_reg st v lbl_true

(*
  | Arch.IUnBranch (uncon, r, lbl_true) ->
    let v = Eval.uncon uncon (get_reg r st) in
    branch_on_reg st v lbl_true

  | Arch.IBinBranch (bincon, r1, r2, lbl_true) ->
    let v = Eval.bincon bincon (get_reg r1 st) (get_reg r2 st) in
    branch_on_reg st v lbl_true
*)

  | Arch.IReturn -> set_pc st (return_pc st)

  | Arch.ILabel _ -> set_next_pc st

  | Arch.ICost cost_lbl ->
    let st = add_trace st cost_lbl in
    set_next_pc st


let compute_result st =
  let vs = get_regs MIPS.result st in
  if List.length vs > 0 && Val.is_int (List.hd vs) then
    IntValue.Int32.cast (Val.to_int_repr (List.hd vs))
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


let init_externals st p =
  let f mem ext = Mem.add_fun_def mem ext.AST.ef_tag ext in
  let mem = List.fold_left f st.mem p.Arch.externals in
  set_mem st mem

let init_sp st =
  let (mem, sp) = Mem.alloc st.mem MIPS.ram_size in
  let sp = Val.change_address_offset sp (Val.Offset.of_int MIPS.ram_size) in
  let st = set_mem st mem in
  (st, sp)

let init_gp st p =
  let (mem, gp) = Mem.alloc st.mem p.Arch.globals in
  let st = set_mem st mem in
  (st, gp)

let init_ra st =
  let (mem, ra) = Mem.alloc st.mem MIPS.ptr_size in
  let st = set_mem st mem in
  let st = set_exit st ra in
  (st, ra)

let init_renv st sp gp ra =
  let f r st = add_reg r Val.undef st in
  let st = MIPS.RegisterSet.fold f MIPS.registers st in
  let st = add_regs MIPS.sp sp st in
  let st = add_regs MIPS.gp gp st in
  let st = add_regs MIPS.ra ra st in
  st


(* Before interpreting, the environment is initialized:
   - Allocate the stack and initialize the stack pointer.
   - Allocate space for the globals and initialize the global pointer.
   - Allocate a dummy return address of the whole program.
   - Initialiaze the physical register environment. All are set to 0, except for
     the stack pointer, the global pointer and the return address register that
     are initialized following previous initializations. *)

let interpret debug p =
  Printf.printf "*** MIPS interpret ***\n%!" ;
  match p.Arch.main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let st = empty_state in
      let st = init_externals st p in
      let (st, sp) = init_sp st in
      let (st, gp) = init_gp st p in
      let (st, ra) = init_ra st in
      let st = init_renv st sp gp ra in
      let st = set_code st p.Arch.code in
      iter_small_step debug st

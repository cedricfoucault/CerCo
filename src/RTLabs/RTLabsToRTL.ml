
(** This module provides a translation of [RTLabs] programs to [RTL]
    programs. *)


let error_prefix = "RTLabs to RTL"
let error = Error.global_error error_prefix

let error_float () = error "float not supported."


(* Helpers *)

let add_graph lbl stmt def =
  { def with RTL.f_graph = Label.Map.add lbl stmt def.RTL.f_graph }

let fresh_label def = Label.Gen.fresh def.RTL.f_luniverse

let fresh_reg def =
  let r = Register.fresh def.RTL.f_runiverse in
  let locals = Register.Set.add r def.RTL.f_locals in
  ({ def with RTL.f_locals = locals }, r)

let rec fresh_regs def n =
  if n = 0 then (def, [])
  else
    let (def, res) = fresh_regs def (n-1) in
    let (def, r) = fresh_reg def in
    (def, r :: res)

let rec register_freshes runiverse n =
  if n <= 0 then []
  else (Register.fresh runiverse) :: (register_freshes runiverse (n-1))

let choose_rest rest1 rest2 = match rest1, rest2 with
  | [], _ -> rest2
  | _, [] -> rest1
  | _ -> assert false (* do not use on these arguments *)

let complete_regs def srcrs1 srcrs2 =
  let nb_added = (List.length srcrs1) - (List.length srcrs2) in
  let (def, added_regs) = fresh_regs def nb_added in
  if nb_added > 0 then (srcrs1, srcrs2 @ added_regs, added_regs)
  else (srcrs1 @ added_regs, srcrs2, added_regs)


let size_of_sig_type = function
  | AST.Sig_int (i, _) -> i
  | AST.Sig_float _ -> error_float ()
  | AST.Sig_offset -> Driver.TargetArch.int_size
  | AST.Sig_ptr -> Driver.TargetArch.ptr_size

let reg_size_of_sig_type sig_type =
  MiscPottier.div_up (size_of_sig_type sig_type) Driver.TargetArch.int_size

let fresh_sig_type def sig_type =
  fresh_regs def (reg_size_of_sig_type sig_type)

let fresh_int def =
  fresh_sig_type def (AST.Sig_int (Driver.TargetArch.int_size, AST.Signed))

let fresh_offset def = fresh_sig_type def AST.Sig_offset

let fresh_pointer def = fresh_sig_type def AST.Sig_ptr

let concrete_offsets_size = Driver.TargetArchMemory.concrete_offsets_size

let concrete_size = Driver.TargetArchMemory.concrete_size

let concrete_offset = Driver.TargetArchMemory.concrete_offset


(* Local environment *)

type local_env = Register.t list Register.Map.t

let mem_local_env = Register.Map.mem
let add_local_env = Register.Map.add
let find_local_env = Register.Map.find

let initialize_local_env runiverse registers result =
  let registers =
    registers @ (match result with None -> [] | Some (r, t) -> [(r, t)]) in
  let f lenv (r, t) =
    let rs = register_freshes runiverse (reg_size_of_sig_type t) in
    add_local_env r rs lenv in
  List.fold_left f Register.Map.empty registers

let map_list_local_env lenv regs =
  let f res r = res @ (find_local_env r lenv) in
  List.fold_left f [] regs

let rtl_args regs_list lenv =
  List.flatten (List.map (fun r -> find_local_env r lenv) regs_list)


(* Global environment *)

type global_env = AST.immediate StringTools.Map.t

let mem_global_env = StringTools.Map.mem
let add_global_env = StringTools.Map.add
let find_global_env = StringTools.Map.find

let make_global_struct globals = AST.SProd (List.map snd globals)

let initialize_global_env globals =
  let (offsets, size) = concrete_offsets_size (make_global_struct globals) in
  let f genv (x, _) offset = StringTools.Map.add x offset genv in
  let genv = List.fold_left2 f StringTools.Map.empty globals offsets in
  (genv, size)


let change_label lbl = function
  | RTL.St_skip _ -> RTL.St_skip lbl
  | RTL.St_cost (cost_lbl, _) -> RTL.St_cost (cost_lbl, lbl)
  | RTL.St_int (r, i, _) -> RTL.St_int (r, i, lbl)
  | RTL.St_move (r1, r2, _) -> RTL.St_move (r1, r2, lbl)
  | RTL.St_unop (unop, dstr, srcr, _) -> RTL.St_unop (unop, dstr, srcr, lbl)
  | RTL.St_binop (binop, dstr, srcr1, srcr2, _) ->
    RTL.St_binop (binop, dstr, srcr1, srcr2, lbl)
  | RTL.St_funaddr (rs, id, _) -> RTL.St_funaddr (rs, id, lbl)
  | RTL.St_stackaddr (rs, _) -> RTL.St_stackaddr (rs, lbl)
  | RTL.St_globaladdr (rs, _) -> RTL.St_globaladdr (rs, lbl)
  | RTL.St_load (size, dstrs, addrrs, _) ->
    RTL.St_load (size, dstrs, addrrs, lbl)
  | RTL.St_store (size, addrrs, srcrs, _) ->
    RTL.St_store (size, addrrs, srcrs, lbl)
  | RTL.St_cond _ as inst -> inst
  | RTL.St_return regs -> RTL.St_return regs
  | RTL.St_call (addr, args, retrs, _) -> RTL.St_call (addr, args, retrs, lbl)
  | RTL.St_tailcall (addr, args) -> RTL.St_tailcall (addr, args)

(* Add a list of instruction in a graph, from one label to another, by creating
   fresh labels inbetween. *)

let rec adds_graph stmt_list start_lbl dest_lbl def = match stmt_list with
  | [] -> add_graph start_lbl (RTL.St_skip dest_lbl) def
  | [stmt] ->
    add_graph start_lbl (change_label dest_lbl stmt) def
  | stmt :: stmt_list ->
    let tmp_lbl = fresh_label def in
    let stmt = change_label tmp_lbl stmt in
    let def = add_graph start_lbl stmt def in
    adds_graph stmt_list tmp_lbl dest_lbl def

(* Process a list of function that adds a list of instructions to a graph, from
   one label to another, and by creating fresh labels inbetween. *)

let rec add_translates translate_list start_lbl dest_lbl def =
  match translate_list with
    | [] -> add_graph start_lbl (RTL.St_skip dest_lbl) def
    | [trans] -> trans start_lbl dest_lbl def
    | trans :: translate_list ->
      let tmp_lbl = fresh_label def in
      let def = trans start_lbl tmp_lbl def in
      add_translates translate_list tmp_lbl dest_lbl def


let rec translate_cst genv cst destrs start_lbl dest_lbl def = match cst with

  | AST.Cst_int _ when destrs = [] ->
    add_graph start_lbl (RTL.St_skip dest_lbl) def

  | AST.Cst_int i ->
    let byte_size = (List.length destrs) * Driver.TargetArch.int_size in
    let nb_regs = List.length destrs in
    let module M = IntValue.Make (struct let size = byte_size end) in
    let is = List.map M.to_int (M.break (M.of_int i) nb_regs) in
    let f r i = RTL.St_int (r, i, dest_lbl) in
    let l = List.map2 f destrs is in
    adds_graph l start_lbl dest_lbl def

  | AST.Cst_float _ -> error_float ()

  | AST.Cst_addrsymbol id when mem_global_env id genv ->
    let off = find_global_env id genv in
    let (def, tmprs) = fresh_offset def in
    add_translates
      [translate_cst genv (AST.Cst_int off) tmprs ;
       adds_graph [RTL.St_globaladdr (destrs, start_lbl)] ;
       translate_op2 genv AST.Op_addp destrs destrs tmprs]
      start_lbl dest_lbl def

  | AST.Cst_addrsymbol id ->
    add_graph start_lbl (RTL.St_funaddr (destrs, id, dest_lbl)) def

  | AST.Cst_stack ->
    add_graph start_lbl (RTL.St_stackaddr (destrs, dest_lbl)) def

  | AST.Cst_offset off ->
    let i = concrete_offset off in
    translate_cst genv (AST.Cst_int i) destrs start_lbl dest_lbl def

  | AST.Cst_sizeof size ->
    let i = concrete_size size in
    translate_cst genv (AST.Cst_int i) destrs start_lbl dest_lbl def


and translate_move genv destrs srcrs start_lbl =
  let ((common1, rest1), (common2, rest2)) = MiscPottier.reduce destrs srcrs in
  let f_common destr srcr = RTL.St_move (destr, srcr, start_lbl) in
  let translates1 = adds_graph (List.map2 f_common common1 common2) in
  let translates2 = translate_cst genv (AST.Cst_int 0) rest1 in
  add_translates [translates1 ; translates2] start_lbl


and cast_insts_init
    src_size dest_size sh_op sign_bit last_bit tmpr src_sign_reg dest_sign_reg
    sh_reg start_lbl =
  let lshift_amount bit = Driver.TargetArch.int_size*8 - 1 - bit in
  if src_size >= dest_size then
    let lshift_amount = lshift_amount last_bit in
    [RTL.St_int (sh_reg, lshift_amount, start_lbl) ;
     RTL.St_binop
       (Arch.OpSllv, dest_sign_reg, src_sign_reg, sh_reg, start_lbl) ;
     RTL.St_binop
       (Arch.OpSrlv, dest_sign_reg, dest_sign_reg, sh_reg, start_lbl) ;
     RTL.St_int (tmpr, 0, start_lbl)]
  else
    let lshift_amount = lshift_amount sign_bit in
    [RTL.St_int (sh_reg, lshift_amount, start_lbl) ;
     RTL.St_binop
       (Arch.OpSllv, dest_sign_reg, src_sign_reg, sh_reg, start_lbl) ;
     RTL.St_binop (sh_op, dest_sign_reg, dest_sign_reg, sh_reg, start_lbl) ;
     RTL.St_int (sh_reg, sign_bit, start_lbl) ;
     RTL.St_binop (sh_op, tmpr, dest_sign_reg, sh_reg, start_lbl)]

and translate_cast genv src_size src_sign dest_size destrs srcrs
    start_lbl dest_lbl def =
  if List.length srcrs = 0 then adds_graph [] start_lbl dest_lbl def
  else
    let (def, tmpr) = fresh_reg def in
    let (def, sh_reg) = fresh_reg def in
    let (_, (common, rest)) = MiscPottier.reduce srcrs destrs in
    let (srcrs', src_sign_reg) = MiscPottier.split_last srcrs in
    let (common, dest_sign_reg) = MiscPottier.split_last common in
    let sign_bit = (src_size*8 - 1) mod (Driver.TargetArch.int_size*8) in
    let last_bit = (dest_size*8 - 1) mod (Driver.TargetArch.int_size*8) in
    let sh_op = match src_sign with
      | AST.Unsigned -> Arch.OpSrlv
      | AST.Signed -> Arch.OpSrav in
    let insts_init =
      cast_insts_init src_size dest_size sh_op sign_bit last_bit tmpr
	src_sign_reg dest_sign_reg sh_reg start_lbl in
    let insts_init = adds_graph insts_init in
    let insts_common = translate_move genv common srcrs' in
    let rs = MiscPottier.make tmpr (List.length rest) in
    let insts_rest = translate_move genv rest rs in
    add_translates
      [insts_init ; insts_common ; insts_rest] start_lbl dest_lbl def


and translate_negint destrs srcrs start_lbl dest_lbl def =
  assert (List.length destrs = List.length srcrs) ;
  let (def, carry) = fresh_reg def in
  let (def, tmpr) = fresh_reg def in
  let f_cmpl destr srcr = RTL.St_unop (Arch.UOpNot, destr, srcr, start_lbl) in
  let insts_cmpl = List.map2 f_cmpl destrs srcrs in
  let init_carry = RTL.St_int (carry, 1, start_lbl) in
  let f_add destr =
    [RTL.St_move (tmpr, destr, start_lbl) ;
     RTL.St_binop (Arch.OpAdd, destr, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, tmpr, destr, tmpr, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, carry, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_add = List.flatten (List.map f_add destrs) in
  adds_graph (insts_cmpl @ [init_carry] @ insts_add) start_lbl dest_lbl def


and translate_notbool genv destrs srcrs start_lbl dest_lbl def =
  match destrs with
    | [] -> add_graph start_lbl (RTL.St_skip dest_lbl) def
    | destr :: destrs ->
      let (def, zero) = fresh_reg def in
      let (def, tmpr) = fresh_reg def in
      let init_zero = RTL.St_int (zero, 0, start_lbl) in
      let init_tmpr = RTL.St_int (tmpr, 0, start_lbl) in
      let f srcr = RTL.St_binop (Arch.OpOr, tmpr, tmpr, srcr, start_lbl) in
      let insts = List.map f srcrs in
      let init_destrs = translate_cst genv (AST.Cst_int 0) destrs in
      let init_destr = RTL.St_binop (Arch.OpEq, destr, tmpr, zero, start_lbl) in
      add_translates
	[adds_graph (init_zero :: init_tmpr :: insts) ; init_destrs ;
	 adds_graph [init_destr]]
	start_lbl dest_lbl def


and translate_op1 genv op1 destrs srcrs start_lbl dest_lbl def = match op1 with

  | AST.Op_cast ((src_size, src_sign), dest_size) ->
    translate_cast genv src_size src_sign dest_size destrs srcrs
      start_lbl dest_lbl def

  | AST.Op_negint ->
    translate_negint destrs srcrs start_lbl dest_lbl def

  | AST.Op_notbool ->
    translate_notbool genv destrs srcrs start_lbl dest_lbl def

  | AST.Op_notint ->
    let f destr srcr = RTL.St_unop (Arch.UOpNot, destr, srcr, start_lbl) in
    let l = List.map2 f destrs srcrs in
    adds_graph l start_lbl dest_lbl def

  | AST.Op_ptrofint | AST.Op_intofptr | AST.Op_id ->
    translate_move genv destrs srcrs start_lbl dest_lbl def


and translate_add destrs srcrs1 srcrs2 start_lbl dest_lbl def =
  let ((srcrs1_common, srcrs1_rest), (srcrs2_common, srcrs2_rest)) =
    MiscPottier.reduce srcrs1 srcrs2 in
  let srcrs_rest = choose_rest srcrs1_rest srcrs2_rest in
  let ((destrs_common, destrs_rest), _) =
    MiscPottier.reduce destrs srcrs1_common in
  let ((destrs_cted, destrs_rest), (srcrs_cted, _)) =
    MiscPottier.reduce destrs_rest srcrs_rest in
  let (def, zero) = fresh_reg def in
  let (def, carry) = fresh_reg def in
  let (def, tmp_srcr1) = fresh_reg def in
  let (def, tmp_srcr2) = fresh_reg def in
  let (def, tmpr) = fresh_reg def in
  let insts_init =
    [RTL.St_int (zero, 0, start_lbl) ;
     RTL.St_int (carry, 0, start_lbl)] in
  let f_add destr srcr1 srcr2 =
    [RTL.St_move (tmp_srcr1, srcr1, start_lbl) ;
     RTL.St_move (tmp_srcr2, srcr2, start_lbl) ;
     RTL.St_binop (Arch.OpAdd, destr, srcr1, srcr2, start_lbl) ;
     RTL.St_binop (Arch.OpAdd, destr, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, tmpr, destr, tmp_srcr1, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, carry, destr, tmp_srcr2, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_add =
    List.flatten
      (MiscPottier.map3 f_add destrs_common srcrs1_common srcrs2_common) in
  let f_add_cted destr srcr =
    [RTL.St_move (tmp_srcr1, srcr, start_lbl) ;
     RTL.St_binop (Arch.OpAdd, destr, srcr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, tmpr, destr, tmp_srcr1, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, carry, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_add_cted =
    List.flatten (List.map2 f_add_cted destrs_cted srcrs_cted) in
  let f_rest destr =
    [RTL.St_binop (Arch.OpAdd, destr, zero, carry, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, tmpr, destr, zero, start_lbl) ;
     RTL.St_binop (Arch.OpLtu, carry, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_rest = List.flatten (List.map f_rest destrs_rest) in
  adds_graph (insts_init @ insts_add @ insts_add_cted @ insts_rest)
    start_lbl dest_lbl def

and translate_sub destrs srcrs1 srcrs2 start_lbl dest_lbl def =
  let ((srcrs1_common, srcrs1_rest), (srcrs2_common, srcrs2_rest)) =
    MiscPottier.reduce srcrs1 srcrs2 in
  let srcrs_rest = choose_rest srcrs1_rest srcrs2_rest in
  let ((destrs_common, destrs_rest), _) =
    MiscPottier.reduce destrs srcrs1_common in
  let ((destrs_cted, destrs_rest), (srcrs_cted, _)) =
    MiscPottier.reduce destrs_rest srcrs_rest in
  let (def, zero) = fresh_reg def in
  let (def, carry) = fresh_reg def in
  let (def, tmp_srcr1) = fresh_reg def in
  let (def, tmp_srcr2) = fresh_reg def in
  let (def, tmpr) = fresh_reg def in
  let insts_init =
    [RTL.St_int (zero, 0, start_lbl) ;
     RTL.St_int (carry, 0, start_lbl)] in
  let f_sub destr srcr1 srcr2 =
    [RTL.St_move (tmp_srcr1, srcr1, start_lbl) ;
     RTL.St_move (tmp_srcr2, srcr2, start_lbl) ;
     RTL.St_binop (Arch.OpSub, destr, srcr1, srcr2, start_lbl) ;
     RTL.St_binop (Arch.OpSub, destr, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpGtu, tmpr, destr, tmp_srcr1, start_lbl) ;
     RTL.St_binop (Arch.OpGtu, carry, destr, tmp_srcr2, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_sub =
    List.flatten
      (MiscPottier.map3 f_sub destrs_common srcrs1_common srcrs2_common) in
  let (op_cted, cmp_cted) =
    if List.length srcrs1 >= List.length srcrs2 then (Arch.OpAdd, Arch.OpLtu)
    else (Arch.OpSub, Arch.OpGtu) in
  let f_cted destr srcr =
    [RTL.St_move (tmp_srcr1, srcr, start_lbl) ;
     RTL.St_binop (op_cted, destr, srcr, carry, start_lbl) ;
     RTL.St_binop (cmp_cted, tmpr, destr, tmp_srcr1, start_lbl) ;
     RTL.St_binop (cmp_cted, carry, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_cted =
    List.flatten (List.map2 f_cted destrs_cted srcrs_cted) in
  let f_rest destr =
    [RTL.St_binop (op_cted, destr, zero, carry, start_lbl) ;
     RTL.St_binop (cmp_cted, tmpr, destr, zero, start_lbl) ;
     RTL.St_binop (cmp_cted, carry, destr, carry, start_lbl) ;
     RTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_rest = List.flatten (List.map f_rest destrs_rest) in
  adds_graph (insts_init @ insts_sub @ insts_cted @ insts_rest)
    start_lbl dest_lbl def

and translate_boolop neutral op destrs srcrs1 srcrs2 start_lbl dest_lbl def =
  let ((srcrs1_common, srcrs1_rest), (srcrs2_common, srcrs2_rest)) =
    MiscPottier.reduce srcrs1 srcrs2 in
  let srcrs_rest = choose_rest srcrs1_rest srcrs2_rest in
  let ((destrs_common, destrs_rest), _) =
    MiscPottier.reduce destrs srcrs1_common in
  let ((destrs_cted, destrs_rest), (srcrs_cted, _)) =
    MiscPottier.reduce destrs_rest srcrs_rest in
  let (def, neutralr) = fresh_reg def in
  let (def, tmpr) = fresh_reg def in
  let init_neutralr =
    [RTL.St_int (neutralr, neutral, start_lbl) ;
     RTL.St_int (tmpr, Driver.TargetArch.int_size*8 - 1, start_lbl) ;
     RTL.St_binop (Arch.OpSllv, neutralr, neutralr, tmpr, start_lbl) ;
     RTL.St_binop (Arch.OpSrav, neutralr, neutralr, tmpr, start_lbl)] in
  let f_op destr srcr1 srcr2 =
    RTL.St_binop (op, destr, srcr1, srcr2, start_lbl) in
  let insts_op =
    MiscPottier.map3 f_op destrs_common srcrs1_common srcrs2_common in
  let f_cted destr srcr = RTL.St_binop (op, destr, srcr, neutralr, start_lbl) in
  let insts_cted = List.map2 f_cted destrs_cted srcrs_cted in
  let f_rest destr = RTL.St_int (destr, 0, start_lbl) in
  let insts_rest = List.map f_rest destrs_rest in
  adds_graph (init_neutralr @ insts_op @ insts_cted @ insts_rest)
    start_lbl dest_lbl def

and translate_eq_ne op destrs srcrs1 srcrs2 start_lbl dest_lbl def =
  match destrs with
    | [] -> adds_graph [] start_lbl dest_lbl def
    | destr :: _ ->
      let ((srcrs1_common, srcrs1_rest), (srcrs2_common, srcrs2_rest)) =
	MiscPottier.reduce srcrs1 srcrs2 in
      let srcrs_rest = choose_rest srcrs1_rest srcrs2_rest in
      let (def, zero) = fresh_reg def in
      let (def, tmpr) = fresh_reg def in
      let (def, resr) = fresh_reg def in
      let insts_init =
	[RTL.St_int (zero, 0, start_lbl) ; RTL.St_int (resr, 0, start_lbl)] in
      let f_eq srcr1 srcr2 =
	[RTL.St_binop (op, tmpr, srcr1, srcr2, start_lbl) ;
	 RTL.St_binop (Arch.OpOr, resr, resr, tmpr, start_lbl)] in
      let insts_eq =
	List.flatten (List.map2 f_eq srcrs1_common srcrs2_common) in
      let f_rest srcr =
	[RTL.St_binop (Arch.OpEq, tmpr, srcr, zero, start_lbl) ;
	 RTL.St_binop (Arch.OpOr, resr, resr, tmpr, start_lbl)] in
      let insts_rest = List.flatten (List.map f_rest srcrs_rest) in
      let f_destrs destr = RTL.St_int (destr, 0, start_lbl) in
      let insts_destrs = List.map f_destrs destrs in
      let inst_destr = [RTL.St_move (destr, resr, start_lbl)] in
      adds_graph
	(insts_init @ insts_eq @ insts_rest @ insts_destrs @ inst_destr)
	start_lbl dest_lbl def

and translate_eq_list tmpr destr leq dummy_lbl =
  let f (srcr1, srcr2) =
    [RTL.St_binop (Arch.OpEq, tmpr, srcr1, srcr2, dummy_lbl) ;
     RTL.St_binop (Arch.OpAnd, destr, destr, tmpr, dummy_lbl)] in
  (RTL.St_int (destr, 1, dummy_lbl)) :: (List.flatten (List.map f leq))

and translate_atom op tmpr1 tmpr2 destr dummy_lbl leq srcr1 srcr2 =
  (translate_eq_list tmpr1 tmpr2 leq dummy_lbl) @
  [RTL.St_binop (op, tmpr1, srcr1, srcr2, dummy_lbl) ;
   RTL.St_binop (Arch.OpAnd, tmpr2, tmpr2, tmpr1, dummy_lbl) ;
   RTL.St_binop (Arch.OpOr, destr, destr, tmpr2, dummy_lbl)]

and translate_cmp_main op last_op tmpr1 tmpr2 destr dummy_lbl srcrs1 srcrs2 =
  let f op (insts, leq) srcr1 srcr2 =
    let added_insts =
      translate_atom op tmpr1 tmpr2 destr dummy_lbl leq srcr1 srcr2 in
    (insts @ added_insts, leq @ [(srcr1, srcr2)]) in
  let rec aux acc srcrs1 srcrs2 = match srcrs1, srcrs2 with
    | [], [] -> acc
    | [srcr1], [srcr2] -> f last_op acc srcr1 srcr2
    | srcr1 :: srcrs1, srcr2 :: srcrs2 ->
      aux (f op acc srcr1 srcr2) srcrs1 srcrs2
    | _ -> assert false (* lists have different sizes *) in
  fst (aux ([], []) srcrs1 srcrs2)

and translate_cmp genv op last_op destrs srcrs1 srcrs2 start_lbl dest_lbl def =
  match destrs with
    | [] -> add_graph start_lbl (RTL.St_skip dest_lbl) def
    | _ ->
      let (def, tmp_destrs) = fresh_regs def (List.length destrs) in
      let tmp_destr = List.hd tmp_destrs in
      let (def, tmpr1) = fresh_reg def in
      let (def, tmpr2) = fresh_reg def in
      let (srcrs1, srcrs2, added) = complete_regs def srcrs1 srcrs2 in
      let srcrs1 = List.rev srcrs1 in
      let srcrs2 = List.rev srcrs2 in
      let insts_init =
	[translate_cst genv (AST.Cst_int 0) tmp_destrs ;
	 translate_cst genv (AST.Cst_int 0) added] in
      let insts_main =
	translate_cmp_main op last_op tmpr1 tmpr2 tmp_destr
	  start_lbl srcrs1 srcrs2 in
      let insts_main = [adds_graph insts_main] in
      let insts_exit = [translate_move genv destrs tmp_destrs] in
      add_translates (insts_init @ insts_main @ insts_exit)
	start_lbl dest_lbl def


and translate_op2 genv op2 destrs srcrs1 srcrs2 start_lbl dest_lbl def =
  match op2, destrs, srcrs1, srcrs2 with

    | (AST.Op_add | AST.Op_addp), _, _, _ ->
      translate_add destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | (AST.Op_sub | AST.Op_subp | AST.Op_subpp), _, _, _ ->
      translate_sub destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | AST.Op_mul, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpMul, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_div, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpDiv, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_divu, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpDivu, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_modu, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpModu, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_shl, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpSllv, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_shr, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpSrav, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_shru, [destr], [srcr1], [srcr2] ->
      add_graph start_lbl
	(RTL.St_binop (Arch.OpSrlv, destr, srcr1, srcr2, dest_lbl)) def

    | AST.Op_and, _, _, _ ->
      translate_boolop 1 Arch.OpAnd destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | AST.Op_or, _, _, _ ->
      translate_boolop 0 Arch.OpOr destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | AST.Op_xor, _, _, _ ->
      translate_boolop 0 Arch.OpXor destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | (AST.Op_cmp AST.Cmp_eq | AST.Op_cmpu AST.Cmp_eq | AST.Op_cmpp AST.Cmp_eq),
      _, _, _ ->
      translate_eq_ne Arch.OpEq destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | (AST.Op_cmp AST.Cmp_ne | AST.Op_cmpu AST.Cmp_ne | AST.Op_cmpp AST.Cmp_ne),
      _, _, _ ->
      translate_eq_ne Arch.OpNe destrs srcrs1 srcrs2 start_lbl dest_lbl def

    | AST.Op_cmp AST.Cmp_lt, _, _, _ ->
      translate_cmp genv Arch.OpLt Arch.OpLt destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | (AST.Op_cmpu AST.Cmp_lt | AST.Op_cmpp AST.Cmp_lt), _, _, _ ->
      translate_cmp genv Arch.OpLtu Arch.OpLtu destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | AST.Op_cmp AST.Cmp_le, _, _, _ ->
      translate_cmp genv Arch.OpLt Arch.OpLe destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | (AST.Op_cmpu AST.Cmp_le | AST.Op_cmpp AST.Cmp_le), _, _, _ ->
      translate_cmp genv Arch.OpLtu Arch.OpLeu destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | AST.Op_cmp AST.Cmp_gt, _, _, _ ->
      translate_cmp genv Arch.OpGt Arch.OpGt destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | (AST.Op_cmpu AST.Cmp_gt | AST.Op_cmpp AST.Cmp_gt), _, _, _ ->
      translate_cmp genv Arch.OpGtu Arch.OpGtu destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | AST.Op_cmp AST.Cmp_ge, _, _, _ ->
      translate_cmp genv Arch.OpGt Arch.OpGe destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | (AST.Op_cmpu AST.Cmp_ge | AST.Op_cmpp AST.Cmp_ge), _, _, _ ->
      translate_cmp genv Arch.OpGtu Arch.OpGeu destrs srcrs1 srcrs2
	start_lbl dest_lbl def

    | (     AST.Op_mul | AST.Op_div | AST.Op_divu | AST.Op_modu | AST.Op_mod
	  | AST.Op_shl | AST.Op_shr | AST.Op_shru), _, _, _ ->
      (* Unsupported, should have been replaced by a runtime function. *)
      assert false


let translate_cond srcrs start_lbl lbl_true lbl_false def =
  let (def, tmpr) = fresh_reg def in
  let tmp_lbl = fresh_label def in
  let init = RTL.St_int (tmpr, 0, start_lbl) in
  let f srcr = RTL.St_binop (Arch.OpOr, tmpr, tmpr, srcr, start_lbl) in
  let def = adds_graph (init :: (List.map f srcrs)) start_lbl tmp_lbl def in
  add_graph tmp_lbl (RTL.St_cond (tmpr, lbl_true, lbl_false)) def


let translate_load genv quantity addr destrs start_lbl dest_lbl def =
  let size = Driver.TargetArchMemory.size_of_quantity quantity in
  let (def, tmpr) = fresh_reg def in
  let (def, tmp_addr) = fresh_regs def (List.length addr) in
  let init_tmpr =
    adds_graph [RTL.St_int (tmpr, Driver.TargetArch.int_size, start_lbl)] in
  let init_tmp_addr = translate_move genv tmp_addr addr in
  let f translates destr =
    translates @
      [adds_graph [RTL.St_load (size, destr, tmp_addr, start_lbl)] ;
       translate_op2 genv AST.Op_addp tmp_addr tmp_addr [tmpr]] in
  let translates = List.fold_left f [] destrs in
  add_translates
    (init_tmpr :: init_tmp_addr :: translates) start_lbl dest_lbl def


let translate_store genv quantity addr srcrs start_lbl dest_lbl def =
  let size = Driver.TargetArchMemory.size_of_quantity quantity in
  let (def, tmpr) = fresh_reg def in
  let (def, tmp_addr) = fresh_regs def (List.length addr) in
  let init_tmpr =
    adds_graph [RTL.St_int (tmpr, Driver.TargetArch.int_size, start_lbl)] in
  let init_tmp_addr = translate_move genv tmp_addr addr in
  let f translates srcr =
    translates @
      [adds_graph [RTL.St_store (size, tmp_addr, srcr, dest_lbl)];
       translate_op2 genv AST.Op_addp tmp_addr tmp_addr [tmpr]] in
  let translates = List.fold_left f [] srcrs in
  add_translates
    (init_tmpr :: init_tmp_addr :: translates) start_lbl dest_lbl def


let translate_stmt genv lenv lbl stmt def = match stmt with

  | RTLabs.St_skip lbl' ->
    add_graph lbl (RTL.St_skip lbl') def

  | RTLabs.St_cost (cost_lbl, lbl') ->
    add_graph lbl (RTL.St_cost (cost_lbl, lbl')) def

  | RTLabs.St_cst (destr, cst, lbl') ->
    translate_cst genv cst (find_local_env destr lenv) lbl lbl' def

  | RTLabs.St_op1 (op1, destr, srcr, lbl') ->
    translate_op1 genv op1
      (find_local_env destr lenv) (find_local_env srcr lenv)
      lbl lbl' def

  | RTLabs.St_op2 (op2, destr, srcr1, srcr2, lbl') ->
    translate_op2 genv op2 (find_local_env destr lenv)
      (find_local_env srcr1 lenv) (find_local_env srcr2 lenv) lbl lbl' def

  | RTLabs.St_load (quantity, addr, destr, lbl') ->
    translate_load genv quantity
      (find_local_env addr lenv) (find_local_env destr lenv)
      lbl lbl' def

  | RTLabs.St_store (quantity, addr, srcr, lbl') ->
    translate_store genv quantity
      (find_local_env addr lenv) (find_local_env srcr lenv)
      lbl lbl' def

  | RTLabs.St_call_id (f, args, None, _, lbl') ->
    let (def, addr) = fresh_pointer def in
    add_translates
      [translate_cst genv (AST.Cst_addrsymbol f) addr ;
       adds_graph [RTL.St_call (addr, rtl_args args lenv, [], lbl)]]
      lbl lbl' def

  | RTLabs.St_call_id (f, args, Some retr, _, lbl') ->
    let (def, addr) = fresh_pointer def in
    let retrs = find_local_env retr lenv in
    add_translates
      [translate_cst genv (AST.Cst_addrsymbol f) addr ;
       adds_graph [RTL.St_call (addr, rtl_args args lenv, retrs, lbl)]]
      lbl lbl' def

  | RTLabs.St_call_ptr (f, args, None, _, lbl') ->
    let addr = find_local_env f lenv in
    add_graph lbl (RTL.St_call (addr, rtl_args args lenv, [], lbl')) def

  | RTLabs.St_call_ptr (f, args, Some retr, _, lbl') ->
    let addr = find_local_env f lenv in
    add_graph lbl
      (RTL.St_call (addr, rtl_args args lenv, find_local_env retr lenv, lbl'))
      def

  | RTLabs.St_tailcall_id (f, args, _) ->
    let (def, addr) = fresh_pointer def in
    add_translates
      [translate_cst genv (AST.Cst_addrsymbol f) addr ;
       adds_graph [RTL.St_tailcall (addr, rtl_args args lenv)]]
      lbl lbl def

  | RTLabs.St_tailcall_ptr (f, args, _) ->
    let addr = find_local_env f lenv in
    add_graph lbl (RTL.St_tailcall (addr, rtl_args args lenv)) def

  | RTLabs.St_cond (r, lbl_true, lbl_false) ->
    translate_cond (find_local_env r lenv) lbl lbl_true lbl_false def

  | RTLabs.St_jumptable _ ->
    error "Jump tables not supported yet."

  | RTLabs.St_return None ->
    add_graph lbl (RTL.St_return []) def

  | RTLabs.St_return (Some r) ->
    add_graph lbl (RTL.St_return (find_local_env r lenv)) def


let translate_internal genv def =
  let runiverse = def.RTLabs.f_runiverse in
  let lenv =
    initialize_local_env runiverse
      (def.RTLabs.f_params @ def.RTLabs.f_locals) def.RTLabs.f_result in
  let set_of_list l = List.fold_right Register.Set.add l Register.Set.empty in
  let params = map_list_local_env lenv (List.map fst def.RTLabs.f_params) in
  let locals = map_list_local_env lenv (List.map fst def.RTLabs.f_locals) in
  let locals = set_of_list locals in
  let result = match def.RTLabs.f_result with
    | None -> []
    | Some (r, _) -> find_local_env r lenv in
  let res =
    { RTL.f_luniverse = def.RTLabs.f_luniverse ;
      RTL.f_runiverse = runiverse ;
      RTL.f_result    = result ;
      RTL.f_params    = params ;
      RTL.f_locals    = locals ;
      RTL.f_stacksize = concrete_size def.RTLabs.f_stacksize ;
      RTL.f_graph     = Label.Map.empty ;
      RTL.f_entry     = def.RTLabs.f_entry ;
      RTL.f_exit      = def.RTLabs.f_exit } in
  Label.Map.fold (translate_stmt genv lenv) def.RTLabs.f_graph res


let translate_fun_def genv = function
  | RTLabs.F_int def -> RTL.F_int (translate_internal genv def)
  | RTLabs.F_ext def -> RTL.F_ext def


let translate p =
  let (genv, globals) = initialize_global_env p.RTLabs.vars in
  let f_funct (id, fun_def) = (id, translate_fun_def genv fun_def) in
  { RTL.globals = globals ;
    RTL.functs  = List.map f_funct p.RTLabs.functs ;
    RTL.main    = p.RTLabs.main }

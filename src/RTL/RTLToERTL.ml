
(** This module provides a translation of [RTL] programs to [ERTL]
    programs. *)


let error_prefix = "RTL to ERTL"
let error = Error.global_error error_prefix

let error_float () = error "float not supported."


(* Helper functions *)

let change_exit_label lbl def =
  { def with ERTL.f_exit = lbl }

let add_graph lbl stmt def =
  { def with ERTL.f_graph = Label.Map.add lbl stmt def.ERTL.f_graph }

let fresh_label def = Label.Gen.fresh def.ERTL.f_luniverse

let change_label lbl = function
  | ERTL.St_skip _ -> ERTL.St_skip lbl
  | ERTL.St_comment (s, _) -> ERTL.St_comment (s, lbl)
  | ERTL.St_cost (cost_lbl, _) -> ERTL.St_cost (cost_lbl, lbl)
  | ERTL.St_newframe _ -> ERTL.St_newframe lbl
  | ERTL.St_delframe _ -> ERTL.St_delframe lbl
  | ERTL.St_framesize (r, _) -> ERTL.St_framesize (r, lbl)
  | ERTL.St_get_hdw (r1, r2, _) -> ERTL.St_get_hdw (r1, r2, lbl)
  | ERTL.St_set_hdw (r1, r2, _) -> ERTL.St_set_hdw (r1, r2, lbl)
  | ERTL.St_hdw_to_hdw (r1, r2, _) -> ERTL.St_hdw_to_hdw (r1, r2, lbl)
  | ERTL.St_move (r1, r2, _) -> ERTL.St_move (r1, r2, lbl)
  | ERTL.St_int (r, i, _) -> ERTL.St_int (r, i, lbl)
  | ERTL.St_unop (unop, dstr, srcr, _) -> ERTL.St_unop (unop, dstr, srcr, lbl)
  | ERTL.St_binop (binop, dstr, srcr1, srcr2, _) ->
    ERTL.St_binop (binop, dstr, srcr1, srcr2, lbl)
  | ERTL.St_addrN (dstrs, id, n, _) -> ERTL.St_addrN (dstrs, id, n, lbl)
  | ERTL.St_load (size, dstrs, addr, _) -> ERTL.St_load (size, dstrs, addr, lbl)
  | ERTL.St_store (size, addr, srcrs, _) ->
    ERTL.St_store (size, addr, srcrs, lbl)
  | ERTL.St_call (f, nb_args, _) -> ERTL.St_call (f, nb_args, lbl)
  | ERTL.St_tailcall _ | ERTL.St_cond _ | ERTL.St_return _ as inst -> inst

(* Add a list of instruction in a graph, from one label to another, by creating
   fresh labels inbetween. *)

let rec adds_graph stmt_list start_lbl dest_lbl def = match stmt_list with
  | [] -> add_graph start_lbl (ERTL.St_skip dest_lbl) def
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
    | [] -> add_graph start_lbl (ERTL.St_skip dest_lbl) def
    | [trans] -> trans start_lbl dest_lbl def
    | trans :: translate_list ->
      let tmp_lbl = fresh_label def in
      let def = trans start_lbl tmp_lbl def in
      add_translates translate_list tmp_lbl dest_lbl def

let size_of_sig_type = function
  | AST.Sig_int (i, _) -> i
  | AST.Sig_float _ -> error_float ()
  | AST.Sig_offset -> Driver.TargetArch.int_size
  | AST.Sig_ptr -> Driver.TargetArch.ptr_size

let reg_size_of_sig_type sig_type =
  MiscPottier.div_up (size_of_sig_type sig_type) Driver.TargetArch.int_size

let fresh_reg def =
  let r = Register.fresh def.ERTL.f_runiverse in
  let locals = Register.Set.add r def.ERTL.f_locals in
  ({ def with ERTL.f_locals = locals }, r)

let rec fresh_regs def n =
  if n = 0 then (def, [])
  else
    let (def, res) = fresh_regs def (n-1) in
    let (def, r) = fresh_reg def in
    (def, r :: res)

let fresh_sig_type def sig_type =
  fresh_regs def (reg_size_of_sig_type sig_type)

let fresh_pointer def = fresh_sig_type def AST.Sig_ptr

let choose_rest rest1 rest2 = match rest1, rest2 with
  | [], _ -> rest2
  | _, [] -> rest1
  | _ -> assert false (* do not use on these arguments *)


let translate_add destrs srcrs1 srcrs2 start_lbl dest_lbl def =
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
    [ERTL.St_int (zero, 0, start_lbl) ;
     ERTL.St_int (carry, 0, start_lbl)] in
  let f_add destr srcr1 srcr2 =
    [ERTL.St_move (tmp_srcr1, srcr1, start_lbl) ;
     ERTL.St_move (tmp_srcr2, srcr2, start_lbl) ;
     ERTL.St_binop (Arch.OpAdd, destr, srcr1, srcr2, start_lbl) ;
     ERTL.St_binop (Arch.OpAdd, destr, destr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpLtu, tmpr, destr, tmp_srcr1, start_lbl) ;
     ERTL.St_binop (Arch.OpLtu, carry, destr, tmp_srcr2, start_lbl) ;
     ERTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_add =
    List.flatten
      (MiscPottier.map3 f_add destrs_common srcrs1_common srcrs2_common) in
  let f_add_cted destr srcr =
    [ERTL.St_move (tmp_srcr1, srcr, start_lbl) ;
     ERTL.St_binop (Arch.OpAdd, destr, srcr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpLtu, tmpr, destr, tmp_srcr1, start_lbl) ;
     ERTL.St_binop (Arch.OpLtu, carry, destr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_add_cted =
    List.flatten (List.map2 f_add_cted destrs_cted srcrs_cted) in
  let f_rest destr =
    [ERTL.St_binop (Arch.OpAdd, destr, zero, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpLtu, tmpr, destr, zero, start_lbl) ;
     ERTL.St_binop (Arch.OpLtu, carry, destr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_rest = List.flatten (List.map f_rest destrs_rest) in
  adds_graph (insts_init @ insts_add @ insts_add_cted @ insts_rest)
    start_lbl dest_lbl def

let translate_sub destrs srcrs1 srcrs2 start_lbl dest_lbl def =
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
    [ERTL.St_int (zero, 0, start_lbl) ;
     ERTL.St_int (carry, 0, start_lbl)] in
  let f_sub destr srcr1 srcr2 =
    [ERTL.St_move (tmp_srcr1, srcr1, start_lbl) ;
     ERTL.St_move (tmp_srcr2, srcr2, start_lbl) ;
     ERTL.St_binop (Arch.OpSub, destr, srcr1, srcr2, start_lbl) ;
     ERTL.St_binop (Arch.OpSub, destr, destr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpGtu, tmpr, destr, tmp_srcr1, start_lbl) ;
     ERTL.St_binop (Arch.OpGtu, carry, destr, tmp_srcr2, start_lbl) ;
     ERTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_sub =
    List.flatten
      (MiscPottier.map3 f_sub destrs_common srcrs1_common srcrs2_common) in
  let (op_cted, cmp_cted) =
    if List.length srcrs1 >= List.length srcrs2 then (Arch.OpAdd, Arch.OpLtu)
    else (Arch.OpSub, Arch.OpGtu) in
  let f_cted destr srcr =
    [ERTL.St_move (tmp_srcr1, srcr, start_lbl) ;
     ERTL.St_binop (op_cted, destr, srcr, carry, start_lbl) ;
     ERTL.St_binop (cmp_cted, tmpr, destr, tmp_srcr1, start_lbl) ;
     ERTL.St_binop (cmp_cted, carry, destr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_cted =
    List.flatten (List.map2 f_cted destrs_cted srcrs_cted) in
  let f_rest destr =
    [ERTL.St_binop (op_cted, destr, zero, carry, start_lbl) ;
     ERTL.St_binop (cmp_cted, tmpr, destr, zero, start_lbl) ;
     ERTL.St_binop (cmp_cted, carry, destr, carry, start_lbl) ;
     ERTL.St_binop (Arch.OpOr, carry, tmpr, carry, start_lbl)] in
  let insts_rest = List.flatten (List.map f_rest destrs_rest) in
  adds_graph (insts_init @ insts_sub @ insts_cted @ insts_rest)
    start_lbl dest_lbl def


(* Translation *)

let save_hdws l lbl =
  let f (destr, srcr) = ERTL.St_get_hdw (destr, srcr, lbl) in
  List.map f l

let save_hdws2 psds hdws =
  assert (List.length psds = List.length hdws) ;
  save_hdws (List.combine psds hdws)

let restore_hdws l lbl =
  let f (destr, srcr) = ERTL.St_set_hdw (destr, srcr, lbl) in
  List.map f (List.map (fun (x, y) -> (y, x)) l)

let restore_hdws2 hdws psds =
  assert (List.length hdws = List.length psds) ;
  restore_hdws (List.combine psds hdws)


let get_params_hdw params =
  assert false (* TODO M1 *)

let get_params_stack params =
  assert false (* TODO M1 *)

(* Parameters are taken from the physical parameter registers first. If there
   are not enough such of these, then the remaining parameters are taken from
   the stack. *)

let get_params params =
  assert false (* TODO M1 *)

let add_prologue params sra sregs def =
  let start_lbl = def.ERTL.f_entry in
  let tmp_lbl = fresh_label def in
  let last_stmt = Label.Map.find start_lbl def.ERTL.f_graph in
  let callee_saved =
    Driver.TargetArch.RegisterSet.elements Driver.TargetArch.callee_saved in
  let def =
    add_translates
      ([adds_graph [ERTL.St_comment ("Prologue", start_lbl)]] @
       (* new frame *)
       [adds_graph [ERTL.St_comment ("New frame", start_lbl) ;
		    ERTL.St_newframe start_lbl]] @
       (* save the return address *)
       [adds_graph [ERTL.St_comment ("Save return address", start_lbl)]] @
       [adds_graph (save_hdws2 sra Driver.TargetArch.ra start_lbl)] @
       (* save callee-saved registers *)
       [adds_graph [ERTL.St_comment ("Save callee-saved registers",
				     start_lbl)]] @
       [adds_graph (save_hdws2 sregs callee_saved start_lbl)] @
       (* fetch parameters *)
       [adds_graph [ERTL.St_comment ("Fetch parameters", start_lbl)]] @
       (get_params params) @
       [adds_graph [ERTL.St_comment ("End Prologue", start_lbl)]])
      start_lbl tmp_lbl def in
  add_graph tmp_lbl last_stmt def


let assign_result ret_regs start_lbl dest_lbl def =
  assert false (* TODO M1 *)

let add_epilogue ret_regs sra sregs def =
  let start_lbl = def.ERTL.f_exit in
  let tmp_lbl = fresh_label def in
  let last_stmt = Label.Map.find start_lbl def.ERTL.f_graph in
  let callee_saved =
    Driver.TargetArch.RegisterSet.elements Driver.TargetArch.callee_saved in
  let def =
    add_translates
      ([adds_graph [ERTL.St_comment ("Epilogue", start_lbl)]] @
       (* assign the result to actual return registers *)
       [adds_graph [ERTL.St_comment ("Set result", start_lbl)]] @
       [assign_result ret_regs] @
       (* restore callee-saved registers *)
       [adds_graph [ERTL.St_comment ("Restore callee-saved registers",
				     start_lbl)]] @
       [adds_graph (restore_hdws2 callee_saved sregs start_lbl)] @
       (* restore the return address *)
       [adds_graph [ERTL.St_comment ("Restore return address", start_lbl)]] @
       [adds_graph (restore_hdws2 Driver.TargetArch.ra sra start_lbl)] @
       (* delete frame *)
       [adds_graph [ERTL.St_comment ("Delete frame", start_lbl) ;
		    ERTL.St_delframe start_lbl]] @
       [adds_graph [ERTL.St_comment ("End Epilogue", start_lbl)]])
      start_lbl tmp_lbl def in
  let def = add_graph tmp_lbl last_stmt def in
  change_exit_label tmp_lbl def


let add_pro_and_epilogue params ret_regs def =
  (* Allocate registers to hold the return address. *)
  let (def, sra) = fresh_pointer def in
  (* Allocate registers to save callee-saved registers. *)
  let nb_callee_saved =
    Driver.TargetArch.RegisterSet.cardinal Driver.TargetArch.callee_saved in
  let (def, sregs) = fresh_regs def nb_callee_saved in
  (* Add a prologue and a epilogue. *)
  let def = add_prologue params sra sregs def in
  let def = add_epilogue ret_regs sra sregs def in
  def


let set_params_hdw params =
  assert false (* TODO M1 *)

let set_params_stack params =
  assert false (* TODO M1 *)

(* Parameters are put in the physical parameter registers first. If there are
   not enough such of these, then the remaining parameters are passed on the
   stack. *)

let set_params params =
  assert false (* TODO M1 *)


(* Fetching the result depends on the type of the function, or rather, the
   number of registers that are waiting for a value. *)

let fetch_result ret_regs start_lbl =
  assert false (* TODO M1 *)


(* When calling a function, we need to set its parameters in specific locations:
   the physical parameter registers as much as possible, and then the stack
   below. When the called function returns, we put the result where the calling
   function expect it to be. *)

let translate_call f args ret_regs start_lbl dest_lbl def =
  let nb_args = List.length args in
  add_translates
    ([adds_graph [ERTL.St_comment ("Starting a call", start_lbl)] ;
      adds_graph [ERTL.St_comment ("Setting up parameters", start_lbl)]] @
     set_params args @
     [adds_graph [ERTL.St_call (f, nb_args, start_lbl)] ;
      adds_graph [ERTL.St_comment ("Fetching result", start_lbl)] ;
      fetch_result ret_regs ;
      adds_graph [ERTL.St_comment ("End of call sequence", start_lbl)]])
    start_lbl dest_lbl def

let translate_tailcall f args start_lbl dummy_lbl def =
  let nb_args = List.length args in
  add_translates
    ([adds_graph [ERTL.St_comment ("Starting a call", start_lbl)] ;
      adds_graph [ERTL.St_comment ("Setting up parameters", start_lbl)]] @
     set_params args @
     [adds_graph [ERTL.St_tailcall (f, nb_args)] ;
      adds_graph [ERTL.St_comment ("End of call sequence", start_lbl)]])
    start_lbl dummy_lbl def


let translate_stmt lbl stmt def = match stmt with

  | RTL.St_skip lbl' ->
    add_graph lbl (ERTL.St_skip lbl') def

  | RTL.St_cost (cost_lbl, lbl') ->
    add_graph lbl (ERTL.St_cost (cost_lbl, lbl')) def

  | RTL.St_int (r, i, lbl') ->
    add_graph lbl (ERTL.St_int (r, i, lbl')) def

  | RTL.St_move (r1, r2, lbl') ->
    add_graph lbl (ERTL.St_move (r1, r2, lbl')) def

  | RTL.St_unop (unop, destr, srcr, lbl') ->
    add_graph lbl (ERTL.St_unop (unop, destr, srcr, lbl')) def

  | RTL.St_binop (binop, destr, srcr1, srcr2, lbl') ->
    add_graph lbl (ERTL.St_binop (binop, destr, srcr1, srcr2, lbl')) def

  | RTL.St_funaddr (destrs, f, lbl') ->
    let f i destr = ERTL.St_addrN (destr, f, i, lbl) in
    adds_graph (MiscPottier.mapi f destrs) lbl lbl' def

  | RTL.St_stackaddr (destrs, lbl') ->
    assert false (* TODO M1 *)

  | RTL.St_globaladdr (destrs, lbl') ->
    assert (List.length destrs = List.length Driver.TargetArch.gp) ;
    let f destr gp = ERTL.St_get_hdw (destr, gp, lbl) in
    adds_graph (List.map2 f destrs Driver.TargetArch.gp) lbl lbl' def

  | RTL.St_load (size, destr, addr, lbl') ->
    add_graph lbl (ERTL.St_load (size, destr, addr, lbl')) def

  | RTL.St_store (size, addr, srcr, lbl') ->
    add_graph lbl (ERTL.St_store (size, addr, srcr, lbl')) def

  | RTL.St_call (f, args, ret_regs, lbl') ->
    translate_call f args ret_regs lbl lbl' def

  | RTL.St_tailcall (f, args) ->
    translate_tailcall f args lbl lbl def

  | RTL.St_cond (srcr, lbl_true, lbl_false) ->
    add_graph lbl (ERTL.St_cond (srcr, lbl_true, lbl_false)) def

  | RTL.St_return ret_regs ->
    add_graph lbl (ERTL.St_return ret_regs) def


let translate_internal def =
  let nb_params = List.length (def.RTL.f_params) in
  (* The stack size is augmented by the number of parameters that cannot fit
     into physical registers. *)
  let target_arch_nb_params = List.length Driver.TargetArch.parameters in
  let added_stacksize = max 0 (nb_params - target_arch_nb_params) in
  let added_stacksize = added_stacksize * Driver.TargetArch.int_size in
  let def' =
    { ERTL.f_luniverse = def.RTL.f_luniverse ;
      ERTL.f_runiverse = def.RTL.f_runiverse ;
      ERTL.f_params    = nb_params ;
      (* ERTL does not know about parameter registers. We need to add them to
	 the locals. *)
      ERTL.f_locals    = Register.Set.union def.RTL.f_locals
	                 (Register.Set.of_list def.RTL.f_params) ;
      ERTL.f_stacksize = def.RTL.f_stacksize + added_stacksize ;
      ERTL.f_graph     = Label.Map.empty ;
      ERTL.f_entry     = def.RTL.f_entry ;
      ERTL.f_exit      = def.RTL.f_exit } in
  let def' = Label.Map.fold translate_stmt def.RTL.f_graph def' in
  let def' = add_pro_and_epilogue def.RTL.f_params def.RTL.f_result def' in
  def'


let translate_funct (id, def) =
  let def' = match def with
    | RTL.F_int def -> ERTL.F_int (translate_internal def)
    | RTL.F_ext def -> ERTL.F_ext def
  in
  (id, def')


(* Move the first cost label of each function at the beginning of the
   function. Indeed, the instructions for calling conventions (stack allocation
   for example) are added at the very beginning of the function, thus before the
   first cost label. *)

let generate stmt def =
  let entry = Label.Gen.fresh def.ERTL.f_luniverse in
  let def =
    { def with ERTL.f_graph = Label.Map.add entry stmt def.ERTL.f_graph } in
  { def with ERTL.f_entry = entry }

let find_and_remove_first_cost_label def =
  let rec aux lbl = match Label.Map.find lbl def.ERTL.f_graph with
    | ERTL.St_cost (cost_label, next_lbl) ->
      let graph = Label.Map.add lbl (ERTL.St_skip next_lbl) def.ERTL.f_graph in
      (Some cost_label, { def with ERTL.f_graph = graph })
    | ERTL.St_skip lbl | ERTL.St_comment (_, lbl) | ERTL.St_get_hdw (_, _, lbl)
    | ERTL.St_set_hdw (_, _, lbl) | ERTL.St_hdw_to_hdw (_, _, lbl)
    | ERTL.St_addrN (_, _, _, lbl) | ERTL.St_int (_, _, lbl)
    | ERTL.St_move (_, _, lbl) | ERTL.St_unop (_, _, _, lbl)
    | ERTL.St_binop (_, _, _, _, lbl) | ERTL.St_load (_, _, _, lbl)
    | ERTL.St_store (_, _, _, lbl) | ERTL.St_call (_, _, lbl)
    | ERTL.St_newframe lbl | ERTL.St_delframe lbl | ERTL.St_framesize (_, lbl)
      ->
      aux lbl
    | ERTL.St_tailcall (_, _) | ERTL.St_cond _ | ERTL.St_return _ ->
      (* No cost label found (no labelling performed). Indeed, the first cost
	 label must after some linear instructions. *)
      (None, def) in
  aux def.ERTL.f_entry

let move_first_cost_label_up_internal def =
  let (cost_label, def) = find_and_remove_first_cost_label def in
  match cost_label with
    | None -> def
    | Some cost_label ->
      generate (ERTL.St_cost (cost_label, def.ERTL.f_entry)) def

let move_first_cost_label_up (id, def) =
  let def' = match def with
    | ERTL.F_int int_fun ->
      ERTL.F_int (move_first_cost_label_up_internal int_fun)
    | _ -> def in
  (id, def')


let translate p =
  (* The tranformation on each RTL function: create an ERTL function and move
     its first cost label at the very beginning. *)
  let f funct = move_first_cost_label_up (translate_funct funct) in
  { ERTL.globals   = p.RTL.globals ;
    ERTL.functs = List.map f p.RTL.functs ;
    ERTL.main   = p.RTL.main }

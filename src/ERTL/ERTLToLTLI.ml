
(* Adapted from Pottier's PP compiler *)

(* This module translates [ERTL] statements into [LTL] statements. It is
   parameterized over a module [Env], whose signature appears below, which
   provides support for mapping pseudo-registers to stack slots or hardware
   registers and for generating instructions (which requires allocating fresh
   control flow graph labels). *)

type decision =
  | Spill of AST.immediate
  | Color of Driver.TargetArch.register

module Make (Env : sig

  val lookup: Register.t -> decision

  (* [generate stmt] returns a fresh statement label, which it associates with
     [stmt] in the control flow graph. *)

  val generate: LTL.statement -> Label.t

  val fresh_label: unit -> Label.t

  val add_graph: Label.t -> LTL.statement -> unit

  val locals: int

  val stacksize: int

end) = struct

  open Env

  let st0 = Driver.TargetArch.st0
  let st1 = Driver.TargetArch.st1
  let st2 = Driver.TargetArch.st2
  let st3 = Driver.TargetArch.st3
  let st4 = Driver.TargetArch.st4
  let st5 = Driver.TargetArch.st5
  let st6 = Driver.TargetArch.st6
  let st_addr0 = Driver.TargetArch.st_addr0
  let st_addr1 = Driver.TargetArch.st_addr1

  let change_label lbl = function
    | LTL.St_skip _ -> LTL.St_skip lbl
    | LTL.St_comment (s, _) -> LTL.St_comment (s, lbl)
    | LTL.St_cost (cost_lbl, _) -> LTL.St_cost (cost_lbl, lbl)
    | LTL.St_int (r, i, _) -> LTL.St_int (r, i, lbl)
    | LTL.St_addr (dstrs, id, _) -> LTL.St_addr (dstrs, id, lbl)
    | LTL.St_unop (unop, dstr, srcr, _) -> LTL.St_unop (unop, dstr, srcr, lbl)
    | LTL.St_binop (binop, dstr, srcr1, srcr2, _) ->
      LTL.St_binop (binop, dstr, srcr1, srcr2, lbl)
    | LTL.St_load (size, dstrs, addr, _) -> LTL.St_load (size, dstrs, addr, lbl)
    | LTL.St_store (size, addr, srcrs, _) ->
      LTL.St_store (size, addr, srcrs, lbl)
    | LTL.St_call (f, _) -> LTL.St_call (f, lbl)
    | LTL.St_tailcall _ | LTL.St_cond _ | LTL.St_return _ as inst -> inst

  (* Add a list of instruction in a graph. *)

  let adds_graph stmt_list dest_lbl =
    let stmt_list = List.rev stmt_list in
    let rec aux l = function
      | [] -> generate (LTL.St_skip l)
      | [stmt] -> generate (change_label l stmt)
      | stmt :: stmt_list -> aux (generate (change_label l stmt)) stmt_list in
    aux dest_lbl stmt_list

  let adds_graph_stmt stmt_list dest_lbl =
    LTL.St_skip (adds_graph stmt_list dest_lbl)

  let choose_rest rest1 rest2 = match rest1, rest2 with
    | [], _ -> rest2
    | _, [] -> rest1
    | _ -> assert false (* do not use on these arguments *)

  let move destr srcr dest_lbl =
    LTL.St_unop (Arch.UOpAddi 0, destr, srcr, dest_lbl)

  let translate_add destrs srcrs1 srcrs2 dest_lbl =
    if List.length destrs = 1 &&
       List.length srcrs1 = 1 &&
       List.length srcrs2 = 1 then
      let destr = List.hd destrs in
      let srcr1 = List.hd srcrs1 in
      let srcr2 = List.hd srcrs2 in
      [LTL.St_binop (Arch.OpAdd, destr, srcr1, srcr2, dest_lbl)]
    else
      let ((srcrs1_common, srcrs1_rest), (srcrs2_common, srcrs2_rest)) =
	MiscPottier.reduce srcrs1 srcrs2 in
      let srcrs_rest = choose_rest srcrs1_rest srcrs2_rest in
      let ((destrs_common, destrs_rest), _) =
	MiscPottier.reduce destrs srcrs1_common in
      let ((destrs_cted, destrs_rest), (srcrs_cted, _)) =
	MiscPottier.reduce destrs_rest srcrs_rest in
      let insts_init =
	[LTL.St_int (st0, 0, dest_lbl)] in
      let f_add destr srcr1 srcr2 =
	[move st1 srcr1 dest_lbl ;
	 move st2 srcr2 dest_lbl ;
	 LTL.St_binop (Arch.OpAdd, destr, srcr1, srcr2, dest_lbl) ;
	 LTL.St_binop (Arch.OpAdd, destr, destr, st0, dest_lbl) ;
	 LTL.St_binop (Arch.OpLtu, st3, destr, st1, dest_lbl) ;
	 LTL.St_binop (Arch.OpLtu, st0, destr, st2, dest_lbl) ;
	 LTL.St_binop (Arch.OpOr, st0, st3, st0, dest_lbl)] in
      let insts_add =
	List.flatten
	  (MiscPottier.map3 f_add destrs_common srcrs1_common srcrs2_common) in
      let f_add_cted destr srcr =
	[move st1 srcr dest_lbl ;
	 LTL.St_binop (Arch.OpAdd, destr, srcr, st0, dest_lbl) ;
	 LTL.St_binop (Arch.OpLtu, st3, destr, st1, dest_lbl) ;
	 LTL.St_binop (Arch.OpLtu, st0, destr, st0, dest_lbl) ;
	 LTL.St_binop (Arch.OpOr, st0, st3, st0, dest_lbl)] in
      let insts_add_cted =
	List.flatten (List.map2 f_add_cted destrs_cted srcrs_cted) in
      let f_rest destr =
	[LTL.St_unop (Arch.UOpAddi 0, destr, st0, dest_lbl) ;
	 LTL.St_unop (Arch.UOpSltiu 0, st3, destr, dest_lbl) ;
	 LTL.St_binop (Arch.OpLtu, st0, destr, st0, dest_lbl) ;
	 LTL.St_binop (Arch.OpOr, st0, st3, st0, dest_lbl)] in
      let insts_rest = List.flatten (List.map f_rest destrs_rest) in
      insts_init @ insts_add @ insts_add_cted @ insts_rest

  let adjust off = locals - (off + Driver.TargetArch.int_size)

  let get_stack r off l =
    let off = adjust off in
    adds_graph
      ([LTL.St_int (st4, off, l)] @
       (translate_add st_addr0 Driver.TargetArch.sp [st4] l) @
       [LTL.St_load (Driver.TargetArch.int_size, r, st_addr0, l)]) 
      l

  let set_stack off r l =
    let off = adjust off in
    adds_graph
      ([LTL.St_int (st4, off, l)] @
       (translate_add st_addr0 Driver.TargetArch.sp [st4] l) @
       [LTL.St_store (Driver.TargetArch.int_size, st_addr0, r, l)]) 
      l


  let write (r : Register.t) (l : Label.t)
      : (Driver.TargetArch.register * Label.t) =
    match lookup r with

      | Color hdw ->
	(* Pseudo-register [r] has been mapped to hardware register
	   [hdw]. Just write into [hdw] and branch to [l]. *)
	(hdw, l)

      | Spill off ->
	(* Pseudo-register [r] has been mapped to offset [off] in the local zone
	   of the stack. Then, write into [sst] (never allocated) and transfer
	   control to an instruction that copies [st4] in the designated
	   location of the stack before branching to [l]. *)
	(st5, set_stack off st5 l)


  let read_hdws decisions sts =
    let f decision st = match decision with
      | Color hdw -> hdw
      | Spill _ -> st in
    List.map2 f decisions sts

  let get_stacks decisions sts l =
    let f decision st l = match decision with
      | Color _ -> l
      | Spill off -> get_stack st off l in
    List.fold_right2 f decisions sts l

  let print_regs =
    MiscPottier.string_of_list " " Driver.TargetArch.print_register

  let read
      (sts : Driver.TargetArch.register list)
      (rs : Register.t list)
      (stmt : Driver.TargetArch.register list -> LTL.statement list)
      (l : Label.t) =
    assert (List.length sts = List.length rs) ;
    let decisions = List.map lookup rs in
    let read_hdws = read_hdws decisions sts in
    get_stacks decisions sts (adds_graph (stmt read_hdws) l)

(*
  let read
      (sts : Driver.TargetArch.register list)
      (rs : Register.t list)
      (stmt : Driver.TargetArch.register -> LTL.statement) =
    assert (List.length sts = List.length rs) ;
    match List.map lookup rs with
      | Color hdw ->
	(* Pseudo-register [r] has been mapped to hardware register [hdw]. Just
	   generate statement [stmt] with a reference to register [hdw]. *)
	generate (stmt hdw)

      | Spill off ->
	(* Pseudo-register [r] has been mapped to offset [off] in the local zone
	   of the stack. Issue a statement that copies the designated area in
	   the stack into the temporary unallocatable hardware register [st4],
	   then generate statement [stmt] with a reference to register
	   [st4]. *)
	let l = generate (stmt st) in
	get_stack st off l
*)


  let read1 r (stmt : Driver.TargetArch.register -> LTL.statement list) l =
    read [st5] [r]
      (fun hdws -> assert (List.length hdws >= 1); stmt (List.hd hdws)) l

  let read2 r1 r2
      (stmt : Driver.TargetArch.register ->
              Driver.TargetArch.register ->
              LTL.statement list)
      l =
    read [st5 ; st6] [r1 ; r2]
      (fun hdws ->
	assert (List.length hdws >= 2) ;
	stmt (List.nth hdws 0) (List.nth hdws 1)) l

  let read_addr addr
      (stmt : Driver.TargetArch.address -> LTL.statement list) l =
    read st_addr1 addr (fun hdws -> stmt hdws) l

  let read_reg_and_addr
      regs (stmt : Driver.TargetArch.register list -> LTL.statement list) l =
    assert (List.length regs = List.length st_addr1 + 1) ;
    read (st5 :: st_addr1) regs (fun hdws -> stmt hdws) l


  let move (dest : decision) (src : decision) l =
    match dest, src with

      (* Both pseudo-registers are translated to hardware registers. Issue move
	 statements, or no statement at all if both pseudo-registers reside in
	 the same hardware register. *)
      | Color desthdw, Color sourcehdw
	when Driver.TargetArch.eq_reg desthdw sourcehdw ->
	l
      | Color desthdw, Color sourcehdw ->
	generate (LTL.St_unop (Arch.UOpAddi 0, desthdw, sourcehdw, l))

      (* One pseudo-register is translated to a hardware register, while the
	 other is spilled. Issue stack access instructions. *)
      | Color desthdw, Spill off -> get_stack desthdw off l
      | Spill off, Color sourcehdw -> set_stack off sourcehdw l

      (* Both pseudo-registers are spilled. Combine the previous two cases. Of
	 course, if the two pseudo-registers have been spilled into the same
	 stack slot, there is nothing to do. *)
      | Spill off1, Spill off2 when off1 = off2 -> l
      | Spill off1, Spill off2 ->
	let temphdw = st5 in
	let l = set_stack off1 temphdw l in
	get_stack temphdw off2 l


  let newframe l =
    if stacksize = 0 then l
    else
      adds_graph
	([LTL.St_int (st5, -stacksize, l)] @
	 (translate_add Driver.TargetArch.sp Driver.TargetArch.sp [st5] l))
	l

  let delframe l =
    if stacksize = 0 then l
    else
      adds_graph
	([LTL.St_int (st5, stacksize, l)] @
	 (translate_add Driver.TargetArch.sp Driver.TargetArch.sp [st5] l))
	l


  (* ------------------------------------------------------------------------ *)

  (* [translate_statement] turns a [ERTL] statement into a [LTL] statement, or
     sequence of statements, that transfers control to the same label(s).

     Existing statement labels are preserved, that is, the labels in the new
     control flow graph form a superset of the labels in the existing control
     flow graph. *)

  let translate_statement (stmt : ERTL.statement) : LTL.statement =
    match stmt with

      | ERTL.St_skip l ->
	LTL.St_skip l

      | ERTL.St_comment (s, l) ->
	LTL.St_comment (s, l)

      | ERTL.St_cost (cost_lbl, l) ->
	LTL.St_cost (cost_lbl, l)

      | ERTL.St_newframe l ->
	LTL.St_skip (newframe l)

      | ERTL.St_delframe l ->
	LTL.St_skip (delframe l)

      | ERTL.St_framesize (r, l) ->
	let (hdw, l) = write r l in
	LTL.St_int (hdw, stacksize, l)

      | ERTL.St_get_hdw (destr, sourcehdw, l) ->
	LTL.St_skip (move (lookup destr) (Color sourcehdw) l)

      | ERTL.St_set_hdw (desthdw, sourcer, l) ->
	LTL.St_skip (move (Color desthdw) (lookup sourcer) l)

      | ERTL.St_hdw_to_hdw (r1, r2, l) ->
	LTL.St_unop (Arch.UOpAddi 0, r1, r2, l)

      | ERTL.St_move (r1, r2, l) ->
	LTL.St_skip (move (lookup r1) (lookup r2) l)

      | ERTL.St_int (r, i, l) ->
	assert false (* TODO M1 *)

      | ERTL.St_unop (unop, destr, srcr, l) ->
	let (dhdw, l) = write destr l in
	LTL.St_skip
	  (read1 srcr (fun shdw -> [LTL.St_unop (unop, dhdw, shdw, l)]) l)

      | ERTL.St_binop (binop, destr, sourcer1, sourcer2, l) ->
	assert false (* TODO M1 *)

      | ERTL.St_addrN (r, id, n, l) ->
	assert (List.length st_addr0 >= n) ;
	let (hdw, l) = write r l in
	adds_graph_stmt
	  [LTL.St_addr (st_addr0, id, l) ;
	   LTL.St_unop (Arch.UOpAddi 0, hdw, List.nth st_addr0 n, l)]
	  l

      | ERTL.St_load (size, destr, addr, l) ->
	let (desthdw, l) = write destr l in
	let l =
	  read_addr addr
	    (fun hdws -> [LTL.St_load (size, desthdw, hdws, l)]) l in
	LTL.St_skip l

      | ERTL.St_store (size, addr, srcr, l) ->
	let l =
	  read_reg_and_addr (srcr :: addr)
	    (fun hdws ->
	      assert (List.length hdws >= 1) ;
	      [LTL.St_store (size, List.tl hdws, List.hd hdws, l)]) l in
	LTL.St_skip l

      | ERTL.St_call (f, _, l) ->
	let l = read_addr f (fun hdws -> [LTL.St_call (hdws, l)]) l in
	LTL.St_skip l

      | ERTL.St_tailcall (f, _) ->
	let l =
	  read_addr f
	    (fun hdws ->
	      [LTL.St_skip (delframe "") ; LTL.St_tailcall hdws]) "" in
	LTL.St_skip l

      | ERTL.St_cond (srcr, lbl_true, lbl_false) ->
	let l =
	  read1 srcr
	    (fun hdw -> [LTL.St_cond (hdw, lbl_true, lbl_false)]) lbl_true in
	LTL.St_skip l

      | ERTL.St_return _ ->
	LTL.St_return

(* ------------------------------------------------------------------------- *)

end

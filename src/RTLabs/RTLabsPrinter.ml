
let n_spaces n = String.make n ' '


let rec print_size = function
  | AST.SQ q -> Memory.string_of_quantity q
  | AST.SProd l -> "struct {" ^ (print_size_list l) ^ "}"
  | AST.SSum l -> "union {" ^ (print_size_list l) ^ "}"
  | AST.SArray (i, se) ->
    (print_size se) ^ "[" ^ (string_of_int i) ^ "]"
and print_size_list l =
  MiscPottier.string_of_list ", " print_size l

let print_global n (x, size) =
  Printf.sprintf "%s\"%s\" { %s }" (n_spaces n) x (print_size size)

let print_globals n globs =
  Printf.sprintf "%sglobals:\n%s"
    (n_spaces n)
    (List.fold_left (fun s g -> s ^ (print_global (n+2) g) ^ "\n") "" globs)


let print_reg = Register.print

let print_oreg = function
  | None -> "_"
  | Some r -> print_reg r

let print_decl (r, t) =
  (Primitive.print_type t) ^ " " ^ (Register.print r)

let rec print_args args =
  Printf.sprintf "[%s]" (MiscPottier.string_of_list ", " print_reg args)

let print_result = function
  | None -> "_"
  | Some (r, t) -> (Primitive.print_type t) ^ " " ^ (Register.print r)

let print_params r =
  Printf.sprintf "(%s)" (MiscPottier.string_of_list ", " print_decl r)

let print_locals r =
  Printf.sprintf "%s" (MiscPottier.string_of_list ", " print_decl r)


let print_cmp = function
  | AST.Cmp_eq -> "eq"
  | AST.Cmp_ne -> "ne"
  | AST.Cmp_gt -> "gt"
  | AST.Cmp_ge -> "ge"
  | AST.Cmp_lt -> "lt"
  | AST.Cmp_le -> "le"

let rec print_size = function
  | AST.SQ q -> Memory.string_of_quantity q
  | AST.SProd l -> "struct {" ^ (print_size_list l) ^ "}"
  | AST.SSum l -> "union {" ^ (print_size_list l) ^ "}"
  | AST.SArray (i, se) ->
    (print_size se) ^ "[" ^ (string_of_int i) ^ "]"
and print_size_list l =
  MiscPottier.string_of_list ", " print_size l

let print_stacksize = print_size

let print_offset (size, depth) =
  (print_size size) ^ ", " ^ (string_of_int depth)

let print_sizeof = print_size

let print_cst = function
  | AST.Cst_int i -> Printf.sprintf "imm_int %d" i
  | AST.Cst_float f -> Printf.sprintf "imm_float %f" f
  | AST.Cst_addrsymbol id -> Printf.sprintf "imm_addr \"%s\"" id
  | AST.Cst_stack -> "imm_addr STACK"
  | AST.Cst_offset off -> Printf.sprintf "imm_offset { %s }" (print_offset off)
  | AST.Cst_sizeof t -> "imm_sizeof (" ^ (print_size t) ^ ")"

let string_of_signedness = function
  | AST.Signed -> "s"
  | AST.Unsigned -> "u"

let string_of_int_type (size, sign) =
  Printf.sprintf "%d%s" size (string_of_signedness sign)

let print_op1 = function
  | AST.Op_cast (int_type, dest_size) ->
    Printf.sprintf "int%sto%d" (string_of_int_type int_type) dest_size
  | AST.Op_negint -> "negint"
  | AST.Op_notbool -> "notbool"
  | AST.Op_notint -> "notint"
  | AST.Op_id -> "id"
  | AST.Op_ptrofint -> "ptrofint"
  | AST.Op_intofptr -> "intofptr"

let print_op2 = function
  | AST.Op_add -> "add"
  | AST.Op_sub -> "sub"
  | AST.Op_mul -> "mul"
  | AST.Op_div -> "div"
  | AST.Op_divu -> "/u"
  | AST.Op_mod -> "mod"
  | AST.Op_modu -> "modu"
  | AST.Op_and -> "and"
  | AST.Op_or -> "or"
  | AST.Op_xor -> "xor"
  | AST.Op_shl -> "shl"
  | AST.Op_shr -> "shr"
  | AST.Op_shru -> "shru"
  | AST.Op_cmp cmp -> print_cmp cmp
  | AST.Op_addp -> "addp"
  | AST.Op_subp -> "subp"
  | AST.Op_subpp -> "subpp"
  | AST.Op_cmpp cmp -> (print_cmp cmp) ^ "p"
  | AST.Op_cmpu cmp -> (print_cmp cmp) ^ "u"


(*
let print_addressing = function
  | RTLabs.Aindexed off -> Printf.sprintf "{ %s }" (print_offset off)
  | RTLabs.Aindexed2 -> "add"
  | RTLabs.Aglobal (id, off) ->
    Printf.sprintf "{ %s }(\"%s\")" (print_offset off) id
  | RTLabs.Abased (id, off) ->
    Printf.sprintf "add, { %s }(\"%s\")" (print_offset off) id
  | RTLabs.Ainstack off -> Printf.sprintf "{ %s }(STACK)" (print_offset off)
*)


let rec print_table = function
  | [] -> ""
  | [lbl] -> lbl
  | lbl :: tbl -> lbl ^ ", " ^ (print_table tbl)


let print_statement = function
  | RTLabs.St_skip lbl -> "--> " ^ lbl
  | RTLabs.St_cost (cost_lbl, lbl) ->
      Printf.sprintf "emit %s --> %s" cost_lbl lbl
  | RTLabs.St_cst (destr, cst, lbl) ->
      Printf.sprintf "imm %s, %s --> %s"
	(print_reg destr)
	(print_cst cst)
	lbl
  | RTLabs.St_op1 (op1, destr, srcr, lbl) ->
      Printf.sprintf "%s %s, %s --> %s"
	(print_op1 op1)
	(print_reg destr)
	(print_reg srcr)
	lbl
  | RTLabs.St_op2 (op2, destr, srcr1, srcr2, lbl) ->
      Printf.sprintf "%s %s, %s, %s --> %s"
	(print_op2 op2)
	(print_reg destr)
	(print_reg srcr1)
	(print_reg srcr2)
	lbl
  | RTLabs.St_addi (i, srcr, destr, lbl) ->
      Printf.sprintf "addi %s, %s, %s --> %s"
      (print_reg destr)
      (print_reg srcr)
      (print_cst i)
      lbl
  | RTLabs.St_load (q, addr, destr, lbl) ->
      Printf.sprintf "load %s, %s, %s --> %s"
	(Memory.string_of_quantity q)
	(print_reg addr)
	(print_reg destr)
	lbl
  | RTLabs.St_loadi (q, i, addr, destr, lbl) ->
      Printf.sprintf "lw %s, %s(%s) --> %s"
      (print_reg destr)
      (print_cst i)
      (print_reg addr)
      lbl
  | RTLabs.St_store (q, addr, srcr, lbl) ->
      Printf.sprintf "store %s, %s, %s --> %s"
	(Memory.string_of_quantity q)
	(print_reg addr)
	(print_reg srcr)
	lbl
	| RTLabs.St_storei (q, i, addr, srcr, lbl) ->
      Printf.sprintf "sw %s, %s(%s) --> %s"
      (print_reg srcr)
      (print_cst i)
      (print_reg addr)
      lbl
  | RTLabs.St_call_id (f, args, res, sg, lbl) ->
      Printf.sprintf "call \"%s\", %s, %s: %s --> %s"
	f
	(print_args args)
	(print_oreg res)
	(Primitive.print_sig sg)
	lbl
  | RTLabs.St_call_ptr (f, args, res, sg, lbl) ->
      Printf.sprintf "call_ptr %s, %s, %s: %s --> %s"
	(print_reg f)
	(print_args args)
	(print_oreg res)
	(Primitive.print_sig sg)
	lbl
  | RTLabs.St_tailcall_id (f, args, sg) ->
      Printf.sprintf "tailcall \"%s\", %s: %s"
	f
	(print_args args)
	(Primitive.print_sig sg)
  | RTLabs.St_tailcall_ptr (f, args, sg) ->
      Printf.sprintf "tailcall_ptr \"%s\", %s: %s"
	(print_reg f)
	(print_args args)
	(Primitive.print_sig sg)
  | RTLabs.St_cond (r, lbl_true, lbl_false) ->
      Printf.sprintf "%s? --> %s, %s"
	(print_reg r)
	lbl_true
	lbl_false
  | RTLabs.St_cond_cmpz (cmp, srcr, lbl_true, lbl_false) ->
      Printf.sprintf "b%sz %s? --> %s, %s"
      (print_cmp cmp)
      (print_reg srcr)
      lbl_true
      lbl_false
  | RTLabs.St_cond_cmp (cmp, srcr1, srcr2, lbl_true, lbl_false) ->
      Printf.sprintf "b%s %s, %s? --> %s, %s"
      (print_cmp cmp)
      (print_reg srcr1)
      (print_reg srcr2)
      lbl_true
      lbl_false
(*
  | RTLabs.St_condcst (cst, t, lbl_true, lbl_false) ->
      Printf.sprintf "(%s) %s --> %s, %s"
	(Primitive.print_type t)
	(print_cst cst)
	lbl_true
	lbl_false
  | RTLabs.St_cond1 (op1, srcr, lbl_true, lbl_false) ->
      Printf.sprintf "%s %s --> %s, %s"
	(print_op1 op1)
	(print_reg srcr)
	lbl_true
	lbl_false
  | RTLabs.St_cond2 (op2, srcr1, srcr2, lbl_true, lbl_false) ->
      Printf.sprintf "%s %s, %s --> %s, %s"
	(print_op2 op2)
	(print_reg srcr1)
	(print_reg srcr2)
	lbl_true
	lbl_false
*)
  | RTLabs.St_jumptable (r, tbl) ->
      Printf.sprintf "j_tbl %s --> %s"
	(print_reg r)
	(print_table tbl)
  | RTLabs.St_return None -> Printf.sprintf "return"
  | RTLabs.St_return (Some r) -> Printf.sprintf "return %s" (print_reg r)


let print_graph n c =
  let f lbl stmt s =
    Printf.sprintf "%s%s: %s\n%s"
      (n_spaces n)
      lbl
      (print_statement stmt)
      s in
  Label.Map.fold f c ""


let print_internal_decl n f def =

  Printf.sprintf
    "%s\"%s\"%s\n%slocals: %s\n%sresult: %s\n%sstacksize: %s\n%sentry: %s\n%sexit: %s\n\n%s"
    (n_spaces n)
    f
    (print_params def.RTLabs.f_params)
    (n_spaces (n+2))
    (print_locals def.RTLabs.f_locals)
    (n_spaces (n+2))
    (print_result def.RTLabs.f_result)
    (n_spaces (n+2))
    (print_stacksize def.RTLabs.f_stacksize)
    (n_spaces (n+2))
    def.RTLabs.f_entry
    (n_spaces (n+2))
    def.RTLabs.f_exit
    (print_graph (n+2) def.RTLabs.f_graph)


let print_external_decl n f def =
  Printf.sprintf "%sextern \"%s\": %s\n"
    (n_spaces n)
    f
    (Primitive.print_sig def.AST.ef_sig)


let print_fun_decl n (f, def) = match def with
  | RTLabs.F_int def -> print_internal_decl n f def
  | RTLabs.F_ext def -> print_external_decl n f def

let print_fun_decls n functs =
  List.fold_left (fun s f -> s ^ (print_fun_decl n f) ^ "\n\n") ""
    functs


let print_program p =
  Printf.sprintf "program:\n\n\n%s\n\n%s"
    (print_globals 2 p.RTLabs.vars)
    (print_fun_decls 2 p.RTLabs.functs)

open AST


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
  "offset[" ^ (print_size size) ^ ", " ^ (string_of_int depth) ^ "]"

let print_sizeof = print_size

let print_global_size = print_size

let print_data = function
(*
  | Data_reserve n -> Printf.sprintf "[%d]" n
*)
  | Data_int8 i -> Printf.sprintf "(int8) %d" i
  | Data_int16 i -> Printf.sprintf "(int16) %d" i
  | Data_int32 i -> Printf.sprintf "%d" i
  | Data_float32 f -> Printf.sprintf "%f" f
  | Data_float64 f -> Printf.sprintf "(float64) %f" f

let print_datas init =
  let rec aux = function
    | [] -> ""
    | [data] -> print_data data
    | data :: datas -> Printf.sprintf "%s, %s" (print_data data) (aux datas)
  in
  Printf.sprintf "{%s}" (aux init)

let print_datas_opt = function
  | None -> ""
  | Some init -> " = " ^ (print_datas init)

let print_var (id, size, init_opt) =
  Printf.sprintf "var \"%s\" : %s%s;\n"
    id (print_global_size size) (print_datas_opt init_opt)

let print_vars = List.fold_left (fun s v -> s ^ (print_var v)) ""

let print_constant = function
  | Cst_int i -> string_of_int i
  | Cst_float f -> string_of_float f
  | Cst_addrsymbol id -> "\"" ^ id ^ "\""
  | Cst_stack -> "&0"
  | Cst_offset off -> "{" ^ (print_offset off) ^ "}"
  | Cst_sizeof t -> "sizeof (" ^ (print_sizeof t) ^ ")"

let print_cmp = function
  | Cmp_eq -> "=="
  | Cmp_ne -> "!="
  | Cmp_gt -> ">"
  | Cmp_ge -> ">="
  | Cmp_lt -> "<"
  | Cmp_le -> "<="

let print_op1 = function
  | Op_cast ((src_size, sign), dest_size) ->
    Printf.sprintf "int%s%sto%s"
      (Primitive.print_size src_size)
      (Primitive.print_signedness sign)
      (Primitive.print_size dest_size)
  | Op_negint -> "-"
  | Op_notbool -> "!"
  | Op_notint -> "~"
  | Op_id -> ""
  | Op_intofptr -> "intofptr"
  | Op_ptrofint -> "ptrofint"

let print_op2 = function
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | Op_divu -> "/u"
  | Op_mod -> "%"
  | Op_modu -> "%u"
  | Op_and -> "&&"
  | Op_or -> "||"
  | Op_xor -> "^"
  | Op_shl -> "<<"
  | Op_shr -> ">>"
  | Op_shru -> ">>u"
  | Op_cmp cmp -> print_cmp cmp
  | Op_cmpu cmp -> (print_cmp cmp) ^ "u"
  | Op_addp -> "+p"
  | Op_subp -> "-p"
  | Op_subpp -> "-pp"
  | Op_cmpp cmp -> (print_cmp cmp) ^ "p"

let rec print_expression (Cminor.Expr (ed, _)) = match ed with
  | Cminor.Id id -> id
  | Cminor.Cst cst -> print_constant cst
  | Cminor.Op1 (op1, e) ->
      Printf.sprintf "%s %s" (print_op1 op1) (add_parenthesis e)
  | Cminor.Op2 (op2, e1, e2) ->
      Printf.sprintf "%s %s %s"
	(add_parenthesis e1)
	(print_op2 op2)
	(add_parenthesis e2)
  | Cminor.Mem (q, e) ->
      Printf.sprintf "%s[%s]" (Memory.string_of_quantity q) (print_expression e)
  | Cminor.Cond (e1, e2, e3) ->
      Printf.sprintf "%s ? %s : %s"
	(add_parenthesis e1)
	(add_parenthesis e2)
	(add_parenthesis e3)
  | Cminor.Exp_cost (lab, e) ->
      Printf.sprintf "/* %s */ %s" lab (print_expression e)
and add_parenthesis (Cminor.Expr (ed, _) as e) = match ed with
  | Cminor.Id _ | Cminor.Cst _ | Cminor.Mem _ -> print_expression e
  | _ -> Printf.sprintf "(%s)" (print_expression e)


let print_args  =
  MiscPottier.string_of_list ", " print_expression

let print_decl (x, t) = (Primitive.print_type t) ^ " " ^ x

let print_decls vars =
  MiscPottier.string_of_list ", " print_decl vars


let n_spaces n = String.make n ' '


let print_table n =
  let f s (case, exit) =
    Printf.sprintf "%s%scase %d: exit %d;\n" s (n_spaces n) case exit
  in
  List.fold_left f ""


let rec print_body n = function
  | Cminor.St_skip -> ""
  | Cminor.St_assign (id, e) ->
      Printf.sprintf "%s%s = %s;\n" (n_spaces n) id (print_expression e)
  | Cminor.St_store (q, e1, e2) ->
      Printf.sprintf "%s%s[%s] = %s;\n"
	(n_spaces n)
	(Memory.string_of_quantity q)
	(print_expression e1)
	(print_expression e2)
  | Cminor.St_call (None, f, args, sg) ->
      Printf.sprintf "%s%s(%s) : %s;\n"
	(n_spaces n)
	(print_expression f)
	(print_args args)
	(Primitive.print_sig sg)
  | Cminor.St_call (Some id, f, args, sg) ->
      Printf.sprintf "%s%s = %s(%s) : %s;\n"
	(n_spaces n)
	id
	(print_expression f)
	(print_args args)
	(Primitive.print_sig sg)
  | Cminor.St_tailcall (f, args, sg) ->
      Printf.sprintf "%stailcall %s(%s) : %s;\n"
	(n_spaces n)
	(print_expression f)
	(print_args args)
	(Primitive.print_sig sg)
  | Cminor.St_seq (s1, s2) -> (print_body n s1) ^ (print_body n s2)
  | Cminor.St_ifthenelse (e, s1, s2) ->
      Printf.sprintf "%sif (%s) {\n%s%s}\n%selse {\n%s%s}\n"
	(n_spaces n)
	(print_expression e)
	(print_body (n+2) s1)
	(n_spaces n)
	(n_spaces n)
	(print_body (n+2) s2)
	(n_spaces n)
  | Cminor.St_loop s ->
      Printf.sprintf "%sloop {\n%s%s}\n"
	(n_spaces n)
	(print_body (n+2) s)
	(n_spaces n)
  | Cminor.St_block s ->
      Printf.sprintf "%sblock {\n%s%s}\n"
	(n_spaces n)
	(print_body (n+2) s)
	(n_spaces n)
  | Cminor.St_exit i ->
      Printf.sprintf "%sexit %d;\n" (n_spaces n) i
  | Cminor.St_switch (e, tbl, dflt) ->
      Printf.sprintf "%sswitch (%s) {\n%s%sdefault: exit %d;\n%s}\n"
	(n_spaces n)
	(print_expression e)
	(print_table ( n+2) tbl)
	(n_spaces (n+2))
	dflt
	(n_spaces n)
  | Cminor.St_return None -> Printf.sprintf "%sreturn;\n" (n_spaces n)
  | Cminor.St_return (Some e) ->
      Printf.sprintf "%sreturn %s;\n" (n_spaces n) (print_expression e)
  | Cminor.St_label (lbl, s) ->
      Printf.sprintf "%s%s:\n%s" (n_spaces n) lbl (print_body n s)
  | Cminor.St_goto lbl ->
      Printf.sprintf "%sgoto %s;\n" (n_spaces n) lbl
  | Cminor.St_cost (lbl, s) ->
      Printf.sprintf "%s%s:\n%s"
	(n_spaces n) lbl (print_body n s)

let print_internal f_name f_def =
  Printf.sprintf "\"%s\" (%s) : %s {\n\n  stack: %s\n\n  vars: %s;\n\n%s}\n\n\n"
    f_name
    (print_decls f_def.Cminor.f_params)
    (Primitive.print_type_return f_def.Cminor.f_return)
    (print_stacksize f_def.Cminor.f_stacksize)
    (print_decls f_def.Cminor.f_vars)
    (print_body 2 f_def.Cminor.f_body)


let print_external f_name f_def =
  Printf.sprintf "extern \"%s\" : %s\n\n\n"
    f_name
    (Primitive.print_sig f_def.ef_sig)


let print_funct (f_name, f_def) = match f_def with
  | Cminor.F_int f_def -> print_internal f_name f_def
  | Cminor.F_ext f_def -> print_external f_name f_def

let print_functs = List.fold_left (fun s f -> s ^ (print_funct f)) ""

let print_program p =
  Printf.sprintf "\n%s\n\n%s"
    (print_vars p.Cminor.vars)
    (print_functs p.Cminor.functs)

let string_of_statement s = match s with
    Cminor.St_skip -> "skip"
  | Cminor.St_assign(_,_) -> "assign"
  | Cminor.St_store(_,_,_) -> "store"
  | Cminor.St_call(_,_,_,_) -> "call"
  | Cminor.St_tailcall(_,_,_) -> "tailcall"
  | Cminor.St_seq(_,_) -> "seq"
  | Cminor.St_ifthenelse(_,_,_) -> "ifthenelse"
  | Cminor.St_loop(_) -> "loop"
  | Cminor.St_block(_) -> "block"
  | Cminor.St_exit(_) -> "exit"
  | Cminor.St_switch(_,_,_) -> "switch"
  | Cminor.St_return(_) -> "return"
  | Cminor.St_label(_,_) -> "label"
  | Cminor.St_goto(_) -> "goto"
  | Cminor.St_cost(_,_) -> "cost"

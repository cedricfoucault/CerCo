

let error_prefix = "Clight to Cminor"
let error = Error.global_error error_prefix
let error_float () = error "float not supported."


(* General helpers *)

let clight_type_of (Clight.Expr (_, t)) = t

let cminor_type_of (Cminor.Expr (_, t)) = t


(* Translate types *)

let byte_size_of_intsize = function
  | Clight.I8 -> 1
  | Clight.I16 -> 2
  | Clight.I32 -> 4

let sig_type_of_ctype = function
  | Clight.Tvoid -> assert false (* do not use on this argument *)
  | Clight.Tint (intsize, sign) ->
    AST.Sig_int (byte_size_of_intsize intsize, sign)
  | Clight.Tfloat _ -> error_float ()
  | Clight.Tfunction _ | Clight.Tstruct _ | Clight.Tunion _
  | Clight.Tpointer _ | Clight.Tarray _ | Clight.Tcomp_ptr _ -> AST.Sig_ptr

let translate_args_types = List.map sig_type_of_ctype

let type_return_of_ctype = function
  | Clight.Tvoid -> AST.Type_void
  | t -> AST.Type_ret (sig_type_of_ctype t)

let quantity_of_sig_type = function
  | AST.Sig_int (size, _) -> AST.QInt size
  | AST.Sig_float _ -> error_float ()
  | AST.Sig_offset -> AST.QOffset
  | AST.Sig_ptr -> AST.QPtr

let quantity_of_ctype t = quantity_of_sig_type (sig_type_of_ctype t)

let rec sizeof_ctype = function
  | Clight.Tvoid | Clight.Tfunction _ -> AST.SQ (AST.QInt 1)
  | Clight.Tfloat _ -> error_float ()
  | Clight.Tint (size, _) -> AST.SQ (AST.QInt (byte_size_of_intsize size))
  | Clight.Tpointer _
  | Clight.Tcomp_ptr _ -> AST.SQ AST.QPtr
  | Clight.Tarray (t, n) -> AST.SArray (n, sizeof_ctype t)
  | Clight.Tstruct (_, fields) ->
    AST.SProd (List.map sizeof_ctype (List.map snd fields))
  | Clight.Tunion (_, fields) ->
    AST.SSum (List.map sizeof_ctype (List.map snd fields))

let global_size_of_ctype = sizeof_ctype


(** Helpers on abstract sizes and offsets *)

let max_stacksize size1 size2 = match size1, size2 with
  | AST.SProd l1, AST.SProd l2 when List.length l1 > List.length l2 -> size1
  | AST.SProd l1, AST.SProd l2 -> size2
  | _ -> raise (Failure "ClightToCminor.max_stacksize")

(** Hypothesis: [offset1] is a prefix of [offset2] or vice-versa. *)
let max_offset offset1 offset2 =
  if List.length offset1 > List.length offset2 then offset1
  else offset2

let next_depth = function
  | AST.SProd l -> List.length l
  | _ -> raise (Failure "ClightToCminor.next_offset")

let add_stack offset =
  let e1 = Cminor.Expr (Cminor.Cst AST.Cst_stack, AST.Sig_ptr) in
  let e2 = Cminor.Expr (Cminor.Cst (AST.Cst_offset offset), AST.Sig_offset) in
  Cminor.Op2 (AST.Op_addp, e1, e2)

let add_stacksize t = function
  | AST.SProd l -> AST.SProd (l @ [sizeof_ctype t])
  | _ -> raise (Failure "ClightToCminor.add_stacksize")

let struct_depth field fields =
  let rec aux i = function
    | [] -> error ("unknown field " ^ field ^ ".")
    | (field', t) :: _ when field' = field -> i
    | (_, t) :: fields -> aux (i+1) fields in
  aux 0 fields

let struct_offset t field fields =
  let size = sizeof_ctype t in
  let depth = struct_depth field fields in
  let offset = (size, depth) in
  let t = AST.Sig_offset in
  Cminor.Expr (Cminor.Cst (AST.Cst_offset offset), t)


(** Sort variables: locals, parameters, globals, in stack. *)

type location = 
  | Local
  | LocalStack of AST.abstract_offset
  | Param
  | ParamStack of AST.abstract_offset
  | Global

(** Below are some helper definitions to ease the manipulation of a translation
    environment for variables. *)

type var_locations = (location * Clight.ctype) StringTools.Map.t

let empty_var_locs : var_locations = StringTools.Map.empty

let add_var_locs : AST.ident -> (location * Clight.ctype) -> var_locations ->
  var_locations =
  StringTools.Map.add

let mem_var_locs : AST.ident -> var_locations -> bool = StringTools.Map.mem

let find_var_locs : AST.ident -> var_locations -> (location * Clight.ctype) =
  StringTools.Map.find

let fold_var_locs : (AST.ident -> (location * Clight.ctype) -> 'a -> 'a) ->
  var_locations -> 'a -> 'a =
  StringTools.Map.fold


let is_local_or_param id var_locs = match find_var_locs id var_locs with
  | (Local, _) | (Param, _) -> true
  | _ -> false

let get_locals var_locs =
  let f id (location, ctype) locals =
    let added = match location with
      | Local -> [(id, sig_type_of_ctype ctype)]
      | _ -> [] in
    locals @ added in
  fold_var_locs f var_locs []

let get_stacksize var_locs =
  let f _ (location, _) res = match location with
    | LocalStack (stacksize, _) | ParamStack (stacksize, _) ->
      max_stacksize res stacksize
    | _ -> res in
  fold_var_locs f var_locs (AST.SProd [])


(* Variables of a function that will go in stack: variables of a complex type
   (array, structure or union) and variables whose address is evaluated. *)

let is_function_ctype = function
  | Clight.Tfunction _ -> true
  | _ -> false

let is_scalar_ctype : Clight.ctype -> bool = function
  | Clight.Tint _ | Clight.Tfloat _ | Clight.Tpointer _ -> true
  | _ -> false

let is_complex_ctype : Clight.ctype -> bool = function
  | Clight.Tarray _ | Clight.Tstruct _ | Clight.Tunion _ | Clight.Tfunction _ ->
    true
  | _ -> false

let complex_ctype_vars cfun =
  let f set (x, t) =
    if is_complex_ctype t then StringTools.Set.add x set else set in
  (* Because of CIL, parameters should not have a complex type, but let's add
     them just in case. *)
  List.fold_left f StringTools.Set.empty
    (cfun.Clight.fn_params @ cfun.Clight.fn_vars)

let union_list = List.fold_left StringTools.Set.union StringTools.Set.empty

let addr_vars_fun_expr (Clight.Expr (ed, _)) sub_exprs_res =
  let res_ed = match ed with
    | Clight.Eaddrof (Clight.Expr (Clight.Evar id, _)) ->
      StringTools.Set.singleton id
    | _ -> StringTools.Set.empty in
  union_list (res_ed :: sub_exprs_res)

let addr_vars_fun_stmt _ sub_exprs_res sub_stmts_res =
  union_list (sub_exprs_res @ sub_stmts_res)

let addr_vars_fun cfun =
  ClightFold.statement2
    addr_vars_fun_expr addr_vars_fun_stmt cfun.Clight.fn_body

let stack_vars cfun =
  StringTools.Set.union (complex_ctype_vars cfun) (addr_vars_fun cfun)


let sort_stacks stack_location vars var_locs =
  let stacksize = get_stacksize var_locs in
  let f (current_stacksize, var_locs) (id, t) =
    let depth = next_depth current_stacksize in
    let current_stacksize = add_stacksize t current_stacksize in
    let offset = (current_stacksize, depth) in
    let var_locs = add_var_locs id (stack_location offset, t) var_locs in
    (current_stacksize, var_locs) in
  snd (List.fold_left f (stacksize, var_locs) vars)

let sort_normals normal_location vars var_locs =
  let f var_locs (id, ctype) =
    add_var_locs id (normal_location, ctype) var_locs in
  List.fold_left f var_locs vars

let sort_vars normal_location stack_location_opt stack_vars vars var_locs =
  let f_stack (x, _) = StringTools.Set.mem x stack_vars in
  let (f_normal, var_locs) = match stack_location_opt with
    | None -> ((fun _ -> true), var_locs)
    | Some stack_location ->
      ((fun var -> not (f_stack var)),
       sort_stacks stack_location (List.filter f_stack vars) var_locs) in
  sort_normals normal_location (List.filter f_normal vars) var_locs

let sort_locals = sort_vars Local (Some (fun offset -> LocalStack offset))

let sort_params = sort_vars Param (Some (fun offset -> ParamStack offset))

let sort_globals stack_vars globals var_locs =
  let globals = List.map (fun ((id, _), ctype) -> (id, ctype)) globals in
  sort_vars Global None stack_vars globals var_locs

(* The order of insertion in the sorting environment is important: it follows
   the scope conventions of C. Local variables hide parameters that hide
   globals. *)

let sort_variables globals cfun =
  let stack_vars = stack_vars cfun in
  let var_locs = empty_var_locs in
  let var_locs = sort_globals stack_vars globals var_locs in
  let var_locs = sort_params stack_vars cfun.Clight.fn_params var_locs in
  let var_locs = sort_locals stack_vars cfun.Clight.fn_vars var_locs in
  var_locs


(* Translate globals *)

let init_to_data = function
  | [Clight.Init_space _] -> None
  | l -> Some (List.map (
  function 
    | Clight.Init_int8 i       -> AST.Data_int8 i 
    | Clight.Init_int16 i      -> AST.Data_int16 i 
    | Clight.Init_int32 i      -> AST.Data_int32 i
    | Clight.Init_float32 _ 
    | Clight.Init_float64 _    -> error_float ()
    | Clight.Init_space n      -> error "bad global initialization style."
    | Clight.Init_addrof (_,_) -> assert false (*TODO*)
) l)

let translate_global ((id,lst),t) = (id,global_size_of_ctype t,init_to_data lst)


(* Translate expression *)

let translate_unop = function
  | Clight.Onotbool -> AST.Op_notbool
  | Clight.Onotint -> AST.Op_notint
  | Clight.Oneg -> AST.Op_negint

let esizeof_ctype res_type t =
  Cminor.Expr (Cminor.Cst (AST.Cst_sizeof (sizeof_ctype t)), res_type)

let translate_add res_type ctype1 ctype2 e1 e2 = match ctype1, ctype2 with
  | Clight.Tint _, Clight.Tint _ ->
    Cminor.Expr (Cminor.Op2 (AST.Op_add, e1, e2), res_type)
  | Clight.Tfloat _, Clight.Tfloat _ -> error_float ()
  | Clight.Tpointer t, Clight.Tint _
  | Clight.Tarray (t, _), Clight.Tint _ ->
    let t2 = cminor_type_of e2 in
    let size = esizeof_ctype t2 t in
    let index = Cminor.Expr (Cminor.Op2 (AST.Op_mul, e2, size), t2) in
    Cminor.Expr (Cminor.Op2 (AST.Op_addp, e1, index), res_type)
  | Clight.Tint _, Clight.Tpointer t
  | Clight.Tint _, Clight.Tarray (t, _) -> 
    let t1 = cminor_type_of e1 in
    let size = esizeof_ctype t1 t in
    let index = Cminor.Expr (Cminor.Op2 (AST.Op_mul, e1, size), t1) in
    Cminor.Expr (Cminor.Op2 (AST.Op_addp, e2, index), res_type)
  | _ -> error "type error."

let translate_sub res_type ctype1 ctype2 e1 e2 = match ctype1, ctype2 with
  | Clight.Tint _, Clight.Tint _ ->
    Cminor.Expr (Cminor.Op2 (AST.Op_sub, e1, e2), res_type)
  | Clight.Tfloat _, Clight.Tfloat _ -> error_float ()
  | Clight.Tpointer t, Clight.Tint _
  | Clight.Tarray (t, _), Clight.Tint _ ->
    let t2 = cminor_type_of e2 in
    let size = esizeof_ctype t2 t in
    let index = Cminor.Expr (Cminor.Op2 (AST.Op_mul, e2, size), t2) in
    Cminor.Expr (Cminor.Op2 (AST.Op_subp, e1, index), res_type)
  | Clight.Tpointer _, Clight.Tpointer _
  | Clight.Tarray _, Clight.Tpointer _
  | Clight.Tpointer _, Clight.Tarray _
  | Clight.Tarray _, Clight.Tarray _ ->
    Cminor.Expr (Cminor.Op2 (AST.Op_subpp, e1, e2), res_type)
  | _ -> error "type error."

let is_signed = function
  | Clight.Tint (_, AST.Signed) -> true
  | _ -> false

let is_pointer = function
  | Clight.Tpointer _ | Clight.Tarray _ -> true
  | _ -> false

let cmp_of_clight_binop = function
  | Clight.Oeq -> AST.Cmp_eq
  | Clight.One -> AST.Cmp_ne
  | Clight.Olt -> AST.Cmp_lt
  | Clight.Ole -> AST.Cmp_le
  | Clight.Ogt -> AST.Cmp_gt
  | Clight.Oge -> AST.Cmp_ge
  | _ -> assert false (* do not use on these arguments *)

let translate_simple_binop t = function
  | Clight.Omul -> AST.Op_mul
  | Clight.Odiv when is_signed t -> AST.Op_div
  | Clight.Odiv -> AST.Op_divu
  | Clight.Omod when is_signed t -> AST.Op_mod
  | Clight.Omod -> AST.Op_modu
  | Clight.Oand -> AST.Op_and
  | Clight.Oor -> AST.Op_or
  | Clight.Oxor -> AST.Op_xor
  | Clight.Oshl -> AST.Op_shl
  | Clight.Oshr when is_signed t -> AST.Op_shr
  | Clight.Oshr -> AST.Op_shru
  | binop when is_pointer t -> AST.Op_cmpp (cmp_of_clight_binop binop)
  | binop when is_signed t -> AST.Op_cmp (cmp_of_clight_binop binop)
  | binop -> AST.Op_cmpu (cmp_of_clight_binop binop)

let translate_binop res_type ctype1 ctype2 e1 e2 binop =
  match binop with
    | Clight.Oadd -> translate_add res_type ctype1 ctype2 e1 e2
    | Clight.Osub -> translate_sub res_type ctype1 ctype2 e1 e2
    | _ ->
      let cminor_binop = translate_simple_binop ctype1 binop in
      Cminor.Expr (Cminor.Op2 (cminor_binop, e1, e2), res_type)

let translate_ident var_locs res_type x =
  assert false (* TODO M1 *)

let translate_field res_type t e field =
  assert false (* TODO M1 *)

let translate_cast e src_type dest_type =
  let res_type = sig_type_of_ctype dest_type in
  match src_type, dest_type with
    | Clight.Tint (size1, sign1), Clight.Tint (size2, _) ->
      let t1 = (byte_size_of_intsize size1, sign1) in
      let t2 = byte_size_of_intsize size2 in
      Cminor.Expr (Cminor.Op1 (AST.Op_cast (t1, t2), e), res_type)
    | Clight.Tint _, Clight.Tpointer _
    | Clight.Tint _, Clight.Tarray _ ->
      Cminor.Expr (Cminor.Op1 (AST.Op_ptrofint, e), res_type)
    | Clight.Tpointer _, Clight.Tint _
    | Clight.Tarray _, Clight.Tint _ ->
      Cminor.Expr (Cminor.Op1 (AST.Op_intofptr, e), res_type)
    | _ -> e

let rec f_expr var_locs (Clight.Expr (ed, t)) sub_exprs_res =
  let t_cminor = sig_type_of_ctype t in
  let cst_int i t = Cminor.Expr (Cminor.Cst (AST.Cst_int i), t) in
  match ed, sub_exprs_res with

  | Clight.Econst_int i, _ ->
    assert false (* TODO M1 *)

  | Clight.Econst_float _, _ -> error_float ()

  | Clight.Evar x, _ when is_function_ctype t ->
    Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol x), t_cminor)

  | Clight.Evar x, _ -> translate_ident var_locs t_cminor x

  | Clight.Ederef _, e :: _ when is_scalar_ctype t ->
    Cminor.Expr (Cminor.Mem (quantity_of_ctype t, e), t_cminor)

  | Clight.Ederef _, e :: _ ->
    (* When dereferencing something pointing to a struct for instance, the
       result is the address of the struct. *)
    e

  | Clight.Eaddrof e, _ -> translate_addrof var_locs e

  | Clight.Eunop (unop, _), e :: _ -> 
    Cminor.Expr (Cminor.Op1 (translate_unop unop, e), t_cminor)

  | Clight.Ebinop (binop, e1, e2), e1' :: e2' :: _ -> 
    translate_binop t_cminor (clight_type_of e1) (clight_type_of e2) e1' e2'
      binop

  | Clight.Ecast (t, Clight.Expr (_, t')), e :: _ -> translate_cast e t' t

  | Clight.Econdition _, e1 :: e2 :: e3 :: _ ->
    assert false (* TODO M1 *)

  | Clight.Eandbool _, e1 :: e2 :: _ -> 
    let zero = cst_int 0 t_cminor in
    let one = cst_int 1 t_cminor in
    let e2_cond = Cminor.Expr (Cminor.Cond (e2, one, zero), t_cminor) in
    Cminor.Expr (Cminor.Cond (e1, e2_cond, zero), t_cminor)

  | Clight.Eorbool _, e1 :: e2 :: _ -> 
    let zero = cst_int 0 t_cminor in
    let one = cst_int 1 t_cminor in
    let e2_cond = Cminor.Expr (Cminor.Cond (e2, one, zero), t_cminor) in
    Cminor.Expr (Cminor.Cond (e1, one, e2_cond), t_cminor)

  | Clight.Esizeof t, _ -> esizeof_ctype t_cminor t

  | Clight.Ecost (lbl, _), e :: _ ->
    Cminor.Expr (Cminor.Exp_cost (lbl, e), t_cminor)

  | Clight.Ecall _, _ -> assert false (* only for annotations *)

  | Clight.Efield (Clight.Expr (_, t), field), e :: _ ->
    translate_field t_cminor t e field

  | _ -> assert false (* wrong number of arguments *)

and translate_addrof_ident res_type var_locs id =
  match find_var_locs id var_locs with
    | (Local, _) | (Param, _) -> assert false (* type error *)
    | (LocalStack off, _) | (ParamStack off, _) ->
      Cminor.Expr (add_stack off, res_type)
    | (Global, _) ->
      Cminor.Expr (Cminor.Cst (AST.Cst_addrsymbol id), res_type)

and translate_addrof_field res_type t field e =
  let (fields, offset) = match t with
    | Clight.Tstruct (_, fields) -> (fields, struct_offset t field fields)
    | Clight.Tunion (_, fields) ->
      (fields, Cminor.Expr (Cminor.Cst (AST.Cst_int 0), AST.Sig_offset))
    | _ -> assert false (* type error *) in
  Cminor.Expr (Cminor.Op2 (AST.Op_addp, e, offset), res_type)

and translate_addrof var_locs (Clight.Expr (ed, _)) =
  let res_type = AST.Sig_ptr in
  match ed with

    | Clight.Evar id -> translate_addrof_ident res_type var_locs id

    | Clight.Ederef e -> translate_expr var_locs e

    | Clight.Efield ((Clight.Expr (_, t) as e), field) ->
      let e = translate_expr var_locs e in
      translate_addrof_field res_type t field e

    | _ -> assert false (* not a lvalue *)

and translate_expr var_locs e = ClightFold.expr2 (f_expr var_locs) e


(* Translate statement *)

let assign var_locs (Clight.Expr (ed, t) as e_res_clight) e_arg_cminor =
  match ed with
    | Clight.Evar id when is_local_or_param id var_locs ->
      Cminor.St_assign (id, e_arg_cminor)
    | _ ->
      let quantity = quantity_of_ctype t in
      let addr = translate_addrof var_locs e_res_clight in
      Cminor.St_store (quantity, addr, e_arg_cminor)

let call_sig ret_type args =
  { AST.args = List.map cminor_type_of args ;
    AST.res = ret_type }

let f_stmt fresh var_locs stmt sub_exprs_res sub_stmts_res =
  let (tmps, sub_stmts_res) = List.split sub_stmts_res in
  let tmps = List.flatten tmps in

  let (added_tmps, stmt) = match stmt, sub_exprs_res, sub_stmts_res with

    | Clight.Sskip, _, _ -> ([], Cminor.St_skip)

    | Clight.Sassign (e1, _), _ :: e2 :: _, _ ->
      ([], assign var_locs e1 e2)

    | Clight.Scall (None, _, _), f :: args, _ ->
      ([], Cminor.St_call (None, f, args, call_sig AST.Type_void args))

    | Clight.Scall (Some e, _, _), _ :: f :: args, _ ->
      assert false (* TODO M1 *)

    | Clight.Swhile _, e :: _, stmt :: _ ->
      assert false (* TODO M1 *)

    | Clight.Sdowhile _, e :: _, stmt :: _ ->
      let econd =
	Cminor.Expr (Cminor.Op1 (AST.Op_notbool, e), cminor_type_of e) in
      let scond =
	Cminor.St_ifthenelse (econd, Cminor.St_exit 0, Cminor.St_skip) in
      ([],
       Cminor.St_block (Cminor.St_loop (Cminor.St_seq (Cminor.St_block stmt,
						       scond))))

    | Clight.Sfor _, e :: _, stmt1 :: stmt2 :: stmt3 :: _ ->
      let econd = 
	Cminor.Expr (Cminor.Op1 (AST.Op_notbool, e), cminor_type_of e) in
      let scond =
	Cminor.St_ifthenelse (econd, Cminor.St_exit 0, Cminor.St_skip) in
      let body = Cminor.St_seq (Cminor.St_block stmt3, stmt2) in
      ([],
       Cminor.St_seq (stmt1,
		      Cminor.St_block
			(Cminor.St_loop (Cminor.St_seq (scond, body)))))

    | Clight.Sifthenelse _, e :: _, stmt1 :: stmt2 :: _ ->
      assert false (* TODO M1 *)

    | Clight.Ssequence _, _, stmt1 :: stmt2 :: _ ->
      assert false (* TODO M1 *)

    | Clight.Sbreak, _, _ -> ([], Cminor.St_exit 1)

    | Clight.Scontinue, _, _ -> ([], Cminor.St_exit 0)

    | Clight.Sswitch _, _, _ ->
      (* Should not appear because of switch transformation performed
	 beforehand. *)
      assert false

    | Clight.Sreturn None, _, _ -> ([], Cminor.St_return None)

    | Clight.Sreturn (Some _), e :: _, _ -> ([], Cminor.St_return (Some e))

    | Clight.Slabel (lbl, _), _, stmt :: _ -> ([], Cminor.St_label (lbl, stmt))

    | Clight.Sgoto lbl, _, _ -> ([], Cminor.St_goto lbl)

    | Clight.Scost (lbl, _), _, stmt :: _ -> ([], Cminor.St_cost (lbl, stmt))

    | _ -> assert false (* type error *) in

  (tmps @ added_tmps, stmt)

let translate_statement fresh var_locs =
  ClightFold.statement2 (f_expr var_locs) (f_stmt fresh var_locs)


(* Translate functions and programs *)

let add_stack_parameter_initialization x t off body =
  let addr = Cminor.Expr (add_stack off, AST.Sig_ptr) in
  let e = Cminor.Expr (Cminor.Id x, sig_type_of_ctype t) in
  let stmt = Cminor.St_store (quantity_of_ctype t, addr, e) in
  Cminor.St_seq (stmt, body)

let add_stack_parameters_initialization var_locs body =
  let f x (location, t) body = match location with
    | ParamStack offset -> add_stack_parameter_initialization x t offset body
    | _ -> body in
  fold_var_locs f var_locs body

let translate_internal fresh globals cfun =
  let var_locs = sort_variables globals cfun in
  let params =
    List.map (fun (x, t) -> (x, sig_type_of_ctype t)) cfun.Clight.fn_params in
  let (tmps, body) = translate_statement fresh var_locs cfun.Clight.fn_body in
  let body = add_stack_parameters_initialization var_locs body in
  { Cminor.f_return = type_return_of_ctype cfun.Clight.fn_return ;
    Cminor.f_params = params ;
    Cminor.f_vars = (get_locals var_locs) @ tmps ;
    Cminor.f_stacksize = get_stacksize var_locs ;
    Cminor.f_body = body }

let translate_external id params return = 
  { AST.ef_tag = id ;
    AST.ef_sig = { AST.args = translate_args_types params ;
		   AST.res = type_return_of_ctype return } }

let translate_funct fresh globals (id, def) =
  let def = match def with
    | Clight.Internal ff -> Cminor.F_int (translate_internal fresh globals ff)
    | Clight.External (i,p,r) -> Cminor.F_ext (translate_external i p r) in
  (id, def)

let translate p =
  let fresh = ClightAnnotator.make_fresh "_tmp" p in
  { Cminor.vars = List.map translate_global p.Clight.prog_vars ;
    Cminor.functs =
      List.map (translate_funct fresh p.Clight.prog_vars) p.Clight.prog_funct ;
    Cminor.main = p.Clight.prog_main }

open AST
open Cminor

module Mem = Driver.CminorMemory
module Val = Mem.Value
module LocalEnv = Map.Make(String)
type local_env = Val.t LocalEnv.t
type memory = Cminor.function_def Mem.memory


let error_prefix = "Cminor interpret"
let error s = Error.global_error error_prefix s
let warning s = Error.warning error_prefix s
let error_float () = error "float not supported."


(* Helpers *)

let value_of_address = List.hd
let address_of_value v = [v]


(* State of execution *)

type continuation = 
    Ct_stop
  | Ct_cont of statement*continuation
  | Ct_endblock of continuation
  | Ct_returnto of
      ident option*internal_function*Val.address*local_env*continuation

type state = 
    State_regular of
      internal_function*statement*continuation*Val.address*local_env*(function_def Mem.memory)
  | State_call of function_def*Val.t list*continuation*(function_def Mem.memory)
  | State_return of Val.t*continuation*(function_def Mem.memory)

let string_of_local_env lenv =
  let f x v s = s ^ x ^ " = " ^ (Val.to_string v) ^ "  " in
  LocalEnv.fold f lenv ""

let string_of_expr = CminorPrinter.print_expression

let string_of_args args =
  "(" ^ (MiscPottier.string_of_list ", " string_of_expr args) ^ ")"

let rec string_of_statement = function
  | St_skip -> "skip"
  | St_assign (x, e) -> x ^ " = " ^ (string_of_expr e)
  | St_store (q, e1, e2) ->
    Printf.sprintf "%s[%s] = %s"
      (Memory.string_of_quantity q) (string_of_expr e1) (string_of_expr e2)
  | St_call (None, f, args, _)
  | St_tailcall (f, args, _) -> (string_of_expr f) ^ (string_of_args args)
  | St_call (Some x, f, args, _) ->
    x ^ " = " ^ (string_of_expr f) ^ (string_of_args args)
  | St_seq _ -> "sequence"
  | St_ifthenelse (e, _, _) -> "if (" ^ (string_of_expr e) ^ ")"
  | St_loop _ -> "loop"
  | St_block _ -> "block"
  | St_exit n -> "exit " ^ (string_of_int n)
  | St_switch (e, _, _) -> "switch (" ^ (string_of_expr e) ^ ")"
  | St_return None -> "return"
  | St_return (Some e) -> "return (" ^ (string_of_expr e) ^ ")"
  | St_label (lbl, _) -> "label " ^ lbl
  | St_goto lbl -> "goto " ^ lbl
  | St_cost (lbl, _) -> "cost " ^ lbl

let print_state = function
  | State_regular (_, stmt, _, sp, lenv, mem) ->
    Printf.printf "Local environment:\n%s\n\nMemory:%s\nStack pointer: %s\n\nRegular state: %s\n\n%!"
      (string_of_local_env lenv)
      (Mem.to_string mem)
      (Val.to_string (value_of_address sp))
      (string_of_statement stmt)
  | State_call (_, args, _, mem) ->
    Printf.printf "Memory:%s\nCall state\n\nArguments:\n%s\n\n%!"
      (Mem.to_string mem)
      (MiscPottier.string_of_list " " Val.to_string args)
  | State_return (v, _, mem) ->
    Printf.printf "Memory:%s\nReturn state: %s\n\n%!"
      (Mem.to_string mem)
      (Val.to_string v)


(* Global and local environment management *)

let init_local_env args params vars =
  let f_param lenv (x, _) v = LocalEnv.add x v lenv in
  let f_var lenv (x, _) = LocalEnv.add x Val.undef lenv in
  let lenv = List.fold_left2 f_param LocalEnv.empty params args in
  List.fold_left f_var lenv vars

let find_fundef f mem =
  let addr = Mem.find_global mem f in
  Mem.find_fun_def mem addr


(* Expression evaluation *)

module Eval_op (M : Memory.S) = struct

  let concrete_stacksize = M.concrete_size

  let ext_fun_of_sign = function
    | AST.Signed -> M.Value.sign_ext
    | AST.Unsigned -> M.Value.zero_ext

  let cast_to_std t v = match t with
    | AST.Sig_int (size, sign) -> (ext_fun_of_sign sign) v size M.int_size
    | AST.Sig_float _ -> error_float ()
    | AST.Sig_offset | AST.Sig_ptr -> v

  let cast_from_std t v = match t with
    | AST.Sig_int (size, _) -> (ext_fun_of_sign AST.Unsigned) v M.int_size size
    | AST.Sig_float _ -> error_float ()
    | AST.Sig_offset | AST.Sig_ptr -> v

  let cst mem sp t = function
    | Cst_int i -> cast_to_std t (M.Value.of_int i)
    | Cst_float _ -> error_float ()
    | Cst_addrsymbol id when M.mem_global mem id ->
      value_of_address (M.find_global mem id)
    | Cst_addrsymbol id -> error ("unknown global variable " ^ id ^ ".")
    | Cst_stack -> value_of_address sp
    | Cst_offset off -> M.Value.of_int (M.concrete_offset off)
    | Cst_sizeof t' -> cast_to_std t (M.Value.of_int (M.concrete_size t'))

  let fun_of_op1 = function
    | Op_cast ((from_size, from_sign), to_size) ->
      (fun v -> (ext_fun_of_sign from_sign) v from_size to_size)
    | Op_negint -> M.Value.negint
    | Op_notbool -> M.Value.notbool
    | Op_notint -> M.Value.negint
    | Op_id -> (fun v -> v)
    | Op_ptrofint
    | Op_intofptr ->
      error "conversion between integers and pointers not supported yet."

  let op1 ret_type t op v =
    cast_from_std ret_type ((fun_of_op1 op) (cast_to_std t v))

  let fun_of_op2 = function
    | Op_add | Op_addp -> M.Value.add
    | Op_sub | Op_subp | Op_subpp -> M.Value.sub
    | Op_mul -> M.Value.mul
    | Op_div -> M.Value.div
    | Op_divu -> M.Value.divu
    | Op_mod -> M.Value.modulo
    | Op_modu -> M.Value.modulou
    | Op_and -> M.Value.and_op
    | Op_or -> M.Value.or_op
    | Op_xor -> M.Value.xor
    | Op_shl -> M.Value.shl
    | Op_shr -> M.Value.shr
    | Op_shru -> M.Value.shru
    | Op_cmp Cmp_eq | Op_cmpp Cmp_eq -> M.Value.cmp_eq
    | Op_cmp Cmp_ne | Op_cmpp Cmp_ne -> M.Value.cmp_ne
    | Op_cmp Cmp_gt | Op_cmpp Cmp_gt -> M.Value.cmp_gt
    | Op_cmp Cmp_ge | Op_cmpp Cmp_ge -> M.Value.cmp_ge
    | Op_cmp Cmp_lt | Op_cmpp Cmp_lt -> M.Value.cmp_lt
    | Op_cmp Cmp_le | Op_cmpp Cmp_le -> M.Value.cmp_le
    | Op_cmpu Cmp_eq -> M.Value.cmp_eq_u
    | Op_cmpu Cmp_ne -> M.Value.cmp_ne_u
    | Op_cmpu Cmp_gt -> M.Value.cmp_gt_u
    | Op_cmpu Cmp_ge -> M.Value.cmp_ge_u
    | Op_cmpu Cmp_lt -> M.Value.cmp_lt_u
    | Op_cmpu Cmp_le -> M.Value.cmp_le_u

  let op2 ret_type t1 t2 op2 v1 v2 =
    let v1 = cast_to_std t1 v1 in
    let v2 = cast_to_std t2 v2 in
    cast_from_std ret_type ((fun_of_op2 op2) v1 v2)
end

module Eval = Eval_op (Mem)

let concrete_stacksize = Eval.concrete_stacksize
let eval_constant = Eval.cst
let eval_unop = Eval.op1
let eval_binop = Eval.op2

let type_of_expr (Cminor.Expr (_, t)) = t

let rec eval_expression stack local_env memory (Cminor.Expr (ed, t)) =
  match ed with
    | Id x when LocalEnv.mem x local_env -> (LocalEnv.find x local_env,[])
    | Id x -> error ("unknown local variable " ^ x ^ ".")
    | Cst(c) -> (eval_constant memory stack t c,[])
    | Op1(op,arg) -> 
      let (v,l) = eval_expression stack local_env memory arg in
      (eval_unop t (type_of_expr arg) op v,l)
    | Op2(op, arg1, arg2) -> 
      let (v1,l1) = eval_expression stack local_env memory arg1 in
      let (v2,l2) = eval_expression stack local_env memory arg2 in
      (eval_binop t (type_of_expr arg1) (type_of_expr arg2) op v1 v2,l1@l2) 
    | Mem(q,a) -> 
      let (v,l) = eval_expression stack local_env memory a in 
      (Mem.loadq memory q (address_of_value v),l)
    | Cond(a1,a2,a3) ->
      let (v1,l1) = eval_expression stack local_env memory a1 in
      if Val.is_true v1 then
	let (v2,l2) = eval_expression stack local_env memory a2 in
	(v2,l1@l2)
      else
	if Val.is_false v1 then
	  let (v3,l3) = eval_expression stack local_env memory a3 in
	  (v3,l1@l3)
	else error "undefined conditional value."
    | Exp_cost(lbl,e) -> 
      let (v,l) = eval_expression stack local_env memory e in
      (v,l@[lbl])

let eval_exprlist sp lenv mem es =
  let f (vs, cost_lbls) e =
    let (v, cost_lbls') = eval_expression sp lenv mem e in
    (vs @ [v], cost_lbls @ cost_lbls') in
  List.fold_left f ([], []) es

(* State transition *)

let rec callcont = function
    Ct_stop                     -> Ct_stop
  | Ct_cont(_,k)                -> callcont k
  | Ct_endblock(k)              -> callcont k
  | Ct_returnto(a,b,c,d,e)      -> Ct_returnto(a,b,c,d,e)

let findlabel lbl st k = 
  let rec fdlbl k = function
    St_skip                     -> None
  | St_assign(_,_)              -> None
  | St_store(_,_,_)             -> None
  | St_call(_,_,_,_)            -> None
  | St_tailcall(_,_,_)          -> None 
  | St_seq(s1,s2)               -> 
      (match fdlbl (Ct_cont(s2,k)) s1 with
	   None -> fdlbl k s2
	 | Some(v) -> Some(v)
      )
  | St_ifthenelse(_,s1,s2)      ->
      (match fdlbl k s1 with
	   None -> fdlbl k s2
	 | Some(v) -> Some(v)
      )
  | St_loop(s)                     -> fdlbl (Ct_cont(St_loop(s),k)) s
  | St_block(s)                    -> fdlbl (Ct_endblock(k)) s
  | St_exit(_)                     -> None
  | St_switch(_,_,_)               -> None
  | St_return(_)                   -> None
  | St_label(l,s) when l = lbl     -> Some((s,k))
  | St_goto(_)                     -> None
  | St_cost (_,s) | St_label (_,s) -> fdlbl k s
  in match fdlbl k st with
      None -> assert false (*Wrong label*)
    | Some(v) -> v 


let call_state sigma e m f params cont =
  let (addr,l1) = eval_expression sigma e m f in
  let fun_def = Mem.find_fun_def m (address_of_value addr) in
  let (args,l2) = eval_exprlist sigma e m params in
  (State_call(fun_def,args,cont,m),l1@l2)

let eval_stmt f k sigma e m s = match s, k with
  | St_skip,Ct_cont(s,k) -> (State_regular(f, s, k, sigma, e, m),[])
  | St_skip,Ct_endblock(k) -> (State_regular(f, St_skip, k, sigma, e, m),[])
  | St_skip, (Ct_returnto _ as k) ->
    (State_return (Val.undef,k,Mem.free m sigma),[])
  | St_skip,Ct_stop -> 
    (State_return (Val.undef,Ct_stop,Mem.free m sigma),[])
  | St_assign(x,exp),_ -> 
    let (v,l) = eval_expression sigma e m exp in
    let e = LocalEnv.add x v e in
    (State_regular(f, St_skip, k, sigma, e, m),l)
  | St_store(q,a1,a2),_ ->
    let (v1,l1) = eval_expression sigma e m a1 in
    let (v2,l2) = eval_expression sigma e m a2 in
    let m = Mem.storeq m q (address_of_value v1) v2 in
    (State_regular(f, St_skip, k, sigma, e, m),l1@l2)
  | St_call(xopt,f',params,_),_ ->
    call_state sigma e m f' params (Ct_returnto(xopt,f,sigma,e,k))
  | St_tailcall(f',params,_),_ -> 
    call_state sigma e m f' params (callcont k)
  | St_seq(s1,s2),_ -> (State_regular(f, s1, Ct_cont(s2, k), sigma, e, m),[])
  | St_ifthenelse(exp,s1,s2),_ ->
    let (v,l) = eval_expression sigma e m exp in
    let next_stmt =
      if Val.is_true v then s1
      else
	if Val.is_false v then s2
	else error "undefined conditional value." in
      (State_regular(f,next_stmt,k,sigma,e,m),l)
  | St_loop(s),_ -> (State_regular(f,s,Ct_cont((St_loop s),k),sigma,e,m),[])
  | St_block(s),_ -> (State_regular(f,s,(Ct_endblock k),sigma,e,m),[])
  | St_exit(n),Ct_cont(s,k) -> (State_regular(f,(St_exit n),k,sigma,e,m),[])
  | St_exit(0),Ct_endblock(k) -> (State_regular(f,St_skip,k,sigma,e,m),[])
  | St_exit(n),Ct_endblock(k) ->
    (State_regular(f,(St_exit (n-1)),k,sigma,e,m),[])
  | St_label(_,s),_ -> (State_regular(f,s,k,sigma,e,m),[])
  | St_goto(lbl),_ -> 
    let (s2,k2) = findlabel lbl f.f_body (callcont k) in
    (State_regular(f,s2,k2,sigma,e,m),[])
  | St_switch(exp,lst,def),_ ->
    let (v,l) = eval_expression sigma e m exp in
    if Val.is_int v then
      try
	let i = Val.to_int v in
	let nb_exit =
	  if List.mem_assoc i lst then List.assoc i lst
	  else def in
	(State_regular(f, St_exit nb_exit,k, sigma, e, m),l)
      with _ -> error "int value too big."
    else error "undefined switch value."
  | St_return(None),_ ->
    (State_return (Val.undef,callcont k,Mem.free m sigma),[])
  | St_return(Some(a)),_ ->
      let (v,l) = eval_expression sigma e m a in
      (State_return (v,callcont k,Mem.free m sigma),l)
  | St_cost(lbl,s),_ -> (State_regular(f,s,k,sigma,e,m),[lbl])
  | _ -> error "state malformation."


module InterpretExternal = Primitive.Interpret (Mem)

let interpret_external k mem f args =
  let (mem', v) = match InterpretExternal.t mem f args with
    | (mem', InterpretExternal.V vs) ->
      let v = if List.length vs = 0 then Val.undef else List.hd vs in
      (mem', v)
    | (mem', InterpretExternal.A addr) -> (mem', value_of_address addr) in
  State_return (v, k, mem')

let step_call vargs k m = function
  | F_int f ->
    let (m, sp) = Mem.alloc m (concrete_stacksize f.f_stacksize) in
    let lenv = init_local_env vargs f.f_params f.f_vars in
    State_regular(f,f.f_body,k,sp,lenv,m)
  | F_ext f -> interpret_external k m f.ef_tag vargs

let step = function
  | State_regular(f,stmt,k,sp,e,m) -> eval_stmt f k sp e m stmt
  | State_call(fun_def,vargs,k,m) -> (step_call vargs k m fun_def,[])
  | State_return(v,Ct_returnto(None,f,sigma,e,k),m) ->
    (State_regular(f,St_skip,k,sigma,e,m),[])
  | State_return(v,Ct_returnto(Some x,f,sigma,e,k),m) ->
    let e = LocalEnv.add x v e in
    (State_regular(f,St_skip,k,sigma,e,m),[])
  | _ -> error "state malformation."


let init_mem prog =
  let f_var mem (x, size, init_datas) = Mem.add_var mem x size init_datas in
  let mem = List.fold_left f_var Mem.empty prog.vars in
  let f_fun_def mem (f, def) = Mem.add_fun_def mem f def in
  List.fold_left f_fun_def mem prog.functs

let compute_result v =
  if Val.is_int v then IntValue.Int32.cast (Val.to_int_repr v)
  else IntValue.Int32.zero

let rec exec debug trace (state, l) =
  let cost_labels = l @ trace in
  let print_and_return_result res =
    if debug then Printf.printf "Result = %s\n%!"
      (IntValue.Int32.to_string res) ;
    (res, cost_labels) in
  if debug then print_state state ;
  match state with
    | State_return(v,Ct_stop,_) -> (* Explicit return in main *)
      print_and_return_result (compute_result v)
    | State_regular(_,St_skip,Ct_stop,_,_,_) -> (* Implicit return in main *)
      print_and_return_result IntValue.Int32.zero
    | state -> exec debug cost_labels (step state)

let interpret debug prog =
  Printf.printf "*** Cminor interpret ***\n%!" ;
  match prog.main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let mem = init_mem prog in
      let first_state =	(State_call (find_fundef main mem,[],Ct_stop,mem),[]) in
      exec debug [] first_state

module Mem = Driver.ClightMemory
module Value = Driver.ClightMemory.Value
module LocalEnv = Map.Make(String)
type localEnv = Value.address LocalEnv.t
type memory = Clight.fundef Mem.memory

open Clight
open AST


let error_prefix = "Clight interpret"
let error s = Error.global_error error_prefix s
let warning s = Error.warning error_prefix s
let error_float () = error "float not supported."


(* Helpers *)

let value_of_address = List.hd
let address_of_value v = [v]


(* State of execution *)

type continuation =
  | Kstop
  | Kseq of statement*continuation
  | Kwhile of expr*statement*continuation
  | Kdowhile of expr*statement*continuation
  | Kfor2 of expr*statement*statement*continuation
  | Kfor3 of expr*statement*statement*continuation 
  | Kswitch of continuation
  | Kcall of (Value.address*ctype) option*cfunction*localEnv*continuation

type state =
  | State of cfunction*statement*continuation*localEnv*memory
  | Callstate of fundef*Value.t list*continuation*memory
  | Returnstate of Value.t*continuation*memory

let string_of_unop = function
  | Onotbool -> "!"
  | Onotint -> "~"
  | Oneg -> "-"

let string_of_binop = function
  | Oadd -> "+"
  | Osub -> "-"
  | Omul -> "*"
  | Odiv -> "/"
  | Omod -> "%"
  | Oand -> "&"
  | Oor	 -> "|"
  | Oxor -> "^"
  | Oshl -> "<<"
  | Oshr -> ">>"
  | Oeq -> "=="
  | One -> "!="
  | Olt -> "<"
  | Ogt -> ">"
  | Ole -> "<="
  | Oge -> ">="

let string_of_signedness = function
  | Signed -> "signed"
  | Unsigned -> "unsigned"

let string_of_sized_int = function
  | I8 -> "char"
  | I16 -> "short"
  | I32 -> "int"

let rec string_of_ctype = function
  | Tvoid -> "void"
  | Tint (size, sign) ->
    (string_of_signedness sign) ^ " " ^ (string_of_sized_int size)
  | Tfloat _ -> error_float ()
  | Tpointer ty -> (string_of_ctype ty) ^ "*"
  | Tarray (ty, _) -> (string_of_ctype ty) ^ "[]"
  | Tfunction _ -> assert false (* do not cast to a function type *)
  | Tstruct (id, _)
  | Tunion (id, _) -> id
  | Tcomp_ptr id -> id ^ "*"

let rec string_of_expr (Expr (e, _)) = string_of_expr_descr e
and string_of_expr_descr = function
  | Econst_int i -> string_of_int i
  | Econst_float _ -> error_float ()
  | Evar x -> x
  | Ederef e -> "*(" ^ (string_of_expr e) ^ ")"
  | Eaddrof e -> "&(" ^ (string_of_expr e) ^ ")"
  | Eunop (unop, e) -> (string_of_unop unop) ^ "(" ^ (string_of_expr e) ^ ")"
  | Ebinop (binop, e1, e2) ->
    "(" ^ (string_of_expr e1) ^ ")" ^ (string_of_binop binop) ^
    "(" ^ (string_of_expr e2) ^ ")"
  | Ecast (ty, e) ->
    "(" ^ (string_of_ctype ty) ^ ") (" ^ (string_of_expr e) ^ ")"
  | Econdition (e1, e2, e3) ->
    "(" ^ (string_of_expr e1) ^ ") ? (" ^ (string_of_expr e2) ^
    ") : (" ^ (string_of_expr e3) ^ ")"
  | Eandbool (e1, e2) ->
    "(" ^ (string_of_expr e1) ^ ") && (" ^ (string_of_expr e2) ^ ")"
  | Eorbool (e1, e2) ->
    "(" ^ (string_of_expr e1) ^ ") || (" ^ (string_of_expr e2) ^ ")"
  | Esizeof ty -> "sizeof(" ^ (string_of_ctype ty) ^ ")"
  | Efield (e, field) -> "(" ^ (string_of_expr e) ^ ")." ^ field
  | Ecost (cost_lbl, e) -> "/* " ^ cost_lbl ^ " */ " ^ (string_of_expr e)
  | Ecall (f, arg, e) ->
    "(" ^ f ^ "(" ^ (string_of_expr arg) ^ "), " ^ (string_of_expr e) ^ ")"

let string_of_args args =
  "(" ^ (MiscPottier.string_of_list ", " string_of_expr args) ^ ")"

let rec string_of_statement = function
  | Sskip -> "skip"
  | Sassign (e1, e2) -> (string_of_expr e1) ^ " = " ^ (string_of_expr e2)
  | Scall (None, f, args) -> (string_of_expr f) ^ (string_of_args args)
  | Scall (Some e, f, args) ->
    (string_of_expr e) ^ " = " ^ (string_of_expr f) ^ (string_of_args args)
  | Ssequence _ -> "sequence"
  | Sifthenelse (e, _, _) -> "if (" ^ (string_of_expr e) ^ ")"
  | Swhile (e, _) -> "while (" ^ (string_of_expr e) ^ ")"
  | Sdowhile _ -> "dowhile"
  | Sfor (s, _, _, _) -> "for (" ^ (string_of_statement s) ^ "; ...)"
  | Sbreak -> "break"
  | Scontinue -> "continue"
  | Sreturn None -> "return"
  | Sreturn (Some e) -> "return (" ^ (string_of_expr e) ^ ")"
  | Sswitch (e, _) -> "switch (" ^ (string_of_expr e) ^ ")"
  | Slabel (lbl, _) -> "label " ^ lbl
  | Sgoto lbl -> "goto " ^ lbl
  | Scost (lbl, _) -> "cost " ^ lbl

let string_of_local_env lenv =
  let f x addr s =
    s ^ x ^ " = " ^ (Value.to_string (value_of_address addr)) ^ "  " in
  LocalEnv.fold f lenv ""

let print_state = function
  | State (_, stmt, _, lenv, mem) ->
    Printf.printf "Local environment:\n%s\n\nMemory:%s\nRegular state: %s\n\n%!"
      (string_of_local_env lenv)
      (Mem.to_string mem)
      (string_of_statement stmt)
  | Callstate (_, args, _, mem) ->
    Printf.printf "Memory:%s\nCall state\n\nArguments:\n%s\n\n%!"
      (Mem.to_string mem)
      (MiscPottier.string_of_list " " Value.to_string args)
  | Returnstate (v, _, mem) ->
    Printf.printf "Memory:%s\nReturn state: %s\n\n%!"
      (Mem.to_string mem)
      (Value.to_string v)


(* Continuations and labels *)

let rec call_cont = function
  | Kseq (_,k) | Kwhile (_,_,k) | Kdowhile (_,_,k)
  | Kfor2 (_,_,_,k) | Kfor3 (_,_,_,k) | Kswitch k -> call_cont k
  | k -> k

let rec seq_of_labeled_statement = function
  | LSdefault s -> s
  | LScase (c,s,sl') -> Ssequence (s,(seq_of_labeled_statement sl'))

let rec find_label1 lbl s k = match s with
   | Ssequence (s1,s2) ->
      (match find_label1 lbl s1 (Kseq (s2,k)) with
      | Some sk -> Some sk
      | None -> find_label1 lbl s2 k
      )
  | Sifthenelse (a,s1,s2) ->
      (match find_label1 lbl s1 k with
      | Some sk -> Some sk
      | None -> find_label1 lbl s2 k
      )
  | Swhile (a,s1) -> find_label1 lbl s1 (Kwhile(a,s1,k))
  | Sdowhile (a,s1) -> find_label1 lbl s1 (Kdowhile(a,s1,k))
  | Sfor (a1,a2,a3,s1) ->
      (match find_label1 lbl a1 (Kseq ((Sfor(Sskip,a2,a3,s1)),k)) with
      | Some sk -> Some sk
      | None ->
          (match find_label1 lbl s1 (Kfor2(a2,a3,s1,k)) with
          | Some sk -> Some sk
          | None -> find_label1 lbl a3 (Kfor3(a2,a3,s1,k))
          ))
  | Sswitch (e,sl) -> find_label_ls lbl sl (Kswitch k)
  | Slabel (lbl',s') -> if lbl=lbl' then Some(s', k) else find_label1 lbl s' k
  | Scost (_,s') -> find_label1 lbl s' k
  | Sskip | Sassign (_,_) | Scall (_,_,_) | Sbreak 
  | Scontinue | Sreturn _ | Sgoto _ -> None
           
and find_label_ls lbl sl k =  match sl with
  | LSdefault s -> find_label1 lbl s k
  | LScase (_,s,sl') ->
      (match find_label1 lbl s (Kseq((seq_of_labeled_statement sl'),k)) with
      | Some sk -> Some sk
      | None -> find_label_ls lbl sl' k
      )

let find_label lbl s k = match find_label1 lbl s k with
  | Some res -> res
  | _ -> assert false (* should be impossible *)

let rec select_switch i = function
  | LSdefault d -> LSdefault d
  | LScase (c,s,sl') when c=i-> LScase (c,s,sl') 
  | LScase (_,_,sl') -> select_switch i sl'


(* ctype functions *)

let sizeof ctype = Mem.concrete_size (ClightToCminor.sizeof_ctype ctype)

let size_of_ctype = function
  | Tint (I8, _)  -> 1
  | Tint (I16, _) -> 2
  | Tint (I32, _) -> 4
  | Tfloat _ -> error_float ()
  | Tcomp_ptr _
  | Tpointer _
  | Tarray _
  | Tstruct _
  | Tunion _ -> Mem.ptr_size
  | _ -> assert false (* do not use on these arguments *)

let is_function_type = function
  | Tfunction _ -> true
  | _ -> false

let is_array_type = function
  | Tarray _ -> true
  | _ -> false

let is_complex_type = function
  | Tstruct _ | Tunion _ -> true
  | _ -> false

let is_big_type t = (is_array_type t) || (is_complex_type t)

let dest_type = function
  | Tpointer ty | Tarray (ty, _) -> ty
  | _ -> assert false (* do not use on these arguments *)


(* Global and local environment management *)

let find_local x lenv =
  if LocalEnv.mem x lenv then LocalEnv.find x lenv
  else error ("Unknown local variable " ^ x ^ ".")

let find_global x mem =
  if Mem.mem_global mem x then Mem.find_global mem x
  else error ("Unknown global variable " ^ x ^ ".")

let find_symbol lenv mem x =
  if LocalEnv.mem x lenv then LocalEnv.find x lenv
  else
    if Mem.mem_global mem x then Mem.find_global mem x
    else error ("Unknown variable " ^ x ^ ".")

let find_fundef f mem =
  let addr = Mem.find_global mem f in
  Mem.find_fun_def mem addr


(* Interpret *)

let byte_of_intsize = function
  | I8 -> 1
  | I16 -> 2
  | I32 -> 4

let choose_cast sign n m v =
  let f = match sign with
    | Signed -> Value.sign_ext
    | Unsigned -> Value.zero_ext in
  f v n m

let eval_cast = function
  (* Cast Integer *)
  | (v,Tint(isize,sign),Tint(isize',_)) ->
    choose_cast sign (byte_of_intsize isize) (byte_of_intsize isize') v
  | (v,_,_) -> v

let to_int32 (v, t) = eval_cast (v, t, Tint (I32, Signed))

let eval_unop ret_ctype ((_, t) as e) op =
  let v = to_int32 e in
  let v = match op with
    | Onotbool -> Value.notbool v
    | Onotint -> Value.notint v
    | Oneg -> Value.negint v in
  eval_cast (v, t, ret_ctype)

let eval_add (v1,t1) (v2,t2) = match t1, t2 with
  | Tpointer ty, Tint _ | Tarray (ty, _), Tint _ ->
    let v = Value.mul (Value.of_int (sizeof ty)) v2 in
    Value.add v1 v
  | Tint _, Tpointer ty | Tint _, Tarray (ty, _) ->
    let v = Value.mul (Value.of_int (sizeof ty)) v1 in
    Value.add v2 v
  | _ -> Value.add v1 v2

let eval_sub (v1,t1) (v2,t2) = match t1, t2 with
  | Tpointer ty, Tint _ | Tarray (ty, _), Tint _ ->
    let v = Value.mul (Value.of_int (sizeof ty)) v2 in
    Value.sub v1 v
  | _ -> Value.sub v1 v2

let choose_sign op_signed op_unsigned v1 v2 t =
  let op = match t with
    | Tint (_, Signed) -> op_signed
    | Tint (_, Unsigned) -> op_unsigned
    | _ -> op_unsigned in
  op v1 v2

let eval_binop ret_ctype ((_, t1) as e1) ((_, t2) as e2) op =
  let v1 = to_int32 e1 in
  let v2 = to_int32 e2 in
  let e1 = (v1, t1) in
  let e2 = (v2, t2) in
  let v = match op with
    | Oadd -> eval_add e1 e2
    | Osub -> eval_sub e1 e2
    | Omul -> Value.mul v1 v2
    | Odiv -> choose_sign Value.div Value.divu v1 v2 t1
    | Omod -> choose_sign Value.modulo Value.modulou v1 v2 t1
    | Oand -> Value.and_op v1 v2
    | Oor -> Value.or_op v1 v2
    | Oxor -> Value.xor v1 v2
    | Oshl-> Value.shl v1 v2
    | Oshr-> Value.shr v1 v2
    | Oeq -> choose_sign Value.cmp_eq Value.cmp_eq_u v1 v2 t1
    | One -> choose_sign Value.cmp_ne Value.cmp_ne_u v1 v2 t1
    | Olt -> choose_sign Value.cmp_lt Value.cmp_lt_u v1 v2 t1
    | Ole -> choose_sign Value.cmp_le Value.cmp_le_u v1 v2 t1
    | Ogt -> choose_sign Value.cmp_gt Value.cmp_gt_u v1 v2 t1
    | Oge -> choose_sign Value.cmp_ge Value.cmp_ge_u v1 v2 t1 in
  eval_cast (v, t1, ret_ctype)

let rec get_offset_struct v size id fields =
  let offsets = fst (Mem.concrete_offsets_size size) in
  let fields = List.combine (List.map fst fields) offsets in
  let off = Value.of_int (List.assoc id fields) in
  Value.add v off

let get_offset v id = function
  | Tstruct (_, fields) as t ->
    let size = ClightToCminor.sizeof_ctype t in
    get_offset_struct v size id fields
  | Tunion _ -> v
  | _ -> assert false (* do not use on these arguments *)

let is_true (v, _) = Value.is_true v
let is_false (v, _) = Value.is_false v

let rec eval_expr localenv m (Expr (ee, tt)) =
  match ee with
    | Econst_int i ->
      let v = eval_cast (Value.of_int i, Tint(I32, Signed), tt) in
      ((v, tt),[]) 
    | Econst_float _ -> error_float ()
    | Evar id when is_function_type tt || is_big_type tt ->
      let v = value_of_address (find_symbol localenv m id) in
      ((v, tt), [])
    | Evar id ->
      let addr = find_symbol localenv m id in
      let v = Mem.load m (size_of_ctype tt) addr in
      ((v, tt), [])
    | Ederef e when is_function_type tt || is_big_type tt ->
      let ((v1,_),l1) = eval_expr localenv m e in
      ((v1,tt),l1) 
    | Ederef e ->
      let ((v1,_),l1) = eval_expr localenv m e in
      let addr = address_of_value v1 in
      let v = Mem.load m (size_of_ctype tt) addr in
      ((v,tt),l1) 
    | Eaddrof exp ->
      let ((addr,_),l) = eval_lvalue localenv m exp in
      ((value_of_address addr,tt),l)
    | Ebinop (op,exp1,exp2) ->  
      let (v1,l1) = eval_expr localenv m exp1 in
      let (v2,l2) = eval_expr localenv m exp2 in
      ((eval_binop tt v1 v2 op,tt),l1@l2)
    | Eunop (op,exp) -> 
      let (e1,l1) = eval_expr localenv m exp in
      ((eval_unop tt e1 op,tt),l1)
    | Econdition (e1,e2,e3) ->
      let (v1,l1) = eval_expr localenv m e1 in
      if is_true v1 then let (v2,l2) = eval_expr localenv m e2 in (v2,l1@l2)
      else
	if is_false v1 then let (v3,l3) = eval_expr localenv m e3 in (v3,l1@l3)
      else (v1,l1)
    | Eandbool (e1,e2) -> 
      let (v1,l1) = eval_expr localenv m e1 in
      if is_true v1 then let (v2,l2) = eval_expr localenv m e2 in (v2,l1@l2)
      else (v1,l1)
    | Eorbool (e1,e2) ->
      let (v1,l1) = eval_expr localenv m e1 in
      if is_false v1 then let (v2,l2) = eval_expr localenv m e2 in (v2,l1@l2)
      else (v1,l1)
    | Esizeof cty -> ((Value.of_int (sizeof cty),tt),[])
    | Efield (e1,id) -> 
      let ((v1,t1),l1) = eval_expr localenv m e1 in
      let addr = address_of_value (get_offset v1 id t1) in
      ((Mem.load m (size_of_ctype tt) addr, tt), l1)
    | Ecost (lbl,e1) ->
      let (v1,l1) = eval_expr localenv m e1 in
      (v1,lbl::l1)
    | Ecall _ -> assert false (* only used by the annotation process *)
    | Ecast (cty,exp) -> 
      let ((v,ty),l1) = eval_expr localenv m exp in
      ((eval_cast (v,ty,cty),tt),l1)

and eval_lvalue localenv m (Expr (e,t)) = match e with
  | Econst_int _ | Econst_float _ | Eaddrof _ | Eunop (_,_) | Ebinop (_,_,_) 
  | Ecast (_,_) | Econdition (_,_,_) | Eandbool (_,_)  | Eorbool (_,_) 
  | Esizeof _ -> assert false (*Not allowed in left side of assignement*)
  | Evar id -> ((find_symbol localenv m id,t),[])
  | Ederef ee ->
    let ((v,_),l1) = eval_expr localenv m ee in 
    ((address_of_value v,t),l1)
  | Efield (ee,id) ->
    let ((v,tt),l1) = eval_expr localenv m ee in 
    let v' = get_offset v id tt in
    ((address_of_value v', t), l1)
  | Ecost (lbl,ee) ->
    let (v,l) = eval_lvalue localenv m ee in
    (v,lbl::l)
  | Ecall _ -> assert false (* only used in the annotation process *)

let eval_exprlist lenv mem es =
  let f (vs, cost_lbls) e =
    let ((v, _), cost_lbls') = eval_expr lenv mem e in
    (vs @ [v], cost_lbls @ cost_lbls') in
  List.fold_left f ([], []) es


let bind (mem, lenv) (x, ty) v =
  let (mem, addr) = Mem.alloc mem (sizeof ty) in
  let mem = Mem.store mem (sizeof ty) addr v in
  let lenv = LocalEnv.add x addr lenv in
  (mem, lenv)

let bind_var (mem, lenv) (x, ty) = bind (mem, lenv) (x, ty) Value.undef 

let init_fun_env mem params args vars =
  let lenv = LocalEnv.empty in
  let (mem, lenv) =
    try List.fold_left2 bind (mem, lenv) params args
    with Invalid_argument _ -> error "wrong size of arguments." in
  List.fold_left bind_var (mem, lenv) vars

let assign m v ty ptr = Mem.store m (size_of_ctype ty) ptr v 


let free_local_env mem lenv =
  let f _ addr mem = Mem.free mem addr in
  LocalEnv.fold f lenv mem

let condition v a_true a_false =
  if Value.is_false v then a_false
  else if Value.is_true v then a_true
  else error "undefined condition guard value."

let eval_stmt f k e m s = match s, k with
  | Sskip, Kseq(s,k) -> (State(f,s,k,e,m),[])
  | Sskip, Kwhile(a,s,k) -> (State(f,Swhile(a,s),k,e,m),[])
  | Sskip, Kdowhile(a,s,k) ->
    let ((v1, _),l1) = eval_expr e m a in 
    let a_false = (State(f,Sskip,k,e,m),l1) in
    let a_true = (State(f,Sdowhile(a,s),k,e,m),l1) in
    condition v1 a_true a_false
  | Sskip, Kfor3(a2,a3,s,k) -> (State(f,Sfor(Sskip,a2,a3,s),k,e,m),[])
  | Sskip, Kfor2(a2,a3,s,k) -> (State(f,a3,Kfor3(a2,a3,s,k),e,m),[])
  | Sskip, Kswitch k -> (State(f,Sskip,k,e,m),[])
  | Sskip, Kcall _ -> 
    let m' = free_local_env m e in
    (Returnstate(Value.undef,k,m'),[])
  | Sassign(a1, a2), _ -> 
    let ((v1,t1),l1) = (eval_lvalue e m a1) in
    let ((v2,t2),l2) = eval_expr e m a2 in
    (State(f,Sskip,k,e,assign m v2 t1 v1),l1@l2)
  | Scall(None,a,al), _ ->
    let ((v1,_),l1) = eval_expr e m a in 
    let fd = Mem.find_fun_def m (address_of_value v1) in
    let (vargs,l2) = eval_exprlist e m al in
    (Callstate(fd,vargs,Kcall(None,f,e,k),m),l1@l2)
  | Scall(Some lhs,a,al), _ ->
    let ((v1,_),l1) = eval_expr e m a in 
    let fd = Mem.find_fun_def m (address_of_value v1) in
    let (vargs,l2) = eval_exprlist e m al in
    let (vt3,l3) = eval_lvalue e m lhs in
    (Callstate(fd,vargs,Kcall(Some vt3,f,e,k),m),l1@l2@l3)
  | Ssequence(s1,s2), _ -> (State(f,s1,Kseq(s2,k),e,m),[])
  | Sifthenelse(a,s1,s2), _ -> 
    let ((v1,_),l1) = eval_expr e m a in 
    let a_true = (State(f,s1,k,e,m),l1) in
    let a_false = (State(f,s2,k,e,m),l1) in
    condition v1 a_true a_false
  | Swhile(a,s), _ ->
    let ((v1,_),l1) = eval_expr e m a in 
    let a_false = (State(f,Sskip,k,e,m),l1) in
    let a_true = (State(f,s,Kwhile(a,s,k),e,m),l1) in
    condition v1 a_true a_false
  | Sdowhile(a,s), _ -> (State(f,s,Kdowhile(a,s,k),e,m),[])
  | Sfor(Sskip,a2,a3,s), _ ->
    let ((v1,_),l1) = eval_expr e m a2 in 
    let a_false = (State(f,Sskip,k,e,m),l1) in
    let a_true = (State(f,s,Kfor2(a2,a3,s,k),e,m),l1) in
    condition v1 a_true a_false
  | Sfor(a1,a2,a3,s), _ -> (State(f,a1,Kseq(Sfor(Sskip,a2,a3,s),k),e,m),[])
  | Sbreak, Kseq(s,k) -> (State(f,Sbreak,k,e,m),[])
  | Sbreak, Kwhile(_,_,k) -> (State(f,Sskip,k,e,m),[])
  | Sbreak, Kdowhile(_,_,k) -> (State(f,Sskip,k,e,m),[])
  | Sbreak, Kfor2(_,_,_,k) -> (State(f,Sskip,k,e,m),[])
  | Sbreak, Kswitch k -> (State(f,Sskip,k,e,m),[])
  | Scontinue, Kseq(_,k) -> (State(f,Scontinue,k,e,m),[])
  | Scontinue, Kwhile(a,s,k) -> (State(f,Swhile(a,s),k,e,m),[])
  | Scontinue, Kdowhile(a,s,k) ->
    let ((v1,_),l1) = eval_expr e m a in 
    let a_false = (State(f,Sskip,k,e,m),l1) in
    let a_true = (State(f,Sdowhile(a,s),k,e,m),l1) in
    condition v1 a_true a_false
  | Scontinue, Kfor2(a2,a3,s,k) -> (State(f,a3,Kfor3(a2,a3,s,k),e,m),[])
  | Scontinue, Kswitch k -> (State(f,Scontinue,k,e,m),[])
  | Sreturn None, _ -> 
    let m' = free_local_env m e in
    (Returnstate(Value.undef,(call_cont k),m'),[])
  | Sreturn (Some a), _ -> 
    let ((v1,_),l1) = eval_expr e m a  in
    let m' = free_local_env m e in
    (Returnstate(v1,call_cont k,m'),l1)
  | Sswitch(a,sl), _ -> 
    let ((v,_),l) = eval_expr e m a in
    let n = Value.to_int v in
    (State(f,(seq_of_labeled_statement (select_switch n sl)),Kswitch k,e,m),l)
  | Slabel(lbl,s), _ -> (State(f,s,k,e,m),[])
  | Scost(lbl,s), _ -> (State(f,s,k,e,m),[lbl])
  | Sgoto lbl, _ ->
    let (s', k') = find_label lbl f.fn_body (call_cont k) in
    (State(f,s',k',e,m),[])
  | _ -> assert false (* should be impossible *)


module InterpretExternal = Primitive.Interpret (Mem)

let interpret_external k mem f args =
  let (mem', v) = match InterpretExternal.t mem f args with
    | (mem', InterpretExternal.V vs) ->
      let v = if List.length vs = 0 then Value.undef else List.hd vs in
      (mem', v)
    | (mem', InterpretExternal.A addr) -> (mem', value_of_address addr) in
  Returnstate (v, k, mem')

let step_call args cont mem = function
  | Internal f -> 
    let (mem', e) = init_fun_env mem f.fn_params args f.fn_vars in
    State (f, f.fn_body, cont, e, mem')
  | External(id,targs,tres) when List.length targs = List.length args -> 
    interpret_external cont mem id args
  | External(id,_,_) -> 
    error ("wrong size of arguments when calling external " ^ id ^ ".")

let step = function
  | State(f,stmt,k,e,m) -> eval_stmt f k e m stmt
  | Callstate(fun_def,vargs,k,m) -> (step_call vargs k m fun_def,[])
  | Returnstate(v,Kcall(None,f,e,k),m) -> (State(f,Sskip,k,e,m),[])
  | Returnstate(v,Kcall((Some(vv, ty)),f,e,k),m) -> 
    let m' = assign m v ty vv in
    (State(f,Sskip,k,e,m'),[])
  | _ -> error "state malformation."


let data_of_init_data = function
  | Init_int8 i         -> Data_int8 i
  | Init_int16 i        -> Data_int16 i
  | Init_int32 i        -> Data_int32 i
  | Init_float32 f      -> error_float ()
  | Init_float64 f      -> error_float ()
  | Init_space i        -> error "bad global initialization style."
  | Init_addrof (x,off) -> assert false (* TODO: need the size of [x]'s cells *)

let datas_of_init_datas = function
  | [Init_space _] -> None
  | l -> Some (List.map data_of_init_data l)

let init_mem prog =
  let f_var mem ((x, init_datas), ty) =
    Mem.add_var mem x (ClightToCminor.sizeof_ctype ty)
      (datas_of_init_datas init_datas) in
  let mem = List.fold_left f_var Mem.empty prog.prog_vars in
  let f_fun_def mem (f, def) = Mem.add_fun_def mem f def in
  List.fold_left f_fun_def mem prog.prog_funct

let compute_result v =
  if Value.is_int v then IntValue.Int32.cast (Value.to_int_repr v)
  else IntValue.Int32.zero

let rec exec debug trace (state, l) =
  let cost_labels = l @ trace in
  let print_and_return_result res =
    if debug then Printf.printf "Result = %s\n%!"
      (IntValue.Int32.to_string res) ;
    (res, cost_labels) in
  if debug then print_state state ;
  match state with
    | Returnstate(v,Kstop,_) -> (* Explicit return in main *)
      print_and_return_result (compute_result v)
    | State(_,Sskip,Kstop,_,_) -> (* Implicit return in main *)
      print_and_return_result IntValue.Int32.zero
    | state -> exec debug cost_labels (step state)

let interpret debug prog =
  Printf.printf "*** Clight interpret ***\n%!" ;
  match prog.prog_main with
    | None -> (IntValue.Int32.zero, [])
    | Some main ->
      let mem = init_mem prog in
      let first_state = (Callstate (find_fundef main mem,[],Kstop,mem),[]) in
      exec debug [] first_state

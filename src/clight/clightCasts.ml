
(** [simplify p] removes unnecessary casts in the Clight program [p].

    Example: [(char) ((int)x + (int)y)] where [x] and [y] are of type [char]
    will be transformed into [x + y]. Primitive operations are thus supposed to
    be polymorphic, but working only on homogene types. *)


let error_prefix = "Clight casts simplification"
let error = Error.global_error error_prefix
let error_float () = error "float not supported."


(* Int sizes *)

let int_of_intsize = function
  | Clight.I8 -> 8
  | Clight.I16 -> 16
  | Clight.I32 -> 32

let intsize_of_int = function
  | i when i <= 8 -> Clight.I8
  | i when i <= 16 -> Clight.I16
  | _ -> Clight.I32

let op_intsize_no_cast op size1 size2 =
  op (int_of_intsize size1) (int_of_intsize size2)

let cmp_intsize cmp size1 size2 = op_intsize_no_cast cmp size1 size2

let op_intsize op size1 size2 =
  intsize_of_int (op_intsize_no_cast op size1 size2)

let max_intsize size1 size2 = op_intsize max size1 size2

let intsize_union size1 size2 = op_intsize (+) size1 size2

let pow2 = MiscPottier.pow 2

let belongs_to_int_type size sign i = match size, sign with
  | Clight.I8, AST.Unsigned -> 0 <= i && i <= (pow2 8) - 1
  | Clight.I8, AST.Signed -> -(pow2 7) <= i && i <= (pow2 7) - 1
  | Clight.I16, AST.Unsigned -> 0 <= i && i <= (pow2 16) - 1
  | Clight.I16, AST.Signed -> -(pow2 15) <= i && i <= (pow2 15) - 1
  | Clight.I32, AST.Unsigned -> 0 <= i
  | Clight.I32, AST.Signed ->
    let pow2_30 = pow2 30 in
    (-(pow2_30 + pow2_30)) <= i &&
    i <= ((pow2_30 - 1) + pow2_30) (* = 2^31 - 1 *)

let smallest_int_type i =
  let (size, sign) = match i with
  | _ when belongs_to_int_type Clight.I8 AST.Signed i ->
    (Clight.I8, AST.Signed)
  | _ when belongs_to_int_type Clight.I8 AST.Unsigned i ->
    (Clight.I8, AST.Unsigned)
  | _ when belongs_to_int_type Clight.I16 AST.Signed i ->
    (Clight.I16, AST.Signed)
  | _ when belongs_to_int_type Clight.I16 AST.Unsigned i ->
    (Clight.I16, AST.Unsigned)
  | _ when belongs_to_int_type Clight.I32 AST.Unsigned i ->
    (Clight.I32, AST.Unsigned)
  | _ ->
    (Clight.I32, AST.Signed) in
  Clight.Tint (size, sign)

let le_int_type size1 sign1 size2 sign2 = match sign1, sign2 with
  | AST.Unsigned, AST.Signed -> cmp_intsize (<) size1 size2
  | AST.Signed, AST.Unsigned -> false
  | _ -> cmp_intsize (<=) size1 size2

let int_type_union t1 t2 =
  let (size, sign) = match t1, t2 with
    | Clight.Tint (size1, sign1), Clight.Tint (size2, sign2)
      when sign1 = sign2 -> (max_intsize size1 size2, sign1)
    | Clight.Tint (size1, sign1), Clight.Tint (size2, sign2) ->
      (intsize_union size1 size2, AST.Signed)
    | _ -> assert false (* only use on int types *)
  in
  Clight.Tint (size, sign)


(* C types *)

let type_of_expr (Clight.Expr (_, t)) = t

let cast_if_needed t (Clight.Expr (ed, t') as e) = match t, ed with
  | _ when t = t' -> e
  | Clight.Tint (size, sign), Clight.Econst_int i
    when belongs_to_int_type size sign i ->
    Clight.Expr (Clight.Econst_int i, t)
  | _ -> Clight.Expr (Clight.Ecast (t, e), t)

let le_ctype t1 t2 = match t1, t2 with
  | Clight.Tint (size1, sign1), Clight.Tint (size2, sign2) ->
    le_int_type size1 sign1 size2 sign2
  | _ -> t1 = t2


(* Simplification *)

let rec simplify_bool_op f_bool t e1 e2 =
  let (e1', e2', t') = simplify_and_same_type t e1 e2 in
  Clight.Expr (f_bool e1' e2', t')

and simplify_and_same_type t e1 e2 =
  let e1' = simplify_expr e1 in
  let e2' = simplify_expr e2 in
  if type_of_expr e1' = type_of_expr e2' then (e1', e2', type_of_expr e1')
  else (cast_if_needed t e1', cast_if_needed t e2', t)

and simplify_expr (Clight.Expr (ed, t) as e) = match ed with

  | Clight.Econst_int i ->
    let t' = smallest_int_type i in
    Clight.Expr (ed, t')

  | Clight.Evar _ -> e

  | Clight.Esizeof _ ->
    let intsize = intsize_of_int (Driver.TargetArch.int_size * 8) in
    Clight.Expr (ed, Clight.Tint (intsize, AST.Unsigned))

  | Clight.Econst_float _ -> error_float ()

  | Clight.Ederef e ->
    let e' = simplify_expr e in
    Clight.Expr (Clight.Ederef e', t)

  | Clight.Eaddrof e ->
    let e' = simplify_expr e in
    Clight.Expr (Clight.Eaddrof e', t)

  | Clight.Eunop _ -> e

  | Clight.Ebinop _ -> e

  (* [(t1) unop ((t2) e)], when [e] simplified has type [t1] and [t1] <= [t2],
     is simplified to [unop e] *)
  | Clight.Ecast
      (t1,
       Clight.Expr
	 (Clight.Eunop (unop, Clight.Expr (Clight.Ecast (_, e'), _)), t2))
      when le_ctype t1 t2 ->
    let e' = simplify_expr e' in
    let t' = type_of_expr e' in
    if t' = t1 then Clight.Expr (Clight.Eunop (unop, e'), t') else e

  (* [(t) ((t') e1 binop (t') e2)], when [e1] and [e2] simplified have type [t]
     and [t] <= [t'], is simplified to [e] *)
  | Clight.Ecast
      (t,
       Clight.Expr
	 (Clight.Ebinop
	    (binop,
	     Clight.Expr (Clight.Ecast (_, e1), _),
	     Clight.Expr (Clight.Ecast (_, e2), _)),
	  t'))
      when le_ctype t t' ->
    let e1 = simplify_expr e1 in
    let t1 = type_of_expr e1 in
    let e2 = simplify_expr e2 in
    let t2 = type_of_expr e2 in
    if t1 = t && t2 = t then Clight.Expr (Clight.Ebinop (binop, e1, e2), t)
    else e

  | Clight.Ecast (t', e) ->
    Clight.Expr (Clight.Ecast (t', simplify_expr e), t')

  | Clight.Econdition (e1, e2, e3) ->
    let e1' = simplify_expr e1 in
    let (e2', e3', t') = simplify_and_same_type t e2 e3 in
    Clight.Expr (Clight.Econdition (e1', e2', e3'), t')

  | Clight.Eandbool (e1, e2) ->
    simplify_bool_op (fun e1' e2' -> Clight.Eandbool (e1', e2')) t e1 e2

  | Clight.Eorbool (e1, e2) ->
    simplify_bool_op (fun e1' e2' -> Clight.Eorbool (e1', e2')) t e1 e2

  | Clight.Efield (e, field) ->
    Clight.Expr (Clight.Efield (simplify_expr e, field), t)

  | Clight.Ecost (lbl, e) ->
    Clight.Expr (Clight.Ecost (lbl, simplify_expr e), t)

  | Clight.Ecall (id, e1, e2) ->
    assert false (* should be impossible *)


let f_ctype ctype _ = ctype

let f_expr e _ _ = e

let f_expr_descr e _ _ =  e

let f_statement stmt _ sub_stmts_res =
  let f_expr b e =
    let e' = simplify_expr e in
    if b then cast_if_needed (type_of_expr e) e'
    else e' in
  let f_exprs b = List.map (f_expr b) in
  let f_sub_exprs = match stmt with
    | Clight.Sassign _ | Clight.Scall _ | Clight.Sreturn _ -> f_exprs true
    | _ -> f_exprs false in
  let sub_exprs = f_sub_exprs (ClightFold.statement_sub_exprs stmt) in
  ClightFold.statement_fill_subs stmt sub_exprs sub_stmts_res

let simplify_stmt = ClightFold.statement f_ctype f_expr f_expr_descr f_statement

let simplify_funct (id, fundef) =
  let fundef' = match fundef with
    | Clight.Internal cfun ->
      Clight.Internal
	{ cfun with Clight.fn_body = simplify_stmt cfun.Clight.fn_body }
    | _ -> fundef in
  (id, fundef')

let simplify p =
  { p with Clight.prog_funct = List.map simplify_funct p.Clight.prog_funct }

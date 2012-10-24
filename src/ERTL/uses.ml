
(* Pasted from Pottier's PP compiler *)

(* This module offers functions that count how many times each
   pseudo-register is used within a piece of [ERTL] code. This
   is used in [Coloring] to drive the spilling heuristics. *)

let lookup uses r =
  try
    Register.Map.find r uses
  with Not_found ->
    0

let count r uses = Register.Map.add r (lookup uses r + 1) uses

let countl l uses =
  let f uses r = count r uses in
  List.fold_left f uses l

let examine_statement _ stmt uses =
  match stmt with
  | ERTL.St_skip _
  | ERTL.St_comment _
  | ERTL.St_cost _
  | ERTL.St_hdw_to_hdw _
  | ERTL.St_newframe _
  | ERTL.St_delframe _
  | ERTL.St_return _ ->
    uses
  | ERTL.St_get_hdw (r, _, _)
  | ERTL.St_set_hdw (_, r, _)
  | ERTL.St_framesize (r, _)
  | ERTL.St_int (r, _, _)
  | ERTL.St_addrN (r, _, _, _)
  | ERTL.St_cond (r, _, _) ->
    count r uses
  | ERTL.St_move (r1, r2, _)
  | ERTL.St_unop (_, r1, r2, _) ->
    countl [r1 ; r2] uses
  | ERTL.St_binop (_, r1, r2, r3, _) ->
    countl [r1 ; r2 ; r3] uses
  | ERTL.St_call (addr, _, _)
  | ERTL.St_tailcall (addr, _) ->
    countl addr uses
  | ERTL.St_load (_, r, addr, _)
  | ERTL.St_store (_, addr, r, _) ->
    countl (r :: addr) uses


let examine_internal int_fun =
  let uses =
    Label.Map.fold examine_statement int_fun.ERTL.f_graph Register.Map.empty
  in
  lookup uses

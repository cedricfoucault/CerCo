(* Pasted from Pottier's PP compiler *)

(* If [i] can store a non-zero value into pseudo-register [r], then
   [nonzeroable i] returns the singleton [r], otherwise it returns the
   empty set. *)

let nonzeroable = function
  | ERTL.St_comment _
  | ERTL.St_call _
  | ERTL.St_tailcall _
  | ERTL.St_newframe _
  | ERTL.St_delframe _
  | ERTL.St_store _
  | ERTL.St_skip _
  | ERTL.St_cond _
  | ERTL.St_return _
  | ERTL.St_int (_, 0, _)
  | ERTL.St_set_hdw _
  | ERTL.St_hdw_to_hdw _
  | ERTL.St_cost _ ->
    Register.Set.empty
  | ERTL.St_move (r, _, _)
  | ERTL.St_framesize (r, _)
  | ERTL.St_addrN (r, _, _, _)
  | ERTL.St_get_hdw (r, _, _)
  | ERTL.St_int (r, _, _)
  | ERTL.St_unop (_, r, _, _)
  | ERTL.St_binop (_, r, _, _, _)
  | ERTL.St_load (_, r, _, _) ->
    Register.Set.singleton r

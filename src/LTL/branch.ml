
(* Pasted from Pottier's PP compiler *)

open LTL

let compress entry graph =

  (* Build a table that maps every graph label to a distinct ``point''
     in the sense of [UnionFind]. *)

  let points =
    Label.Map.mapi (fun label _ ->
      UnionFind.fresh label
    ) graph
  in

  let lookup label =
    try
      Label.Map.find label points
    with Not_found ->
      assert false
  in

  (* For every [St_skip] statement, make the source label an alias for
     the target label, unless the former is already an alias for the
     latter (which means that the graph contains a cycle of [St_skip]
     statements). *)

  Label.Map.iter (fun src stmt ->
    let source = lookup src in
    match stmt with
    | St_skip trgt ->
	let target = lookup trgt in
	if UnionFind.equivalent source target then
	  assert false (* can happen if the program contains an empty infinite loop, but let's ignore that *)
	else
	  UnionFind.union source target
    | _ ->
	()
  ) graph;

  (* Transform the graph by replacing every label with its representative. *)

  let rep label =
    UnionFind.find (lookup label)
  in

  rep entry, Label.Map.map (function
    | LTL.St_skip l ->
      LTL.St_skip (rep l) (* statement will be unreachable *)
    | LTL.St_comment (s, l) ->
      LTL.St_comment (s, rep l)
    | LTL.St_cost (lbl, l) ->
      LTL.St_cost (lbl, rep l)
    | LTL.St_int (r, i, l) ->
      LTL.St_int (r, i, rep l)
    | LTL.St_addr (addr, x, l) ->
      LTL.St_addr (addr, x, rep l)
    | LTL.St_unop (unop, destr, srcr, l) ->
      LTL.St_unop (unop, destr, srcr, rep l)
    | LTL.St_binop (binop, destr, srcr1, srcr2, l) ->
      LTL.St_binop (binop, destr, srcr1, srcr2, rep l)
    | LTL.St_load (size, destr, addr, l) ->
      LTL.St_load (size, destr, addr, rep l)
    | LTL.St_store (size, addr, srcr, l) ->
      LTL.St_store (size, addr, srcr, rep l)
    | LTL.St_call (f, l) ->
      LTL.St_call (f, rep l)
    | LTL.St_tailcall f ->
      LTL.St_tailcall f
    | LTL.St_cond (r, lbl_true, lbl_false) ->
      LTL.St_cond (r, rep lbl_true, rep lbl_false)
    | LTL.St_return ->
      LTL.St_return
  ) graph

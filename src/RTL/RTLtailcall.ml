
(** Temporary issue backdoor: tranform tail calls into regular calls *)

let simplify_stmt exit lbl stmt graph = match stmt with
  | RTL.St_tailcall (f, args) ->
    Label.Map.add lbl (RTL.St_call (f, args, [], exit)) graph
  | _ -> graph

let simplify_graph exit graph =
  Label.Map.fold (simplify_stmt exit) graph graph

let simplify_internal def =
  { def with RTL.f_graph = simplify_graph def.RTL.f_exit def.RTL.f_graph }

let simplify_funct (id, def) =
  let def' = match def with
    | RTL.F_int def -> RTL.F_int (simplify_internal def)
    | RTL.F_ext def -> RTL.F_ext def
  in
  (id, def')

let simplify p =
  { p with RTL.functs = List.map simplify_funct p.RTL.functs }

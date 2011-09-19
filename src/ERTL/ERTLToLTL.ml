
(* This module provides a translation of ERTL programs to LTL
   programs. *)

(* Adapted from Pottier's PP compiler *)

let translate_internal f (int_fun : ERTL.internal_function)
    : LTL.internal_function =

  (* Allocate a reference that will hold the control flow
     graph. Define a function that generates a statement at a fresh
     label. *)

  let graph = ref Label.Map.empty in

  let fresh_label () = Label.Gen.fresh int_fun.ERTL.f_luniverse in

  let add_graph lbl stmt = graph := Label.Map.add lbl stmt !graph in

  let generate stmt =
    let l = fresh_label () in
    add_graph l stmt ;
    l in

  (* Build an interference graph for this function, and color
     it. Define a function that allows consulting the coloring. *)

  let module G = struct
    let liveafter, graph = Build.build int_fun
    let uses = Uses.examine_internal int_fun
    let verbose = false
    let () =
      if verbose then
	Printf.printf "Starting hardware register allocation for %s.\n" f
  end in

  let module C = Coloring.Color (G) in

  let lookup r =
    Interference.Vertex.Map.find (Interference.lookup G.graph r) C.coloring
  in

  (* Restrict the interference graph to concern spilled vertices only,
     and color it again, this time using stack slots as colors. *)

  let module H = struct
    let graph = Interference.droph (Interference.restrict G.graph (fun v ->
      match Interference.Vertex.Map.find v C.coloring with
      | Coloring.Spill ->
	  true
      | Coloring.Color _ ->
	  false
    ))
    let verbose = false
    let () =
      if verbose then
	Printf.printf "Starting stack slot allocation for %s.\n" f
  end in

  let module S = Spill.Color (H) in

  (* Define a new function that consults both colorings at once. *)

  let lookup r =
    match lookup r with
    | Coloring.Spill ->
	ERTLToLTLI.Spill (Interference.Vertex.Map.find (Interference.lookup H.graph r) S.coloring)
    | Coloring.Color color ->
	ERTLToLTLI.Color color
  in

  (* We are now ready to instantiate the functor that deals with the
     translation of instructions. The reason why we make this a
     separate functor is purely pedagogical. Smaller modules are
     easier to understand. *)

  (* We add the additional stack size required for spilled register to the stack
     size previously required for the function: this is the final stack size
     required for the locals. *)

  let locals = S.locals + int_fun.ERTL.f_stacksize in

  (* The full stack size is then the size of the locals in the stack plus the
     size of the formal parameters of the function that will live in the
     stack. *)

  let stacksize =
    let formals_in_stack =
      max 0 (int_fun.ERTL.f_params -
	     (List.length Driver.TargetArch.parameters)) in
    let formals_stack_size = Driver.TargetArch.int_size * formals_in_stack in
    formals_stack_size + locals in

  let module I = ERTLToLTLI.Make (struct
    let lookup = lookup
    let generate = generate
    let fresh_label = fresh_label
    let add_graph = add_graph
    let locals = locals
    let stacksize = stacksize
  end) in

  (* Translate the instructions in the existing control flow graph.
     Pure instructions whose destination pseudo-register is dead are
     eliminated on the fly. *)

  let () =
    Label.Map.iter (fun label stmt ->
      let stmt =
	match Liveness.eliminable (G.liveafter label) stmt with
	| Some successor ->
	    LTL.St_skip successor
	| None ->
	    I.translate_statement stmt
      in
      graph := Label.Map.add label stmt !graph
    ) int_fun.ERTL.f_graph
  in

  (* Build a [LTL] function. *)

  {
    LTL.f_luniverse = int_fun.ERTL.f_luniverse;
    LTL.f_stacksize = stacksize ;
    LTL.f_entry = int_fun.ERTL.f_entry;
    LTL.f_exit = int_fun.ERTL.f_exit;
    LTL.f_graph = !graph
  }


let translate_funct (name, def) =
  let def' = match def with
    | ERTL.F_int def -> LTL.F_int (translate_internal name def)
    | ERTL.F_ext def -> LTL.F_ext def in
  (name, def')

let translate (p : ERTL.program) : LTL.program =
  { LTL.globals = p.ERTL.globals ;
    LTL.functs = List.map translate_funct p.ERTL.functs ;
    LTL.main = p.ERTL.main }

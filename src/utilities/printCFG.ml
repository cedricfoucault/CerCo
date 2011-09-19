(* Pasted from Pottier's PP compiler *)

open PrintPottier

let print_graph instruction successors () (graph, entry) =

  (* Print instructions in depth-first order; this is a
     rather natural and readable order. *)

  let rec visit (visited, lines) l =
    if Label.Set.mem l visited then
      visited, lines
    else
      let visited = Label.Set.add l visited in
	let i = Label.Map.find l graph in
        let lines = instruction l i :: lines in
	List.fold_left visit (visited, lines) (successors i)
(*
      try
	let i = Label.Map.find l graph in
        let lines = instruction l i :: lines in
	List.fold_left visit (visited, lines) (successors i)
      with Not_found ->
	visited, lines
*)
  in

  let _, lines = visit (Label.Set.empty, []) entry in
  String.concat "\n" (catenate (transposerev lines))


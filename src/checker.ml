let same_traces (traces : ((Languages.ast * AST.trace) list)) =
  let compare_trace trace1 trace2 =
    let occs_trace1 = Misc.ListExt.multi_set_of_list trace1
    and occs_trace2 = Misc.ListExt.multi_set_of_list trace2 in
    Misc.ListExt.assoc_diff occs_trace1 occs_trace2
  in
  let check_trace (_, (_, trace1)) (_, (_, trace2)) =
    compare_trace trace1 trace2 = []
  in
  let print_trace lang1 lang2 ds = 
    let string_of_value = function
      | None -> "is not present"
      | Some v -> Printf.sprintf "appears %d times" v
    in 
    let sentence (k, (v1, v2)) =
      Printf.sprintf "  Label %s %s in language `%s' \
                        whereas it %s in language `%s'."
	k (string_of_value v1) lang1 (string_of_value v2) lang2
    in
    String.concat "\n" (List.map sentence ds) 
  in		
  match Misc.ListExt.transitive_forall2 check_trace traces with
    | None -> ()
    | Some ((ast1, (res1, trace1)), (ast2, (res2, trace2))) ->
      let lang1 = Languages.to_string (Languages.language_of_ast ast1)
      and lang2 = Languages.to_string (Languages.language_of_ast ast2) in
      let diff = compare_trace trace1 trace2 in
      Error.global_error "during trace comparison"
	(Printf.sprintf 
	   "The traces of two intermediate programs differ:\n%s"
	   (print_trace lang1 lang2 diff))

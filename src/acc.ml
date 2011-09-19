open Options

(** Parse the command line. *)
let input_files = OptionsParsing.results ()

(** For each input file of the source language: 

    1. Parse.

    2. Add runtime functions.

    3. Labelize.

    4. Compile to the target language. 
       (And keep track of annotations if required). 
  
    5. Annotate the input program with collected costs. 

    6. Save the annotated input program.

    Optionnally, we can interpret the intermediate programs
    if {!Options.interpretation_requested}. 
*)
let process filename = 
  let _ = Printf.printf "Processing %s.\n%!" filename in
  let src_language = Options.get_source_language () in
  let tgt_language = Options.get_target_language () in
  let is_lustre_file = Options.is_lustre_file () in
  let remove_lustre_externals = Options.is_remove_lustre_externals () in
  let input_ast =
    Languages.parse ~is_lustre_file ~remove_lustre_externals
      src_language filename in
  let input_ast = Languages.add_runtime input_ast in 
  let input_ast = Languages.labelize input_ast in
  let (exact_output, output_filename) = match Options.get_output_files () with
    | None -> (false, filename)
    | Some filename' -> (true, filename') in
  let save ?(suffix="") ast = 
    Languages.save exact_output output_filename suffix ast 
  in
  let target_asts =
    (** If debugging is enabled, the compilation function returns all
	the intermediate programs. *)
    Languages.compile (Options.is_debug_enabled ()) 
      src_language tgt_language input_ast
  in
  let final_ast, intermediate_asts = Misc.ListExt.cut_last target_asts in
  save final_ast;
  if Options.annotation_requested () then
    begin
      let (annotated_input_ast, cost_id, cost_incr, extern_cost_variables) =
	Languages.annotate input_ast final_ast in (
	  save ~suffix:"-annotated" annotated_input_ast;
	  Languages.save_cost exact_output output_filename cost_id cost_incr
	    extern_cost_variables);
    end;
					     
  if Options.is_debug_enabled () then 
    List.iter save intermediate_asts;

  if Options.interpretations_requested () then
    begin
      let asts = target_asts in
      let debug = Options.is_debug_enabled () in
      let label_traces = List.map (Languages.interpret debug) asts in
      Printf.eprintf "Checking execution traces...%!";
      Checker.same_traces (List.combine asts label_traces);
      Printf.eprintf "OK.\n%!";
    end;

  if Options.interpretation_requested () then
    ignore (Languages.interpret (Options.is_debug_enabled ()) final_ast)

let lustre_test filename =
  let lustre_test       = match Options.get_lustre_test () with
    | None -> assert false (* do not use on this argument *)
    | Some lustre_test -> lustre_test in
  let lustre_test_cases = Options.get_lustre_test_cases () in
  let lustre_test_cycles = Options.get_lustre_test_cycles () in
  let lustre_test_min_int = Options.get_lustre_test_min_int () in
  let lustre_test_max_int = Options.get_lustre_test_max_int () in
  let src_language      = Languages.Clight in
  let tgt_language      = Languages.Clight in
  let input_ast         = Languages.parse src_language filename in
  let input_ast         =
    Languages.add_lustre_main lustre_test lustre_test_cases lustre_test_cycles
      lustre_test_min_int lustre_test_max_int input_ast in 
  let (exact_output, output_filename) = match Options.get_output_files () with
    | None -> (false, filename)
    | Some filename' -> (true, filename') in
  let save ast = Languages.save exact_output output_filename "" ast in
  let target_asts = Languages.compile false src_language tgt_language input_ast
  in
  let final_ast, _ = Misc.ListExt.cut_last target_asts in
  save input_ast ;
  save final_ast

let _ =
  if Options.is_dev_test_enabled () then Dev_test.do_dev_test input_files
  else
    if Options.get_lustre_test () <> None then List.iter lustre_test input_files
    else List.iter process input_files

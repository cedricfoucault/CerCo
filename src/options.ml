open Misc.ArgExt

let default_choice       = "default"
let option_settings_step = "during option settings"

let language_from_string kind default s =
  try 
    Languages.from_string s
  with Not_found -> 
    if s = default_choice then 
      default
    else 
      Error.global_error option_settings_step 
	(Printf.sprintf "`%s' is not a valid %s language." s kind)
  
let source_language_of_string   = language_from_string "source" Languages.Clight
let source_language             = ref (source_language_of_string default_choice)
let set_source_language s       = source_language := source_language_of_string s
let get_source_language ()      = !source_language

let target_language_of_string   = language_from_string "target" Languages.ASM
let target_language             = ref (target_language_of_string default_choice)
let set_target_language s       = target_language := target_language_of_string s
let get_target_language ()      = !target_language

let input_files		        = ref []
let add_input_file f            = input_files := f :: !input_files
let input_files ()		= !input_files

let output_files                = ref None
let set_output_files s          = output_files := Some s
let get_output_files ()		= !output_files

let annotation_flag		= ref false
let request_annotation		= (:=) annotation_flag
let annotation_requested ()	= !annotation_flag

let interpretation_flag         = ref false
let request_interpretation      = (:=) interpretation_flag
let interpretation_requested () = !interpretation_flag

let interpretations_flag         = ref false
let request_interpretations      = (:=) interpretations_flag
let interpretations_requested () = !interpretations_flag

let debug_flag			= ref false
let set_debug			= (:=) debug_flag
let is_debug_enabled ()		= !debug_flag

let lustre_flag			= ref false
let set_lustre_file		= (:=) lustre_flag
let is_lustre_file ()		= !lustre_flag

let remove_lustre_externals	  = ref false
let set_remove_lustre_externals	  = (:=) remove_lustre_externals
let is_remove_lustre_externals () = !remove_lustre_externals

let lustre_test                 = ref None
let set_lustre_test s           = lustre_test := Some s
let get_lustre_test ()  	= !lustre_test

let lustre_test_cases           = ref 100
let set_lustre_test_cases       = (:=) lustre_test_cases
let get_lustre_test_cases ()	= !lustre_test_cases

let lustre_test_cycles          = ref 100
let set_lustre_test_cycles      = (:=) lustre_test_cycles
let get_lustre_test_cycles () 	= !lustre_test_cycles

let lustre_test_min_int         = ref (-1000)
let set_lustre_test_min_int     = (:=) lustre_test_min_int
let get_lustre_test_min_int () 	= !lustre_test_min_int

let lustre_test_max_int         = ref 1000
let set_lustre_test_max_int     = (:=) lustre_test_max_int
let get_lustre_test_max_int () 	= !lustre_test_max_int

(*
let print_result_flag		= ref false
let set_print_result		= (:=) print_result_flag
let is_print_result_enabled ()	= !print_result_flag
*)

let dev_test			= ref false
let set_dev_test		= (:=) dev_test
let is_dev_test_enabled ()	= !dev_test

let options = OptionsParsing.register [
(*
  "-s", Arg.String set_source_language,
  " Choose the source language between:";
  extra_doc " Clight, Cminor";
  extra_doc " [default is C]";
*)

  "-l", Arg.String set_target_language,
  " Choose the target language between:";
  extra_doc " Clight, Cminor, RTLabs, RTL, ERTL, LTL, LIN, ASM.";
  extra_doc " [default is ASM]";

  "-a", Arg.Set annotation_flag,
  " Add cost annotations on the source code.";

  "-i", Arg.Set interpretation_flag,
  " Interpret the compiled code.";

  "-is", Arg.Set interpretations_flag,
  " Interpret all the compilation passes.";

  "-d", Arg.Set debug_flag,
  " Debugging mode.";

  "-o", Arg.String set_output_files,
  " Prefix of the output files.";

  "-lustre", Arg.Set lustre_flag,
  " Input file is a Lustre file.";

  "-remove-lustre-externals", Arg.Set remove_lustre_externals,
  " Remove Lustre externals.";

  "-lustre-test", Arg.String set_lustre_test,
  " Input file is a Lustre file, testing requested.";

  "-lustre-test-cases", Arg.Int set_lustre_test_cases,
  " Set the number of test cases when testing a Lustre";
  extra_doc " file.";
  extra_doc " [default is 100]";

  "-lustre-test-cycles", Arg.Int set_lustre_test_cycles,
  " Set the number of cycles for each case when testing";
  extra_doc " a Lustre file.";
  extra_doc " [default is 100]";

  "-lustre-test-min-int", Arg.Int set_lustre_test_min_int,
  " Random int minimum value when testing a Lustre file.";
  extra_doc " [default is -1000]";

  "-lustre-test-max-int", Arg.Int set_lustre_test_max_int,
  " Random int maximum value when testing a Lustre file.";
  extra_doc " [default is 1000]";

(*
  "-res", Arg.Set print_result_flag,
  " Print the result of interpretations.";
*)

  "-dev", Arg.Set dev_test,
  " Playground for developers.";
]


let error_prefix = "Clight-Lustre main"
let error s = Error.global_error error_prefix s


let extract_info lustre_test =
  let error () = error ("bad format of file " ^ lustre_test) in
  try
    let ic = open_in lustre_test in
    let step_fun = input_line ic in
    let step_cost = input_line ic in
    let cost_var = input_line ic in
    let cost_incr = input_line ic in
    let rec aux_external_costs () =
      try
	let s = input_line ic in
	if s = "" then StringTools.Map.empty
	else
	  if String.contains s ' ' then
	    let i = String.index s ' ' in
	    let extern_name = String.sub s 0 i in
	    let cost = String.sub s (i+1) ((String.length s) - (i+1)) in
	    StringTools.Map.add extern_name cost (aux_external_costs ())
	  else error ()
      with End_of_file -> error () in
    let rec aux_inputs () =
      try
	let s = input_line ic in
	if s = "" then []
	else s :: (aux_inputs ())
      with End_of_file -> error () in
    let rec aux_booleans () =
      try
	let s = input_line ic in
	s :: (aux_booleans ())
      with End_of_file -> [] in
    let external_costs = aux_external_costs () in
    let inputs = aux_inputs () in
    let booleans = aux_booleans () in
    (booleans, inputs, step_fun, step_cost,
     cost_var, cost_incr, external_costs)
  with Sys_error _ | End_of_file -> error ()    


let define_void_external cost_incr arg_types ret_type cost_var =
  let fresh = StringTools.make_fresh StringTools.Set.empty "x" in
  let fn_return = ret_type in
  let fn_params = List.map (fun t -> (fresh (), t)) arg_types in
  let fn_vars = [] in
  let int_type = Clight.Tint (Clight.I32, AST.Signed) in
  let f_type = Clight.Tfunction ([int_type], Clight.Tvoid) in
  let f = Clight.Expr (Clight.Evar cost_incr, f_type) in
  let args = [Clight.Expr (Clight.Evar cost_var, int_type)] in
  let fn_body = Clight.Scall (None, f, args) in
  { Clight.fn_return = fn_return ; Clight.fn_params = fn_params ;
    Clight.fn_vars = fn_vars ; Clight.fn_body = fn_body }

let define_void_externals_funct cost_incr external_costs (id, def) =
  let def' = match def with
    | Clight.External (_, args, Clight.Tvoid)
	when StringTools.Map.mem id external_costs ->
      Clight.Internal
	(define_void_external cost_incr args Clight.Tvoid
	   (StringTools.Map.find id external_costs))
    | _ -> def in
  (id, def')

let define_void_externals cost_incr external_costs p =
  let prog_funct =
    List.map
      (define_void_externals_funct cost_incr external_costs)
      p.Clight.prog_funct in
  { p with Clight.prog_funct = prog_funct }


let get_struct_arg fun_name p =
  let error () =
    error ("could not fetch the structure of the context of function " ^
	      fun_name ^ ".") in
  if List.mem_assoc fun_name p.Clight.prog_funct then
    match List.assoc fun_name p.Clight.prog_funct with
      | Clight.Internal def when List.length def.Clight.fn_params = 1 ->
	(match snd (List.hd def.Clight.fn_params) with
	  | Clight.Tpointer (Clight.Tstruct (struct_name, fields)) ->
	    (struct_name, fields)
	  | _ -> error ())
      | _ -> error ()
  else error ()

let first_init_field ctx (id, t) = match t with
  | Clight.Tint _ -> ctx ^ "." ^ id ^ " = 0;\n"
  | _ when id = "client_data" -> ""
  | _ -> error ("unsupported type " ^ (ClightPrinter.string_of_ctype t) ^ ".")

let init_fields ctx fields =
  let f res field = res ^ (first_init_field ctx field) in
  List.fold_left f "" fields

let init_field
    lustre_test_min_int lustre_test_max_int booleans inputs ctx (id, t) =
  let lustre_full_range = (lustre_test_max_int - lustre_test_min_int) + 1 in
  match t with
    | Clight.Tint _ when List.mem id inputs && List.mem id booleans ->
      ctx ^ "." ^ id ^ " = rand_bool();\n"
    | Clight.Tint _ when List.mem id inputs ->
      ctx ^ "." ^ id ^ " = rand_int(" ^ (string_of_int lustre_full_range) ^
	") - " ^ (string_of_int lustre_test_min_int) ^ ";\n"
    | _ when id = "client_data" || not (List.mem id inputs) -> ""
    | _ -> error ("unsupported type " ^ (ClightPrinter.string_of_ctype t) ^ ".")

let main_fields
    lustre_test_min_int lustre_test_max_int booleans inputs ctx fields =
  let f res field =
    res ^
      (init_field
	 lustre_test_min_int lustre_test_max_int booleans inputs ctx field) in
  List.fold_left f "" fields

let main_def
    lustre_test_cases lustre_test_cycles lustre_test_min_int lustre_test_max_int
    (struct_name, fields) booleans inputs step_fun step_cost cost_var =
  let ctx = "ctx" in
  let reset_fun = Str.global_replace (Str.regexp "_step") "_reset" step_fun in
  let big_init = init_fields ctx fields in
  let init_inputs =
    main_fields lustre_test_min_int lustre_test_max_int booleans inputs ctx
      fields in

  "int main () {\n" ^
  (* Initializations *)
  "  " ^ struct_name ^ " " ^ ctx ^ ";\n" ^
  "  int wcet = " ^ step_cost ^ ";\n" ^
  "  int old_cost;\n" ^
  "  int et = 0;\n" ^
  "  int min = -1, max = -1, nb_cycles = 0\n;" ^
  "  int i_case, i_cycle;\n" ^

  (* Body *)
  big_init ^
  "  for (i_case = 0 ; i_case < " ^ (string_of_int lustre_test_cases) ^
    " ; i_case++) {\n" ^
  "    old_cost = " ^ cost_var ^ ";\n" ^
  "    " ^ reset_fun ^ "(&" ^ ctx ^ ");\n" ^
  "    " ^ cost_var ^ " = old_cost;\n" ^
  init_inputs ^
  "    for (i_cycle = 0 ; i_cycle < " ^ (string_of_int lustre_test_cycles) ^
    " ; i_cycle++) {\n" ^
  "      old_cost = " ^ cost_var ^ ";\n" ^
  "      " ^ step_fun ^ "(&" ^ ctx ^ ");\n" ^
  "      et = " ^ cost_var ^ " - old_cost;\n" ^
  "      if ((min == -1) || (et < min)) min = et;\n" ^
  "      if ((max == -1) || (et > max)) max = et;\n" ^
  "      nb_cycles++;\n" ^
  "    }\n" ^
  "  }\n" ^

  (* Printing the results *)
  "  print_sint(wcet);\n" ^
  "  newline();\n" ^
  "  print_sint(min);\n" ^
  "  newline();\n" ^
  "  print_sint(max);\n" ^
  "  newline();\n" ^
  "  if (nb_cycles == 0) print_sint(-1);\n" ^
  "  else print_sint(" ^ cost_var ^ "/nb_cycles);\n" ^
  "  newline();\n" ^
  "  return(0);\n" ^
  "}\n"

let add_main_def
    lustre_test_cases lustre_test_cycles lustre_test_min_int
    lustre_test_max_int booleans inputs step_fun step_cost cost_var p =
  let tmp_file = Filename.temp_file "lustre_add_main" ".c" in
  try
    let struct_arg = get_struct_arg step_fun p in
    let s =
      (ClightPrinter.print_program p) ^
      (main_def
	 lustre_test_cases lustre_test_cycles lustre_test_min_int
	 lustre_test_max_int struct_arg booleans inputs
	 step_fun step_cost cost_var) in
    let oc = open_out tmp_file in
    output_string oc s ;
    close_out oc ;
    ClightParser.process tmp_file
  with Sys_error _ ->
    error ("could not save temporary file " ^ tmp_file ^ " with main.")


let add lustre_test lustre_test_cases lustre_test_cycles
    lustre_test_min_int lustre_test_max_int  p =
  let (booleans, inputs, step_fun, step_cost,
       cost_var, cost_incr, external_costs) =
    extract_info lustre_test in
  let p = define_void_externals cost_incr external_costs p in
  add_main_def lustre_test_cases lustre_test_cycles lustre_test_min_int
    lustre_test_max_int booleans inputs step_fun step_cost cost_var p

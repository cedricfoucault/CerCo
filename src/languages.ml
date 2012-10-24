type name = 
  | Clight
  | Cminor
  | RTLabs
  | RTL
  | ERTL
  | LTL
  | LIN
  | ASM

let strings = [
  "Clight", Clight;
  "Cminor", Cminor;
  "RTLabs", RTLabs;
  "RTL"   , RTL;
  "ERTL"  , ERTL;
  "LTL"   , LTL;
  "LIN"   , LIN;
  "ASM"   , ASM;
]

let from_string s = 
  List.assoc s strings

let to_string l = 
  List.assoc l (Misc.ListExt.inv_assoc strings)

type ast = 
  | AstClight of Clight.program
  | AstCminor of Cminor.program
  | AstRTLabs of RTLabs.program
  | AstRTL    of RTL.program
  | AstERTL   of ERTL.program
  | AstLTL    of LTL.program
  | AstLIN    of LIN.program
  | AstASM    of ASM.program

let language_of_ast = function
  | AstClight _ -> Clight
  | AstCminor _ -> Cminor
  | AstRTLabs _ -> RTLabs
  | AstRTL _	-> RTL
  | AstERTL _	-> ERTL
  | AstLTL _    -> LTL
  | AstLIN _	-> LIN
  | AstASM _	-> ASM

let extension = function
  | ASM      -> ["s" ; "hex"]
  | Clight   -> ["c"]
  | language -> [to_string language]

let parse ?is_lustre_file ?remove_lustre_externals = function
  | Clight -> 
    fun filename ->
      AstClight
	(ClightParser.process ?is_lustre_file ?remove_lustre_externals filename)

(*
  | Cminor -> 
    fun filename -> 
      AstCminor 
	(SyntacticAnalysis.process
	   ~lexer_init: (fun filename -> Lexing.from_channel (open_in filename))
	   ~lexer_fun:  CminorLexer.token
	   ~parser_fun: CminorParser.program
	   ~input:      filename)
*)

  | _ ->
    (* FIXME: Will be completed in the next commits. *)
    assert false


let labelize = function
  | AstClight p -> 
    AstClight (ClightLabelling.add_cost_labels p)

(*
  | AstCminor p -> 
    AstCminor (CminorLabelling.add_cost_labels p)
*)

  | x -> 
    (* For the other languages, no labelling is defined. *)
    x 


let clight_to_cminor = function
  | AstClight p ->
    AstCminor (ClightToCminor.translate p)
  | _ -> assert false

let cminor_to_rtlabs = function
  | AstCminor p -> 
    AstRTLabs (CminorToRTLabs.translate p)
  | _ -> assert false

let rtlabs_to_rtl = function
  | AstRTLabs p -> 
    AstRTL (RTLabsToRTL.translate p)
  | _ -> assert false

let rtl_to_ertl = function
  | AstRTL p -> 
    AstERTL (RTLToERTL.translate p)
  | _ -> assert false

let ertl_to_ltl = function
  | AstERTL p -> 
    AstLTL (ERTLToLTL.translate p)
  | _ -> assert false

let ltl_to_lin = function
  | AstLTL p -> 
    AstLIN (LTLToLIN.translate p)
  | _ -> assert false

let lin_to_asm = function
  | AstLIN p -> 
    AstASM (LINToASM.translate p)
  | _ -> assert false

(* We explicitly denote the compilation chain as a list of 
   passes that must be composed to translate a program 
   from a source language to a target language. *)
let compilation_chain = [
  (* Source language | Target language | Compilation function *) 
  Clight,              Cminor,           clight_to_cminor;
  Cminor,              RTLabs,           cminor_to_rtlabs;
  RTLabs,              RTL,              rtlabs_to_rtl;
  RTL,                 ERTL,             rtl_to_ertl;
  ERTL,                LTL,              ertl_to_ltl;
  LTL,                 LIN,              ltl_to_lin;
  LIN,                 ASM,              lin_to_asm;
]

let compile debug src tgt = 
  (* Find the maximal suffix of the chain that starts with the
     language [src]. *)
  let rec subchain = function
    | [] -> 
      (* The chain is assumed to be well-formed: such a suffix 
	 exists. *)
      assert false 
    | ((l, _, _) :: _) as chain when l = src -> chain
    | _ :: chain -> subchain chain
  in
  (* Compose the atomic translations to build a compilation function
     from [src] to [tgt]. Again, we assume that the compilation chain
     is well-formed. Thus, if we cannot find [tgt] in the compilation
     chain then the user must have made a mistake to ask for a
     translation from [src] to [tgt]. *)
  let rec compose iprogs src tgt chains ast = 
    if src = tgt then List.rev (ast :: iprogs)
    else 
      match chains with
	| [] -> 
	  Error.global_error "During compilation configuration"
	    (Printf.sprintf "It is not possible to compile from `%s' to `%s'."
	       (to_string src)
	       (to_string tgt))
	    
	| (l1, l2, src_to_l2) :: chain ->
	  assert (l1 = src);
	  let l2_to_tgt = compose iprogs l2 tgt chain in
	  let iprog = src_to_l2 ast in
	  ast :: l2_to_tgt iprog
  in
  compose [] src tgt (subchain compilation_chain)


(** [add_runtime ast] adds runtime functions for the operations not supported by
    the target processor. *)
let add_runtime = function
  | AstClight p ->
    AstClight (Unsupported.replace (ClightSwitch.simplify p))
  | x -> 
    (* For the other languages, no runtime functios are defined. *)
    x 


let compute_costs = function
  | AstClight p -> 
  (* Computing costs on Clight programs cannot be done directly
     because the control-flow is not explicit. Yet, for
     incremental construction and test of the compiler, we 
     build a stupid mapping from labels to costs for a Clight
     program that gives cost 1 to every label. *)
    CostLabel.constant_map (ClightAnnotator.cost_labels p) 1

  | AstCminor p -> 
  (* Computing costs on Cminor programs cannot be done directly
     because the control-flow is not explicit. Yet, for
     incremental construction and test of the compiler, we 
     build a stupid mapping from labels to costs for a Cminor
     program that gives cost 1 to every label. *)
    CostLabel.constant_map (CminorAnnotator.cost_labels p) 1

  | AstASM p ->
    ASMCosts.compute p

  | ast -> 
    Error.global_error "during cost computing"
      (Printf.sprintf 
	 "Cost computing is not implemented for language `%s'\ 
          Please compile to ASM if you want to annotate the input \
          file or deactivate annotation using the '-no-annotation' flag."
	 (to_string (language_of_ast ast)))

(* FIXME *)
let instrument costs_mapping = function
  | AstClight p ->
    let (p', cost_id, cost_incr, extern_cost_variables) =
      ClightAnnotator.instrument p costs_mapping in
    (AstClight p', cost_id, cost_incr, extern_cost_variables)
(*
  | AstCminor p ->
    let (p', cost_id, cost_incr) = CminorAnnotator.instrument p costs_mapping in
    (AstCminor p', cost_id, cost_incr)
*)
  | p -> 
    Error.warning "during instrumentation"
      (Printf.sprintf 
	 "Instrumentation is not implemented for source language `%s'."
	 (to_string (language_of_ast p)));
    (p, "", "", StringTools.Map.empty)

let annotate input_ast final = 
  let costs_mapping = compute_costs final in 
  instrument costs_mapping input_ast

let string_output = function
  | AstClight p -> 
    [ClightPrinter.print_program p]
  | AstCminor p ->
    [CminorPrinter.print_program p]
  | AstRTLabs p ->
    [RTLabsPrinter.print_program p]
  | AstRTL p ->
    [RTLPrinter.print_program p]
  | AstERTL p ->
    [ERTLPrinter.print_program p]
  | AstLTL p ->
    [LTLPrinter.print_program p]
  | AstLIN p ->
    [LINPrinter.print_program p]
  | AstASM p ->
    ASMPrinter.print_program p

let save exact_output filename suffix ast =
  let ext_chopped_filename =
    if exact_output then filename 
    else
      try Filename.chop_extension filename
      with Invalid_argument ("Filename.chop_extension") -> filename in
  let ext_chopped_filename = ext_chopped_filename ^ suffix in
  let ext_filenames =
    List.map (fun ext -> ext_chopped_filename ^ "." ^ ext)
      (extension (language_of_ast ast)) in
  let output_filenames =
    if exact_output then ext_filenames
    else List.map Misc.SysExt.alternative ext_filenames in
  let output_strings = string_output ast in
  let f filename s =
    let cout = open_out filename in
    output_string cout s;
    flush cout;
    close_out cout in
  List.iter2 f output_filenames output_strings

let save_cost exact_name filename cost_id cost_incr extern_cost_variables =
  let filename =
    if exact_name then filename
    else
      try Filename.chop_extension filename
      with Invalid_argument ("Filename.chop_extension") -> filename in
  let cout = open_out (filename ^ ".cerco") in
  let f fun_name cost_var =
    output_string cout (fun_name ^ " " ^ cost_var ^ "\n") in
  output_string cout (cost_id ^ "\n");
  output_string cout (cost_incr ^ "\n");
  StringTools.Map.iter f extern_cost_variables;
  flush cout;
  close_out cout

let interpret debug = function
  | AstClight p ->
    ClightInterpret.interpret debug p
  | AstCminor p ->
    CminorInterpret.interpret debug p  
  | AstRTLabs p ->
    RTLabsInterpret.interpret debug p
  | AstRTL p ->
    RTLInterpret.interpret debug p
  | AstERTL p ->
    ERTLInterpret.interpret debug p
  | AstLTL p ->
    LTLInterpret.interpret debug p
  | AstLIN p ->
    LINInterpret.interpret debug p
  | AstASM p ->
    ASMInterpret.interpret debug p

let add_lustre_main
    lustre_test lustre_test_cases lustre_test_cycles
    lustre_test_min_int lustre_test_max_int = function
  | AstClight p ->
    AstClight
      (ClightLustreMain.add lustre_test lustre_test_cases lustre_test_cycles
	 lustre_test_min_int lustre_test_max_int p)
  | _ ->
    Error.global_error "during main generation"
      "Lustre testing is only available with C programs."

(** This module defines the intermediate languages. 

    This is a dispatching module that is aware of the whole
    compilation chain. It can also be used as an homogeneous way to
    deal with the intermediate languages functionalities.
*)


type name = 
  | Clight
  | Cminor
  | RTLabs
  | RTL
  | ERTL
  | LTL
  | LIN
  | ASM

(** {2 Abstract syntax trees} *)

(** The types of abstract syntax trees of each language. *)
type ast = 
  | AstClight  of Clight.program
  | AstCminor  of Cminor.program
  | AstRTLabs  of RTLabs.program
  | AstRTL     of RTL.program
  | AstERTL    of ERTL.program
  | AstLTL     of LTL.program
  | AstLIN     of LIN.program
  | AstASM     of ASM.program

(** [language_of_ast ast] returns the programming language of the 
    abstract syntax tree [ast]. *)
val language_of_ast : ast -> name

(** [parse ?is_lustre_file ?remove_lustre_externals name] returns the parsing
    function of the language [name]. *)
val parse : ?is_lustre_file:bool -> ?remove_lustre_externals:bool ->
  name -> (string -> ast) 

(** {2 Compilation} *)

(** [compile debug l1 l2] returns the compilation function that
    translates the language [l1] to the language [l2]. This may be the
    composition of several compilation functions. If [debug] is
    [true], all the intermediate programs are inserted in the
    output. *)
val compile : bool -> name -> name -> (ast -> ast list)

(** [add_runtime ast] adds runtime functions for the operations not supported by
    the target processor. *)
val add_runtime : ast -> ast

(** {2 Annotation} 

    Labelling consists in the insertion of so-called "cost labels" 
    which are useful control points in order to compute costs.

    The annotation process first computes cost of constant-time
    execution path starting from each cost label on the lowest-level
    language. Then, it instruments the (high-level) source code with
    these computed costs.
*)

(** [labelize ast] inserts cost labels in the program [ast]. *)
val labelize : ast -> ast

(** [annotate input_ast target_ast] inserts cost annotations into the input AST
    from the (final) target AST. It also returns the name of the cost variable,
    the name of the cost increment function, and a the name of a fresh
    uninitialized global variable for each external function. *)
val annotate : ast -> ast -> (ast * string * string * string StringTools.Map.t)

(** [interpret debug ast] runs the program [ast] from the default initial
    configuration. This interpretation may emit some cost labels. *)
val interpret : bool -> ast -> AST.trace

(** {2 Serialization} *)

(** [save exact_output filename input_ast] pretty prints [input_ast] in a file
    whose name is prefixed by [filename] and whose extension is deduced from the
    language of the AST. If [exact_output] is false then the written file will
    be fresh. *)
val save : bool -> string -> string -> ast -> unit

(** [save_cost_incr exact_name filename cost_id cost_incr extern_cost_variables]
    prints the name [cost_id] of the cost variable, then the name [cost_incr] of
    the cost increment function, and the entries of the mapping
    [extern_cost_variables] (key first, then binding, seperated by a space) in a
    seperate line in the file prefixed by [filename] and extended with
    ".cost". If the file already exists, it is overwritten. *)
val save_cost : bool -> string -> string -> string ->
  string StringTools.Map.t -> unit

(** [from_string s] parses [s] as an intermediate language name. *)
val from_string : string -> name

(** [to_string n] prints [n] as a string. *)
val to_string   : name -> string

(** [add_lustre_main lustre_test lustre_test_cases lustre_test_cycles
    lustre_test_min_int lustre_test_max_int ast] adds a main function that tests
    a Lustre step function to a Clight AST. The file [lustre_test] contains
    CerCo information (e.g. the name of the cost variable). The integer
    [lustre_test_cases] is the number of test cases that are performed, and the
    integer [lustre_test_cycles] is the number of cycles per test
    case. [lustre_test_min_int] (resp. [lustre_test_max_int]) is the minimum
    (resp. maximum) integer value randomly generated during testing, and. *)
val add_lustre_main : string -> int -> int -> int -> int -> ast -> ast

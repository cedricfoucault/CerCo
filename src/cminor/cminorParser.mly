/* Adapted from Leroy's CompCert */
/* TODO: check coherence with CminorPrinter */

/* tokens ALLOC, DOLLAR, IN, LET, p_let were unused and have been removed */
/* precedence levels unused were also removed */

/* *********************************************************************/
/*                                                                     */
/*              The Compcert verified compiler                         */
/*                                                                     */
/*          Xavier Leroy, INRIA Paris-Rocquencourt                     */
/*                                                                     */
/*  Copyright Institut National de Recherche en Informatique et en     */
/*  Automatique.  All rights reserved.  This file is distributed       */
/*  under the terms of the GNU General Public License as published by  */
/*  the Free Software Foundation, either version 2 of the License, or  */
/*  (at your option) any later version.  This file is also distributed */
/*  under the terms of the INRIA Non-Commercial License Agreement.     */
/*                                                                     */
/* *********************************************************************/

%{

  open AST
  open Cminor
  open Memory

  let error_prefix = "Cminor parser"
  let error s = Error.global_error error_prefix s
  let warning s = Error.warning error_prefix s
  let error_float () = error "float not supported."

  let uint32 = (4, Unsigned)
  let int32 = (4, Signed)

  (* Function calls are not allowed in the AST of expressions, but function
     calls in the AST of statements have a special argument which can be used to
     store the result of the function call in a side-effect manner.
     Example: the statement
       x = y + f(z,g(t));
     will be transformed into the (simplified syntax) AST statements
       g(_t1,t)
       f(_t2,y,_t1);
       x = y + _t2
     where _t1 and _t2 are fresh temporary variables. *)


  (* Thus, to deal with function calls in expressions, we need to create fresh
     temporary variables *)

  let temp_counter = ref 0
  let temporaries = ref []

  let mktemp () =
    incr temp_counter;
    let id = Printf.sprintf "_t%d" !temp_counter in
      temporaries := id :: !temporaries;
      id


  (* Expressions with function calls *)

  type rexpr =
    | RId of ident
    | RCst of cst
    | ROp1 of op1 * rexpr
    | ROp2 of op2 * rexpr * rexpr
    | RMem of Memory.quantity * rexpr
    | RCond of rexpr * rexpr * rexpr
    | RCall of rexpr * rexpr list * signature

  (* [convert_accu] stores the function calls of expressions with function
     calls being converted to expressions without function calls *)
  let convert_accu = ref []

  (* [convert_rexpr rexpr] converts the expression with function calls [rexpr]
     into an expression without function calls. The function calls in [rexpr]
     are stored in [convert_accu] *)
  let rec convert_rexpr = function
    | RId id -> Id id
    | RCst c -> Cst c
    | ROp1 (op, e1) -> Op1 (op, convert_rexpr e1)
    | ROp2 (op, e1, e2) -> Op2 (op, convert_rexpr e1, convert_rexpr e2)
    | RMem (chunk, e1) -> Mem (chunk, convert_rexpr e1)
    | RCond (e1, e2, e3) ->
	Cond (convert_rexpr e1, convert_rexpr e2, convert_rexpr e3)
    | RCall(e1, el, sg) ->
	let c1 = convert_rexpr e1 in
	let cl = convert_rexpr_list el in
	let t = mktemp() in
	  convert_accu := St_call (Some t, c1, cl, sg) :: !convert_accu;
	  Id t

  and convert_rexpr_list el = List.map convert_rexpr el

  (* [prepend_seq stmts last] reverses and sequences the list of statements
     [stmts] and puts [last] at the end *)
  let rec prepend_seq stmts last =
    match stmts with
      | [] -> last
      | s1 :: sl -> prepend_seq sl (St_seq (s1, last))

  (* [mkeval e] creates the AST statement associated to the Cminor instruction
       e;
     where [e] is an expression with possible function calls *)
  let mkeval e =
    convert_accu := [];
    match e with
      | RCall (e1, el, sg) ->
	  let c1 = convert_rexpr e1 in
	  let cl = convert_rexpr_list el in
	    prepend_seq !convert_accu (St_call (None, c1, cl, sg))
      | _ ->
	  ignore (convert_rexpr e);
	  prepend_seq !convert_accu St_skip

  (* [mkeval id e] creates the AST statement associated to the Cminor
     instruction
       id = e;
     where [e] is an expression with possible function calls *)
  let mkassign id e =
    convert_accu := [];
    match e with
      | RCall (e1, el, sg) ->
	  let c1 = convert_rexpr e1 in
	  let cl = convert_rexpr_list el in
	    prepend_seq !convert_accu (St_call (Some id, c1, cl, sg))
      | _ ->
	  let c = convert_rexpr e in
	    prepend_seq !convert_accu (St_assign (id, c))

  (* [mkstore size e1 e2] creates the AST statement associated to the Cminor
     instruction
       size[e1] = e2;
     where [e1] and [e2] are expressions with possible function calls *)
  let mkstore size e1 e2 =
    convert_accu := [];
    let c1 = convert_rexpr e1 in
    let c2 = convert_rexpr e2 in
      prepend_seq !convert_accu (St_store (size, c1, c2))

  (* [mkifthenelse e s1 s2] creates the AST statement associated to the Cminor
     instruction
       if (e) { s1 } else { s2 }
     where [e] is an expression with possible function calls *)
  let mkifthenelse e s1 s2 =
    convert_accu := [];
    let c = convert_rexpr e in
      prepend_seq !convert_accu (St_ifthenelse (c, s1, s2))

  (* [mkreturn_some e] creates the AST statement associated to the Cminor
     instruction
       return e;
     where [e] is an expression with possible function calls *)
  let mkreturn_some e =
    convert_accu := [];
    let c = convert_rexpr e in
      prepend_seq !convert_accu (St_return (Some c))

  (* [mkswitch e (cases, dfl)] creates the AST statement associated to the
     Cminor instruction
       switch (e) {
         case i: exit j_i;
         ...
         default: exit j_default; }
     where [e] is an expression with possible function calls *)
  let mkswitch e (cases, dfl) =
    convert_accu := [];
    let c = convert_rexpr e in
      prepend_seq !convert_accu (St_switch (c, cases, dfl))

  (* The Cminor instruction
      match (e) {
        case 0: s0;
        case 1: s1;
        case 2: s2; }
     is syntaxic sugar for the Cminor instruction
       block {
         block {
           block {
             block {
               switch (e) {
                 case 0: exit 0;
                 case 1: exit 1;
                 default: exit 2; }
             } s0; exit 2;
           } s1; exit 1;
         } s2;
       }
     Note that matches are assumed to be exhaustive *)

  let mkmatch_aux e cases =
    let ncases = List.length cases in
    let rec mktable n = function
      | [] -> assert false
      | [key, action] -> []
      | (key, action) :: rem -> (key, n) :: mktable (n+1) rem in
    let sw =
      St_switch (e, mktable 0 cases, pred ncases) in
    let rec mkblocks body n = function
      | [] -> assert false
      | [key, action] -> St_block (St_seq (body, action))
      | (key, action) :: rem ->
          mkblocks
            (St_block (St_seq (body, St_seq (action, St_exit n))))
            (pred n) rem in
      mkblocks (St_block sw) (pred ncases) cases

  (* [mkmatch e cases] creates the AST statement associated to the Cminor
     instruction
       match (e) {
         case i: s_i;
         ... }
     where [e] is an expression with possible function calls *)
  let mkmatch e cases =
    convert_accu := [];
    let c = convert_rexpr e in
    let s =
      match cases with
	| [] -> St_skip (* ??? *)
	| [key, action] -> action
	| _ -> mkmatch_aux c cases in
      prepend_seq !convert_accu s

  (* [mktailcall f [e1;e2;...] sig] creates the AST statement associated to the
     Cminor instruction
     tailcall f(e1,e2,...): sig
     where [e], [e1], [e2], ... are expressions with possible function calls *)
  let mktailcall e1 el sg =
    convert_accu := [];
    let c1 = convert_rexpr e1 in
    let cl = convert_rexpr_list el in
      prepend_seq !convert_accu (St_tailcall (c1, cl, sg))

  (* Parse error handler *)
  let raise_error (_, pos) s = Error.error "parse error" pos (s ^ "\n")

%}

%token ABSF
%token AMPERSAND
%token AMPERSANDAMPERSAND
%token BANG
%token BANGEQUAL
%token BANGEQUALF
%token BANGEQUALU
%token BAR
%token BARBAR
%token CARET
%token CASE
%token COLON
%token COMMA
%token DEFAULT
%token ELSE
%token EQUAL
%token EQUALEQUAL
%token EQUALEQUALF
%token EQUALEQUALU
%token EOF
%token EXIT
%token EXTERN
%token FLOAT
%token FLOAT32
%token FLOAT64
%token <float> FLOATLIT
%token FLOATOFINT
%token FLOATOFINTU
%token GREATER
%token GREATERF
%token GREATERU
%token GREATEREQUAL
%token GREATEREQUALF
%token GREATEREQUALU
%token GREATERGREATER
%token GREATERGREATERU
%token <string> IDENT
%token IF
%token INT
%token INT8
%token INT16
%token INT32
%token INT8STO8
%token INT8STO16
%token INT8STO32
%token INT8UTO8
%token INT8UTO16
%token INT8UTO32
%token INT16STO8
%token INT16STO16
%token INT16STO32
%token INT16UTO8
%token INT16UTO16
%token INT16UTO32
%token INT32STO8
%token INT32STO16
%token INT32STO32
%token INT32UTO8
%token INT32UTO16
%token INT32UTO32
%token <int> INTLIT
%token INTOFFLOAT
%token INTUOFFLOAT
%token LBRACE
/* %token LBRACELBRACE */
%token LBRACKET
%token LESS
%token LESSU
%token LESSF
%token LESSEQUAL
%token LESSEQUALU
%token LESSEQUALF
%token LESSLESS
%token LOOP
%token LPAREN
%token MATCH
%token MINUS
%token MINUSF
%token MINUSGREATER
%token PERCENT
%token PERCENTU
%token PLUS
%token PLUSF
%token QUESTION
%token RBRACE
/* %token RBRACERBRACE */
%token RBRACKET
%token RETURN
%token RPAREN
%token SEMICOLON
%token SLASH
%token SLASHF
%token SLASHU
%token STACK
%token STAR
%token STARF
%token <string> STRINGLIT
%token SWITCH
%token TILDE
%token TAILCALL
%token VAR
%token VOID
%token GOTO BLOCK
%token PTR

/* Unused */
/* %token ALLOC DOLLAR IN LET p_let */

/* Precedences from low to high */

/* %left COMMA */
/* %left p_let */
/* %right EQUAL */
%right QUESTION COLON
%left BARBAR
%left AMPERSANDAMPERSAND
%left BAR
%left CARET
%left AMPERSAND
%left EQUALEQUAL BANGEQUAL LESS LESSEQUAL GREATER GREATEREQUAL EQUALEQUALU BANGEQUALU LESSU LESSEQUALU GREATERU GREATEREQUALU EQUALEQUALF BANGEQUALF LESSF LESSEQUALF GREATERF GREATEREQUALF 
%left LESSLESS GREATERGREATER GREATERGREATERU
%left PLUS PLUSF MINUS MINUSF
%left STAR SLASH PERCENT STARF SLASHF SLASHU PERCENTU
%nonassoc BANG TILDE p_uminus ABSF INTOFFLOAT INTUOFFLOAT FLOATOFINT FLOATOFINTU FLOAT32 /* ALLOC */
%left LPAREN

/* Entry point */

%start program
%type <Cminor.program> program

%%

%inline position(X): x = X { (x, Position.lex_join $startpos $endpos) }

/* Programs */

program:
    global_declarations proc_list EOF { { vars   = List.rev $1 ;
				          functs = List.rev $2 ;
				          main   = Some "main" } }
;

global_declarations:
    /* empty */                            { [] }
  | global_declarations global_declaration { $2 :: $1 }
;

global_declaration:
    VAR STRINGLIT init_datas { ($2, List.rev $3) }
  | pos = position(error)    { raise_error pos
				 "Global declaration syntax error" }
;

init_datas:
    /* empty */                  { [] }
  | init_data                    { [$1] }
  | LBRACE init_data_list RBRACE { $2 }
;

init_data:
    INTLIT                         { AST.Data_int32 $1 }
  | FLOATLIT                       { AST.Data_float32 $1 }
  | LPAREN INT8 RPAREN INTLIT      { AST.Data_int8 $4 }
  | LPAREN INT16 RPAREN INTLIT     { AST.Data_int16 $4 }
  | LPAREN INT32 RPAREN INTLIT     { AST.Data_int32 $4 }
  | LPAREN FLOAT32 RPAREN FLOATLIT { AST.Data_float32 $4 }
  | LPAREN FLOAT64 RPAREN FLOATLIT { AST.Data_float64 $4 }
  | LBRACKET INTLIT RBRACKET       { AST.Data_reserve $2 }
;

quantity:
    INTLIT { Memory.QInt $1 }
  | PTR    { Memory.QPtr }

init_data_list:
    init_data                      { [$1] }
  | init_data COMMA init_data_list { $1 :: $3 }
;

proc_list:
    /* empty */    { [] }
  | proc_list proc { $2 :: $1 }
;

/* Procedures */

proc:
    STRINGLIT LPAREN parameters RPAREN COLON signature
    LBRACE
      stack_declaration
      var_declarations
      stmt_list
    RBRACE
  { let tmp = !temporaries in
      temporaries := [];
      temp_counter := 0;
      ($1, F_int { f_sig = $6 ;
		   f_params = List.rev $3 ;
		   f_vars = List.rev (tmp @ $9) ;
		   f_ptrs = [] (* TODO *) ;
		   f_stacksize = $8 ;
		   f_body = $10 }) }
  | EXTERN STRINGLIT COLON signature { ($2, F_ext { ef_tag = $2 ;
						    ef_sig = $4 }) }
  | pos = position(error) { raise_error pos
			    "Procedure or function declaration syntax error" }
;

parameters:
    /* empty */    { [] }
  | parameter_list { $1 }
;

parameter_list:
    IDENT                      { $1 :: [] }
  | parameter_list COMMA IDENT { $3 :: $1 }
  | pos = position(error) { raise_error pos
			    "Parameter declaration syntax error" }
;

signature:
    type_  { { args = [] ; res = Type_ret $1 } }
  | VOID
      { { args = [] ; res = Type_void } }
  | type_ MINUSGREATER signature
	  { let s = $3 in {s with args = $1 :: s.args } }
  | pos = position(error) { raise_error pos "Signature syntax error" }
;

stack_declaration:
    /* empty */            { 0 }
  | STACK INTLIT SEMICOLON { $2 }
  | pos = position(error)  { raise_error pos "Stack declaration syntax error" }
;

var_declarations:
    /* empty */                      { [] }
  | var_declarations var_declaration { $2 @ $1 }
  | pos = position(error)            { raise_error pos
					 "Variable declaration syntax error" }
;

var_declaration:
    VAR parameter_list SEMICOLON { $2 }
;

/* Statements */

stmt:
    expr SEMICOLON                         { mkeval $1 }
  | IDENT EQUAL expr SEMICOLON             { mkassign $1 $3 }
  | quantity LBRACKET expr RBRACKET EQUAL expr SEMICOLON
      { mkstore $1 $3 $6 }
  | IF LPAREN expr RPAREN stmts ELSE stmts { mkifthenelse $3 $5 $7 }
  | IF LPAREN expr RPAREN stmts            { mkifthenelse $3 $5 St_skip }
  | LOOP stmts                             { St_loop $2 }
  | BLOCK LBRACE stmt_list RBRACE          { St_block $3 }
  | EXIT SEMICOLON                         { St_exit 0 }
  | EXIT INTLIT SEMICOLON                  { St_exit $2 }
  | RETURN SEMICOLON                       { St_return None }
  | RETURN expr SEMICOLON                  { mkreturn_some $2 }
  | GOTO IDENT SEMICOLON                   { St_goto $2 }
  | IDENT COLON stmt                       { St_label ($1, $3) }
  | SWITCH LPAREN expr RPAREN LBRACE switch_cases RBRACE
        { mkswitch $3 $6 }
  | MATCH LPAREN expr RPAREN LBRACE match_cases RBRACE
	  { mkmatch $3 $6 }
  | TAILCALL expr LPAREN expr_list RPAREN COLON signature SEMICOLON
	      { mktailcall $2 $4 $7 }
;

stmt_list:
    /* empty */    { St_skip }
  | stmt stmt_list { St_seq ($1, $2) }
  | pos = position(error) { raise_error pos "Statement syntax error" }
;

stmts:
    LBRACE stmt_list RBRACE { $2 }
  | stmt                    { $1 }
;

switch_cases:
    DEFAULT COLON EXIT INTLIT SEMICOLON
    { ([], $4) }
  | CASE INTLIT COLON EXIT INTLIT SEMICOLON switch_cases
        { let (cases, dfl) = $7 in (($2, $5) :: cases, dfl) }
  | pos = position(error) { raise_error pos "Syntax error in switch construct" }
;

match_cases:
    /* empty */                             { [] }
  | CASE INTLIT COLON stmt_list match_cases { ($2, $4) :: $5 }
  | pos = position(error) { raise_error pos "Syntax error in match construct" }
;

/* Expressions */

expr:
    LPAREN expr RPAREN                      { $2 }
  | IDENT                               { RId $1 }
  | INTLIT                              { RCst (Cst_int $1) }
  | FLOATLIT                            { RCst (Cst_float $1) }
  | STRINGLIT                           { RCst (Cst_addrsymbol $1) }
  | AMPERSAND INTLIT                    { RCst (Cst_stackoffset $2) }
  | MINUS expr    %prec p_uminus        { ROp1 (Op_negint int32, $2) }
  | MINUSF expr   %prec p_uminus        { error_float () }
  | ABSF expr                           { error_float () }
  | INTOFFLOAT expr                     { error_float () }
  | INTUOFFLOAT expr                    { error_float () }
  | FLOATOFINT expr                     { error_float () }
  | FLOATOFINTU expr                    { error_float () }
  | TILDE expr                          { ROp1 (Op_notint int32, $2) }
  | BANG expr                           { ROp1 (Op_notbool, $2) }
  | INT8STO8 expr                       { ROp1 (Op_cast ((8, Signed), 8), $2) }
  | INT8STO16 expr                      { ROp1 (Op_cast ((8, Signed), 16), $2) }
  | INT8STO32 expr                      { ROp1 (Op_cast ((8, Signed), 32), $2) }
  | INT8UTO8 expr                      { ROp1 (Op_cast ((8, Unsigned), 8), $2) }
  | INT8UTO16 expr                    { ROp1 (Op_cast ((8, Unsigned), 16), $2) }
  | INT8UTO32 expr                    { ROp1 (Op_cast ((8, Unsigned), 32), $2) }
  | INT16STO8 expr                     { ROp1 (Op_cast ((16, Signed), 16), $2) }
  | INT16STO16 expr                    { ROp1 (Op_cast ((16, Signed), 16), $2) }
  | INT16STO32 expr                    { ROp1 (Op_cast ((16, Signed), 32), $2) }
  | INT16UTO8 expr                    { ROp1 (Op_cast ((16, Unsigned), 8), $2) }
  | INT16UTO16 expr                  { ROp1 (Op_cast ((16, Unsigned), 16), $2) }
  | INT16UTO32 expr                  { ROp1 (Op_cast ((16, Unsigned), 32), $2) }
  | INT32STO8 expr                      { ROp1 (Op_cast ((32, Signed), 8), $2) }
  | INT32STO16 expr                    { ROp1 (Op_cast ((32, Signed), 16), $2) }
  | INT32STO32 expr                    { ROp1 (Op_cast ((32, Signed), 32), $2) }
  | INT32UTO8 expr                    { ROp1 (Op_cast ((32, Unsigned), 8), $2) }
  | INT32UTO16 expr                  { ROp1 (Op_cast ((32, Unsigned), 16), $2) }
  | INT32UTO32 expr                  { ROp1 (Op_cast ((32, Unsigned), 32), $2) }
  | FLOAT32 expr                        { error_float () }
  | expr PLUS expr                      { ROp2 (Op_add int32, $1, $3) }
  | expr MINUS expr                     { ROp2 (Op_sub int32, $1, $3) }
  | expr STAR expr                      { ROp2 (Op_mul int32, $1, $3) }
  | expr SLASH expr                     { ROp2 (Op_div int32, $1, $3) }
  | expr PERCENT expr                   { ROp2 (Op_mod int32, $1, $3) }
  | expr SLASHU expr                    { ROp2 (Op_div uint32, $1, $3) }
  | expr PERCENTU expr                  { ROp2 (Op_mod uint32, $1, $3) }
  | expr AMPERSAND expr                 { ROp2 (Op_and, $1, $3) }
  | expr BAR expr                       { ROp2 (Op_or, $1, $3) }
  | expr CARET expr                     { ROp2 (Op_xor, $1, $3) }
  | expr LESSLESS expr                  { ROp2 (Op_shl int32, $1, $3) }
  | expr GREATERGREATER expr            { ROp2 (Op_shr int32, $1, $3) }
  | expr GREATERGREATERU expr           { ROp2 (Op_shr uint32, $1, $3) }
  | expr PLUSF expr                     { error_float () }
  | expr MINUSF expr                    { error_float () }
  | expr STARF expr                     { error_float () }
  | expr SLASHF expr                    { error_float () }
  | expr EQUALEQUAL expr               { ROp2 (Op_cmp (Cmp_eq, int32), $1, $3) }
  | expr BANGEQUAL expr                { ROp2 (Op_cmp (Cmp_ne, int32), $1, $3) }
  | expr LESS expr                     { ROp2 (Op_cmp (Cmp_lt, int32), $1, $3) }
  | expr LESSEQUAL expr                { ROp2 (Op_cmp (Cmp_le, int32), $1, $3) }
  | expr GREATER expr                  { ROp2 (Op_cmp (Cmp_gt, int32), $1, $3) }
  | expr GREATEREQUAL expr             { ROp2 (Op_cmp (Cmp_ge, int32), $1, $3) }
  | expr EQUALEQUALU expr             { ROp2 (Op_cmp (Cmp_eq, uint32), $1, $3) }
  | expr BANGEQUALU expr              { ROp2 (Op_cmp (Cmp_ne, uint32), $1, $3) }
  | expr LESSU expr                   { ROp2 (Op_cmp (Cmp_lt, uint32), $1, $3) }
  | expr LESSEQUALU expr              { ROp2 (Op_cmp (Cmp_le, uint32), $1, $3) }
  | expr GREATERU expr                { ROp2 (Op_cmp (Cmp_gt, uint32), $1, $3) }
  | expr GREATEREQUALU expr           { ROp2 (Op_cmp (Cmp_ge, uint32), $1, $3) }
  | expr EQUALEQUALF expr               { error_float () }
  | expr BANGEQUALF expr                { error_float () }
  | expr LESSF expr                     { error_float () }
  | expr LESSEQUALF expr                { error_float () }
  | expr GREATERF expr                  { error_float () }
  | expr GREATEREQUALF expr             { error_float () }
  | quantity LBRACKET expr RBRACKET       { RMem ($1, $3) }
  | expr AMPERSANDAMPERSAND expr        { RCond ($1, $3, RCst (Cst_int 0)) }
  | expr BARBAR expr                    { RCond ($1, RCst (Cst_int 1), $3) }
  | expr QUESTION expr COLON expr       { RCond ($1, $3, $5) }
  | expr LPAREN expr_list RPAREN COLON signature
      { RCall ($1, $3, $6) }
  | pos = position(error) { raise_error pos "Expression syntax error" }
;

expr_list:
    /* empty */ { [] }
  | expr_list_1 { $1 }
;

expr_list_1:
    expr /* %prec COMMA */ { $1 :: [] }
  | expr COMMA expr_list_1 { $1 :: $3 }
;

type_:
    INT   { Sig_int }
  | FLOAT { Sig_float }
;


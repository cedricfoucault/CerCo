
(** This module defines the abstract syntax tree of [Cminor]. *)

(* This file describes the abstract syntax of the Cminor language.
   Only types are: int8, int16, int32 and void. *)

type etype = AST.sig_type

type expression = Expr of expr_descr * etype
and expr_descr =
  | Id of AST.ident
  | Cst of AST.cst
  | Op1 of AST.op1 * expression
  | Op2 of AST.op2 * expression * expression
  | Mem of AST.quantity * expression          (* Memory read *)
  | Cond of expression * expression * expression (* Ternary expression *)
  | Exp_cost of CostLabel.t * expression         (* Labelled expression *)

type statement =
  | St_skip
  | St_assign of AST.ident * expression
  | St_store of AST.quantity * expression * expression

  (* Function call. Parameters are an optional variable to store the
     result of the function, the name of the function, the arguments,
     and finally its signature. *)
  | St_call of AST.ident option * expression * expression list * AST.signature

  (* Tail call to a function, that is, a call to a function following
     by a return statement. Parameters are the name of the function,
     the arguments and its signature. *)
  | St_tailcall of expression * expression list * AST.signature

  | St_seq of statement * statement
  | St_ifthenelse of expression * statement * statement
  | St_loop of statement
  | St_block of statement
  | St_exit of int

  (* Switch. Parameters are the expression whose value is switch, a
     table of cases and their corresponding number of blocks to exit,
     and the number of block to exit in the default case. *)
  | St_switch of expression * (int*int) list * int

  | St_return of expression option
  | St_label of AST.ident * statement
  | St_goto of string
  | St_cost of CostLabel.t * statement


type internal_function =
    { f_return    : AST.type_return ;
      f_params    : (AST.ident * etype) list ;
      f_vars      : (AST.ident * etype) list ;
      f_stacksize : AST.abstract_size ;
      f_body      : statement }

type function_def =
  | F_int of internal_function
  | F_ext of AST.external_function

(* A program is a list of global variables and their initialization
   datas, a list of function names and their definition, and the name
   of the main function. *)

type program =
    { vars   : (AST.ident * AST.abstract_size * AST.data list option) list ;
      functs : (AST.ident * function_def) list ;
      main   : AST.ident option }

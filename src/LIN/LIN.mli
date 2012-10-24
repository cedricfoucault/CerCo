
(** This module defines the abstract syntax tree of [LIN]. *)

(** Compared to LTL where functions were graphs, the functions of a LIN program
    are sequential instructions. *)

type register = Driver.TargetArch.register

type address = Driver.TargetArch.register list

type statement =

  (* Unconditional branch. *)
  | St_skip of Label.t

  (* Label a statement. *)
  | St_label of Label.t

  (* Comment. *)
  | St_comment of string

  (* Emit a cost label. *)
  | St_cost of CostLabel.t

  (* Assign an integer constant to a register. Parameters are the destination
     register, and the integer. *)
  | St_int of register * int

  (* Assign the address of a symbol to DPTR. Parameter is the symbol. *)
  | St_addr of address * AST.ident

  (* Apply an unary operation. Parameters are the operation, the destination
     register, and the source register. *)
  | St_unop of Arch.unop * register * register

  (* Apply a binary operation. Parameters are the operation, the destination
     register, and source registers. *)
  | St_binop of Arch.binop * register * register * register

  (* Memory load. Parameters are the size of what to load, the destination
     register, and the address registers. *)
  | St_load of int * register * address

  (* Mmeory store. Parameters are the size of what to store, the address
     registers, and the source register. *)
  | St_store of int * address * register

  (* Call to a function given its address. Parameters are the address
     registers. *)
  | St_call of address

  (* Tail call to a function given its address. Parameters are the address
     registers. *)
  | St_tailcall of address

  (* Branch. Parameter are the source register, and the label to go to when the
     register is not 0. *)
  | St_cond of register * Label.t

  (* Transfer control to the address stored in the return address registers. *)
  | St_return

type internal_function = statement list

type function_def =
  | F_int of internal_function
  | F_ext of AST.external_function

(* A program is a list of global variables and their reserved space, a list of
   function names and their definition, and the name of the main function. *)

type program =
    { globals : int (* size in bytes *) ;
      functs  : (AST.ident * function_def) list ;
      main    : AST.ident option }

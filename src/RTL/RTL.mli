
(** This module defines the abstract syntax tree of [RTL]. *)

(* The main differences between RTLabs and RTL is instruction selection.

   Also, for genericity with architecture where addresses are several word long,
   the instructions involving addresses use list of registers to represent
   them. *)

type registers = Register.t list

type address = Register.t list

type statement =

  (* The empty statement. *)
  | St_skip of Label.t

  (* Emit a cost label. *)
  | St_cost of CostLabel.t * Label.t

  (* Assign an integer constant to a register. Parameters are the destination
     register, the integer and the label of the next statement. *)
  | St_int of Register.t * int * Label.t

  (* Move the content of a register to another. Parameters are the destination
     register, the source register, and the label of the next statement. *)
  | St_move of Register.t * Register.t * Label.t

  (* Apply an unary operation. Parameters are the operation, the destination
     register, the source register, and the label of the next statement. *)
  | St_unop of Arch.unop * Register.t * Register.t * Label.t

  (* Apply a binary operation. Parameters are the operation, the destination
     register, the source registers, and the label of the next statement. *)
  | St_binop of Arch.binop * Register.t * Register.t * Register.t * Label.t

  (* Assign the address of a function symbol to registers. Parameters are the
     destination registers (low bytes first), the symbol and the label of the
     next statement. *)
  | St_funaddr of address * AST.ident * Label.t

  (* Assign the stack pointer to registers. Parameters are the destination
     registers (low bytes first), and the label of the next statement. *)
  | St_stackaddr of address * Label.t

  (* Assign the base address of the globals to registers. Parameters are the
     destination registers (low bytes first), and the label of the next
     statement. *)
  | St_globaladdr of address * Label.t

  (* Load from memory. Parameters are the size of what to load, the destination
     register, the address registers (low bytes first), and the label of the
     next statement. *)
  | St_load of int * Register.t * address * Label.t

  (* Store to memory. Parameters are the size of what to store, the address
     registers (low bytes first), the source register, and the label of the next
     statement. *)
  | St_store of int * address * Register.t * Label.t

  (* Branch. Parameters are a register, the label to go to when the register is
     not 0, and the label to go to when the register is 0. *)
  | St_cond of Register.t * Label.t * Label.t

  (*
  (* Unary condition branch. Parameters are the operation, the register that the
    condition is applied to, the label to go to when the result is not 0, and
    the label to go to when the result is 0. *)
  | St_uncon of Arch.uncon * Register.t * Label.t * Label.t

  (* Binary condition branch. Parameters are the operation, the registers that
     the condition is applied to, the label to go to when the result is not 0,
     and the label to go to when the result is 0. *)
  | St_bincon of Arch.bincon * Register.t * Register.t * Label.t * Label.t
*)

  (* Return the value of some registers (low bytes first). *)
  | St_return of registers

  (* Call to a function given its address. Parameters are the registers holding
     the address of the function (low bytes first), the arguments of the
     function, the destination registers, and the label of the next
     statement. *)
  | St_call of address * Register.t list * registers * Label.t

  (* Tail call to a function given its address. Parameters are the registers
     holding the address of the function (low bytes first), and the arguments of
     the function. *)
  | St_tailcall of address * Register.t list


type graph = statement Label.Map.t

type internal_function =
    { f_luniverse : Label.Gen.universe ;
      f_runiverse : Register.universe ;
      f_result    : Register.t list (* low bytes first *) ;
      f_params    : Register.t list ;
      f_locals    : Register.Set.t ;
      f_stacksize : int ;
      f_graph     : graph ;
      f_entry     : Label.t ;
      f_exit      : Label.t }

type function_def =
  | F_int of internal_function
  | F_ext of AST.external_function

(* A program is a list of global variables and their reserved space, a list of
   function names and their definition, and the name of the main function. *)

type program =
    { globals : int (* reserved space in bytes *) ;
      functs  : (AST.ident * function_def) list ;
      main    : AST.ident option }

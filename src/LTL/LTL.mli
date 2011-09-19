
(** This module defines the abstract syntax tree of [LTL]. *)

(** The main difference with ERTL is that only physical registers are present in
    LTL (no more pseudo-registers). Pseudo-registers are associated either a
    physical register or a location on the stack. This is done by a coloring
    algorithm. Actually, this coloring algorithm relies on the result of a
    liveness analysis that will also allow to remove dead code. *)

type register = Driver.TargetArch.register

type address = register list

type statement =

  (* The empty statement. *)
  | St_skip of Label.t

  (* Comment. *)
  | St_comment of string * Label.t

  (* Emit a cost label. *)
  | St_cost of CostLabel.t * Label.t

  (* Assign an integer constant to a register. Parameters are the destination
     register, the integer and the label of the next statement. *)
  | St_int of register * int * Label.t

  (* Assign the address of a symbol to registers. Parameters are the registers,
     the symbol, and the label of the next statement. *)
  | St_addr of address * AST.ident * Label.t

  (* Read a register, apply a unary operator, write to a register.  Parameters
     are operator, destination register, source register, and the label of the
     next statement. *)
  | St_unop of Arch.unop * register * register * Label.t

  (* Read two registers, apply a binary operator, write to a register.
     Parameters are operator, destination register, source registers, and the
     label of the next statement. *)
  | St_binop of Arch.binop * register * register * register * Label.t

  (* Memory read. Parameters are the size of what to load, the destination
     register, the source (address) register, and the label of the next
     statement. *)
  | St_load of int * register * address * Label.t

  (* Memory write. Parameters are the size of what to store, the source
     (address) register, the destination register, and the label of the next
     statement. *)
  | St_store of int * address * register * Label.t

  (* Call to a function given its address. Parameters are the registers holding
     the address of the function, and the label of the next statement. *)
  | St_call of address * Label.t

  (* Tail call to a function given its address. Parameter is the registers
     holding the address of the function. *)
  | St_tailcall of address

  (* Branch. Parameters are the register holding the value for the branching,
     the label to go to when the value is not 0, and the label to go to when the
     value is 0. *)
  | St_cond of register * Label.t * Label.t

  (* Transfer control to the address stored in the return address registers. *)
  | St_return

type graph = statement Label.Map.t

type internal_function =
    { f_luniverse : Label.Gen.universe ;
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

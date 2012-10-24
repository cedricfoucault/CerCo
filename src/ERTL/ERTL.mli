
(** This module defines the abstract syntax tree of [RTL]. *)

(* Adapted from Pottiers's Pseudo-Pascal pedagogical compiler *)

(* This is the definition of the abstract syntax for the Explicit
   Register Transfer Language (ERTL).

   This language is very much like RTL, except the calling convention
   has been made explicit. That is, functions and procedures no longer
   accept parameters and return results via a high-level mechanism;
   instead, they do so via either hardware registers or stack
   slots.

   Functions and procedures no longer magically return to their caller: instead,
   a new [St_return] instruction appears, whose semantics is to transfer control
   to the address stored in the return address registers.

   Functions and procedures are no longer explicitly distinguished: functions
   are simply procedures that happen to write the hardware register. There only
   remains a distinction at [St_return] instructions (see below).

   Two new instructions, [St_newframe] and [St_delframe], appear in order to
   allocate and release stack frames. They will be translated, in the final
   assembly code, to arithmetic on the stack pointer registers. *)

(* Stack organization. 

   The stack is going from top to bottom. Below is a schema of the stack
   organization viewed by the function being executed. 

                    formal parameters (the first parameter is at the top)
                    spilled variables (the first spilled variable is a the top)
                    local variables (the first local variable is at the bottom)
   stack pointer ->
*)

type registers = Register.t list

type address = Register.t list

type statement =

  (* The empty statement. *)
  | St_skip of Label.t

  (* Comment. *)
  | St_comment of string * Label.t

  (* Emit a cost label. *)
  | St_cost of CostLabel.t * Label.t

  (* Allocate required space on the stack for the function. Parameter is the
     label of the next statement. *)
  | St_newframe of Label.t

  (* Deallocate required space on the stack for the function. Parameter is the
     label of the next statement. *)
  | St_delframe of Label.t

  (* Assign the frame size to a register. Parameters are the destination
     register, and the label of the next statement. *)
  | St_framesize of Register.t * Label.t

  (* Assign the content of a hardware register to a pseudo register. Parameters
     are the destination pseudo register, the source hardware register, and the
     label of the next statement. *)
  | St_get_hdw of Register.t * Driver.TargetArch.register * Label.t

  (* Assign the content of a pseudo register to a hardware register. Parameters
     are the destination hardware register, the source pseudo register, and the
     label of the next statement. *)
  | St_set_hdw of Driver.TargetArch.register * Register.t * Label.t

  (* Assign the content of a hardware register to a hardware
     register. Parameters are the destination register, the source register, and
     the label of the next statement. Only used to save the return value before
     the epilogue and restore it right before leaving the function. *)
  | St_hdw_to_hdw of Driver.TargetArch.register * Driver.TargetArch.register *
                     Label.t

  (* Move the content of a register to another. Parameters are the destination
     register, the source register, and the label of the next statement. *)
  | St_move of Register.t * Register.t * Label.t

  (* Assign an integer constant to a register. Parameters are the destination
     register, the integer and the label of the next statement. *)
  | St_int of Register.t * int * Label.t

  (* Apply an unary operation. Parameters are the operation, the destination
     register, the source register, and the label of the next statement. *)
  | St_unop of Arch.unop * Register.t * Register.t * Label.t

  (* Apply a binary operation. Parameters are the operation, the destination
     register, the source registers, and the label of the next statement. *)
  | St_binop of Arch.binop * Register.t * Register.t * Register.t * Label.t

  (* Assign the nth byte of the address (the least significant byte starts at 0)
     of a symbol to a register. Parameters are the destination register, the
     symbol, the byte number, and the label of the next statement. *)
  | St_addrN of Register.t * AST.ident * int * Label.t

  (* Load from external memory. Parameters are the size of what to load, the
     destination register, the address registers (low bytes first), and the
     label of the next statement. *)
  | St_load of int * Register.t * address * Label.t

  (* Store to external memory. Parameters are the size of what to store, the
     address registers (low bytes first), the source register, and the label of
     the next statement. *)
  | St_store of int * address * Register.t * Label.t

  (* Call to a function given its address. Parameters are the registers holding
     the address of the function, the number of arguments of the function, and
     the label of the next statement. *)
  | St_call of address * int * Label.t

  (* Tail call to a function given its address. Parameters are the registers
     holding the address of the function, the number of arguments of the
     function, and the label of the next statement. *)
  | St_tailcall of address * int

  (* Branch. Parameters are the register holding the value for the branching,
     the label to go to when the value is not 0, and the label to go to when the
     value is 0. *)
  | St_cond of Register.t * Label.t * Label.t

  (* Transfer control to the address stored in the return address registers.
     The registers argument does not affect the semantics of the instruction,
     but tells whether the caller is expected to read the value of the hardware
     return registers. This is exploited in the liveness analysis. *)
  | St_return of registers

type graph = statement Label.Map.t

type internal_function =
    { f_luniverse : Label.Gen.universe ;
      f_runiverse : Register.universe ;
      f_params    : int ;
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

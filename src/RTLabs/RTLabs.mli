
(** This module defines the abstract syntax tree of [RTLabs]. *)

(* A program in RTLabs associates to each function of the program a
   Control Flow Graph. Pseudo-registers are used to represent the
   variables. The operations and instructions of the language are
   those of Cminor.

   RTLabs is the last language of the frontend. It is intended to
   ease retargetting. *)


(* A function in RTLabs is a mapping from labels to
   statements. Statements explicitely mention their successors. *)

type statement =

  (* The empty statement. *)
  | St_skip of Label.t

  (* Emit a cost label. *)
  | St_cost of CostLabel.t * Label.t

  (* Assign a constant to registers. Parameters are the destination register,
     the constant and the label of the next statement. *)
  | St_cst of Register.t * AST.cst * Label.t

  (* Application of an unary operation. Parameters are the operation, the
     destination register, the argument register and the label of the next
     statement. *)
  | St_op1 of AST.op1 * Register.t * Register.t * Label.t

  (* Application of a binary operation. Parameters are the operation, the
     destination register, the two argument registers and the label of the
     next statement. *)
  | St_op2 of AST.op2 * Register.t * Register.t * Register.t * Label.t

  (* Add integer statement. Parameters are the integer constant to add,
     the source register, the destination register and the label of the
     next statement*)
  | St_addi of AST.cst * Register.t * Register.t * Label.t

  (* Memory load. Parameters are the size in bytes of what to load, the
     register containing the address, the destination register and the label
     of the next statement. *)
  | St_load of AST.quantity * Register.t * Register.t * Label.t

  (* Memory load with offset addressing.
     Parameters are the size in bytes of what to load,
     the integer offset to add to the refence address,
     the register containing the reference address,
     the destination register where we store the loaded value,
     and the label of the next statement. *)
  | St_loadi of AST.quantity * AST.cst * Register.t * Register.t * Label.t

  (* Memory store. Parameters are the size in bytes of what to store, the
     register containing the address, the source register and the label of the
     next statement. *)
  | St_store of AST.quantity * Register.t * Register.t * Label.t

  (* Memory store with offset addressing.
     Parameters are the size in bytes of what to store,
     the integer offset to add to the reference address,
     the register containing the reference address,
     the source register from which we take the value to store,
     and the label of the next statement. *)
  | St_storei of AST.quantity * AST.cst * Register.t * Register.t * Label.t

  (* Call to a function given its name. Parameters are the name of the
     function, the arguments of the function, the destination
     register, the signature of the function and the label of the next
     statement. *)
  | St_call_id of AST.ident * Register.t list * Register.t option *
                  AST.signature * Label.t

  (* Call to a function given its address. Parameters are the register
     holding the address of the function, the arguments of the
     function, the destination register, the signature of the function
     and the label of the next statement. This statement with an
     [St_op] before can represent a [St_call_id]. However, we
     differenciate the two to allow translation to a formalism with no
     function pointer. *)
  | St_call_ptr of Register.t * Register.t list * Register.t option *
                   AST.signature * Label.t

  (* Tail call to a function given its name. Parameters are the name of the
     function, the arguments of the function, the signature of the function and
     the label of the next statement. *)
  | St_tailcall_id of AST.ident * Register.t list * AST.signature

  (* Tail call to a function given its address. Parameters are a register
     holding the address of the function, the arguments of the function, the
     signature of the function and the label of the next statement. Same remark
     as for the [St_call_ptr]. *)
  | St_tailcall_ptr of Register.t * Register.t list * AST.signature

  (* Branch. Parameters are the register holding the value to branch on, the
     label to go to when the value evaluates to true (not 0), and the label
     to go to when the value evaluates to false (0). *)
  | St_cond of Register.t * Label.t * Label.t
  
  (* Conditional branch based on comparison to zero.
     Parameters are the comparison operator used (lt, le, gt, ge, eq, neq),
     the register holding the value to compare to zero,
     the label to go to when the comparison is verified,
     and the label to go to when it is not (next instruction). *)
  | St_cond_cmpz of AST.cmp * Register.t * Label.t * Label.t
  
  (* Conditional branch based on comparison to zero.
     Parameters are the comparison operator used (lt, le, gt, ge, eq, neq),
     the register holding the value to compare (left operand),
     the register holding the value to compare (right operand),
     the label to go to when the comparison is verified,
     and the label to go to when it is not (next instruction). *)
  | St_cond_cmp of AST.cmp * Register.t * Register.t * Label.t * Label.t

  (* Jump statement. Parameters are a register and a list of
     labels. The execution will go to the [n]th label of the list of
     labels, where [n] is the natural value held in the register. *)
  | St_jumptable of Register.t * Label.t list

  (* Return statement. *)
  | St_return of Register.t option


type graph = statement Label.Map.t

type internal_function =
    { f_luniverse : Label.Gen.universe ;
      f_runiverse : Register.universe ;
      f_result    : (Register.t * AST.sig_type) option ;
      f_params    : (Register.t * AST.sig_type) list ;
      f_locals    : (Register.t * AST.sig_type) list ;
      f_stacksize : AST.abstract_size ;
      f_graph     : graph ;
      f_entry     : Label.t ;
      f_exit      : Label.t }

type function_def =
  | F_int of internal_function
  | F_ext of AST.external_function

(* A program is a list of global variables and their reserved space, a list of
   function names and their definition, and the name of the main function. *)

type program =
    { vars   : (AST.ident * AST.abstract_size) list ;
      functs : (AST.ident * function_def) list ;
      main   : AST.ident option }

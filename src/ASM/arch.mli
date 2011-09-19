
open PrintPottier

(* Unary (integer arithmetic) operators. *)

type unop =
  | UOpAddi of AST.immediate
  | UOpSlti of AST.immediate (* set on less than immediate *)
  | UOpSltiu of AST.immediate
  | UOpAndi of AST.immediate
  | UOpOri of AST.immediate
  | UOpXori of AST.immediate
  | UOpNeg
  | UOpNot

(* Binary (integer arithmetic or integer comparison) operators. Among
   the comparison operators, only [OpLt] corresponds to a MIPS binary
   comparison instruction, namely [slt]. All others correspond to
   pseudo-instructions. They are exploited because they are
   convenient. *)

type binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpDivu
  | OpModu
  | OpLt                    
  | OpLtu
  | OpLe
  | OpLeu
  | OpGt
  | OpGtu
  | OpGe
  | OpGeu
  | OpEq
  | OpNe
  | OpSllv
  | OpSrav
  | OpSrlv
  | OpAnd
  | OpOr
  | OpXor

(*
(* Unary branch conditions. *)

  type uncon =

(* Greater than or equal to zero. *)

  | UConGez

(* Greater than zero. *)

  | UConGtz

(* Less than or equal to zero. *)

  | UConLez

(* Less than zero. *)

  | UConLtz

(* Binary branch conditions. *)

  and bincon =

(* Equal. *)

  | ConEq

(* Not equal. *)

  | ConNe
*)


val print_unop: 'reg printer -> (unop * 'reg * 'reg) printer
val print_binop: binop -> string
(*
  val print_uncon: 'reg printer -> (uncon * 'reg) printer
  val print_bincon: bincon -> string
*)


module Eval (Val : Value.S) : sig
  val unop: unop -> (Val.t -> Val.t)
  val binop: binop -> (Val.t -> Val.t -> Val.t)

(*
  val uncon: uncon -> (Val.t -> Val.t)
  val bincon: bincon -> (Val.t -> Val.t -> Val.t)
*)
end


(* The size of what can be loaded and stored. *)

type data_size = Word | HalfWord | Byte

val byte_size_of_data_size : data_size -> int
val data_size_of_byte_size : int -> data_size


module type ARCH = sig

  val int_size : int
  val ptr_size : int
  val alignment : int option

  (* Size of the RAM *)

  val ram_size : int

  (* This is the type of hardware registers. *)

  type register

  val eq_reg: register -> register -> bool

  val print_register : register -> string

  type address = register list

  (* Sets of hardware registers. *)

  module RegisterSet : sig
    include Set.S with type elt = register
    val disjoint: t -> t -> bool
    val of_list: elt list -> t
  end

  (* Maps over hardware registers. *)

  module RegisterMap : sig
    include Map.S with type key = register
    (* [lift f s] turns the set [s] into a map where every element [x]
       is mapped to [f x]. *)
    val lift: (key -> 'a) -> RegisterSet.t -> 'a t
  end

  (* A list of the registers used for passing function parameters. *)

  val parameters: register list

  (* The return address registers. It is best thought of as registers
     that are used to pass a parameter (namely, the return address). *)

  val ra: address

  (* The registers used for returning function results (low bytes first). *)

  val result: register list

  (* The stack pointer registers (low bytes first). *)

  val sp: address

  (* The global pointer register. *)

  val gp: address

  (* The zero register always holds the value 0. Although it is a
     special register, it is considered allocatable; see module [Zero]
     for an explanation. *)

  val zero: register

  (* Four non-allocatable registers, reserved for transferring spilled
     pseudo-registers to and from the stack. *)

  val st0: register
  val st1: register
  val st2: register
  val st3: register
  val st4: register
  val st5: register
  val st6: register
  val st_addr0: address
  val st_addr1: address

  (* A set of registers that cannot be allocated. *)

  val forbidden: RegisterSet.t

  (* A set of all allocatable hardware registers, that is, of all
     registers that are available for use by the register allocator --
     as opposed to reserved for some fixed use. *)

  val allocatable: RegisterSet.t

  (* A set of all allocatable ``caller-saved'' hardware registers, that
     is, of all allocatable registers that might be overwritten during a
     function call. This includes the so-called ``caller-saved temporary
     registers'' [$t0-$t9] as well as the registers used to implement
     the calling convention, namely [$a0-$a3], [$v0], and [$ra]. *)

  val caller_saved: RegisterSet.t

  (* A set of all allocatable ``callee-saved'' hardware registers, that
     is, of all allocatable registers that must be preserved by function
     calls. *)

  val callee_saved: RegisterSet.t

  (* A set of all registers that are used in the code that we generate.
     This includes all allocatable registers, plus the four special
     registers mentioned above. *)

  val registers: RegisterSet.t

  (* An association list of replacement for unsupported operations. *)

  val unsupported: Runtime.op_replacement list

end


(* This is the definition of the abstract syntax for a subset of the assembly
   language (ASM). *)

(* Instructions. *)

type 'register generic_instructions = 'register generic_instruction list

and 'register address = 'register list

and 'register generic_instruction =

  (* Issue a comment into the assembler code. This is a
     pseudo-instruction. The Boolean flag tells whether the comment
     should be preceded with a line break. *)

  | IComment of bool * string

  (* No operation. *)

  | INop

  (* Load an immediate value into a register. *)

  | IConst of 'register * AST.immediate

  (* Read a register, apply a unary operator, write to a register.
     Parameters are operator, destination register, and source register. *)

  | IUnOp of unop * 'register * 'register

  (* Read two registers, apply a binary operator, write to a register.
     Parameters are operator, destination register, and source registers. *)

  | IBinOp of binop * 'register * 'register * 'register

  (* Load the address of an instruction into registers. *)

  | ILoadAddr of 'register address * Label.t

  (* Function call. *)

  | ICall of 'register address

  (* Memory read. Parameters are the size of what to load, the destination
     register, and the source (address) registers. *)

  | ILoad of data_size * 'register * 'register address

  (* Memory write. Parameters are the size of what to store, the address
     registers, and the value register. *)

  | IStore of data_size * 'register address * 'register

  (* Unconditional branch. *)

  | IGoto of Label.t

  (* Unconditional indirect branch. *)

  | IGotor of 'register address

  (* Branch. Parameters are a register, and the label that is jumped to if the
     register is different from 0. *)

  | IBranch of 'register * Label.t

  (*
  (* Unary conditional branch. Parameters are a unary condition, the
    register that the condition is applied to, and the label that
    is jumped to if the condition holds. *)

    | IUnBranch of Arch.uncon * register * Label.t

  (* Binary conditional branch. Parameters are a binary condition,
    the registers that the condition is applied to, and the label
    that is jumped to if the condition holds. *)

    | IBinBranch of Arch.bincon * register * register * Label.t
  *)

  (* Transfer control back to the caller. This means jumping to
    the address held in register [ra]. *)

  | IReturn

  (* System call. Only for printing purposes. *)

  | ISyscall

  (* The pseudo-instruction [ILabel l] designates its immediate
     successor as carrying label [l]. *)

  | ILabel of Label.t

  | ICost of CostLabel.t

(* Programs. *)

and 'register generic_program = {

  (* The space required by global variables, in bytes. *)

  globals: int;

  (* External functions of the program. *)

  externals: AST.external_function list;

  (* Whether the program has a main or not. *)

  main: AST.ident option;

  (* A sequence of instructions. *)

  code: 'register generic_instructions

}

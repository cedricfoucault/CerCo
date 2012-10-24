

let error_prefix = "Arch"
let error s = Error.global_error error_prefix s


open Printf
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


(* Some of the instructions that we emit are in fact pseudo-instructions. *)

(* We use [addu], [addiu], and [subu] instead of [add], [addi], and
   [sub]. The only difference is that the former never generate
   overflow exceptions. This is what we desire, since the semantics
   of Pseudo-Pascal says nothing about overflow exceptions. Overflow
   is silent. *)

let print_unop reg f (op, dst, src) =
  match op with
  | UOpAddi 0 ->
      sprintf "move    %a, %a" reg dst reg src (* pseudo-instruction *)
  | UOpAddi i ->
      sprintf "addi    %a, %a, %d" reg dst reg src i
  | UOpSlti i ->
      sprintf "slti    %a, %a, %d" reg dst reg src i
  | UOpSltiu i ->
      sprintf "sltiu   %a, %a, %d" reg dst reg src i
  | UOpAndi i ->
      sprintf "andi    %a, %a, %d" reg dst reg src i
  | UOpOri i ->
      sprintf "ori     %a, %a, %d" reg dst reg src i
  | UOpXori i ->
      sprintf "xori    %a, %a, %d" reg dst reg src i
  | UOpNeg ->
      sprintf "neg     %a, %a" reg dst reg src
  | UOpNot ->
      sprintf "not     %a, %a" reg dst reg src

let print_binop = function
  | OpAdd ->
      "add    "
  | OpSub ->
      "sub    "
  | OpMul ->
      "mulo   "
  | OpDiv ->
      "div    " (* pseudo-instruction *)
  | OpDivu ->
      "divu   " (* pseudo-instruction *)
  | OpModu ->
      "remu   " (* pseudo-instruction *)
  | OpLt ->
      "slt    "
  | OpLtu ->
      "sltu   "
  | OpLe ->
      "sle    " (* pseudo-instruction *)
  | OpLeu ->
      "sleu   " (* pseudo-instruction *)
  | OpGt ->
      "sgt    " (* pseudo-instruction *)
  | OpGtu ->
      "sgtu   " (* pseudo-instruction *)
  | OpGe ->
      "sge    " (* pseudo-instruction *)
  | OpGeu ->
      "sgeu   " (* pseudo-instruction *)
  | OpEq ->
      "seq    " (* pseudo-instruction *)
  | OpNe ->
      "sne    " (* pseudo-instruction *)
  | OpSllv ->
      "sllv   "
  | OpSrav ->
      "srav   "
  | OpSrlv ->
      "srlv   "
  | OpAnd ->
      "and    "
  | OpOr ->
      "or     "
  | OpXor ->
      "xor    "

(*
let print_uncon reg f (cond, src) =
  match cond with
  | UConGez ->
      sprintf "bgez   %a" reg src
  | UConGtz ->
      sprintf "bgtz   %a" reg src
  | UConLez ->
      sprintf "blez   %a" reg src
  | UConLtz ->
      sprintf "bltz   %a" reg src

let print_bincon = function
  | ConEq ->
      "beq    "
  | ConNe ->
      "bne    "
*)


module Eval (Val : Value.S) = struct

  let unop = function
    | UOpAddi i -> Val.add (Val.of_int i)
    | UOpSlti i -> (fun v -> Val.cmp_lt v (Val.of_int i))
    | UOpSltiu i -> (fun v -> Val.cmp_lt_u v (Val.of_int i))
    | UOpAndi i -> Val.and_op (Val.of_int i)
    | UOpOri i -> Val.or_op (Val.of_int i)
    | UOpXori i -> Val.xor (Val.of_int i)
    | UOpNeg -> Val.negint
    | UOpNot -> Val.notint

  let binop = function
    | OpAdd -> Val.add
    | OpSub -> Val.sub
    | OpMul -> Val.mul
    | OpDiv -> Val.div
    | OpDivu -> Val.divu
    | OpModu -> Val.modulo
    | OpLt -> Val.cmp_lt
    | OpLtu -> Val.cmp_lt_u
    | OpLe -> Val.cmp_le
    | OpLeu -> Val.cmp_le_u
    | OpGt -> Val.cmp_gt
    | OpGtu -> Val.cmp_gt_u
    | OpGe -> Val.cmp_ge
    | OpGeu -> Val.cmp_ge_u
    | OpEq -> Val.cmp_eq
    | OpNe -> Val.cmp_ne
    | OpSllv -> Val.shl
    | OpSrav -> Val.shr
    | OpSrlv -> Val.shru
    | OpAnd -> Val.and_op
    | OpOr -> Val.or_op
    | OpXor -> Val.xor

(*
  let fun_of_uncon = function
    | UConGez -> Val.cmp_ge
    | UConGtz -> Val.cmp_gt
    | UConLez -> Val.cmp_le
    | UConLtz -> Val.cmp_lt

  let uncon con = (fun_of_uncon con) Val.zero

  let bincon = function
    | ConEq -> Val.cmp_eq
    | ConNe -> Val.cmp_ne
*)

end


(* The size of what can be loaded and stored. *)

type data_size = Word | HalfWord | Byte

let data_size_assoc = [(Word, 4) ; (HalfWord, 2) ; (Byte, 1)]
let data_size_assoc_rev = List.map (fun (x, y) -> (y, x)) data_size_assoc

let byte_size_of_data_size data_size =
  if List.mem_assoc data_size data_size_assoc then
    List.assoc data_size data_size_assoc
  else error "unknown data size"

let data_size_of_byte_size size =
  if List.mem_assoc size data_size_assoc_rev then
    List.assoc size data_size_assoc_rev
  else error ("unsupported size:" ^ (string_of_int size))


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

  (* Memory read. Parameters the size of what to load are destination
     register, source (address) registers, and a constant offset. *)

  | ILoad of data_size * 'register * 'register address

  (* Memory write. Parameters are the size of what to store address registers,
     a constant offset, and value register. *)

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

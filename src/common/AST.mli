
(** This file defines some common structures of several languages. *)

(** Types and Signatures *)

type signedness = Signed | Unsigned

type size = int (* in bytes *)

type sig_type =
  | Sig_int of size * signedness
  | Sig_float of size * signedness
  | Sig_offset
  | Sig_ptr

type type_return = Type_ret of sig_type | Type_void

type signature = { args: sig_type list ; res: type_return }


type ident = string (* identifiers for variable and function names *)

type immediate = int (* immediate values for assembler constants and offsets *)


(** Memory quantities are the size of what can fit in memory. *)

type quantity =
  | QInt of size (* concrete size in bytes *)
  | QOffset      (* size of an offset *)
  | QPtr         (* size of a pointer *)

type abstract_size =
  | SQ of quantity
  | SProd of abstract_size list
  | SSum of abstract_size list
  | SArray of int * abstract_size

type abstract_offset = abstract_size * int (* nth in size *)


(** Comparison between integers or floats *)

type cmp = Cmp_eq | Cmp_ne | Cmp_gt | Cmp_ge | Cmp_lt | Cmp_le

(** Constants in high level languages *)

type cst =
  | Cst_int of int                     (* integer constant *)
  | Cst_float of float                 (* float constant *)
  | Cst_addrsymbol of ident            (* address of a global symbol *)
  | Cst_stack                          (* address of the stack *)
  | Cst_offset of abstract_offset      (* offset *)
  | Cst_sizeof of abstract_size        (* size of a type *)

(** Unary operations *)

type op1 =  
  | Op_cast of (size * signedness) * size
  | Op_negint           (**r integer opposite *)
  | Op_notbool          (**r boolean negation  *)
  | Op_notint           (**r bitwise complement  *)
  | Op_id               (**r identity *)
  | Op_ptrofint         (**r int to pointer *)
  | Op_intofptr         (**r pointer to int *)

(** Binary operations *)

type op2 =
  | Op_add         (**r integer addition *)
  | Op_sub         (**r integer subtraction *)
  | Op_mul         (**r integer multiplication *)
  | Op_div         (**r integer division *)
  | Op_divu        (**r integer unsigned division *)
  | Op_mod         (**r integer modulus *)
  | Op_modu        (**r integer unsigned modulus *)
  | Op_and         (**r bitwise ``and'' *)
  | Op_or          (**r bitwise ``or'' *)
  | Op_xor         (**r bitwise ``xor'' *)
  | Op_shl         (**r left shift *)
  | Op_shr         (**r right shift *)
  | Op_shru        (**r unsigned right shift *)
  | Op_cmp of cmp  (**r integer comparison *)
  | Op_cmpu of cmp (**r unsigned integer comparison *)
  | Op_addp        (**r addition for a pointer and an integer *)
  | Op_subp        (**r substraction for a pointer and a integer *)
  | Op_subpp       (**r substraction for two pointers *)
  | Op_cmpp of cmp (**r pointer comparaison *)

(* Datas are used to initialize the value of variables *)

type data = 
(* (* Disabled: needed abstraction. *)
  | Data_reserve of int (* only reserve some space *)
*)
  | Data_int8 of int
  | Data_int16 of int
  | Data_int32 of int
  | Data_float32 of float
  | Data_float64 of float

type data_size = Byte | HalfWord | Word

(* External functions. *)

type external_function = { ef_tag: ident ; ef_sig: signature }

(* Traces returned by interpreters: result and cost labels are observed. The
   result is interpreted as an 8 bits integer for coherence between
   languages. *)

type trace = IntValue.int32 * CostLabel.t list

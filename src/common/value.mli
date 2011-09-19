
(** This module describes the values manipulated by high level languages. *)

(** The representation of values depends on the size of integers and the size of
    addresses.  *)

(** This is the signature of the parameter module. *)

module type DATA_SIZE =
sig
  val int_size : int
  val ptr_size : int
end

(** This is the signature of the module that provides types and functions to
    manipulate values. *)

module type S =
sig

  val int_size : int
  val ptr_size : int

  (** The type of values. A value may be: a bounded integer, a chunk of an
      address (exactly an address when they have the same size as a machine
      register), or undefined. *)
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val eq : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string

  (* The functions of this module may raise a Failure exception when
     trying to convert them to their various representation. *)

  val is_int : t -> bool
  val to_int : t -> int
  val of_int : int -> t

  val of_int_repr : IntValue.int_repr -> t
  val to_int_repr : t -> IntValue.int_repr

  val of_bool   : bool -> t
  val to_bool   : t -> bool

  val zero      : t
  val val_true  : t
  val val_false : t
  val is_true   : t -> bool
  val is_false  : t -> bool
  val undef     : t

  (** The cast operations below returns the undefined value for non-integer
      values. For integer values, it will return the integer value that
      represents the same quantity, but using every bits (sign or zero
      extension) of an integer value. For example, the function [cast8unsigned]
      should be read as "cast from an 8 bits unsigned integer". *)
  val cast8unsigned  : t -> t
  val cast8signed    : t -> t
  val cast16unsigned : t -> t
  val cast16signed   : t -> t
  val cast32         : t -> t

  (** [zero_ext v n m] performs a zero extension on [v] where [n] bytes are
      significant to a value where [m] bytes are significant. *)
  val zero_ext : t -> int -> int -> t
  (** [sign_ext v n m] performs a sign extension on [v] where [n] bytes are
      significant to a value where [m] bytes are significant. *)
  val sign_ext : t -> int -> int -> t

  (** Integer opposite *)
  val negint  : t -> t
  (** Boolean negation *)
  val notbool : t -> t
  (** Bitwise not *)
  val notint  : t -> t         

  val succ : t -> t
  val pred : t -> t
  val cmpl : t -> t

  (** [add_and_of v1 v2] returns the sum of [v1] and [v2], and whether this sum
      overflows. *)
  val add_and_of : t -> t -> (t * t)
  val add        : t -> t -> t
  (** [add_of v1 v2] returns the [1] value if the sum of [v1] and [v2]
      overflows, and [0] otherwise. *)
  val add_of     : t -> t -> t
  (** [sub_and_uf v1 v2] returns the substraction of [v1] and [v2], and whether
      this substraction underflows. *)
  val sub_and_uf : t -> t -> (t * t)
  val sub        : t -> t -> t
  (** [sub_uf v1 v2] returns the [1] value if the substraction of [v1] and [v2]
      underflows, and [0] otherwise. *)
  val sub_uf     : t -> t -> t
  val mul        : t -> t -> t
  val div        : t -> t -> t
  val divu       : t -> t -> t
  val modulo     : t -> t -> t
  val modulou    : t -> t -> t
  val and_op     : t -> t -> t
  val or_op      : t -> t -> t
  val xor        : t -> t -> t
  val shl        : t -> t -> t
  val shr        : t -> t -> t
  val shru       : t -> t -> t

  (** Signed comparisions *)
  val cmp_eq : t -> t -> t
  val cmp_ne : t -> t -> t
  val cmp_lt : t -> t -> t
  val cmp_ge : t -> t -> t
  val cmp_le : t -> t -> t
  val cmp_gt : t -> t -> t

  (** Unsigned comparisions *)
  val cmp_eq_u : t -> t -> t
  val cmp_ne_u : t -> t -> t
  val cmp_lt_u : t -> t -> t
  val cmp_ge_u : t -> t -> t
  val cmp_le_u : t -> t -> t
  val cmp_gt_u : t -> t -> t

  (** A chunk is a part of a value that has the size of a memory cell. *)

  type chunk
  val string_of_chunk : chunk -> string
  val undef_byte : chunk
  val is_undef_byte : chunk -> bool

  (** [break v] cuts [v] in chunks that each fits into a memory cell. In the
      resulting list, the first element is the low bits, and the last is the
      high bits (little endian representation). *)
  val break : t -> chunk list
  (** [merge l] creates the value where the first element of [l] is its low
      bits, etc, and the last element of [l] is its high bits (little endian
      representation). *)
  val merge : chunk list -> t

  (** Addresses from interpreters point of view. *)

  (** Some architectures have pointers bigger than integers. In this case, only
      a chunk of pointer can fit into a machine register. Thus, an address is
      represented by several values (little endian representation). *)

  type address = t list
  val string_of_address : address -> string
  val null : address

  (** Addresses from the memory point of view. *)

  (** Addresses are represented by a block, i.e. a base address, and an offset
      from this block. Both blocks and offsets are represented using bounded
      integers. *)
  module Block  : IntValue.S
  module Offset : IntValue.S

  type mem_address

  val is_mem_address : address -> bool

  val of_mem_address        : mem_address -> address
  val to_mem_address        : address -> mem_address
  val make_mem_address      : Block.t -> Offset.t -> mem_address
  val decompose_mem_address : mem_address -> Block.t * Offset.t
  val block_of_address      : address -> Block.t
  val offset_of_address     : address -> Offset.t

  val change_address_offset : address -> Offset.t -> address
  val add_address           : address -> Offset.t -> address
  val eq_address            : address -> address -> bool

end


(** The functor to create bounded values from a size and a signedness. *)

module Make (D : DATA_SIZE) : S

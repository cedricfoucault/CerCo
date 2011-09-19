
(** This module defines functions to manipulate bounded integers. They can be
    used to represent sequences of bits. *)

(* Integers, whatever their size, will be represented using the Big_int
   module. This allows immediate conversion, and allows the representation of
   any integer (that fits into memory). *)

type int_repr = Big_int.big_int
val print_int_repr : int_repr -> string

(* The parameter module. Bounded integers are characterized by the number of
   bits used to represent them. *)
    
module type INTTYPE = 
sig
  val size : int (* in bytes *)
end

(* The signature provided to manipulate bounded integers. *)

module type S = sig

  type t = Big_int.big_int

  val compare   : t -> t -> int
  val to_string : t -> string
  val zero      : t
  val one       : t

  val to_signed_int_repr   : t -> int_repr
  val to_unsigned_int_repr : t -> int_repr

  val succ    : t -> t
  val pred    : t -> t
  val add     : t -> t -> t
  (** [add_of i1 i2] returns [true] iff adding [i1] and [i2] overflows. *)
  val add_of  : t -> t -> bool
  val sub     : t -> t -> t
  (** [sub_uf i1 i2] returns [true] iff substracting [i1] and [i2]
      underflows. *)
  val sub_uf  : t -> t -> bool
  val mul     : t -> t -> t
  val div     : t -> t -> t
  val divu    : t -> t -> t
  val modulo  : t -> t -> t
  val modulou : t -> t -> t
  val eq      : t -> t -> bool
  val neq     : t -> t -> bool
  val lt      : t -> t -> bool
  val ltu     : t -> t -> bool
  val le      : t -> t -> bool
  val leu     : t -> t -> bool
  val gt      : t -> t -> bool
  val gtu     : t -> t -> bool
  val ge      : t -> t -> bool
  val geu     : t -> t -> bool
  val neg     : t -> t
  val lognot  : t -> t
  val logand  : t -> t -> t
  val logor   : t -> t -> t
  val logxor  : t -> t -> t
  val shl     : t -> t -> t
  val shr     : t -> t -> t
  val shrl    : t -> t -> t
  val max     : t -> t -> t
  val maxu    : t -> t -> t
  val min     : t -> t -> t
  val minu    : t -> t -> t
  val cast    : int_repr -> t
  val of_int  : int -> t
  val to_int  : t -> int

  (** [zero_ext n a] performs zero extension on [a] where [n] bits are
      significant. *)
  val zero_ext : int -> t -> t
  (** [sign_ext n a] performs sign extension on [a] where [n] bits are
      significant. *)
  val sign_ext : int -> t -> t

  (** [break i n] cuts [i] in [n] parts. In the resulting list, the first
      element is the low bits, and the last is the high bits (little endian
      representation). *)
  val break : t -> int -> t list
  (** [merge l] creates the integer where the first element of [l] is its low
      bits, etc, and the last element of [l] is its high bits (little endian
      representation). *)
  val merge : t list -> t

end

(** The functor to create bounded integers from a size. *)

module Make: functor (IntType: INTTYPE) -> S

module Int8  : S
module Int16 : S
module Int32 : S

type int8  = Int8.t
type int16 = Int16.t
type int32 = Int32.t


(*
module Int8s   : S
module Int8u   : S
module Int16s  : S
module Int16u  : S
module Int32   : S

(** Unbounded integers. *)
module Integer : S

type int8s   = Int8s.t
type int8u   = Int8u.t
type int16s  = Int16s.t
type int16u  = Int16u.t
type int32   = Int32.t
type integer = Integer.t
*)

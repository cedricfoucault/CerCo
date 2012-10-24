(* Pasted from Pottier's PP compiler *)

(** This module provides access to several of the operations defined
    in Objective Caml's standard library module [Int32], under the
    names usually reserved for operations on integers of type
    [int]. This allows switching from 31- or 63-bit arithmetic to
    32-bit arithmetic. *)

val (+): int32 -> int32 -> int32
val (-): int32 -> int32 -> int32
val ( * ): int32 -> int32 -> int32
val (/): int32 -> int32 -> int32
val (mod): int32 -> int32 -> int32
val (land): int32 -> int32 -> int32
val (lor): int32 -> int32 -> int32
val (lxor): int32 -> int32 -> int32
val (lsl): int32 -> int -> int32
val (asr): int32 -> int -> int32
val (lsr): int32 -> int -> int32
val (<=): int32 -> int32 -> bool
val (<): int32 -> int32 -> bool
val (>=): int32 -> int32 -> bool
val (>): int32 -> int32 -> bool
val (~-): int32 -> int32
val max: int32 -> int32 -> int32

val max_int: int32
val min_int: int32

(* ------------------------------------------------------------------------- *)
(* Here are some extra operations. *)

(* [fits16 i] tells whether [i] fits in 16 bits, that is, whether it
   lies within the range [-2^15 .. 2^15 - 1]. *)

val fits16: int32 -> bool

(* [is_power_of_two i] tells whether [i] is a positive power of 2. In
   that case, [log2 i] is its logarithm. [exp2 i] is two to the [i]. *)

val is_power_of_two: int32 -> bool
val log2: int32 -> int32
val exp2: int32 -> int32

(* ------------------------------------------------------------------------- *)
(* Here are some operations on arrays with 32-bit indices. *)

module Array : sig

  val make: int32 -> 'a -> 'a array
  val init: int32 -> (int32 -> 'a) -> 'a array
  val get: 'a array -> int32 -> 'a
  val set: 'a array -> int32 -> 'a -> unit
  val length: 'a array -> int32

end

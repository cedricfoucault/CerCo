
(** This module defines functions to manipulate bounded integers. They can be
    used to represent sequences of bits. *)

open Big_int


(* Integers, whatever their size, will be represented using the Big_int
   module. This allows immediate conversion, and allows the representation of
   any integer (that fits into memory). *)

type int_repr = Big_int.big_int

let print_int_repr = Big_int.string_of_big_int

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


module Make (IntType: INTTYPE) : S = 
struct

  type t = Big_int.big_int

  let size = IntType.size * 8 (* real size, i.e. in bits *)

  let compare = compare_big_int
  let zero = zero_big_int
  let one = unit_big_int
  let two = succ_big_int unit_big_int

  (* Integers will all be taken modulo the following value. *)
  let _mod = power_int_positive_int 2 size

  (* The lower bound (inclusive). *)
  let lower_bound = zero
  (* The upper bound (inclusive). *)
  let upper_bound = pred_big_int _mod

  (* [cast a] returns a modulo of [a] such that the result fits in the interval
     of representation. *)
  let cast a = mod_big_int a _mod

  (* Half bound (exclusive), i.e. upper bound of signed integers. *)
  let half_bound = power_int_positive_int 2 (size-1)

  (* Signed value of [a]. *)
  let signed a =
    let a = cast a in
    if lt_big_int a half_bound then a
    else sub_big_int a _mod

  let to_string a = string_of_big_int (signed a)

  let to_signed_int_repr a = signed a

  let to_unsigned_int_repr a = a

  let signed_op op a b = op (signed a) (signed b)

  let succ a = cast (succ_big_int a)
  let pred a = cast (pred_big_int a)
  let add a b = cast (add_big_int a b)

  (* [add_of i1 i2] returns [true] iff adding [i1] and [i2] overflows. *)
  let add_of a b = gt_big_int (add_big_int a b) upper_bound

  let sub a b = cast (sub_big_int a b)

  let cast_op op a b = op (cast a) (cast b)

  let mul a b = cast (mult_big_int a b)
  let div a b = cast (signed_op div_big_int a b)
  let divu a b = cast_op div_big_int a b
  let modulo a b = cast (signed_op mod_big_int a b)
  let modulou a b = cast_op mod_big_int a b

  let eq = eq_big_int
  let neq a b = not (eq a b)
  let lt a b = signed_op lt_big_int a b
  let le a b = signed_op le_big_int a b
  let gt a b = signed_op gt_big_int a b
  let ge a b = signed_op ge_big_int a b
  let ltu a b = cast_op lt_big_int a b
  let leu a b = cast_op le_big_int a b
  let gtu a b = cast_op gt_big_int a b
  let geu a b = cast_op ge_big_int a b

  (* [sub_uf i1 i2] returns [true] iff substracting [i1] and [i2] underflows. *)
  let sub_uf a b = lt_big_int (sub_big_int a b) zero

  let of_int i = cast (big_int_of_int i)
  let to_int i = int_of_big_int (signed (cast i))

  let neg a = cast (minus_big_int a)

  let lognot = sub upper_bound

  let shl a b =
    let pow = power_int_positive_big_int 2 (cast b) in
    cast (mult_big_int a pow)

  let shr a b =
    let a = cast a in
    let b = cast b in
    let added =
      if lt_big_int a half_bound then zero
      else half_bound in
    let rec aux acc b =
      if eq b zero then acc
      else
	let cont_acc = add added (divu acc two) in
	let cont_b = pred b in
	aux cont_acc cont_b
    in
    cast (aux a b)

  let shrl a b =
    let pow = power_int_positive_big_int 2 (cast b) in
    cast (div_big_int (cast a) pow)

  let max a b = if lt a b then b else a
  let min a b = if gt a b then b else a
  let maxu a b = if ltu a b then b else a
  let minu a b = if gtu a b then b else a

  let is_odd a = eq (modulou a two) one
  (* [to_bits a] returns the list of bits (0 or 1) that [a] represents. *)
  let to_bits a =
    let rec aux acc a i =
      if i >= size then acc
      else aux ((is_odd a) :: acc) (divu a two) (i+1)
    in
    aux [] (cast a) 0

  (* [from_bits bits] returns the integer that the list of bits [bits]
     represents. *)
  let from_bits bits =
    let rec aux acc = function
      | [] -> acc
      | b :: bits ->
	let next_acc = mul acc two in
	let next_acc = if b then succ next_acc else next_acc in
	aux next_acc bits
    in
    aux zero bits

  (* [binary_log_op f a b] applies the binary boolean operation [f]
     pointwisely to the bits that [a] and [b] represent. *)
  let binary_log_op f a b =
    from_bits (List.map2 f (to_bits a) (to_bits b))

  let xor a b = (a || b) && (not (a && b))

  let logand = binary_log_op (&&)
  let logor = binary_log_op (||)
  let logxor = binary_log_op xor


  (* [zero_ext n a] performs zero extension on [a] where [n] bits are
     significant. *)
  let zero_ext n a =
    let pow2 = power_int_positive_int 2 n in
    cast (mod_big_int a pow2)

  (* [sign_ext n a] performs sign extension on [a] where [n] bits are
     significant. *)
  let sign_ext n a =
    let a' = zero_ext n a in
    let pow2 = power_int_positive_int 2 (n-1) in
    let sign = divu a pow2 in
    if is_odd sign then
      let added = shr half_bound (of_int (n-1)) in
      add a' added
    else a'


  (* [break i n] cuts [i] in [n] parts. In the resulting list, the first element
     is the low bits, and the last is the high bits (little endian
     representation). *)
  let break a n =
    let chunk_size = size / n in
    let pow2_chunk_size = power_int_positive_int 2 chunk_size in
    let rec aux acc a i =
      if i = 0 then acc
      else
	let (next, chunk) = quomod_big_int a pow2_chunk_size in
	aux ((cast chunk) :: acc) next (i-1)
    in
    List.rev (aux [] (cast a) n)

  (* [merge l] creates the integer where the first element of [l] is its low
     bits, etc, and the last element of [l] is its high bits (little endian
     representation). *)
  let merge = function
    | [] -> zero
    | al ->
      let nb_chunks = List.length al in
      let chunk_size = size / nb_chunks in
      let pow2_chunk_size = power_int_positive_int 2 chunk_size in
      let rec aux pow2 = function
	| [] -> zero
	| a :: al -> add (mul a pow2) (aux (mul pow2 pow2_chunk_size) al)
      in
      aux one al

end


module Int8  : S = Make (struct let size = 1 end)
module Int16 : S = Make (struct let size = 2 end)
module Int32 : S = Make (struct let size = 4 end)

type int8  = Int8.t
type int16 = Int16.t
type int32 = Int32.t

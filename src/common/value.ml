
(** This module describes the values manipulated by high level
    languages. *)


let error_prefix = "Value"
let error s = Error.global_error error_prefix s

open IntValue


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

  (** Addresses from the memory point of view. *)

  (** Some architectures have pointers bigger than integers. In this case, only
      a chunk of pointer can fit into a machine register. Thus, an address is
      represented by several values (little endian representation). *)

  type address = t list
  val string_of_address : address -> string
  val null : address

  (** Addresses from the memory point of view. Only use the functions below in
      the Memory module.*)

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


module Make (D : DATA_SIZE) =
struct

  let int_size = D.int_size
  let ptr_size = D.ptr_size

  (* Integer values. *)
  module ValInt = IntValue.Make (struct let size = D.int_size end)

  (* Addresses are represented by a block, i.e. a base address, and an offset
     from this block. Both blocks and offsets are represented using bounded
     integers. However, values must fit into a machine register. Some
     architectures, like the 8051, have addresses bigger than registers. Pointer
     values will only represent a part of a full pointer (or exactly a pointer
     when addresses fit into a register). *)

  (* Blocks and offsets need [D.ptr_size] bits each to be represented. Indeed,
     one may allocate 2^[D.ptr_size] blocks of size one byte, or one block of
     size 2^[D.ptr_size] bytes. *)

  module ValBlock  = IntValue.Make (struct let size = D.int_size end)
  module ValOffset = IntValue.Make (struct let size = D.int_size end)

  type t =
    | Val_int of ValInt.t
    | Val_ptr of ValBlock.t * ValOffset.t
    | Val_undef

  let compare a b = match a, b with
    | Val_int i1, Val_int i2 -> ValInt.compare i1 i2
    | Val_ptr (b1, off1), Val_ptr (b2, off2) ->
      let i1 = ValBlock.compare b1 b2 in
      if i1 = 0 then ValOffset.compare off1 off2
      else i1
    | Val_undef, Val_undef -> 0
    | Val_int _, _ -> 1
    | _, Val_int _ -> -1
    | Val_ptr _, _ -> 1
    | _, Val_ptr _ -> -1

  (*
    let hash = function 
    | Val_int i -> ValInt.to_int i
    | Val_float f -> int_of_float f
    | Val_undef -> 0
    | Val_ptr (b,o)
    | Val_ptrh (b,o)
    | Val_ptrl (b,o) -> ValInt.to_int (ValInt.add b o)
  *)

  let hash = Hashtbl.hash

  let equal a b = compare a b = 0
  let eq a b = compare a b = 0

  let to_string = function
    | Val_int i -> ValInt.to_string i
    | Val_ptr (b, off) ->
      "VPtr(" ^ (ValBlock.to_string b) ^ ", " ^ (ValOffset.to_string off) ^ ")"
    | Val_undef -> "undef"

  let is_int = function
    | Val_int _ -> true
    | _ -> false

  let to_int = function
    | Val_int i -> ValInt.to_int i
    | _ -> raise (Failure "Value.to_int")

  let of_int i = Val_int (ValInt.of_int i)
  let one = of_int 1

  let of_int_repr i = Val_int i

  let to_int_repr = function
    | Val_int i -> i
    | _ -> raise (Failure "Value.to_int_repr")

  let zero      = Val_int ValInt.zero
  let val_true  = Val_int ValInt.one
  let val_false = Val_int ValInt.zero

  let is_true = function
    | Val_int i -> ValInt.neq i ValInt.zero
    | Val_ptr (b, off) ->
      (ValBlock.neq b ValBlock.zero) || (ValOffset.neq off ValOffset.zero)
    | _ -> false

  let is_false = function
    | Val_int i -> ValInt.eq i ValInt.zero
    | Val_ptr (b, off) ->
      (ValBlock.eq b ValBlock.zero) && (ValOffset.eq off ValOffset.zero)
    | _ -> false

  let of_bool = function
    | true -> val_true
    | false -> val_false

  let to_bool v =
    if is_true v then true
    else
      if is_false v then false
      else error "Undefined value."

  let undef = Val_undef

  let cast cast_fun = function
    | Val_int i -> Val_int (cast_fun i)
    | _ -> Val_undef

  (** Sign or 0 extensions from various bounded integers. *)
  let cast8unsigned = cast (ValInt.zero_ext 8)
  let cast8signed = cast (ValInt.sign_ext 8)
  let cast16unsigned = cast (ValInt.zero_ext 16)
  let cast16signed = cast (ValInt.sign_ext 16)
  let cast32 = cast (ValInt.zero_ext 32)


  let unary_int_op f = function
    | Val_int i -> Val_int (f i)
    | _ -> Val_undef

  let binary_int_op f v1 v2 = match v1, v2 with
    | Val_int i1, Val_int i2 -> Val_int (f i1 i2)
    | _ -> Val_undef

  let negint = unary_int_op ValInt.neg

  let notbool v =
    if is_true v then val_false
    else
      if is_false v then val_true
      else Val_undef

  let notint = unary_int_op ValInt.lognot

  let cmpl = unary_int_op ValInt.lognot

  (** [add_and_of v1 v2] returns the sum of [v1] and [v2], and whether this sum
      overflows. *)
  let add_and_of v1 v2 = match v1, v2 with
    | Val_int i1, Val_int i2 ->
      (Val_int (ValInt.add i1 i2), of_bool (ValInt.add_of i1 i2))
    | Val_int i, Val_ptr (b, off)
    | Val_ptr (b, off), Val_int i ->
      let i = ValOffset.cast i in
      (Val_ptr (b, ValOffset.add off i), of_bool (ValOffset.add_of off i))
    | _, _ -> (Val_undef, Val_undef)

  let add v1 v2 = fst (add_and_of v1 v2)
  let add_of v1 v2 = snd (add_and_of v1 v2)

  let succ v = add v (Val_int ValInt.one)

  (** [sub_and_uf v1 v2] returns the substraction of [v1] and [v2], and whether
      this substraction underflows. *)
  let sub_and_uf v1 v2 = match v1, v2 with
    | Val_int i1, Val_int i2 ->
      (Val_int (ValInt.sub i1 i2), of_bool (ValInt.sub_uf i1 i2))
    | Val_ptr (b, off), Val_int i ->
      let i = ValOffset.cast i in
      (Val_ptr (b, ValOffset.sub off i), of_bool (ValOffset.sub_uf off i))
    | Val_ptr (b1, off1), Val_ptr (b2, off2) when ValBlock.eq b1 b2 ->
      (Val_int (ValInt.cast (ValOffset.sub off1 off2)),
       of_bool (ValOffset.sub_uf off1 off2))
    | _, _ -> (Val_undef, Val_undef)

  let sub v1 v2 = fst (sub_and_uf v1 v2)
  let sub_uf v1 v2 = snd (sub_and_uf v1 v2)

  let pred v = sub v (Val_int ValInt.one)

  let mul = binary_int_op ValInt.mul

  let is_zero = function
    | Val_int i when ValInt.eq i ValInt.zero -> true
    | _ -> false

  let error_if_zero op v1 v2 =
    if is_zero v2 then error "Division by zero."
    else binary_int_op op v1 v2

  let div     = error_if_zero ValInt.div
  let divu    = error_if_zero ValInt.divu
  let modulo  = error_if_zero ValInt.modulo
  let modulou = error_if_zero ValInt.modulou

  let and_op = binary_int_op ValInt.logand
  let or_op  = binary_int_op ValInt.logor
  let xor    = binary_int_op ValInt.logxor
  let shl    = binary_int_op ValInt.shl
  let shr    = binary_int_op ValInt.shr
  let shru   = binary_int_op ValInt.shrl

  let ext sh v n m =
    let n = n * 8 in
    let m = m * 8 in
    let real_size = D.int_size * 8 in
    let int_sh sh v n = sh v (of_int n) in
    if n >= m then
      if m = real_size then v
      else modulou v (shl one (of_int m))
    else
      let v = int_sh shl v (real_size - n) in
      let v = int_sh sh v (m - n) in
      int_sh shru v (real_size - m)

  let zero_ext = ext shru
  let sign_ext = ext shr

  let cmp f_int f_off v1 v2 = match v1, v2 with
    | Val_int i1, Val_int i2 -> of_bool (f_int i1 i2)
    | Val_ptr (b1, off1), Val_ptr (b2, off2) when ValBlock.eq b1 b2 ->
      of_bool (f_off off1 off2)
    | _ -> Val_undef

  let cmp_eq = cmp ValInt.eq ValOffset.eq
  let cmp_ne = cmp ValInt.neq ValOffset.neq
  let cmp_lt = cmp ValInt.lt ValOffset.lt
  let cmp_ge = cmp ValInt.ge ValOffset.ge
  let cmp_le = cmp ValInt.le ValOffset.le
  let cmp_gt = cmp ValInt.gt ValOffset.gt

  let cmp_eq_u = cmp ValInt.eq ValOffset.eq
  let cmp_ne_u = cmp ValInt.neq ValOffset.neq
  let cmp_lt_u = cmp ValInt.ltu ValOffset.ltu
  let cmp_ge_u = cmp ValInt.geu ValOffset.geu
  let cmp_le_u = cmp ValInt.leu ValOffset.leu
  let cmp_gt_u = cmp ValInt.gtu ValOffset.gtu


  (* The memory is based on byte values. In order to be able to fit a bigger
     integer or pointer value in memory, we need to be able to break this value
     into several values of size a byte. An integer will be broken into multiple
     8 bits integers. A pointer will be broken into several couples of 8 bits
     blocks and 8 bits offsets. *)

  (* 8 bits integers *)
  module IntChunk = IntValue.Int8
  (* 8 bits blocks *)
  module BlockChunk = IntValue.Int8
  (* 8 bits offsets *)
  module OffsetChunk = IntValue.Int8

  (** A chunk is a part of a value that has the size of a memory cell. *)

  type chunk =
    | Byte_int of IntChunk.t
    | Byte_ptr of BlockChunk.t * OffsetChunk.t
    | Byte_undef

  let string_of_chunk = function
    | Byte_int i -> IntChunk.to_string i
    | Byte_ptr (b, off) ->
      "BPtr(" ^ (BlockChunk.to_string b) ^ ", " ^
      (OffsetChunk.to_string off) ^ ")"
    | Byte_undef -> "Bundef"

  let undef_byte = Byte_undef

  let is_undef_byte = function
    | Byte_undef -> true
    | _ -> false

  let break_and_cast break cast x n =
    let ys = break x n in
    List.map cast ys

  (** [break v] cuts [v] in chunks that each fits into a memory cell. In the
      resulting list, the first element is the low bits, and the last is the
      high bits (little endian representation). *)
  let break v =
    let nb_chunks = D.int_size in
    match v with
      | Val_ptr (b, off) ->
	let bbs = break_and_cast ValBlock.break BlockChunk.cast b nb_chunks in
	let boffs =
	  break_and_cast ValOffset.break OffsetChunk.cast off nb_chunks in
	List.map2 (fun bb boff -> Byte_ptr (bb, boff)) bbs boffs
      | Val_int i ->
	let bis = break_and_cast ValInt.break IntChunk.cast i nb_chunks in
	List.map (fun i' -> Byte_int i') bis
      | _ -> MiscPottier.make Byte_undef nb_chunks

  (** [all_are_pointers l] returns [true] iff the values in the list [l] all are
      pointers. *)
  let all_are_pointers =
    let f b v = b && (match v with Byte_ptr _ -> true | _ -> false) in
    List.fold_left f true

  (** [all_are_integers l] returns [true] iff the values in the list [l] all are
      integers. *)
  let all_are_integers =
    let f b v = b && (match v with Byte_int _ -> true | _ -> false) in
    List.fold_left f true

  let bblock_of_chunk = function
    | Byte_ptr (b, _) -> b
    | _ -> assert false (* do not use on this argument *)

  let boffset_of_chunk = function
    | Byte_ptr (_, off) -> off
    | _ -> assert false (* do not use on this argument *)

  let bint_of_chunk = function
    | Byte_int i -> i
    | _ -> assert false (* do not use on this argument *)

  let cast_and_merge cast merge transform l =
    merge (List.map (fun x -> cast (transform x)) l)

  (** [merge l] creates the value where the first element of [l] is its low
      bits, etc, and the last element of [l] is its high bits (little endian
      representation). *)
  let merge = function
    | l when all_are_pointers l ->
      let b = cast_and_merge ValBlock.cast ValBlock.merge bblock_of_chunk l in
      let off =
	cast_and_merge ValOffset.cast ValOffset.merge boffset_of_chunk l in
      Val_ptr (b, off)
    | l when all_are_integers l ->
      Val_int (cast_and_merge ValInt.cast ValInt.merge bint_of_chunk l)
    | _ -> Val_undef


  (** Addresses from the memory point of view. *)

  (** Some architectures have pointers bigger than integers. In this case, only
      a chunk of pointer can fit into a machine register. Thus, an address is
      represented by several values (little endian representation). *)

  type address = t list

  let string_of_address vs =
    "[" ^ (MiscPottier.string_of_list " " to_string vs) ^ "]"


  (** Addresses are represented by a block, i.e. a base address, and an offset
      from this block. Both blocks and offsets are represented using bounded
      integers. *)

  let ptr_int_size = max (D.ptr_size / D.int_size) 1

  module Block  = IntValue.Make (struct let size = D.ptr_size end)
  module Offset = IntValue.Make (struct let size = D.ptr_size end)

  type mem_address = Block.t * Offset.t

  let is_mem_address =
    let f b v = b && (match v with | Val_ptr _ -> true | _ -> false) in
    List.fold_left f true

  let of_mem_address (b, off) =
    let bs = Block.break b ptr_int_size in
    let offs = Offset.break off ptr_int_size in
    let f b off = Val_ptr (ValBlock.cast b, ValOffset.cast off) in
    List.map2 f bs offs

  let decompose_Val_ptr = function
    | Val_ptr (b, off) -> (b, off)
    | _ -> error "Not an address."

  let to_mem_address vs =
    let (bs, offs) = List.split (List.map decompose_Val_ptr vs) in
    let b = Block.merge (List.map Block.cast bs) in
    let off = Offset.merge (List.map Offset.cast offs) in
    (b, off)

  let make_mem_address b off = (b, off)
  let decompose_mem_address addr = addr
  let block_of_address vs = fst (to_mem_address vs)
  let offset_of_address vs = snd (to_mem_address vs)

  let null = of_mem_address (Block.zero, Offset.zero)

  let change_address_offset vs off =
    let (b, _) = decompose_mem_address (to_mem_address vs) in
    of_mem_address (make_mem_address b off)

  let add_address vs off' =
    let (b, off) = decompose_mem_address (to_mem_address vs) in
    let off = Offset.add off off' in
    of_mem_address (make_mem_address b off)

  let eq_address addr1 addr2 =
    let f b v1 v2 = b && (eq v1 v2) in
    List.fold_left2 f true addr1 addr2

end

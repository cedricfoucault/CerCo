
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig

  type a
  type b
  type t

  val empty : t
  val is_empty : t -> bool

  val add1 : a -> b -> t -> t
  val add2 : b -> a -> t -> t

  val find1 : a -> t -> b
  val find2 : b -> t -> a

  val remove1 : a -> t -> t
  val remove2 : b -> t -> t

  val mem1 : a -> t -> bool
  val mem2 : b -> t -> bool

  val iter1 : (a -> b -> unit) -> t -> unit
  val iter2 : (b -> a -> unit) -> t -> unit

  val fold1 : (a -> b -> 'c -> 'c) -> t -> 'c -> 'c
  val fold2 : (b -> a -> 'c -> 'c) -> t -> 'c -> 'c

  val compare1 : (b -> b -> int) -> t -> t -> int
  val compare2 : (a -> a -> int) -> t -> t -> int

  val equal1 : (b -> b -> bool) -> t -> t -> bool
  val equal2 : (a -> a -> bool) -> t -> t -> bool

end

module Make (O1 : OrderedType) (O2 : OrderedType) : S with type a = O1.t and
							   type b = O2.t

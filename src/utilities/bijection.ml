
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


module Make (O1 : OrderedType) (O2 : OrderedType) : S
  with type a = O1.t and type b = O2.t = struct

  module Map1 = Map.Make (O1)
  module Map2 = Map.Make (O2)

  type a = O1.t
  type b = O2.t
  type t = O2.t Map1.t * O1.t Map2.t

  let eq1 a1 a2 = O1.compare a1 a2 = 0
  let eq2 b1 b2 = O2.compare b1 b2 = 0

  let empty = (Map1.empty, Map2.empty)
  let is_empty (a_b, _) = Map1.is_empty a_b

  let add1 a b (a_b, b_a) = (Map1.add a b a_b, Map2.add b a b_a)
  let add2 b a (a_b, b_a) = (Map1.add a b a_b, Map2.add b a b_a)

  let find1 a (a_b, _) = Map1.find a a_b
  let find2 b (_, b_a) = Map2.find b b_a

  let remove1 a (a_b, b_a) =
    let f b' a' b_a' = if eq1 a' a then b_a' else Map2.add b' a' b_a' in
    (Map1.remove a a_b, Map2.fold f b_a Map2.empty)

  let remove2 b (a_b, b_a) =
    let f a' b' a_b' = if eq2 b' b then a_b' else Map1.add a' b' a_b' in
    (Map1.fold f a_b Map1.empty, Map2.remove b b_a)

  let mem1 a (a_b, _) = Map1.mem a a_b
  let mem2 b (_, b_a) = Map2.mem b b_a

  let iter1 f (a_b, _) = Map1.iter f a_b
  let iter2 f (_, b_a) = Map2.iter f b_a

  let fold1 f (a_b, _) c = Map1.fold f a_b c
  let fold2 f (_, b_a) c = Map2.fold f b_a c

  let compare1 f (a_b1, _) (a_b2, _) = Map1.compare f a_b1 a_b2
  let compare2 f (_, b_a1) (_, b_a2) = Map2.compare f b_a1 b_a2

  let equal1 f (a_b1, _) (a_b2, _) = Map1.equal f a_b1 a_b2
  let equal2 f (_, b_a1) (_, b_a2) = Map2.equal f b_a1 b_a2

end

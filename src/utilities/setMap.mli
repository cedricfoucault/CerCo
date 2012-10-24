(* Pasted from Pottier's PP compiler *)

(** This signature defines a few operations over maps of keys to
    nonempty sets of items. Keys and items can have distinct types,
    hence the name [Heterogeneous].

    These maps can be used to represent directed bipartite graphs whose
    source vertices are keys and whose target vertices are items. Each
    key is mapped to the set of its successors. *)

module type Heterogeneous = sig

  (* These are the types of keys, items, and sets of items. *)

  type key
  type item
  type itemset

  (* This is the type of maps of keys to sets of items. *)

  type t

  (* [find x m] is the item set associated with key [x] in map [m], if
     such an association is defined; it is the empty set otherwise. *)

  val find: key -> t -> itemset

  (* [add x is m] extends [m] with a binding of [x] to the item set
     [is], if [is] is nonempty. If [is] is empty, it removes [x] from
     [m]. *)

  val add: key -> itemset -> t -> t

  (* [update x f m] is [add x (f (find x m)) m]. *)

  val update: key -> (itemset -> itemset) -> t -> t

  (* [mkedge x i m] extends [m] with a binding of [x] to the union of
     the set [m x] and the singleton [i], where [m x] is taken to be
     empty if undefined. In terms of graphs, [mkedge x i m] extends
     the graph [m] with an edge of [x] to [i]. *)

  val mkedge: key -> item -> t -> t

  (* [rmedge x i m] extends [m] with a binding of [x] to the
     difference of the set [m x] and the singleton [i], where the
     binding is considered undefined if that difference is empty.  In
     terms of graphs, [rmedge x i m] removes an edge of [x] to [i]
     to the graph [m]. *)

  val rmedge: key -> item -> t -> t

  (* [iter] and [fold] iterate over all edges in the graph. *)

  val iter: (key * item -> unit) -> t -> unit
  val fold: (key * item -> 'a -> 'a) -> t -> 'a -> 'a

  (* [pick m p] returns an arbitrary edge that satisfies predicate
     [p], if the graph contains one. *)

  val pick: t -> (key * item -> bool) -> (key * item) option

end

(* This functor offers an implementation of [Heterogeneous] out of
   standard implementations of sets and maps. *)

module MakeHetero
    (Set : sig
      type elt
      type t
      val empty: t
      val is_empty: t -> bool
      val add: elt -> t -> t
      val remove: elt -> t -> t
      val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    end)
    (Map : sig
      type key
      type 'a t
      val add: key -> 'a -> 'a t -> 'a t
      val find: key -> 'a t -> 'a
      val remove: key -> 'a t -> 'a t
      val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    end)
    : Heterogeneous with type key = Map.key
                     and type item = Set.elt
                     and type itemset = Set.t
		     and type t = Set.t Map.t

(* This signature defines a few common operations over maps of keys
   to sets of keys -- that is, keys and items have the same type,
   hence the name [Homogeneous].

   These maps can be used to represent general directed graphs. *)

module type Homogeneous = sig

  include Heterogeneous (* [key] and [item] intended to be equal *)

  (* [mkbiedge x1 x2 m] is [mkedge x1 x2 (mkedge x2 x1 m)]. *)

  val mkbiedge: key -> key -> t -> t

  (* [rmbiedge x1 x2 m] is [rmedge x1 x2 (rmedge x2 x1 m)]. *)

  val rmbiedge: key -> key -> t -> t

  (* [reverse m] is the reverse of graph [m]. *)

  val reverse: t -> t

  (* [restrict m] is the graph obtained by keeping only the vertices
     that satisfy predicate [p]. *)

  val restrict: (key -> bool) -> t -> t

end

module MakeHomo
    (Set : sig
      type elt
      type t
      val empty: t
      val is_empty: t -> bool
      val add: elt -> t -> t
      val remove: elt -> t -> t
      val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val filter: (elt -> bool) -> t -> t
    end)
    (Map : sig
      type key = Set.elt
      type 'a t
      val empty: 'a t
      val add: key -> 'a -> 'a t -> 'a t
      val find: key -> 'a t -> 'a
      val remove: key -> 'a t -> 'a t
      val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    end)
    : Homogeneous with type key = Set.elt
                   and type item = Set.elt
                   and type itemset = Set.t
                   and type t = Set.t Map.t


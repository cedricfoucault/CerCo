(* Pasted from Pottier's PP compiler *)

(** This signature describes atoms, that is, abstract entities
    equipped with a fresh element generation operation. *)

module type S = sig

  (* ------------------------------------------------------------------------- *)
  (* This is the type of atoms. *)

  type t

  (* Atoms do not exist in the ether -- they are taken from universes.
     Creating a fresh atom requires specifying which universe it
     should be taken from. Atoms that belong to distinct universes
     cannot be mixed. *)

  type universe

  (* One can create as many universes as desired. A universe initially
     contains no atoms. A universe carries a name (a string) that is
     used when converting atoms to strings. *)

  val new_universe: string -> universe

  (* A universe is populated by creating fresh atoms. The atom produced
     by [fresh u] is guaranteed to be distinct from all existing atoms
     in the universe [u]. *)

  val fresh: universe -> t

  (* Comparison of atoms. Only atoms that belong to a common universe
     can be compared. *)

  val equal: t -> t -> bool
  val compare: t -> t -> int

  (* Added by Nicolas Ayache: flexible comparison. Atoms from
     different universes can be compared. *)
  val flex_compare: t -> t -> int
  val flex_equal: t -> t -> bool

  (* Added by Nicolas Ayache. *)
  val universe_of: t -> universe
  val same_universe: t -> t -> bool

  (* [print a] converts the atom [a] to a string. The string
     representation is unique within the universe that [a] belongs
     to. It is globally unique if universe names are unique. *)

  val print: t -> string

  (* ------------------------------------------------------------------------- *)


  (* Added by Nicolas Ayache: more flexible maps and sets where atoms
     from different universes can coexist. *)

  module FlexSet : Set.S with type elt = t

  module FlexMap : Map.S with type key = t


  (* Sets of atoms. *)

  module Set : sig

    type elt = t

    (* This is the type of sets of atoms. Every set of atoms is
       implicitly and permanently associated with a universe, which
       all members of the set inhabit. *)

    type t

    (* Operations over sets include those defined in Objective Caml's
       standard [Set] module, with the restriction that operations
       should never mix atoms, or sets of atoms, that inhabit distinct
       universes. Consult [Set.S] in Objective Caml's
       documentation. *)

    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val remove: elt -> t -> t
    val singleton: elt -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val choose: t -> elt
    val equal: t -> t -> bool
    val cardinal: t -> int
    val elements: t -> elt list
    val filter: (elt -> bool) -> t -> t

    (* [disjoint s1 s2] tells whether the intersection of [s1]
       and [s2] is empty. *)

    val disjoint: t -> t -> bool

    (* [couple x1 x2] is the set that contains [x1] and [x2]. It
       can be a singleton set if [x1] and [x2] are equal. *)

    val couple: elt -> elt -> t

    (* [of_list xs] is the set whose members are the elements
       of the list [xs]. *)

    val of_list: elt list -> t

    (* [pick s] returns a pair of an element [x] of [s] and of the
       set [remove x s]. It raises [Not_found] if [s] is empty. *)

    val pick: t -> elt * t

    (* [exhaust s accu f] takes an element [x] off the set [s], and
       applies [f] to [x] and [accu]. This yields a number of new
       elements, which are added to [s], and a new accumulator [accu].
       This is repeated until [s] becomes empty, at which point the
       final value of the accumulator is returned. In short, this is a
       version of [fold] where the function [f] is allowed to produce
       new set elements. *)

    val exhaust: t -> 'a -> (elt -> 'a -> t * 'a) -> 'a

    (* [print s] converts the set [s] to a string. *)

    val print: t -> string

  end

  (* ------------------------------------------------------------------------- *)
  (* Maps over atoms. *)

  module Map : sig

    type key = t

    (* This is the type of maps over atoms. Every map over atoms is
       implicitly and permanently associated with a universe, which
       all keys inhabit. *)

    type +'a t

    (* Operations over maps include those defined in Objective Caml's
       standard [Map] module, with the restriction that operations
       should never mix atoms, or maps over atoms, that inhabit
       distinct universes. Consult [Map.S] in Objective Caml's
       documentation.*)

    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem: key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val remove: key -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

    (* [singleton x d] is the map that maps [x] to [d]. *)

    val singleton: key -> 'a -> 'a t

    (* [addm m1 m2] adds the bindings in the map [m1] to the map [m2],
       overriding any previous binding if [m1] and [m2] have common
       keys. *)

    val addm: 'a t -> 'a t -> 'a t

    (* [domain m] is the domain of the map [m]. *)

    val domain: 'a t -> Set.t

    (* [lift f s] lifts the set [s] into a map that maps every
       member [x] of [s] to [f x]. *)

    val lift: (key -> 'a) -> Set.t -> 'a t

    (* [restrict p m] restricts the domain of the map [m] to those
       keys that satisfy the predicate [p]. *)

    val restrict: (key -> bool) -> 'a t -> 'a t

    (* [generator u] creates a fresh reference [m] that holds an
       initially empty map; defines a function [generate] such that
       [generate d] generates a fresh atom [a], adds a mapping of [m]
       to [d] to [m], and returns [a]; and returns a pair of [m] and
       [generate]. *)

    val generator: universe -> 'a t ref * ('a -> key)

  end

  (* ------------------------------------------------------------------------- *)
  (* An imperative interface to maps. *)

  module ImperativeMap : sig
    type key = Map.key
    type 'data t
    val create: unit -> 'data t
    val clear: 'data t -> unit
    val add: key -> 'data -> 'data t -> unit
    val find: key -> 'data t -> 'data
    val iter: (key -> 'data -> unit) -> 'data t -> unit
  end

  (* ------------------------------------------------------------------------- *)
  (* Maps of atoms to sets of atoms. Consult the definition of
     [SetMap.Homogeneous] for a list of operations. *)

  module SetMap : SetMap.Homogeneous with type key = t
                                      and type item = t
	                              and type itemset = Set.t
			              and type t = Set.t Map.t

end

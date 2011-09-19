(* Pasted from Pottier's PP compiler *)

(** This module offers sets of elements where each element carries an
    integer priority. All operations execute in logarithmic time with
    respect to the number of elements in the set. *)

module Make (X : Set.OrderedType) : sig

  (* This is the type of priority sets. *)

  type t

  (* [empty] is the empty set. *)

  val empty: t

  (* [add x p s] inserts element [x] with priority [p]. *)

  val add: X.t -> int -> t -> t

  (* [remove x s] removes element [x]. *)

  val remove: X.t -> t -> t

  (* [change x p s] changes the priority of element [x] to [p]. *)

  val change: X.t -> int -> t -> t

  (* [increment x d s] increases the priority of element [x] by [d]. *)

  val increment: X.t -> int -> t -> t

  (* [incrementifx x p s] increases the priority of element [x] by [d]
     if [x] is a member of the priority set. *)

  val incrementifx: X.t -> int -> t -> t

  (* [priority x s] looks up the priority of element [x]. *)

  val priority: X.t -> t -> int

  (* [lowest s] returns [Some (x, p)], where element [x] has minimum
     priority [p] among all elements of [s]. It returns [None] if [s]
     is empty. *)

  val lowest: t -> (X.t * int) option

  (* [fold f s accu] fold over the set [s]. Elements are presented
     to [f] in increasing order of priority. *)

   val fold: (X.t -> int -> 'a -> 'a) -> t -> 'a -> 'a

end


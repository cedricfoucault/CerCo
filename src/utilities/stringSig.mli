
(** The module type declares functions to manipulate and create fresh
    strings. *)

module type S = sig

  type t = string

  val compare : t -> t -> int

  module Set : sig
    include Set.S with type elt = t
    val of_list : elt list -> t
    val unionl : t list -> t
  end

  module Map : Map.S with type key = t

  module Gen : sig
    type universe
    val fresh_prefix : Set.t -> string -> string
    val new_universe : string -> universe
    val fresh : universe -> string
  end

  val make_unique : Set.t -> (string -> string)
  val make_fresh  : Set.t -> string -> (unit -> string)

end

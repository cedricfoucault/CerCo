
(** Hardware registers. *)

type t

val eq : t -> t -> bool

module Set : Set.S

module Map : Map.S

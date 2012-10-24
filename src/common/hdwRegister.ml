
(** Hardware registers. *)

module OrdInt =
struct
  type t = int
  let compare = Pervasives.compare
end

type t = OrdInt.t
let eq r1 r2 = OrdInt.compare r1 r2 = 0

module Set = Set.Make(OrdInt)

module Map = Map.Make(OrdInt)

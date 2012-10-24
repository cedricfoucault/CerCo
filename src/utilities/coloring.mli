(* Pasted from Pottier's PP compiler *)

(** This module performs graph coloring. It is used for register
    allocation. *)

(* A coloring is a partial function of graph vertices to decisions,
   where a decision is of the form either [Spill] -- the vertex could
   not be colored and should be spilled into a stack slot -- or
   [Color] -- the vertex was assigned a hardware register. Vertices
   that are not in the domain of the coloring are waiting for a
   decision to be made. *)

type decision =
  | Spill
  | Color of Driver.TargetArch.register

type coloring =
    decision Interference.Vertex.Map.t

(* Here is the coloring algorithm. Out of an interference graph, it
   produces a coloring. The client should provide information about
   the number of uses of each pseudo-register; the higher the number,
   the more undesirable it is to spill that pseudo-register. If the
   [verbose] flag is set, the algorithm prints information messages to
   the standard output channel. *)

module Color (G : sig

  val graph: Interference.graph
  val uses: Register.t -> int
  val verbose: bool

end) : sig

  val coloring: coloring

end


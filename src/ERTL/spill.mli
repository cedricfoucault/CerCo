(* Pasted from Pottier's PP compiler *)

(** This module performs graph coloring with an unlimited number of
    colors and aggressive coalescing. It is used for assigning stack
    slots to the pseudo-registers that have been spilled by register
    allocation. *)

(* A coloring is a partial function of graph vertices to stack
   slots. Vertices that are not in the domain of the coloring are
   waiting for a decision to be made. *)

type decision =
    AST.immediate

type coloring =
    decision Interference.Vertex.Map.t

(* Here is the coloring algorithm. Out of an interference graph, it
   produces a coloring and reports how many colors (stack slots) were
   required. The graph is expected to contain interference and
   preferences edges between vertices only -- no hardware registers
   are involved. If the [verbose] flag is set, the algorithm prints
   information messages to the standard output channel. *)

module Color (G : sig

  val graph: Interference.graph
  val verbose: bool

end) : sig

  val coloring: coloring
  val locals: int

end


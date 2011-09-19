(* Pasted from Pottier's PP compiler *)

open Interference
(* open Integer *)
open Printf

(* ------------------------------------------------------------------------- *)
(* Colorings. *)

(* This module performs graph coloring with an unlimited number of
   colors and aggressive coalescing. It is used for assigning stack
   slots to the pseudo-registers that have been spilled by register
   allocation. *)

(* A coloring is a partial function of graph vertices to stack
   slots. Vertices that are not in the domain of the coloring are
   waiting for a decision to be made. *)

type decision =
    AST.immediate

type coloring =
    decision Vertex.Map.t

(* ------------------------------------------------------------------------- *)
(* Here is the coloring algorithm. *)

module Color (G : sig

  val graph: graph
  val verbose: bool

end) = struct

  module SlotSet =
    Set.Make(struct type t = int let compare = Pervasives.compare end)

  (* [forbidden_slots graph coloring v] is the set of stack slots that
     cannot be assigned to [v] considering the (partial) coloring
     [coloring]. This takes into account [v]'s possible interferences
     with other spilled vertices. *)

  let add_slot coloring r slots =
    SlotSet.add (Vertex.Map.find r coloring) slots

  let forbidden_slots graph coloring v =
    Vertex.Set.fold (add_slot coloring) (ipp graph v) SlotSet.empty

  (* [allocate_slot forbidden] returns a stack slot that is not a
     member of the set [forbidden]. Unlike hardware registers, stack
     slots are infinitely many, so it is always possible to allocate a
     new one. The reference [locals] holds the space that must be
     reserved on the stack for locals. *)

  let locals =
    ref 0

  let allocate_slot forbidden =
    let rec loop slot =
      if SlotSet.mem slot forbidden then
	loop (slot + Driver.TargetArch.int_size)
      else
	slot
    in
    let slot = loop 0 in
    locals := max (slot + Driver.TargetArch.int_size) !locals;
    slot

  (* Allocation is in two phases, implemented by [coalescing] and
     [simplification]. Each of these functions produces a coloring of its
     graph argument. *)

  (* [simplification] expects a graph that does not contain any preference
     edges. It picks a vertex [v], removes it, colors the remaining graph,
     then colors [v] using a color that is still available. Such a color must
     exist, since there is an unlimited number of colors. *)

  (* Following Appel, [v] is chosen with lowest degree: this will make this
     vertex easier to color and might (?) help use fewer colors. *)

  let rec simplification graph : coloring =

    match lowest graph with
    | Some (v, _) ->

	if G.verbose then
	  printf "SPILL: Picking vertex: %s.\n" (print_vertex graph v);

	(* Remove [v] from the graph and color what remains. *)

	let coloring = simplification (Interference.remove graph v) in

	(* Choose a color for [v]. *)

	let decision =
	  allocate_slot (forbidden_slots graph coloring v)
	in

	if G.verbose then
	  printf "SPILL: Decision concerning %s: offset %d.\n" (print_vertex graph v) decision;

	(* Record our decision and return. *)

	Vertex.Map.add v decision coloring

    | None ->

	(* The graph is empty. Return an empty coloring. *)

	Vertex.Map.empty

  (* [coalescing] looks for a preference edge, that is, for two vertices
     [x] and [y] such that [x] and [y] are move-related. In that case,
     [x] and [y] cannot interfere, because the [Interference] module
     does not allow two vertices to be related by both an interference
     edge and a preference edge. If [coalescing] finds such an edge, it
     coalesces [x] and [y] and continues coalescing. Otherwise, it
     invokes the next phase, [simplification].

     This is aggressive coalescing: we coalesce all preference edges,
     without fear of creating high-degree nodes. This is good because
     a move between two pseudo-registers that have been spilled in
     distinct stack slots is very expensive: one load followed by one
     store. *)

  let rec coalescing graph : coloring =

    match pppick graph (fun _ -> true) with
    | Some (x, y) ->

	if G.verbose then
	  printf "SPILL: Coalescing %s and %s.\n" (print_vertex graph x) (print_vertex graph y);

	let graph = Interference.coalesce graph x y in
	let coloring = coalescing graph in
	Vertex.Map.add x (Vertex.Map.find y coloring) coloring

    | None ->

	simplification graph

  (* Run the algorithm. [coalescing] runs first and calls [simplification]
     when it is done. *)

  let coloring =
    coalescing G.graph

  (* Report how much stack space was used. *)

  let locals =
    !locals

end

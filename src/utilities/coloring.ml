(* Pasted from Pottier's PP compiler *)

open ERTL
open Interference
open Printf

(* ------------------------------------------------------------------------- *)
(* Decisions. *)

(* A decision is of the form either [Spill] -- the vertex could
   not be colored and should be spilled into a stack slot -- or
   [Color] -- the vertex was assigned a hardware register. *)

type decision =
  | Spill
  | Color of Driver.TargetArch.register

(* [print_decision] turns a decision into a string. *)

let print_decision = function
  | Spill ->
      "spilled"
  | Color hwr ->
      Printf.sprintf "colored $%s" (Driver.TargetArch.print_register hwr)

(* ------------------------------------------------------------------------- *)
(* Colorings. *)

(* A coloring is a partial function of graph vertices to decisions.
   Vertices that are not in the domain of the coloring are waiting for
   a decision to be made. *)

type coloring =
    decision Vertex.Map.t

(* ------------------------------------------------------------------------- *)
(* Sets of colors. *)

module ColorSet =
  Driver.TargetArch.RegisterSet

(* [add_color coloring r colors] returns the union of the set [colors] with
   the element [color], if the vertex [r] was assigned color [color], and
   returns [colors] if [r] was spilled. *)

let add_color coloring r colors =
  match Vertex.Map.find r coloring with
  | Spill ->
      colors
  | Color color ->
      ColorSet.add color colors

(* These are the colors that we work with. *)

let colors : ColorSet.t =
  Driver.TargetArch.allocatable

(* This is the number of available colors. *)

let k : int =
  ColorSet.cardinal colors

(* ------------------------------------------------------------------------- *)
(* Choices of colors. *)

(* [forbidden_colors graph coloring v] is the set of colors that cannot be
   assigned to [v] considering [coloring], a coloring of every vertex in
   [graph] except [v]. *)
(* This takes into account [v]'s possible interferences with hardware
   registers, which are viewed as forbidden colors. *)

let forbidden_colors graph coloring v =
  Vertex.Set.fold (add_color coloring) (ipp graph v) (iph graph v)

(* ------------------------------------------------------------------------- *)
(* Low and high vertices. *)

(* A vertex is low (or insignificant) if its degree is less than [k].
   It is high (or significant) otherwise. *)

let high graph v =
  degree graph v >= k

(* [high_neighbors graph v] is the set of all high neighbors of [v]. *)

let high_neighbors graph v =
  Vertex.Set.filter (high graph) (ipp graph v)

(* ------------------------------------------------------------------------- *)
(* George's conservative coalescing criterion. *)

(* According to this criterion, two vertices [a] and [b] can be
   coalesced, suppressing [a] and keeping [b], if the following
   two conditions hold:

     1. (pseudo-registers) every high neighbor of [a] is a neighbor of [b];
     2. (hardware registers) every hardware register that interferes with
        [a] also interferes with [b].

   This means that, after all low vertices have been removed, any color that
   is suitable for [b] is also suitable for [a]. *)

let georgepp graph (a, b) =
  Vertex.Set.subset (high_neighbors graph a) (ipp graph b) &&
  Driver.TargetArch.RegisterSet.subset (iph graph a) (iph graph b)

(* According to this criterion, a vertex [a] and a hardware register
   [c] can be coalesced (that is, [a] can be assigned color [c]) if
   every high neighbor of [a] interferes with [c]. *)

let georgeph graph (a, c) =
  Vertex.Set.fold (fun neighbor accu ->
    accu &&
    Driver.TargetArch.RegisterSet.mem c (iph graph neighbor)
  ) (high_neighbors graph a) true

(* ------------------------------------------------------------------------- *)
(* Here is the coloring algorithm. *)

module Color (G : sig

  val graph: graph
  val uses: Register.t -> int
  val verbose: bool

end) = struct

  (* The cost function heuristically evaluates how much it might cost
     to spill vertex [v]. Here, the cost is the ratio of the number of
     uses of the pseudo-registers represented by [v] by the degree of
     [v]. One could also take into account the number of nested loops
     that the uses appear within, but that is not done here. *)

  let cost graph v =
    let uses =
      Register.Set.fold (fun r uses ->
	G.uses r + uses
      ) (registers graph v) 0
    in
    (float_of_int uses) /. (float_of_int (degree graph v))

  (* The algorithm maintains a transformed graph as it runs. It is
     obtained from the original graph by removing, coalescing, and
     freezing vertices. *)

  (* Each of the functions that follow returns a coloring of the graph
     that it is passed. These functions correspond to the various
     states of the algorithm (simplification, coalescing, freezing,
     spilling, selection). The function [simplification] is the
     initial state. *)

  (* [simplification] removes non-move-related nodes of low degree. *)

  let rec simplification graph : coloring =

    match lowest_non_move_related graph with

    | Some (v, d) when d < k ->

	(* We found a non-move-related node [v] of low degree. Color
	   the rest of the graph, then color [v]. This is what I call
	   selection. *)

	if G.verbose then
	  printf "Simplifying low vertex: %s.\n%!" (print_vertex graph v);

	selection graph v

    | _ ->

	(* There are no non-move-related nodes of low degree.
	   Could not simplify further. Start coalescing. *)

	coalescing graph

  (* [coalescing] looks for a preference edge that can be collapsed.
     It is called after [simplification], so it is known, at this
     point, that all nodes of low degree are move-related. *)

  and coalescing graph : coloring =

    (* Find a preference edge between two vertices that passes
       George's criterion.

       [pppick] examines all preference edges in the graph, so its use
       is inefficient. It would be more efficient instead to examine
       only areas of the graph that have changed recently. More
       precisely, it is useless to re-examine a preference edge that
       did not pass George's criterion the last time it was examined
       and whose neighborhood has not been modified by simplification,
       coalescing or freezing. Indeed, in that case, and with a
       sufficiently large definition of ``neighborhood'', this edge is
       guaranteed to again fail George's criterion. It would be
       possible to modify the [Interference.graph] data structure so
       as to keep track of which neighborhoods have been modified and
       provide a specialized, more efficient version of [pppick]. This
       is not done here. *)

    match pppick graph (georgepp graph) with

    | Some (a, b) ->

	if G.verbose then
	  printf "Coalescing %s with %s.\n%!"
	    (print_vertex graph a)
	    (print_vertex graph b);

	(* Coalesce [a] with [b] and color the remaining graph. *)

	let coloring = simplification (coalesce graph a b) in

	(* Assign [a] the same color as [b]. *)

	Vertex.Map.add a (Vertex.Map.find b coloring) coloring

    | None ->

	(* Find a preference edge between a vertex and a hardware
	   register that passes George's criterion. Like [pppick],
	   [phpick] is slow. *)

	match phpick graph (georgeph graph) with

	| Some (a, c) ->

	    if G.verbose then
	      printf "Coalescing %s with $%s.\n%!"
		(print_vertex graph a)
		(Driver.TargetArch.print_register c);

	    (* Coalesce [a] with [c] and color the remaining graph. *)

	    let coloring = simplification (coalesceh graph a c) in

	    (* Assign [a] the color [c]. *)

	    Vertex.Map.add a (Color c) coloring

	| None ->

	    (* Could not coalesce further. Start freezing. *)

	    freezing graph

  (* [freezing] begins after [simplification] and [coalescing] are
     finished, so it is known, at this point, that all nodes of low
     degree are move-related and no coalescing is possible. [freezing]
     looks for a node of low degree (which must be move-related) and
     removes the preference edges that it carries. This potentially
     opens new opportunities for simplification and coalescing. *)

  and freezing graph : coloring =

    match lowest graph with

    | Some (v, d) when d < k ->

	(* We found a move-related node [v] of low degree.
	   Freeze it and start over. *)

	if G.verbose then
	  printf "Freezing low vertex: %s.\n%!" (print_vertex graph v);

	simplification (freeze graph v)

    | _ ->

	(* Could not freeze further. Start spilling. *)

	spilling graph

  (* [spilling] begins after [simplification], [coalescing], and
     [freezing] are finished, so it is known, at this point, that
     there are no nodes of low degree.

     Thus, we are facing a potential spill. However, we do optimistic
     coloring: we do not spill a vertex right away, but proceed
     normally, just as if we were doing simplification. So, we pick a
     vertex [v], remove it, and check whether a color can be assigned
     to [v] only after coloring what remains of the graph.

     It is crucial to pick a vertex that has few uses in the code. It
     would also be good to pick one that has high degree, as this will
     help color the rest of the graph. Thus, we pick a vertex that has
     minimum cost, where the cost is obtained as the ratio of the
     number of uses of the pseudo-registers represented by this vertex
     in the code by the degree of the vertex. One could also take into
     account the number of nested loops that the uses appear within,
     but that is not done here.

     The use of [minimum] is inefficient, because this function
     examines all vertices in the graph. It would be possible to
     augment the [Interference.graph] data structure so as to keep
     track of the cost associated with each vertex and provide
     efficient access to a minimum cost vertex. This is not done
     here. *)

  and spilling graph : coloring =

    match minimum (cost graph) graph with
    | Some v ->
	
	if G.verbose then
	  printf "Spilling high vertex: %s.\n%!" (print_vertex graph v);
	
	selection graph v

    | None ->

	(* The graph is empty. Return an empty coloring. *)

	Vertex.Map.empty

  (* [selection] removes the vertex [v] from the graph, colors the
     remaining graph, then selects a color for [v].

     If [v] is low, that is, if [v] has degree less than [k], then at
     least one color must still be available for [v], regardless of
     how the remaining graph was colored.

     If [v] was a potential spill, then it is not certain that a color
     is still available. If one is, though, then we are rewarded for
     being optimistic. If none is, then [v] becomes an actual
     spill. *)

  and selection graph v : coloring =

    (* Remove [v] from the graph and color what remains. *)

    let coloring = simplification (remove graph v) in

    (* Determine which colors are allowed. *)

    let allowed = ColorSet.diff colors (forbidden_colors graph coloring v) in

    (* Make a decision.

       We pick a color randomly among those that are allowed. One could
       attempt to use biased coloring, that is, to pick a color that seems
       desirable (or not undesirable) according to the preference edges
       found in the initial graph. But that is probably not worth the
       trouble. *)

    let decision =
      try
	Color (ColorSet.choose allowed)
      with Not_found ->
	Spill
    in

    if G.verbose then
      printf "Decision concerning %s: %s.\n%!"
	(print_vertex graph v)
	(print_decision decision);

    (* Record our decision and return. *)

    Vertex.Map.add v decision coloring

  (* Run the algorithm. *)

  let coloring =
    simplification G.graph

end


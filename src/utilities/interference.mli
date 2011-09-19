(* Pasted from Pottier's PP compiler *)

(** This module implements a data structure for interference graphs.
    It provides functions that help construct, transform and inspect
    interference graphs. *)

(* Interference graphs record two kinds of edges: interference edges
   (``these two vertices cannot receive the same color'') and
   preference edges (``these two vertices should preferably receive
   the same color''). Furthermore, each kind of edge can relate either
   two pseudo-registers or one pseudo-register and one hardware
   register. Thus, an interference graph keeps track of four kinds of
   relationships.

   This module automatically maintains the invariant that two vertices
   [x] and [y] cannot be related by both an interference edge and a
   preference edge. When such a situation appears (for instance,
   because of coalescing), the preference edge is automatically
   removed. *)

type graph

(* The vertices of an interference graph initially correspond to
   pseudo-registers. However, interference graphs support coalescing,
   which means that a new graph can be constructed by coalescing two
   vertices in an existing graph. As a result, in general, the vertices
   of an interference graph correspond to sets of pseudo-registers. *)

(* ------------------------------------------------------------------------- *)

(* Operations over vertices: sets of vertices, maps over vertices. *)

module Vertex : sig

  type t

  (* The usual operations on sets, see [Set.S] in Objective Caml's
     documentation. *)

  module Set : Set.S with type elt = t

  (* The usual operations on maps, see [Map.S] in Objective Caml's
     documentation. One slight difference is that [find] expects
     the key to be present in the map -- it will fail otherwise. *)

  module Map : MyMap.S with type key = t

end

(* ------------------------------------------------------------------------- *)

(* Building interference graphs. *)

(* [create regs] creates an interference graph whose vertices are
   the pseudo-registers [regs] and that does not have any edges. *)

val create: Register.Set.t -> graph

(* [mki graph regs1 regs2] adds interference edges between all pairs
   of (pseudo- or hardware) registers [r1] and [r2], where [r1] ranges
   over [regs1], [r2] ranges over [regs2], and [r1] and [r2] are
   distinct. *)

val mki: graph ->
         Register.Set.t * Driver.TargetArch.RegisterSet.t ->
	 Register.Set.t * Driver.TargetArch.RegisterSet.t ->
	 graph

(* [mkiph graph regs hwregs] adds interference edges between all pairs
   of a pseudo-register [r] and a hardware register [hwr], where [r]
   ranges over [regs] and [hwr] ranges over [hwregs]. *)

val mkiph: graph -> Register.Set.t -> Driver.TargetArch.RegisterSet.t -> graph

(* [mkppp graph r1 r2] adds a preference edge between the
    pseudo-registers [r1] and [r2]. *)

val mkppp: graph -> Register.t -> Register.t -> graph

(* [mkpph graph r h] adds a preference edge between the
    pseudo-register [r] and the hardware register [h]. *)

val mkpph: graph -> Register.t -> Driver.TargetArch.register -> graph

(* ------------------------------------------------------------------------- *)

(* Transforming interference graphs. *)

(* [coalesce graph v1 v2] is a new graph where the vertices [v1] and
   [v2] are coalesced. [v1] and [v2] must not interfere. The new
   coalesced vertex is known under the name [v2]. *)

val coalesce: graph -> Vertex.t -> Vertex.t -> graph

(* [coalesceh graph v h] coalesces the vertex [v] with the hardware register
   [h]. This produces a new graph where [v] no longer exists and all edges
   leading to [v] are replaced with edges leading to [h]. *)

val coalesceh: graph -> Vertex.t -> Driver.TargetArch.register -> graph

(* [remove graph v] is a new graph where vertex [v] is removed. *)

val remove: graph -> Vertex.t -> graph

(* [freeze graph x] is a new graph where all preference edges carried
   by [x] are removed. *)

val freeze: graph -> Vertex.t -> graph

(* [restrict graph p] is a new graph where only those vertices that
   satisfy predicate [p] are kept. *)

val restrict: graph -> (Vertex.t -> bool) -> graph

(* [droph graph] is a new graph where all information concerning hardware
   registers has been dropped. *)

val droph: graph -> graph

(* ------------------------------------------------------------------------- *)

(* Inspecting interference graphs. *)

(* [lookup graph r] returns the graph vertex associated with
   pseudo-register [r]. *)

val lookup: graph -> Register.t -> Vertex.t

(* Conversely, [registers graph v] returns the set of pseudo-registers
   associated with vertex [v]. *)

val registers: graph -> Vertex.t -> Register.Set.t

(* [degree graph v] is the degree of the vertex [v], that is, the number
   of vertices and hardware registers that [v] interferes with. *)

val degree: graph -> Vertex.t -> int

(* [lowest graph] returns [Some (v, d)], where the vertex [v] has
   minimum degree [d], or returns [None] if the graph is empty. *)

val lowest: graph -> (Vertex.t * int) option

(* [lowest_non_move_related graph] returns [Some (v, d)], where the
   vertex [v] has minimum degree [d] among the vertices that are not
   move-related, or returns [None] if all vertices are move-related. A
   vertex is move-related if it carries a preference edge. *)

val lowest_non_move_related: graph -> (Vertex.t * int) option

(* [minimum f graph] returns a vertex [v] such that the value of [f x]
   is minimal. The values returned by [f] are compared using Objective
   Caml's generic comparison operator [<]. If the graph is empty,
   [None] is returned. *)

val minimum: (Vertex.t -> 'a) -> graph -> Vertex.t option

(* [fold f graph accu] folds over all vertices. *)

val fold: (Vertex.t -> 'a -> 'a) -> graph -> 'a -> 'a

(* [ipp graph v] is the set of vertices that the vertex [v] interferes
   with. *)

val ipp: graph -> Vertex.t -> Vertex.Set.t

(* [iph graph v] is the set of hardware registers that the vertex [v]
   interferes with. *)

val iph: graph -> Vertex.t -> Driver.TargetArch.RegisterSet.t

(* [ppp graph v] is the set of vertices that should preferably be
   assigned the same color as the vertex [v]. *)

val ppp: graph -> Vertex.t -> Vertex.Set.t

(* [pph graph v] is the set of hardware registers that [v] should
   preferably be assigned. *)

val pph: graph -> Vertex.t -> Driver.TargetArch.RegisterSet.t

(* [pppick graph p] returns an arbitrary preference edge that
   satisfies the predicate [p], if the graph contains one. *)

type ppedge =
    Vertex.t * Vertex.t

val pppick: graph -> (ppedge -> bool) -> ppedge option

(* [phpick graph p] returns an arbitrary preference edge that
   satisfies the predicate [p], if the graph contains one. *)

type phedge =
    Vertex.t * Driver.TargetArch.register

val phpick: graph -> (phedge -> bool) -> phedge option

(* ------------------------------------------------------------------------- *)

(* Displaying interference graphs. *)

(* [print_vertex graph v] produces a string representation of the
   vertex [v]. *)

val print_vertex: graph -> Vertex.t -> string

(* [print f graph] prints a representation of the interference graph
   [graph] in [dot] format to the output channel [f]. Interference
   edges are drawn as plain lines; preference edges are drawn as
   dotted lines. *)

val print: out_channel -> graph -> unit


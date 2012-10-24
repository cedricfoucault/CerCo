(* Pasted from Pottier's PP compiler *)

(* This module implements a data structure for interference graphs.
   It provides functions that help construct, transform and inspect
   interference graphs. *)

(* ------------------------------------------------------------------------- *)

(* Vertices are represented as integers. We need sets of vertices, maps over
   vertices, maps of vertices to nonempty sets of vertices, maps of vertices
   to nonempty sets of hardware registers, and priority sets over vertices. *)

module Vertex = struct

  module V = struct
    type t = int
    let compare = compare
  end

  include V
      
  module Set = Set.Make(V)

  module Map = MyMap.Make(V)

end

module VertexSetMap =
  SetMap.MakeHomo(Vertex.Set)(Vertex.Map)

module TargetArchRegisterSetMap =
  SetMap.MakeHetero(Driver.TargetArch.RegisterSet)(Vertex.Map)

module PrioritySet =
  PrioritySet.Make(Vertex)

(* ------------------------------------------------------------------------- *)

(* Each vertex maps to a set of pseudo-registers, which initially is a
   singleton set, but can grow due to coalescing. Conversely, each
   pseudo-register maps to a single vertex. *)

module RegMap : sig

  type t

  (* [empty] is the empty map. *)

  val empty: t

  (* [forward] maps a vertex to a set of pseudo-registers. *)

  val forward: Vertex.t -> t -> Register.Set.t

  (* [backward] maps a pseudo-register to a vertex. *)

  val backward: Register.t -> t -> Vertex.t

  (* [add r v m] adds a relation between pseudo-register [r] and
     vertex [v], both of which are assumed fresh. *)

  val add: Register.t -> Vertex.t -> t -> t

  (* [fold f m accu] folds over all vertices. *)

  val fold: (Vertex.t -> Register.Set.t -> 'a -> 'a) -> t -> 'a -> 'a

  (* [coalesce x y m] coalesces vertices [x] and [y]. Vertex [x] is
     removed and the pseudo-registers associated with it become
     associated with [y] instead. *)

  val coalesce: Vertex.t -> Vertex.t -> t -> t

  (* [remove x m] removes vertex [x]. The pseudo-registers associated
     with [x] disappear. *)

  val remove: Vertex.t -> t -> t

  (* [restrict] keeps only those vertices that satisfy predicate [p]. *)

  val restrict: (Vertex.t -> bool) -> t -> t

end = struct

  type t = {
      forward: Register.Set.t Vertex.Map.t;
      backward: Vertex.t Register.Map.t
    }

  let empty = {
    forward = Vertex.Map.empty;
    backward = Register.Map.empty
  }

  let forward v m =
    Vertex.Map.find v m.forward

  let backward r m =
    try
      Register.Map.find r m.backward
    with Not_found ->
      assert false (* bad pseudo-register *)

  let add r v m = {
    forward = Vertex.Map.add v (Register.Set.singleton r) m.forward;
    backward = Register.Map.add r v m.backward
  }

  let fold f m accu =
    Vertex.Map.fold f m.forward accu

  let coalesce x y m =
    let rx, forward = Vertex.Map.find_remove x m.forward in
    let forward = Vertex.Map.update y (Register.Set.union rx) forward in
    let backward =
      Register.Set.fold (fun r backward ->
        Register.Map.add r y backward
      ) rx m.backward
    in
    {
      forward = forward;
      backward = backward
    }

  let remove x m =
    let rx, forward = Vertex.Map.find_remove x m.forward in
    let backward = Register.Set.fold Register.Map.remove rx m.backward in
    {
      forward = forward;
      backward = backward
    }

  let restrict p m = {
    forward = Vertex.Map.restrict p m.forward;
    backward = Register.Map.restrict (fun r -> p (backward r m)) m.backward
  }

end

(* ------------------------------------------------------------------------- *)

(* Graphs. *)

type graph = {

    (* A two-way correspondence between vertices and pseudo-registers.
       This data structure is also used to keep a record of the set of
       all vertices. *)

    regmap: RegMap.t;

    (* Interference edges between two vertices: ``these two vertices
       cannot receive the same color''. *)

    ivv: VertexSetMap.t;

    (* Interference edges between a vertex and a hardware register:
       ``this vertex cannot receive this color''. *)

    ivh: TargetArchRegisterSetMap.t;

    (* Preference edges between two vertices: ``these two vertices
       should preferably receive the same color''. *)

    pvv: VertexSetMap.t;

    (* Preference edges between a vertex and a hardware register:
       ``this vertex should preferably receive this color''. *)

    pvh: TargetArchRegisterSetMap.t;

    (* The degree of each vertex [v], that is, the number of vertices
       and hardware registers that [v] interferes with, is recorded at
       all times. We use a ``priority set'' so as to be able to
       efficiently find a vertex of minimum degree. *)

    degree: PrioritySet.t;

    (* The degree of each *non-move-related* vertex [v]. This
	information is partially redundant with the [degree] field
	above. It is nevertheless required in order to be able to
	efficiently find a *non-move-related* vertex of minimum
	degree. *)

    nmr: PrioritySet.t;

  }

(* ------------------------------------------------------------------------- *)

(* Our graphs are made up of two subgraphs: the subgraph formed by the
   interference edges alone and the one formed by the preference edges
   alone.

   In order to allow more code sharing, we define functions that allow
   dealing with a single subgraph at a time. They provide operations
   such as inspecting the neighbors of a vertex, adding edges,
   removing edges, coalescing two vertices, removing a vertex, etc.

   We first define functions that deal with a ``generic'' subgraph,
   then (via inheritance) specialize them to deal with the
   interference subgraph and the preference subgraph with their
   specific features. *)

class virtual subgraph = object (self)

  (* These methods provide access to the fields of the [graph] data
     structure that define the subgraph of interest. All data is
     stored in the [graph] data structure. The object [self] has no
     state and holds no data. *)

  method virtual getvv: graph -> VertexSetMap.t
  method virtual setvv: graph -> VertexSetMap.t -> graph
  method virtual getvh: graph -> TargetArchRegisterSetMap.t
  method virtual setvh: graph -> TargetArchRegisterSetMap.t -> graph

  (* Accessing the neighbors of a vertex and testing whether edges
     exist. *)

  method neighborsv graph v =
    VertexSetMap.find v (self#getvv graph)

  method existsvv graph v1 v2 =
    Vertex.Set.mem v1 (self#neighborsv graph v2)

  method neighborsh graph v =
    TargetArchRegisterSetMap.find v (self#getvh graph)

  method existsvh graph v h =
    Driver.TargetArch.RegisterSet.mem h (self#neighborsh graph v)

  (* [degree graph v] is the degree of vertex [v] with respect to the
     subgraph. *)

  method degree graph v =
    Vertex.Set.cardinal (self#neighborsv graph v) + Driver.TargetArch.RegisterSet.cardinal (self#neighborsh graph v)

  (* [hwregs graph] is the set of all hardware registers mentioned in
     the subgraph. *)

  method hwregs graph =
   let union _ = Driver.TargetArch.RegisterSet.union in
   Vertex.Map.fold union (self#getvh graph) Driver.TargetArch.RegisterSet.empty

  (* [iter graph fvv fvh] iterates over all edges in the subgraph.
     Vertex-to-vertex edges are presented only once. *)

  method iter graph fvv fvh =
    Vertex.Map.iter (fun vertex neighbors ->
      Vertex.Set.iter (fun neighbor ->
	if vertex < neighbor then
	  fvv vertex neighbor
      ) neighbors
    ) (self#getvv graph);
    Vertex.Map.iter (fun vertex neighbors ->
      Driver.TargetArch.RegisterSet.iter (fun neighbor ->
	fvh vertex neighbor
      ) neighbors
    ) (self#getvh graph)

  (* [mkvv graph v1 v2] adds an edge between vertices [v1] and [v2]. *)

  method mkvv graph v1 v2 =
   if v1 = v2 then
     graph (* avoid creating self-edge *)
   else if self#existsvv graph v1 v2 then
     graph (* avoid re-adding an existing edge *)
   else
     self#mkvvi graph v1 v2

  method mkvvi graph v1 v2 =
     self#setvv graph (VertexSetMap.mkbiedge v1 v2 (self#getvv graph))

  (* [rmvv graph v1 v2] removes an edge between vertices [v1] and [v2].
     [rmvvifx] removes an edge if it exists. *)

  method rmvv graph v1 v2 =
    assert (self#existsvv graph v1 v2);
    self#setvv graph (VertexSetMap.rmbiedge v1 v2 (self#getvv graph))

  method rmvvifx graph v1 v2 =
    if self#existsvv graph v1 v2 then
      self#rmvv graph v1 v2
    else
      graph

  (* [mkvh graph v h] adds an edge between vertex [v] and hardware
     register [h]. *)

  method mkvh graph v h =
    if self#existsvh graph v h then
      graph (* avoid re-adding an existing edge *)
    else
      self#mkvhi graph v h

  method mkvhi graph v h =
     self#setvh graph (TargetArchRegisterSetMap.update v (Driver.TargetArch.RegisterSet.add h) (self#getvh graph))

  (* [rmvh v h] removes an edge between vertex [v] and hardware
     register [h]. [rmvhifx] removes an edge if it exists. *)

  method rmvh graph v h =
    assert (self#existsvh graph v h);
    self#setvh graph (TargetArchRegisterSetMap.update v (Driver.TargetArch.RegisterSet.remove h) (self#getvh graph))

  method rmvhifx graph v h =
    if self#existsvh graph v h then
      self#rmvh graph v h
    else
      graph

  (* [coalesce graph x y] turns every neighbor [w] or [h] of [x] into
      a neighbor of [y] instead. [w] ranges over both vertices and
      hardware registers. *)

  method coalesce graph x y =
    let graph =
      Vertex.Set.fold (fun w graph ->
	self#mkvv (self#rmvv graph x w) y w
      ) (self#neighborsv graph x) graph
    in
    let graph =
      Driver.TargetArch.RegisterSet.fold (fun h graph ->
	self#mkvh (self#rmvh graph x h) y h
      ) (self#neighborsh graph x) graph
    in
    graph

  (* [coalesceh graph x h] turns every neighbor [w] of [x] into a
      neighbor of [h] instead. [w] ranges over both vertices and
      hardware registers. Edges between two hardware registers are not
      recorded. *)

  method coalesceh graph x h =
    let graph =
      Vertex.Set.fold (fun w graph ->
	self#mkvh (self#rmvv graph x w) w h
      ) (self#neighborsv graph x) graph
    in
    let graph =
      Driver.TargetArch.RegisterSet.fold (fun k graph ->
	self#rmvh graph x k
      ) (self#neighborsh graph x) graph
    in
    graph

  (* [remove graph x] removes all edges carried by vertex [x]. *)

  method remove graph x =
    let graph =
      Vertex.Set.fold (fun w graph ->
	self#rmvv graph x w
      ) (self#neighborsv graph x) graph
    in
    let graph =
      Driver.TargetArch.RegisterSet.fold (fun h graph ->
	self#rmvh graph x h
      ) (self#neighborsh graph x) graph
    in
    graph

end

(* ------------------------------------------------------------------------- *)

(* The interference subgraph.

   This is a subgraph with the following specific features: (1) the
   degree of every vertex is recorded in the [degree] field of the
   [graph] data structure; (2) the degree of every non-move-related
   vertex is recorded in the [nmr] field of the [graph] data
   structure; (3) creating an edge in the interference subgraph
   automatically destroys a corresponding edge in the preference
   subgraph. *)

class interference (preference : preference Lazy.t) = object (self)

  inherit subgraph as super

  method getvv graph = graph.ivv
  method setvv graph m = { graph with ivv = m }
  method getvh graph = graph.ivh
  method setvh graph m = { graph with ivh = m }

  (* Override the edge creation and destruction methods. *)

  method mkvvi graph v1 v2 =
    let graph = super#mkvvi graph v1 v2 in
    let graph = (Lazy.force preference)#rmvvifx graph v1 v2 in (* do not constrain an existing preference edge *)
    { graph with
      degree = PrioritySet.increment v1 1 (PrioritySet.increment v2 1 graph.degree);
      nmr = PrioritySet.incrementifx v1 1 (PrioritySet.incrementifx v2 1 graph.nmr);
    }

  method rmvv graph v1 v2 =
    let graph = super#rmvv graph v1 v2 in
    { graph with
      degree = PrioritySet.increment v1 (-1) (PrioritySet.increment v2 (-1) graph.degree);
      nmr = PrioritySet.incrementifx v1 (-1) (PrioritySet.incrementifx v2 (-1) graph.nmr);
    }

  method mkvhi graph v h =
    let graph = super#mkvhi graph v h in
    let graph = (Lazy.force preference)#rmvhifx graph v h in (* do not constrain an existing preference edge *)
    { graph with
      degree = PrioritySet.increment v 1 graph.degree;
      nmr = PrioritySet.incrementifx v 1 graph.nmr;
    }

  method rmvh graph v h =
    let graph = super#rmvh graph v h in
    { graph with
      degree = PrioritySet.increment v (-1) graph.degree;
      nmr = PrioritySet.incrementifx v (-1) graph.nmr;
    }

end

(* ------------------------------------------------------------------------- *)

(* The preference subgraph.

   This is a subgraph with the following specific features: (1) an
   edge in the preference subgraph cannot be created if a
   corresponding edge exists in the interference subgraph; (2) adding
   an edge can make a vertex move-related, which requires taking that
   vertex out of the [nmr] set; conversely, removing an edge can make
   a vertex non-move-related, which requires adding that vertex to the
   [nmr] set. *)

and preference (interference : interference Lazy.t) = object (self)

  inherit subgraph as super

  method getvv graph = graph.pvv
  method setvv graph m = { graph with pvv = m }
  method getvh graph = graph.pvh
  method setvh graph m = { graph with pvh = m }

  (* [nmr graph v] tells whether vertex [v] is non-move-related. *)

  method nmr graph v =
    Vertex.Set.is_empty (self#neighborsv graph v) &&
    Driver.TargetArch.RegisterSet.is_empty (self#neighborsh graph v)

  (* [mkcheck graph v] moves [v] out of the [nmr] set if [v] is
     non-move-related. *)

  method mkcheck graph v =
   if self#nmr graph v then
     { graph with
       nmr = PrioritySet.remove v graph.nmr }
   else
     graph

  (* Override the edge creation methods. *)

  method mkvvi graph v1 v2 =
    if (Lazy.force interference)#existsvv graph v1 v2 then
      graph (* avoid creating constrained preference edge *)
    else 
      let graph = self#mkcheck graph v1 in
      let graph = self#mkcheck graph v2 in
      super#mkvvi graph v1 v2

  method mkvhi graph v h =
    if (Lazy.force interference)#existsvh graph v h then
      graph (* avoid creating constrained preference edge *)
    else
      let graph = self#mkcheck graph v in
      super#mkvhi graph v h

  (* [rmcheck graph v] moves [v] into the [nmr] set if [v] is
     non-move-related. *)
	
  method rmcheck graph v =
    if self#nmr graph v then
      { graph with
	nmr = PrioritySet.add v (PrioritySet.priority v graph.degree) graph.nmr
      }
    else
      graph

  (* Override the edge destruction methods. *)

  method rmvv graph v1 v2 =
    let graph = super#rmvv graph v1 v2 in
    let graph = self#rmcheck graph v1 in
    let graph = self#rmcheck graph v2 in
    graph

  method rmvh graph v h =
    let graph = super#rmvh graph v h in
    let graph = self#rmcheck graph v in
    graph

end

(* ------------------------------------------------------------------------- *)

(* Because the interference and preference subgraphs are mutually
   referential, a recursive definition is required. It is made
   somewhat inelegant by Objective Caml's insistence on using the
   [Lazy] mechanism. *)

let rec interference = lazy (new interference preference)
    and preference   = lazy (new preference interference)
let interference     = Lazy.force interference
let preference       = Lazy.force preference

(* ------------------------------------------------------------------------- *)

(* Inspecting interference graphs. *)

(* [ipp graph v] is the set of vertices that the vertex [v] interferes
   with. *)

let ipp graph v =
  interference#neighborsv graph v

(* [iph graph v] is the set of hardware registers that the vertex [v]
   interferes with. *)

let iph graph v =
  interference#neighborsh graph v

(* [ppp graph v] is the set of vertices that should preferably be
   assigned the same color as the vertex [v]. *)

let ppp graph v =
  preference#neighborsv graph v

(* [pph graph v] is the set of hardware registers that [v] should
   preferably be assigned. *)

let pph graph v =
  preference#neighborsh graph v

(* [degree graph v] is the degree of the vertex [v], that is, the number
   of vertices and hardware registers that [v] interferes with. *)

let degree graph v =
  PrioritySet.priority v graph.degree

(* [lowest graph] returns [Some (v, d)], where the vertex [v] has
   minimum degree [d], or returns [None] if the graph is empty. *)

let lowest graph =
  PrioritySet.lowest graph.degree

(* [lowest_non_move_related graph] returns [Some (v, d)], where the
   vertex [v] has minimum degree [d] among the vertices that are not
   move-related, or returns [None] if all vertices are move-related. A
   vertex is move-related if it carries a preference edge. *)

let lowest_non_move_related graph =
  PrioritySet.lowest graph.nmr

(* [fold f graph accu] folds over all vertices. *)

let fold f graph accu =
  RegMap.fold (fun v _ accu -> f v accu) graph.regmap accu

(* [minimum f graph] returns a vertex [v] such that the value of [f x]
   is minimal. The values returned by [f] are compared using Objective
   Caml's generic comparison operator [<]. If the graph is empty,
   [None] is returned. *)

let minimum f graph =
  match
    fold (fun w accu ->
      let dw = f w in
      match accu with
      | None ->
	  Some (dw, w)
      | Some (dv, v) ->
	  if dw < dv then
	    Some (dw, w)
	  else
	    accu
    ) graph None
  with
  | None ->
      None
  | Some (_, v) ->
      Some v

(* [pppick graph p] returns an arbitrary preference edge that
   satisfies the predicate [p], if the graph contains one. *)

type ppedge =
    Vertex.t * Vertex.t

let pppick graph p =
  VertexSetMap.pick graph.pvv p

(* [phpick graph p] returns an arbitrary preference edge that
   satisfies the predicate [p], if the graph contains one. *)

type phedge =
    Vertex.t * Driver.TargetArch.register

let phpick graph p =
  TargetArchRegisterSetMap.pick graph.pvh p

(* ------------------------------------------------------------------------- *)

(* Constructing interference graphs. *)

(* [create regs] creates an interference graph whose vertices are
   the pseudo-registers [regs] and that does not have any edges. *)

let create regs =
  let (_ : int), regmap, degree =
    Register.Set.fold (fun r (v, regmap, degree) ->
      v+1,
      RegMap.add r v regmap,
      PrioritySet.add v 0 degree
    ) regs (0, RegMap.empty, PrioritySet.empty)
  in
  {
    regmap = regmap;
    ivv = Vertex.Map.empty;
    ivh = Vertex.Map.empty;
    pvv = Vertex.Map.empty;
    pvh = Vertex.Map.empty;
    degree = degree;
    nmr = degree
  }

(* [lookup graph r] returns the graph vertex associated with
   pseudo-register [r]. *)

let lookup graph r =
  RegMap.backward r graph.regmap

(* Conversely, [registers graph v] returns the set of pseudo-registers
   associated with vertex [v]. *)

let registers graph v =
  RegMap.forward v graph.regmap

(* [mkipp graph regs1 regs2] adds interference edges between all pairs
   of pseudo-registers [r1] and [r2], where [r1] ranges over [regs1],
   [r2] ranges over [regs2], and [r1] and [r2] are distinct. *)

let mkipp graph regs1 regs2 =
  Register.Set.fold (fun r1 graph ->
    let v1 = lookup graph r1 in
    Register.Set.fold (fun r2 graph ->
      interference#mkvv graph v1 (lookup graph r2)
    ) regs2 graph
  ) regs1 graph

(* [mkiph graph regs hwregs] adds interference edges between all pairs
   of a pseudo-register [r] and a hardware register [hwr], where [r]
   ranges over [regs] and [hwr] ranges over [hwregs]. *)

let mkiph graph regs hwregs =
  Register.Set.fold (fun r graph ->
    let v = lookup graph r in
    Driver.TargetArch.RegisterSet.fold (fun h graph ->
      interference#mkvh graph v h
    ) hwregs graph
  ) regs graph

(* [mki graph regs1 regs2] adds interference edges between all pairs
   of (pseudo- or hardware) registers [r1] and [r2], where [r1] ranges
   over [regs1], [r2] ranges over [regs2], and [r1] and [r2] are
   distinct. *)

let mki graph (regs1, hwregs1) (regs2, hwregs2) =
  let graph = mkipp graph regs1 regs2 in
  let graph = mkiph graph regs1 hwregs2 in
  let graph = mkiph graph regs2 hwregs1 in
  graph

(* [mkppp graph r1 r2] adds a preference edge between the
    pseudo-registers [r1] and [r2]. *)

let mkppp graph r1 r2 =
  let v1 = lookup graph r1
  and v2 = lookup graph r2 in
  let graph = preference#mkvv graph v1 v2 in
  graph

(* [mkpph graph r h] adds a preference edge between the
    pseudo-register [r] and the hardware register [h]. *)

let mkpph graph r h =
  let v = lookup graph r in
  let graph = preference#mkvh graph v h in
  graph

(* ------------------------------------------------------------------------- *)

(* Displaying interference graphs. *)

open Printf

let hwregs graph =
  Driver.TargetArch.RegisterSet.union (interference#hwregs graph) (preference#hwregs graph)

let print_vertex graph v =
  Register.Set.print (registers graph v)

let print f graph =

  fprintf f "graph G {\n";
(*  fprintf f "size=\"6, 3\";\n"; (* in inches *)*)
  fprintf f "orientation = landscape;\n";
  fprintf f "rankdir = LR;\n";
  fprintf f "ratio = compress;\n\n"; (* compress or fill or auto *)
  
  RegMap.fold (fun vertex regs () ->
    fprintf f "r%d [ label=\"%s\" ] ;\n" vertex (Register.Set.print regs)
  ) graph.regmap ();

  Driver.TargetArch.RegisterSet.iter (fun hwr ->
    let name = Driver.TargetArch.print_register hwr in
    fprintf f "hwr%s [ label=\"$%s\" ] ;\n" name name
  ) (hwregs graph);

  interference#iter graph
    (fun vertex neighbor ->
      fprintf f "r%d -- r%d ;\n" vertex neighbor)
    (fun vertex neighbor ->
      fprintf f "r%d -- hwr%s ;\n" vertex (Driver.TargetArch.print_register neighbor));

  preference#iter graph
    (fun vertex neighbor ->
      fprintf f "r%d -- r%d [ style = dashed ] ;\n" vertex neighbor)
    (fun vertex neighbor ->
      fprintf f "r%d -- hwr%s [ style = dashed ] ;\n" vertex (Driver.TargetArch.print_register neighbor));

  fprintf f "\n}\n"

(* ------------------------------------------------------------------------- *)

(* Coalescing. *)

(* [coalesce graph v1 v2] is a new graph where the vertices [v1] and [v2]
   are coalesced. The new coalesced vertex is known under the name [v2]. *)

let coalesce graph x y =

  assert (x <> y); (* attempt to coalesce one vertex with itself *)
  assert (not (interference#existsvv graph x y)); (* attempt to coalesce two interfering vertices *)

  (* Perform coalescing in the two subgraphs. *)

  let graph = interference#coalesce graph x y in
  let graph = preference#coalesce graph x y in

  (* Remove [x] from all tables. *)

  {
    graph with
    regmap = RegMap.coalesce x y graph.regmap;
    ivh = Vertex.Map.remove x graph.ivh;
    pvh = Vertex.Map.remove x graph.pvh;
    degree = PrioritySet.remove x graph.degree;
    nmr = PrioritySet.remove x graph.nmr;
  }

(* [coalesceh graph v h] coalesces the vertex [v] with the hardware register
   [h]. This produces a new graph where [v] no longer exists and all edges
   leading to [v] are replaced with edges leading to [h]. *)

let coalesceh graph x h =

  assert (not (interference#existsvh graph x h)); (* attempt to coalesce interfering entities *)

  (* Perform coalescing in the two subgraphs. *)

  let graph = interference#coalesceh graph x h in
  let graph = preference#coalesceh graph x h in

  (* Remove [x] from all tables. *)

  {
    graph with
    regmap = RegMap.remove x graph.regmap;
    ivh = Vertex.Map.remove x graph.ivh;
    pvh = Vertex.Map.remove x graph.pvh;
    degree = PrioritySet.remove x graph.degree;
    nmr = PrioritySet.remove x graph.nmr;
  }

(* ------------------------------------------------------------------------- *)

(* [freeze graph x] is a new graph where all preference edges carried
   by [x] are removed. *)

let freeze graph x =
  preference#remove graph x

(* ------------------------------------------------------------------------- *)

(* Removal. *)

(* [remove graph v] is a new graph where vertex [v] is removed. *)

let remove graph v =

  (* Remove all edges carried by [v]. *)

  let graph = interference#remove graph v in
  let graph = preference#remove graph v in

  (* Remove [v] from all tables. *)

  {
    graph with
    regmap = RegMap.remove v graph.regmap;
    degree = PrioritySet.remove v graph.degree;
    nmr = PrioritySet.remove v graph.nmr;
  }

(* ------------------------------------------------------------------------- *)

(* [mkdeg graph] recomputes degree information from scratch. *)

let mkdeg graph =
  let degree, nmr =
    fold (fun v (degree, nmr) ->
      let d = interference#degree graph v in
      PrioritySet.add v d degree,
      if preference#nmr graph v then PrioritySet.add v d nmr else nmr
      ) graph (PrioritySet.empty, PrioritySet.empty)
  in
  { graph with
    degree = degree;
    nmr = nmr;
  }

(* [restrict graph p] is a new graph where only those vertices that
   satisfy predicate [p] are kept. The same effect could be obtained
   by repeated application of [remove], but [restrict] is likely to be
   more efficient if many vertices are removed. *)

let restrict graph p =
  mkdeg {
    graph with
    regmap = RegMap.restrict p graph.regmap;
    ivv = VertexSetMap.restrict p graph.ivv;
    ivh = Vertex.Map.restrict p graph.ivh;
    pvv = VertexSetMap.restrict p graph.pvv;
    pvh = Vertex.Map.restrict p graph.pvh;
  }

(* [droph graph] is a new graph where all information concerning hardware
   registers has been dropped. *)

let droph graph =
  mkdeg {
    graph with
    ivh = Vertex.Map.empty;
    pvh = Vertex.Map.empty;
  }


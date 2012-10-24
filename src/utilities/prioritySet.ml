(* Pasted from Pottier's PP compiler *)

(* This module offers sets of elements where each element carries an
   integer priority. All operations execute in logarithmic time with
   respect to the number of elements in the set. *)

module Make (X : Set.OrderedType)
= struct

  (* First, define normal sets and maps. *)

  module Set = Set.Make(X)

  module Map = MyMap.Make(X)

  (* Next, define maps of integers to nonempty sets of elements. *)

  module IntMap = struct

    module M = MyMap.Make (struct
      type t = int
      let compare = compare
    end)
 
    include M

    module H = SetMap.MakeHetero(Set)(M)

    let update = H.update

  end

  (* Now, define priority sets. *)

  type t = {

      (* A mapping of elements to priorities. *)

      priority: int Map.t;

      (* A mapping of priorities to sets of elements. By convention, a
	 priority has no entry in this table if that entry would be an
	 empty set of elements. This allows finding the
	 lowest-priority element in logarithmic time. *)

      level: Set.t IntMap.t

    }

  (* [empty] is the empty set. *)

  let empty =
    {
      priority = Map.empty;
      level = IntMap.empty
    }

  (* [priority x s] looks up the priority of element [x]. *)

  let priority x s =
    try
      Map.find x s.priority
    with Not_found ->
      assert false

  (* [add x p s] inserts element [x] with priority [p]. *)

  let add x p s =
    assert (not (Map.mem x s.priority));
    {
      priority = Map.add x p s.priority;
      level = IntMap.update p (Set.add x) s.level
    }

  (* [remove x s] removes element [x]. *)

  let remove x s =
    let p, priority =
      try
	Map.find_remove x s.priority
      with Not_found ->
	assert false 
    in
    let level =
      IntMap.update p (function xs ->
	assert (Set.mem x xs);
	Set.remove x xs
      ) s.level
    in
    { 
      priority = priority;
      level = level
    }

  (* [change x p s] changes the priority of element [x] to [p]. *)

  let change x p1 s =
    let p0 = priority x s in
    if p0 = p1 then
      s
    else
      {
        priority = Map.add x p1 s.priority; (* overriding previous entry *)
        level = IntMap.update p1 (Set.add x) (IntMap.update p0 (Set.remove x) s.level)
      }

  (* [increment x d s] increases the priority of element [x] by [d]. *)

  let increment x d s =
    change x (priority x s + d) s

  (* [incrementifx x p s] increases the priority of element [x] by [d]
     if [x] is a member of the priority set. *)

  let incrementifx x d s =
    if Map.mem x s.priority then
      increment x d s
    else
      s

  (* [lowest s] returns [Some (x, p)], where element [x] has minimum
     priority [p] among all elements of [s]. It returns [None] if [s]
     is empty. *)

  let lowest s =
    try
      let p, xs = IntMap.minimum s.level in (* can fail if set is empty *)
      try
	Some (Set.choose xs, p) (* cannot fail *)
      with Not_found ->
	assert false
    with Not_found ->
      None

  (* [fold f s accu] fold over the set [s]. Elements are presented
     to [f] in increasing order of priority. *)

  let fold f s accu =
    IntMap.fold (fun p xs accu ->
      Set.fold (fun x accu ->
	f x p accu
      ) xs accu
    ) s.level accu

end


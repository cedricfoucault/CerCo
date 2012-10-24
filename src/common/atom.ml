(* Adapted from Pottier's PP compiler *)

(* A universe is a record, whose address defines the identity of the
   universe. The integer counter [next] holds the number of the next
   fresh atom. The [name] field holds the name of the universe. *)

type universe = {
    mutable next: int;
    name: string
  }

let new_universe name = {
  next = 0;
  name = name
}

(* An atom is a pair of a universe and an integer. The latter defines
   the atom's identity within its universe. *)

type t =
   universe * int

let fresh u =
   let id = u.next in
   u.next <- id + 1;
   u, id

let equal (u1, id1) (u2, id2) =
  assert (u1 == u2);
  ((id1 : int) = (id2 : int))

let compare (u1, id1) (u2, id2) =
  assert (u1 == u2);
  compare (id1 : int) (id2 : int)

let flex_compare (u1, id1) (u2, id2) =
  if String.compare u1.name u2.name = 0 then Pervasives.compare id1 id2
  else String.compare u1.name u2.name

let flex_equal l1 l2 = flex_compare l1 l2 = 0

let universe_of (u, _) = u

let same_universe (u1, _) (u2, _) = u1 = u2

let ends_with_a_digit s =
  let n = String.length s in
  n > 0 && s.[n-1] >= '0' && s.[n-1] <= '9'

(* This function is injective, that is, [u] and [id] can be recovered
   out of [print (u, id)]. *)

let print (u, id) =
  Printf.sprintf "%s%s%d" u.name (if ends_with_a_digit u.name then "_" else "") id

(* Added by Nicolas Ayache: more flexible maps and sets where atoms
   from different universes can coexist. *)

type label_t = t

module OrderedLabel = struct
  type t = label_t
  let compare = flex_compare
end

module FlexSet = Set.Make (OrderedLabel)

module FlexMap = Map.Make (OrderedLabel)


module OrderedInt = struct
  type t = int
  let compare x1 x2 = x1 - x2
end

(* We internally rely upon Objective Caml's integer sets and maps. *)

module ISet = Set.Make (OrderedInt)
module IMap = Map.Make (OrderedInt)

(* Sets. *)

module Set = struct

  (* A set is either empty or a pair of a universe and a (possibly
     empty) internal set. The fact that we do not require the empty
     set to be explicitly associated with a universe means that
     [empty] can be a constant, as opposed to an operation that
     expects a universe as a parameter. *)

  type elt =
      t

  type t =
    | E
    | U of universe * ISet.t

  let empty =
    E

  let is_empty = function
    | E ->
	true
    | U (_, s) ->
	ISet.is_empty s

  let mem (u1, x) = function
    | E ->
	false
    | U (u2, s) ->
	assert (u1 == u2);
	(u1 = u2) && (ISet.mem x s)

  let add (u1, x) = function
    | E ->
	U (u1, ISet.singleton x)
    | U (u2, s) ->
	assert (u1 == u2);
	U (u1, ISet.add x s)

  let remove (u1, x) = function
    | E ->
	E
    | U (u2, s) ->
	assert (u1 == u2);
	(* set can become empty but retains its universe *)
	U (u1, ISet.remove x s)

  let singleton x =
    add x empty

  let couple x1 x2 =
    add x1 (singleton x2)

  let of_list xs =
    List.fold_right add xs empty

  let union s1 s2 =
    match s1, s2 with
    | E, s
    | s, E ->
	s
    | U (u1, s1), U (u2, s2) ->
	assert (u1 == u2);
	U (u1, ISet.union s1 s2)

  let inter s1 s2 =
    match s1, s2 with
    | E, s
    | s, E ->
	E
    | U (u1, s1), U (u2, s2) ->
	assert (u1 == u2);
	U (u1, ISet.inter s1 s2)

  let disjoint s1 s2 =
    is_empty (inter s1 s2)

  let diff s1 s2 =
    match s1, s2 with
    | E, _ ->
	E
    | s, E ->
	s
    | U (u1, s1), U (u2, s2) ->
	assert (u1 == u2);
	U (u1, ISet.diff s1 s2)

  let iter f = function
    | E ->
	()
    | U (u, s) ->
	ISet.iter (fun x -> f (u, x)) s

  let fold f s accu =
    match s with
    | E ->
	accu
    | U (u, s) ->
	ISet.fold (fun x accu -> f (u, x) accu) s accu

  let choose = function
    | E ->
	raise Not_found
    | U (u, s) ->
	u, ISet.choose s

  let equal s1 s2 =
    match s1, s2 with
    | E, s
    | s, E ->
	is_empty s
    | U (u1, s1), U (u2, s2) ->
	assert (u1 == u2);
	ISet.equal s1 s2

  let cardinal = function
    | E ->
	0
    | U (_, s) ->
	ISet.cardinal s

  let elements = function
    | E ->
	[]
    | U (u, s) ->
	List.map (fun x -> (u, x)) (ISet.elements s)

  let filter p = function
    | E ->
	E
    | U (u, s) ->
	U (u, ISet.filter (fun x -> p (u, x)) s)

  let pick s =
    let x = choose s in
    let s = remove x s in
    x, s

  let rec exhaust s accu f =
    if is_empty s then
      accu
    else
      let x, s = pick s in
      let s', accu = f x accu in
      exhaust (union s s') accu f

  open Print

  let print s =
    seplist comma (fun () x -> print x) () (elements s)

end

(* Maps. *)

module Map = struct

  (* A map is either empty or a pair of a universe and a (possibly
     empty) internal map. The fact that we do not require the empty
     map to be explicitly associated with a universe means that
     [empty] can be a constant, as opposed to an operation that
     expects a universe as a parameter. *)

  type key =
      t

  type 'a t =
    | E
    | U of universe * 'a IMap.t

  let empty =
    E

  let is_empty = function
    | E ->
	true
    | U (_, m) ->
	IMap.is_empty m

  let mem (u1, x) = function
    | E ->
	false
    | U (u2, m) ->
	assert (u1 == u2);
	IMap.mem x m

  let add (u1, x) d = function
    | E ->
	U (u1, IMap.add x d IMap.empty)
    | U (u2, m) ->
	assert (u1 == u2);
	U (u1, IMap.add x d m)

  let remove (u1, x) = function
    | E ->
	E
    | U (u2, m) ->
	assert (u1 == u2);
	U (u1, IMap.remove x m)

  let singleton x d =
    add x d empty

  let find (u1, x) = function
    | E ->
	raise Not_found
    | U (u2, m) ->
	assert (u1 == u2);
	IMap.find x m

  let iter f = function
    | E ->
	()
    | U (u, m) ->
	IMap.iter (fun x d -> f (u, x) d) m

  let fold f m accu =
    match m with
    | E ->
	accu
    | U (u, m) ->
	IMap.fold (fun x d accu -> f (u, x) d accu) m accu

  let map f = function
    | E ->
	E
    | U (u, m) ->
	U (u, IMap.map f m)

  let mapi f = function
    | E ->
	E
    | U (u, m) ->
	U (u, IMap.mapi (fun x d -> f (u, x) d) m)

  let domain = function
    | E ->
	Set.E
    | U (u, m) ->
	Set.U (u, IMap.fold (fun x _ s ->
	            ISet.add x s
	          ) m ISet.empty
              )

  let lift f = function
    | Set.E ->
	E
    | Set.U (u, s) ->
	U (u, ISet.fold (fun x m ->
                IMap.add x (f (u, x)) m
              ) s IMap.empty
          )


  let generator u =
    let m = ref empty in
    let generate d =
      let label = fresh u in
      m := add label d !m;
      label
    in
    m, generate

  let addm m1 m2 =
    fold add m1 m2

  let restrict p m =
    fold (fun x d m ->
      if p x then
	add x d m
      else
	m
    ) m empty

end

(* An imperative interface to maps. *)

module ImperativeMap = struct

  type key =
      Map.key
  
  type 'data t =
      'data Map.t ref
      
  let create () =
    ref Map.empty

  let clear t =
    t := Map.empty
    
  let add k d t =
    t := Map.add k d !t

  let find k t =
    Map.find k !t

  let iter f t =
    Map.iter f !t

end

(* Maps of atoms to sets of atoms. *)

module SetMap = SetMap.MakeHomo(Set)(Map)

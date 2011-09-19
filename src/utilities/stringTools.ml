
(** FIXME: document here *)

type t = string

let compare = String.compare


module Set = struct 
  include Set.Make (String)
  let of_list l =
    let f res e = add e res in
    List.fold_left f empty l
  let unionl l = List.fold_left union empty l
end


module Map = Map.Make (String)


module Gen = struct

  type universe = { prefix : string ; mutable next : int }

  let added_suffix = "_"

  let is_prefix prefix s =
    let l = String.length prefix in
    (String.length s >= l) && (String.sub s 0 l = prefix)

  let has_prefix set prefix =
    let f s b = b || (is_prefix prefix s) in
    Set.fold f set false

  let fresh_prefix set prefix =
    let rec aux prefix =
      if has_prefix set prefix then aux (prefix ^ added_suffix)
      else prefix in
    aux prefix


  let new_universe s = { prefix = s ; next = 0 }

  let fresh u =
    let s = u.prefix ^ (string_of_int u.next) in
    u.next <- u.next + 1 ;
    s

end

let make_unique set =
  let set_ref = ref set in
  let unique s =
    let res = Gen.fresh_prefix !set_ref s in
    set_ref := Set.add res !set_ref ;
    res in
  unique

let make_fresh set prefix =
  let fresh_prefix = Gen.fresh_prefix set prefix in
  let universe = Gen.new_universe fresh_prefix in
  (fun () -> Gen.fresh universe)

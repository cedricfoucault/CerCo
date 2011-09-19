
let rec map3 f al bl cl =
  let f' ((a, b), c) = f a b c in
  List.map f' (List.combine (List.combine al bl) cl)

let rec max_list = function
  | [] -> raise (Invalid_argument "MiscPottier.max_list")
  | [a] -> a
  | a :: l -> max a (max_list l)

let rec reduce l1 l2 = match l1, l2 with
  | [], _ -> (([], []), ([], l2))
  | _, [] -> (([], l1), ([], []))
  | a :: l1, b :: l2 ->
    let ((common1, rest1), (common2, rest2)) = reduce l1 l2 in
    ((a :: common1, rest1), (b :: common2, rest2))

let pow a b =
  if b < 0 then raise (Invalid_argument "MiscPottier.pow2")
  else
    let rec aux = function
      | 0 -> 1
      | i -> a * aux (i-1) in
    aux b

let rec make a n =
  if n = 0 then []
  else a :: (make a (n-1))

let index_of x =
  let rec aux i = function
    | [] -> raise Not_found
    | y :: l -> if y = x then i else aux (i+1) l
  in
  aux 0

let rec remove_n_first n =
  let rec aux i = function
  | [] -> []
  | l when i = n -> l
  | _ :: l -> aux (i+1) l in
  aux 0

let foldi_from_until n m f a l =
  let rec aux i res = function
    | [] -> res
    | _ when i >= m -> res
    | e :: l -> aux (i+1) (f i res e) l in
  aux 0 a (remove_n_first n l)

let foldi_from n f a l = foldi_from_until n (List.length l) f a l

let foldi_until m f a l = foldi_from_until 0 m f a l

let foldi f a l = foldi_from_until 0 (List.length l) f a l

let pos e l =
  let f i res e' = if e' = e then Some i else res in
  match foldi f None l with
    | None -> raise Not_found
    | Some i -> i

let iteri f l =
  let rec aux i = function
    | [] -> ()
    | e :: l -> f i e ; aux (i+1) l
  in
  aux 0 l

let mapi f l =
  let rec aux i = function
    | [] -> []
    | e :: l -> (f i e) :: (aux (i+1) l)
  in
  aux 0 l

let rec last = function
  | [] -> raise Not_found
  | [a] -> a
  | _ :: l -> last l

(* [split a i] splits the list a in two lists: one with the elements
   up until the [i]th (exclusive) and one with the rest. *)

let rec split l i =
  if i = 0 then ([], l)
  else
    let (l1, l2) = split (List.tl l) (i-1) in
    ((List.hd l) :: l1, l2)

(* [split_last l] returns the list [l] without its last element and its last
   element. Raises Invalid_argument "MiscPottier.split_last" if the list is
   empty. *)

let split_last l = match split l ((List.length l) - 1) with
  | l', last :: _ -> (l', last)
  | _ -> raise (Invalid_argument "MiscPottier.split_last")

let rec update_list_assoc a b = function
  | [] -> []
  | (a', b') :: l ->
      if a' = a then (a, b) :: l else (a', b') :: (update_list_assoc a b l)

(* Pasted from Pottier's PP compiler *)

let rec combine xs1 xs2 =
  match xs1, xs2 with
  | [], _
  | _, [] ->
      []
  | x1 :: xs1, x2 :: xs2 ->
      (x1, x2) :: combine xs1 xs2

let rec subtract xs1 xs2 =
  match xs1, xs2 with
  | [], _ ->
      []
  | _, [] ->
      xs1
  | _ :: xs1, _ :: xs2 ->
      subtract xs1 xs2

let mirror l =
  List.map (fun (x, y) -> (y, x)) l

let length l =
  Int32.of_int (List.length l)

let rec prefix k l =
  match k, l with
  | 0, _
  | _, [] ->
      []
  | _, x :: xs ->
      x :: prefix (k - 1) xs

let memoize f =
  let table = Hashtbl.create 131 in
  fun key ->
    try
      Hashtbl.find table key
    with Not_found ->
      let data = f key in
      Hashtbl.add table key data;
      data

let filter_map filter map =
  let rec aux = function
    | [] -> []
    | e :: l -> (if filter e then [map e] else []) @ (aux l)
  in
  aux

let string_of_list sep f =
  let rec aux = function
    | [] -> ""
    | [e] -> f e
    | e :: l -> (f e) ^ sep ^ (aux l)
  in
  aux

let div_up a b =
  let quo = a / b in
  if quo * b < a then quo + 1 else quo

let make_sequence step first size =
  let rec aux current i =
    if i >= size then []
    else current :: (aux (step current) (i+1)) in
  aux first 0

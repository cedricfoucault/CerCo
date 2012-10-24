module LexingExt = struct

  open Lexing

  let new_line lexbuf = 
    lexbuf.lex_curr_p <- { 
      lexbuf.lex_curr_p with 
	pos_bol  = 0;
	pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1
    }

end

module ListExt = struct

  let inv_assoc l = List.map (fun (x, y) -> (y, x)) l

  exception EmptyList

  let last l = try List.hd (List.rev l) with _ -> raise EmptyList

  let cut_last l = 
    let rec aux l = function
      | []      -> raise EmptyList
      | [ x ]   -> (x, List.rev l)
      | x :: xs -> aux (x :: l) xs
    in
    aux [] l

  let multi_set_of_list l = 
    let h = Hashtbl.create 13 in 
    let incr_occ x = 
      let o = try Hashtbl.find h x with Not_found -> 0 in
      Hashtbl.replace h x (o + 1)
    in
    List.iter incr_occ l;
    Hashtbl.fold (fun k v accu -> (k, v) :: accu) h []

  let hashtbl_of_assoc l = 
    let h = Hashtbl.create 13 in 
    List.iter (fun (k, v) -> Hashtbl.add h k v) l;
    h

  exception Conflict
  let assoc_union l1 l2 = 
    let h1 = hashtbl_of_assoc l1 in
    l1 
    @ List.filter 
      (fun (k, v1) -> 
	try 
	  let v2 = Hashtbl.find h1 k in
	  if v1 <> v2 then raise Conflict;
	  false
	with _ -> true) l2

  let assoc_diff l1 l2 = 
    let h1 = hashtbl_of_assoc l1 in
    let h2 = hashtbl_of_assoc l2 in
    let diff h1 h2 f = 
      Hashtbl.fold
	(fun k v1 accu -> 
	  let v2 = 
	    try Some (Hashtbl.find h2 k)
	    with Not_found -> None
	  in
	  if Some v1 <> v2 then 
	    if f then 
	      (k, (Some v1, v2)) :: accu
	    else 
	      (k, (v2, Some v1)) :: accu
	  else
	    accu)
	h1 []
    in
    let d1 = diff h1 h2 true in
    let d2 = diff h2 h1 false in
    try assoc_union d1 d2
    with Conflict -> assert false

  let transitive_forall2 p l = 
    let rec aux = function
      | []  -> None
      | [x] -> None
      | x1 :: ((x2 :: _) as xs) -> 
	if not (p x1 x2) then Some (x1, x2) else aux xs
    in
    aux l

end

module ArgExt = struct

  let extra_doc s = "", Arg.Unit ignore, s

end

module SysExt = struct

  let safe_remove name =
    try Sys.remove name with Sys_error _ -> ()

  let rec alternative name = 
    if not (Sys.file_exists name) then
      name
    else 
      let dirname = Filename.dirname name in
      let filename = Filename.basename name in
      let r = Str.regexp "\\([0-9]+\\)-\\(.*\\)" in
      let filename = 
	if Str.string_match r filename 0 then
	  let i = int_of_string (Str.matched_group 1 filename) in
	  Printf.sprintf "%02d-%s" (i + 1) (Str.matched_group 2 filename)
	else 
	  "01-" ^ filename
      in
      alternative (Filename.concat dirname filename)
      
end

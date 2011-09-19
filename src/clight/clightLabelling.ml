
(** This module defines the labelling of a [Clight] program. *)

open Clight
open CostLabel


(* Add a cost label in front of a statement. *)

let add_starting_cost_label cost_universe stmt =
  Clight.Scost (CostLabel.Gen.fresh cost_universe, stmt)

(* Add a cost label at the end of a statement. *)

let add_ending_cost_label cost_universe stmt =
  Clight.Ssequence (stmt, add_starting_cost_label cost_universe Clight.Sskip)


let int_type = Tint (I32, AST.Signed)
let const_int i = Expr (Econst_int i, int_type)


let typeof e = let Expr (_,t) = e in t


let add_cost_label_e cost_universe e =
  Expr (Ecost (CostLabel.Gen.fresh cost_universe, e), typeof e)


(* Add cost labels to an expression. *)

let rec add_cost_labels_e cost_universe = function  
  | Expr (exp,cty) -> Expr (add_cost_labels_expr cost_universe exp,cty)

and add_cost_labels_expr cost_universe exp = match exp with
  | Econst_int _ | Econst_float _ | Evar _ | Esizeof _ -> exp
  | Ederef e -> Ederef (add_cost_labels_e cost_universe e)
  | Eaddrof e -> Eaddrof (add_cost_labels_e cost_universe e)
  | Eunop (op,e) -> Eunop (op,(add_cost_labels_e cost_universe e))
  | Ebinop (op,e1,e2) ->
      Ebinop (op,
	      add_cost_labels_e cost_universe e1,
	      add_cost_labels_e cost_universe e2)
  | Ecast (t,e)	-> Ecast (t, add_cost_labels_e cost_universe e)
  | Eandbool (e1,e2) ->
      let e1' = add_cost_labels_e cost_universe e1 in
      let e2' = add_cost_labels_e cost_universe e2 in
      let b1 = add_cost_label_e cost_universe (const_int 1) in
      let b2 = add_cost_label_e cost_universe (const_int 0) in
      let e2' =	Expr (Econdition (e2', b1, b2), int_type) in
      let e2' = add_cost_label_e cost_universe e2' in
      let e3' = add_cost_label_e cost_universe (const_int 0) in
      Econdition (e1', e2', e3')
  | Eorbool (e1,e2) ->
      let e1' = add_cost_labels_e cost_universe e1 in
      let e2' = add_cost_label_e cost_universe (const_int 1) in
      let e3' = add_cost_labels_e cost_universe e2 in
      let b1 = add_cost_label_e cost_universe (const_int 1) in
      let b2 = add_cost_label_e cost_universe (const_int 0) in
      let e3' =	Expr (Econdition (e3', b1, b2), int_type) in
      let e3' = add_cost_label_e cost_universe e3' in
      Econdition (e1', e2', e3')
  | Efield (e,id) -> Efield (add_cost_labels_e cost_universe e,id)
  | Econdition (e1,e2,e3) ->
      let e1' = add_cost_labels_e cost_universe e1 in
      let e2' = add_cost_labels_e cost_universe e2 in
      let e2' = add_cost_label_e cost_universe e2' in
      let e3' = add_cost_labels_e cost_universe e3 in
      let e3' = add_cost_label_e cost_universe e3' in
      Econdition (e1', e2', e3')
  | Ecost (_,_) | Ecall _ -> assert false (* Should not happen *)

let add_cost_labels_opt cost_universe = function
  | None -> None
  | Some e -> Some (add_cost_labels_e cost_universe e)

let add_cost_labels_lst cost_universe l =
  List.map (add_cost_labels_e cost_universe) l


(* Add cost labels to a statement. *)

let rec add_cost_labels_st cost_universe = function
  | Sskip -> Sskip
  | Sassign (e1,e2) ->
      Sassign (add_cost_labels_e cost_universe e1,
	       add_cost_labels_e cost_universe e2)
  | Scall (e1,e2,lst) ->
      Scall (add_cost_labels_opt cost_universe e1,
             add_cost_labels_e cost_universe e2,
             add_cost_labels_lst cost_universe lst)
  | Ssequence (s1,s2) ->
      Ssequence (add_cost_labels_st cost_universe s1,
		 add_cost_labels_st cost_universe s2) 
  | Sifthenelse (e,s1,s2) ->
      let s1' = add_cost_labels_st cost_universe s1 in
      let s1' = add_starting_cost_label cost_universe s1' in
      let s2' = add_cost_labels_st cost_universe s2 in
      let s2' = add_starting_cost_label cost_universe s2' in
      Sifthenelse (add_cost_labels_e cost_universe e, s1', s2')
  | Swhile (e,s) ->
      let s' = add_cost_labels_st cost_universe s in
      let s' = add_starting_cost_label cost_universe s' in
      add_ending_cost_label cost_universe
	(Swhile (add_cost_labels_e cost_universe e, s'))
  | Sdowhile (e,s) ->
      let s1 = add_cost_labels_st cost_universe s in
      let s2 = add_cost_labels_st cost_universe s in
      let s2' = add_starting_cost_label cost_universe s2 in
      add_ending_cost_label cost_universe
	(Ssequence (s1, Swhile (add_cost_labels_e cost_universe e, s2')))
  | Sfor (s1,e,s2,s3) ->
      let s1' = add_cost_labels_st cost_universe s1 in
      let s2' = add_cost_labels_st cost_universe s2 in
      let s3' = add_cost_labels_st cost_universe s3 in
      let s3' = add_starting_cost_label cost_universe s3' in
      add_ending_cost_label cost_universe
	(Sfor (s1', add_cost_labels_e cost_universe e, s2', s3'))
  | Sreturn e -> Sreturn (add_cost_labels_opt cost_universe e)
  | Sswitch (e,ls) ->
      Sswitch (add_cost_labels_e cost_universe e,
	       add_cost_labels_ls cost_universe ls)
  | Slabel (lbl,s) ->
      let s' = add_cost_labels_st cost_universe s in
      let s' = add_starting_cost_label cost_universe s' in
      Slabel (lbl,s')
  | Scost (_,_) -> assert false
  | _ as stmt -> stmt

and add_cost_labels_ls cost_universe = function
  | LSdefault s ->
      let s' = add_cost_labels_st cost_universe s in
      let s' = add_starting_cost_label cost_universe s' in
      LSdefault s'
  | LScase (i,s,ls) ->
      let s' = add_cost_labels_st cost_universe s in
      let s' = add_starting_cost_label cost_universe s' in
      LScase (i, s', add_cost_labels_ls cost_universe ls)


(* Add cost labels to a function. *)

let add_cost_labels_f cost_universe = function
  | (id,Internal fd) -> (id,Internal {
      fn_return = fd.fn_return ;
      fn_params = fd.fn_params ;
      fn_vars = fd.fn_vars ;
      fn_body = add_starting_cost_label cost_universe
			     (add_cost_labels_st cost_universe fd.fn_body)
    })
  | (id,External (a,b,c)) -> (id,External (a,b,c))


(** [add_cost_labels prog] inserts some labels to enable
    cost annotation. *)

let add_cost_labels p =
  let labs = ClightAnnotator.all_labels p in
  let labs = StringTools.Set.fold CostLabel.Set.add labs CostLabel.Set.empty in
  let cost_prefix = CostLabel.Gen.fresh_prefix labs "_cost" in
  let cost_universe = CostLabel.Gen.new_universe cost_prefix in
  {
    prog_funct = List.map (add_cost_labels_f cost_universe) p.prog_funct;
    prog_main = p.prog_main;
    prog_vars = p.prog_vars
  }

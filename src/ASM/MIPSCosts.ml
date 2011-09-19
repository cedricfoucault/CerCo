
let error_prefix = "MIPSCosts"
let error s = Error.global_error error_prefix s
let warning s = Error.warning error_prefix s


type instruction_nature =
  | Goto of Label.t | Branch of Label.t | Fun_call | Return | Seq
  | Cost_label of CostLabel.t

let inst_cost = function
  | Arch.IComment _ | Arch.ILabel _ | Arch.ICost _ -> 0
  | _ -> 1 (* TODO *)

let index_of_inst inst code = MiscPottier.pos inst code
let index_of_label lbl code = index_of_inst (Arch.ILabel lbl) code
let mem_label lbl code = List.mem (Arch.ILabel lbl) code

let fetch_inst code pc =
  if pc >= List.length code then error "instruction address out of code"
  else List.nth code pc

let inst_infos code pc =
  let next_pc = pc + 1 in (* pc of the instruction below *)
  let inst = fetch_inst code pc in
  let (nature, next_pcs (* pc of the next instructions in the flow *)) =
    match inst with
      | Arch.IComment _
      | Arch.INop _
      | Arch.IConst _
      | Arch.IUnOp _
      | Arch.IBinOp _
      | Arch.ILoadAddr _
      | Arch.ILoad _
      | Arch.IStore _
      | Arch.ILabel _
      | Arch.ICost _ -> (Seq, [next_pc])
      | Arch.ICall _ | Arch.IGotor _ -> (Fun_call, [next_pc])
      | Arch.IGoto lbl -> (Goto lbl, [index_of_label lbl code])
      | Arch.IBranch (_, lbl_true) ->
	(Branch lbl_true, [next_pc ; index_of_label lbl_true code])
      | Arch.IReturn -> (Return, []) in
  (nature, next_pc, next_pcs, inst_cost inst)

let inst_nature code pc =
  let (nature, _, _, _) = inst_infos code pc in
  nature

let is_cost_label code pc = match inst_nature code pc with
  | Cost_label _ -> true
  | _ -> false


let rec compare = function
  | [] -> assert false (* do not use an this argument *)
  | [(_, cost)] -> cost
  | (_, cost1) :: (pc2, cost2) :: l when cost1 <> cost2 ->
    warning "imprecision found; considering the max." ;
    max cost1 (compare ((pc2, cost2) :: l))
  | _ :: l -> compare l

let rec block_costl code = function
  | [] -> 0
  | [pc] when is_cost_label code pc -> 0
  | [pc] -> block_cost code pc
  | next_pcs ->
    compare (List.map (fun pc -> (pc, block_costl code [pc])) next_pcs)

and block_cost code pc =
  let (_, _, next_pcs, cost) = inst_infos code pc in
  cost + (block_costl code next_pcs)

let traverse_code p =
  let rec aux pc =
    if pc >= List.length p.Arch.code then CostLabel.Map.empty
    else
      let (nature, next_pc, _, _) = inst_infos p.Arch.code pc in
      let costs_mapping = aux next_pc in
      match nature with
	| Cost_label cost_lbl ->
	  let cost = block_cost p.Arch.code next_pc in
	  CostLabel.Map.add cost_lbl cost costs_mapping
	| _ -> costs_mapping in
  aux 0


let init_instructions_cost p =
  let rec aux pc = match inst_infos p.Arch.code pc with
    | (Fun_call, _, _, cost) -> cost
    | (Seq, _, [next_pc], cost) -> cost + (aux next_pc)
    | _ ->
      (* Such instructions should not occur in the initialization code. *)
      assert false in
  aux 0

let first_cost_label main p =
  let rec aux pc = match fetch_inst p.Arch.code pc with
    | Arch.ICost cost_lbl -> cost_lbl
    | Arch.IComment _
    | Arch.ILabel _ -> aux (pc+1)
    | _ ->
      (* Such instructions should not occur before the first label of a
	 function. *)
      assert false in
  aux (index_of_label main p.Arch.code)


let initialize_cost main costs_mapping p =
  let cost = init_instructions_cost p in
  let cost_lbl = first_cost_label main p in
  let old_cost =
    if CostLabel.Map.mem cost_lbl costs_mapping then
      CostLabel.Map.find cost_lbl costs_mapping
    else 0 in
  let new_cost = old_cost + cost in
  CostLabel.Map.add cost_lbl new_cost costs_mapping


let compute p =
  let costs_mapping = traverse_code p in
  match p.Arch.main with
    | None -> costs_mapping
    | Some main -> initialize_cost main costs_mapping p

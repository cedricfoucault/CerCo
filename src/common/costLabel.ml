
include StringTools

let constant_map d x = 
  Set.fold (fun k accu -> Map.add k x accu) d Map.empty

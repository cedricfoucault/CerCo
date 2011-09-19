
open Clight


(* TODO: Alignment constraints? *)
let rec size_of_ctype = function
  | Tvoid               -> 0
  | Tint (I8,_)         -> 1
  | Tint (I16,_)        -> 2
  | Tint (I32,_)        -> 4
  | Tfloat _            -> assert false (* Not supported *)
  | Tpointer (sp,_)     -> Memory.size_of_region sp
  | Tarray (sp,t,s)     -> s*(size_of_ctype t)
  | Tfunction (_,_)     -> assert false (* Not supported *) 
  | Tstruct (_,lst)     -> 
      List.fold_left (fun n (_,ty) -> n + (size_of_ctype ty)) 0 lst 
  | Tunion (_,lst)      -> 
      List.fold_left 
        (fun n (_,ty)   -> 
           let sz = (size_of_ctype ty) in (if n>sz then n else sz)
        ) 0 lst 
  | Tcomp_ptr (sp,_)    -> Memory.size_of_region sp


let rec memory_q_of_ctype = function
  | Tvoid               -> assert false
  | Tint (I8,Signed)    -> Memory.MQ_int8signed 
  | Tint (I8,Unsigned)  -> Memory.MQ_int8unsigned
  | Tint (I16,Signed)   -> Memory.MQ_int16signed
  | Tint (I16,Unsigned) -> Memory.MQ_int16unsigned
  | Tint (I32,Signed)   -> Memory.MQ_int32
  | Tint (I32,Unsigned) -> assert false (* Not supported *)
  | Tfloat _            -> assert false (* Not supported *)
  | Tpointer (sp,_)     -> Memory.mq_of_region sp
  | Tarray (sp,c,s)     -> Memory.mq_of_region sp
  | Tfunction (_,_)     -> assert false (* should not happen thanks to CIL *)
  | Tstruct (_,_)       -> assert false (* should not happen thanks to CIL *)
  | Tunion (_,_)        -> assert false (* should not happen thanks to CIL *)
  | Tcomp_ptr (sp,_)    -> Memory.mq_of_region sp


let is_int_type = function
  | Tint (_,_)    -> true
  | _ -> false

let is_float_type = function
  | Tfloat _  -> true
  | _ -> false

let is_pointer_type = function
  | Tpointer _ | Tarray _ | Tcomp_ptr _ -> true
  | _ -> false

let is_stack_type = function
  | Tarray _ | Tstruct _ | Tunion _ -> true
  | _ -> false

let is_struct = function
  | Tstruct _ | Tunion _ -> true
  | _ -> false

let is_ptr_to_struct = function
  | Tpointer (_,t) when is_struct t -> true
  | Tcomp_ptr _ -> true
  | _ -> false  

let is_function = function
  | Tfunction _ -> true
  | _ -> false

let region_of_pointer_type = function
  | Tpointer (sp,_) | Tarray (sp,_,_) | Tcomp_ptr (sp,_) -> sp
  | _ -> assert false (* do not use on those arguments *)

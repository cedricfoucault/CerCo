
val size_of_ctype : Clight.ctype -> int

val memory_q_of_ctype : Clight.ctype -> Memory.memory_q

val is_int_type : Clight.ctype -> bool
val is_float_type : Clight.ctype -> bool
val is_pointer_type : Clight.ctype -> bool
val is_stack_type : Clight.ctype -> bool
val is_struct : Clight.ctype -> bool
val is_ptr_to_struct : Clight.ctype -> bool
val is_function : Clight.ctype -> bool

val region_of_pointer_type : Clight.ctype -> AST.region

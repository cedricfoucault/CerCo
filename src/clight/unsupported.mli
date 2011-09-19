
(** This module adds runtime functions in a [Clight] program. These functions
    implement unsupported functions by the target architecture that introduce a
    branch. We need to define them at the [Clight] level in order to have a
    correct labelling. *)

val add :
  Clight.program -> Runtime.op_replacement list ->
  (Clight.program *
   (Runtime.operation * string) list (* operation association *))

val replace : Clight.program -> Clight.program

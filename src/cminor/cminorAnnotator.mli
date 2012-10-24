
(** This module defines the instrumentation of a [Cminor] program. *)

(*
(** [instrument prog cost_map] instruments the program [prog]. First a fresh
    global variable --- the so-called cost variable --- is added to the program.
    Then, each cost label in the program is replaced by an increment of the cost
    variable, following the mapping [cost_map]. The function also returns the
    name of the cost variable and the name of the cost increment function. *)

val instrument : Cminor.program -> int CostLabel.Map.t ->
                 Cminor.program * string * string
*)

val cost_labels : Cminor.program -> CostLabel.Set.t
val user_labels : Cminor.program -> Label.Set.t
val all_labels  : Cminor.program -> StringTools.Set.t

val prog_idents : Cminor.program -> StringTools.Set.t


(** This module defines the instrumentation of a [Clight] program. *)

(** [instrument prog cost_map] instruments the program [prog]. First a fresh
    global variable --- the so-called cost variable --- is added to the program.
    Then, each cost label in the program is replaced by an increment of the cost
    variable, following the mapping [cost_map]. The function also returns the
    name of the cost variable, the name of the cost increment function, and a
    fresh uninitialized global (cost) variable associated to each extern
    function. *)

val instrument : Clight.program -> int CostLabel.Map.t ->
                 Clight.program * string * string * string StringTools.Map.t

val cost_labels : Clight.program -> CostLabel.Set.t
val user_labels : Clight.program -> Label.Set.t
val all_labels  : Clight.program -> StringTools.Set.t
val all_idents  : Clight.program -> StringTools.Set.t

val fresh_ident : string (* base *) -> Clight.program -> string

val fresh_universe :
  string (* prefix *) -> Clight.program -> StringTools.Gen.universe

val make_fresh :
  string (* prefix *) -> Clight.program -> (unit -> string)

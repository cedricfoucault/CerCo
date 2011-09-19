
(** This module defines the cost increment associated to each cost
    label of a [ASM] program. *)

val compute : ASM.program -> int CostLabel.Map.t

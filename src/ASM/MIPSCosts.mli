
(** This module defines the cost increment associated to each cost
    label of a [MIPS] program. *)

val compute : Driver.TargetArch.program -> int CostLabel.Map.t

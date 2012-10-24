
module Make (A : Arch.ARCH) : sig

  val compute : A.register Arch.generic_program -> int CostLabel.Map.t

end

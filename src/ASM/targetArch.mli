
module type S = sig

  include Arch.ARCH

  type program = register Arch.generic_program

  val print_program : program -> string list (* [.s ; .hex] *)

  val interpret : bool -> program -> AST.trace

  val compute_costs : program -> int CostLabel.Map.t

end


module Make (A : Arch.ARCH) : S

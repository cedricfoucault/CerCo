
module type S = sig

  include Arch.ARCH

  type program = register Arch.generic_program

  val print_program : program -> string list (* [.s ; .hex] *)

  val interpret : bool -> program -> AST.trace

  val compute_costs : program -> int CostLabel.Map.t

end


module Make (A : Arch.ARCH) = struct

  include A

  type program = register Arch.generic_program

  module Printer = ArchPrinter.Make (A)

  let print_program = Printer.print_program

  module Interpret = ArchInterpret.Make (A)

  let interpret = Interpret.interpret

  module Costs = ArchCosts.Make (A)

  let compute_costs = Costs.compute

end

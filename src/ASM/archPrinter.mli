
(** This module provides a function to print [Arch] programs in the SPIM
    syntax. *)

module Make (A : Arch.ARCH) : sig

  val print_program : A.register Arch.generic_program -> string list

end

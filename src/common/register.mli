(* Pasted from Pottier's PP compiler *)

(** This module offers an abstract type of pseudo-registers, used in
    [RTL] and [ERTL].

    This module really is a copy of module [Atom]. However,
    [Register.t] is an abstract type -- it is not equal to [Atom.t] or
    [Label.t]. *)

include AtomSig.S


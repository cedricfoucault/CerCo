
(** [simplify p] removes unnecessary casts in the Clight program [p].

    Example: [(char) ((int)x + (int)y)] where [x] and [y] are of type [char]
    will be transformed into [x + y]. Primitive operations are thus supposed to
    be polymorphic, but working only on homogene types. *)

val simplify : Clight.program -> Clight.program

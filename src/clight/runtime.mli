
type operation =
  | Unary of Clight.unary_operation * Clight.ctype
  | Binary of Clight.binary_operation * Clight.ctype * Clight.ctype
  | Cast of Clight.ctype (* destination type *) * Clight.ctype (* source type *)
  | Fun of string (* name of the function *)

type op_replacement =
    (* operation to be replaced *)
    operation *
    (* base name of the replacement function *)
    string *
    (* C definition of the replacement function, provided a name for the
       function *)
    (string -> string) *
    (* dependences *)
    operation list

val mod8s  : op_replacement
val mod16s : op_replacement
val mod32s : op_replacement

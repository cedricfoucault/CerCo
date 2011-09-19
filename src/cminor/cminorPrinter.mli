
(** This module provides functions to print elements of [Cminor]
    programs. *)

val print_expression : Cminor.expression -> string

val print_body : int (* indentation *) -> Cminor.statement -> string

val string_of_statement : Cminor.statement -> string

val print_program :  Cminor.program -> string


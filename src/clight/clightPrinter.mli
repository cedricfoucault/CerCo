(** This module provides functions to print elements of [Clight]
    programs. *)

val print_program: Clight.program -> string

val print_expression: Clight.expr -> string

val string_of_ctype: Clight.ctype -> string

val print_statement: Clight.statement -> string

val print_ctype_prot: Clight.ctype -> string

val print_ctype_def: Clight.ctype -> string

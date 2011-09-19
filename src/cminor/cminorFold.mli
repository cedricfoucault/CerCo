
(** This module provides folding functions over the constructors of the
    [Cminor]'s AST. *)

val expression_subs : Cminor.expression -> Cminor.expression list

val expression_fill_subs : Cminor.expression -> Cminor.expression list ->
                           Cminor.expression

(* In [expression f e], [f]'s second argument is the list of
   [expression]'s results on [e]'s sub-expressions. *)

val expression : (Cminor.expression -> 'a list -> 'a) ->
                 Cminor.expression ->
                 'a

val statement_subs : Cminor.statement ->
                     (Cminor.expression list * Cminor.statement list)

val statement_fill_subs : Cminor.statement ->
                          Cminor.expression list ->
                          Cminor.statement list ->
                          Cminor.statement

(* In [statement f_expr f_stmt stmt], [f_stmt]'s second argument is the
   list of [expression f_expr]'s results on [stmt]'s sub-expressions, and
   [f_stmt]'s third argument is the list of [statement]'s results
   on [stmt]'s sub-statements. *)

val statement : (Cminor.expression -> 'a list -> 'a) ->
                (Cminor.statement -> 'a list -> 'b list -> 'b) ->
                Cminor.statement ->
                'b

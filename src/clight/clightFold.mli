
(** This module provides folding functions over the constructors of the
    [Clight]'s AST. *)

val ctype_fill_subs : Clight.ctype -> Clight.ctype list -> Clight.ctype

val ctype : (Clight.ctype -> 'a list -> 'a) -> Clight.ctype -> 'a

val expr_fill_subs :
  Clight.expr -> Clight.ctype list -> Clight.expr_descr list ->
  Clight.expr

val expr_fill_exprs :
  Clight.expr -> Clight.expr list -> Clight.expr

val expr :
  (Clight.ctype -> 'a list -> 'a) ->
  (Clight.expr -> 'a list -> 'b list -> 'c) ->
  (Clight.expr_descr -> 'a list -> 'c list -> 'b) ->
  Clight.expr ->
  'c

val expr2 :
  (Clight.expr -> 'a list -> 'a) -> Clight.expr -> 'a

val expr_descr_subs :
  Clight.expr_descr -> Clight.ctype list * Clight.expr list

val expr_descr_fill_subs :
  Clight.expr_descr -> Clight.ctype list -> Clight.expr list ->
  Clight.expr_descr

val expr_descr :
  (Clight.ctype -> 'a list -> 'a) ->
  (Clight.expr -> 'a list -> 'b list -> 'c) ->
  (Clight.expr_descr -> 'a list -> 'c list -> 'b) ->
  Clight.expr_descr ->
  'b

val statement_subs :
  Clight.statement ->
  (Clight.expr list * Clight.statement list)

val statement_sub_exprs : Clight.statement -> Clight.expr list

val statement_fill_subs :
  Clight.statement -> Clight.expr list -> Clight.statement list ->
  Clight.statement

val statement :
  (Clight.ctype -> 'a list -> 'a) ->
  (Clight.expr -> 'a list -> 'b list -> 'c) ->
  (Clight.expr_descr -> 'a list -> 'c list -> 'b) ->
  (Clight.statement -> 'c list -> 'd list -> 'd) ->
  Clight.statement ->
  'd

val statement2 :
  (Clight.expr -> 'a list -> 'a) ->
  (Clight.statement -> 'a list -> 'b list -> 'b) ->
  Clight.statement ->
  'b

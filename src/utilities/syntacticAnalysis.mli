(** This module provides a general gluing function between lexers and
    parsers. *)

(** [process init lex parse input] initialized a lexing buffer using
    [init input] and make [lex] and [parse] communicate through it 
    to produce an abstract syntax tree. *)
val process :
  lexer_init : ('a -> 'lexbuf) ->
  lexer_fun  : ('lexbuf -> 'token) ->
  parser_fun : (('lexbuf -> 'token) -> 'lexbuf -> 'ast) ->
  input      : 'a ->
  'ast

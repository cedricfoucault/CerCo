(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

{
  open CminorParser
  exception Error of string
}

let ret = ['\010']
let blank = [' ' '\009' '\012' '\013']
let floatlit = 
  ['0'-'9'] ['0'-'9' '_']* 
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']*
let intlit = "-"? ( ['0'-'9']+ | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
                               | "0o" ['0'-'7']+ | "0b" ['0'-'1']+ )
let stringlit = "\"" [ ^ '"' ] * '"'

rule token = parse
  | ret   { Misc.LexingExt.new_line lexbuf ; token lexbuf }
  | blank +      { token lexbuf }
  | "/*"         { comment lexbuf; token lexbuf }
  | "absf" { ABSF }
  | "&" { AMPERSAND }
  | "&&" { AMPERSANDAMPERSAND }
  | "!"    { BANG }
  | "!="    { BANGEQUAL }
  | "!=f"    { BANGEQUALF }
  | "!=u"    { BANGEQUALU }
  | "|"     { BAR }
  | "||"    { BARBAR }
  | "^"     { CARET }
  | "case"  { CASE }
  | ":"    { COLON }
  | ","    { COMMA }
  | "default" { DEFAULT }
(*  | "$"    { DOLLAR } *)
  | "else"    { ELSE }
  | "="    { EQUAL }
  | "=="    { EQUALEQUAL }
  | "==f"    { EQUALEQUALF }
  | "==u"    { EQUALEQUALU }
  | "exit"    { EXIT }
  | "extern"    { EXTERN }
  | "float"    { FLOAT }
  | "float32"    { FLOAT32 }
  | "float64"    { FLOAT64 }
  | "floatofint"    { FLOATOFINT }
  | "floatofintu"    { FLOATOFINTU }
  | ">"    { GREATER }
  | ">f"    { GREATERF }
  | ">u"    { GREATERU }
  | ">="    { GREATEREQUAL }
  | ">=f"    { GREATEREQUALF }
  | ">=u"    { GREATEREQUALU }
  | ">>"    { GREATERGREATER }
  | ">>u"    { GREATERGREATERU }
  | "if"    { IF }
(*  | "in"    { IN } *)
  | "int"    { INT }
  | "int8"    { INT8 }
  | "int16"    { INT16 }
  | "int32"    { INT32 }
  | "int8sto8"    { INT8STO8 }
  | "int8sto16"    { INT8STO16 }
  | "int8sto32"    { INT8STO32 }
  | "int8uto8"    { INT8UTO8 }
  | "int8uto16"    { INT8UTO16 }
  | "int8uto32"    { INT8UTO32 }
  | "int16sto8"    { INT16STO8 }
  | "int16sto16"    { INT16STO16 }
  | "int16sto32"    { INT16STO32 }
  | "int16uto8"    { INT16UTO8 }
  | "int16uto16"    { INT16UTO16 }
  | "int16uto32"    { INT16UTO32 }
  | "int32sto8"    { INT32STO8 }
  | "int32sto16"    { INT32STO16 }
  | "int32sto32"    { INT32STO32 }
  | "int32uto8"    { INT32UTO8 }
  | "int32uto16"    { INT32UTO16 }
  | "int32uto32"    { INT32UTO32 }
  | "intoffloat"    { INTOFFLOAT }
  | "intuoffloat"    { INTUOFFLOAT }
  | "ptr" { PTR }
  | "{"    { LBRACE }
(*  | "{{"    { LBRACELBRACE } *)
  | "["    { LBRACKET }
  | "<"    { LESS }
  | "<u"    { LESSU }
  | "<f"    { LESSF }
  | "<="    { LESSEQUAL }
  | "<=u"    { LESSEQUALU }
  | "<=f"    { LESSEQUALF }
  | "<<"    { LESSLESS }
(*  | "let"     { LET } *)
  | "loop"    { LOOP }
  | "("    { LPAREN }
  | "match" { MATCH }
  | "-"    { MINUS }
  | "->"    { MINUSGREATER }
  | "-f"    { MINUSF }
  | "%"    { PERCENT }
  | "%u"    { PERCENTU }
  | "+"    { PLUS }
  | "+f"    { PLUSF }
  | "?"    { QUESTION }
  | "}"    { RBRACE }
(*  | "}}"    { RBRACERBRACE } *)
  | "]"    { RBRACKET }
  | "return"    { RETURN }
  | ")"    { RPAREN }
  | ";"    { SEMICOLON }
  | "/"    { SLASH }
  | "/f"    { SLASHF }
  | "/u"    { SLASHU }
  | "stack"    { STACK }
  | "*" { STAR }
  | "*f"    { STARF }
  | "switch"    { SWITCH }
  | "tailcall"  { TAILCALL }
  | "~"    { TILDE }
  | "var"    { VAR }
  | "void"    { VOID }
  | "goto" { GOTO }
  | "block" { BLOCK }
  | intlit    { INTLIT(int_of_string(Lexing.lexeme lexbuf)) }
  | floatlit     { FLOATLIT(float_of_string(Lexing.lexeme lexbuf)) }
  | stringlit { let s = Lexing.lexeme lexbuf in
                STRINGLIT(String.sub s 1 (String.length s - 2)) }
  | ident    { IDENT(Lexing.lexeme lexbuf) }
  | eof      { EOF }
  | _        { raise(Error("illegal character `" ^ Char.escaped (Lexing.lexeme_char lexbuf 0) ^ "'")) }

and comment = parse
    "*/"     { () }
  | eof      { raise(Error "unterminated comment") }
  | _        { comment lexbuf }

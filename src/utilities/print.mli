(* Pasted from Pottier's PP compiler *)

(** This module provides printing utilities. *)

type punctuation =
    unit -> string

type 'a printer =
    unit -> 'a -> string

(* [nl] prints a newline character and a certain amount of
   whitespace that reflects the current indentation level. *)

val nl: punctuation

(* [indent ofs] transforms a printer into another printer
   that indents material [ofs] characters to the right. *)

val indent: int -> 'a printer -> 'a printer

(* [list] prints a list without any delimiters. *)

val list: 'a printer -> 'a list printer

(* [preclist] prints a list where a delimiter precedes every
   element. *)

val preclist: punctuation -> 'a printer -> 'a list printer

(* [termlist] prints a list where a delimiter terminates every
   element. *)

val termlist: punctuation -> 'a printer -> 'a list printer

(* [seplist] prints a list where a separator separates every two
   consecutive elements. *)

val seplist: punctuation -> 'a printer -> 'a list printer

(* [annlist] prints nothing if its list argument is empty, and prints
   an announcement followed by the list if the list is nonempty. *)

val annlist: punctuation -> 'a list printer -> 'a list printer

(* Punctuation. *)

val space: punctuation
val comma: punctuation
val semicolon: punctuation
val var: punctuation
val seminl: punctuation
val nlspace: int -> punctuation
val nlnl: punctuation

(* [atmost n delimiter stop] normally prints a [delimiter], except that,
   every [n] calls, it prints a [stop] in addition. *)

val atmost: int -> punctuation -> punctuation -> punctuation

(* [catenate] turns a list of columns into a single column, adding
   padding (whitespace) to enforce alignment. *)

val catenate: string list list -> string list

(* [transposerev] turns a reversed list of lines into a list of columns. *)

val transposerev: string list list -> string list list

(* [showif flag printer x] displays [x] on standard output using
   [printer] when [flag] is set. It returns [x]. *)

val showif: bool -> 'a printer -> 'a -> 'a


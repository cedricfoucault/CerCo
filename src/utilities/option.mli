
(** This module provides some functions to manipulate values of type
    [option]. *)

(* Pasted from Pottier's PP compiler *)

open PrintPottier

val map: ('a -> 'b) -> 'a option -> 'b option
val iter: ('a -> unit) -> 'a option -> unit
val fold: ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
val print: 'a printer -> 'a option printer

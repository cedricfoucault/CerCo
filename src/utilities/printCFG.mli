
(** This module provides a function to print Control Flow Graphs. *)

(* Pasted from Pottier's PP compiler *)

open PrintPottier

val print_graph:
    (Label.t -> 'instruction -> string list) ->  (* prints instruction [i] at label [l] *)
    ('instruction -> Label.t list) ->            (* provides succcessors of instruction [i] *)
    ('instruction Label.Map.t * Label.t) printer (* control flow graph and entry point printer *)


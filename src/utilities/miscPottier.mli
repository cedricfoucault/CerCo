
(** This module provides some additional functions on lists or arithmetics. *)

(* raises Not_found *)
val pos : 'a -> 'a list -> int

val reduce : 'a list -> 'b list -> ('a list * 'a list) * ('b list * 'b list)

(* raise Failure "MiscPottier.map3" if the list arguments are not of the same
   size. *)
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

val max_list : 'a list -> 'a

val pow : int -> int -> int

val make: 'a -> int -> 'a list

val index_of : 'a -> 'a list -> int

val foldi_until : int -> (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val foldi : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(* Raises Not_found if the list is empty. *)
val last : 'a list -> 'a

(* [split l i] splits the list [l] in two lists: one with the elements
   up until the [i]th (exclusive) and one with the rest. *)
val split: 'a list -> int -> ('a list * 'a list)

(* [split_last l] returns the list [l] without its last element and its last
   element. Raises Invalid_argument "MiscPottier.split_last" if the list is
   empty. *)
val split_last : 'a list -> ('a list * 'a)

val update_list_assoc: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(* Pasted from Pottier's PP compiler *)

(* [combine] turns a pair of lists into a list of pairs. It never
   fails: the length of the output list is the minimum of the lengths
   of the input lists. *)

val combine: 'a list -> 'b list -> ('a * 'b) list

(* [subtract xs1 xs2] returns the list [xs1] deprived of as many
   elements as there are in the list [xs2]. *)

val subtract: 'a list -> 'b list -> 'a list

(* [mirror] reverses the order of the pair components in a list
   of pairs. *)

val mirror: ('a * 'b) list -> ('b * 'a) list

(* [length l] is the length of the list [l]. *)

val length: 'a list -> int32

(* [prefix k xs] returns the prefix of length [k] of the list [xs].
   If [xs] has length less than [k], [xs] is returned. *)

val prefix: int -> 'a list -> 'a list

(* [memoize f] produces a memoizing version of the function [f].
   It requires the domain of [f] to support generic equality. *)

val memoize: ('a -> 'b) -> ('a -> 'b)

(* [filter_map filter map l] returns the list [l] where elements satisfying the
   [filter] function have been replaced by their application to the [map]
   function. Elements that do not satisfy [filter] are not in the result
   list. *)

val filter_map: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list

(* [string_of_list sep f l] returns the string obtained by applying [f] to each
   element of [l] and separating their output with [sep]. *)

val string_of_list: string -> ('a -> string) -> 'a list -> string

(* [div_up a b] returns the upper bound of [a] divided by [b]. *)

val div_up : int -> int -> int

(* [make_sequence step first size] returns the [size] first terms of a sequence
   u whose first term is [first] and where for all n, u_n+1 = [step] u_n. *)

val make_sequence : ('a -> 'a) -> 'a -> int -> 'a list


(** This module provides functions to manipulate and create fresh cost
    labels. *)

include StringSig.S

(** [constant_map d x] produces a finite map which associates 
    [x] to every element of the set [d]. *)
val constant_map : Set.t -> 'a -> 'a Map.t

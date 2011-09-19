(** This module extends the standard library of Objective Caml. *)

module LexingExt : sig

  (** [new_line lexbuf] update lexbuf to increment its line 
      counter. *)
  val new_line : Lexing.lexbuf -> unit

end

module ListExt : sig

  (** [inv_assoc l] inverses a bijective associative list [l]. *)
  val inv_assoc : ('a * 'b) list -> ('b * 'a) list

  exception EmptyList

  (** [last l] returns the last element of a list. 
      Raise [EmptyList] if there is no such thing in [l]. *)
  val last : 'a list -> 'a

  (** [cut_last l] returns the last element of a list [l] and the
      elements that come before it in [l].
      Raise [EmptyList] if there is no such thing in [l]. *)
  val cut_last : 'a list -> 'a * 'a list

  (** [multi_set_of_list l] returns an associative list that 
      relates every element of [l] to its frequency in [l]. *)
  val multi_set_of_list : 'a list -> ('a * int) list

  (** [hashtbl_of_assoc l] converts an associative list into 
      an hash table. *)
  val hashtbl_of_assoc : ('a * 'b) list -> ('a, 'b) Hashtbl.t

  (** [assoc_diff l1 l2] returns the difference between two
      associative lists. *)
  val assoc_diff : ('a * 'b) list -> ('a * 'b) list 
    -> ('a * ('b option * 'b option)) list

  (** [transitive_forall2 p l] checks that the binary predicate [p] is 
      true between each successive elements of [l]. If this is false,
      the function returns the first pair of elements that falsify [p]. *)
  val transitive_forall2 : ('a -> 'a -> bool) -> 'a list -> ('a * 'a) option

end

module ArgExt : sig

  (** [extra_doc s] adds an extra line of documentation for an
      Arg.spec row. *)
  val extra_doc : string -> (Arg.key * Arg.spec * Arg.doc)

end

module SysExt : sig

  (** [safe_remove filename] deletes a file named [filename], 
      but do not crash if a system error happens. (For instance,
      if the file does not exist.) *)
  val safe_remove : string -> unit

  (** [alternative filename] finds an alternative name different
      from [filename] that is not already used. *)
  val alternative : string -> string

end



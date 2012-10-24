
(** This module implements a parser for [C] based on [gcc] and
    [CIL]. *)

(** [process ?is_lustre_file ?remove_lustre_externals filename] parses the
    contents of [filename] to obtain an abstract syntax tree that represents a
    Clight program. *)
val process :
  ?is_lustre_file:bool -> ?remove_lustre_externals:bool ->
  string -> Clight.program

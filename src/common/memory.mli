
(** This file gives a memory model that can be used by the interpreter
    of various languages throughout the compilation process and
    following the memory model of the CompCert compiler. *)

(** In the module, every size is expressed in bytes. *)


val string_of_quantity : AST.quantity -> string

val size_of_data  : AST.data -> int

val all_offsets : AST.abstract_size -> AST.abstract_offset list list


(** This is the signature of the parameter module of the functor. *)

module type DATA_SIZE =
sig
  val int_size  : int
  val ptr_size  : int
  val alignment : int option
end


(** This is the signature of the module that provides functions and types to
    manipulate memories. *)

module type S =
sig

  val int_size  : int
  val ptr_size  : int
  val alignment : int option

  val size_of_quantity : AST.quantity -> int

  module Value : Value.S

  (* Memory. A memory contains values and function definitions. Since the memory
     module will be used by the interpreters of the various languages of the
     compilation chain, the type of memory is polymorphic with the type of
     function definitions. *)

  type 'fun_def memory

  (* Memory manipulation *)

  val empty : 'fun_def memory

  (** [alloc mem size] allocates a block of [size] bytes in the memory [mem]. It
      returns the new memory and the address of the beginning of the newly
      allocated area. *)
  val alloc : 'fun_def memory -> int -> 'fun_def memory * Value.address

  (* Memory free *)

  val free : 'fun_def memory -> Value.address -> 'fun_def memory

  (* Memory load and store *)

  (** [load mem size addr] reads [size] bytes from address [addr] in memory
      [mem] and returns the value found. *)
  val load  : 'fun_def memory -> int -> Value.address -> Value.t
  val loadq : 'fun_def memory -> AST.quantity -> Value.address -> Value.t

  (** [store mem size addr v] writes the [size] first bytes (little endian
      representation) of value [v] at address [addr] in memory [mem]. *)
  val store  : 'fun_def memory -> int -> Value.address -> Value.t ->
               'fun_def memory
  val storeq : 'fun_def memory -> AST.quantity -> Value.address -> Value.t ->
               'fun_def memory

  (* Globals management *)

  (** [add_var mem x offsets init_datas] stores the datas [init_datas] of
      offsets [offsets] in a new block of memory [mem], and associates the
      global variable [x] with the address of the block. *)
  val add_var :
    'fun_def memory -> AST.ident -> AST.abstract_size -> AST.data list option ->
    'fun_def memory

  (** [add_fun_def mem f def] stores the function definition [def] in a new
      block of memory [mem], and associates the function name [f] with the
      address of the block. *)
  val add_fun_def : 'fun_def memory -> AST.ident -> 'fun_def -> 'fun_def memory

  val mem_global : 'fun_def memory -> AST.ident -> bool

  (** [find_global mem x] returns the address associated with the global symbol
      [x] in memory [mem]. [x] may be a global variable or the name of a
      function. *)
  val find_global : 'fun_def memory -> AST.ident -> Value.address

  (** [find_fun_def mem addr] returns the function definition found at address
      [addr] in memory [mem]. Raises an error if no function definition is
      found. *)
  val find_fun_def : 'fun_def memory -> Value.address -> 'fun_def


  (** [align off size] returns the aligned offsets (starting at [off]) of datas
      of size [size]. *)
  val align : int (* starting offset *) -> AST.abstract_size (* sizes *) ->
              (int list (* resulting offsets *) * int (* full size *))

  val concrete_offsets_size : AST.abstract_size -> int list * int

  val concrete_offsets : AST.abstract_size -> int list

  val concrete_size : AST.abstract_size -> int

  val concrete_offset : AST.abstract_offset -> int

(*
  val size_of_datas : AST.data list -> int

  (** [offsets_of_datas datas] returns the aligned offsets for the datas
      [datas], starting at offset 0. *)
  val offsets_of_datas : AST.data list -> (AST.data * int (* offset *)) list

  val alloc_datas : 'fun_def memory -> AST.data list ->
                    ('fun_def memory * Value.address)
*)

  val to_string : 'fun_def memory -> string
  val print : 'fun_def memory -> unit

end


(** The functor to a memory module. *)

module Make (D : DATA_SIZE) : S

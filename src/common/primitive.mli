
(** These are the functions provided by the runtime system. *)

val primitives : StringTools.Set.t

val is_primitive : string -> bool

(** Available primitives are :
extern void print_schar(signed char);
extern void print_uchar(unsigned char);
extern void print_sshort(signed short);
extern void print_ushort(unsigned short);
extern void print_sint(signed int);
extern void print_uint(unsigned int);
extern int scan_int(void);
extern int* alloc(int);
extern void newline(void);
extern void space(void);
(* The following functions only work from Clight to LIN (both included). *)
extern int rand_bool();
extern int rand_int(int, int); *)

val print_schar : string
val print_uchar : string
val print_sshort : string
val print_ushort : string
val print_sint : string
val print_uint : string
val scan_int : string
val alloc : string
val newline : string
val space : string
val rand_bool : string
val rand_int : string

val args_quantity : string -> AST.quantity list
val nb_args       : string -> int

val print_type        : AST.sig_type -> string
val print_type_return : AST.type_return -> string
val print_sig         : AST.signature -> string
val print_signedness  : AST.signedness -> string
val print_size        : AST.size -> string

module Interpret (M : Memory.S) : sig
  type res = V of M.Value.t list | A of M.Value.address
  val t : 'a M.memory -> string -> M.Value.t list -> 'a M.memory * res
end

val prototypes : string

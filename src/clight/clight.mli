(** This module defines the abstract syntax tree of [Clight]. 

    This is a (quasi-)direct translation of the Coq definition that 
    can be found in the CompCert development. *)

open AST

(** ** Types *)

type intsize = I8 | I16 | I32

type floatsize = F32 | F64

(** The syntax of type expressions.  Some points to note:
  - Array types [Tarray n] carry the size [n] of the array.
  Arrays with unknown sizes are represented by pointer types.
  - Function types [Tfunction targs tres] specify the number and types
  of the function arguments (list [targs]), and the type of the
  function result ([tres]).  Variadic functions and old-style unprototyped
  functions are not supported.
  - In C, struct and union types are named and compared by name.
  This enables the definition of recursive struct types such as
  {[
  struct s1 { int n; struct * s1 next; };
  ]}
  Note that recursion within types must go through a pointer type.
  For instance, the following is not allowed in C.
  {[
  struct s2 { int n; struct s2 next; };
  ]}
  In Clight, struct and union types [Tstruct id fields] and
  [Tunion id fields] are compared by structure: the [fields]
  argument gives the names and types of the members.  The AST_common.identifier
  [id] is a local name which can be used in conjuction with the
  [Tcomp_ptr] constructor to express recursive types.  [Tcomp_ptr id]
  stands for a pointer type to the nearest enclosing [Tstruct]
  or [Tunion] type named [id].  For instance. the structure [s1]
  defined above in C is expressed by
  {[
  Tstruct "s1" (Fcons "n" (Tint I32 Signed)
  (Fcons "next" (Tcomp_ptr "s1")
  Fnil))
  ]}
  Note that the incorrect structure [s2] above cannot be expressed at
  all, since [Tcomp_ptr] lets us refer to a pointer to an enclosing
  structure or union, but not to the structure or union directly.
  *)

type ctype =
  | Tvoid					(**r the [void] type *)
  | Tint of intsize*signedness			(**r integer types *)
  | Tfloat of floatsize				(**r floating-point types *)
  | Tpointer of ctype				(**r pointer types ([*ty]) *)
  | Tarray of ctype*int				(**r array types ([ty[len]]) *)
  | Tfunction of ctype list*ctype		(**r function types *)
  | Tstruct of ident*(ident*ctype) list
  (**r struct types *)
  | Tunion of ident*(ident*ctype) list
  (**r union types *)
  | Tcomp_ptr of ident		(**r pointer to named struct or union *)

(** ** Expressions *)

(** Arithmetic and logical operators. *)

type unary_operation =
  | Onotbool	(**r boolean negation ([!] in C) *)
  | Onotint	(**r integer complement ([~] in C) *)
  | Oneg	(**r opposite (unary [-]) *)

type binary_operation =
  | Oadd	(**r addition (binary [+]) *)
  | Osub	(**r subtraction (binary [-]) *)
  | Omul	(**r multiplication (binary [*]) *)
  | Odiv	(**r division ([/]) *)
  | Omod	(**r remainder ([%]) *)
  | Oand	(**r bitwise and ([&]) *)
  | Oor		(**r bitwise or ([|]) *)
  | Oxor	(**r bitwise xor ([^]) *)
  | Oshl	(**r left shift ([<<]) *)
  | Oshr	(**r right shift ([>>]) *)
  | Oeq		(**r comparison ([==]) *)
  | One		(**r comparison ([!=]) *)
  | Olt		(**r comparison ([<]) *)
  | Ogt		(**r comparison ([>]) *)
  | Ole		(**r comparison ([<=]) *)
  | Oge		(**r comparison ([>=]) *)

(** Clight expressions are a large subset of those of C.
  The main omissions are string literals and assignment operators
  ([=], [+=], [++], etc).  In Clight, assignment is a statement,
  not an expression.  

  All expressions are annotated with their types.  An expression
  (type [expr]) is therefore a pair of a type and an expression
  description (type [expr_descr]).
  *)

type expr =
  | Expr of expr_descr*ctype

                         and expr_descr =
  | Econst_int of int				(**r integer literal *)
  | Econst_float of float			(**r float literal *)
  | Evar of ident			        (**r variable *)
  | Ederef of expr				(**r pointer dereference (unary [*]) *)
  | Eaddrof of expr				(**r address-of operator ([&]) *)
  | Eunop of unary_operation*expr		(**r unary operation *)
  | Ebinop of binary_operation*expr*expr	(**r binary operation *)
  | Ecast of ctype*expr				(**r type cast ([(ty) e]) *)
  | Econdition of expr*expr*expr		(**r conditional ([e1 ? e2 : e3]) *)
  | Eandbool of expr*expr			(**r sequential and ([&&]) *)
  | Eorbool of expr*expr			(**r sequential or ([||]) *)
  | Esizeof of ctype				(**r size of a type *)
  | Efield of expr*ident		        (**r access to a member of a struct or union *)

  (** The following constructors are used by the annotation process only. *)

  | Ecost of CostLabel.t*expr			(**r cost label. *)
  | Ecall of ident * expr * expr


(** ** Statements *)

(** Clight statements include all C statements.
  Only structured forms of [switch] are supported; moreover,
  the [default] case must occur last.  Blocks and block-scoped declarations
  are not supported. *)

type label = Label.t

type statement =
  | Sskip					(**r do nothing *)
  | Sassign of expr*expr			(**r assignment [lvalue = rvalue] *)
  | Scall of expr option*expr*expr list		(**r function call *)
  | Ssequence of statement*statement		(**r sequence *)
  | Sifthenelse of expr*statement*statement	(**r conditional *)
  | Swhile of expr*statement	         	(**r [while] loop *)
  | Sdowhile of expr*statement		        (**r [do] loop *)
  | Sfor of statement*expr*statement*statement	(**r [for] loop *)
  | Sbreak					(**r [break] statement *)
  | Scontinue					(**r [continue] statement *)
  | Sreturn of expr option			(**r [return] statement *)
  | Sswitch of expr*labeled_statements		(**r [switch] statement *)
  | Slabel of label*statement
  | Sgoto of label
  | Scost of CostLabel.t * statement

and labeled_statements =                        (**r cases of a [switch] *)
  | LSdefault of statement
  | LScase of int*statement*labeled_statements

(** ** Functions *)

(** A function definition is composed of its return type ([fn_return]),
  the names and types of its parameters ([fn_params]), the names
  and types of its local variables ([fn_vars]), and the body of the
  function (a statement, [fn_body]). *)

type cfunction = {
  fn_return : ctype ;
  fn_params : (ident*ctype) list ;
  fn_vars : (ident * ctype) list ;
  fn_body : statement
}

(** Functions can either be defined ([Internal]) or declared as
  external functions ([External]). *)

type fundef =
  | Internal of cfunction
  | External of ident*ctype list*ctype

(** ** Programs *)

(** A program is a collection of named functions, plus a collection
  of named global variables, carrying their types and optional initialization 
  data.  See module [AST] for more details. *)

type init_data =
  | Init_int8 of int
  | Init_int16 of int
  | Init_int32 of int
  | Init_float32 of float
  | Init_float64 of float
  | Init_space of int
  | Init_addrof of ident*int  (**r address of symbol + offset *)

type program = {
  prog_funct: (ident * fundef) list ;
  prog_main: ident option;
  prog_vars: ((ident * init_data list) * ctype) list
}

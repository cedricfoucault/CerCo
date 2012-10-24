
(** These are the functions provided by the runtime system. *)


let error_prefix = "Primitives"
let error s = Error.global_error error_prefix s
let warning s = Error.warning error_prefix s

let _ = Random.self_init ()


let print_schar =
  ("print_schar", "extern void print_schar(signed char);")
let print_uchar =
  ("print_uchar", "extern void print_uchar(unsigned char);")
let print_sshort =
  ("print_sshort", "extern void print_sshort(signed short);")
let print_ushort =
  ("print_ushort", "extern void print_ushort(unsigned short);")
let print_sint =
  ("print_sint", "extern void print_sint(signed int);")
let print_uint =
  ("print_uint", "extern void print_uint(unsigned int);")
let scan_int =
  ("scan_int", "extern int scan_int(void);")
let alloc =
  ("alloc", "extern int* alloc(int);")
let newline =
  ("newline", "extern void newline(void);")
let space =
  ("space", "extern void space(void);")  
let rand_bool =
  ("rand_bool", "extern int rand_bool(void);")  
let rand_int =
  ("rand_int", "extern int rand_int(int);")  

let ident = fst

let proto = snd

let primitives_list =
  [print_schar ; print_uchar ; print_sshort ; print_ushort ;
   print_sint ; print_uint ; scan_int ; alloc ; newline ; space ;
   rand_bool ; rand_int]


let args_quantity = function
  | s when s = ident print_schar || s = ident print_uchar -> [AST.QInt 1]
  | s when s = ident print_sshort || s = ident print_ushort -> [AST.QInt 2]
  | s when s = ident print_sint || s = ident print_uint || s = ident rand_int ->
    [AST.QInt 4]
  | s when s = ident scan_int || s = ident newline || s = ident space ||
	s = ident rand_bool   ->
    []
  | s when s = ident alloc -> [AST.QPtr]
  | s -> error ("unknown primitive " ^ s ^ ".")

let nb_args = function
  | s when
      s = ident print_schar || s = ident print_uchar ||
      s = ident print_sshort || s = ident print_ushort ||
      s = ident print_sint || s = ident print_uint ||
      s = ident alloc || s = ident rand_int -> 1
  | s when s = ident scan_int || s = ident newline || s = ident space ||
	s = ident rand_bool -> 0
  | s -> error ("unknown primitive " ^ s ^ ".")


let primitives =
  List.fold_left (fun res f -> StringTools.Set.add f res) StringTools.Set.empty
    (List.map ident primitives_list)

let is_primitive f = StringTools.Set.mem f primitives


module Interpret (M : Memory.S) = struct

  type res = V of M.Value.t list | A of M.Value.address

  let print_integer_primitives =
    List.map ident
      [print_schar ; print_uchar ; print_sshort ; print_ushort ;
       print_sint ; print_uint]

  let is_print_integer_primitive f = List.mem f print_integer_primitives

  let print_integer_primitive_funs = function
    | f when f = ident print_schar ->
      (IntValue.Int8.cast, IntValue.Int8.to_signed_int_repr)
    | f when f = ident print_uchar ->
      (IntValue.Int8.cast, IntValue.Int8.to_unsigned_int_repr)
    | f when f = ident print_sshort ->
      (IntValue.Int16.cast, IntValue.Int16.to_signed_int_repr)
    | f when f = ident print_ushort ->
      (IntValue.Int16.cast, IntValue.Int16.to_unsigned_int_repr)
    | f when f = ident print_sint ->
      (IntValue.Int32.cast, IntValue.Int32.to_signed_int_repr)
    | f when f = ident print_uint ->
      (IntValue.Int32.cast, IntValue.Int32.to_unsigned_int_repr)
    | f -> error ("unknown integer printing primitive " ^ f ^ ".")

  let make_int_value vs = IntValue.Int32.merge (List.map M.Value.to_int_repr vs)

  let print_integer f mem vs =
    let (cast, to_int_repr) = print_integer_primitive_funs f in
    let i = make_int_value vs in
    let i = cast i in
    let i = to_int_repr i in
    Printf.printf "%s%!" (IntValue.print_int_repr i) ;
    (mem, V [])

  let are_ints args =
    let f res v = res && M.Value.is_int v in
    List.fold_left f true args

  let res_of_int i =
    let i = IntValue.Int32.of_int i in
    let is = IntValue.Int32.break i (4 / M.Value.int_size) in
    List.map M.Value.of_int_repr is

  let t mem f = function
    | args when is_print_integer_primitive f && are_ints args ->
      print_integer f mem args
    | _ when f = ident scan_int ->
      Printf.printf ": %!" ;
      (mem, V (res_of_int (int_of_string (read_line ()))))
    | args when f = ident alloc && are_ints args ->
      let size = IntValue.Int32.to_int (make_int_value args) in
      let (mem, addr) = M.alloc mem size in 
      (mem, A addr)
    | _ when f = ident newline ->
      Printf.printf "\n%!" ;
      (mem, V [])
    | _ when f = ident space ->
      Printf.printf " %!" ;
      (mem, V [])
    | _ when f = ident rand_bool ->
      (mem, V (res_of_int (Random.int 2)))
    | args when f = ident rand_int && are_ints args ->
      let i = IntValue.Int32.to_int (make_int_value args) in
      (mem, V (res_of_int (Random.int i)))
    | _ -> error ("unknown primitive " ^ f ^ " or bad arguments.")
end


let print_signedness = function
  | AST.Signed -> "s"
  | AST.Unsigned -> "u"

let print_size = string_of_int

let print_type = function
  | AST.Sig_int (size, sign) ->
    "int" ^ (print_size size) ^ (print_signedness sign)
  | AST.Sig_float (size, sign) ->
    "float" ^ (print_size size) ^ (print_signedness sign)
  | AST.Sig_offset -> "offset"
  | AST.Sig_ptr -> "ptr"

let print_type_return = function
  | AST.Type_ret t -> print_type t
  | AST.Type_void -> "void"

let rec print_arg_types = function
  | [] -> ""
  | t :: ts -> (print_type t) ^ " -> " ^ (print_arg_types ts)

let print_sig sg =
  Printf.sprintf "%s%s"
    (print_arg_types sg.AST.args)
    (print_type_return sg.AST.res)

let prototypes =
  let f res s = res ^ "\n" ^ s in
  (List.fold_left f "" (List.map proto primitives_list)) ^ "\n\n"


let print_schar = ident print_schar
let print_uchar = ident print_uchar
let print_sshort = ident print_sshort
let print_ushort = ident print_ushort
let print_sint = ident print_sint
let print_uint = ident print_uint
let scan_int = ident scan_int
let alloc = ident alloc
let newline = ident newline
let space = ident space
let rand_bool = ident rand_bool
let rand_int = ident rand_int

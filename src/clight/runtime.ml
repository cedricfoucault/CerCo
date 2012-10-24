
type operation =
  | Unary of Clight.unary_operation * Clight.ctype
  | Binary of Clight.binary_operation * Clight.ctype * Clight.ctype
  | Cast of Clight.ctype (* destination type *) * Clight.ctype (* source type *)
  | Fun of string (* name of the function *)

type op_replacement =
    (* operation to be replaced *)
    operation *
    (* base name of the replacement function *)
    string *
    (* C definition of the replacement function, provided a name for the
       function *)
    (string -> string) *
    (* dependences *)
    operation list


let cint size sign = Clight.Tint (size, sign)

let cints size = cint size AST.Signed
let cintu size = cint size AST.Unsigned

let cint8s = cints Clight.I8
let cint8u = cintu Clight.I8
let cint16s = cints Clight.I16
let cint16u = cintu Clight.I16
let cint32s = cints Clight.I32
let cint32u = cintu Clight.I32

let byte_size_of_intsize = function
  | Clight.I8 -> 1
  | Clight.I16 -> 2
  | Clight.I32 -> 4

let bit_size_of_intsize size = (byte_size_of_intsize size) * 8

let string_of_intsize size = string_of_int (bit_size_of_intsize size)

let ctype_of_intsize = function
  | Clight.I8 -> "char"
  | Clight.I16 -> "short"
  | Clight.I32 -> "int"


(* Unsigned divisions and modulos *)

let divumodu_fun res t s =
  "unsigned " ^ t ^ " " ^ s ^ " (unsigned " ^ t ^ " x, unsigned " ^ t ^ " y)" ^
    "{\n" ^
  "  unsigned " ^ t ^ " quo = 0;\n" ^
  "  unsigned " ^ t ^ " rem = x;\n" ^
  "  while (rem >= y) {\n" ^
  "    rem = rem - y;\n" ^
  "    quo = quo + 1;\n" ^
  "  }\n" ^
  "  return (" ^ res ^ ");\n" ^
  "}\n\n"

let divumodu op sizes sizec t =
  let (prefix, res) = match op with
    | Clight.Odiv -> ("div", "quo")
    | Clight.Omod -> ("mod", "rem")
    | _ -> assert false (* do not use on these arguments *) in
  let cu = cintu sizec in
  (Binary (op, cu, cu), "_" ^ prefix ^ sizes ^ "u", divumodu_fun res t, [])

let divu = divumodu Clight.Odiv
let modu = divumodu Clight.Omod


(* Unsigned divisions *)

(* 16 bits unsigned division *)

let div16u = divu "16" Clight.I16 "short"

(* 32 bits unsigned division *)

let div32u = divu "32" Clight.I32 "int"

(* Signed divisions *)

let divs_fun t s =
  "signed " ^ t ^ " " ^ s ^ " (signed " ^ t ^ " x, signed " ^ t ^ " y) {\n" ^
  "  unsigned " ^ t ^ " x1 = (unsigned " ^ t ^ ") x;\n" ^
  "  unsigned " ^ t ^ " y1 = (unsigned " ^ t ^ ") y;\n" ^
  "  signed " ^ t ^ " sign = 1;\n" ^
  "  if (x < 0) { x1 = (unsigned " ^ t ^ ") (-x); sign = -sign; }\n" ^
  "  if (y < 0) { y1 = (unsigned " ^ t ^ ") (-y); sign = -sign; }\n" ^
  "  return (sign * ((signed " ^ t ^ ") (x1/y1)));\n" ^
  "}\n\n"

let divs sizes sizec t =
  let cs = cints sizec in
  let cu = cintu sizec in
  (Binary (Clight.Odiv, cs, cs), "_div" ^ sizes ^ "s", divs_fun t,
   [Binary (Clight.Odiv, cu, cu)])

(* 8 bits signed division *)

let div8s = divs "8" Clight.I8 "char"

(* 16 bits signed division *)

let div16s = divs "16" Clight.I16 "short"

(* 32 bits signed division *)

let div32s = divs "32" Clight.I32 "int"


(* Unsigned modulos *)

(* 16 bits unsigned modulo *)

let mod16u = modu "16" Clight.I16 "short"

(* 32 bits unsigned modulo *)

let mod32u = modu "32" Clight.I32 "int"

(* Signed modulos *)

let mods_fun t s =
  "signed " ^ t ^ " " ^ s ^ " (signed " ^ t ^ " x, signed " ^ t ^ " y) {\n" ^
  "  return (x - (x/y) * y);\n" ^
  "}\n\n"

let mods size ct t =
  (Binary (Clight.Omod, ct, ct), "_mod" ^ size ^ "s", mods_fun t,
   [Binary (Clight.Odiv, ct, ct)])

(* 8 bits signed modulo *)

let mod8s = mods "8" cint8s "char"

(* 16 bits signed modulo *)

let mod16s = mods "16" cint16s "short"

(* 32 bits signed modulo *)

let mod32s = mods "32" cint32s "int"


(* Shifts *)

let sh_fun op t s =
  t ^ " " ^ s ^ " (" ^ t ^ " x, " ^ t ^ " y) {\n" ^
  "  " ^ t ^ " res = x, i;\n" ^
  "  for (i = 0 ; i < y ; i++) res = res " ^ op ^ " 2;\n" ^
  "  return res;\n" ^
  "}\n\n"

let sh cop sop direction deps size sign =
  let sizes = string_of_intsize size in
  let ct = Clight.Tint (size, sign) in
  let (short_sign, long_sign) = match sign with
    | AST.Signed -> ("s", "signed ")
    | AST.Unsigned -> ("u", "unsigned ") in
  let t = long_sign ^ (ctype_of_intsize size) in
  (Binary (cop, ct, ct), "_sh" ^ direction ^ sizes ^ short_sign,
   sh_fun sop t, deps)


(* Shift lefts *)

let shl = sh Clight.Oshl "*" "l" []

(* Signed shift lefts *)

(* 8 bits signed shift left *)

let shl8s = shl Clight.I8 AST.Signed

(* 16 bits signed shift left *)

let shl16s = shl Clight.I16 AST.Signed

(* 32 bits signed shift left *)

let shl32s = shl Clight.I32 AST.Signed

(* Unsigned shift lefts *)

(* 8 bits unsigned shift left *)

let shl8u = shl Clight.I8 AST.Unsigned

(* 16 bits unsigned shift left *)

let shl16u = shl Clight.I16 AST.Unsigned

(* 32 bits unsigned shift left *)

let shl32u = shl Clight.I32 AST.Unsigned


(* Shift rights *)

(* Signed shift rights *)

let shrs_fun size t s =
  "signed " ^ t ^ " " ^ s ^ " (signed " ^ t ^ " x, signed " ^ t ^ " y) {\n" ^
  "  unsigned " ^ t ^ " x1, y1, to_add, res, i;\n" ^
  "  if (y < 0) return 0;\n" ^
  "  x1 = x; y1 = y; to_add = 1; res = x1;" ^
  "  for (i = 0 ; i < " ^ size ^ " ; i++) to_add = to_add * 2;\n" ^
  "  to_add = to_add & x1;\n" ^
  "  for (i = 0 ; i < y1 ; i++) res = (res / 2) + to_add;\n" ^
  "  return ((signed " ^ t ^ ") res);\n" ^
  "}\n\n"

let shrs size =
  let sizes = string_of_int (bit_size_of_intsize size) in
  let op_sizes = string_of_int ((bit_size_of_intsize size) - 1) in
  let t = ctype_of_intsize size in
  let ct = Clight.Tint (size, AST.Signed) in
  let ctdep = Clight.Tint (size, AST.Unsigned) in
  (Binary (Clight.Oshr, ct, ct), "_shr" ^ sizes ^ "s", shrs_fun op_sizes t,
   [Binary (Clight.Odiv, ctdep, ctdep)])

(* 8 bits signed shift right *)

let shr8s = shrs Clight.I8

(* 16 bits signed shift right *)

let shr16s = shrs Clight.I16

(* 32 bits signed shift right *)

let shr32s = shrs Clight.I32

(* Unsigned shift rights *)

let shru size =
  let t_dep = Clight.Tint (size, AST.Unsigned) in
  sh Clight.Oshr "/" "r" [Binary (Clight.Odiv, t_dep, t_dep)] size AST.Unsigned

(* 8 bits unsigned shift right *)

let shr8u = shru Clight.I8

(* 16 bits unsigned shift right *)

let shr16u = shru Clight.I16

(* 32 bits unsigned shift right *)

let shr32u = shru Clight.I32

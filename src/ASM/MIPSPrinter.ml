
(** This module provides a function to print [MIPS] programs in the SPIM
    syntax. *)

(* Adapted from Pottier's PP compiler *)

open Printf
open PrintPottier
open Arch
open MIPS


let reg () r =
  sprintf "$%s" (MIPS.print_register r)

let regs () rs = MiscPottier.string_of_list " " (reg ()) rs

let load_op () = function
  | Arch.Byte -> "lb "
  | Arch.HalfWord -> "lhw"
  | Arch.Word -> "lw "

let store_op () = function
  | Arch.Byte -> "sb "
  | Arch.HalfWord -> "shw"
  | Arch.Word -> "sw "

let print_instruction () = function
    | IComment (break, c) ->
	sprintf "%s# %s" (if break then "\n" else "") c
    | INop ->
	sprintf "nop"
    | IConst (r, i) ->
	sprintf "li      %a, %d" reg r i (* pseudo-instruction *)
    | IUnOp (op, r1, r2) ->
	Arch.print_unop reg () (op, r1, r2)
    | IBinOp (op, r, r1, r2) ->
	sprintf "%s %a, %a, %a" (Arch.print_binop op) reg r reg r1 reg r2
    | ILoadAddr (rs, lab) ->
	sprintf "la      %a, %s" regs rs lab
    | ICall rf ->
	sprintf "jalr    %a" regs rf
    | ILoad (size, r1, r2) ->
	sprintf "%a     %a, %a" load_op size reg r1 regs r2
    | IStore (size, r1, r2) ->
	sprintf "%a     %a, %a" store_op size reg r2 regs r1
    | IGoto l ->
	sprintf "j       %s" l
    | IGotor rs ->
	sprintf "jr      %a" regs rs
    | IBranch (r, l) ->
	sprintf "bnez    %a, %s" reg r l
(*
    | IUnBranch (cond, r, l) ->
	sprintf "%a, %s" (Arch.print_uncon reg) (cond, r) l
    | IBinBranch (cond, r1, r2, l) ->
	sprintf "%s %a, %a, %s" (Arch.print_bincon cond) reg r1 reg r2 l
*)
    | IReturn ->
	sprintf "jr      %a" regs MIPS.ra
    | ILabel l | ICost l ->
	sprintf "%s:" l

let print_external e =
  if Primitive.is_primitive e.AST.ef_tag then "#TODO: primitive" (* TODO *)
  else
    sprintf "# extern %s: %s\n" e.AST.ef_tag (Primitive.print_sig e.AST.ef_sig)

let print_program () p =

  let externals =
    List.fold_right (fun e s -> (print_external e) ^ s) p.externals "\n"
  in

  (* Allocate space for the globals. *)

  let start =
    (print_instruction () (Arch.IComment (false, "begin preamble"))) ^ "\n" ^
    ".data\n" ^
    (if p.globals = 0 then ""
     else ("globals:\n" ^ "  .space " ^ (string_of_int p.globals) ^ "\n")) ^
    ".text\n" ^
    (if p.globals = 0 then ""
     else
       (print_instruction () (Arch.ILoadAddr (MIPS.gp, "globals")))) ^
    "\n" ^
    (print_instruction () (Arch.IGoto "main")) ^ "\n" ^
    (print_instruction () (Arch.IComment (false, "end preamble"))) ^ "\n"
  in

  (* The bulk of the code. *)

  let bulk =
    sprintf "%a" (termlist nl print_instruction) p.code
  in

  externals ^ start ^ bulk

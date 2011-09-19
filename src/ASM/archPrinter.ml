
(** This module provides a function to print [Arch] programs in the SPIM
    syntax. *)

(* Adapted from Pottier's PP compiler *)


let error_prefix = "Arch printer"
let error s = Error.global_error error_prefix s


open Printf
open PrintPottier


let load_op () = function
  | Arch.Byte -> "lb "
  | Arch.HalfWord -> "lhw"
  | Arch.Word -> "lw "

let store_op () = function
  | Arch.Byte -> "sb "
  | Arch.HalfWord -> "shw"
  | Arch.Word -> "sw "


module Make (A : Arch.ARCH) = struct

  let nl_label = "nl"
  let space_label = "space_char"

  let a0 () = List.hd A.parameters
  let v0 () = List.hd A.result

  let reg () r = "$" ^ (A.print_register r)

  let ptr () = MiscPottier.string_of_list ", " (reg ())

  let print_instruction () = function
    | Arch.IComment (break, c) ->
      sprintf "%s# %s" (if break then "\n" else "") c
    | Arch.INop ->
      sprintf "nop"
    | Arch.IConst (r, i) ->
      sprintf "li      %a, %d" reg r i (* pseudo-instruction *)
    | Arch.IUnOp (unop, r1, r2) ->
      Arch.print_unop reg () (unop, r1, r2)
    | Arch.IBinOp (binop, r, r1, r2) ->
      sprintf "%s %a, %a, %a" (Arch.print_binop binop) reg r reg r1 reg r2
    | Arch.ILoadAddr (addr, lbl) ->
      sprintf "la      %a, %s" ptr addr lbl
    | Arch.ICall addr ->
      sprintf "jalr    %a" ptr addr
    | Arch.ILoad (size, r, addr) ->
      sprintf "%a     %a, 0(%a)" load_op size reg r ptr addr
    | Arch.IStore (size, addr, r) ->
      sprintf "%a     %a, 0(%a)" store_op size reg r ptr addr
    | Arch.IGoto l ->
      sprintf "j       %s" l
    | Arch.IGotor addr ->
      sprintf "jr      %a" ptr addr
    | Arch.IBranch (r, l) ->
      sprintf "bnez    %a, %s" reg r l
    | Arch.IReturn ->
      sprintf "jr      %a" ptr A.ra
    | Arch.ISyscall ->
      "syscall"
    | Arch.ILabel l | Arch.ICost l ->
      sprintf "%s:" l

  let size_for_primitive = function
    | f when f = Primitive.print_uchar || f = Primitive.print_schar -> 1
    | f when f = Primitive.print_ushort || f = Primitive.print_sshort -> 2
    | f when f = Primitive.print_uint || f = Primitive.print_sint -> 4
    | _ -> assert false (* do not use on these arguments *)

  let asm_of_primitive = function
    | f when f = Primitive.print_uint || f = Primitive.print_sint ->
      assert (List.length A.result > 0) ;
      [Arch.ILabel f ;
       Arch.IConst (v0 (), 1) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f when f = Primitive.print_schar || f = Primitive.print_sshort ->
      assert (List.length A.parameters > 0) ;
      assert (List.length A.result > 0) ;
      let shift_amount = (A.int_size - (size_for_primitive f)) * 8 in
      [Arch.ILabel f ;
       Arch.IConst (v0 (), shift_amount) ;
       Arch.IBinOp (Arch.OpSllv, a0 (), a0 (), v0 ()) ;
       Arch.IBinOp (Arch.OpSrav, a0 (), a0 (), v0 ()) ;
       Arch.IConst (v0 (), 1) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f when f = Primitive.print_uchar || f = Primitive.print_ushort ->
      assert (List.length A.parameters > 0) ;
      assert (List.length A.result > 0) ;
      let shift_amount = (A.int_size - (size_for_primitive f)) * 8 in
      [Arch.ILabel f ;
       Arch.IConst (v0 (), shift_amount) ;
       Arch.IBinOp (Arch.OpSllv, a0 (), a0 (), v0 ()) ;
       Arch.IBinOp (Arch.OpSrlv, a0 (), a0 (), v0 ()) ;
       Arch.IConst (v0 (), 1) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f when f = Primitive.scan_int ->
      assert (List.length A.result > 0) ;
      [Arch.ILabel f ;
       Arch.IConst (v0 (), 5) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f when f = Primitive.alloc ->
      assert (List.length A.result > 0) ;
      [Arch.ILabel f ;
       Arch.IConst (v0 (), 9) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f when f = Primitive.newline ->
      assert (List.length A.result > 0) ;
      let ptr_int_size = max 1 (A.ptr_size / A.int_size) in
      assert (List.length A.parameters >= ptr_int_size) ;
      let params = MiscPottier.prefix ptr_int_size A.parameters in
      [Arch.ILabel f ;
       Arch.ILoadAddr (params, nl_label) ;
       Arch.IConst (v0 (), 4) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f when f = Primitive.space ->
      assert (List.length A.result > 0) ;
      let ptr_int_size = max 1 (A.ptr_size / A.int_size) in
      assert (List.length A.parameters >= ptr_int_size) ;
      let params = MiscPottier.prefix ptr_int_size A.parameters in
      [Arch.ILabel f ;
       Arch.ILoadAddr (params, space_label) ;
       Arch.IConst (v0 (), 4) ;
       Arch.ISyscall ;
       Arch.IReturn]
    | f -> error ("unknown primitive " ^ f)

  let print_external e =
    if Primitive.is_primitive e.AST.ef_tag then asm_of_primitive e.AST.ef_tag  
    else
      (* error ("unknown primitive " ^ e.AST.ef_tag) *)
      [Arch.IComment (true, "extern " ^ e.AST.ef_tag)]

  let print_program p =

    let externals =
      List.fold_right (fun e s -> (print_external e) @ s) p.Arch.externals []
    in

    (* Allocate space for the globals. *)

    let start =
      (print_instruction () (Arch.IComment (false, "begin preamble"))) ^ "\n" ^
	".data\n" ^
	(if p.Arch.globals = 0 then ""
	 else
	    ("globals:\n" ^ "  .space " ^
		(string_of_int p.Arch.globals) ^ "\n")) ^
	nl_label ^ ":\n" ^
	"  .asciiz \"\\n\"\n" ^
	"  .align 2\n" ^
	space_label ^ ":\n" ^
	"  .asciiz \" \"\n" ^
	"  .align 2\n" ^
	".text\n" ^
	(if p.Arch.globals = 0 then ""
	 else
	    (print_instruction () (Arch.ILoadAddr (A.gp, "globals")))) ^
	"\n" ^
	(print_instruction () (Arch.IComment (false, "end preamble"))) ^ "\n"
    in

    (* The bulk of the code. *)

    let bulk =
      sprintf "%a" (termlist nl print_instruction) (externals @ p.Arch.code)
    in

    [start ^ bulk ; "Not implemented for MIPS."]

end

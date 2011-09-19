
(** This module provides a function to print [LIN] programs. *)


let n_spaces n = String.make n ' '


let reg = Driver.TargetArch.print_register

let print_reg () = reg

let ptr rl =
  "[" ^ (MiscPottier.string_of_list " ; " reg rl) ^ "]"


let print_statement = function
  | LIN.St_skip lbl -> "goto " ^ lbl
  | LIN.St_label lbl -> lbl ^ ":"
  | LIN.St_comment s ->
    Printf.sprintf "*** %s ***" s
  | LIN.St_cost cost_lbl ->
    Printf.sprintf "emit %s" cost_lbl
  | LIN.St_int (dstr, i) ->
    Printf.sprintf "imm %s, %d" (reg dstr) i
  | LIN.St_addr (addr, id) ->
    Printf.sprintf "addr %s, %s" (ptr addr) id
  | LIN.St_unop (unop, destr, srcr) ->
    Printf.sprintf "%a" (Arch.print_unop print_reg) (unop, destr, srcr)
  | LIN.St_binop (binop, destr, srcr1, srcr2) ->
    Printf.sprintf "%s %s, %s, %s"
      (Arch.print_binop binop) (reg destr) (reg srcr1) (reg srcr2)
  | LIN.St_load (size, destr, addr) ->
    Printf.sprintf "load %s, %d[%s]" (reg destr) size (ptr addr)
  | LIN.St_store (size, addr, srcr) ->
    Printf.sprintf "load %d[%s], %s" size (ptr addr) (reg srcr)
  | LIN.St_call f -> Printf.sprintf "call %s" (ptr f)
  | LIN.St_tailcall f -> Printf.sprintf "tailcall %s" (ptr f)
  | LIN.St_cond (r, lbl_true) ->
    Printf.sprintf "branch %s <> 0, %s" (reg r) lbl_true
  | LIN.St_return -> "return"


let print_code n c =
  let f s stmt =
    Printf.sprintf "%s\n%s%s" s (n_spaces n) (print_statement stmt) in
  List.fold_left f "" c


let print_internal_decl n f def =

  Printf.sprintf
    "%s\"%s\"\n\n%s"
    (n_spaces n)
    f
    (print_code (n+2) def)


let print_external_decl n f def =
  Printf.sprintf "%sextern \"%s\": %s\n"
    (n_spaces n)
    f
    (Primitive.print_sig def.AST.ef_sig)


let print_fun_decl n (f, def) = match def with
  | LIN.F_int def -> print_internal_decl n f def
  | LIN.F_ext def -> print_external_decl n f def

let print_fun_decls n functs =
  List.fold_left (fun s f -> s ^ (print_fun_decl n f) ^ "\n\n") ""
    functs


let print_program p =
  Printf.sprintf "program:\n\n\nGlobals: %d\n\n%s"
    p.LIN.globals
    (print_fun_decls 2 p.LIN.functs)

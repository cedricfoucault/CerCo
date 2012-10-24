
(** This module provides a function to print [LTL] programs. *)


let n_spaces n = String.make n ' '


let reg = Driver.TargetArch.print_register
let print_reg () = Driver.TargetArch.print_register

let print_reg_list first last sep f rl =
  Printf.sprintf "%s%s%s"
    first (MiscPottier.string_of_list sep f rl) last

let print_ptr rl = print_reg_list "[" "]" " ; " reg rl


let print_statement = function
  | LTL.St_skip lbl -> "--> " ^ lbl
  | LTL.St_comment (s, lbl) ->
    Printf.sprintf "*** %s *** --> %s" s lbl
  | LTL.St_cost (cost_lbl, lbl) ->
    Printf.sprintf "emit %s --> %s" cost_lbl lbl
  | LTL.St_int (dstr, i, lbl) ->
    Printf.sprintf "imm %s, %d --> %s" (reg dstr) i lbl
  | LTL.St_addr (addr, id, lbl) ->
    Printf.sprintf "addr %s, %s --> %s" (print_ptr addr) id lbl
  | LTL.St_unop (unop, destr, srcr, lbl) ->
    Printf.sprintf "%a --> %s"
      (Arch.print_unop print_reg) (unop, destr, srcr) lbl
  | LTL.St_binop (binop, destr, srcr1, srcr2, lbl) ->
    Printf.sprintf "%s %s, %s, %s --> %s"
      (Arch.print_binop binop) (reg destr) (reg srcr1) (reg srcr2) lbl
  | LTL.St_load (size, destr, addr, lbl) ->
    Printf.sprintf "load %s, %d[%s] --> %s"
      (reg destr) size (print_ptr addr) lbl
  | LTL.St_store (size, addr, srcr, lbl) ->
    Printf.sprintf "store %d[%s], %s --> %s"
      size (print_ptr addr) (reg srcr) lbl
  | LTL.St_call (f, lbl) -> Printf.sprintf "call %s --> %s" (print_ptr f) lbl
  | LTL.St_tailcall f -> Printf.sprintf "tailcall %s" (print_ptr f)
  | LTL.St_cond (srcr, lbl_true, lbl_false) ->
    Printf.sprintf "branch %s <> 0 --> %s, %s" (reg srcr) lbl_true lbl_false
  | LTL.St_return -> Printf.sprintf "return"


let print_graph n c =
  let f lbl stmt s =
    Printf.sprintf "%s%s: %s\n%s"
      (n_spaces n)
      lbl
      (print_statement stmt)
      s in
  Label.Map.fold f c ""


let print_internal_decl n f def =

  Printf.sprintf
    "%s\"%s\"\n%sstacksize: %d\n%sentry: %s\n%sexit: %s\n\n%s"
    (n_spaces n)
    f
    (n_spaces (n+2))
    def.LTL.f_stacksize
    (n_spaces (n+2))
    def.LTL.f_entry
    (n_spaces (n+2))
    def.LTL.f_exit
    (print_graph (n+2) def.LTL.f_graph)


let print_external_decl n f def =
  Printf.sprintf "%sextern \"%s\": %s\n"
    (n_spaces n)
    f
    (Primitive.print_sig def.AST.ef_sig)


let print_fun_decl n (f, def) = match def with
  | LTL.F_int def -> print_internal_decl n f def
  | LTL.F_ext def -> print_external_decl n f def

let print_fun_decls n functs =
  List.fold_left (fun s f -> s ^ (print_fun_decl n f) ^ "\n\n") ""
    functs


let print_program p =
  Printf.sprintf "program:\n\n\nGlobals: %d\n\n%s"
    p.LTL.globals
    (print_fun_decls 2 p.LTL.functs)

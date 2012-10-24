
(** This module provides a function to print [RTL] programs. *)


let n_spaces n = String.make n ' '


let print_reg () r = Register.print r

let reg = Register.print

let reg_set_to_list rs =
  let f r l = l @ [r] in
  Register.Set.fold f rs []

let print_reg_list first last sep f rl =
  Printf.sprintf "%s%s%s"
    first (MiscPottier.string_of_list sep f rl) last

let print_ptr rl = print_reg_list "[" "]" " ; " reg rl

let print_args rl = print_reg_list "(" ")" ", " reg rl

let print_return rl = print_reg_list "[" "]" " ; " reg rl

let print_params rl = print_reg_list "(" ")" ", " reg rl

let print_locals rs =
  let rl = reg_set_to_list rs in
  Printf.sprintf "%s" (print_reg_list "" "" ", " reg rl)

let print_result rl = print_reg_list "[" "]" " ; " reg rl


let print_statement = function
  | RTL.St_skip lbl -> "--> " ^ lbl
  | RTL.St_cost (cost_lbl, lbl) ->
    Printf.sprintf "emit %s --> %s" cost_lbl lbl
  | RTL.St_int (dstr, i, lbl) ->
    Printf.sprintf "imm %a, %d --> %s" print_reg dstr i lbl
  | RTL.St_move (dstr, srcr, lbl) ->
    Printf.sprintf "move %a, %a --> %s" print_reg dstr print_reg srcr lbl
  | RTL.St_unop (unop, dstr, srcr, lbl) ->
    Printf.sprintf "%a --> %s"
      (Arch.print_unop print_reg) (unop, dstr, srcr) lbl
  | RTL.St_binop (binop, dstr, srcr1, srcr2, lbl) ->
    Printf.sprintf "%s %a, %a, %a --> %s"
      (Arch.print_binop binop)
      print_reg dstr print_reg srcr1 print_reg srcr2 lbl
  | RTL.St_funaddr (dstrs, id, lbl) ->
    Printf.sprintf "imm %s, %s --> %s" (print_ptr dstrs) id lbl
  | RTL.St_stackaddr (dstrs, lbl) ->
    Printf.sprintf "imm %s, STACK --> %s" (print_ptr dstrs) lbl
  | RTL.St_globaladdr (dstrs, lbl) ->
    Printf.sprintf "imm %s, GLOBAL --> %s" (print_ptr dstrs) lbl
  | RTL.St_load (size, dstr, addr, lbl) ->
    Printf.sprintf "load %a, %d[%s] --> %s"
      print_reg dstr size (print_ptr addr) lbl
  | RTL.St_store (size, addr, srcr, lbl) ->
    Printf.sprintf "store %d[%s], %a --> %s"
      size (print_ptr addr) print_reg srcr lbl
  | RTL.St_cond (srcr, lbl_true, lbl_false) ->
    Printf.sprintf "branch %a <> 0 --> %s, %s"
      print_reg srcr lbl_true lbl_false
  | RTL.St_return regs ->
    Printf.sprintf "return %s" (print_return regs)
  | RTL.St_call (addr, args, dstrs, lbl) ->
    Printf.sprintf "call %s, %s, %s --> %s"
      (print_ptr addr) (print_args args) (print_return dstrs) lbl
  | RTL.St_tailcall (addr, args) ->
    Printf.sprintf "tailcall %s, %s" (print_ptr addr) (print_args args)


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
    "%s\"%s\"%s\n%slocals: %s\n%sresult: %s\n%sstacksize: %d\n%sentry: %s\n%sexit: %s\n\n%s"
    (n_spaces n)
    f
    (print_params def.RTL.f_params)
    (n_spaces (n+2))
    (print_locals def.RTL.f_locals)
    (n_spaces (n+2))
    (print_result def.RTL.f_result)
    (n_spaces (n+2))
    def.RTL.f_stacksize
    (n_spaces (n+2))
    def.RTL.f_entry
    (n_spaces (n+2))
    def.RTL.f_exit
    (print_graph (n+2) def.RTL.f_graph)


let print_external_decl n f def =
  Printf.sprintf "%sextern \"%s\": %s\n"
    (n_spaces n)
    f
    (Primitive.print_sig def.AST.ef_sig)


let print_fun_decl n (f, def) = match def with
  | RTL.F_int def -> print_internal_decl n f def
  | RTL.F_ext def -> print_external_decl n f def

let print_fun_decls n functs =
  List.fold_left (fun s f -> s ^ (print_fun_decl n f) ^ "\n\n") ""
    functs


let print_program p =
  Printf.sprintf "program:\n\n\nglobals: %d\n\n%s"
    p.RTL.globals
    (print_fun_decls 2 p.RTL.functs)

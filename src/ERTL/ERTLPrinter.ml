
(** This module provides a function to print [ERTL] programs. *)


let n_spaces n = String.make n ' '


let print_global n (x, size) =
  Printf.sprintf "%s\"%s\" [%d]" (n_spaces n) x size

let print_globals n globs =
  Printf.sprintf "%sglobals:\n%s"
    (n_spaces n)
    (List.fold_left (fun s g -> s ^ (print_global (n+2) g) ^ "\n") "" globs)


let print_reg () r = Register.print r

let reg_set_to_list rs =
  let f r l = l @ [r] in
  Register.Set.fold f rs []

let print_reg_list first last sep f rl =
  Printf.sprintf "%s%s%s"
    first (MiscPottier.string_of_list sep f rl) last

let print_ptr rl = print_reg_list "[" "]" " ; " Register.print rl

let print_args rl = print_reg_list "(" ")" ", " Register.print rl

let print_return rl = print_reg_list "[" "]" " ; " Register.print rl

let print_params rl = print_reg_list "(" ")" ", " Register.print rl

let print_locals rs =
  let rl = reg_set_to_list rs in
  Printf.sprintf "%s" (print_reg_list "" "" ", " Register.print rl)

let print_result rl = print_reg_list "[" "]" " ; " Register.print rl


let print_statement = function
  | ERTL.St_skip lbl -> "--> " ^ lbl
  | ERTL.St_comment (s, lbl) ->
    Printf.sprintf "*** %s *** --> %s" s lbl
  | ERTL.St_cost (cost_lbl, lbl) ->
    Printf.sprintf "emit %s --> %s" cost_lbl lbl
  | ERTL.St_newframe lbl ->
    Printf.sprintf "newframe --> %s" lbl
  | ERTL.St_delframe lbl ->
    Printf.sprintf "delframe --> %s" lbl
  | ERTL.St_framesize (r, lbl) ->
    Printf.sprintf "imm %s, FRAMESIZE --> %s" (Register.print r) lbl
  | ERTL.St_get_hdw (r1, r2, lbl) ->
    Printf.sprintf "move %s, %s --> %s"
      (Register.print r1) (Driver.TargetArch.print_register r2) lbl
  | ERTL.St_set_hdw (r1, r2, lbl) ->
    Printf.sprintf "move %s, %s --> %s"
      (Driver.TargetArch.print_register r1) (Register.print r2) lbl
  | ERTL.St_hdw_to_hdw (r1, r2, lbl) ->
    Printf.sprintf "move %s, %s --> %s"
      (Driver.TargetArch.print_register r1)
      (Driver.TargetArch.print_register r2) lbl
  | ERTL.St_move (dstr, srcr, lbl) ->
    Printf.sprintf "move %s, %s --> %s"
      (Register.print dstr) (Register.print srcr) lbl
  | ERTL.St_int (dstr, i, lbl) ->
    Printf.sprintf "imm %s, %d --> %s" (Register.print dstr) i lbl
  | ERTL.St_unop (unop, dstr, srcr, lbl) ->
    Printf.sprintf "%a --> %s"
      (Arch.print_unop print_reg) (unop, dstr, srcr) lbl
  | ERTL.St_binop (binop, dstr, srcr1, srcr2, lbl) ->
    Printf.sprintf "%s %s, %s, %s --> %s"
      (Arch.print_binop binop)
      (Register.print dstr) (Register.print srcr1) (Register.print srcr2) lbl
  | ERTL.St_addrN (dstr, id, n, lbl) ->
    Printf.sprintf "addr(%d) %s, %s --> %s" n (Register.print dstr) id lbl
  | ERTL.St_load (size, dstr, addr, lbl) ->
    Printf.sprintf "load %a, %d[%s] --> %s"
      print_reg dstr size (print_ptr addr) lbl
  | ERTL.St_store (size, addr, srcr, lbl) ->
    Printf.sprintf "store %d[%s], %a --> %s"
      size (print_ptr addr) print_reg srcr lbl
  | ERTL.St_call (f, nb_args, lbl) ->
    Printf.sprintf "call %s, %d --> %s" (print_ptr f) nb_args lbl
  | ERTL.St_tailcall (f, nb_args) ->
    Printf.sprintf "tailcall %s, %d" (print_ptr f) nb_args
  | ERTL.St_cond (srcr, lbl_true, lbl_false) ->
    Printf.sprintf "branch %s <> 0 --> %s, %s"
      (Register.print srcr) lbl_true lbl_false
  | ERTL.St_return ret_regs ->
    Printf.sprintf "return %s" (print_return ret_regs)


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
    "%s\"%s\" %d\n%slocals: %s\n%sstacksize: %d\n%sentry: %s\n%sexit: %s\n\n%s"
    (n_spaces n)
    f
    def.ERTL.f_params
    (n_spaces (n+2))
    (print_locals def.ERTL.f_locals)
    (n_spaces (n+2))
    def.ERTL.f_stacksize
    (n_spaces (n+2))
    def.ERTL.f_entry
    (n_spaces (n+2))
    def.ERTL.f_exit
    (print_graph (n+2) def.ERTL.f_graph)


let print_external_decl n f def =
  Printf.sprintf "%sextern \"%s\": %s\n"
    (n_spaces n)
    f
    (Primitive.print_sig def.AST.ef_sig)


let print_fun_decl n (f, def) = match def with
  | ERTL.F_int def -> print_internal_decl n f def
  | ERTL.F_ext def -> print_external_decl n f def

let print_fun_decls n functs =
  List.fold_left (fun s f -> s ^ (print_fun_decl n f) ^ "\n\n") ""
    functs


let print_program p =
  Printf.sprintf "program:\n\n\nGlobal: %d\n\n%s"
    p.ERTL.globals
    (print_fun_decls 2 p.ERTL.functs)

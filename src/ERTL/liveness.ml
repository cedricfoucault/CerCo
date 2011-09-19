(* Pasted from Pottier's PP compiler *)

open ERTL

(* In the following, a ``variable'' means a pseudo-register or an
   allocatable hardware register. *)

(* These functions allow turning an [ERTL] control flow graph into an
   explicit graph, that is, making successor edges explicit. This is
   useful in itself and facilitates the computation of predecessor
   edges. *)

let statement_successors (stmt : statement) =
  match stmt with
  | St_tailcall _
  | St_return _ ->
    Label.Set.empty
  | St_skip l
  | St_comment (_, l)
  | St_cost (_, l)
  | St_newframe l
  | St_delframe l
  | St_framesize (_, l)
  | St_set_hdw (_, _, l)
  | St_get_hdw (_, _, l)
  | St_hdw_to_hdw (_, _, l)
  | St_move (_, _, l)
  | St_int (_, _, l)
  | St_addrN (_, _, _, l)
  | St_unop (_, _, _, l)
  | St_binop (_, _, _, _, l)
  | St_load (_, _, _, l)
  | St_store (_, _, _, l)
  | St_call (_, _, l) ->
    Label.Set.singleton l
  | St_cond (_, l1, l2) ->
    Label.Set.add l1 (Label.Set.singleton l2)

(* The analysis uses the lattice of sets of variables. The lattice's
   join operation is pointwise set union, which reflects the fact that
   a variable is deemed live at a program point if and only if it is
   live at any of the successors of that program point. *)

module L = struct

  (* A set of variable is represented as a pair of a set of
     pseudo-registers and a set of hardware registers. *)

  type t =
      Register.Set.t * Driver.TargetArch.RegisterSet.t

  type property =
      t

  let bottom =
    Register.Set.empty, Driver.TargetArch.RegisterSet.empty

  let psingleton r =
    Register.Set.singleton r, Driver.TargetArch.RegisterSet.empty

  let pset set = set, Driver.TargetArch.RegisterSet.empty

  let plist psds =
    Register.Set.of_list psds, Driver.TargetArch.RegisterSet.empty

  let hsingleton hwr =
    Register.Set.empty, Driver.TargetArch.RegisterSet.singleton hwr

  let hset set = Register.Set.empty, set

  let hlist hwrs =
    Register.Set.empty, Driver.TargetArch.RegisterSet.of_list hwrs

  let join (rset1, hwrset1) (rset2, hwrset2) =
    (Register.Set.union rset1 rset2, Driver.TargetArch.RegisterSet.union hwrset1 hwrset2)

  let diff (rset1, hwrset1) (rset2, hwrset2) =
    (Register.Set.diff rset1 rset2, Driver.TargetArch.RegisterSet.diff hwrset1 hwrset2)

  let equal (rset1, hwrset1) (rset2, hwrset2) =
    Register.Set.equal rset1 rset2 && Driver.TargetArch.RegisterSet.equal hwrset1 hwrset2

  let is_maximal _ =
    false

end

module Label_ImperativeMap = struct

  type key =
      Label.Map.key
  
  type 'data t =
      'data Label.Map.t ref
      
  let create () =
    ref Label.Map.empty

  let clear t =
    t := Label.Map.empty
    
  let add k d t =
    t := Label.Map.add k d !t

  let find k t =
    Label.Map.find k !t

  let iter f t =
    Label.Map.iter f !t

end

module F = Fix.Make (Label_ImperativeMap) (L)

(* These are the sets of variables defined at (written by) a statement. *)

let defined (stmt : statement) : L.t =
  match stmt with
  | St_skip _
  | St_comment _
  | St_cost _
  | St_store _
  | St_cond _
  | St_tailcall _
  | St_return _ ->
    L.bottom
  | St_get_hdw (r, _, _)
  | St_framesize (r, _)
  | St_move (r, _, _)
  | St_int (r, _, _)
  | St_addrN (r, _, _, _)
  | St_unop (_, r, _, _)
  | St_binop (_, r, _, _, _)
  | St_load (_, r, _, _) ->
    L.psingleton r
  | St_set_hdw (r, _, _)
  | St_hdw_to_hdw (r, _, _) ->
    L.hsingleton r
  | St_call _ ->
      (* Potentially destroys all caller-save hardware registers. *)
    Register.Set.empty, Driver.TargetArch.caller_saved
  | St_newframe _
  | St_delframe _ ->
    L.hlist Driver.TargetArch.sp

(* This is the set of variables used at (read by) a statement. *)

let used (stmt : statement) : L.t =
  match stmt with
  | St_skip _
  | St_comment _
  | St_cost _
  | St_framesize _
  | St_addrN _
  | St_int _ ->
    L.bottom
  | St_call (addr, nparams, _)
  | St_tailcall (addr, nparams) ->
    (* Reads the hardware registers that are used to pass parameters. *)
    L.join
      (L.plist addr)
      (L.hlist (MiscPottier.prefix nparams Driver.TargetArch.parameters))
  | St_get_hdw (_, r, _)
  | St_hdw_to_hdw (_, r, _) ->
    L.hsingleton r
  | St_set_hdw (_, r, _)
  | St_move (_, r, _)
  | St_unop (_, _, r, _)
  | St_cond (r, _, _) ->
    L.psingleton r
  | St_binop (_, _, r1, r2, _) ->
    L.join (L.psingleton r1) (L.psingleton r2)
  | St_load (_, _, addr, _) ->
    L.plist addr
  | St_store (_, addr, r, _) ->
    L.join (L.plist addr) (L.psingleton r)
  | St_newframe _
  | St_delframe _ ->
    L.hlist Driver.TargetArch.sp
  | St_return _ ->
    L.join
      (L.join
	 (L.hset Driver.TargetArch.callee_saved)
	 (L.hlist Driver.TargetArch.result))
      (L.hlist Driver.TargetArch.ra)

(* A statement is considered pure if it has no side effect, that is, if
   its only effect is to write a value to its destination variable.

   A pure statement whose destination is dead after the statement will
   be eliminated during the translation of [ERTL] to [LTL]. This is done by
   replacing the statement with an [St_skip] statement.

   [eliminable liveafter stmt] returns [Some l], where [l] is [stmt]'s single
   successor, if statement [stmt] is eliminable. Otherwise, it returns
   [None]. The parameter [liveafter] is the set of variables that are live
   after the statement. *)

let eliminable ((pliveafter, hliveafter) : L.t) (stmt : statement) =
  match stmt with
  | St_skip _
  | St_comment _
  | St_cost _
  | St_newframe _
  | St_delframe _
  | St_store _
  | St_call _
  | St_tailcall _
  | St_cond _
  | St_return _ ->
    None
  | St_get_hdw (r, _, l)
  | St_framesize (r, l)
  | St_int (r, _, l)
  | St_addrN (r, _, _, l)
  | St_move (r, _, l)
  | St_unop (_, r, _, l)
  | St_binop (_, r, _, _, l)
  | St_load (_, r, _, l) ->
    if (Register.Set.mem r pliveafter) then None else Some l
  | St_set_hdw (r, _, l)
  | St_hdw_to_hdw (r, _, l) ->
    if Driver.TargetArch.RegisterSet.mem r hliveafter then None else Some l


(* This is the abstract semantics of instructions. It defines the
   variables that are live before the instruction in terms of
   those that are live after the instruction. *)

(* The standard definition is: a variable is considered live
   before the instruction if either (1) it is used by the instruction,
   or (2) it is live after the instruction and not defined by the
   instruction.

   As an exception to this rule, if the instruction is eliminable,
   then a variable is considered live before the instruction
   if and only if it is live after the instruction. This anticipates
   on the instruction's elimination.

   This exception means that the source variables of a pure
   instruction need not be considered live if the instruction's result
   is unused. This allows a sequence of pure instructions whose end
   result is dead to be considered entirely dead.

   It is a simple, but not entirely trivial, exercise to check that
   this transfer function is monotone. *)

let statement_semantics (stmt : statement) (liveafter : L.t) : L.t =
  match eliminable liveafter stmt with
  | None ->
      L.join (L.diff liveafter (defined stmt)) (used stmt)
  | Some _ ->
      liveafter

(* A valuation is a function that maps a program point (a control flow
   graph label) to the set of variables that are live after that
   point. *)

type valuation =
    Label.t -> L.t

(* This is how we turn an [ERTL] procedure into a liveness analysis
   problem and solve it. *)

let analyze (int_fun : internal_function) : valuation =

  (* Formulate the problem. Construct a system (recursive) equations
     that describe the live variables before and after each
     instruction. *)

  (* The following two functions, [livebefore] and [liveafter],
     define these equations. Both use an oracle, a valuation --
     also called [liveafter] -- which is supposed to tell us
     which variables are live after each label. *)

  (* The live variables before an instruction are computed, using the
     instruction's semantics, in terms of the live variables after the
     instruction -- which are given by the oracle. *)

  let livebefore label (liveafter : valuation) : L.t =
    let stmt : statement = Label.Map.find label int_fun.f_graph in
    statement_semantics stmt (liveafter label)
  in

  (* The live variables after an instruction are the union of the live
     variables before each of the instruction's successors. *)

  let liveafter label (liveafter : valuation) : L.t =
    let stmt : statement = Label.Map.find label int_fun.f_graph in
    Label.Set.fold (fun successor accu ->
      L.join (livebefore successor liveafter) accu
    ) (statement_successors stmt) L.bottom
  in

  (* Compute the least fixed point of these recursive equations. *)

  F.lfp liveafter

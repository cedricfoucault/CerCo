
(** This module provides a function to distinguish between the [zero]
    register and other registers. *)

(* Pasted from Pottier's PP compiler *)

(* On the MIPS architecture, the hardware register [$zero] is special. It can
   be written with arbitrary values, but, when it is read, it always contains
   the value [0].

   This register is useful when the value [0] is desired. Instead of
   explicitly loading the constant [0] into another register, which increases
   pressure on the register allocator, one simply uses [$zero].

   The simplest way of achieving this effect is as follows. We view register
   [$zero] as allocatable, so the register allocator does not know that it is
   special. However, when building the interference graph, we let every
   pseudo-register [r] interfere with [$zero] unless we can prove that the
   values stored into [r] are always [0]. In that particular case, it is safe
   to allocate the hardware register [$zero] for [r].

   This approach leads us to emitting [LTL] instructions of the form
   [li $zero, 0], which we eliminate on the fly.

   We say that [r] is zeroable when it is safe to allocate [$zero] for it, and
   nonzeroable otherwise. *)

(* If [i] can store a non-zero value into pseudo-register [r], then
   [nonzeroable i] returns the singleton [r], otherwise it returns the
   empty set. *)

val nonzeroable: ERTL.statement -> Register.Set.t


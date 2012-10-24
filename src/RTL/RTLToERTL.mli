
(** This module provides a translation of [RTL] programs to [ERTL] programs. *)

(** The work consists in expliciting the calling convention.

    Function call instructions will be expanded so has to explicitely pass
    arguments on dedicated physical registers, and then on specific places in
    the stack if there are not enough registers.

    We add a prologue at the beginning of each function that does the following
    things:
    - Allocate enough space on the stack for the function to execute.
    - Save the return address.
    - Save callee-saved registers.
    - Fetch parameters from physical registers and potentially from the stack.

    Also, we add an epilogue which pretty much reverses the effects of the
    prologue:
    - Save the return value in physical registers that can only be used for
      this.
    - Restore callee-saved register.
    - Restore the return address.
    - Free the allocated space on the stack.
    - Assign the return value to the physical return registers. *)

val translate : RTL.program -> ERTL.program

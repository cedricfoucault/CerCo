
(** This module defines the target architecture and instanciates the memory
    functor for each intermediate language. *)

module DataSize32 =
struct
  let alignment = Some 4
  let int_size = 4
  let ptr_size = 4
end

(* The target architecture: MIPS. *)

module TargetArch = TargetArch.Make (MIPS)

module Memory32         = Memory.Make (DataSize32)
module TargetArchMemory = Memory.Make (TargetArch)

module ClightMemory = Memory32
module CminorMemory = Memory32
module RTLabsMemory = Memory32
module RTLMemory    = TargetArchMemory
module ERTLMemory   = TargetArchMemory
module LTLMemory    = TargetArchMemory
module LINMemory    = TargetArchMemory

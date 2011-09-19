
(** This module defines the target architecture and instanciates the memory
    functor for each intermediate language. *)

module TargetArch : TargetArch.S

module TargetArchMemory : Memory.S

module ClightMemory : Memory.S
module CminorMemory : Memory.S
module RTLabsMemory : Memory.S
(* module RTLMemory    : Memory.S *)
(* module ERTLMemory   : Memory.S *)
(* module LTLMemory    : Memory.S *)
(* module LINMemory    : Memory.S *)

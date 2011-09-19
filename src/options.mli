(** This module defines the compiler general options. *)

(** {2 Source language} *)
val set_source_language : string -> unit
val get_source_language : unit -> Languages.name

(** {2 Target language} *)
val set_target_language : string -> unit
val get_target_language : unit -> Languages.name

(** {2 Interpretation request} *)
val request_interpretation   : bool -> unit
val interpretation_requested : unit -> bool

(** {2 Interpretation requests} *)
val request_interpretations   : bool -> unit
val interpretations_requested : unit -> bool

(** {2 Annotation requests} *)
val request_annotation   : bool -> unit
val annotation_requested : unit -> bool

(** {2 Input files} *)
val add_input_file : string -> unit
val input_files	   : unit -> string list

(** {2 Output files} *)
val set_output_files : string -> unit
val get_output_files : unit -> string option

(** {2 Verbose mode} *)
val is_debug_enabled : unit -> bool

(** {2 Lustre file} *)
val set_lustre_file : bool -> unit
val is_lustre_file  : unit -> bool

(** {2 Remove Lustre externals} *)
val set_remove_lustre_externals : bool -> unit
val is_remove_lustre_externals  : unit -> bool

(** {2 Lustre file and test requested} *)
val set_lustre_test : string -> unit
val get_lustre_test : unit -> string option

(** {2 Lustre file: number of test cases} *)
val set_lustre_test_cases : int -> unit
val get_lustre_test_cases : unit -> int

(** {2 Lustre file: number of cycles for each case} *)
val set_lustre_test_cycles : int -> unit
val get_lustre_test_cycles : unit -> int

(** {2 Lustre file: random int minimum value} *)
val set_lustre_test_min_int : int -> unit
val get_lustre_test_min_int : unit -> int

(** {2 Lustre file: random int maximum value} *)
val set_lustre_test_max_int : int -> unit
val get_lustre_test_max_int : unit -> int

(*
(** {2 Print results requests} *)
val is_print_result_enabled : unit -> bool
*)

(** {2 Developers' playground} *)
val is_dev_test_enabled : unit -> bool

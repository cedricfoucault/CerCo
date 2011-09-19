
(** [add_lustre_main lustre_test lustre_test_cases lustre_test_cycles
    lustre_test_min_int lustre_test_max_int ast] adds a main function that tests
    a Lustre step function to a Clight AST. The file [lustre_test] contains
    CerCo information (e.g. the name of the cost variable). The integer
    [lustre_test_cases] is the number of test cases that are performed, and the
    integer [lustre_test_cycles] is the number of cycles per test
    case. [lustre_test_min_int] (resp. [lustre_test_max_int]) is the minimum
    (resp. maximum) integer value randomly generated during testing, and. *)

val add : string -> int -> int -> int -> int -> Clight.program -> Clight.program


(** This module defines the labelling of a [Cminor] program. *)

(** [add_cost_labels prog] inserts some labels to enable
    cost annotation. 

    The labelling of a function proceeds as follows:

    - A label is added at the beginning of the function.

    - For each branching instruction in the function, a cost label is added at
      the beginning of each branch. The concerned instructions are:
      - ternary expressions;
      - conditionals;
      - loops;
      - switches.

    - For each label instruction in the function, a cost label is added after
      the label, in order to capture loops potentially created by gotos.
*)

val add_cost_labels : Cminor.program -> Cminor.program

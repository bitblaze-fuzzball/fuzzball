(**
The functions in this module implement comparison between Vine IR
values of various types. For a value of type Vine.x, you can use a
function Vine_compare.compare_x in place of OCaml's usual polymorphic
"compare" function. For compatibility, these functions compare in the
same order as the polymorphic compare (first by variant, than by field
lexicographically). Some OCaml programmers perfer to avoid polymorphic
comparison because it breaks abstraction barriers and can fail at
runtime. These problems are not as severe for the Vine IR as they
might be for some other data structures. However we probably do have
some subtle dependencies on this ordering in the simplification rules
in Vine_opt depending on the way in which subexpressions are ordered
among operators like +. If in the future we were to use these versions
exclusively, we could separate that dependence from the physical
representation. Using these functions is also sometimes faster than
the polymorphic compare: these implementations have to do extra
branching because they are implemented using pattern matching, but
they are specialized by type and don't require crossing the OCaml/C
boundary.
*)

val compare_endian : Vine.endian -> Vine.endian -> int

val compare_typ : Vine.typ -> Vine.typ -> int

val compare_var : Vine.var -> Vine.var -> int

val compare_cast_type : Vine.cast_type -> Vine.cast_type -> int

val compare_fcast_type : Vine.fcast_type -> Vine.fcast_type -> int

val compare_binop_type : Vine.binop_type -> Vine.binop_type -> int

val compare_fbinop_type : Vine.fbinop_type -> Vine.fbinop_type -> int

val compare_unop_type : Vine.unop_type -> Vine.unop_type -> int

val compare_funop_type : Vine.funop_type -> Vine.funop_type -> int

val compare_round_mode : Vine_util.round_mode -> Vine_util.round_mode -> int

val compare_value : Vine.value -> Vine.value -> int

val compare_attribute : Vine.attribute -> Vine.attribute -> int

val compare_lvalue : Vine.lvalue -> Vine.lvalue -> int

val compare_exp : Vine.exp -> Vine.exp -> int

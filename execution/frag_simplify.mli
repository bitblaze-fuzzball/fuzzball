(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val constant_fold_rec : Vine.exp -> Vine.exp

val simplify_rec : Vine.exp -> Vine.exp

val expr_size : Vine.exp -> int
val stmt_size : Vine.stmt -> int

val rm_unused_stmts : Vine.stmt list -> Vine.stmt list

val simplify_frag : Vine.program -> Vine.program

(*
  Copyright (C) BitBlaze, 2009-2012. All rights reserved.
*)

val constant_fold_rec : Vine.exp -> Vine.exp

val simplify_rec : Vine.exp -> Vine.exp

val simplify_fp : Vine.exp -> Vine.exp

val expr_size : Vine.exp -> int
val stmt_size : Vine.stmt -> int

val rm_unused_stmts : Vine.stmt list -> Vine.stmt list

val simplify_frag : Vine.program -> Vine.program

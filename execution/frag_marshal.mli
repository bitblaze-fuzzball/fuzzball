(*
  Copyright (C) BitBlaze, 2010-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module VarByInt :
sig
  type t = Vine.var
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

val free_var : Vine.var -> unit

val encode_exp : Vine.exp -> (string * Vine.var list)
val encode_printable_exp : Vine.exp -> string
val decode_exp : string -> Vine.exp


(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
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
val decode_exp : string -> Vine.exp


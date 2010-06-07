(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val fuzz : int64 -> int64 -> int64 list -> Fragment_machine.fragment_machine
  -> Asmir.varctx -> (unit -> unit) -> unit

(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val trans_cache : (int64, Vine.program) Hashtbl.t

val runloop : Fragment_machine.fragment_machine
  -> int64 -> Asmir.varctx -> (int64 -> bool) -> unit

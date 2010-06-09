(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val check_memory_usage : Fragment_machine.fragment_machine ->
  (int64, Vine.program) Hashtbl.t -> unit


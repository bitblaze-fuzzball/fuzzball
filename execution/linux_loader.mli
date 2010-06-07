(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val load_dynamic_program : Fragment_machine.fragment_machine
  -> string -> int64 -> bool -> bool ->
  (int64 * int64) list -> string list -> int64

val load_core : Fragment_machine.fragment_machine -> string -> int64

val setup_tls_segment : Fragment_machine.fragment_machine 
  -> int64 -> int64 -> unit

val proc_identities : (int * int * int * int) option ref

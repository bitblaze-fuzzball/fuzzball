(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val load_mem_state : Fragment_machine.fragment_machine -> string -> int64

val state_loader_cmdline_opts : (string * Arg.spec * string) list

val apply_state_loader_cmdline_opts : Fragment_machine.fragment_machine -> unit


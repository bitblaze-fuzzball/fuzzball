(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val set_linux_defaults_for_concrete : unit -> unit
val linux_cmdline_opts : (string * Arg.spec * string) list
val apply_linux_cmdline_opts : Fragment_machine.fragment_machine -> unit

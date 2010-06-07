(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val set_defaults_for_concrete : unit -> unit

val cmdline_opts              : (string * Arg.spec * string) list
val trace_replay_cmdline_opts : (string * Arg.spec * string) list

val set_program_name : string -> unit

val apply_cmdline_opts : Fragment_machine.fragment_machine
  -> Vine.decl list -> unit

val make_symbolic_init : Fragment_machine.fragment_machine -> (unit -> unit)

val decide_start_addrs : unit -> (int64 * int64)

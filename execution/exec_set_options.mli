(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val set_defaults_for_concrete : unit -> unit

val cmdline_opts                : (string * Arg.spec * string) list
val influence_cmdline_opts      : (string * Arg.spec * string) list
val concrete_state_cmdline_opts : (string * Arg.spec * string) list
val symbolic_state_cmdline_opts : (string * Arg.spec * string) list
val concolic_state_cmdline_opts : (string * Arg.spec * string) list
val explore_cmdline_opts        : (string * Arg.spec * string) list
val tags_cmdline_opts           : (string * Arg.spec * string) list
val fuzzball_cmdline_opts       : (string * Arg.spec * string) list
val trace_replay_cmdline_opts   : (string * Arg.spec * string) list

val set_program_name : string -> unit

val default_on_missing : (Fragment_machine.fragment_machine -> unit) ref

val apply_cmdline_opts_early : Fragment_machine.fragment_machine
  -> Vine.decl list -> unit

val apply_cmdline_opts_late : Fragment_machine.fragment_machine -> unit

val apply_cmdline_opts_nonlinux : Fragment_machine.fragment_machine -> unit

val make_symbolic_init : Fragment_machine.fragment_machine 
  -> Exec_no_influence.influence_manager -> (unit -> unit)

val decide_start_addrs : unit -> (int64 * int64)

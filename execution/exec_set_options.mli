(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val opt_tracepoint_strings : (int64 * string) list ref
val opt_string_tracepoint_strings : (int64 * string) list ref
val opt_check_condition_at_strings : (string * string) list ref
val opt_extra_condition_strings : string list ref
val opt_svn_version : bool ref

val opt_initial_eax : int64 option ref
val opt_initial_ebx : int64 option ref
val opt_initial_ecx : int64 option ref
val opt_initial_edx : int64 option ref
val opt_initial_esi : int64 option ref
val opt_initial_edi : int64 option ref
val opt_initial_esp : int64 option ref
val opt_initial_ebp : int64 option ref
val opt_initial_eflagsrest : int64 option ref
val opt_store_bytes : (int64 * int64) list ref
val opt_store_shorts : (int64 * int64) list ref
val opt_store_words : (int64 * int64) list ref
val opt_store_longs : (int64 * int64) list ref

val opt_symbolic_regions : (int64 * int64) list ref
val opt_symbolic_strings : (int64 * int64) list ref
val opt_symbolic_cstrings : (int64 * int64) list ref
val opt_symbolic_cstrings_fulllen : (int64 * int64) list ref
val opt_symbolic_string16s : (int64 * int64) list ref
val opt_symbolic_regs : bool ref
val opt_symbolic_bytes : (int64 * string) list ref
val opt_symbolic_shorts : (int64 * string) list ref
val opt_symbolic_words : (int64 * string) list ref
val opt_symbolic_longs : (int64 * string) list ref
val opt_sink_regions : (string * int64) list ref
val opt_measure_expr_influence_at_strings : (string * string) option ref

val opt_concolic_strings : (int64 * string) list ref
val opt_concolic_cstrings : (int64 * string) list ref
val opt_concolic_cstring_files : (int64 * string) list ref

val opt_fuzz_start_addr : int64 option ref

val opt_symbolic_bytes_influence : (int64 * string) list ref
val opt_symbolic_shorts_influence : (int64 * string) list ref
val opt_symbolic_words_influence : (int64 * string) list ref
val opt_symbolic_longs_influence : (int64 * string) list ref

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

val set_program_name_guess_arch : string -> unit
val require_explicit_arch : unit -> unit

val default_on_missing : (Fragment_machine.fragment_machine -> unit) ref

val apply_cmdline_opts_early : Fragment_machine.fragment_machine
  -> Vine.decl list -> unit

val apply_cmdline_opts_late : Fragment_machine.fragment_machine -> unit

val apply_cmdline_opts_nonlinux : Fragment_machine.fragment_machine -> unit

val make_symbolic_init : Fragment_machine.fragment_machine 
  -> Exec_no_influence.influence_manager -> (unit -> unit)

val decide_start_addrs : unit -> (int64 * int64)

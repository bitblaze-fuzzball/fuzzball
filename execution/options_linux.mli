(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val opt_core_file_name : string option ref
val opt_use_ids_from_core : bool ref
val opt_setup_initial_proc_state : bool option ref
val opt_load_extra_regions : (int64 * int64) list ref
val opt_load_data : bool ref
val opt_load_base : int64 option ref
val opt_tls_base : int64 option ref
val opt_linux_syscalls : bool ref
val opt_symbolic_files : string list ref

val set_linux_defaults_for_concrete : unit -> unit
val linux_cmdline_opts : (string * Arg.spec * string) list
val apply_linux_cmdline_opts : Fragment_machine.fragment_machine -> unit

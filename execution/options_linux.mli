(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val set_linux_defaults_for_concrete : unit -> unit
val linux_cmdline_opts : (string * Arg.spec * string) list
val apply_linux_cmdline_opts : Fragment_machine.fragment_machine -> unit

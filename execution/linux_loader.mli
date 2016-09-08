(*
  Copyright (C) BitBlaze, 2009-2012. All rights reserved.
*)

val load_x87_emulator : Fragment_machine.fragment_machine -> string -> int64

val load_dynamic_program : Fragment_machine.fragment_machine
  -> string -> int64 -> bool -> bool ->
  (int64 * int64) list -> string list -> int64

val load_core : Fragment_machine.fragment_machine -> string -> int64

val setup_tls_segment : Fragment_machine.fragment_machine 
  -> int64 -> int64 -> unit

val opt_hwcap : int64 option ref

val proc_identities : (int * int * int * int) option ref

val addr_to_io : string -> int64 -> IO.input


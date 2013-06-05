(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val check_memory_usage : Fragment_machine.fragment_machine ->
  (int64, Vine.program) Hashtbl.t -> unit

val final_check_memory_usage : unit -> unit

val periodic_stats : Fragment_machine.fragment_machine ->
  bool -> bool -> unit

val add_periodic_hook : Fragment_machine.fragment_machine -> int64 -> unit

(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val check_memory_usage : Fragment_machine.fragment_machine ->
  (int64, Vine.program) Hashtbl.t -> unit

val final_check_memory_usage : unit -> unit

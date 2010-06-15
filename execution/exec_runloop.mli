(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

val runloop : Fragment_machine.fragment_machine
  -> int64 -> Asmir.varctx -> (int64 -> bool) -> unit

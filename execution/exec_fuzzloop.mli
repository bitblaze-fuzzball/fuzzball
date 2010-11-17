(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

val fuzz : int64 -> int64 -> int64 list -> Fragment_machine.fragment_machine
  -> Asmir.varctx -> (unit -> unit) -> (unit -> unit) -> unit

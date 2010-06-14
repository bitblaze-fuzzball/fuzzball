(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

class stp_external_engine : string -> object
  inherit Query_engine.query_engine

  method prepare : Vine.var list -> Vine.var list -> unit
  method assert_eq : Vine.var -> Vine.exp -> unit
  method query : Vine.exp -> (bool option) * ((string * int64) list)
  method unprepare : bool -> unit
end

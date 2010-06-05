(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

class stp_external_engine : string -> object
  inherit Query_engine.query_engine

  method prepare : Vine.var list -> Vine.var list -> unit
  method assert_eq : Vine.var -> Vine.exp -> unit
  method query : Vine.exp -> (bool option) * ((string * int64) list)
  method unprepare : bool -> unit
end

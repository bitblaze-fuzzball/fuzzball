(*
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

class stp_external_engine : string -> object
  inherit Query_engine.query_engine

  method start_query : unit
  method add_decl : Query_engine.qe_decl -> unit
  method add_condition : Vine.exp -> unit
  method push : unit
  method pop : unit
  method query : Vine.exp -> (bool option) * Query_engine.sat_assign
  method after_query : bool -> unit
  method reset : unit
end

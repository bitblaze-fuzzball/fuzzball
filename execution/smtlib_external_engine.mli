(*
  Based on stp_external_engine.mli, which bears the following notice:
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

class smtlib_external_engine : string -> object
  inherit Query_engine.query_engine

  method start_query : unit
  method add_free_var : Vine.var -> unit
  method add_temp_var : Vine.var -> unit
  method assert_eq : Vine.var -> Vine.exp -> unit
  method add_condition : Vine.exp -> unit
  method push : unit
  method pop : unit
  method query : Vine.exp -> (bool option) * ((string * int64) list)
  method after_query : bool -> unit
  method reset : unit
end

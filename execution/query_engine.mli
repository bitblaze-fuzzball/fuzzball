(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

class virtual query_engine : object
  method virtual prepare : Vine.var list -> Vine.var list -> unit
  method virtual assert_eq : Vine.var -> Vine.exp -> unit
  method virtual query : Vine.exp -> (bool option) * ((string * int64) list)
  method virtual unprepare : bool -> unit
end

val print_ce : (string * int64) list -> unit

class parallel_check_engine : query_engine -> query_engine -> object
  inherit query_engine
  method prepare : Vine.var list -> Vine.var list -> unit
  method assert_eq : Vine.var -> Vine.exp -> unit
  method query : Vine.exp -> (bool option) * ((string * int64) list)
  method unprepare : bool -> unit
end

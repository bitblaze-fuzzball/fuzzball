(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

(* An extra numeric counter for debugging purposes like query
   filenames. *)
val query_extra_counter : int ref

(* Satisfying assignment, AKA counterexample: a mapping from variable
   names to bit-vector values. For historical and brevity reasons, the
   code mostly abbreviates this as "ce" for "counterexample". *)
type sat_assign

val ce_from_list : (string * int64) list -> sat_assign
val ce_lookup_nf : sat_assign -> string -> int64
val ce_iter : sat_assign -> (string -> int64 -> unit) -> unit

class virtual query_engine : object
  method virtual start_query : unit
  method virtual add_free_var : Vine.var -> unit
  method virtual add_temp_var : Vine.var -> unit
  method prepare : Vine.var list -> Vine.var list -> unit
  method virtual assert_eq : Vine.var -> Vine.exp -> unit
  method virtual add_condition : Vine.exp -> unit
  method virtual push : unit
  method virtual pop : unit
  method virtual query : Vine.exp -> (bool option) * sat_assign
  method virtual after_query : bool -> unit
  method virtual reset : unit
  method unprepare : bool -> unit
end

class dummy_query_engine : object
  inherit query_engine
  method start_query : unit
  method add_free_var : Vine.var -> unit
  method add_temp_var : Vine.var -> unit
  method assert_eq : Vine.var -> Vine.exp -> unit
  method add_condition : Vine.exp -> unit
  method push : unit
  method pop : unit
  method query : Vine.exp -> (bool option) * sat_assign
  method after_query : bool -> unit
  method reset : unit
end

val print_ce : sat_assign -> unit

class parallel_check_engine : query_engine -> query_engine -> object
  inherit query_engine
  method start_query : unit
  method add_free_var : Vine.var -> unit
  method add_temp_var : Vine.var -> unit
  method assert_eq : Vine.var -> Vine.exp -> unit
  method add_condition : Vine.exp -> unit
  method push : unit
  method pop : unit
  method query : Vine.exp -> (bool option) * sat_assign
  method after_query : bool -> unit
  method reset : unit
end

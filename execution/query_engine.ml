(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

module V = Vine;;

class virtual query_engine = object(self)
  method virtual start_query : unit
  method virtual add_free_var : V.var -> unit
  method virtual add_temp_var : V.var -> unit

  method prepare free_vars temp_vars =
    self#start_query;
    List.iter self#add_free_var free_vars;
    List.iter self#add_temp_var temp_vars

  method virtual assert_eq : V.var -> V.exp -> unit
  method virtual add_condition : V.exp -> unit
  method virtual push : unit
  method virtual pop : unit
  method virtual query : V.exp -> (bool option) * ((string * int64) list)
  method virtual after_query : bool -> unit
  method virtual reset : unit

  method unprepare save =
    self#after_query save;
    self#reset
end

let no s = failwith (s ^ " called on dummy_query_engine")

class dummy_query_engine = object(self)
  inherit query_engine

  method start_query = no "start_query"
  method add_free_var v = no "add_free_var"
  method add_temp_var v = no "add_temp_var"
  method assert_eq v e = no "assert_eq"
  method add_condition e = no "add_condition"
  method push = no "push"
  method pop = no "pop"
  method query e = no "query"
  method after_query b = no "after_query"
  method reset = no "reset"
end

let print_ce ce =
  List.iter
    (fun (var_s, value) ->
       if value <> 0L then
	 Printf.printf "%s=0x%Lx " var_s value)
    ce;
  Printf.printf "\n";

class parallel_check_engine (e1:query_engine) (e2:query_engine) = object(self)
  inherit query_engine

  val mutable eqns = []
  val mutable conds = []

  method start_query =
    e1#start_query;
    e2#start_query

  method add_free_var var =
    e1#add_free_var var;
    e2#add_free_var var

  method add_temp_var var =
    e1#add_temp_var var;
    e2#add_temp_var var

  method assert_eq var rhs =
    e1#assert_eq var rhs;
    e2#assert_eq var rhs;
    eqns <- (var, rhs) :: eqns

  method add_condition e =
    e1#add_condition e;
    e2#add_condition e;
    conds <- e :: conds

  method push =
    e1#push;
    e2#push

  method pop =
    e1#pop;
    e2#pop

  method query e =
    let string_of_result r = match r with
      | None -> "failure"
      | Some true -> "unsat"
      | Some false -> "sat" in
    let (r1, ce1) = e1#query e and
	(r2, ce2) = e2#query e in
      if r1 <> r2 then
	(Printf.printf "Solver result mismatch:\n";
	 List.iter
	   (fun (var, rhs) ->
	      Printf.printf "%s := %s\n" (V.var_to_string var)
		(V.exp_to_string rhs)) (List.rev eqns);
	 List.iter
	   (fun e -> Printf.printf "%s AND\n" (V.exp_to_string e))
	   (List.rev conds);
	 Printf.printf "in query %s\n" (V.exp_to_string e);
	 Printf.printf "Solver 1 says %s, solver 2 says %s\n"
	   (string_of_result r1) (string_of_result r2);
	 (match r1 with
	    | Some false ->
		Printf.printf "Solver 1's assignment is:\n";
		print_ce ce1;
	    | _ -> ());
	 (match r2 with
	    | Some false ->
		Printf.printf "Solver 2's assignment is:\n";
		print_ce ce2;
	    | _ -> ());
	 (None, []))	  
      else
	(r1, ce1)

  method after_query save_results =
    e1#after_query save_results;
    e2#after_query save_results

  method reset =
    e1#reset;
    e2#reset;
    eqns <- [];
    conds <- []
end

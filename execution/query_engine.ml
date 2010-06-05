(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

class virtual query_engine = object(self)
  method virtual prepare : V.var list -> V.var list -> unit
  method virtual assert_eq : V.var -> V.exp -> unit
  method virtual query : V.exp -> (bool option) * ((string * int64) list)
  method virtual unprepare : bool -> unit
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

  method prepare free_vars temp_vars =
    e1#prepare free_vars temp_vars;
    e2#prepare free_vars temp_vars;
    eqns <- []

  method assert_eq var rhs =
    e1#assert_eq var rhs;
    e2#assert_eq var rhs;
    eqns <- (var, rhs) :: eqns

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
		(V.exp_to_string rhs)) eqns;
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

  method unprepare save_results =
    e1#unprepare save_results;
    e2#unprepare save_results;
    eqns <- []
end

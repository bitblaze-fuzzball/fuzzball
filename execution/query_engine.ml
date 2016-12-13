(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

module V = Vine;;

let query_extra_counter = ref 0

type sat_assign = ((string * int64) list) * (string, int64) Hashtbl.t

let ce_from_list l =
  let n = List.length l in
  let h = Hashtbl.create n in
    List.iter (fun (s, i) -> Hashtbl.replace h s i) l;
    (l, h)

let ce_lookup_nf (_, h) s = Hashtbl.find h s

let ce_iter (l,_) f = List.iter (fun (s, v) -> f s v) l

(* Each one of these represents one named object we want to tell a
   solver about. They're in one data-type so that we can have a list
   of them in a dependency-respecting order. *)
type qe_decl =
  | InputVar of V.var (* variable with no particular known value *)
  | TempVar of V.var * V.exp (* variable used as shorthand for an expression *)
  | TempArray of V.var * (V.exp list) (* like TempVar, but array type *)

class virtual query_engine = object(self)
  method virtual start_query : unit
  method virtual add_decl : qe_decl -> unit
  method virtual add_condition : V.exp -> unit
  method virtual push : unit
  method virtual pop : unit
  method virtual query : V.exp -> (bool option) * sat_assign
  method virtual after_query : bool -> unit
  method virtual reset : unit
end

let no s = failwith (s ^ " called on dummy_query_engine")

class dummy_query_engine = object(self)
  inherit query_engine

  method start_query = no "start_query"
  method add_decl d = no "add_decl"
  method add_condition e = no "add_condition"
  method push = no "push"
  method pop = no "pop"
  method query e = no "query"
  method after_query b = no "after_query"
  method reset = no "reset"
end

let print_ce ce =
  let rec is_all_digits s pos len =
    if pos = len then true
    else
      match s.[pos] with
	| '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' ->
	    is_all_digits s (pos + 1) len
	| _ -> false
  in
    ce_iter ce
    (fun var_s value ->
       let is_tmp =
	 if String.sub var_s 0 1 = "t" then
	   is_all_digits var_s 1 (String.length var_s)
	 else
	   false
       in
       if value <> 0L && not is_tmp then
	 Printf.printf "%s=0x%Lx " var_s value);
  Printf.printf "\n";

class parallel_check_engine (e1:query_engine) (e2:query_engine) = object(self)
  inherit query_engine

  val mutable eqns = []
  val mutable conds = []

  method start_query =
    e1#start_query;
    e2#start_query

  method add_decl decl =
    e1#add_decl decl;
    e2#add_decl decl

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
	 (None, ce_from_list []))
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

(*
  Based on stp_external_engine.ml, which bears the following notice:
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;
open Query_engine;;
open Stpvc_engine;;
open Smt_lib2;;

let output_string_log log_file channel str =
  if !opt_save_solver_files then
    output_string log_file str;
  output_string channel str

let rename_var name =
  let new_name = ref "" in
    for i = 0 to (String.length name) - 1 do
      match name.[i] with
        | '_' -> new_name := !new_name ^ "-"
        | '-' -> new_name := !new_name ^ "_"
	| '|' -> ()
        | _ -> new_name := !new_name ^ (Char.escaped name.[i])
    done;
    !new_name

let map_lines f chan =
  let results = ref [] in
    (try while true do
       match (f (input_line chan)) with
	 | Some x -> results := x :: !results
	 | None -> ()
     done
     with End_of_file -> ());
    List.rev !results

let parse_counterex line =
  if line = "sat" then
    raise End_of_file
  else
    (assert((String.sub line 0 8) = "ASSERT( ");
     assert((String.sub line ((String.length line) - 3) 3) = " );");
     let trimmed = String.sub line 8 ((String.length line) - 11) in
     let eq_loc = String.index trimmed '=' in
     let lhs = String.sub trimmed 0 eq_loc and
	 rhs = (String.sub trimmed (eq_loc + 1)
		  ((String.length trimmed) - eq_loc - 1)) in
       assert((String.sub lhs ((String.length lhs) - 1) 1) = " "
	   || (String.sub lhs ((String.length lhs) - 1) 1) = "<");
       let lhs_rtrim =
	 if (String.sub lhs ((String.length lhs) - 2) 1) = " " then
	   2 else 1
       in
       let rhs_rtrim =
	 if (String.sub rhs ((String.length rhs) - 1) 1) = " " then
	   1 else 0
       in
       let varname = rename_var
	 (String.sub lhs 0 ((String.length lhs) - lhs_rtrim))
       in
       let value =
	 let rhs' = String.sub rhs 0 ((String.length rhs) - rhs_rtrim) in
	 let len = String.length rhs' in
	   (Int64.of_string
	      (if len >= 6 && (String.sub rhs' 0 5) = " 0hex" then
		 ("0x" ^ (String.sub rhs' 5 (len - 5)))
	       else if len >= 4 && (String.sub rhs' 0 3) = " 0x" then
		 ("0x" ^ (String.sub rhs' 3 (len - 3)))
	       else if len >= 4 && (String.sub rhs' 0 3) = " 0b" then
		 ("0b" ^ (String.sub rhs' 3 (len - 3)))
	       else if rhs' = ">FALSE" then
		 "0"
	       else if rhs' = ">TRUE" then
		 "1"
	       else
		 failwith "Failed to parse value in counterexample"))
       in
	 Some (varname, value))


class smtlib_external_engine fname = object(self)
  inherit query_engine

  val mutable visitor = None
  val mutable first_query = true
  val mutable solver_chans =
    let timeout_opt = match !opt_solver_timeout with
      | Some s -> "-g " ^ (string_of_int s) ^ " "
      | None -> ""
    in
      Unix.open_process (!opt_stp_path ^ " --SMTLIB2 -p " ^ timeout_opt)

  val mutable log_file =
    if !opt_save_solver_files then
      open_out "solver_input.smt"
    else
      stdout

  method private visitor =
    match visitor with
      | Some v -> v
      | None -> failwith "Missing visitor in smtlib_external_engine"

  method start_query =
    ()

  method add_free_var var =
    if first_query then self#real_prepare;
    self#real_add_free_var var

  method private real_add_free_var var =
    self#visitor#declare_var var

  method add_temp_var var =
    ()

  method assert_eq var rhs =
    if first_query then self#real_prepare;
    self#real_assert_eq (var, rhs)

  method add_condition e =
    if first_query then self#real_prepare;
    let (solver_in, solver_out) = solver_chans
    and visitor = (self#visitor :> V.vine_visitor) in
      output_string_log log_file solver_out "(assert ";
      ignore(V.exp_accept visitor e);
      output_string_log log_file solver_out ")\n"

  method push =
    let (solver_in, solver_out) = solver_chans in
      if first_query then self#real_prepare;
      output_string_log log_file solver_out "\n(push 1)\n"

  method pop =
    let (solver_in, solver_out) = solver_chans in
      output_string_log log_file solver_out "(pop 1)\n\n"

  method private real_assert_eq (var, rhs) =
    try
      self#visitor#declare_var_value var rhs
    with
      | V.TypeError(err) ->
	  Printf.printf "Typecheck failure on %s: %s\n"
	    (V.exp_to_string rhs) err;
	  failwith "Typecheck failure in assert_eq"

  method private real_prepare =
    let (solver_in, solver_out) = solver_chans in
      visitor <- Some(new Smt_lib2.vine_smtlib_print_visitor
                       (output_string_log log_file solver_out));
      output_string_log log_file solver_out
	"(set-logic QF_BV)\n(set-info :smt-lib-version 2.0)\n\n";
      first_query <- false

  method query qe =
    if first_query then
      self#real_prepare;
    let (solver_in, solver_out) = solver_chans in
    let visitor = (self#visitor :> V.vine_visitor) in
      output_string_log log_file solver_out "(assert ";
      ignore(V.exp_accept visitor qe);
      output_string_log log_file solver_out ")\n";
      output_string_log log_file solver_out "(check-sat)\n";
      flush solver_out;
      if !opt_save_solver_files then flush log_file;
      let result_s = input_line solver_in in
      let first_assert = (String.sub result_s 0 3) = "ASS" in
      let result = match result_s with
       | "unsat" -> Some true
       | "Timed Out." -> Printf.printf "STP timeout\n"; None
       | "sat" -> Some false
       | _ when first_assert -> Some false
       | _ -> failwith ("Unexpected first output line " ^ result_s)
      in
      let first_assign = if first_assert then
         [(match parse_counterex result_s with
         | Some ce -> ce | None -> failwith "Unexpected parse failure")]
      else
        [] in
      let ce =
	if (result = Some false) && first_assert then
          map_lines parse_counterex solver_in
	else
          []
      in
        (result, first_assign @ ce)

  method after_query save_results =
    if save_results then
      Printf.printf "Solver queries are in solver_input.smt\n"

  method reset =
    let (solver_in, solver_out) = solver_chans
    and timeout_opt = match !opt_solver_timeout with
      | Some s -> "-g " ^ (string_of_int s) ^ " "
      | None -> ""
    in
      output_string_log log_file solver_out "(exit)\n\n\n\n\n";
      ignore(Unix.close_process solver_chans);
      solver_chans <-
       Unix.open_process (!opt_stp_path ^ " --SMTLIB2 -p " ^ timeout_opt);
      first_query <- true;
      visitor <- None;

end

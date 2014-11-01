(*
  Based on stp_external_engine.ml, which bears the following notice:
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;
open Query_engine;;
open Solvers_common;;
open Smt_lib2;;

let output_string_log log_file channel str =
  if !opt_save_solver_files then
    output_string log_file str;
  output_string channel str

let parse_counterex e_s_t line =
  match parse_ce e_s_t line with
    | No_CE_here -> None
    | End_of_CE -> raise End_of_file
    | Assignment(s, i) -> Some (s, i)

class smtlib_external_engine solver = object(self)
  inherit query_engine

  val mutable visitor = None
  val mutable first_query = true
  val mutable solver_chans =
    if solver = CVC4 then
      let timeout_opt = match !opt_solver_timeout with
	| Some s -> "--tlimit-per " ^ (string_of_int s) ^ "000 "
	| None -> ""
      in
        Unix.open_process (!opt_solver_path ^ " --lang smt -im " ^ timeout_opt)
    else
      let timeout_opt = match !opt_solver_timeout with
	| Some s -> "-g " ^ (string_of_int s) ^ " "
	| None -> ""
      in
        Unix.open_process (!opt_solver_path ^ " --SMTLIB2 -p " ^ timeout_opt)

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
	| "unknown" -> Printf.printf "Solver timeout\n"; None
	| "sat" -> Some false
	| _ when first_assert -> Some false
	| _ -> failwith ("Unexpected first output line " ^ result_s)
      in
      let first_assign = if first_assert then
	  [(match parse_counterex solver result_s with
	  | Some ce -> ce | None -> failwith "Unexpected parse failure")]
	else
	  [] in
      let ce =
	if (result = Some false) && first_assert then
	  map_lines (parse_counterex solver) solver_in
	else if (result = Some false) && solver = CVC4 then
	  (output_string_log log_file solver_out "(get-model)\n";
	  flush solver_out;
	  if !opt_save_solver_files then flush log_file;
	  map_lines (parse_counterex solver) solver_in)
	else
          []
      in
        (result, first_assign @ ce)

  method after_query save_results =
    if save_results then
      Printf.printf "Solver queries are in solver_input.smt\n"

  method private reset_solver_chans =
    solver_chans <-
      if solver = CVC4 then
	let timeout_opt = match !opt_solver_timeout with
	  | Some s -> "--tlimit-per " ^ (string_of_int s) ^ " "
	  | None -> ""
	in
          Unix.open_process (!opt_solver_path ^ " --lang smt -im " ^ timeout_opt)
      else
	let timeout_opt = match !opt_solver_timeout with
	  | Some s -> "-g " ^ (string_of_int s) ^ " "
	  | None -> ""
	in
          Unix.open_process (!opt_solver_path ^ " --SMTLIB2 -p " ^ timeout_opt)

  method reset =
    let (solver_in, solver_out) = solver_chans in
      output_string_log log_file solver_out "(exit)\n\n\n\n\n";
      ignore(Unix.close_process solver_chans);
      self#reset_solver_chans;
      first_query <- true;
      visitor <- None;

end

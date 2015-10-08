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
    | Assignment(s, i, is_final) -> Some (s, i)

let parse_stateful_ce fn chan =
  let results = ref [] and
      var_state = ref None
  in
    (try while true do
       match fn (input_line chan) !var_state with
	 | (End_of_CE, _) -> raise End_of_file
	 | (No_CE_here, v') ->
	     var_state := v'
	 | (Assignment(s, i, is_final), v') ->
	     results := (s, i) :: !results;
	     var_state := v';
	     if is_final then
	       raise End_of_file
     done
     with End_of_file -> ());
    List.rev !results

let parse_z3_ce = parse_stateful_ce parse_z3_ce_line

let parse_mathsat_ce = parse_stateful_ce parse_mathsat_ce_line

let start_solver solver =
  let path = !opt_solver_path in
  let timeout_opt =
    match !opt_solver_timeout with
      | None -> ""
      | Some s ->
	  (match solver with
	     | CVC4 -> "--tlimit-per " ^ (string_of_int s) ^ "000 "
	     | (STP_SMTLIB2|STP_CVC) -> "-g " ^ (string_of_int s) ^ " "
	     | BOOLECTOR -> "-t " ^ (string_of_int s) ^ " "
	     | Z3 -> "-t:" ^ (string_of_int s) ^ "000 "
	     | MATHSAT -> failwith "No timeout option for mathsat")
  in
  let cmd =
    match solver with
      | CVC4 -> path ^ " --lang smt -im " ^ timeout_opt
      | STP_SMTLIB2 -> path ^ " --SMTLIB2 -p " ^ timeout_opt
      | Z3 -> path ^ " -smt2 -in " ^ timeout_opt
      | MATHSAT -> path ^ timeout_opt
      | _ -> failwith "Unsupported solver in smtlib_external_engine"
  in
    Unix.open_process cmd


class smtlib_external_engine solver = object(self)
  inherit query_engine

  val mutable visitor = None
  val mutable first_query = true

  val mutable solver_chans = start_solver solver

  val mutable log_file =
    if !opt_save_solver_files then
      open_out "solver_input.smt2"
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
    self#visitor#assert_exp e

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
    let logic = match solver with
      | (Z3|MATHSAT) -> "QF_FPBV"
      | _ -> "QF_BV"
    in
      visitor <- Some(new Smt_lib2.vine_smtlib_printer
			(output_string_log log_file solver_out));
      output_string_log log_file solver_out ("(set-logic " ^ logic ^ ")\n");
      output_string_log log_file solver_out
	"(set-info :smt-lib-version 2.0)\n\n";
      if solver = MATHSAT then
	output_string_log log_file solver_out
	  "(set-option :produce-models true)\n\n";
      first_query <- false

  method query qe =
    if first_query then
      self#real_prepare;
    let (solver_in, solver_out) = solver_chans in
      self#visitor#assert_exp qe;
      output_string_log log_file solver_out "(check-sat)\n";
      if solver = MATHSAT then
	for i = 1 to 8192 do
	  output_char solver_out ' '
	done;
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
      let ce_list =
	if (result = Some false) && first_assert then
	  map_lines (parse_counterex solver) solver_in
	else if (result = Some false) &&
	  (solver = CVC4 || solver = Z3 || solver = MATHSAT)
	then
	  (output_string_log log_file solver_out "(get-model)\n";
	   if solver = MATHSAT then
	     for i = 1 to 8192 do
	       output_char solver_out ' '
	     done;
	   flush solver_out;
	   if !opt_save_solver_files then flush log_file;
	   if solver = Z3 then
	     parse_z3_ce solver_in
	   else if solver = MATHSAT then
	     parse_mathsat_ce solver_in
	   else
	     map_lines (parse_counterex solver) solver_in)
	else
          []
      in
        (result, ce_from_list (first_assign @ ce_list))

  method after_query save_results =
    if save_results then
      Printf.printf "Solver queries are in solver_input.smt\n"

  method private reset_solver_chans =
    solver_chans <- start_solver solver

  method reset =
    let (solver_in, solver_out) = solver_chans in
      output_string_log log_file solver_out "(exit)\n\n\n\n\n";
      ignore(Unix.close_process solver_chans);
      self#reset_solver_chans;
      first_query <- true;
      visitor <- None;

end

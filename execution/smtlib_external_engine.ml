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

let output_string_log log_file_opt channel str =
  (match log_file_opt with
     | Some log_file -> output_string log_file str;
     | None -> ());
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


class smtlib_external_engine solver fname = object(self)
  inherit query_engine

  val mutable visitor = None
  val mutable first_query = true

  val mutable solver_chans = start_solver solver

  val mutable temp_dir = None
  val mutable filenum = 0
  val mutable curr_fname = fname

  method private get_temp_dir =
    match temp_dir with
      | Some t -> t
      | None ->
	  let t = create_temp_dir "fuzzball-solver-log" in
	    temp_dir <- Some t;
	    t

  method private get_fresh_fname =
    let dir = self#get_temp_dir in
      filenum <- filenum + 1;
      curr_fname <- pick_fresh_fname dir fname filenum;
      if !opt_trace_solver then
	Printf.printf "Creating SMTLIB2 log file: %s.smt2\n" curr_fname;
      curr_fname

  val mutable log_chan = None

  method private puts out_chan s =
    output_string_log log_chan out_chan s

  method private flush_log =
    match log_chan with
      | Some chan -> flush chan
      | None -> ()

  method private visitor =
    match visitor with
      | Some v -> v
      | None -> failwith "Missing visitor in smtlib_external_engine"

  method start_query =
    ()

  method private real_add_free_var var =
    self#visitor#declare_var var

  method add_condition e =
    if first_query then self#real_prepare;
    self#visitor#assert_exp e

  method add_decl d =
    if first_query then self#real_prepare;
    match d with
      | InputVar(v) -> self#real_add_free_var v
      | TempVar(v, e) -> self#real_assert_eq (v, e)
      | TempArray(v, el) -> self#real_add_table v el

  method push =
    let (solver_in, solver_out) = solver_chans in
      if first_query then self#real_prepare;
      self#puts solver_out "\n(push 1)\n"

  method pop =
    let (solver_in, solver_out) = solver_chans in
      self#puts solver_out "(pop 1)\n\n"

  method private real_assert_eq (var, rhs) =
    try
      self#visitor#declare_var_value var rhs
    with
      | V.TypeError(err) ->
	  Printf.printf "Typecheck failure on %s: %s\n"
	    (V.exp_to_string rhs) err;
	  failwith "Typecheck failure in assert_eq"

  method private real_add_table var el =
    self#visitor#declare_var var;
    self#visitor#assert_array_contents var el

  method private real_prepare =
    let (solver_in, solver_out) = solver_chans in
      visitor <- Some(new Smt_lib2.vine_smtlib_printer
			(self#puts solver_out));
      if !opt_save_solver_files then
	(let log_fname = self#get_fresh_fname in
	   log_chan <- Some(open_out (log_fname ^ ".smt2")));
      (match choose_smtlib_logic solver with
	 | Some logic ->
	     self#puts solver_out
	       ("(set-logic "^logic^")\n");
	 | None -> ());
      self#puts solver_out "(set-info :smt-lib-version 2.0)\n\n";
      if solver = MATHSAT then
	self#puts solver_out "(set-option :produce-models true)\n\n";
      first_query <- false

  method query qe =
    if first_query then
      self#real_prepare;
    let (solver_in, solver_out) = solver_chans in
      self#visitor#assert_exp qe;
      self#puts solver_out "(check-sat)\n";
      if solver = MATHSAT then
	for i = 1 to 8192 do
	  output_char solver_out ' '
	done;
      flush solver_out;
      self#flush_log;
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
	  (self#puts solver_out "(get-model)\n";
	   if solver = MATHSAT then
	     for i = 1 to 8192 do
	       output_char solver_out ' '
	     done;
	   flush solver_out;
	   self#flush_log;
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
      if !opt_save_solver_files then
	Printf.printf "Solver queries are in %s\n" curr_fname
      else
	Printf.printf "Turn on -save-solver-files to save the query\n"

  method private reset_solver_chans =
    solver_chans <- start_solver solver

  method reset =
    let (solver_in, solver_out) = solver_chans in
      self#puts solver_out "(exit)\n\n\n\n\n";
      ignore(Unix.close_process solver_chans);
      self#reset_solver_chans;
      first_query <- true;
      visitor <- None;

end

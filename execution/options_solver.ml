(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;
open Solvers_common;;

let opt_solver = ref "stp-external"
let opt_solver_check_against = ref "none"

let opt_smtlib_solver_type = ref None

let solver_cmdline_opts =
  [
    ("-solver", Arg.Set_string(opt_solver),
     "solver smtlib, smtlib-batch, stpvc (internal) or stp-external");
    ("-solver-check-against", Arg.Set_string(opt_solver_check_against),
     "solver Compare solver results with the given one");
    ("-solver-path", Arg.Set_string(opt_solver_path),
     "path Location of external SMT solver binary");
    ("-stp-path", Arg.Set_string(opt_solver_path),
     "path Former name of -solver-path");
    ("-smtlib-solver-type", Arg.String
       (function
	  | "stp" -> opt_smtlib_solver_type := Some STP_SMTLIB2
	  | "cvc4" -> opt_smtlib_solver_type := Some CVC4
	  | "btor"|"boolector" -> opt_smtlib_solver_type := Some BOOLECTOR
	  | "z3" -> opt_smtlib_solver_type := Some Z3
	  | "mathsat" -> opt_smtlib_solver_type := Some MATHSAT
	  | _ -> failwith "Unrecognized -smtlib-solver-type"
       ),
     "type stp,cvc4,btor,z3 (default is guessed from path)");
    ("-save-solver-files", Arg.Set(opt_save_solver_files),
     " Retain solver input and output files");
    ("-solver-slow-time", Arg.Set_float(opt_solver_slow_time),
     "secs Save queries that take longer than SECS");
    ("-solver-timeout", Arg.String
       (fun s -> opt_solver_timeout := Some (int_of_string s)),
     "secs Run each query for at most SECS seconds");
    ("-timeout-as-unsat", Arg.Set(opt_timeout_as_unsat),
       " Treat solver timeouts the same as \"unsat\" results");
    ("-trace-assigns", Arg.Set(opt_trace_assigns),
     " Print satisfying assignments");
    ("-trace-assigns-string", Arg.Set(opt_trace_assigns_string),
     " Print satisfying assignments as a string");
    ("-trace-solver", Arg.Set(opt_trace_solver),
     " Print calls to decision procedure");
    ("-solver-stats", Arg.Set(opt_solver_stats),
     " Print solver statistics");
    ("-nonfatal-solver", Arg.Set(opt_nonfatal_solver),
     " Keep going even if the solver fails/crashes");
  ]

let ends_with long suffix =
  let long_len = String.length long and
      suffix_len = String.length suffix
  in
  if long_len < suffix_len then
    false
  else
    (String.sub long (long_len - suffix_len) suffix_len) = suffix

let solvers_table = 
  (let h = Hashtbl.create 7 in
     Hashtbl.replace h "none" (fun _ -> None);
     Hashtbl.replace h "stpvc"
       (fun _ ->
	  Some (new Stpvc_engine.stpvc_engine :> Query_engine.query_engine));
     Hashtbl.replace h "stp-external"
       (fun s ->
	  Some (new Stp_external_engine.stp_external_engine ("fuzz" ^ s)));
     Hashtbl.replace h "smtlib-batch"
       (fun s ->
	  let stype = match !opt_smtlib_solver_type with
	    | Some s -> s
	    | None ->
		if ends_with !opt_solver_path "stp" then
		  STP_SMTLIB2
		else if ends_with !opt_solver_path "cvc4" then
		  CVC4
		else if ends_with !opt_solver_path "boolector" then
		  BOOLECTOR
		else if ends_with !opt_solver_path "z3" then
		  Z3
		else if ends_with !opt_solver_path "mathsat" then
		  MATHSAT
		else
		  failwith "Please specify -smtlib-solver-type"
	  in
	    Some (new Smtlib_batch_engine.smtlib_batch_engine
		    stype ("fuzz" ^ s)));
     Hashtbl.replace h "smtlib"
       (fun s ->
	  let stype = match !opt_smtlib_solver_type with
	    | Some BOOLECTOR ->
		failwith "Boolector does not support incremental solving"
	    | Some s -> s
	    | None ->
		if ends_with !opt_solver_path "stp" then
		  STP_SMTLIB2
		else if ends_with !opt_solver_path "cvc4" then
		  CVC4
		else if ends_with !opt_solver_path "boolector" then
		  failwith "Boolector does not support incremental solving"
		else if ends_with !opt_solver_path "z3" then
		  Z3
		else if ends_with !opt_solver_path "mathsat" then
		  MATHSAT
		else
		  failwith "Please specify -smtlib-solver-type"
	  in
	    Some (new Smtlib_external_engine.smtlib_external_engine stype));
     h)

let construct_solver suffix =
  let checking_solver_opt =
    try (Hashtbl.find solvers_table !opt_solver_check_against)
      ("-check" ^ suffix)
    with Not_found -> failwith "Unknown solver for -solver-check-against"
  in
  let main_solver_opt =
    try (Hashtbl.find solvers_table !opt_solver) suffix
    with Not_found -> failwith "Unknown -solver"
  in
  let main_solver = match main_solver_opt with
    | Some s -> s
    | None -> failwith "-solver none is not supported"
  in
    match checking_solver_opt with
      | None -> main_solver
      | Some cs -> new Query_engine.parallel_check_engine main_solver cs
	  
let apply_solver_cmdline_opts (fm : Fragment_machine.fragment_machine) =
  fm#set_query_engine (construct_solver "")

  

(*
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;

let opt_solver = ref "stp-external"
let opt_solver_check_against = ref "none"

let solver_cmdline_opts =
  [
    ("-solver", Arg.Set_string(opt_solver),
     "solver stpvc (internal) or stp-external (cf. -stp-path)");
    ("-solver-check-against", Arg.Set_string(opt_solver_check_against),
     "solver Compare solver results with the given one");
    ("-stp-path", Arg.Set_string(opt_stp_path),
     "path Location of external STP binary");
    ("-save-solver-files", Arg.Set(opt_save_solver_files),
     " Retain STP input and output files");
    ("-solver-slow-time", Arg.Set_float(opt_solver_slow_time),
     "secs Save queries that take longer than SECS");
    ("-solver-timeout", Arg.String
       (fun s -> opt_solver_timeout := Some (int_of_string s)),
     "secs Run each query for at most SECS seconds");
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

let solvers_table = 
  (let h = Hashtbl.create 7 in
     Hashtbl.replace h "none" (fun _ -> None);
     Hashtbl.replace h "stpvc"
       (fun _ ->
	  Some (new Stpvc_engine.stpvc_engine :> Query_engine.query_engine));
     Hashtbl.replace h "stp-external"
       (fun s ->
	  Some (new Stp_external_engine.stp_external_engine ("fuzz" ^ s)));
     h)

let construct_solver () =
  let checking_solver_opt =
    try (Hashtbl.find solvers_table !opt_solver_check_against) "-check"
    with Not_found -> failwith "Unknown solver for -solver-check-against"
  in
  let main_solver_opt =
    try (Hashtbl.find solvers_table !opt_solver) ""
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
  fm#set_query_engine (construct_solver ())

  

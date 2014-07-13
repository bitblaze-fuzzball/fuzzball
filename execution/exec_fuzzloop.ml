(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

module V = Vine;;

open Exec_domain;;
open Exec_exceptions;;
open Exec_utils;;
open Exec_options;;
open Frag_simplify;;
open Fragment_machine;;
open Sym_path_frag_machine;;
open Sym_region_frag_machine;;
open Decision_tree;;
open Exec_run_common;;
open Exec_runloop;;
open Exec_stats;;

let loop_w_probing count (dt : decision_tree) (frag : fragment_machine) fn =
(*  let outer_iteration = ref 0L 
  and num_probes = 2L 
  and best_path = ref ""
  and probe_depth = 3L in
  opt_path_depth_limit := (-1L);
  
  while true
  do
    opt_path_depth_limit := Int64.add !opt_path_depth_limit probe_depth;
    Printf.printf "Probe depth: %s\n" (Int64.to_string !opt_path_depth_limit);
    Printf.printf "Using prefix: [%s]\n" !opt_follow_path;
    outer_iteration := Int64.add !outer_iteration 1L;
    
    (* reset our probe counter *)
    let inner_iteration = ref 0L in
    (try
       while (!inner_iteration < num_probes)
       do
	 inner_iteration := Int64.add !inner_iteration 1L;
	 Printf.printf "Iteration #%s Probe #%s\n" 
	   (Int64.to_string !outer_iteration)
	   (Int64.to_string !inner_iteration);


	 fn !inner_iteration;
	 let cur_path = dt#get_most_recent_traversal in
	 Printf.printf "recent path: %s\n%!" cur_path;
(*	 if Random.bool () then *)
	 best_path := cur_path
       done
     with
       LastIteration -> Printf.printf "LastIteration Thrown\n");

    (* update the path prefix with the best we've seen *)
    opt_follow_path := !best_path
	  
  done;*)
  Printf.printf "DID NOTHING\n";
  Printf.printf "Exited loop\n"
      
let loop_w_stats count fn =
  let iter = ref 0L and
      start_wtime = Unix.gettimeofday () and
      start_ctime = Sys.time () in
    (try
       while (match count with
		| None -> true
		| Some i -> !iter < i)
       do
	 iter := Int64.add !iter 1L;
	 let old_wtime = Unix.gettimeofday () and
             old_ctime = Sys.time () in
	   if !opt_trace_iterations then 
	     Printf.printf "Iteration %Ld:\n" !iter;
	   fn !iter;
	   let wtime = Unix.gettimeofday() in
	     if !opt_time_stats then
	       ((let ctime = Sys.time() in
		   Printf.printf "CPU time %f sec, %f total\n"
		     (ctime -. old_ctime) (ctime -. start_ctime));
		(Printf.printf "Wall time %f sec, %f total\n"
		   (wtime -. old_wtime) (wtime -. start_wtime)));
	     flush stdout;
	     match !opt_total_timeout with
	       | None -> ()
	       | Some t ->
		   if (wtime -. start_wtime) > t then
		     (Printf.printf "Total exploration time timeout.\n";
		      raise LastIteration)
       done
     with
	 LastIteration -> ());
    if !opt_gc_stats then
      Gc.full_major () (* for the benefit of leak checking *)

let log_fuzz_restart log str = 
  log (
    Yojson_logger.LazyJson (lazy 
			      (`Assoc 
				  ["function", `String "fuzz";
				   "type", `String "restart";
				   "restart_reason", `String str;
				  ]
			      )
    )
  )

let fuzz start_eip opt_fuzz_start_eip end_eips
    (fm : fragment_machine) asmir_gamma symbolic_init reset_cb
    (*(dt : decision_tree)*)  =
  if !opt_trace_setup then
    (Printf.printf "Initial registers:\n";
     fm#print_regs);
  (match !opt_periodic_stats with
     | Some p -> add_periodic_hook fm p
     | None -> ());
  flush stdout;
  if !opt_gc_stats then
    at_exit final_check_memory_usage;
  let fuzz_start_eip = ref opt_fuzz_start_eip
  and extra_setup = ref (fun () -> ()) in
  (try
     Sys.set_signal  Sys.sighup
       (Sys.Signal_handle(fun _ -> raise (Signal "HUP")));
     Sys.set_signal  Sys.sigint
       (Sys.Signal_handle(fun _ -> raise (Signal "INT")));
     Sys.set_signal Sys.sigterm
       (Sys.Signal_handle(fun _ -> raise (Signal "TERM")));
     Sys.set_signal Sys.sigquit
       (Sys.Signal_handle(fun _ -> raise (Signal "QUIT")));
     Sys.set_signal Sys.sigusr1
       (Sys.Signal_handle(fun _ -> raise (Signal "USR1")));
     Sys.set_signal Sys.sigusr2
       (Sys.Signal_handle(fun _ -> periodic_stats fm false true));
     (try 
	if start_eip <> opt_fuzz_start_eip then
	  (if !opt_trace_setup then Printf.printf "Pre-fuzzing execution...\n";
	   flush stdout;
	   runloop fm start_eip asmir_gamma
	     (fun a ->
		if a = opt_fuzz_start_eip then
		  (decr opt_fuzz_start_addr_count;
		   !opt_fuzz_start_addr_count = 0)
		else
		  false))
      with
	| StartSymbolic(eip, setup) ->
	    fuzz_start_eip := eip;
	    extra_setup := setup);
     fm#start_symbolic;
     if !opt_trace_setup then
       (Printf.printf "Setting up symbolic values:\n"; flush stdout);
     symbolic_init ();
     !extra_setup ();
     fm#make_snap ();
     if !opt_trace_setup then
       (Printf.printf "Took snapshot\n"; flush stdout);
     (try
 	loop_w_stats !opt_num_paths
(*	loop_w_probing !opt_num_paths dt fm *)
	  (fun iter ->
	     let old_tcs = Hashtbl.length trans_cache in
	     let stop str = if !opt_trace_stopping then
	       let stop_eip = fm#get_eip in
	         Printf.printf "Stopping %s at 0x%08Lx\n" str stop_eip
	     in
	       fm#set_iter_seed (Int64.to_int iter);
	     let module Log = 
		   (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in
	       (try
		  runloop fm !fuzz_start_eip asmir_gamma
		    (fun a -> List.mem a end_eips);
		with
		  | SimulatedExit(_) -> 
		    log_fuzz_restart Log.trace "when program called exit()";
		    stop "when program called exit()"
		  | SimulatedAbort -> 
		    log_fuzz_restart Log.trace "when program called abort()";
		    stop "when program called abort()"
		  | KnownPath ->
		    log_fuzz_restart Log.trace "on previously-explored path";
		    stop "on previously-explored path"
		      (* KnownPath currently shouldn't happen *)
		  | DeepPath ->
		    log_fuzz_restart Log.trace "on too-deep path";
		    stop "on too-deep path"
		  | SymbolicJump ->
		    log_fuzz_restart Log.trace "at symbolic jump";
		    stop "at symbolic jump"
		  | NullDereference ->
		    if !opt_finish_on_null_deref then (
		      log_fuzz_restart Log.trace "concrete null dereference";
		      finish_fuzz "concrete null dereference"
		    );
		    log_fuzz_restart Log.trace "at null deref";
		    stop "at null deref"
		  | JumpToNull -> 
		    log_fuzz_restart Log.trace "at jump to null";
		    stop "at jump to null"
		  | DivideByZero -> 
		    log_fuzz_restart Log.trace "at division by zero";
		    stop "at division by zero"
		  | TooManyIterations ->
		    log_fuzz_restart Log.trace "after too many iterations";
		    stop "after too many loop iterations"
		  | UnhandledTrap -> 
		    log_fuzz_restart Log.trace "at trap";
		    stop "at trap"
		  | IllegalInstruction -> 
		    log_fuzz_restart Log.trace "at bad instruction";
		    stop "at bad instruction"
		  | UnhandledSysCall(s) ->
		    Printf.printf "[trans_eval WARNING]: %s\n%!" s;
		    log_fuzz_restart Log.trace "at unhandled system call";
		    stop "at unhandled system call"
		  | SymbolicSyscall ->
		    log_fuzz_restart Log.trace "at symbolic system call";
		    stop "at symbolic system call"
		  | ReachedMeasurePoint ->
		    log_fuzz_restart Log.trace "at measurement point";
		    stop "at measurement point"
		  | ReachedInfluenceBound -> 
		    log_fuzz_restart Log.trace "at influence bound";
		    stop "at influence bound"
		  | DisqualifiedPath ->
		    log_fuzz_restart Log.trace "on disqualified path";
		    stop "on disqualified path"
		  | BranchLimit ->
		    log_fuzz_restart Log.trace "on branch limit";
		    stop "on branch limit"
		  | SolverFailure when !opt_nonfatal_solver -> 
		    log_fuzz_restart Log.trace "on solver failure";
		    stop "on solver failure"
		  | UnproductivePath ->
		    log_fuzz_restart Log.trace "on unproductive path";
		    stop "on unproductive path"
		  | Signal("USR1") -> 
		    log_fuzz_restart Log.trace "on SIGUSR1";
		    stop "on SIGUSR1"
		  (* | NotConcrete(_) -> () (* shouldn't happen *)
		     | Simplify_failure(_) -> () (* shouldn't happen *)*)
	       ); 
	       if !opt_coverage_stats && 
		 (Hashtbl.length trans_cache - old_tcs > 0) then
		   Printf.printf "Coverage increased to %d on %Ld\n"
		     (Hashtbl.length trans_cache) iter;
	       periodic_stats fm false false;
	       if not fm#finish_path then raise LastIteration;
	       if !opt_concrete_path then raise LastIteration;
	       (match !Fragment_machine.fuzz_finish_reasons with
	       | [] -> ()
	       | _ ->
		 if !opt_trace_stopping then (
		   Printf.printf "Finished, %s\n" (List.fold_left (fun a s ->
		     if a = "" then s
		     else a ^ ", " ^ s
		   ) "" !Fragment_machine.fuzz_finish_reasons);
		 ); raise LastIteration);
	       if !opt_concrete_path_simulate then
		 opt_concrete_path_simulate := false; (* First iter. only *)
	       reset_cb ();
	       fm#reset ()
	  );
      with
	| LastIteration -> ()
	| Signal("QUIT") -> Printf.printf "Caught SIGQUIT\n");
     fm#after_exploration
   with
     | LastIteration -> ()
     | Signal(("INT"|"HUP"|"TERM") as s) -> Printf.printf "Caught SIG%s\n" s
    (*
     | e -> Printf.printf "Caught fatal error %s\n" (Printexc.to_string e);
	 Printexc.print_backtrace stderr *) );
  if !opt_coverage_stats then
    Printf.printf "Final coverage: %d\n"
      (Hashtbl.length trans_cache);
  periodic_stats fm true false

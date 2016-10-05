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
open Exec_run_common;;
open Exec_runloop;;
open Exec_stats;;

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
             old_ctime = Sys.time () and
	     is_final = ref false in
	   if !opt_trace_iterations then 
	     Printf.printf "Iteration %Ld:\n%!" !iter;
	   (try
	      fn !iter;
	    with LastIteration -> is_final := true);
	   let wtime = Unix.gettimeofday() in
	     if !opt_time_stats then
	       ((let ctime = Sys.time() in
		   Printf.printf "CPU time %f sec, %f total\n"
		     (ctime -. old_ctime) (ctime -. start_ctime));
		(Printf.printf "Wall time %f sec, %f total\n"
		   (wtime -. old_wtime) (wtime -. start_wtime)));
	     flush stdout;
	     (match !opt_total_timeout with
		| None -> ()
		| Some t ->
		    if (wtime -. start_wtime) > t then
		      (Printf.printf "Total exploration time timeout.\n";
		       raise LastIteration));
	     if !is_final then raise LastIteration
       done
     with
	 LastIteration -> ());
    if !opt_gc_stats then
      Gc.full_major () (* for the benefit of leak checking *)

let fuzz start_eip opt_fuzz_start_eip end_eips
    (fm : fragment_machine) asmir_gamma symbolic_init reset_cb =
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
		   if !opt_fuzz_start_addr_count = 0 then 
		     opt_iteration_limit_enforced := Some !opt_iteration_limit;
		   !opt_fuzz_start_addr_count = 0
		  )
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
	  (fun iter ->
	     let old_tcs = Hashtbl.length trans_cache in
	     let stop str = if !opt_trace_stopping then
	       let stop_eip = fm#get_eip in
	         Printf.printf "Stopping %s at 0x%08Lx\n" str stop_eip
	     in
	       fm#set_iter_seed (Int64.to_int iter);
	       (try
		  runloop fm !fuzz_start_eip asmir_gamma
		    (fun a -> List.mem a end_eips);
		with
		  | SimulatedExit(_) -> stop "when program called exit()"
		  | SimulatedAbort -> stop "when program called abort()"
		  | KnownPath -> stop "on previously-explored path"
		      (* KnownPath currently shouldn't happen *)
		  | DeepPath -> stop "on too-deep path"
		  | SymbolicJump -> stop "at symbolic jump"
		  | NullDereference -> stop "at null deref"
		  | JumpToNull -> stop "at jump to null"
		  | DivideByZero -> stop "at division by zero"
		  | TooManyIterations -> stop "after too many loop iterations"
		  | UnhandledTrap -> stop "at trap"
		  | IllegalInstruction -> stop "at bad instruction"
		  | UnhandledSysCall(s) ->
		      Printf.printf "[trans_eval WARNING]: %s\n%!" s;
		      stop "at unhandled system call"
		  | SymbolicSyscall -> stop "at symbolic system call"
		  | ReachedMeasurePoint -> stop "at measurement point"
		  | ReachedInfluenceBound -> stop "at influence bound"
		  | DisqualifiedPath -> stop "on disqualified path"
		  | BranchLimit -> stop "on branch limit"
		  | SolverFailure when !opt_nonfatal_solver
		      -> stop "on solver failure"
		  | UnproductivePath -> stop "on unproductive path"
		  | Signal("USR1") -> stop "on SIGUSR1"
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
	       (match fm#finish_reasons with
		  | (s :: rest) as l
		      when (List.length l) >= !opt_finish_reasons_needed
			->
		      if !opt_trace_stopping then
			Printf.printf "Finished, %s\n"
			  (String.concat ", " l);
		      raise LastIteration
		  | _ -> ());
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

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

let prefuzz_region start_eip opt_fuzz_start_eip fuzz_start_eip fm asmir_gamma extra_setup =
  assert (start_eip <> opt_fuzz_start_eip);
  let prefuzz a =
    if a = opt_fuzz_start_eip
    then (decr opt_fuzz_start_addr_count;
	  !opt_fuzz_start_addr_count = 0)
    else false in
  try
    if !opt_trace_setup
    then Printf.printf "Pre-fuzzing execution...\n";
    flush stdout;
    runloop fm start_eip asmir_gamma prefuzz
  with
  | StartSymbolic(eip, setup) ->
    fuzz_start_eip := eip;
    extra_setup := setup

let fuzz_sighandle_setup fm =
  Sys.set_signal Sys.sighup
    (Sys.Signal_handle(fun _ -> raise (Signal "HUP")));
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle(fun _ -> raise (Signal "INT")));
  Sys.set_signal Sys.sigterm
    (Sys.Signal_handle(fun _ -> raise (Signal "TERM")));
  Sys.set_signal Sys.sigquit
    (Sys.Signal_handle(fun _ -> raise (Signal "QUIT")));
  Sys.set_signal Sys.sigusr1
    (Sys.Signal_handle(fun _ -> raise (Signal "USR1")));
  Sys.set_signal Sys.sigusr2
    (Sys.Signal_handle(fun _ -> periodic_stats fm false true))

let fuzz_runloop fm fuzz_start_eip asmir_gamma end_eips =
  let module Log =  (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in
  let stop str = if !opt_trace_stopping then
      Printf.printf "Stopping %s at 0x%08Lx\n" str fm#get_eip in
  try
    runloop fm fuzz_start_eip asmir_gamma (fun a -> List.mem a end_eips)
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
      fm#finish_fuzz "concrete null dereference"
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
  | FinishNow ->
    log_fuzz_restart Log.trace "on -finish-immediately";
    stop "on -finish_immediately";
  | Signal("USR1") -> 
		    log_fuzz_restart Log.trace "on SIGUSR1";
    stop "on SIGUSR1"
  | Double_Free ->
    log_fuzz_restart Log.trace "on double free";
    stop "on double free"
  | Dealloc_Not_Alloc ->
    log_fuzz_restart Log.trace "on deallocating something not allocated";
    stop "on deallocating something not allocated"
  | Alloc_Dealloc_Length_Mismatch ->
    log_fuzz_restart Log.trace "on deallocating a size different than that allocated";
    stop "on deallocating a size different than that allocated"
  | Unsafe_Memory_Access ->
    log_fuzz_restart Log.trace "on unsafe memory access";
    stop "on unsafe memory access"
  | Uninitialized_Memory ->
    log_fuzz_restart Log.trace "use of uninitialized memory";
    stop "use of uninitialized memory"    
  | NotConcrete(_) -> failwith "fuzz raised NotConcrete, but it should have been caught before now!"
  | Simplify_failure(_) -> failwith "fuzz raised Simplify_failure, but it should have been caught before now!"
    

(* opt_fuzz_start_eip comes... sometimes... fuzz_start_addr *)
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
  and extra_setup = ref (fun () -> ())
  and flush_print str = Printf.printf "%s" str; flush stdout
  in
  (try
     fuzz_sighandle_setup fm;
     if start_eip <> opt_fuzz_start_eip then
       prefuzz_region start_eip opt_fuzz_start_eip fuzz_start_eip fm asmir_gamma extra_setup;
     fm#start_symbolic;
     if !opt_trace_setup then flush_print "Setting up symbolic values:\n";
     symbolic_init ();
     !extra_setup ();
     fm#make_snap ();
     if !opt_trace_setup then flush_print "Took snapshot\n";
     (try
 	loop_w_stats !opt_num_paths
	  (fun iter ->
	    let old_tcs = Hashtbl.length trans_cache in
	    fm#set_iter_seed (Int64.to_int iter);
	    fuzz_runloop fm !fuzz_start_eip asmir_gamma end_eips;
	       if !opt_coverage_stats && 
		 (Hashtbl.length trans_cache - old_tcs > 0) then
		   Printf.printf "Coverage increased to %d on %Ld\n"
		     (Hashtbl.length trans_cache) iter;
	       periodic_stats fm false false;
	       if not fm#finish_path then raise LastIteration;
	       if !opt_concrete_path then raise LastIteration;
	       (match fm#finish_reasons with
		  | (_ :: _) as l
		      when (List.length l) >= !opt_finish_reasons_needed
			->
		      if !opt_trace_stopping then
			Printf.printf "Finished, %s\n"
			  (List.fold_left (fun a s ->
					     if a = "" then s
					     else a ^ ", " ^ s)
			     "" l);
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

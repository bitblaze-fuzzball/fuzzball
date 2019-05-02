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
open Exec_assert_minder;;
      
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
	     Printf.eprintf "Iteration %Ld:\n%!" !iter;
	   (try
	      fn !iter;
	    with LastIteration -> is_final := true);
	   let wtime = Unix.gettimeofday() in
	     if !opt_time_stats then
	       ((let ctime = Sys.time() in
		   Printf.eprintf "CPU time %f sec, %f total\n"
		     (ctime -. old_ctime) (ctime -. start_ctime));
		(Printf.eprintf "Wall time %f sec, %f total\n"
		   (wtime -. old_wtime) (wtime -. start_wtime)));
	     flush stdout;
	     (match !opt_total_timeout with
		| None -> ()
		| Some t ->
		    if (wtime -. start_wtime) > t then
		      (Printf.eprintf "Total exploration time timeout.\n";
		       raise LastIteration));
	     if !is_final then raise LastIteration
       done
     with
	 LastIteration -> ());
    if !opt_gc_stats then
      Gc.full_major () (* for the benefit of leak checking *)

let restarts = ref 0

let log_fuzz_restart log str ispov fm = 
  let eip = fm#get_eip in
  let extra_details = `Assoc
    (Hashtbl.fold
       (fun k v l -> (k, v) :: l)
       fm#get_event_details [])
  in
  let event_history = `Assoc fm#get_event_history
  in
  log (
    Yojson_list_logger.LazyJson
      (lazy 
	 (`Assoc 
	     ["function", `String "fuzz";
	      "type", `String "restart";
	      "restart_reason", `String str;
	      "restarted_at", `String (Printf.sprintf "0x%08LX" eip);
	      "restart_at_depth", `String (Printf.sprintf "%i" fm#get_depth);
	      "extra_details", extra_details;
	      "event_history", event_history;
	      "special_handlers_state", fm#special_handlers_state_json;
	     ]
	 )
      )
  )

let close_logs_and_send_sexp log fm ispov =
  let module Log = (val !Loggers.cgc_restart_json : Yojson_list_logger.JSONListLog) in
  let module SEXP = (val !Loggers.cgc_sexp_logger : Text_logger.TextLog) in
    Pov_xml.write_pov (get_program_name ()) fm;
    Log.close_list ();
    (* note that we're totally relying on the assertion that the input directory for the info logger
       is the same as the pov output.  And we're constructing the names here rather than fetching them
       from the configs.  I should fix this eventually. *)
    let pov_filename = Printf.sprintf "%s/pov-%i.xml" !Pov_xml.out_channel_name !restarts
    and info_filename = Printf.sprintf "%s/info-%i.json" !Pov_xml.out_channel_name !restarts in
    let sexp = Text_logger.LazyString
      (lazy
	 (Printf.sprintf "((:type :new-test-case) (:from :fuzzball) (:xml \"%s\") (:info \"%s\") (:provenance %s) (:expected-pov %s))"
	    pov_filename info_filename 
	    (if !opt_symbolic_receive then
		":fuzzball-symbolic" else
		":fuzzball-concolic")
	    (if ispov then
		"t"
	     else "nil"))) in
  if !opt_emit_pollers || ispov then SEXP.always ~sign:false sexp;
  restarts := !restarts + 1;
  Eip_sequence_logger.flush ()

let prefuzz_region start_eip opt_fuzz_start_eip fuzz_start_eip fm asmir_gamma extra_setup =
  g_assert (start_eip <> opt_fuzz_start_eip) 100 "Exec_fuzzloop.prefuzz_region";
  let prefuzz a =
    if a = opt_fuzz_start_eip
    then (decr opt_fuzz_start_addr_count;
	  if !opt_fuzz_start_addr_count = 0 then 
	    opt_iteration_limit_enforced := Some !opt_iteration_limit;
	  !opt_fuzz_start_addr_count = 0)
    else false in
  try
    if !opt_trace_setup
    then Printf.eprintf "Pre-fuzzing execution...\n";
    flush stdout;
    runloop fm start_eip asmir_gamma prefuzz
  with
  | StartSymbolic(eip, setup) ->
    fuzz_start_eip := eip;
    extra_setup := setup
  | SimulatedExit(code) ->
      Printf.eprintf "Program exited (code %Ld) before reaching fuzz-start-addr\n" code;
      Printf.eprintf "(Maybe recheck your fuzz start address?)\n";
      exit 2 (* This used to be an uncaught exception *)

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
  let module Log = (val !Loggers.cgc_restart_json : Yojson_list_logger.JSONListLog) in
  let stop str ispov =
    close_logs_and_send_sexp Log.always fm ispov;
    if !opt_trace_stopping
    then Printf.eprintf "Stopping %s at 0x%08Lx\n" str fm#get_eip in
  try
    runloop fm fuzz_start_eip asmir_gamma (fun a -> List.mem a end_eips)
  with
  | SimulatedExit(_) -> 
    log_fuzz_restart Log.always ":exit()" false fm;
    stop "when program called exit()" false
  | SimulatedAbort -> 
    log_fuzz_restart Log.always ":abort()" false fm;
    stop "when program called abort()" false
  | KnownPath ->
    log_fuzz_restart Log.always ":previously_explored_path" false fm;
    stop "on previously-explored path" false
		(* KnownPath currently shouldn't happen *)
  | DeepPath ->
    log_fuzz_restart Log.always ":too_deep_path" false fm;
    stop "on too-deep path" false
  | SymbolicJump ->
    log_fuzz_restart Log.always ":symbolic_jump" false fm;
    stop "at symbolic jump" false
  | NullDereference info ->
    Log.always (
      Yojson_list_logger.LazyJson
	(lazy
	   (`Assoc
	       ["function", `String "fuzz";
		"type", `String ":null-dereference-info";
		"dereferenced_at",
		`String (Printf.sprintf "0x%08LX" info.eip_of_deref);
		"set_to_null_at",
		`String (Printf.sprintf "0x%08LX" info.last_set_to_null);
		"addr_derefed",
		`String (Printf.sprintf "0x%08LX" info.addr_derefed);])));
    if !opt_finish_on_null_deref then (
      log_fuzz_restart Log.always ":concrete_null_dereference" true fm;
      try
	fm#finish_fuzz "concrete null dereference"
      with FinishNow -> stop "an null deref" true
    );
    log_fuzz_restart Log.always ":null_deref" true fm;
    stop "at null deref" true
  | JumpToNull -> 
    log_fuzz_restart Log.always ":jump_to_null" true fm;
    stop "at jump to null" true
  | DivideByZero -> 
    log_fuzz_restart Log.always ":division_by_zero" true fm;
    stop "at division by zero" true
  | TooManyIterations ->
    log_fuzz_restart Log.always ":too_many_iterations" true fm;
    stop "after too many loop iterations" true
  | UnhandledTrap -> 
    log_fuzz_restart Log.always ":trap" false fm;
    stop "at trap"  false
  | IllegalInstruction -> 
    log_fuzz_restart Log.always ":bad_instruction" false fm;
    stop "at bad instruction"  false
  | UnhandledSysCall(s) ->
    Printf.eprintf "[trans_eval WARNING]: %s\n%!" s;
    log_fuzz_restart Log.always ":unhandled_system_call" false fm;
    stop "at unhandled system call" false
  | SymbolicSyscall ->
    log_fuzz_restart Log.always ":symbolic_system_call" false fm;
    stop "at symbolic system call" false
  | ReachedMeasurePoint ->
    log_fuzz_restart Log.always ":measurement_point" false fm;
    stop "at measurement point" false
  | ReachedInfluenceBound -> 
    log_fuzz_restart Log.always ":influence_bound" false fm;
    stop "at influence bound" false
  | DisqualifiedPath ->
    log_fuzz_restart Log.always ":disqualified_path" false fm;
    stop "on disqualified path" false
  | BranchLimit ->
    log_fuzz_restart Log.always ":branch_limit" false fm;
    stop "on branch limit" false
  | SolverFailure when !opt_nonfatal_solver -> 
    log_fuzz_restart Log.always ":solver_failure" false fm;
    stop "on solver failure" false
  | UnproductivePath ->
    log_fuzz_restart Log.always ":unproductive_path" false fm;
    stop "on unproductive path" false
  | SentErrorMessage(s) ->
      log_fuzz_restart Log.always ":error-message" false fm;
      stop ("after printing error message " ^ s) false
  | FinishNow -> (* split into multiple cases *)
    Printf.eprintf "Catching finish now\n";
    flush stderr;
    log_fuzz_restart Log.always ":-finish-immediately" false fm;
    stop "on -finish_immediately" false
  | Signal("USR1") -> 
    log_fuzz_restart Log.always ":SIGUSR1" false fm;
    stop "on SIGUSR1" false
  | Double_Free ->
    log_fuzz_restart Log.always ":double_free" true fm;
    stop "on double free" true
  | Dealloc_Not_Alloc ->
    log_fuzz_restart Log.always ":deallocating_unallocated" true fm;
    stop "on deallocating something not allocated" true
  | Alloc_Dealloc_Length_Mismatch ->
    (* Note at the moment we don't raise this anywhere, since there's
       nothing matching this description which an error and which we can
       detect. *) 
    log_fuzz_restart Log.always ":partial_dealloc" true fm;
    stop "on deallocating a size different than that allocated" true
  | Unsafe_Memory_Access ->
    log_fuzz_restart Log.always ":unsafe_memory_access" true fm;
    stop "on unsafe memory access" true
  | Uninitialized_Memory ->
    log_fuzz_restart Log.always ":uninitialized_memory_access" true fm;
    stop "use of uninitialized memory" true   
  | WeirdSymbolicAddress ->
    log_fuzz_restart Log.always ":weird-symbolic-address" false fm;
    stop "use of weird symbolic address" true
  | NotConcrete(_) ->
    log_fuzz_restart Log.always ":not_concrete" false fm;
    stop "Something's symbolic that oughtn't be." false;
    failwith "fuzz raised NotConcrete, but it should have been caught before now!"
  | Simplify_failure(s) ->
    let fail_string = Printf.sprintf "Something can't be simplified, but we should have caught it earlier: %s" s in
    log_fuzz_restart Log.always ":simplify_failure" false fm;
    stop fail_string false;
    failwith fail_string
    

(* opt_fuzz_start_eip comes... sometimes... fuzz_start_addr *)
let fuzz start_eip opt_fuzz_start_eip end_eips
    (fm : fragment_machine) asmir_gamma symbolic_init reset_cb =
  if !opt_trace_setup then
    (Printf.eprintf "start:0x%08Lx fuzz-start:0x%08Lx\n" start_eip opt_fuzz_start_eip;
     Printf.eprintf "Initial registers:\n";
     fm#print_regs);
  add_remove_hook fm;
  (match !opt_periodic_stats with
     | Some p -> add_periodic_hook fm p
     | None -> ());
  flush stdout;
  if !opt_gc_stats then
    at_exit final_check_memory_usage;
  let fuzz_start_eip = ref opt_fuzz_start_eip
  and extra_setup = ref (fun () -> ())
  and flush_print str = Printf.eprintf "%s" str; flush stdout
  in
  (try
     fuzz_sighandle_setup fm;
     if start_eip <> opt_fuzz_start_eip then
       prefuzz_region start_eip opt_fuzz_start_eip fuzz_start_eip fm asmir_gamma extra_setup;
     let path_cond = fm#get_path_cond in
     if path_cond <> [] then 
       failwith ("The path condition is non-empty before fm#start_symbolic,"^
	 "you may want to re-run fuzzball with the -zero-memory option");
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
	    fuzz_runloop fm (if fm#get_start_eip = 0L then !fuzz_start_eip else fm#get_start_eip) asmir_gamma end_eips;
	       if !opt_coverage_stats && 
		 (Hashtbl.length trans_cache - old_tcs > 0) then
		   Printf.eprintf "Coverage increased to %d on %Ld\n"
		     (Hashtbl.length trans_cache) iter;
	       periodic_stats fm false false;
	       if not fm#finish_path then raise LastIteration;
	       if !opt_concrete_path then raise LastIteration;

	       (match fm#finish_reasons with
		  | (_ :: _) as l
		      when (List.length l) >= !opt_finish_reasons_needed
			->
		      if !opt_trace_stopping then
			Printf.eprintf "Finished, %s\n"
			  (String.concat ", " l);
		      raise LastIteration
		  | _ -> ());
	       if !opt_concrete_path_simulate then
		 opt_concrete_path_simulate := false; (* First iter. only *)
	       reset_cb ();
	       fm#reset ();
	  );
      with
      | LastIteration -> Printf.eprintf "Exiting with LastIteration\n"
      | Signal("QUIT") -> Printf.eprintf "Caught SIGQUIT\n");
     fm#after_exploration
   with
     | LastIteration -> Printf.eprintf "Exiting with LastIteration\n"
     | Signal(("INT"|"HUP"|"TERM") as s) -> Printf.eprintf "Caught SIG%s\n" s);
    
  if !opt_coverage_stats then
    Printf.eprintf "Final coverage: %d\n"
      (Hashtbl.length trans_cache);
  periodic_stats fm true false

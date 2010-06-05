(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Exec_domain;;
open Exec_exceptions;;
open Concrete_domain;;
open Exec_options;;
open Frag_simplify;;
open Fragment_machine;;
open Sym_path_frag_machine;;
open Sym_region_frag_machine;;
open Exec_runloop;;

let check_memory_size () =
  let chan = open_in "/proc/self/status" in
    for i = 1 to 11 do ignore(input_line chan) done;
    let line = input_line chan in 
      close_in chan;
      assert((String.sub line 0 7) = "VmSize:");
      String.sub line 7 ((String.length line) - 7)

let check_memory_usage fm =
  Printf.printf "Counted size is %d\n"
    (fm#measure_size +
       (Hashtbl.fold
	  (fun k (dl, sl) s -> s + (stmt_size (V.Block(dl,sl))))
	  trans_cache 0));
  Printf.printf "/proc size is %s\n" (check_memory_size ());
  flush stdout;
  Gc.print_stat stdout

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
	   if !opt_time_stats then
	     ((let ctime = Sys.time() in
		 Printf.printf "CPU time %f sec, %f total\n"
		   (ctime -. old_ctime) (ctime -. start_ctime));
	      (let wtime = Unix.gettimeofday() in
		 Printf.printf "Wall time %f sec, %f total\n"
		   (wtime -. old_wtime) (wtime -. start_wtime)));
	   flush stdout
       done
     with
	 LastIteration -> ());
    if !opt_gc_stats then
      Gc.full_major () (* for the benefit of leak checking *)

let print_tree fm =
  let chan = open_out "fuzz.tree" in
    fm#print_tree chan;
    close_out chan

let periodic_stats fm at_end force = 
  if true || force then
    print_tree fm;
  if !opt_gc_stats || force then
    check_memory_usage fm;
  if !opt_gc_stats || force then
    Gc.print_stat stdout;
  if (!opt_solver_stats && at_end) || force then
    (Printf.printf "Solver returned satisfiable %Ld time(s)\n" !solver_sats;
     Printf.printf "Solver returned unsatisfiable %Ld time(s)\n"
       !solver_unsats;
     Printf.printf "Solver failed %Ld time(s)\n" !solver_fails)

let fuzz start_eip fuzz_start_eip end_eips
    (fm : 
     <
     compute_all_multipath_influence : unit;
     compute_multipath_influence : string -> unit;
     finish_path : bool;
     load_byte_conc : int64 -> int;
     make_snap : unit -> unit;
     measure_size : int;
     print_tree : out_channel -> unit;
     print_x86_regs : unit;
     reset : unit -> unit;
     run : unit -> string;
     run_eip_hooks : unit;
     set_eip : int64 -> unit;
     set_frag : Vine.decl list * Vine.stmt list -> unit;
     set_iter_seed : int -> unit;
     set_word_reg_symbolic : Fragment_machine.register_name -> string -> unit;
     set_word_var : Fragment_machine.register_name -> int64 -> unit;
     start_symbolic : unit;
     watchpoint : unit;
     .. >)
    asmir_gamma symbolic_init =
  if !opt_trace_setup then
    (Printf.printf "Initial registers:\n";
     fm#print_x86_regs);
  flush stdout;
  (if start_eip <> fuzz_start_eip then
     (if !opt_trace_setup then Printf.printf "Pre-fuzzing execution...\n";
      flush stdout;
      runloop fm start_eip asmir_gamma (fun a -> a = fuzz_start_eip)));
  fm#start_symbolic;
  if !opt_trace_setup then
    (Printf.printf "Setting up symbolic values:\n"; flush stdout);
  symbolic_init ();
  fm#make_snap ();
  if !opt_trace_setup then
    (Printf.printf "Took snapshot\n"; flush stdout);
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
     (try
	loop_w_stats !opt_num_paths
	  (fun iter ->
	     let old_tcs = Hashtbl.length trans_cache in
	     let stop str = if !opt_trace_stopping then
	       Printf.printf "Stopping %s\n" str
	     in
	       fm#set_iter_seed (Int64.to_int iter);
	       (try
		  runloop fm fuzz_start_eip asmir_gamma
		    (fun a -> List.mem a end_eips);
		with
		  | SimulatedExit(_) -> stop "when program called exit()"
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
		  | Signal("USR1") -> stop "on SIGUSR1"
		  (* | NotConcrete(_) -> () (* shouldn't happen *)
		     | Simplify_failure(_) -> () (* shouldn't happen *)*)
	       ); 
	       if not fm#finish_path then raise LastIteration;
	       if !opt_coverage_stats && 
		 (Hashtbl.length trans_cache - old_tcs > 0) then
		   Printf.printf "Coverage increased to %d on %Ld\n"
		     (Hashtbl.length trans_cache) iter;
	       periodic_stats fm false false;
	       fm#reset ()
	  );
      with
	| LastIteration -> ()
	| Signal("QUIT") -> Printf.printf "Caught SIGQUIT\n");
     (match (!opt_measure_deref_influence_at,
	     !opt_measure_expr_influence_at) with
	| (Some eip, _) -> fm#compute_multipath_influence 
	    (Printf.sprintf "eip 0x%08Lx" eip)
	| (_, Some (eip, expr)) -> fm#compute_multipath_influence 
	    (Printf.sprintf "eip 0x%08Lx" eip)
	| _ -> fm#compute_all_multipath_influence)
   with
     | Signal(("INT"|"HUP"|"TERM") as s) -> Printf.printf "Caught SIG%s\n" s
     | e -> Printf.printf "Caught fatal error %s\n" (Printexc.to_string e);
	 Printexc.print_backtrace stderr);
  periodic_stats fm true false

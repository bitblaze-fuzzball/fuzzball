(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;

let opt_fuzz_start_addr = ref None
let opt_initial_eax = ref None
let opt_initial_ebx = ref None
let opt_initial_ecx = ref None
let opt_initial_edx = ref None
let opt_initial_esi = ref None
let opt_initial_edi = ref None
let opt_initial_esp = ref None
let opt_initial_ebp = ref None
let opt_initial_eflagsrest = ref None
let opt_store_bytes = ref []
let opt_store_shorts = ref []
let opt_store_words = ref []
let opt_store_longs = ref []
let opt_symbolic_regs = ref false
let opt_symbolic_strings = ref []
let opt_symbolic_cstrings = ref []
let opt_symbolic_cstrings_fulllen = ref []
let opt_symbolic_string16s = ref []
let opt_symbolic_bytes = ref []
let opt_symbolic_shorts = ref []
let opt_symbolic_words = ref []
let opt_symbolic_longs = ref []
let opt_symbolic_bytes_influence = ref []
let opt_symbolic_shorts_influence = ref []
let opt_symbolic_words_influence = ref []
let opt_symbolic_longs_influence = ref []
let opt_symbolic_regions = ref []
let opt_concolic_cstrings = ref []
let opt_concolic_strings = ref []
let opt_sink_regions = ref []
let opt_measure_expr_influence_at_strings = ref None
let opt_check_condition_at_strings = ref []
let opt_extra_condition_strings = ref []
let opt_tracepoint_strings = ref []
let opt_string_tracepoint_strings = ref []

let set_defaults_for_concrete () =
  opt_zero_memory := true

let influence_cmdline_opts =
  [  
    ("-disqualify-addr", Arg.String
       (fun s -> opt_disqualify_addrs :=
	  (Int64.of_string s) :: !opt_disqualify_addrs),
     "addr As -fuzz-end-addr, but also remove from influence");
    ("-symbolic-byte-influence", Arg.String
       (add_delimited_num_str_pair opt_symbolic_bytes_influence '='),
     "addr=var Like -symbolic-byte, but also use for -periodic-influence");
    ("-symbolic-short-influence", Arg.String
       (add_delimited_num_str_pair opt_symbolic_shorts_influence '='),
     "addr=var Like -symbolic-short, but also use for -periodic-influence");
    ("-symbolic-word-influence", Arg.String
       (add_delimited_num_str_pair opt_symbolic_words_influence '='),
     "addr=var Like -symbolic-word, but also use for -periodic-influence");
    ("-symbolic-long-influence", Arg.String
       (add_delimited_num_str_pair opt_symbolic_longs_influence '='),
     "addr=var Like -symbolic-long, but also use for -periodic-influence");
    ("-measure-influence-derefs", Arg.Set(opt_measure_influence_derefs),
     " Measure influence on uses of sym. pointer values");
    ("-measure-influence-reploops", Arg.Set(opt_measure_influence_reploops),
     " Measure influence on %ecx at rep-prefixed instructions");
    ("-measure-influence-syscall-args", Arg.Set(opt_measure_influence_syscall_args),
     " Measure influence on uses of sym. system call args.");
    ("-measure-deref-influence-at", Arg.String
       (fun s -> opt_measure_deref_influence_at :=
	  Some (Int64.of_string s)),
     "eip Measure influence of pointer at given code address");
    ("-multipath-influence-only", Arg.Set(opt_multipath_influence_only),
     " Skip single-path influence measurements");
    ("-stop-at-measurement", Arg.Set(opt_stop_at_measurement),
     " Stop paths after an '-at' influence measurement");
    ("-measure-expr-influence-at", Arg.String
       (fun s -> let (eip_s, expr_s) = split_string ':' s in
	  opt_measure_expr_influence_at_strings :=
	    Some (eip_s, expr_s)),
     "eip:expr Measure influence of value at given code address");
    ("-periodic-influence", Arg.String
       (fun s ->
	  let k = int_of_string s in
	    opt_periodic_influence := Some k;
	    next_periodic_influence := k),
     "k Check influence every K bits of branching");
    ("-influence-bound", Arg.Set_float(opt_influence_bound),
     "float Stop path when influence is <= this value");
  ]

let concrete_state_cmdline_opts =
  [
    ("-start-addr", Arg.String
       (fun s -> opt_start_addr := Some(Int64.of_string s)),
     "addr Code address to start executing");
    ("-initial-eax", Arg.String
       (fun s -> opt_initial_eax := Some(Int64.of_string s)),
     "word Concrete initial value for %eax register");
    ("-initial-rax", Arg.String
       (fun s -> opt_initial_eax := Some(Int64.of_string s)),
     "word Concrete initial value for %rax register");
    ("-initial-ebx", Arg.String
       (fun s -> opt_initial_ebx := Some(Int64.of_string s)),
     "word Concrete initial value for %ebx register");
    ("-initial-rbx", Arg.String
       (fun s -> opt_initial_ebx := Some(Int64.of_string s)),
     "word Concrete initial value for %rbx register");
    ("-initial-ecx", Arg.String
       (fun s -> opt_initial_ecx := Some(Int64.of_string s)),
     "word Concrete initial value for %ecx register");
    ("-initial-rcx", Arg.String
       (fun s -> opt_initial_ecx := Some(Int64.of_string s)),
     "word Concrete initial value for %rcx register");
    ("-initial-edx", Arg.String
       (fun s -> opt_initial_edx := Some(Int64.of_string s)),
     "word Concrete initial value for %edx register");
    ("-initial-rdx", Arg.String
       (fun s -> opt_initial_edx := Some(Int64.of_string s)),
     "word Concrete initial value for %rdx register");
    ("-initial-esi", Arg.String
       (fun s -> opt_initial_esi := Some(Int64.of_string s)),
     "word Concrete initial value for %esi register");
    ("-initial-rsi", Arg.String
       (fun s -> opt_initial_esi := Some(Int64.of_string s)),
     "word Concrete initial value for %rsi register");
    ("-initial-edi", Arg.String
       (fun s -> opt_initial_edi := Some(Int64.of_string s)),
     "word Concrete initial value for %edi register");
    ("-initial-rdi", Arg.String
       (fun s -> opt_initial_edi := Some(Int64.of_string s)),
     "word Concrete initial value for %rdi register");
    ("-initial-esp", Arg.String
       (fun s -> opt_initial_esp := Some(Int64.of_string s)),
     "word Concrete initial value for %esp (stack pointer)");
    ("-initial-rsp", Arg.String
       (fun s -> opt_initial_esp := Some(Int64.of_string s)),
     "word Concrete initial value for %rsp (stack pointer)");
    ("-initial-ebp", Arg.String
       (fun s -> opt_initial_ebp := Some(Int64.of_string s)),
     "word Concrete initial value for %ebp (frame pointer)");
    ("-initial-rbp", Arg.String
       (fun s -> opt_initial_ebp := Some(Int64.of_string s)),
     "word Concrete initial value for %rbp");
    ("-initial-eflagsrest", Arg.String
       (fun s -> opt_initial_eflagsrest := Some(Int64.of_string s)),
     "word Concrete value for %eflags, less [CPAZSO]F");
    ("-store-byte", Arg.String
       (add_delimited_pair opt_store_bytes '='),
     "addr=val Set the byte at address to a concrete value");
    ("-store-short", Arg.String
       (add_delimited_pair opt_store_shorts '='),
     "addr=val Set 16-bit location to a concrete value");
    ("-store-word", Arg.String
       (add_delimited_pair opt_store_words '='),
     "addr=val Set a 32-bit memory word to a concrete value");
    ("-store-long", Arg.String
       (add_delimited_pair opt_store_longs '='),
     "addr=val Set 64-bit location to a concrete value");
    ("-skip-call-ret", Arg.String
       (add_delimited_pair opt_skip_call_addr '='),
     "addr=retval Replace the call at address 'addr' with a nop, and return 'retval' in EAX");
    ("-skip-func-ret", Arg.String
       (add_delimited_pair opt_skip_func_addr '='),
     "addr=retval Replace the function at address 'addr' with a nop, and return 'retval' in EAX");
  ]

let symbolic_state_cmdline_opts =
  [
    ("-symbolic-region", Arg.String
       (add_delimited_pair opt_symbolic_regions '+'),
     "base+size Memory region of unknown structure");
    ("-symbolic-string", Arg.String
       (add_delimited_pair opt_symbolic_strings '+'),
     "base+size Make a byte string with given size, no terminator");
    ("-symbolic-cstring", Arg.String
       (add_delimited_pair opt_symbolic_cstrings '+'),
     "base+size Make a C string with given size, concrete \\0");
    ("-symbolic-cstring-fulllen", Arg.String
       (add_delimited_pair opt_symbolic_cstrings_fulllen '+'),
     "base+size As above, but all chars are non-null");
    ("-symbolic-string16", Arg.String
       (add_delimited_pair opt_symbolic_string16s '+'),
     "base+16s As above, but with 16-bit characters");
    ("-symbolic-regs", Arg.Set(opt_symbolic_regs),
     " Give symbolic values to registers");
    ("-symbolic-byte", Arg.String
       (add_delimited_num_str_pair opt_symbolic_bytes '='),
     "addr=var Make a memory byte symbolic");
    ("-symbolic-short", Arg.String
       (add_delimited_num_str_pair opt_symbolic_shorts '='),
     "addr=var Make a 16-bit memory valule symbolic");
    ("-symbolic-word", Arg.String
       (add_delimited_num_str_pair opt_symbolic_words '='),
     "addr=var Make a memory word symbolic");
    ("-symbolic-long", Arg.String
       (add_delimited_num_str_pair opt_symbolic_longs '='),
     "addr=var Make a 64-bit memory valule symbolic");
    ("-sink-region", Arg.String
       (add_delimited_str_num_pair opt_sink_regions '+'),
     "var+size Range-check but ignore writes to a region");
    ("-skip-call-ret-symbol", Arg.String
       (add_delimited_num_str_pair opt_skip_call_addr_symbol '='),
     "addr=symname Like -s-c-r, but return a fresh symbol");
    ("-skip-func-ret-symbol", Arg.String
       (add_delimited_num_str_pair opt_skip_func_addr_symbol '='),
     "addr=symname Like -s-f-r, but return a fresh symbol");
    ("-skip-call-ret-region", Arg.String
       (add_delimited_num_str_pair opt_skip_call_addr_region '='),
     "addr=symname Like -s-c-r-s, but hint the symbol is a mem region");
    ("-skip-func-ret-region", Arg.String
       (add_delimited_num_str_pair opt_skip_func_addr_region '='),
     "addr=symname Like -s-f-r-s, but hint the symbol is a mem region");
    ("-skip-call-ret-symbol-once", Arg.String
       (add_delimited_num_str_pair opt_skip_call_addr_symbol_once '='),
     "addr=symname Like -s-c-r-s, but always the same variable");
  ]

let slurp_file fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  let str = String.create len in
    really_input ic str 0 len;
    close_in ic;
    str

let concolic_state_cmdline_opts =
  [
    ("-concrete-path", Arg.Set(opt_concrete_path),
     " Execute only according to concrete values");
    ("-solve-path-conditions", Arg.Set(opt_solve_path_conditions),
     " Solve conditions along a concrete path");
    ("-concolic-string", Arg.String
       (add_delimited_num_escstr_pair opt_concolic_strings '='),
     "base=\"str\" Make a byte string with given size, no terminator");
    ("-concolic-cstring", Arg.String
       (add_delimited_num_escstr_pair opt_concolic_cstrings '='),
     "base=\"str\" Make a C string with given size, concrete \\0");
    ("-concolic-cstring-file", Arg.String
       (fun s ->
	  let (s1, s2) = split_string '=' s in
	  let str = slurp_file s2 in
	    opt_concolic_cstrings :=
	      ((Int64.of_string s1), str) :: !opt_concolic_cstrings),
     "base=file As above, but read contents from a file");
    ("-concolic-prob", Arg.String
       (fun s -> (opt_concolic_prob := Some (float_of_string s))),
     "frac Take concolic branch with probability 0 <= FRAC <= 1");
  ]

let read_lines_file fname =
  let ic = open_in fname and
      l = ref [] in
    try
      while true do
	l := (input_line ic) :: !l
      done;
      failwith "Unreachable (infinite loop)"
    with
      | End_of_file ->
	  List.rev !l

let explore_cmdline_opts =
  [
    ("-fuzz-start-addr", Arg.String
       (fun s -> opt_fuzz_start_addr := Some(Int64.of_string s)),
     "addr Code address to start fuzzing");
    ("-fuzz-start-addr-count", Arg.Set_int(opt_fuzz_start_addr_count),
     "count Start at nth occurrence (vs. 1st)");
    ("-fuzz-end-addr", Arg.String
       (fun s -> opt_fuzz_end_addrs :=
	  (Int64.of_string s) :: !opt_fuzz_end_addrs),
     "addr Code address to finish fuzzing, may be repeated");
    ("-trace-end-jump", Arg.String
       (fun s -> opt_trace_end_jump := Some (Int64.of_string s)),
     " Print the target of the jump at the address specified by -fuzz-end-addr");
    ("-iteration-limit", Arg.String
       (fun s -> opt_iteration_limit := Int64.of_string s),
     "N Stop path if a loop iterates more than N times");
    ("-insn-limit", Arg.String
       (fun s -> opt_insn_limit := Int64.of_string s),
     "N Stop path after N instructions");
    ("-path-depth-limit", Arg.String
       (fun s -> opt_path_depth_limit := Int64.of_string s),
     "N Stop path after N bits of symbolic branching");
    ("-query-branch-limit", Arg.Set_int opt_query_branch_limit,
     "N Try at most N possibilities per branch");
    ("-num-paths", Arg.String
       (fun s -> opt_num_paths := Some (Int64.of_string s)),
     "N Stop after N different paths");
    ("-concretize-divisors", Arg.Set(opt_concretize_divisors),
     " Choose concrete values for divisors in /, %");
    ("-trace-binary-paths-delimited",
     Arg.Set(opt_trace_binary_paths_delimited),
     " As above, but with '-'s separating queries");
    ("-trace-binary-paths-bracketed",
     Arg.Set(opt_trace_binary_paths_bracketed),
     " As above, but with []s around multibit queries");
    ("-trace-decision-tree", Arg.Set(opt_trace_decision_tree),
     " Print internal decision tree operations");
    ("-trace-randomness", Arg.Set(opt_trace_randomness),
     " Print operation of PRNG 'random' choices");
    ("-trace-sym-addr-details", Arg.Set(opt_trace_sym_addr_details),
     " Print even more about symbolic address values");
    ("-coverage-stats", Arg.Set(opt_coverage_stats),
     " Print pseudo-BB coverage statistics");
    ("-offset-strategy", Arg.String
       (fun s -> opt_offset_strategy := offset_strategy_of_string s),
     "strategy Strategy for offset concretization: uniform, biased-small");
    ("-follow-path", Arg.Set_string(opt_follow_path),
     "string String of 0's and 1's signifying the specific path decisions to make.");
    ("-branch-preference", Arg.String
       (fun s -> let (s1, s2) = split_string ':' s in
	  Hashtbl.add opt_branch_preference (Int64.of_string s1)
	    (Int64.of_string s2)),
     "eip:(0|1) Prefer given direction for a symbolic branch");
    ("-branch-preference-unchecked", Arg.String
       (fun s -> let (s1, s2) = split_string ':' s in
	  Hashtbl.add opt_branch_preference_unchecked (Int64.of_string s1)
	    (Int64.of_string s2)),
     "eip:(0|1) Prefer given direction without solving");
    ("-always-prefer", Arg.Bool
       (fun b -> opt_always_prefer := Some b),
     "bool Prefer given branch direction instead of random");
    ("-random-seed", Arg.Set_int opt_random_seed,
     "N Use given seed for path choice");
    ("-save-decision-tree-interval",
     Arg.String (fun s -> opt_save_decision_tree_interval
		   := Some (float_of_string s)),
     "SECS Output decision tree every SECS seconds");
    ("-decision-tree-use-file", Arg.Set(opt_decision_tree_use_file),
     " Store the decision tree in a file (default: in memory)");
    ("-total-timeout", Arg.String
       (fun s -> opt_total_timeout := Some (float_of_string s)),
     "SECS Finish exploration after a given time has elapsed");
    ("-target-string", Arg.String
       (fun s -> let (s1, s2) = split_string '=' s in
	  opt_target_region_start := Some (Int64.of_string s1);
	  opt_target_region_string := Exec_utils.unescaped s2),
     "base=string Try to make a buffer have the given contents");
    ("-target-string-file", Arg.String
       (fun s -> let (s1, s2) = split_string '=' s in
	  opt_target_region_start := Some (Int64.of_string s1);
	  opt_target_region_string := slurp_file s2),
     "base=filename same, but read string direct from a file");
    ("-target-formulas", Arg.String
       (fun s -> let (s1, s2) = split_string '=' s in
	  opt_target_region_start := Some (Int64.of_string s1);
	  opt_target_region_formula_strings := read_lines_file s2),
     "base=exprs-file Try to make a buffer have the given contents");
    ("-trace-target", Arg.Set(opt_trace_target),
     " Print targeting checks");
    ("-target-no-prune", Arg.Set(opt_target_no_prune),
     " Do not stop path at target mismatch");
    ("-finish-on-target-match", Arg.Set(opt_finish_on_target_match),
     " Finish exploration on -target-string match");
    ("-target-guidance", Arg.Set_float(opt_target_guidance),
     "PROB Prefer better target matches with given probability");
    ("-trace-guidance", Arg.Set(opt_trace_guidance),
     " Print operation of -target-guidance");
    ("-trace-tables", Arg.Set(opt_trace_tables),
     " Print table lookups");
    ("-table-limit", Arg.Set_int(opt_table_limit),
     "BITS Match tables with at most 2**bits entries");
    ("-offset-limit", Arg.Set_int(opt_offset_limit),
     "BITS Concretize offsets with at most 2**bits entries");
    ("-trace-offset-limit", Arg.Set(opt_trace_offset_limit),
     " Print offset width information");
    ("-no-table-store", Arg.Set(opt_no_table_store),
     " Disable symbolic treatment of table stores");
    ("-implied-value-conc", Arg.Set(opt_implied_value_conc),
     " Concretize based on path condition");
    ("-trace-ivc", Arg.Set(opt_trace_ivc),
     " Print operations of -implied-value-conc");
    ("-ite-ivc", Arg.Set(opt_ite_ivc),
     " Do implied-value-conc on if-then-else conditions");
    ("-trace-working-ce-cache", Arg.Set(opt_trace_working_ce_cache),
     " Print working cache after each query");
    ("-trace-global-ce-cache", Arg.Set(opt_trace_global_ce_cache),
     " Print global and working caches after each query");
    ("-global-ce-cache-limit", Arg.Set_int(opt_global_ce_cache_limit),
     " Set an integer limit on the global cache size");
    ("-disable-ce-cache", Arg.Set(opt_disable_ce_cache),
     " Do not use cached satisfying assingments at all");
    ("-narrow-bitwidth-cutoff", Arg.String
       (fun s -> opt_narrow_bitwidth_cutoff := Some (int_of_string s)),
     "BITS Treat values narrower than width as non-pointers");
    ("-t-expr-size", Arg.String
       (fun s -> opt_t_expr_size := int_of_string s),
     "SIZE Introduce temporaries for exprs of size or larger");
  ]


let tags_cmdline_opts =
  [
    ("-use-tags", Arg.Set(opt_use_tags),
     " Track data flow with numeric tags");
  ]

(* Conceptually, these could be applied to drivers other than
   FuzzBALL, but don't yet because they would need more implementation,
   are immature, etc. *)
let fuzzball_cmdline_opts =
  [
    ("-check-for-null", Arg.Set(opt_check_for_null),
     " Check whether dereferenced values can be null");
    (* This flag is misspelled, and will be renamed in the future. *)
    ("-no-fail-on-huer", Arg.Clear(opt_fail_offset_heuristic),
     " Do not fail when a heuristic (e.g. offset optimization) fails.");
  ]

let cmdline_opts =
  [
    ("-arch", Arg.String
       (fun s -> opt_arch := execution_arch_of_string s ),
     "arch x86 (default), x64, arm");
    ("-translation-cache-size", Arg.String
       (fun s -> opt_translation_cache_size := Some (int_of_string s)),
     "N Save translations of at most N instructions");
    ("-random-memory", Arg.Set(opt_random_memory),
     " Use random values for uninit. memory reads");
    ("-symbolic-memory", Arg.Set(opt_symbolic_memory),
     " Use symbolic values for uninit. memory reads");
    ("-zero-memory", Arg.Set(opt_zero_memory),
     " Use zero values for uninit. memory reads");
    ("-trace-basic",
     (Arg.Unit
	(fun () ->
	   opt_trace_binary_paths := true;
	   opt_trace_conditions := true;
	   opt_trace_decisions := true;
	   opt_trace_iterations := true;
	   opt_trace_setup := true;
	   opt_trace_stopping := true;
	   opt_trace_sym_addrs := true;
	   opt_trace_unexpected := true;
	   opt_coverage_stats := true;
	   opt_time_stats := true)),
     " Enable several common trace and stats options");
    ("-trace-binary-paths", Arg.Set(opt_trace_binary_paths),
     " Print decision paths as bit strings");
    ("-trace-conditions", Arg.Set(opt_trace_conditions),
     " Print branch conditions");
    ("-trace-decisions", Arg.Set(opt_trace_decisions),
     " Print symbolic branch choices");
    ("-trace-detailed",
     (Arg.Unit
	(fun () ->
	   opt_trace_insns := true;
	   opt_trace_loads := true;
	   opt_trace_stores := true;
	   opt_trace_temps := true;
	   opt_trace_syscalls := true;
	   opt_trace_registers := true;
	   opt_trace_segments := true;
	   opt_trace_taint := true)),
     " Enable several verbose tracing options");
    ("-trace-detailed-range", Arg.String
       (add_delimited_pair opt_trace_detailed_ranges '-'),
     "N-M As above, but only for an eip range");
    ("-trace-eip", Arg.Set(opt_trace_eip),
     " Print PC of each insn executed");
    ("-trace-eval", Arg.Set(opt_trace_eval),
     " Print details of IR evaluation");
    ("-trace-fpu", Arg.Set(opt_trace_fpu),
     " Print floating point operations");
    ("-trace-unique-eips", Arg.Set(opt_trace_unique_eips),
     " Print PC of each new insn executed");
    ("-trace-insns", Arg.Set(opt_trace_insns),
     " Print assembly-level instructions");
    ("-trace-ir", Arg.Set(opt_trace_ir),
     " Print Vine IR before executing it");
    ("-trace-orig-ir", Arg.Set(opt_trace_orig_ir),
     " Print Vine IR as produced by Asmir");
    ("-trace-iterations", Arg.Set(opt_trace_iterations),
     " Print iteration count");
    ("-trace-loads", Arg.Set(opt_trace_loads),
     " Print each memory load");
    ("-trace-stores", Arg.Set(opt_trace_stores),
     " Print each memory store");
    ("-trace-callstack", Arg.Set(opt_trace_callstack),
     " Print calls and returns");
    ("-trace-regions", Arg.Set(opt_trace_regions),
     " Print symbolic memory regions");
    ("-trace-registers", Arg.Set(opt_trace_registers),
     " Print register contents");
    ("-trace-setup", Arg.Set(opt_trace_setup),
     " Print progress of program loading");
    ("-trace-stmts", Arg.Set(opt_trace_stmts),
     " Print each IR statement executed");
    ("-trace-stopping", Arg.Set(opt_trace_stopping),
     " Print why paths terminate");
    ("-trace-sym-addrs", Arg.Set(opt_trace_sym_addrs),
     " Print symbolic address values");
    ("-trace-temps", Arg.Set(opt_trace_temps),
     " Print intermediate formulas");
    ("-trace-temps-encoded", Arg.Set(opt_trace_temps_encoded),
     " -trace-temps in a line-noise-like format");
    ("-gc-stats", Arg.Set(opt_gc_stats),
     " Print memory usage statistics");
    ("-time-stats", Arg.Set(opt_time_stats),
     " Print running time statistics");
    ("-periodic-stats", Arg.String
       (fun s -> opt_periodic_stats := Some (Int64.of_string s)),
     "period Trigger statistics every PERIOD instructions");
    ("-watch-expr", Arg.String
       (fun s -> opt_watch_expr_str := Some s),
     "expr Print Vine expression on each instruction");
    ("-tracepoint", Arg.String
       (fun s -> add_delimited_num_str_pair opt_tracepoint_strings
	  ':' s),
     "eip:expr Print scalar expression on given EIP");
    ("-tracepoint-string", Arg.String
       (fun s -> add_delimited_num_str_pair opt_string_tracepoint_strings
	  ':' s),
     "eip:expr Print string expression on given EIP");
    ("-check-condition-at", Arg.String
       (fun s -> let (eip_s, expr_s) = split_string ':' s in
	  opt_check_condition_at_strings :=
	    (eip_s, expr_s) :: !opt_check_condition_at_strings),
     "eip:expr Check boolean assertion at address");
    ("-finish-on-nonfalse-cond", Arg.Set(opt_finish_on_nonfalse_cond),
     " Finish exploration if -c-c-a condition could be true");
    ("-finish-reasons-needed", Arg.Set_int(opt_finish_reasons_needed),
     "n Require N finish reasons to finish");
    ("-extra-condition", Arg.String
       (fun s -> opt_extra_condition_strings :=
	  s :: !opt_extra_condition_strings),
     "cond Add an extra constraint for solving");
    ("-extra-conditions-file", Arg.String
       (fun s -> opt_extra_condition_strings :=
	  !opt_extra_condition_strings @ (read_lines_file s)),
     "filename Read '-extra-condition's one per line from file");
    ("-omit-pf-af", Arg.Set(opt_omit_pf_af),
     " Omit computation of the (rarely used) PF and AF flags");
    ("-nop-system-insns", Arg.Set(opt_nop_system_insns),
     " Treat some unhandled system instructions as no-ops");
    ("-x87-emulator", Arg.String (fun s -> opt_x87_emulator := Some s),
     "emulator.so Enable x87 emulation with given code");
    ("-final-pc", Arg.Set(opt_final_pc),
     " Print final path condition at end of trace");
    ("-solve-final-pc", Arg.Set(opt_solve_final_pc),
     " Solve final path condition");
    ("-git-version", Arg.Unit
       (fun () -> Printf.printf "GIT version %s\n" Git_version.git_version),
     " Print GIT revision hash");
  ]

let trace_replay_cmdline_opts =
  [
    ("-solve-path-conditions", Arg.Set(opt_solve_path_conditions),
     " Solve conditions along a concrete path");
    ("-check-read-operands", Arg.Set(opt_check_read_operands),
     " Compare insn inputs against trace");
    ("-check-write-operands", Arg.Set(opt_check_write_operands),
     " Compare insn outputs against trace");
    ("-fix-write-operands", Arg.Set(opt_fix_write_operands),
     " Modify outputs to match trace");
    ("-trace-segments", Arg.Set(opt_trace_segments),
     " Print messages about non-default segments");
    ("-trace-taint", Arg.Set(opt_trace_taint),
     " Print messages about tainted values");
    ("-trace-unexpected", Arg.Set(opt_trace_unexpected),
     " Print when our execution doesn't match the trace");
    ("-progress-interval", Arg.String
       (fun s -> opt_progress_interval := Some (Int64.of_string s)),
     "insns Print every INSNsth instruction");
    ("-skip-untainted", Arg.Set(opt_skip_untainted),
     " Skip replaying instructions that are not tainted");
  ]

let set_program_name s =
  match !opt_program_name with 
    | None -> opt_program_name := Some s
    | Some prev ->
	Printf.printf "Multiple args: %s, %s\n" prev s;
	failwith "Multiple non-option args not allowed"

let default_on_missing = ref (fun fm -> fm#on_missing_zero)

let apply_cmdline_opts_early (fm : Fragment_machine.fragment_machine) dl =
  if !opt_random_memory then
    fm#on_missing_random
  else if !opt_zero_memory then
    fm#on_missing_zero
  else if !opt_symbolic_memory then
    fm#on_missing_symbol
  else
    (!default_on_missing fm);
  (match !opt_watch_expr_str with
     | Some s -> opt_watch_expr :=
	 Some (Vine_parser.parse_exp_from_string dl s)
     | None -> ());
  (match !opt_measure_expr_influence_at_strings with
     | Some (eip_s, expr_s) ->
	 opt_measure_expr_influence_at :=
	   Some ((Int64.of_string eip_s),
		 (Vine_parser.parse_exp_from_string dl expr_s))
     | None -> ());
  opt_check_condition_at :=
    List.map (fun (eip_s, expr_s) ->
		((Int64.of_string eip_s),
		 (Vine_parser.parse_exp_from_string dl expr_s)))
      !opt_check_condition_at_strings;
  opt_tracepoints := List.map
    (fun (eip, s) ->
       (eip, s, (Vine_parser.parse_exp_from_string dl s)))
    !opt_tracepoint_strings;
  opt_string_tracepoints := List.map
	(fun (eip, s) ->
	   (eip, s, (Vine_parser.parse_exp_from_string dl s)))
	!opt_string_tracepoint_strings;
  if !opt_symbolic_regs then
    fm#make_regs_symbolic
  else
    fm#make_regs_zero;
  fm#add_special_handler
    ((new Special_handlers.trap_special_nonhandler fm)
     :> Fragment_machine.special_handler);
  fm#add_special_handler
    ((new Special_handlers.cpuid_special_handler fm)
     :> Fragment_machine.special_handler)

let apply_cmdline_opts_late (fm : Fragment_machine.fragment_machine) =
  (* If the user specified both a -state file and -symbolic-regs, we
     want the symbolic register values to override the concrete values
     from the state. It would be nice to find a more elegant way of
     achieving this. *)
  if !opt_symbolic_regs then
    fm#make_regs_symbolic;
  (match (!opt_initial_eax, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_EAX v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RAX v
     | (Some v, ARM) -> failwith "ARM has no %eax or %rax"
     | (None, _) -> ());
  (match (!opt_initial_ebx, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_EBX v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RBX v
     | (Some v, ARM) -> failwith "ARM has no %ebx or %rbx"
     | (None, _) -> ());
  (match (!opt_initial_ecx, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_ECX v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RCX v
     | (Some v, ARM) -> failwith "ARM has no %ecx or %rcx"
     | (None, _) -> ());
  (match (!opt_initial_edx, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_EDX v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RDX v
     | (Some v, ARM) -> failwith "ARM has no %edx or %rdx"
     | (None, _) -> ());
  (match (!opt_initial_esi, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_ESI v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RSI v
     | (Some v, ARM) -> failwith "ARM has no %esi or %rsi"
     | (None, _) -> ());
  (match (!opt_initial_edi, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_EDI v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RDI v
     | (Some v, ARM) -> failwith "ARM has no %edi or %rdi"
     | (None, _) -> ());
  (match (!opt_initial_esp, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_ESP v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RSP v
     | (Some v, ARM) -> fm#set_word_var Fragment_machine.R13 v
     | (None, _) -> ());
  (match (!opt_initial_ebp, !opt_arch) with
     | (Some v, X86) -> fm#set_word_var Fragment_machine.R_EBP v
     | (Some v, X64) -> fm#set_long_var Fragment_machine.R_RBP v
     | (Some v, ARM) -> failwith "ARM has no %ebp or %rbp"
     | (None, _) -> ());
  (match !opt_initial_eflagsrest with
     | Some v -> fm#set_word_var Fragment_machine.EFLAGSREST v
     | None -> ());
  List.iter (fun (addr,v) -> fm#store_byte_conc addr 
	       (Int64.to_int v)) !opt_store_bytes;
  List.iter (fun (addr,v) -> fm#store_short_conc addr
	       (Int64.to_int v)) !opt_store_shorts;
  List.iter (fun (addr,v) -> fm#store_word_conc addr v) !opt_store_words;
  List.iter (fun (addr,v) -> fm#store_long_conc addr v) !opt_store_longs

let apply_cmdline_opts_nonlinux (fm : Fragment_machine.fragment_machine) =
  fm#add_special_handler
    ((new Special_handlers.linux_special_nonhandler fm)
     :> Fragment_machine.special_handler)

let make_symbolic_init (fm:Fragment_machine.fragment_machine) 
    (infl_man:Exec_no_influence.influence_manager) =
  (fun () ->
     let new_max i =
       max_input_string_length :=
	 max (!max_input_string_length) (Int64.to_int i)
     in
       opt_extra_conditions := [];
       List.iter (fun (base, len) ->
		    new_max len;
		    fm#make_symbolic_region base (Int64.to_int len))
	 !opt_symbolic_regions;
       List.iter (fun (base, len) ->
		    new_max len;
		    fm#store_symbolic_cstr base (Int64.to_int len) false false)
	 !opt_symbolic_strings;
       List.iter (fun (base, len) ->
		    new_max len;
		    fm#store_symbolic_cstr base (Int64.to_int len) false true)
	 !opt_symbolic_cstrings;
       List.iter (fun (base, len) ->
		    new_max len;
		    fm#store_symbolic_cstr base (Int64.to_int len) true true)
	 !opt_symbolic_cstrings_fulllen;
       List.iter (fun (base, str) ->
		    new_max (Int64.of_int (String.length str));
		    fm#store_concolic_cstr base str false)
	 !opt_concolic_strings;
       List.iter (fun (base, str) ->
		    new_max (Int64.of_int (String.length str));
		    fm#store_concolic_cstr base str true)
	 !opt_concolic_cstrings;
       if !opt_concolic_prob <> None then
	 opt_concrete_path_simulate := true;
       List.iter (fun (base, len) ->
		    new_max (Int64.mul 2L len);
		    fm#store_symbolic_wcstr base (Int64.to_int len))
	 !opt_symbolic_string16s;
       List.iter (fun (addr, varname) ->
		    fm#store_symbolic_byte addr varname)
	 !opt_symbolic_bytes;
       List.iter (fun (addr, varname) ->
		    infl_man#store_symbolic_byte_influence addr varname)
	 !opt_symbolic_bytes_influence;
       List.iter (fun (addr, varname) ->
		    fm#store_symbolic_short addr varname)
	 !opt_symbolic_shorts;
       List.iter (fun (addr, varname) ->
		    infl_man#store_symbolic_short_influence addr varname)
	 !opt_symbolic_shorts_influence;
       List.iter (fun (addr, varname) ->
		    fm#store_symbolic_word addr varname)
	 !opt_symbolic_words;
       List.iter (fun (addr, varname) ->
		    infl_man#store_symbolic_word_influence addr varname)
	 !opt_symbolic_words_influence;
       List.iter (fun (addr, varname) ->
		    fm#store_symbolic_long addr varname)
	 !opt_symbolic_longs;
       List.iter (fun (addr, varname) ->
		    infl_man#store_symbolic_long_influence addr varname)
	 !opt_symbolic_longs_influence;
       List.iter (fun (varname, size) ->
		    fm#make_sink_region varname size)
	 !opt_sink_regions;
       opt_target_region_formulas :=
	 List.map (fun s -> fm#parse_symbolic_expr s)
	   !opt_target_region_formula_strings;
       opt_extra_conditions := !opt_extra_conditions @ 
	 List.map (fun s -> fm#parse_symbolic_expr s)
	   !opt_extra_condition_strings)

let decide_start_addrs () =
  let (start_addr, fuzz_start) = match
    (!opt_start_addr, !opt_fuzz_start_addr,
     !state_start_addr) with
      | (None,     None,      None) ->
	  failwith "Missing starting address"
      | (None,     None,      Some ssa) -> (ssa,  ssa)
      | (None,     Some ofsa, Some ssa) -> (ssa,  ofsa)
      | (None,     Some ofsa, None    ) -> (ofsa, ofsa)
      | (Some osa, Some ofsa, _       ) -> (osa,  ofsa)
      | (Some osa, None,      _       ) -> (osa,  osa)
  in
    if !opt_trace_setup then
      Printf.printf "%s 0x%08Lx, fuzz start 0x%08Lx\n"
	"Starting address" start_addr fuzz_start;
    (start_addr, fuzz_start)

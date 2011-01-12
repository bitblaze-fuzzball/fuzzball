(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

type offset_strategy = UniformStrat | BiasedSmallStrat

let offset_strategy_of_string s =
  match s with
    | "uniform" -> UniformStrat
    | "biased-small" -> BiasedSmallStrat
    | _ -> failwith "Unknown offset strategy"

let max_input_string_length = ref 0
let input_string_mem_prefix = ref None

let next_periodic_influence : int ref = ref (-1)

let opt_trace_temps = ref false
let opt_use_tags = ref false
let opt_print_callrets = ref false
let opt_fail_offset_heuristic = ref true
let opt_trace_solver = ref false
let opt_measure_influence_syscall_args = ref false
let opt_solver_timeout = ref None
let opt_solver_slow_time = ref 1.0
let opt_save_solver_files = ref false
let opt_stp_path = ref "stp"
let opt_follow_path  = ref ""
let opt_iteration_limit = ref 1000000000000L
let opt_watch_expr_str = ref None
let opt_watch_expr = ref None
let opt_path_depth_limit = ref 1000000000000L
let opt_query_branch_limit = ref 999999999
let opt_random_seed = ref 0
let opt_trace_decision_tree = ref false
let opt_save_decision_tree_interval = ref None
let opt_trace_randomness = ref false
let opt_measure_influence_derefs = ref false
let opt_measure_influence_reploops = ref false
let opt_measure_deref_influence_at = ref None
let opt_measure_expr_influence_at = ref None
let opt_multipath_influence_only = ref false
let opt_stop_at_measurement = ref false
let opt_periodic_influence = ref None
let opt_influence_bound = ref (-2.0)
let opt_disqualify_addrs = ref []
let opt_check_condition_at = ref None
let opt_trace_assigns = ref false
let opt_trace_assigns_string = ref false
let opt_trace_conditions = ref false
let opt_trace_decisions = ref false
let opt_trace_binary_paths = ref false
let opt_trace_binary_paths_delimited = ref false
let opt_trace_binary_paths_bracketed = ref false
let opt_trace_insns = ref false
let opt_trace_loads = ref false
let opt_trace_stores = ref false
let opt_trace_sym_addrs = ref false
let opt_trace_sym_addr_details = ref false
let opt_trace_syscalls = ref false
let opt_trace_detailed_ranges = ref []
let opt_extra_conditions = ref []
let opt_tracepoints = ref []
let opt_string_tracepoints = ref []
let opt_concrete_path = ref false
let opt_solve_path_conditions = ref false
let opt_trace_regions = ref false
let opt_check_for_null = ref false
let opt_offset_strategy = ref UniformStrat
let opt_concretize_divisors = ref false
let opt_trace_stopping = ref false
let opt_trace_setup = ref false
let opt_extra_env = Hashtbl.create 10
let opt_skip_call_addr = ref []
let opt_skip_func_addr = ref []
let opt_skip_call_addr_symbol = ref []
let opt_skip_func_addr_symbol = ref []
let opt_trace_eip = ref false
let opt_trace_unique_eips = ref false
let opt_trace_ir = ref false
let opt_trace_orig_ir = ref false
let opt_trace_iterations = ref false
let opt_coverage_stats = ref false
let opt_gc_stats = ref false
let opt_solver_stats = ref false
let opt_time_stats = ref false
let opt_nonfatal_solver = ref false
let opt_num_paths = ref None
let opt_pid = ref (-1)
let opt_translation_cache_size = ref None
let opt_prefix_out = ref false
let opt_omit_pf_af = ref false
let opt_symbolic_syscall_error = ref None
let opt_chroot_path = ref None
let opt_finish_on_nonfalse_cond = ref false

let opt_symbolic_memory = ref false
let opt_zero_memory = ref false
let opt_random_memory = ref false

let opt_fuzz_end_addrs = ref []

let opt_check_read_operands = ref false
let opt_check_write_operands = ref false
let opt_fix_write_operands = ref false
let opt_trace_registers = ref false
let opt_trace_segments = ref false
let opt_trace_taint = ref false
let opt_trace_unexpected = ref false
let opt_progress_interval = ref None
let opt_final_pc = ref false
let opt_solve_final_pc = ref false
let opt_skip_untainted = ref false

let split_string char s =
  let delim_loc = String.index s char in
  let s1 = String.sub s 0 delim_loc in
  let s2 = String.sub s (delim_loc + 1) ((String.length s) - delim_loc - 1)
  in
    (s1, s2)

let unescape str =
  let len = String.length str in
  let s' = String.create len in
  let rec loop i j =
    if i >= len then
      j
    else
      match str.[i] with
	| '\\' when i + 1 < len ->
	    let char inc c =
	      s'.[j] <- c;
	      loop (i + inc + 1) (j + 1)
	    in
	      (match str.[i+1] with
		 | 'n' -> char 1 '\n'
		 | 'r' -> char 1 '\r'
		 | 't' -> char 1 '\t'
		 | 'b' -> char 1 '\b'
		 | '\\' -> char 1 '\\'
		 | '\'' -> char 1 '\''
		 | '"' -> char 1 '"'
		 | ' ' -> char 1 ' '
		 | 'x' when i + 3 < len ->
		     char 3 (Char.chr (int_of_string
					 ("0x" ^ (String.sub str (i+2) 2))))
		 | '0' .. '9' when i + 3 < len ->
		     char 3 (Char.chr (int_of_string
					 (String.sub str (i+1) 3)))
		 | _ -> failwith "Unexpected escape in string unescape"
	      )
	| _ ->
	    s'.[j] <- str.[i];
	    loop (i+1) (j+1)
  in
  let len' = loop 0 0 in
    String.sub s' 0 len'

let add_delimited_pair opt char s =
  let (s1, s2) = split_string char s in
    opt := ((Int64.of_string s1), (Int64.of_string s2)) :: !opt

let add_delimited_num_str_pair opt char s =
  let (s1, s2) = split_string char s in
    opt := ((Int64.of_string s1), s2) :: !opt

let add_delimited_num_escstr_pair opt char s =
  let (s1, s2) = split_string char s in
    opt := ((Int64.of_string s1), (unescape s2)) :: !opt

let add_delimited_str_num_pair opt char s =
  let (s1, s2) = split_string char s in
    opt := (s1, (Int64.of_string s2)) :: !opt

let opt_program_name = ref None
let opt_start_addr = ref None
let opt_argv = ref []
let state_start_addr = ref None

(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

type offset_strategy = UniformStrat | BiasedSmallStrat

val offset_strategy_of_string : string -> offset_strategy

val max_input_string_length : int ref
val next_periodic_influence : int ref

val opt_trace_temps : bool ref
val opt_use_tags : bool ref
val opt_print_callrets : bool ref
val opt_fail_offset_heuristic : bool ref
val opt_trace_solver : bool ref
val opt_measure_influence_syscall_args : bool ref
val opt_solver_timeout : int option ref
val opt_solver_slow_time : float ref
val opt_save_solver_files : bool ref
val opt_stp_path : string ref
val opt_follow_path : string ref
val opt_iteration_limit : int64 ref
val opt_watch_expr_str : string option ref
val opt_watch_expr : Vine.exp option ref
val opt_path_depth_limit : int64 ref
val opt_query_branch_limit : int ref
val opt_random_seed : int ref
val opt_trace_decision_tree : bool ref
val opt_trace_randomness : bool ref
val opt_measure_influence_derefs : bool ref
val opt_measure_influence_reploops : bool ref
val opt_measure_deref_influence_at : int64 option ref
val opt_measure_expr_influence_at : (int64 * Vine.exp) option ref
val opt_multipath_influence_only : bool ref
val opt_stop_at_measurement : bool ref
val opt_periodic_influence : int option ref
val opt_influence_bound : float ref
val opt_disqualify_addrs : int64 list ref
val opt_check_condition_at : (int64 * Vine.exp) option ref
val opt_trace_assigns : bool ref
val opt_trace_assigns_string : bool ref
val opt_trace_conditions : bool ref
val opt_trace_decisions : bool ref
val opt_trace_binary_paths : bool ref
val opt_trace_binary_paths_delimited : bool ref
val opt_trace_binary_paths_bracketed : bool ref
val opt_trace_insns : bool ref
val opt_trace_loads : bool ref
val opt_trace_stores : bool ref
val opt_trace_sym_addrs : bool ref
val opt_trace_sym_addr_details : bool ref
val opt_trace_syscalls : bool ref
val opt_trace_detailed_ranges : (int64 * int64) list ref
val opt_extra_conditions : Vine.exp list ref
val opt_tracepoints : (int64 * string * Vine.exp) list ref
val opt_string_tracepoints : (int64 * string * Vine.exp) list ref
val opt_concrete_path : bool ref
val opt_solve_path_conditions : bool ref
val opt_trace_regions : bool ref
val opt_check_for_null : bool ref
val opt_offset_strategy : offset_strategy ref
val opt_concretize_divisors : bool ref
val opt_trace_stopping : bool ref
val opt_trace_setup : bool ref
val opt_extra_env : (string, string) Hashtbl.t
val opt_skip_call_addr : (int64 * int64) list ref
val opt_skip_call_addr_symbol : (int64 * string) list ref
val opt_trace_eip : bool ref
val opt_trace_ir : bool ref
val opt_trace_orig_ir : bool ref
val opt_trace_iterations : bool ref
val opt_coverage_stats : bool ref
val opt_gc_stats : bool ref
val opt_solver_stats : bool ref
val opt_time_stats : bool ref
val opt_nonfatal_solver : bool ref
val opt_num_paths : int64 option ref
val opt_check_write_operands : bool ref
val opt_pid : int ref

val opt_fuzz_end_addrs : int64 list ref

val opt_trace_registers : bool ref
val opt_trace_segments : bool ref
val opt_trace_taint : bool ref

(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

type offset_strategy = UniformStrat | BiasedSmallStrat

val offset_strategy_of_string : string -> offset_strategy

type execution_arch = X86 | X64 | ARM

val execution_arch_of_string : string -> execution_arch

val asmir_arch_of_execution_arch : execution_arch -> Asmir.arch
val libasmir_arch_of_execution_arch : execution_arch ->
  Libasmir.asmir_arch

val max_input_string_length : int ref
val input_string_mem_prefix : string option ref

val next_periodic_influence : int ref

val opt_trace_temps : bool ref
val opt_trace_temps_encoded : bool ref
val opt_use_tags : bool ref
val opt_fail_offset_heuristic : bool ref
val opt_trace_solver : bool ref
val opt_measure_influence_syscall_args : bool ref
val opt_solver_timeout : int option ref
val opt_timeout_as_unsat : bool ref
val opt_solver_slow_time : float ref
val opt_save_solver_files : bool ref
val opt_solver_path : string ref
val opt_follow_path : string ref
val opt_branch_preference : (int64, int64) Hashtbl.t
val opt_branch_preference_unchecked : (int64, int64) Hashtbl.t
val opt_always_prefer : bool option ref
val opt_iteration_limit : int64 ref
val opt_iteration_limit_enforced : int64 option ref
val opt_insn_limit : int64 ref
val opt_watch_expr_str : string option ref
val opt_watch_expr : Vine.exp option ref
val opt_path_depth_limit : int64 ref
val opt_query_branch_limit : int ref
val opt_random_seed : int ref
val opt_trace_decision_tree : bool ref
val opt_save_decision_tree_interval : float option ref
val opt_decision_tree_use_file : bool ref
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
val opt_check_condition_at : (int64 * Vine.exp) list ref
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
val opt_trace_callstack : bool ref
val opt_trace_sym_addrs : bool ref
val opt_trace_sym_addr_details : bool ref
val opt_trace_syscalls : bool ref
val opt_trace_detailed_ranges : (int64 * int64) list ref
val opt_extra_conditions : Vine.exp list ref
val opt_tracepoints : (int64 * string * Vine.exp) list ref
val opt_string_tracepoints : (int64 * string * Vine.exp) list ref
val opt_concrete_path : bool ref
val opt_concrete_path_simulate : bool ref
val opt_concolic_prob : float option ref
val opt_solve_path_conditions : bool ref
val opt_trace_regions : bool ref
val opt_check_for_null : bool ref
val opt_offset_strategy : offset_strategy ref
val opt_concretize_divisors : bool ref
val opt_trace_stopping : bool ref
val opt_trace_setup : bool ref
val opt_extra_env : (string, string) Hashtbl.t
val opt_skip_call_addr : (int64 * int64) list ref
val opt_skip_func_addr : (int64 * int64) list ref
val opt_skip_call_addr_symbol : (int64 * string) list ref
val opt_skip_func_addr_symbol : (int64 * string) list ref
val opt_skip_call_addr_region : (int64 * string) list ref
val opt_skip_func_addr_region : (int64 * string) list ref
val opt_skip_call_addr_symbol_once : (int64 * string) list ref
val opt_trace_eip : bool ref
val opt_trace_unique_eips : bool ref
val opt_trace_ir : bool ref
val opt_trace_orig_ir : bool ref
val opt_trace_iterations : bool ref
val opt_coverage_stats : bool ref
val opt_gc_stats : bool ref
val opt_solver_stats : bool ref
val opt_time_stats : bool ref
val opt_nonfatal_solver : bool ref
val opt_num_paths : int64 option ref
val opt_pid : int ref
val opt_external_uname : bool ref
val opt_translation_cache_size : int option ref
val opt_prefix_out : string option ref
val opt_omit_pf_af : bool ref
val opt_nop_system_insns : bool ref
val opt_symbolic_syscall_error : int64 option ref
val opt_stop_on_symbolic_syscall_args : bool ref
val opt_skip_output_concretize : bool ref
val opt_chroot_path : string option ref
val opt_finish_on_nonfalse_cond : bool ref
val opt_finish_reasons_needed : int ref
val opt_total_timeout : float option ref
val opt_x87_emulator : string option ref
val opt_x87_entry_point : int64 option ref
val opt_trace_fpu : bool ref
val opt_target_region_start : int64 option ref
val opt_target_region_string : string ref
val opt_target_region_formula_strings : string list ref
val opt_target_region_formulas : Vine.exp list ref
val opt_trace_target : bool ref
val opt_target_no_prune : bool ref
val opt_finish_on_target_match : bool ref
val opt_target_guidance : float ref
val opt_trace_guidance : bool ref
val opt_trace_tables : bool ref
val opt_table_limit : int ref
val opt_offset_limit : int ref
val opt_trace_offset_limit : bool ref
val opt_no_table_store : bool ref
val opt_implied_value_conc : bool ref
val opt_trace_ivc : bool ref
val opt_ite_ivc : bool ref
val opt_periodic_stats : int64 option ref
val opt_trace_global_ce_cache : bool ref
val opt_trace_working_ce_cache : bool ref
val opt_global_ce_cache_limit : int ref
val opt_disable_ce_cache : bool ref
val opt_narrow_bitwidth_cutoff : int option ref
val opt_t_expr_size : int ref

val opt_symbolic_memory : bool ref
val opt_zero_memory : bool ref
val opt_random_memory : bool ref

val opt_fuzz_start_addr_count : int ref 
val opt_fuzz_end_addrs : int64 list ref
val opt_trace_end_jump : int64 option ref

val opt_check_read_operands : bool ref
val opt_check_write_operands : bool ref
val opt_fix_write_operands : bool ref
val opt_trace_registers : bool ref
val opt_trace_segments : bool ref
val opt_trace_taint : bool ref
val opt_trace_unexpected : bool ref
val opt_progress_interval : int64 option ref
val opt_final_pc : bool ref
val opt_solve_final_pc : bool ref
val opt_skip_untainted : bool ref
val opt_arch : execution_arch ref
val opt_trace_stmts : bool ref
val opt_trace_eval : bool ref

val asmir_arch : unit -> Asmir.arch

val split_string : char -> string -> (string * string)
val add_delimited_pair :
  (int64 * int64) list ref -> char -> string -> unit
val add_delimited_num_str_pair :
  (int64 * string) list ref -> char -> string -> unit
val add_delimited_num_escstr_pair :
  (int64 * string) list ref -> char -> string -> unit
val add_delimited_str_num_pair :
  (string * int64) list ref -> char -> string -> unit

val opt_program_name : string option ref
val opt_start_addr : int64 option ref
val opt_argv : string list ref
val state_start_addr : int64 option ref

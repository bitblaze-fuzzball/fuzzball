module EO = Exec_options
module ESO = Exec_set_options
module OL = Options_linux
module SL = State_loader
module OS = Options_solver

type parameter = Bool of bool ref
		 | BoolOpt of bool option ref
		 | Float of float ref
		 | FloatOpt of float option ref
		 | Int of int ref
		 | Int64 of int64 ref
		 | Int64Hashtbl of (int64, int64) Hashtbl.t
		 | Int64List of int64 list ref
		 | Int64Opt of int64 option ref
		 | Int64PairList of (int64 * int64) list ref
		 | Int64StringPairList of (int64 * string) list ref
		 | IntOpt of int option ref
		 | String of string ref
		 | StringHashtbl of (string, string) Hashtbl.t
		 | StringInt64PairList of (string * int64) list ref
		 | StringList of string list ref
		 | StringOpt of string option ref
		 | StringPairList of (string * string) list ref
		 | StringPairOpt of (string * string) option ref

let parameter_table =  Hashtbl.create 0

let add_parameter key value = 
  if not (Hashtbl.mem parameter_table key) then
    Hashtbl.add parameter_table key value

let logging = ref "NOT IMPLEMENTED"

let  getParameterTable () =
  if Hashtbl.length parameter_table == 0 then (

    add_parameter "LOGGING" (String logging);
(*
    add_parameter "--always" (StringPairList OLG.opt_always);
    add_parameter "--debug" (StringPairList OLG.opt_debug);
    add_parameter "--trace" (StringPairList OLG.opt_trace);
    add_parameter "--never" (StringPairList OLG.opt_never);
*)
    (* Options_solver.solver_cmdline_opts *)

    add_parameter "-solver" (String OS.opt_solver);
    add_parameter "-solver-check-against" (String OS.opt_solver_check_against);
    add_parameter "-stp-path" (String EO.opt_stp_path);
    add_parameter "-save-solver-files" (Bool EO.opt_save_solver_files);
    add_parameter "-solver-slow-time" (Float EO.opt_solver_slow_time);
    add_parameter "-solver-timeout" (IntOpt EO.opt_solver_timeout);
    add_parameter "-trace-assigns" (Bool EO.opt_trace_assigns);
    add_parameter "-trace-assigns-string" (Bool EO.opt_trace_assigns_string);
    add_parameter "-trace-solver" (Bool EO.opt_trace_solver);
    add_parameter "-solver-stats" (Bool EO.opt_solver_stats);
    add_parameter "-nonfatal-solver" (Bool EO.opt_nonfatal_solver);

    (* Options_linux.linux_cmdline_opts *)

    add_parameter "-core" (StringOpt OL.opt_core_file_name);
    add_parameter "-pid" (Int EO.opt_pid);
    add_parameter "-use-ids-from-core" (Bool OL.opt_use_ids_from_core);
    add_parameter "-external-uname" (Bool EO.opt_external_uname);
    add_parameter "-setup-init-proc-state" (BoolOpt OL.opt_setup_initial_proc_state);
    add_parameter "-load-region" (Int64PairList OL.opt_load_extra_regions);
    add_parameter "-load-base" (Int64Opt OL.opt_load_base);
    add_parameter "-load-data" (Bool OL.opt_load_data);
    add_parameter "-env" (StringHashtbl EO.opt_extra_env);
    add_parameter "-tls-base" (Int64Opt OL.opt_tls_base);
    add_parameter "-linux-syscalls" (Bool OL.opt_linux_syscalls);
    add_parameter "-trace-syscalls" (Bool EO.opt_trace_syscalls);
    add_parameter "-prefix-out" (StringOpt EO.opt_prefix_out);
    add_parameter "-symbolic-file" (StringList OL.opt_symbolic_files);
    add_parameter "-symbolic-syscall-error" (Int64Opt EO.opt_symbolic_syscall_error);
    add_parameter "-stop-on-symbolic-syscall-args" (Bool EO.opt_stop_on_symbolic_syscall_args);
    add_parameter "-chroot" (StringOpt EO.opt_chroot_path);
    add_parameter "-decree" (Bool EO.opt_decree);
    add_parameter "-symbolic-receive" (Bool EO.opt_symbolic_receive);
    add_parameter "-symbolic-random" (Bool EO.opt_symbolic_random);


    (* State_loader.state_loader_cmdline_opts *)
    
    add_parameter "-state" (StringOpt SL.opt_state_file);
    
    (* Exec_set_options.influence_cmdline_opts *)

    add_parameter "-disqualify-addr" (Int64List EO.opt_disqualify_addrs);
    add_parameter "-symbolic-byte-influence" (Int64StringPairList ESO.opt_symbolic_bytes_influence);
    add_parameter "-symbolic-short-influence" (Int64StringPairList ESO.opt_symbolic_shorts_influence);
    add_parameter "-symbolic-word-influence" (Int64StringPairList ESO.opt_symbolic_words_influence);
    add_parameter "-symbolic-long-influence" (Int64StringPairList ESO.opt_symbolic_longs_influence);
    add_parameter "-measure-influence-derefs" (Bool EO.opt_measure_influence_derefs);
    add_parameter "-measure-influence-reploops" (Bool EO.opt_measure_influence_reploops);
    add_parameter "-measure-influence-syscall-args" (Bool EO.opt_measure_influence_syscall_args);
    add_parameter "-measure-deref-influence-at" (Int64Opt EO.opt_measure_deref_influence_at);
    add_parameter "-multipath-influence-only" (Bool EO.opt_multipath_influence_only);
    add_parameter "-stop-at-measurement"  (Bool EO.opt_stop_at_measurement);
    add_parameter "-measure-expr-influence-at" (StringPairOpt ESO.opt_measure_expr_influence_at_strings);
    add_parameter "-periodic-influence" (IntOpt EO.opt_periodic_influence);
    add_parameter "-influence-bound" (Float EO.opt_influence_bound);

    (* Exec_set_options.concrete_state_cmdline_opts *)

    add_parameter "-start-addr" (Int64Opt EO.opt_start_addr);
    add_parameter "-initial-eax" (Int64Opt ESO.opt_initial_eax);
    add_parameter "-initial-ebx" (Int64Opt ESO.opt_initial_ebx);
    add_parameter "-initial-ecx" (Int64Opt ESO.opt_initial_ecx);
    add_parameter "-initial-edx" (Int64Opt ESO.opt_initial_edx);
    add_parameter "-initial-esi" (Int64Opt ESO.opt_initial_esi);
    add_parameter "-initial-edi" (Int64Opt ESO.opt_initial_edi);
    add_parameter "-initial-esp" (Int64Opt ESO.opt_initial_esp);
    add_parameter "-initial-ebp" (Int64Opt ESO.opt_initial_ebp);
    add_parameter "-initial-eflagsrest" (Int64Opt ESO.opt_initial_eflagsrest);
    add_parameter "-store-byte" (Int64PairList ESO.opt_store_bytes);
    add_parameter "-store-short" (Int64PairList ESO.opt_store_shorts);
    add_parameter "-store-word" (Int64PairList ESO.opt_store_words);
    add_parameter "-store-long" (Int64PairList ESO.opt_store_longs);
    add_parameter "-skip-call-ret" (Int64PairList EO.opt_skip_call_addr);
    add_parameter "-skip-func-ret" (Int64PairList EO.opt_skip_func_addr);

    (* Exec_set_options.symbolic_state_cmdline_opts *)
    add_parameter "-symbolic-region" (Int64PairList ESO.opt_symbolic_regions);
    add_parameter "-symbolic-string" (Int64PairList ESO.opt_symbolic_strings);
    add_parameter "-symbolic-cstring" (Int64PairList ESO.opt_symbolic_cstrings);
    add_parameter "-symbolic-cstring-fulllen" (Int64PairList ESO.opt_symbolic_cstrings_fulllen);
    add_parameter "-symbolic-string16" (Int64PairList ESO.opt_symbolic_string16s);
    add_parameter "-symbolic-regs" (Bool ESO.opt_symbolic_regs);
    add_parameter "-symbolic-byte" (Int64StringPairList ESO.opt_symbolic_bytes);
    add_parameter "-symbolic-short" (Int64StringPairList ESO.opt_symbolic_shorts);
    add_parameter "-symbolic-word" (Int64StringPairList ESO.opt_symbolic_words);
    add_parameter "-symbolic-long" (Int64StringPairList ESO.opt_symbolic_longs);
    add_parameter "-sink-region" (StringInt64PairList ESO.opt_sink_regions);
    add_parameter "-skip-call-ret-symbol" (Int64StringPairList EO.opt_skip_call_addr_symbol);
    add_parameter "-skip-func-ret-symbol" (Int64StringPairList EO.opt_skip_func_addr_symbol);
    add_parameter "-skip-call-ret-region" (Int64StringPairList EO.opt_skip_call_addr_region);

    add_parameter "-skip-func-ret-region" (Int64StringPairList EO.opt_skip_func_addr_region);

    (* Exec_set_options.concolic_state_cmdline_opts *)

    add_parameter "-concrete-path" (Bool EO.opt_concrete_path);
    add_parameter "-solve-path-conditions" (Bool EO.opt_solve_path_conditions);
    add_parameter "-concolic-string" (Int64StringPairList ESO.opt_concolic_strings);
    add_parameter "-concolic-cstring" (Int64StringPairList ESO.opt_concolic_cstrings);
    add_parameter "-concolic-cstring-file" (Int64StringPairList ESO.opt_concolic_cstring_files);
    add_parameter "-concolic-prob" (FloatOpt EO.opt_concolic_prob);

  (* Exec_set_options.explore_cmdline_opts *)

    add_parameter "-fuzz-start-addr" (Int64Opt ESO.opt_fuzz_start_addr);
    add_parameter "-fuzz-start-addr-count" (Int EO.opt_fuzz_start_addr_count);

    add_parameter "-fuzz-end-addr" (Int64List EO.opt_fuzz_end_addrs);
    add_parameter "-iteration-limit" (Int64 EO.opt_iteration_limit);
    add_parameter "-path-depth-limit" (Int64 EO.opt_path_depth_limit);
    add_parameter "-query-branch-limit" (Int EO.opt_query_branch_limit);
    add_parameter "-num-paths" (Int64Opt EO.opt_num_paths);
    add_parameter "-concretize-divisors" (Bool EO.opt_concretize_divisors);
    add_parameter "-trace-binary-paths-delimited" (Bool EO.opt_trace_binary_paths_delimited);
    add_parameter "-trace-binary-paths-bracketed" (Bool EO.opt_trace_binary_paths_bracketed);
    add_parameter "-trace-decision-tree" (Bool EO.opt_trace_decision_tree);
    add_parameter "-trace-randomness" (Bool EO.opt_trace_randomness);
    add_parameter "-trace-sym-addr-details" (Bool EO.opt_trace_sym_addr_details);
    add_parameter "-coverage-stats" (Bool EO.opt_coverage_stats);
    add_parameter "-offset-strategy" (String EO.opt_offset_strategy_string);
    add_parameter "-follow-path" (String EO.opt_follow_path);
    add_parameter "-branch-preference" (Int64Hashtbl EO.opt_branch_preference);
    add_parameter "-random-seed" (Int EO.opt_random_seed);
    add_parameter "-save-decision-tree-interval" (FloatOpt EO.opt_save_decision_tree_interval);
    add_parameter "-decision-tree-use-file" (Bool EO.opt_decision_tree_use_file);
    add_parameter "-total-timeout" (FloatOpt EO.opt_total_timeout);
    add_parameter "-target-string" (StringPairList EO.opt_target_strings);
    add_parameter "-target-string-file" (StringPairList EO.opt_target_string_files);
    add_parameter "-target-formulas" (StringPairList EO.opt_target_formulas);
    add_parameter "-trace-target" (Bool EO.opt_trace_target);
    add_parameter "-finish-on-target-match" (Bool EO.opt_finish_on_target_match);
    add_parameter "-target-guidance" (Float EO.opt_target_guidance);
    add_parameter "-trace-guidance" (Bool EO.opt_trace_guidance);    add_parameter "-trace-guidance" (Bool EO.opt_trace_guidance);
    add_parameter "-trace-tables" (Bool EO.opt_trace_tables);
    add_parameter "-table-limit" (Int EO.opt_table_limit);
    add_parameter "-implied-value-conc" (Bool EO.opt_implied_value_conc);
    add_parameter "-trace-ivc" (Bool EO.opt_trace_ivc);
    add_parameter "-trace-working-ce-cache" (Bool EO.opt_trace_working_ce_cache);
    add_parameter "-trace-global-ce-cache" (Bool EO.opt_trace_global_ce_cache);
    add_parameter "-global-ce-cache-limit" (Int EO.opt_global_ce_cache_limit);

    (* Exec_set_options.tags_cmdline_opts *)
    
     add_parameter "-use-tags" (Bool EO.opt_use_tags);   

     (* Exec_set_options.fuzzball_cmdline_opts *)

     add_parameter "-check-for-null"  (Bool EO.opt_check_for_null);
     add_parameter "-finish-on-null-deref" (Bool EO.opt_finish_on_null_deref);
     add_parameter "-print-callrets" (Bool EO.opt_print_callrets);
     add_parameter "-no-fail-on-huer" (Bool EO.opt_fail_offset_heuristic);

    (* Exec_set_options.cmdline_opts *)
    
    add_parameter "-arch" (StringOpt EO.opt_arch_string);
    add_parameter "-translation-cache-size" (IntOpt EO.opt_translation_cache_size);
    add_parameter "-random-memory" (Bool EO.opt_random_memory);
    add_parameter "-symbolic-memory" (Bool EO.opt_symbolic_memory);
    add_parameter "-zero-memory" (Bool EO.opt_zero_memory);
    add_parameter "-trace-basic" (Bool EO.opt_trace_basic);
    add_parameter "-trace-binary_paths" (Bool EO.opt_trace_binary_paths);
    add_parameter "-trace-conditions" (Bool EO.opt_trace_conditions);
    add_parameter "-trace-decisions" (Bool EO.opt_trace_decisions);
    add_parameter "-trace-detailed" (Bool EO.opt_trace_detailed);
    add_parameter "-trace-detailed-range" (Int64PairList EO.opt_trace_detailed_ranges);
    add_parameter "-trace-eip" (Bool EO.opt_trace_eip);
    add_parameter "-trace-fpu" (Bool EO.opt_trace_fpu);
    add_parameter "-trace-unique-eips" (Bool EO.opt_trace_unique_eips);
    add_parameter "-trace-insns" (Bool EO.opt_trace_insns);
    add_parameter "-trace-ir" (Bool EO.opt_trace_ir);
    add_parameter "-trace-orig-ir" (Bool EO.opt_trace_orig_ir);
    add_parameter "-trace-EO.opt_iterations" (Bool EO.opt_trace_iterations);
    add_parameter "-trace-loads" (Bool EO.opt_trace_loads); 
    add_parameter "-trace-stores" (Bool EO.opt_trace_stores);
    add_parameter "-trace-regions" (Bool EO.opt_trace_regions);
    add_parameter "-trace-registers" (Bool EO.opt_trace_registers);
    add_parameter "-trace-setup" (Bool EO.opt_trace_setup);
    add_parameter "-trace-stopping" (Bool EO.opt_trace_stopping); 
    add_parameter "-trace-sym-addrs" (Bool EO.opt_trace_sym_addrs);
    add_parameter "-trace-temps" (Bool EO.opt_trace_temps);
    add_parameter "-trace-temps-encoded" (Bool EO.opt_trace_temps_encoded);
    add_parameter "-gc_stats" (Bool EO.opt_gc_stats);
    add_parameter "-time_stats" (Bool EO.opt_time_stats);
    add_parameter "-periodic_stats" (Int64Opt EO.opt_periodic_stats);
    add_parameter "-watch-expr" (StringOpt EO.opt_watch_expr_str);
    add_parameter "-tracepoint" (Int64StringPairList ESO.opt_tracepoint_strings);
    add_parameter "-tracepoint-string" (Int64StringPairList ESO.opt_string_tracepoint_strings);
    add_parameter "-check-condition-at" (StringPairList ESO.opt_check_condition_at_strings);
    add_parameter "-finish-on-nonfalse-cond" (Bool EO.opt_finish_on_nonfalse_cond);
    add_parameter "-extra-condition" (StringList ESO.opt_extra_condition_strings);
    add_parameter "-check-for-jump-to" (Int64List EO.opt_check_for_jump_to);
    add_parameter "-finish-on-controlled-jump" (Bool EO.opt_finish_on_controlled_jump);
    add_parameter "-omit-pf-af" (Bool EO.opt_omit_pf_af);
    add_parameter "-nop-system-insns" (Bool EO.opt_nop_system_insns);
    add_parameter "-x87-emulator" (StringOpt EO.opt_x87_emulator);
    add_parameter "-print-pc" (Bool EO.opt_final_pc);
    add_parameter "-solve-final-pc" (Bool EO.opt_solve_final_pc);
    add_parameter "-svn-version" (Bool ESO.opt_svn_version);

    (* Exec_options_table.trace_replay_cmdline_opts *)

    add_parameter "-solve-path-conditions" (Bool EO.opt_solve_path_conditions);
    add_parameter "-check-read-operands" (Bool EO.opt_check_read_operands);
    add_parameter "-check-write-operands" (Bool EO.opt_check_write_operands);
    add_parameter "-fix-write-operands" (Bool EO.opt_fix_write_operands);
    add_parameter "-trace-segments" (Bool EO.opt_trace_segments);
    add_parameter "-trace-taint" (Bool EO.opt_trace_taint);
    add_parameter "-trace-unexpected" (Bool EO.opt_trace_unexpected);
    add_parameter "-progress-interval" (Int64Opt EO.opt_progress_interval);
    add_parameter "-skip-untainted" (Bool EO.opt_skip_untainted);
  );
  parameter_table


let undefined_string = "undefined"
let empty_string = "()"

let stringPairToString s1 s2 =
  Printf.sprintf "{%s, %s}" s1 s2

let isLastInList l =
  let count = ref 0 in
  (fun () ->
    count := !count + 1;
    List.length l == !count
  )

let isLastInHashtbl h =
  let count = ref 0 in
  (fun () ->
    count := !count + 1;
    Hashtbl.length h == !count
  )

let boolToString b = Pervasives.string_of_bool b

let boolOptToString b = 
  match b with
  | Some b -> boolToString b
  | None -> undefined_string

let floatToString f = Pervasives.string_of_float f

let floatOptToString f =
  match f with
  | Some f -> floatToString f
  | None -> undefined_string

let intToString i = Pervasives.string_of_int i

let intOptToString i =
  match i with
  | Some i -> intToString i
  | None -> undefined_string 

let int64ToString i = Int64.to_string i

let int64HashtblToString ih =
  if Hashtbl.length ih == 0 then empty_string
  else
    let isLast = isLastInHashtbl ih in
    Hashtbl.fold (fun key value accum ->
      let pair = stringPairToString (int64ToString key)
	(int64ToString value) in
      if isLast () then Printf.sprintf "%s%s)" accum pair
      else Printf.sprintf "%s%s, " accum pair
    ) ih "("

let int64ListToString il =
  if List.length il == 0 then empty_string
  else
    let isLast = isLastInList il in
    List.fold_left (fun accum i ->
      if isLast () then Printf.sprintf "%s%s)" accum (int64ToString i)
      else Printf.sprintf "%s%s, " accum (int64ToString i)
    ) "(" il

let int64OptToString i =
  match i with
  | Some i -> int64ToString i
  | None -> undefined_string 

let int64PairListToString iil =
  if List.length iil == 0 then empty_string
  else
    let isLast = isLastInList iil in
    List.fold_left (fun accum (i1, i2) ->
      let pair = stringPairToString (int64ToString i1) (int64ToString i2) in
      if isLast () then Printf.sprintf "%s%s)" accum pair
      else Printf.sprintf "%s%s, " accum pair
    ) "(" iil
      
let int64StringPairListToString isl =
  if List.length isl == 0 then empty_string
  else
    let isLast = isLastInList isl in
    List.fold_left (fun accum (i, s) ->
      let pair = stringPairToString (int64ToString i) s in
      if isLast () then Printf.sprintf "%s%s)" accum pair
      else Printf.sprintf "%s%s, " accum pair
    ) "(" isl

(* I don't know maybe some day you want to do something weird *)
let stringToString s = s 

let stringHashtblToString sh =
  if Hashtbl.length sh == 0 then empty_string
  else
    let isLast = isLastInHashtbl sh in
    Hashtbl.fold (fun key value accum ->
      let pair = stringPairToString key value in
      if isLast () then Printf.sprintf "%s%s)" accum pair
      else Printf.sprintf "%s%s, " accum pair
    ) sh "("

let stringInt64PairListToString sil =
  if List.length sil == 0 then empty_string
  else
    let isLast = isLastInList sil in
    List.fold_left (fun accum (s, i) ->
      let pair = stringPairToString s (int64ToString i) in
      if isLast () then Printf.sprintf "%s%s)" accum pair
      else Printf.sprintf "%s%s, " accum pair
    ) "(" sil

let stringListToString sl =
  if List.length sl == 0 then empty_string
  else
    let isLast = isLastInList sl in
    List.fold_left (fun accum s ->
      if isLast () then Printf.sprintf "%s%s)" accum s
      else Printf.sprintf "%s%s, " accum s
    ) "(" sl

let stringOptToString s =
  match s with
  | Some s -> stringToString s
  | None -> undefined_string

let stringPairListToString ssl =
  if List.length ssl == 0 then empty_string
  else
    let isLast = isLastInList ssl in
    List.fold_left (fun accum (s1, s2) ->
      let pair = stringPairToString s1 s2 in
      if isLast () then Printf.sprintf "%s%s)" accum pair
      else Printf.sprintf "%s%s, " accum pair
    ) "(" ssl

let stringPairOptToString spo =
  match spo with
  | Some sp -> 
    let s1, s2 = sp in
    stringPairToString s1 s2
  | None -> undefined_string

let parameterToString param =
  match param with
  | Bool b -> boolToString !b
  | BoolOpt b -> boolOptToString !b
  | Float f -> floatToString !f
  | FloatOpt f -> floatOptToString !f
  | Int i -> intToString !i
  | Int64 i -> int64ToString !i
  | Int64Hashtbl ih -> int64HashtblToString ih
  | Int64List il -> int64ListToString !il
  | Int64Opt i -> int64OptToString !i
  | Int64PairList iil -> int64PairListToString !iil
  | Int64StringPairList isl -> int64StringPairListToString !isl
  | IntOpt i -> intOptToString !i
  | String s -> stringToString !s
  | StringHashtbl sh -> stringHashtblToString sh
  | StringInt64PairList sil -> stringInt64PairListToString !sil
  | StringList sl -> stringListToString !sl
  | StringOpt s -> stringOptToString !s
  | StringPairList ssl -> stringPairListToString !ssl
  | StringPairOpt ss -> stringPairOptToString !ss

let parameterToJSON key param =
  match param with
  | Bool b -> (key, `Bool !b)
  | Float f -> (key, `Float !f)
  | BoolOpt b ->
    (key ^ "_optional",
     `Variant (key, 
	       (match !b with
	       | Some b -> Some  (`Bool b)
	       | None -> None
	       )
     )
    )
  | FloatOpt f ->
    (key ^ "_optional",
     `Variant (key, 
	       (match !f with
	       | Some f -> Some  (`Float f)
	       | None -> None
	       )
     )
    )
  | Int i -> (key, `Int !i)
  | Int64 i -> (key, (`String (int64ToString !i)))
  | Int64Hashtbl ih ->
    (key, 
     `Assoc (
       Hashtbl.fold (fun key value accum ->
	 ((int64ToString key),
	  `String (int64ToString value)) :: accum
       ) ih []
     )
    )
  | Int64List il -> 
    (key, 
     `List (
       List.fold_left (fun accum value ->
	 (`String (int64ToString value)) :: accum
       ) [] !il
     )
    )
  | Int64Opt i ->
    (key ^ "_optional",
     `Variant (key, 
	       (match !i with
	       | Some i -> Some (`String (int64ToString i))
	       | None -> None
	       )
     )
    )
  | Int64PairList iil ->
    (key,
     `List (
       List.fold_left (fun accum (first, second) ->
	 (`Tuple [
	   `String (int64ToString first);
	   `String (int64ToString second)]) :: accum
       ) [] !iil
     )
  )
  | Int64StringPairList isl ->
    (key, 
     `List (
       List.fold_left (fun accum (first, second) ->
	 (`Tuple [
	   `String (int64ToString first);
	   `String second]) :: accum
       ) [] !isl
     )
    )
  | IntOpt i ->
    (key ^ "_optional",
     `Variant (key, 
	       (match !i with
	       | Some i -> Some (`Int i)
	       | None -> None
	       )
     )
    )
  | String s -> (key, `String !s)
  | StringHashtbl sh ->
    (key, 
     `Assoc (
       Hashtbl.fold (fun key value accum ->
	 (key,
	  `String value) :: accum
       ) sh []
     )
    )
  | StringInt64PairList sil ->
    (key, 
     `List (
       List.fold_left (fun accum (first, second) ->
	 (`Tuple [
	   `String first;
	   `String (int64ToString second)]) :: accum
       ) [] !sil
     )
    )
  | StringList sl ->
    (key, 
     `List (
       List.fold_left (fun accum value ->
	 (`String value) :: accum
       ) [] !sl
     )
    )
  | StringOpt s ->
    (key ^ "_optional",
     `Variant (key, 
	       (match !s with
	       | Some s -> Some (`String s)
	       | None -> None
	       )
     )
    )
  | StringPairList ssl ->
    (key, 
     `List (
       List.fold_left (fun accum (first, second) ->
	 (`Tuple [
	   `String first;
	   `String second]) :: accum
       ) [] !ssl
     )
    )
  | StringPairOpt ss ->
    (key ^ "_optional",
     `Variant (key, 
	       (match !ss with
	       | Some ss -> 
		 let first, second = ss in
		 Some (`Tuple [`String first; `String second])
	       | None -> None
	       )
     )
    )

let logParametersText logFunction =
  let parameterTable = getParameterTable () in
  Hashtbl.iter (fun key value ->
    logFunction (
      Text_logger.LazyString (
	lazy (
	  let strValue = parameterToString value in
	  Printf.sprintf "%s=%s" key strValue
	)
      )
    )
  ) parameterTable

let logParametersJSON logFunction =
  let parameterTable = getParameterTable () in
  Hashtbl.iter (fun key value ->
    logFunction (
      Yojson_logger.LazyJson (
	lazy (
	  let jsonValue = parameterToJSON key value in
	  `Assoc [jsonValue]
	)
      )
    )
  ) parameterTable

(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_utils;;
open Exec_domain;;
open Exec_exceptions;;
open Exec_options;;
open Formula_manager;;
open Query_engine;;
open Stp_external_engine;;
open Smtlib_external_engine;;
open Exec_no_influence;;
open Granular_memory;;
open Fragment_machine;;
open Decision_tree;;

let solver_sats = ref 0L
let solver_unsats = ref 0L
let solver_fails = ref 0L

module SymPathFragMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module FormMan = FormulaManagerFunctor(D)
  module GM = GranularMemoryFunctor(D)
  module FM = FragmentMachineFunctor(D)

  class sym_path_frag_machine (dt:decision_tree) = object(self)
    inherit FM.frag_machine as fm

    method get_depth = dt#get_depth
    method get_hist_str = dt#get_hist_str
    method measure_dt_size = dt#measure_size

    val mutable infl_man = ((new no_influence_manager) :> influence_manager)

    method set_influence_manager im = infl_man <- im

    val mutable query_engine = new stp_external_engine "fuzz"

    method set_query_engine qe = query_engine <- qe

    method match_input_var s =
      try
	let len = String.length s in
	let ismp = match !input_string_mem_prefix with
	  | None -> "input0_byte_"
	  | Some s -> s in
	let plen = String.length ismp in
	  if len > plen && (String.sub s 0 plen) = ismp
	  then
	    let wo_input = String.sub s plen (len - plen) in
	      Some (int_of_string wo_input)
	  else if len >= 7 && (String.sub s 0 7) = "input0_" then
	    let wo_input = String.sub s 7 (len - 7) in
	      Some (int_of_string wo_input)
	  else
	    None
      with
	| Not_found -> None
	| Failure "int_of_string" -> None

    method private ce_to_input_str ce =
      (* Ideally, I'd like to turn high characters into \\u1234 escapes *)
      let char_of_int_unbounded i =
	if i >= 0 && i <= 255 then
	  char_of_int i
	else
	  '?'
      in
      let str = String.make (!max_input_string_length) ' ' in
	List.iter
	  (fun (var_s, value) ->
	     match self#match_input_var var_s with
	       | Some n -> 
		   assert(n < !max_input_string_length);
		   str.[n] <-
		     char_of_int_unbounded (Int64.to_int value)
	       | None -> ())
	  ce;
	let str' = ref str in
	  (try 
	     while String.rindex !str' ' ' = (String.length !str') - 1 do
	       str' := String.sub !str' 0 ((String.length !str') - 1)
	     done;
	   with Not_found -> ());
	  (try
	     while String.rindex !str' '\000' = (String.length !str') - 1
	     do
	       str' := String.sub !str' 0 ((String.length !str') - 1)
	     done;
	   with Not_found -> ());
	  !str'

    method print_ce ce = print_ce ce

    val mutable path_cond = []
    val mutable var_seen_hash = V.VarHash.create 101
    val mutable global_ce_cache = Hashtbl.create 1009
    val mutable global_ce_limit = !opt_global_ce_cache_limit
    val mutable working_ce_cache = []
    val mutable new_path = false

    method private print_global_cache =
      let print_entry ce_ref count_ref =
	Printf.printf "CE ~~~ ";
	self#print_ce !ce_ref;
	Printf.printf "Count ~~~ %d\n\n" !count_ref
      in
        Hashtbl.iter print_entry global_ce_cache

    method private print_working_cache =
      let rec loop = function
        | ce_ref :: rest ->
	    self#print_ce !ce_ref;
	    loop rest
	| [] -> ()
      in
        loop working_ce_cache

    method private add_to_global_cache ce_ref =
      try
	let count_ref = Hashtbl.find global_ce_cache ce_ref in
	  count_ref := !count_ref + 1
      with
	Not_found ->
	  if !opt_trace_working_ce_cache || !opt_trace_global_ce_cache then
	    (Printf.printf "Adding CE to global cache: ";
	     self#print_ce !ce_ref);
	  Hashtbl.add global_ce_cache ce_ref (ref 0);
	  if (Hashtbl.length global_ce_cache) > global_ce_limit then
	    self#prune_global_cache

    method private add_to_working_cache ce_ref =
      working_ce_cache <- ce_ref :: working_ce_cache

    method private prune_global_cache =
      let goal = global_ce_limit / 2 in
      let rec loop cutoff =
	let f key value =
	  if !value < cutoff then
	    Hashtbl.remove global_ce_cache key
	in
	  Hashtbl.iter f global_ce_cache;
	  if !opt_trace_global_ce_cache || !opt_trace_working_ce_cache then
	    Printf.printf "Global cache size after pruning counts <%d: %d\n"
	      cutoff (Hashtbl.length global_ce_cache);
	  if (Hashtbl.length global_ce_cache > goal) then
	    loop (cutoff lsl 1)
	  else
	    self#reset_global_cache_counts
      in
        loop 1

    method private reset_global_cache_counts =
      let f key value =
	value := 0
      in
        Hashtbl.iter f global_ce_cache

    method private fill_working_cache =
      match (List.rev path_cond) with
        | cond :: rest ->
	    let conj = List.fold_left
	      (fun es e -> V.BinOp(V.BITAND, e, es)) cond rest
	    in
	    let select ce_ref count_ref =
	      if (form_man#eval_expr_from_ce !ce_ref conj) <> 0L then
		self#add_to_working_cache ce_ref
	    in
	      Hashtbl.iter select global_ce_cache
	| [] -> ()

    method private filter_working_cache new_cond =
      let conj = List.fold_left
	(fun es e -> V.BinOp(V.BITAND, e, es)) new_cond path_cond
      in
      let filter ce_ref = (form_man#eval_expr_from_ce !ce_ref conj) <> 0L in
        working_ce_cache <- List.filter filter working_ce_cache

    method input_depth =
      let count = ref 0 in
	V.VarHash.iter
	  (fun v _ ->
	     form_man#if_expr_temp_unit v
	       (function
		  | Some _ -> ()
		  | None -> count := !count + 1
	       )
	  )
	  var_seen_hash;
	!count

    method private ensure_extra_conditions =
      if path_cond = [] && !opt_extra_conditions <> [] then
	List.iter
	  (fun cond ->
	     (* Similar to self#add_to_path_cond, but without the call
		to ensure_extra_conditions. *)
	     path_cond <- cond :: path_cond;
	     self#push_cond_to_qe cond)
	  (List.rev !opt_extra_conditions)

    method get_path_cond =
      self#ensure_extra_conditions;
      path_cond

    method private quick_check_in_path_cond cond =
      if List.mem cond path_cond then
	Some true
      else
	match cond with
	  | V.UnOp(V.NOT, cond') when List.mem cond' path_cond
	      -> Some false
	  | _ -> None 

    method private push_cond_to_qe cond =
      let (decls, assigns, cond_e, new_vars) =
	form_man#one_cond_for_solving cond var_seen_hash
      in
	List.iter query_engine#add_free_var decls;
	List.iter (fun (v,_) -> query_engine#add_temp_var v) assigns;
	List.iter (fun (v,e) -> query_engine#assert_eq v e) assigns;
	query_engine#add_condition cond_e

    method add_to_path_cond cond =
      self#ensure_extra_conditions;
      if (self#quick_check_in_path_cond cond) <> Some true then
	(self#filter_working_cache cond;
	 path_cond <- cond :: path_cond;
	 self#push_cond_to_qe cond)

    method restore_path_cond f =
      self#ensure_extra_conditions;
      let saved_pc = path_cond in
      let saved_vsh = var_seen_hash in
	var_seen_hash <- V.VarHash.copy saved_vsh; (* could be expensive *)
	query_engine#push;
	let ret = f () in
	  query_engine#pop;
	  path_cond <- saved_pc;
	  var_seen_hash <- saved_vsh;
	  ret

    method query_with_path_cond cond verbose =
      self#query_with_path_cond_wcache cond verbose true

    method private query_with_path_cond_wcache cond verbose with_cache =
      self#ensure_extra_conditions;
      if new_path then
	(self#fill_working_cache;
	 new_path <- false);
      let get_time () = Unix.gettimeofday () in
      let cond' = form_man#rewrite_for_solver cond in
      let conj = List.fold_left
	(fun es e -> V.BinOp(V.BITAND, e, es)) cond' (List.rev path_cond)
      in
      let ce_opt =
	let rec loop = function
	  | ce_ref :: rest
	      when (form_man#eval_expr_from_ce !ce_ref conj) <> 0L ->
	      Some !ce_ref
	  | ce_ref :: rest -> loop rest
	  | [] -> None
	in
	  loop working_ce_cache
      in
      let (is_sat, ce) = match (ce_opt, with_cache) with
	| (Some ce', true) ->
	    if !opt_trace_working_ce_cache || !opt_trace_global_ce_cache then
	      (Printf.printf "CE Cache Hit: ";
	       self#print_ce ce');
	    (true, ce')
	| _ ->
	    let (decls, assigns, cond_e, new_vars) =
	      form_man#one_cond_for_solving cond var_seen_hash
	    in
	      query_engine#push;
	      query_engine#start_query;
	      List.iter query_engine#add_free_var decls;
	      List.iter (fun (v,_) -> query_engine#add_temp_var v) assigns;
	      List.iter (fun (v,e) -> query_engine#assert_eq v e) assigns;
	      let time_before = get_time () in
	      let (result_o, ce') = query_engine#query cond_e in
	      let is_sat' = match result_o with
		| Some true ->
		    solver_unsats := Int64.succ !solver_unsats;
		    false
	        | Some false ->
		    solver_sats := Int64.succ !solver_sats;
		    true
		| None ->
		    solver_fails := Int64.succ !solver_fails;
		    query_engine#after_query true;
		    raise SolverFailure
	      in
	      let time = (get_time ()) -. time_before in
	      let is_slow = time > !opt_solver_slow_time in
	        if is_slow then
		Printf.printf "Slow query (%f sec)\n"
		  ((get_time ()) -. time_before);
	        flush stdout;
		query_engine#after_query is_slow;
		query_engine#pop;
		List.iter (fun v -> V.VarHash.remove var_seen_hash v) new_vars;
		infl_man#maybe_periodic_influence;
		if is_sat' then
		  self#add_to_working_cache (ref ce');
		(is_sat', ce')
      in
        if verbose then
	  (if is_sat then
	     (if !opt_trace_decisions then
		Printf.printf "Satisfiable.\n";
	      if !opt_trace_assigns_string then
		Printf.printf "Input: \"%s\"\n"
		  (Exec_utils.escaped (self#ce_to_input_str ce));
	      if !opt_trace_assigns then
		(Printf.printf "Input vars: ";
		 self#print_ce ce))
	   else
	     if !opt_trace_decisions then
	       Printf.printf "Unsatisfiable.\n");
        if is_sat then
	  (self#add_to_global_cache (ref ce);
	   if !opt_trace_global_ce_cache then
	     (Printf.printf "\n******* Global Cache *******\n";
	      self#print_global_cache;
	      Printf.printf "****************************\n");
	   if !opt_trace_global_ce_cache || !opt_trace_working_ce_cache then
	     (Printf.printf "\n^^^^^^^ Working Cache ^^^^^^^\n";
	      self#print_working_cache;
	      Printf.printf "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n\n"));
	(is_sat, ce)


    method query_unique_value exp ty =
      let taut = V.BinOp(V.EQ, exp, exp) in
      let (is_sat, ce) = self#query_with_path_cond taut !opt_trace_ivc in
	assert(is_sat);
	if !opt_trace_ivc then
	  Printf.printf "QUV of %s\n%!" (V.exp_to_string exp);
	let v = form_man#eval_expr_from_ce ce exp in
	  if !opt_trace_ivc then
	    Printf.printf "Sat value is 0x%Lx\n%!" v;
	  let const_e = V.Constant(V.Int(ty, v)) in
	  let another_exp = V.BinOp(V.NEQ, exp, const_e) in
	  let (is_another, ce2) =
	    self#query_with_path_cond another_exp !opt_trace_ivc
	  in
	    if is_another then
	      let v2 = form_man#eval_expr_from_ce ce2 exp in
		assert(v2 <> v);
		if !opt_trace_ivc then
		  Printf.printf "Not unique, another is 0x%Lx\n%!" v2;
		None
	    else
	      (if !opt_trace_ivc then
		 Printf.printf "Unique!\n%!";
	       Some v)

    method private check_concolic_value exp ty =
      let conc_val = form_man#eval_expr exp in
      let conc_e = V.Constant(V.Int(ty, conc_val)) in
      let cond = V.BinOp(V.EQ, exp, conc_e) in
      let (is_sat, ce) = self#query_with_path_cond cond true in
	if not is_sat then
	  Printf.printf "Concolic value 0x%Lx for %s is infeasible\n"
	    conc_val (V.exp_to_string exp);
	assert(is_sat);
	Printf.printf "Concolic value 0x%Lx for %s is feasible\n"
	  conc_val (V.exp_to_string exp)

    method eval_int_exp_simplify exp =
      let (d, ty) = self#eval_int_exp_ty exp in
	form_man#simplify_with_callback
	  (fun e2 ty ->
	     (* self#check_concolic_value e2 ty; *)
	     if !opt_implied_value_conc then
	       match self#query_unique_value e2 ty with
		 | Some v ->
		     Some (V.Constant(V.Int(ty, v)))
		 | None -> None
	     else
	       None)
	  d ty

    method follow_or_random =
	let currpath_str = dt#get_hist_str in
	let followplen = String.length !opt_follow_path and
	    currplen = String.length currpath_str in
	let pref =
          if followplen > currplen then
	    let follow_prefix = String.sub !opt_follow_path 0 currplen in
	      if follow_prefix = currpath_str
	      then
		Some ((String.sub !opt_follow_path currplen 1) = "1")
	      else 
		None
          else
            None
	in
	  match pref with
	    | Some b -> b
	    | None ->
		(match !opt_always_prefer with
		   | Some b -> b
		   | _ ->
		       if !opt_trace_guidance then
			 Printf.printf "No guidance, choosing randomly\n";
		       dt#random_bit)

    method query_with_pc_choice cond verbose choice =
      let trans_func b =
	if b then cond else V.UnOp(V.NOT, cond)
      in
      let try_func b cond' =
	if verbose && !opt_trace_decisions then
	  Printf.printf "Trying %B: " b;
	match self#quick_check_in_path_cond cond' with
	  | Some b -> b
	  | None -> 
	      let (is_sat, _) =
		self#query_with_path_cond cond' verbose in
		is_sat
      in
      let non_try_func b =
	if verbose && !opt_trace_decisions then
	  Printf.printf "Known %B\n" b
      in
	if !opt_trace_binary_paths then
	  Printf.printf "Current Path String: %s\n" dt#get_hist_str;
	let r = dt#try_extend trans_func try_func non_try_func choice
	  self#get_eip
	in
	  if !opt_trace_binary_paths then
	    Printf.printf "Current Path String: %s\n" dt#get_hist_str;
	  if !opt_trace_binary_paths_delimited then
	    Printf.printf "Current path: %s\n" dt#get_hist_str_queries;
	  if !opt_trace_binary_paths_bracketed then
	    Printf.printf "Current path: %s\n" dt#get_hist_str_bracketed;
	  r

    method extend_pc_random cond verbose =
      if !opt_concrete_path_simulate ||
	(match !opt_concolic_prob with
	   | Some p -> (dt#random_float < p)
	   | None -> false)
      then
	self#extend_pc_known cond verbose ((form_man#eval_expr cond) <> 0L)
      else
	let (result, cond') = (self#query_with_pc_choice cond verbose
				 (fun () -> self#follow_or_random)) in
	  self#add_to_path_cond cond';
	  result

    method extend_pc_known cond verbose b =
      let (result, cond') = (self#query_with_pc_choice cond verbose
			       (fun () -> b)) in
	self#add_to_path_cond cond';
	result

    (* Like _known, but check the concolic path before the supplied
       preference *)
    method extend_pc_pref cond verbose pref =
      if !opt_concrete_path_simulate ||
	(match !opt_concolic_prob with
	   | Some p -> (dt#random_float < p)
	   | None -> false)
      then
	self#extend_pc_known cond verbose ((form_man#eval_expr cond) <> 0L)
      else
	self#extend_pc_known cond verbose pref

    method random_case_split verbose =
      let trans_func b = V.Unknown("unused") in
      let try_func b _ =
	if verbose then Printf.printf "Trying %B: " b;
	true
      in
      let non_try_func b =
	if verbose then Printf.printf "Known %B\n" b
      in
      let (result, _) = (dt#try_extend trans_func try_func non_try_func
			   (fun () -> self#follow_or_random) self#get_eip) in
	result

    method private eval_bool_exp_conc_path e =
      let b = (form_man#eval_expr e) <> 0L in
	if !opt_trace_conditions then 
	  Printf.printf "Computed concrete value %b\n" b;
	if !opt_solve_path_conditions then
	  (let b' = self#extend_pc_known e true b in
	   let choices = dt#check_last_choices in
	     assert(b = b');
	     (b, choices))
	else
	  (self#add_to_path_cond
	     (if b then e else V.UnOp(V.NOT, e));
	   (b, Some b))

    method private eval_bool_exp_tristate exp choice =
      let v = self#eval_int_exp exp in
	try
	  if (D.to_concrete_1 v) = 1 then
	    (true, Some true)
	  else
	    (false, Some false)
	with
	    NotConcrete _ ->
	      let e = D.to_symbolic_1 v in
		if !opt_trace_conditions then 
		  Printf.printf "Symbolic branch condition (0x%08Lx) %s\n"
		    (self#get_eip) (V.exp_to_string e);
		if !opt_concrete_path then
		  self#eval_bool_exp_conc_path e
		else 
		  (dt#start_new_query_binary;
		   let b = match choice with
		     | None -> self#extend_pc_random e true
		     | Some bit -> self#extend_pc_known e true bit
		   in
		   let choices = dt#check_last_choices in
		     dt#count_query;
		     (b, choices))

    method eval_bool_exp e = 
      let (b, _) = self#eval_bool_exp_tristate e None in
	b

    val mutable cjmp_heuristic = None

    method set_cjmp_heuristic f = cjmp_heuristic <- Some f

    method private call_cjmp_heuristic eip t1 t2 dir =
      match cjmp_heuristic with
	| None -> None
	| Some h -> h eip t1 t2 (dt#random_float) dir

    method private cjmp_choose targ1 targ2 =
      let eip = self#get_eip in
	try let pref = Hashtbl.find opt_branch_preference eip in
	  match pref with
	    | 0L -> Some false
	    | 1L -> Some true
	    | _ -> failwith "Unsupported branch preference"
	with
	  | Not_found ->
	      match dt#heur_preference with
		| Some b ->
		    let choice = dt#random_float in
		      if choice < !opt_target_guidance then
			(if !opt_trace_guidance then
			   Printf.printf "On %f, using heuristic choice %b\n"
			     choice b;
			 Some b)
		      else
			(if !opt_trace_guidance then
			   Printf.printf "On %f, falling to cjmp_heuristic\n"
			     choice;
			 self#call_cjmp_heuristic eip targ1 targ2 None)
		| None -> (self#call_cjmp_heuristic eip targ1 targ2 None)

    method eval_cjmp exp targ1 targ2 =
      let eip = self#get_eip in
      let v = form_man#simplify1 (self#eval_int_exp exp) in
      let (is_conc, result) =
	if Hashtbl.mem opt_branch_preference_unchecked eip then
	  match Hashtbl.find opt_branch_preference_unchecked eip with
	    | 0L -> (true, false)
	    | 1L -> (true, true)
	    | _ -> failwith "Unsupported branch preference"
	else
	  try (true, (D.to_concrete_1 v) = 1)
	  with NotConcrete _ -> (false, false)
      in
	if is_conc then
	  (ignore(self#call_cjmp_heuristic eip targ1 targ2 (Some result));
	   result)
	else
	  let e = D.to_symbolic_1 v in
	    if !opt_trace_conditions then 
	      Printf.printf "Symbolic branch condition (0x%08Lx) %s\n"
		(self#get_eip) (V.exp_to_string e);
	    if !opt_concrete_path then
	      let (b, _) = self#eval_bool_exp_conc_path e in
		b
	    else
	      (dt#start_new_query_binary;
	       let choice = if dt#have_choice then
		 self#cjmp_choose targ1 targ2
	       else
		 None
	       in
	       let b = match choice with
		 | None -> self#extend_pc_random e true
		 | Some bit -> self#extend_pc_known e true bit
	       in
		 dt#count_query;
		 ignore(self#call_cjmp_heuristic eip targ1 targ2 (Some b));
		 b)

    method eval_addr_exp exp =
      let c32 x = V.Constant(V.Int(V.REG_32, x)) in
      let v = self#eval_int_exp_simplify exp in
	try (D.to_concrete_32 v)
	with
	    NotConcrete _ ->
	      let e = (D.to_symbolic_32 v) in
	      let eip = self#get_eip in
		if !opt_trace_sym_addrs then
		  Printf.printf "Symbolic address %s @ (0x%Lx)\n"
		    (V.exp_to_string e) eip;
		infl_man#maybe_measure_influence_deref e;
		dt#start_new_query;
		let bits = ref 0L in
		  self#restore_path_cond
		    (fun () ->
		       for b = 31 downto 0 do
			 let bit = self#extend_pc_random
			   (V.Cast(V.CAST_LOW, V.REG_1,
				   (V.BinOp(V.ARSHIFT, e,
					    (c32 (Int64.of_int b)))))) false
			 in
			   bits := (Int64.logor (Int64.shift_left !bits 1)
				      (if bit then 1L else 0L));
		       done);
		  Printf.printf "Picked concrete value 0x%Lx\n" !bits;
		  self#add_to_path_cond (V.BinOp(V.EQ, e, (c32 !bits)));
		  dt#count_query;
		  !bits

    method private on_missing_random_m (m:GM.granular_memory) =
      let rec random_int width =
	if width = 0 then 0 else
	  2 * (random_int width - 1) + 
	    (if self#random_case_split false then 1 else 0)
      in
      let rec random_int64 width =
	if width = 0 then 0L else
	  Int64.add (Int64.mul 2L (random_int64 (width - 1)))
	    (if self#random_case_split false then 1L else 0L)
      in
      m#on_missing
	(fun size _ -> match size with
	   | 8  -> D.from_concrete_8  (random_int 8)
	   | 16 -> D.from_concrete_16 (random_int 16)
	   | 32 -> D.from_concrete_32 (random_int64 32)
	   | 64 -> D.from_concrete_64 (random_int64 64)
	   | _ -> failwith "Bad size in on_missing_random")

    method on_missing_random =
      self#on_missing_random_m (mem :> GM.granular_memory)

    method private on_missing_zero_m (m:GM.granular_memory) =
      m#on_missing
	(fun size _ -> match size with
	   | 8  -> D.from_concrete_8  0
	   | 16 -> D.from_concrete_16 0
	   | 32 -> D.from_concrete_32 0L
	   | 64 -> D.from_concrete_64 0L
	   | _ -> failwith "Bad size in on_missing_zero")

    method on_missing_zero =
      self#on_missing_zero_m (mem :> GM.granular_memory)

    val mutable saved_details_flags =
      (false, false, false, false, false, false, false, false)

    method eip_hook eip = 
      fm#eip_hook eip;
      List.iter
	(fun (f_eip, _) -> 
	   if f_eip = eip then
	     (saved_details_flags <- 
		(!opt_trace_insns, !opt_trace_loads, !opt_trace_stores,
		 !opt_trace_temps, !opt_trace_syscalls, !opt_trace_registers,
		 !opt_trace_segments, !opt_trace_taint);	      
	      opt_trace_insns := true;
	      opt_trace_loads := true;
	      opt_trace_stores := true;
	      opt_trace_temps := true;
	      opt_trace_syscalls := true;
	      opt_trace_registers := true;
	      opt_trace_segments := true;
	      opt_trace_taint := true))
	!opt_trace_detailed_ranges;
      List.iter
	(fun (eip', e_str, expr) ->
	   if eip = eip' then
	     let str = self#eval_expr_to_string expr in
	       Printf.printf "At %08Lx, %s is %s\n"
		 eip e_str str)
	!opt_tracepoints;
      List.iter
	(fun (eip', e_str, expr) ->
	   if eip = eip' then
	     let str_addr = self#eval_addr_exp expr in
	     let str = if str_addr = 0L then
	       "(null)" else try 
		 let bytes = self#read_cstr str_addr in
		   (try
		      let line1 = String.sub bytes 0 (String.index bytes '\n')
		      in
			"\"" ^ (escaped line1) ^ "\\n\"" with
			  | Not_found -> "\"" ^ (escaped bytes) ^ "\"")
	       with
		   NotConcrete _ -> "<not concrete>" in
	       Printf.printf "At %08Lx, %s (%08Lx) is %s\n"
		 eip e_str str_addr str)
	!opt_string_tracepoints;
      infl_man#eip_hook eip;
      List.iter
	(fun (eip', expr) ->
	   if eip' = eip then
	     let (_, choices) = self#eval_bool_exp_tristate expr (Some true) in
	       Printf.printf "At 0x%08Lx, condition %s %s\n"
		 eip (V.exp_to_string expr)
		 (match choices with
		    | Some true -> "is true"
		    | Some false -> "is false"
		    | None -> "can be true or false");
	       (if !opt_finish_on_nonfalse_cond then
		 if choices <> Some false then
		   self#finish_fuzz "supplied condition non-false"
		 else
		   self#unfinish_fuzz "supplied condition false"))
	!opt_check_condition_at;
      List.iter
	(fun (_, t_eip) -> 
	   if t_eip = eip then
	     let (i, l, s, t, sc, r, sg, ta) = saved_details_flags in
	      opt_trace_insns := i;
	      opt_trace_loads := l;
	      opt_trace_stores := s;
	      opt_trace_temps := t;
	      opt_trace_syscalls := sc;
	      opt_trace_registers := r;
	      opt_trace_segments := sg;
	      opt_trace_taint := ta)
	!opt_trace_detailed_ranges
	  
    method finish_path =
      dt#set_heur 1;
      dt#mark_all_seen;
      infl_man#finish_path;
      if !opt_trace_binary_paths then
	Printf.printf "Path: %s\n" dt#get_hist_str;
      if !opt_trace_binary_paths_delimited then
	Printf.printf "Final path: %s\n" dt#get_hist_str_queries;
      if !opt_trace_binary_paths_bracketed then
	Printf.printf "Final path: %s\n" dt#get_hist_str_bracketed;
      if !opt_final_pc then
	(Printf.printf "Path condition: true\n";
	 List.iter (fun e -> Printf.printf "& (%s)\n" (V.exp_to_string e))
	   (List.rev (self#get_path_cond)));
      if !opt_solve_final_pc then
	assert(let (b,_) =
		 self#query_with_path_cond_wcache V.exp_true true false
	       in b);
      dt#try_again_p

    method print_tree chan = dt#print_tree chan

    method set_iter_seed i = dt#set_iter_seed i
      
    method reset () =
      fm#reset ();
      form_man#reset_mem_axioms;
      path_cond <- [];
      V.VarHash.clear var_seen_hash;
      query_engine#reset;
      infl_man#reset;
      dt#reset;
      working_ce_cache <- [];
      new_path <- true

    method after_exploration =
      infl_man#after_exploration
  end
end

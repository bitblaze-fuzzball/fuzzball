(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_domain;;
open Exec_exceptions;;
open Exec_options;;
open Formula_manager;;
open Query_engine;;
open Stp_external_engine;;
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

    val mutable path_cond = []

    method get_depth = dt#get_depth
    method get_hist_str = dt#get_hist_str

    val mutable infl_man = ((new no_influence_manager) :> influence_manager)

    method set_influence_manager im = infl_man <- im

    method get_path_cond = path_cond

    method add_to_path_cond cond =
      if path_cond = [] then
	path_cond <- !opt_extra_conditions;
      path_cond <- cond :: path_cond
      
    method restore_path_cond f =
      let saved_pc = path_cond in
      let ret = f () in
	path_cond <- saved_pc;
	ret

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
	  else if (String.sub s 0 7) = "input0_" then
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

    method query_with_path_cond path_cond_a cond verbose =
      let get_time () = Unix.gettimeofday () in
      let pc = cond :: path_cond_a and
	  expr = V.Unknown("") in
      let (decls, assigns, cond_e, _, _) =
	form_man#collect_for_solving [] pc expr
      in
      let assign_vars = List.map (fun (v, exp) -> v) assigns in
	query_engine#prepare decls assign_vars;
	List.iter (fun (v, exp) -> (query_engine#assert_eq v exp)) assigns;
	let time_before = get_time () in
	let (result_o, ce) = query_engine#query cond_e in
	let is_sat = match result_o with
	  | Some true ->
	      solver_unsats := Int64.succ !solver_unsats;
	      false
	  | Some false ->
	      solver_sats := Int64.succ !solver_sats;
	      true
	  | None ->
	      solver_fails := Int64.succ !solver_fails;
	      query_engine#unprepare true;
	      raise SolverFailure
	in
	  if verbose then
	    (if is_sat then
	       (if !opt_trace_decisions then
		  Printf.printf "Satisfiable.\n";
		if !opt_trace_assigns_string then
		  Printf.printf "Input: \"%s\"\n"
		    (String.escaped (self#ce_to_input_str ce));
		if !opt_trace_assigns then
		  (Printf.printf "Input vars: ";
		   self#print_ce ce))
	     else
	       if !opt_trace_decisions then Printf.printf "Unsatisfiable.\n");
	  let time = (get_time ()) -. time_before in
	  let is_slow = time > !opt_solver_slow_time in
	    if is_slow then
	      Printf.printf "Slow query (%f sec)\n"
		((get_time ()) -. time_before);
	    flush stdout;
	    query_engine#unprepare is_slow;
	    infl_man#maybe_periodic_influence;
	    (is_sat, ce)

    method follow_or_random =
	let currpath_str = dt#get_hist_str in
	let followplen = String.length !opt_follow_path and
	    currplen = String.length currpath_str in
        if followplen > currplen then
	  let follow_prefix = String.sub !opt_follow_path 0 currplen in
	    if follow_prefix = currpath_str
	    then
	      (String.sub !opt_follow_path currplen 1) = "1"
	    else 
	      dt#random_bit
          else
            dt#random_bit

    method query_with_pc_choice cond verbose choice =
      let trans_func b =
	if b then cond else V.UnOp(V.NOT, cond)
      in
      let try_func b cond' =
	if verbose && !opt_trace_decisions then
	  Printf.printf "Trying %B: " b;
	let (is_sat, _) = self#query_with_path_cond path_cond cond' verbose in
	  is_sat
      in
      let non_try_func b =
	if verbose && !opt_trace_decisions then
	  Printf.printf "Known %B\n" b
      in
	if !opt_trace_binary_paths then
	  Printf.printf "Current Path String: %s\n" dt#get_hist_str;
	let r = dt#try_extend trans_func try_func non_try_func choice in
	  if !opt_trace_binary_paths then
	    Printf.printf "Current Path String: %s\n" dt#get_hist_str;
	  if !opt_trace_binary_paths_delimited then
	    Printf.printf "Current path: %s\n" dt#get_hist_str_queries;
	  if !opt_trace_binary_paths_bracketed then
	    Printf.printf "Current path: %s\n" dt#get_hist_str_bracketed;
	  r

    method extend_pc_random cond verbose =
      let (result, cond') = (self#query_with_pc_choice cond verbose
			       (fun () -> self#follow_or_random)) in
	self#add_to_path_cond cond';
	result

    method extend_pc_known cond verbose b =
      let (result, cond') = (self#query_with_pc_choice cond verbose
			       (fun () -> b)) in
	self#add_to_path_cond cond';
	result

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
			   (fun () -> self#follow_or_random)) in
	result

    method eval_bool_exp exp =
      let v = self#eval_int_exp exp in
	try
	  if (D.to_concrete_1 v) = 1 then true else false
	with
	    NotConcrete _ ->
	      let e = D.to_symbolic_1 v in
		if !opt_trace_conditions then 
		  Printf.printf "Symbolic branch condition %s\n"
		    (V.exp_to_string e);
		if !opt_concrete_path then
		  let b = (form_man#eval_expr e) <> 0L in
		    if !opt_trace_conditions then 
		      Printf.printf "Computed concrete value %b\n" b;
		    if !opt_solve_path_conditions then
		      (let b' = self#extend_pc_known e true b in
			assert(b = b'))
		    else
		      self#add_to_path_cond
			(if b then e else V.UnOp(V.NOT, e));
		    b
		else 
		  (dt#start_new_query_binary;
		   let b = self#extend_pc_random e true in
		     dt#count_query;
		     b)
		
    method eval_addr_exp exp =
      let c32 x = V.Constant(V.Int(V.REG_32, x)) in
      let v = self#eval_int_exp_simplify exp in
	try (D.to_concrete_32 v)
	with
	    NotConcrete _ ->
	      let e = (D.to_symbolic_32 v) in
	      let eip = self#get_word_var R_EIP in
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
		 "\"" ^ (self#read_cstr str_addr) ^ "\"" with
		     NotConcrete _ -> "<not concrete>" in
	       Printf.printf "At %08Lx, %s (%08Lx) is %s\n"
		 eip e_str str_addr str)
	!opt_string_tracepoints;
      infl_man#eip_hook eip;
      (match !opt_check_condition_at with
	 | Some (eip', expr) when eip' = eip ->
	     let b = self#eval_bool_exp expr in
	       Printf.printf "At 0x%08Lx, condition %s can be %b\n"
		 eip (V.exp_to_string expr) b
	 | _ -> ());
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
	   (List.rev path_cond));
      if !opt_solve_final_pc then
	assert(let (b,_) = self#query_with_path_cond path_cond V.exp_true true
	       in b);
      dt#try_again_p

    method print_tree chan = dt#print_tree chan

    method set_iter_seed i = dt#set_iter_seed i
      
    method reset () =
      fm#reset ();
      form_man#reset_mem_axioms;
      path_cond <- [];
      infl_man#reset;
      dt#reset

    method after_exploration =
      infl_man#after_exploration
  end
end

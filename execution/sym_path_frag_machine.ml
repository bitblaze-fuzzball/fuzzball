(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Exec_domain;;
open Exec_exceptions;;
open Exec_options;;
open Formula_manager;;
open Query_engine;;
open Stp_external_engine;;
open Exec_influence;;
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

  class sym_path_frag_machine = object(self)
    inherit FM.frag_machine as fm

    val mutable path_cond = []
    val dt = (new decision_tree)#init

    method add_to_path_cond cond =
      if path_cond = [] then
	path_cond <- !opt_extra_conditions;
      path_cond <- cond :: path_cond
      
    method restore_path_cond f =
      let saved_pc = path_cond in
      let ret = f () in
	path_cond <- saved_pc;
	ret

    val measured_values = Hashtbl.create 30
      
    method private take_measure key e =
      let old = (try Hashtbl.find measured_values key
		 with Not_found -> []) in
	Hashtbl.replace measured_values key ((path_cond, e) :: old)

    method take_measure_eip e =
      let eip = self#get_word_var R_EIP in
      let str = Printf.sprintf "eip 0x%08Lx" eip in
	self#take_measure str e

    method take_measure_expr key_expr e =
      let str = Printf.sprintf "expr %s" (V.exp_to_string key_expr) in
	self#take_measure str e

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

    method measure_influence_common free_decls assigns cond_e target_e =
      let assigns_sl =
	List.fold_left
	  (fun a (lvar, lvexp) -> a @ [V.Move(V.Temp(lvar), lvexp)])
	  [] assigns in
      let assign_vars = List.map (fun (v, exp) -> v) assigns in
      let prog = (free_decls @ assign_vars,
		  assigns_sl @ [V.Assert(cond_e)]) in
	(* V.pp_program (fun x -> Printf.printf "%s" x) prog; *)
      let () = ignore(prog) in
	0.0

    method measure_influence (target_expr : V.exp) =
      let (free_decls, assigns, cond_e, target_e, inputs_influencing) =
	form_man#collect_for_solving [] path_cond target_expr in
      let i =
	self#measure_influence_common free_decls assigns cond_e target_e in
	Printf.printf "Estimated influence on %s is %f\n" (V.exp_to_string target_expr) i;
	Printf.printf "Inputs contributing to this target expression: %s\n" 
	  (List.fold_left (fun a varble -> a ^ ", " ^ (V.var_to_string varble)) "" inputs_influencing);
	i

    method compute_multipath_influence loc =
      let fresh_cond_var =
	let counter = ref 0 in
	  fun () -> counter := !counter + 1;
	  V.newvar ("cond_" ^ (string_of_int !counter)) V.REG_1
      in
      (* A note about the List.rev here: in the nested if-then-else we
	 construct below, it's important to check more specific
	 conditions before more general ones, otherwise the more specific
	 ones will be shadowed. As we collect path conditions along a
	 single path, later PCs will be more specific than earlier
	 ones. So we're OK if we put them first, i.e. check the PCs in
	 the reverse order that they are collected. The list is reversed
	 once because it's collected by pushing elements onto a list, and
	 once in the fold_left below, so to make the total number of
	 reversals odd we reverse it one more time here. *)
      let measurements = List.rev (try Hashtbl.find measured_values loc
				   with Not_found -> []) in
      let vtype = match measurements with
	| (_, e) :: rest  -> Vine_typecheck.infer_type None e
	| _ -> V.REG_32 in
      let conjoined = List.map
	(fun (pc, e) -> (fresh_cond_var (), form_man#conjoin pc, e))
	measurements in
      let cond_assigns =
	List.map (fun (lhs, rhs, _) -> (lhs, rhs)) conjoined in
      let cond_vars = List.map (fun (v, _) -> v) cond_assigns in
      let cond_var_exps = List.map (fun v -> V.Lval(V.Temp(v))) cond_vars in
      let cond = form_man#disjoin cond_var_exps in
      let expr = List.fold_left
	(fun e (cond_v, _, v_e) ->
	   V.exp_ite (V.Lval(V.Temp(cond_v))) vtype v_e e)
	(V.Constant(V.Int(vtype, 0L))) conjoined in
      let (free_decls, t_assigns, cond_e, target_e, inputs_influencing) =
	form_man#collect_for_solving cond_assigns [cond] expr in
      let i =
	self#measure_influence_common free_decls t_assigns cond_e target_e in
	Printf.printf "Estimated multipath influence at %s is %f\n"
	  loc i;
	Printf.printf "Inputs contributing to this target expression: %s\n"
          (List.fold_left (fun a varble -> a ^ ", " ^ (V.var_to_string varble)) "" inputs_influencing);

    method compute_all_multipath_influence =
      Hashtbl.iter (fun eip _ -> self#compute_multipath_influence eip)
	measured_values

    val mutable periodic_influence_exprs = []

    method store_symbolic_byte_influence addr varname =
      let v = form_man#fresh_symbolic_8 varname in
	self#store_byte addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_8 v) :: periodic_influence_exprs

    method store_symbolic_short_influence addr varname =
      let v = form_man#fresh_symbolic_16 varname in
	self#store_short addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_16 v) :: periodic_influence_exprs

    method store_symbolic_word_influence addr varname =
      let v = form_man#fresh_symbolic_32 varname in
	self#store_word addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_32 v) :: periodic_influence_exprs

    method store_symbolic_long_influence addr varname =
      let v = form_man#fresh_symbolic_64 varname in
	self#store_long addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_64 v) :: periodic_influence_exprs

    method maybe_periodic_influence =
      match !opt_periodic_influence with
	| None -> ()
	| Some period when dt#get_depth >= !next_periodic_influence ->
	    next_periodic_influence := dt#get_depth + period;
	    let num_bounded = ref 0 in
	      List.iter
		(fun e -> let i = self#measure_influence e in
		   if i <= !opt_influence_bound then
		     incr num_bounded)
		periodic_influence_exprs;
	      if !num_bounded = List.length periodic_influence_exprs then
		raise ReachedInfluenceBound
	| Some _ -> ()

    method path_end_influence =
      List.iter
	(fun e -> self#take_measure_expr e e)
	periodic_influence_exprs

    method query_with_path_cond cond verbose =
      let get_time () = Unix.gettimeofday () in
      let pc = cond :: path_cond and
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
	    self#maybe_periodic_influence;
	    is_sat

    method follow_or_random =
	let currpath_str = dt#get_hist_str in
	let followplen = String.length !opt_follow_path and
	    currplen = String.length currpath_str in
	  if followplen > currplen then
	    let follow_prefix = String.sub !opt_follow_path 0 currplen in
	      if follow_prefix = currpath_str then
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
	self#query_with_path_cond cond' verbose
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
		
    val unique_measurements = Hashtbl.create 30

    method measure_point_influence name e = 
      let eip = self#get_word_var R_EIP in
      let loc = Printf.sprintf "%s %s:%08Lx:%Ld" name
		(dt#get_hist_str) eip loop_cnt in
	if Hashtbl.mem unique_measurements loc then
	  (if !opt_trace_sym_addrs then
	     Printf.printf
	       "Skipping redundant influence measurement at %s\n" loc)
	else
	  (Hashtbl.replace unique_measurements loc ();
	   self#take_measure_eip e;
	   if !opt_trace_sym_addrs then
	     Printf.printf "Took influence measurement at %s\n" loc;
	   if not !opt_multipath_influence_only then
	     ignore(self#measure_influence e))

    method maybe_measure_influence_deref e =
      let eip = self#get_word_var R_EIP in
	match !opt_measure_deref_influence_at with
	  | Some addr when addr = eip ->
	      self#take_measure_eip e;
	      if !opt_trace_sym_addrs then
		Printf.printf "Took influence measurement at eip %08Lx\n" eip;
	      if not !opt_multipath_influence_only then
		ignore(self#measure_influence e);
	      if !opt_stop_at_measurement then
		raise ReachedMeasurePoint
	  | _ -> if !opt_measure_influence_derefs then
	      self#measure_point_influence "deref" e

    method measure_influence_rep =
      let count = self#get_int_var (Hashtbl.find reg_to_var R_ECX) in
	try ignore(D.to_concrete_32 count)
	with NotConcrete _ ->	    
	  self#measure_point_influence "reploop" (D.to_symbolic_32 count)

    method measure_influence_expr expr =
      let (v, ty) = self#eval_int_exp_ty expr in
	try (match ty with
	       | V.REG_1  -> ignore(D.to_concrete_1 v)
	       | V.REG_8  -> ignore(D.to_concrete_8 v)
	       | V.REG_16 -> ignore(D.to_concrete_16 v)
	       | V.REG_32 -> ignore(D.to_concrete_32 v)
	       | V.REG_64 -> ignore(D.to_concrete_64 v)
	       | _ -> failwith "Bad type in measure_influence_expr")
	with NotConcrete _ ->	    
	  self#measure_point_influence "expr" (D.to_symbolic_64 v)

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
		self#maybe_measure_influence_deref e;
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

    val mutable qualified = true

    method disqualify_path = qualified <- false

    val mutable saved_details_flags = (false, false, false, false, false)

    method eip_hook eip = 
      fm#eip_hook eip;
      List.iter
	(fun (f_eip, _) -> 
	   if f_eip = eip then
	     (saved_details_flags <- 
		(!opt_trace_insns, !opt_trace_loads, !opt_trace_stores,
		 !opt_trace_temps, !opt_trace_syscalls);	      
	      opt_trace_insns := true;
	      opt_trace_loads := true;
	      opt_trace_stores := true;
	      opt_trace_temps := true;
	      opt_trace_syscalls := true))
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
      if List.mem eip !opt_disqualify_addrs then
	(self#disqualify_path;
	 raise DisqualifiedPath);
      (if !opt_measure_influence_reploops then
	 let prefix = self#load_byte_conc eip in
	   match prefix with
	     | 0xf2 | 0xf3 ->
		 self#measure_influence_rep
	     | _ -> ());
      (match !opt_measure_expr_influence_at with
	 | Some (eip', expr) when eip' = eip ->
	     self#measure_influence_expr expr;
	      if !opt_stop_at_measurement then
		raise ReachedMeasurePoint
	 | _ -> ());
      (match !opt_check_condition_at with
	 | Some (eip', expr) when eip' = eip ->
	     let b = self#eval_bool_exp expr in
	       Printf.printf "At 0x%08Lx, condition %s can be %b\n"
		 eip (V.exp_to_string expr) b
	 | _ -> ());
      List.iter
	(fun (_, t_eip) -> 
	   if t_eip = eip then
	     let (i, l, s, t, sc) = saved_details_flags in
	      opt_trace_insns := i;
	      opt_trace_loads := l;
	      opt_trace_stores := s;
	      opt_trace_temps := t;
	      opt_trace_syscalls := sc)
	!opt_trace_detailed_ranges
	  
    method finish_path =
      dt#mark_all_seen;
      if qualified then
	self#path_end_influence;
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
	assert(self#query_with_path_cond V.exp_true true);
      dt#try_again_p

    method print_tree chan = dt#print_tree chan

    method set_iter_seed i = dt#set_iter_seed i
      
    method reset () =
      fm#reset ();
      form_man#reset_mem_axioms;
      path_cond <- [];
      qualified <- true;
      dt#reset
  end
end

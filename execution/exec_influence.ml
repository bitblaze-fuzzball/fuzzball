(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine

open Exec_options
open Exec_exceptions
open Query_engine
open Options_solver
open Stpvc_engine
open Formula_manager
open Fragment_machine
open Sym_path_frag_machine

let collect_let_vars e =
  let rec loop e = match e with
    | V.BinOp(_, e1, e2) -> (loop e1) @ (loop e2)
    | V.FBinOp(_, _, e1, e2) -> (loop e1) @ (loop e2)
    | V.UnOp(_, e1) -> loop e1
    | V.FUnOp(_, _, e1) -> loop e1
    | V.Constant(_) -> []
    | V.Lval(_) -> []
    | V.Name(_) -> []
    | V.Cast(_, _, e1) -> loop e1
    | V.FCast(_, _, _, e1) -> loop e1
    | V.Unknown(_) -> []
    | V.Let(V.Mem(_, _, _), _, _)
	-> failwith "Let-mem unsupported in collect_let_vars"
    | V.Let(V.Temp(var), e1, e2) -> var :: (loop e1) @ (loop e2)
    | V.Ite(ce, te, fe) -> (loop ce) @ (loop te) @ (loop fe)
  in
    loop e

let log2_of_int i =
  (log (float_of_int i)) /. (log 2.0)

let log2_of_uint64 i64 =
  let f = Vine_util.int64_u_to_float i64 in
    (log f) /. (log 2.0)

let sx ty v =
  let bits = 64 - V.bits_of_width ty in
    Int64.shift_right (Int64.shift_left v bits) bits

let zx ty v =
  let bits = 64 - V.bits_of_width ty in
    Int64.shift_right_logical (Int64.shift_left v bits) bits

let diff_to_range_i diff =
  if diff = Int64.max_int then
    64.0
  else
    log2_of_uint64 (Int64.succ diff)

let random_xor_constraint ty target_e num_terms =
  (* (out & (1 << k)) != 0, k random in [0, bits(out)-1] *)
  let random_term () = 
    let k = Random.int (V.bits_of_width ty) in
    let mask = V.Constant(V.Int(ty, (Int64.shift_left 1L k))) in
      V.BinOp(V.NEQ,
	      V.BinOp(V.BITAND, target_e, mask),
	      V.Constant(V.Int(ty, 0L)))
  in
  let rec random_terms k = match k with
    | 1 -> random_term ()
    | _ -> V.BinOp(V.XOR, random_term (),
		   random_terms (k - 1))
  in
  let parity = V.Constant(V.Int(V.REG_1, Random.int64 2L)) and
      terms = random_terms num_terms in
    V.BinOp(V.EQ, terms, parity)

let random_xor_constraints ty target_e num_terms k = 
  let rec loop k =
    match k with
      | 1 -> random_xor_constraint ty target_e num_terms
      | _ -> V.BinOp(V.BITAND, (random_xor_constraint ty target_e num_terms),
		     loop (k - 1))
  in
    loop k

let opt_influence_details = ref false

module InfluenceManagerFunctor =
  functor (D : Exec_domain.DOMAIN) ->
struct
  class influence_manager
    (fm : SymPathFragMachineFunctor(D).sym_path_frag_machine) =
  object(self)
    val form_man = fm#get_form_man

    (* This class is currently constructed before the command line
       options that relate to the choice of solver are parsed. So we want
       to delay constructing the query engine until it's needed. Using a
       dummy object will satisfy OCaml's type system so that we don't
       need a cumbersome option type. *)
    val mutable qe = new Query_engine.dummy_query_engine
    val mutable qe_ready = false

    val measured_values = Hashtbl.create 30
      
    method private take_measure key e =
      let old = (try Hashtbl.find measured_values key
		 with Not_found -> []) in
	Hashtbl.replace measured_values key ((fm#get_path_cond, e) :: old)

    method take_measure_eip e =
      let eip = fm#get_eip in
      let str = Printf.sprintf "eip 0x%08Lx" eip in
	self#take_measure str e

    method take_measure_expr key_expr e =
      let str = Printf.sprintf "expr %s" (V.exp_to_string key_expr) in
	self#take_measure str e

    method private check_sat target_eq cond =
      qe#push;
      qe#start_query;
      let (result, ce) = qe#query (V.BinOp(V.BITAND, target_eq, cond)) in
	match result with
	  | Some false ->
	      if !opt_influence_details then
		(Printf.printf "Yup, condition is satisfiable, by\n";
		 print_ce ce);
	      qe#after_query !opt_influence_details;
	      qe#pop;
	      let v = ref 0L in
		ce_iter ce
		  (fun s i64 -> if s = "influence_target" then
		     v := i64);
		(* It's intentional here that v will be set to zero if
		   the variable doesn't appear in the counterexample, since
		   some of the solvers do that. *)
		if !opt_influence_details then
		  Printf.printf "Satisfying value is 0x%Lx\n" !v;
		Some !v
	  | Some true ->
	      if !opt_influence_details then
		Printf.printf "No, condition is unsat\n";
	      qe#after_query false;
	      qe#pop;
	      None
	  | None ->
	      qe#after_query true;
	      qe#pop;
	      raise SolverFailure

    method private check_valid target_eq cond =
      qe#push;
      qe#start_query;
      let (result, ce) = qe#query
	(V.BinOp(V.BITAND, target_eq, V.UnOp(V.NOT, cond)))
      in
	match result with
          | Some false ->
              if !opt_influence_details then
		(Printf.printf "No, condition is invalid; counterexample:\n";
                 print_ce ce);
              qe#after_query !opt_influence_details;
              qe#pop;
              false
          | Some true ->
              if !opt_influence_details then
		Printf.printf "Yes, condition is valid\n";
              qe#after_query false;
              qe#pop;
              true
          | None ->
              qe#after_query true;
              qe#pop;
              raise SolverFailure

    method private find_bound target_eq target_e is_signed is_max =
      let ty = Vine_typecheck.infer_type None target_e in
      let le_op = if is_signed then V.SLE else V.LE in
      let midpoint a b =
	let half_rounded x =
	  let half = Int64.shift_right_logical x 1 in
	    if Int64.logand x 1L = 0L then
	      half
	    else
	      Int64.add half (if is_max then 0L else 1L)
	in
	  if is_signed then
	    let (a', b') = ((sx ty a), (sx ty b)) in
	    let sz = Int64.sub b' a' in
	    let hf_sz = half_rounded sz in
	    let mid = Int64.add a' hf_sz in
	      assert(a' <= mid);
	      assert(mid <= b');
	      zx ty mid
	  else
	    let sz = Int64.sub b a in
	    let hf_sz = half_rounded sz in
	    let mid = Int64.add a hf_sz in
	      assert(Vine_util.int64_ucompare a mid <= 0);
	      assert(Vine_util.int64_ucompare mid b <= 0);
	      mid
      in
      let rec loop min max =
	if !opt_influence_details then
	  Printf.printf "Binary search in [0x%Lx, 0x%Lx]\n"
	    min max;
	assert(is_signed || Vine_util.int64_ucompare min max <= 0);
	if min = max then
	  min
	else
	  let mid = midpoint min max in
	  let mid_e = V.Constant(V.Int(ty, mid)) in
	    if is_max then
	      let cond_e = V.BinOp(le_op, target_e, mid_e) in
	      let in_bounds = self#check_valid target_eq cond_e in
		if in_bounds then
		  loop min mid
		else
		  loop (zx ty (Int64.succ mid)) max
	    else
	      let cond_e = V.BinOp(le_op, mid_e, target_e) in
	      let in_bounds = self#check_valid target_eq cond_e in
		if in_bounds then
		  loop mid max
		else
		  loop min (zx ty (Int64.pred mid))
      in
      let wd = V.bits_of_width ty in
      let (min_limit, max_limit) =
	if is_signed then
	  (sx ty (Int64.shift_left 1L (wd - 1)),
	   Int64.shift_right_logical (-1L) (64-wd+1))
	else
	  (0L, Int64.shift_right_logical (-1L) (64-wd))
      in
      let limit = loop min_limit max_limit in
	limit

    method private pointwise_enum target_eq target_e conds max_vals =
      let ty = Vine_typecheck.infer_type None target_e in
      let neq_exp v =
	V.BinOp(V.NEQ, target_e, V.Constant(V.Int(ty, v)))
      in
      let rec loop vals n =
	if n = 0 then
	  vals
	else
	  let not_old = conjoin (conds @ (List.map neq_exp vals)) in
	    match self#check_sat target_eq not_old with
	      | None ->
		  vals
	      | Some v ->
		  loop (v :: vals) (n - 1)
      in
	loop [] max_vals

    method private random_sample target_eq target_e num_samples =
      let ty = Vine_typecheck.infer_type None target_e in
      let rand_val () =
	match ty with
	  | V.REG_1 -> if Random.bool () then 1L else 0L
	  | V.REG_8 -> Int64.of_int (Random.int 256)
	  | V.REG_16 -> Int64.of_int (Random.int 65536)
	  | V.REG_32 -> Int64.of_int32 (Random.int32 Int32.max_int)
	  | V.REG_64 -> Random.int64 Int64.max_int
	  | _ -> failwith "Unexpected type in rand_val"
      in
      let num_hits = ref 0 in
	for i = 1 to num_samples do
	  let v = rand_val () in
	  let cond = V.BinOp(V.EQ, target_e, V.Constant(V.Int(ty, v))) in
	    match self#check_sat target_eq cond with
	      | None -> ()
	      | Some v' ->
		  assert(v = v');
		  num_hits := !num_hits + 1
	done;
	!num_hits

    (* This is basically the #SAT algorithm used in the PLAS'09
       paper. *)
    method private xor_walk_simple target_eq target_e count =
      let ty = Vine_typecheck.infer_type None target_e in
      let start = float ((V.bits_of_width ty) / 2) in
      let num_terms = (V.bits_of_width ty) / 4 in
    
      let rec loop guess i = if i >= count then
	guess
      else
	let experiment hypo =
	  let cond = random_xor_constraints ty target_e num_terms hypo in
	    assert(hypo > 0);
	    match self#check_sat target_eq cond with
	      | None    -> false
              | Some(_) -> true
	in
	let delta = 2.0 /. (1.0 +. (float i) /. 5.0) and
	    fexp = experiment (int_of_float (floor (guess -. 0.5))) and
	    cexp = experiment (int_of_float (ceil  (guess -. 0.5))) in
	let new_guess = match (fexp, cexp) with
	  | (true,  true ) -> guess +. delta
	  | (false, false) -> guess -. delta
	  | _ -> guess
	in
	  if !opt_influence_details then
	    Printf.printf "XOR iteration %d estimate is %f +- %f\n"
	      i new_guess delta;
	  loop new_guess (i + 1)
      in
	loop start 0

    (* The general idea of using enumeration to get more precision,
       embodied in this function, seems like a good one. This particular
       approach also sometimes does well when the other constraints are
       well-structured. But after thiking about it more I believe generating
       only one set of XOR constraints isn't enough to get a precise and
       accurate result, because the factor by which any one set of
       constraints shrink the solution space has high variance. -SMcC *)
    method private xor_then_enum target_eq target_e count =
      let ty = Vine_typecheck.infer_type None target_e in
      let num_terms = (V.bits_of_width ty) / 4 in
      let infty = match ty with
	| V.REG_1 -> 3
	| V.REG_8 -> 257
	| V.REG_16 -> 65537
	| _ -> 0x3ffffffe
      in
      let rec add_xors_loop k =
	if !opt_influence_details then
	  Printf.printf "Trying with %d constraints\n" k;
	let cond = random_xor_constraints ty target_e num_terms k in
	  match self#check_sat target_eq cond with
	    | None    -> k
            | Some(_) -> add_xors_loop (k + 1)
      in
      let rec rm_xors_loop k =
	let cond = random_xor_constraints ty target_e num_terms k in
	let vals = self#pointwise_enum target_eq target_e [cond] infty in
	let n = List.length vals in
	  if !opt_influence_details then
	    Printf.printf "With %d constraints got %d solutions\n" k n;
	  if n < count then
	    rm_xors_loop (k - 1)
	  else
	    (float k) +. log2_of_int n
      in
      let num_xors = add_xors_loop 1 in
	Printf.printf "Reached unsat after %d constraints\n" num_xors;
	rm_xors_loop num_xors

    method private influence_strategies target_eq target_e ty =
      (let unsign_min = self#find_bound target_eq target_e false false and
	   unsign_max = self#find_bound target_eq target_e false true and
	   signed_min = self#find_bound target_eq target_e true false and
	   signed_max = self#find_bound target_eq target_e true true
       in
       let unsign_range_i = (diff_to_range_i
			       (Int64.sub unsign_max unsign_min)) and
	   signed_range_i = (diff_to_range_i
			       (Int64.sub (sx ty signed_max)
				  (sx ty signed_min)))
       in
	 Printf.printf "Upper bound from unsigned range [0x%Lx, 0x%Lx]: %f\n"
	   unsign_min unsign_max unsign_range_i;
	 Printf.printf "Upper bound from signed range [0x%Lx, 0x%Lx]: %f\n"
	   signed_min signed_max signed_range_i);
      let points_bits = 6 in
      let points_max = (1 lsl points_bits) + 1 in
      let points = self#pointwise_enum target_eq target_e [] points_max in
      let num_points = List.length points in
      let points_i = log2_of_int num_points in
	if num_points < points_max then
	  (Printf.printf "Exact influence from %d points is %f\n"
	     num_points points_i;
	   points_i)
	else
	  (Printf.printf "Lower bound from %d points is %f\n"
	     num_points points_i;
	   let num_samples = 20 in
	   let num_hits = self#random_sample target_eq target_e num_samples in
	     Printf.printf "Random sampling: %d hits out of %d samples\n"
	       num_hits num_samples;
	     if num_hits > 1 then
	       let frac =
		 (float_of_int num_hits) /. (float_of_int num_samples)
	       in
	       let log_frac = (log frac) /. (log 2.0) and
		   max_bits = float_of_int (V.bits_of_width ty) in
	       let sampled_i = log_frac +. max_bits in
		 Printf.printf "Samples influence is %f\n" sampled_i;
		 sampled_i
	     else
	       (* Here's where we need XOR-streamlining *)
	       self#xor_walk_simple target_eq target_e 50
	       (* self#xor_then_enum target_eq target_e 50 *)
	  )

    method measure_influence_common decls assigns cond_e target_e =
      Printf.printf "In measure_influence_common\n";
      if not qe_ready then
	(qe <- construct_solver "-influence";
	 qe_ready <- true);
      let ty = Vine_typecheck.infer_type None target_e in
      let temp_vars = List.map (fun (var, e) -> var) assigns in
      let let_vars = (collect_let_vars target_e) @
	List.concat (List.map (fun (var, e) -> collect_let_vars e) assigns) in
      let target_var = V.newvar "influence_target" ty in
      let free_decls =
	target_var :: Vine_util.list_difference
	  (Vine_util.list_difference decls temp_vars)
	  let_vars
      in
      let target_eq = V.BinOp(V.EQ, V.Lval(V.Temp(target_var)), target_e) in
      let target_e' = V.Lval(V.Temp(target_var)) in
	Printf.printf "Free variables are";
	List.iter (fun v -> Printf.printf " %s" (V.var_to_string v))
	  free_decls;
	Printf.printf "\n";
	List.iter qe#add_free_var free_decls;
	Printf.printf "Temp assignments are:\n";
	List.iter (fun (v, exp) ->
		     Printf.printf " %s = %s\n"
		       (V.var_to_string v) (V.exp_to_string exp))
	  assigns;
	List.iter (fun (v, exp) ->
		     qe#add_temp_var v;
		     qe#assert_eq v exp)
	  assigns;
	Printf.printf "Conditional expr is %s\n" (V.exp_to_string cond_e);
	qe#add_condition cond_e;
	Printf.printf "Target expr is %s\n" (V.exp_to_string target_e);
	assert(self#check_sat target_eq V.exp_true <> None);
	let i = self#influence_strategies target_eq target_e' ty in
	  qe#reset;
	  i

    method measure_influence (target_expr : V.exp) =
      let (decls, assigns, cond_e, target_e, inputs_influencing) =
	form_man#collect_for_solving [] fm#get_path_cond target_expr in
      let i =
	self#measure_influence_common decls assigns cond_e target_e in
	Printf.printf "Estimated influence on %s is %f\n"
	  (V.exp_to_string target_expr) i;
	Printf.printf "Inputs contributing to this target expression: %s\n" 
	  (List.fold_left
	     (fun a varble ->
		a ^ ", " ^ (V.var_to_string varble)) "" inputs_influencing);
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
	(fun (pc, e) -> (fresh_cond_var (), conjoin pc, e))
	measurements in
      let cond_assigns =
	List.map (fun (lhs, rhs, _) -> (lhs, rhs)) conjoined in
      let cond_vars = List.map (fun (v, _) -> v) cond_assigns in
      let cond_var_exps = List.map (fun v -> V.Lval(V.Temp(v))) cond_vars in
      let cond = disjoin cond_var_exps in
      let expr = List.fold_left
	(fun e (cond_v, _, v_e) ->
	   V.exp_ite (V.Lval(V.Temp(cond_v))) vtype v_e e)
	(V.Constant(V.Int(vtype, 0L))) conjoined in
      let (free_decls, t_assigns, cond_e, target_e, inputs_influencing) =
	form_man#collect_for_solving cond_assigns [cond] expr in
	if measurements = [] then
	  Printf.printf "No influence measurements at %s\n" loc
	else
	  let i = (self#measure_influence_common free_decls t_assigns
		     cond_e target_e)
	  in
	    Printf.printf "Estimated multipath influence at %s is %f\n"
	      loc i;
	    Printf.printf "Inputs contributing to this target expression: %s\n"
              (List.fold_left
		 (fun a varble -> a ^ ", " ^ (V.var_to_string varble))
		 "" inputs_influencing);

    method compute_all_multipath_influence =
      Hashtbl.iter (fun eip _ -> self#compute_multipath_influence eip)
	measured_values

    val mutable periodic_influence_exprs = []

    method store_symbolic_byte_influence addr varname =
      let v = form_man#fresh_symbolic_8 varname in
	fm#store_byte addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_8 v) :: periodic_influence_exprs

    method store_symbolic_short_influence addr varname =
      let v = form_man#fresh_symbolic_16 varname in
	fm#store_short addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_16 v) :: periodic_influence_exprs

    method store_symbolic_word_influence addr varname =
      let v = form_man#fresh_symbolic_32 varname in
	fm#store_word addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_32 v) :: periodic_influence_exprs

    method store_symbolic_long_influence addr varname =
      let v = form_man#fresh_symbolic_64 varname in
	fm#store_long addr v;
	periodic_influence_exprs <-
	  (D.to_symbolic_64 v) :: periodic_influence_exprs

    method maybe_periodic_influence =
      match !opt_periodic_influence with
	| None -> ()
	| Some period when fm#get_depth >= !next_periodic_influence ->
	    next_periodic_influence := fm#get_depth + period;
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

    val unique_measurements = Hashtbl.create 30

    method measure_point_influence name e = 
      let eip = fm#get_eip in
      let loc = Printf.sprintf "%s %s:%08Lx:%Ld" name
		(fm#get_hist_str) eip fm#get_loop_cnt in
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
      let eip = fm#get_eip in
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
      assert(!opt_arch = X86);
      let count = fm#get_word_var_d R_ECX in
	try ignore(D.to_concrete_32 count)
	with NotConcrete _ ->	    
	  self#measure_point_influence "reploop" (D.to_symbolic_32 count)

    method measure_influence_expr expr =
      let (v, ty) = fm#eval_int_exp_ty expr in
      let e = match ty with
	| V.REG_1  -> D.to_symbolic_1 v
	| V.REG_8  -> D.to_symbolic_8 v
	| V.REG_16 -> D.to_symbolic_16 v
	| V.REG_32 -> D.to_symbolic_32 v
	| V.REG_64 -> D.to_symbolic_64 v
	| _ -> failwith "Bad type in measure_influence_expr"
      in
	self#measure_point_influence "expr" e

    val mutable qualified = true

    method disqualify_path = qualified <- false

    method eip_hook eip =
      if List.mem eip !opt_disqualify_addrs then
	(self#disqualify_path;
	 fm#unfinish_fuzz "Disqualified path";
	 raise DisqualifiedPath);
      (if !opt_measure_influence_reploops then
	 let prefix = fm#load_byte_conc eip in
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

    method finish_path =
      if qualified then
	self#path_end_influence

    method reset =
      qualified <- true

    method after_exploration =
      match (!opt_measure_deref_influence_at,
	     !opt_measure_expr_influence_at) with
	| (Some eip, _) -> self#compute_multipath_influence 
	    (Printf.sprintf "eip 0x%08Lx" eip)
	| (_, Some (eip, expr)) -> self#compute_multipath_influence 
	    (Printf.sprintf "eip 0x%08Lx" eip)
	| _ -> self#compute_all_multipath_influence
  end
end

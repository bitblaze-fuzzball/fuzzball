(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_options
open Exec_exceptions
open Query_engine
open Stpvc_engine
open Formula_manager
open Fragment_machine
open Sym_path_frag_machine

type argparams_t = {
  mutable _tmp_name : string;
  mutable _get_val_bounds : bool;
  mutable _low_bound_to : float;
  mutable _sample_pts : int;
  mutable _xor_count : int;
  mutable _xor_seed : int;
  mutable _stp_file : string;
  mutable _ps_file : string;
} ;;

let get_influence (prog: Vine.program) (args: argparams_t) (q: V.exp) =
  0.0
;;

module InfluenceManagerFunctor =
  functor (D : Exec_domain.DOMAIN) ->
struct
  class influence_manager
    (fm : SymPathFragMachineFunctor(D).sym_path_frag_machine) =
  object(self)
    val form_man = fm#get_form_man

    val measured_values = Hashtbl.create 30
      
    method private take_measure key e =
      let old = (try Hashtbl.find measured_values key
		 with Not_found -> []) in
	Hashtbl.replace measured_values key ((fm#get_path_cond, e) :: old)

    method take_measure_eip e =
      let eip = fm#get_word_var R_EIP in
      let str = Printf.sprintf "eip 0x%08Lx" eip in
	self#take_measure str e

    method take_measure_expr key_expr e =
      let str = Printf.sprintf "expr %s" (V.exp_to_string key_expr) in
	self#take_measure str e

    method measure_influence_common free_decls assigns cond_e target_e =
      let assigns_sl =
	List.fold_left
	  (fun a (lvar, lvexp) -> a @ [V.Move(V.Temp(lvar), lvexp)])
	  [] assigns in
      let assign_vars = List.map (fun (v, exp) -> v) assigns in
      let prog = (free_decls @ assign_vars,
		  assigns_sl @ [V.Assert(cond_e)]) in
	(* V.pp_program (fun x -> Printf.printf "%s" x) prog; *)
	let args = {
	  _tmp_name = "";
	  _get_val_bounds = true;
	  _low_bound_to = 6.0;
	  _sample_pts = 20;
	  _xor_count = 0;
	  _xor_seed = 0;
	  _stp_file = (if !opt_save_solver_files then "fuzz-valset-stpvc.stp"
		       else "");
	  _ps_file = "";
	} in
	  get_influence prog args target_e

    method measure_influence (target_expr : V.exp) =
      let (free_decls, assigns, cond_e, target_e, inputs_influencing) =
	form_man#collect_for_solving [] fm#get_path_cond target_expr in
      let i =
	self#measure_influence_common free_decls assigns cond_e target_e in
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
      let eip = fm#get_word_var R_EIP in
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
      let eip = fm#get_word_var R_EIP in
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
      let count = fm#get_word_var_d R_ECX in
	try ignore(D.to_concrete_32 count)
	with NotConcrete _ ->	    
	  self#measure_point_influence "reploop" (D.to_symbolic_32 count)

    method measure_influence_expr expr =
      let (v, ty) = fm#eval_int_exp_ty expr in
	try (match ty with
	       | V.REG_1  -> ignore(D.to_concrete_1 v)
	       | V.REG_8  -> ignore(D.to_concrete_8 v)
	       | V.REG_16 -> ignore(D.to_concrete_16 v)
	       | V.REG_32 -> ignore(D.to_concrete_32 v)
	       | V.REG_64 -> ignore(D.to_concrete_64 v)
	       | _ -> failwith "Bad type in measure_influence_expr")
	with NotConcrete _ ->	    
	  self#measure_point_influence "expr" (D.to_symbolic_64 v)

    val mutable qualified = true

    method disqualify_path = qualified <- false

    method eip_hook eip =
      if List.mem eip !opt_disqualify_addrs then
	(self#disqualify_path;
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

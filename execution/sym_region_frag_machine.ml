(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_domain;;
open Exec_utils;;
open Exec_exceptions;;
open Exec_options;;
open Frag_simplify;;
open Formula_manager;;
open Query_engine;;
open Granular_memory;;
open Fragment_machine;;
open Decision_tree;;
open Sym_path_frag_machine;;

module SymRegionFragMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module FormMan = FormulaManagerFunctor(D)
  module GM = GranularMemoryFunctor(D)
  module SPFM = SymPathFragMachineFunctor(D)

  let is_high_mask ty v =
    let is_power_of_2_or_zero x =
      Int64.logand x (Int64.pred x) = 0L
    in
    let mask64 =
      match ty with
	| V.REG_1 -> fix_s1 v
	| V.REG_8 -> fix_s8 v
	| V.REG_16 -> fix_s16 v
	| V.REG_32 -> fix_s32 v
	| V.REG_64 -> v
	| _ -> failwith "Bad type in is_high_mask"
    in
      is_power_of_2_or_zero (Int64.succ (Int64.lognot mask64))

  let floor_log2 i =
    let rec loop = function
      | 0L -> -1
      | 1L -> 0
      | 2L|3L -> 1
      | 4L|5L|6L|7L -> 2
      | i when i < 16L -> 2 + loop(Int64.shift_right i 2)
      | i when i < 256L -> 4 + loop(Int64.shift_right i 4)
      | i when i < 65536L -> 8 + loop(Int64.shift_right i 8)
      | i when i < 0x100000000L -> 16 + loop(Int64.shift_right i 16)
      | _ -> 32 + loop(Int64.shift_right i 32)
    in
      loop i

  let narrow_bitwidth form_man e =
    let rec loop e = 
      match e with
	| V.Constant(V.Int(ty, v)) -> 1 + floor_log2 v
	| V.BinOp(V.BITAND, e1, e2) -> min (loop e1) (loop e2)
	| V.BinOp(V.BITOR, e1, e2) -> max (loop e1) (loop e2)
	| V.BinOp(V.PLUS, e1, e2) -> 1 + (max (loop e1) (loop e2))
	| V.Cast(_, V.REG_32, e1) -> min 32 (loop e1)
	| V.Cast(_, V.REG_16, e1) -> min 16 (loop e1)
	| V.Cast(_, V.REG_8, e1)  -> min 8  (loop e1)
	| V.Cast(_, V.REG_1, e1)  -> min 1  (loop e1)
	| V.Lval(V.Temp((_, _,  V.REG_1) as var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> min 1  (loop e'))  1 (fun v -> ())
	| V.Lval(V.Temp((_, _,  V.REG_8) as var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> min 8  (loop e'))  8 (fun v -> ())
	| V.Lval(V.Temp((_, _, V.REG_16) as var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> min 16 (loop e')) 16 (fun v -> ())
	| V.Lval(V.Temp((_, _, V.REG_32) as var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> min 32 (loop e')) 32 (fun v -> ())
	| V.Lval(V.Temp((_, _, V.REG_64) as var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> min 64 (loop e')) 64 (fun v -> ())
	| V.Lval(V.Mem(_, _, V.REG_8))  ->  8
	| V.Lval(V.Mem(_, _, V.REG_16)) -> 16
	| V.Lval(V.Mem(_, _, V.REG_32)) -> 32
	| V.BinOp((V.EQ|V.NEQ|V.LT|V.LE|V.SLT|V.SLE), _, _) -> 1
	| V.BinOp(V.LSHIFT, e1, V.Constant(V.Int(_, v))) ->
	    (loop e1) + (Int64.to_int v)
	| _ -> 64
    in
      loop e

  let map_n fn n =
    let l = ref [] in
      for i = n downto 0 do
	l := (fn i) :: !l
      done;
      !l

  let rec lookup_tree form_man e bits ty expr_list = 
    let rec nth_tail n l = match (n, l) with
      | (0, l) -> l
      | (_, []) -> failwith "List too short in nth_tail"
      | (n, l) -> nth_tail (n-1) (List.tl l)
    in
      assert((List.length expr_list) >= (1 lsl bits));
      if bits = 0 then
	List.hd expr_list
      else
	let shift_amt = Int64.of_int (bits - 1) in
	let cond_e = V.Cast(V.CAST_LOW, V.REG_1,
			    V.BinOp(V.RSHIFT, e, 
				    V.Constant(V.Int(V.REG_8, shift_amt))))
	in
	let half_two = nth_tail (1 lsl (bits - 1)) expr_list in
	  form_man#make_ite (D.from_symbolic cond_e) ty
	    (lookup_tree form_man e (bits - 1) ty half_two)
	    (lookup_tree form_man e (bits - 1) ty expr_list)

  let split_terms e form_man =
    let rec loop e =
      match e with
	| V.BinOp(V.PLUS, e1, e2) -> (loop e1) @ (loop e2)
(*	| V.BinOp(V.BITAND, e, V.Constant(V.Int(ty, v)))
	    when is_high_mask ty v ->
	    (* x & 0xfffffff0 = x - (x & 0xf), etc. *)
	    (loop e) @
	      (loop
		 (V.UnOp(V.NEG,
			 V.BinOp(V.BITAND, e,
				 V.UnOp(V.NOT, V.Constant(V.Int(ty, v)))))))
	| V.BinOp(V.BITOR, e1, e2) ->
	    let (w1, w2) = (narrow_bitwidth e1), (narrow_bitwidth e2) in
(* 	      Printf.printf "In %s (OR) %s, widths are %d and %d\n" *)
(* 		(V.exp_to_string e1) (V.exp_to_string e2) w1 w2; *)
	      if min w1 w2 <= 8 then
		(* x | y = x - (x & m) + ((x & m) | y)
		   where m is a bitmask >= y. *)
		let (e_x, e_y, w) = 
		  if w1 < w2 then
		    (e2, e1, w1)
		  else
		    (e1, e2, w2)
		in
		  assert(w >= 0); (* x & 0 should have been optimized away *)
		  let mask = Int64.pred (Int64.shift_left 1L w) in
		  let ty_y = Vine_typecheck.infer_type None e_y in
		  let masked = V.BinOp(V.BITAND, e_y,
				       V.Constant(V.Int(ty_y, mask))) in
		    (loop e_x) @ 
		      [V.UnOp(V.NEG, masked);
		       V.BinOp(V.BITOR, masked, e_y)]
	      else
		[e] *)
	| V.Lval(V.Temp(var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> loop e') [e] (fun v -> ())
	| e -> [e]
    in
      loop e

  type term_kind = | ConstantBase of int64
		   | ConstantOffset of int64
		   | ExprOffset of V.exp
		   | Symbol of V.exp

  let classify_term form_man e =
    match e with
      | V.Constant(V.Int(V.REG_32, off))
	  when (Int64.abs (fix_s32 off)) < 0x4000L
	    -> ConstantOffset(off)
      | V.Constant(V.Int(V.REG_32, off)) when (fix_s32 off) > 0x8000000L
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0xc0000000L && off < 0xe1000000L (* Linux kernel *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x80800000L && off < 0x88000000L (* ReactOS kernel *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x82800000L && off < 0x94000000L (* Windows 7 kernel *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0xf88f0000L && off < 0xf88fffffL
	    (* ReactOS kernel stack *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x9b200000L && off < 0x9b300000L
	    (* Windows 7 kernel stack *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0xff400000L && off < 0xffc00000L
	    (* Windows 7 kernel something *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x7ff00000L && off < 0x80000000L
	    (* Windows 7 shared user/kernel something *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x80000000L && off < 0xffffffffL
	    (* XXX let Windows 7 wander over the whole top half *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	    (* XXX -random-memory can produce any value at all *)
	  -> ConstantBase(off)
      | V.UnOp(V.NEG, _) -> ExprOffset(e)
      | V.BinOp(V.LSHIFT, _, V.Constant(V.Int(V.REG_8, (1L|2L|3L|4L|5L))))
	  -> ExprOffset(e)
      | V.BinOp(V.TIMES, _, _)
	  -> ExprOffset(e)
      | e when (narrow_bitwidth form_man e) < 23
	  -> ExprOffset(e)
      | V.BinOp(V.ARSHIFT, _, _)
	  -> ExprOffset(e)
      | V.BinOp(V.RSHIFT, _, _)
	  -> ExprOffset(e)
      | V.BinOp(V.LSHIFT, _, _)
	  -> ExprOffset(e)
(*       | V.BinOp(V.BITAND, _, _) *)
(*       | V.BinOp(V.BITOR, _, _) (* XXX happens in Windows 7, don't know why *) *)
(* 	  -> ExprOffset(e) *)
      | V.Lval(_) -> Symbol(e)
      | _ -> if (!opt_fail_offset_heuristic) then (
	  failwith ("Strange term "^(V.exp_to_string e)^" in address")
	) else ExprOffset(e)
	  
  let classify_terms e form_man =
    let l = List.map (classify_term form_man) (split_terms e form_man) in
    let (cbases, coffs, eoffs, syms) = (ref [], ref [], ref [], ref []) in
      List.iter
	(function
	   | ConstantBase(o) ->  cbases := o :: !cbases
	   | ConstantOffset(o) -> coffs := o :: !coffs
	   | ExprOffset(e) ->     eoffs := e :: !eoffs
	   | Symbol(v) ->          syms := v :: !syms)
	l;
      (!cbases, !coffs, !eoffs, !syms)

  let select_one l rand_func =
    let split_list l =
      let a = Array.of_list l in
      let len = Array.length a in 
      let k = len / 2 in
	((Array.to_list (Array.sub a 0 k)),
	 (Array.to_list (Array.sub a k (len - k))))
    in
    let rec loop l =
      match l with
	| [] -> failwith "Empty list in select_one"
	| [a] -> (a, [])
	| [a; b] -> if rand_func () then (a, [b]) else (b, [a])
	| l -> let (h1, h2) = split_list l in
	    if rand_func () then
	      let (e, h1r) = loop h1 in
		(e, h1r @ h2)
	    else
	      let (e, h2r) = loop h2 in
		(e, h1 @ h2r)
    in
      loop l

  let sum_list l = 
    match l with 
      | [] -> V.Constant(V.Int(V.REG_32, 0L))
      | [a] -> a
      | e :: r -> List.fold_left (fun a b -> V.BinOp(V.PLUS, a, b))
	  e r

  class sym_region_frag_machine (dt:decision_tree) = object(self)
    inherit SPFM.sym_path_frag_machine dt as spfm

    val mutable regions = []
    val region_vals = Hashtbl.create 101

    val mutable location_id = 0L

    method set_eip i =
      location_id <- i;
      spfm#set_eip i

    val sink_mem = new GM.granular_sink_memory

    method private region r =
      match r with
	| None -> (sink_mem :> (GM.granular_memory))
	| Some 0 -> (mem :> (GM.granular_memory))
	| Some r_num -> List.nth regions (r_num - 1)

    method private fresh_region =
      let new_idx = 1 + List.length regions in
      let region = (new GM.granular_hash_memory)  and
	  name = "region_" ^ (string_of_int new_idx) in
	regions <- regions @ [region];
	if !opt_zero_memory then
	  spfm#on_missing_zero_m region
	else
	  spfm#on_missing_symbol_m region name;
	new_idx

    method private region_for e =
      try
	Hashtbl.find region_vals e
      with Not_found ->
	let new_region = self#fresh_region in
	  Hashtbl.replace region_vals e new_region;
	  if !opt_trace_regions then
	    Printf.printf "Address %s is region %d\n"
	      (V.exp_to_string e) new_region;
	  new_region

    method private is_region_base e =
      Hashtbl.mem region_vals e

    val mutable sink_regions = []

    method private add_sink_region (e:Vine.exp) (size:int64) =
      self#on_missing_symbol_m sink_mem "sink";
      sink_regions <- ((self#region_for e), size) :: sink_regions

    method private choose_conc_offset_uniform ty e =
      let byte x = V.Constant(V.Int(V.REG_8, (Int64.of_int x))) in
      let bits = ref 0L in
	self#restore_path_cond
	  (fun () ->
	     if ty = V.REG_1 then
	       (* This special case avoids shifting REG_1s, which appears
		  to be legal in Vine IR but tickles bugs in multiple of
		  our solver backends. *)		  
	       let bit = self#extend_pc_random e false in
		 bits := (if bit then 1L else 0L)
	     else
	       for b = (V.bits_of_width ty) - 1 downto 0 do
		 let bit = self#extend_pc_random
		   (V.Cast(V.CAST_LOW, V.REG_1,
			   (V.BinOp(V.ARSHIFT, e,
				    (byte b))))) false
		 in
		   bits := (Int64.logor (Int64.shift_left !bits 1)
			      (if bit then 1L else 0L));
	       done);
	!bits

    method private choose_conc_offset_biased ty e =
      let const x = V.Constant(V.Int(ty, x)) in
      let rec try_list l =
	match l with
	  | [] -> self#choose_conc_offset_uniform ty e
	  | v :: r ->
	      if self#extend_pc_random (V.BinOp(V.EQ, e, (const v))) false then
		v
	      else
		try_list r
      in
      let bits = ref 0L in
	self#restore_path_cond
	  (fun () ->
	     bits := try_list
	       [0L; 1L; 2L; 4L; 8L; 16L; 32L; 64L; -1L; -2L; -4L; -8L]);
	!bits

    val mutable concrete_cache = Hashtbl.create 101

    method private choose_conc_offset_cached ty e =
      let const x = V.Constant(V.Int(ty, x)) in
      let (bits, verb) = 
	if Hashtbl.mem concrete_cache e then
	  (Hashtbl.find concrete_cache e, "Reused")
	else
	  let bits = 
	    match !opt_offset_strategy with
	      | UniformStrat -> self#choose_conc_offset_uniform ty e
	      | BiasedSmallStrat -> self#choose_conc_offset_biased ty e
	  in
	    Hashtbl.replace concrete_cache e bits;
	    (bits, "Picked") in
	if !opt_trace_sym_addrs then
	  Printf.printf "%s concrete value 0x%Lx for %s\n"
	    verb bits (V.exp_to_string e);
	self#add_to_path_cond (V.BinOp(V.EQ, e, (const bits)));
	bits

    method private concretize_inner ty e =
      match e with 
	| V.Cast((V.CAST_UNSIGNED|V.CAST_SIGNED) as ckind, cty, e2) ->
	    assert(cty = ty);
	    let ty2 = Vine_typecheck.infer_type None e2 in
	    let bits = self#choose_conc_offset_cached ty2 e2 in
	    let expand =
	      match (ckind, ty2) with
		| (V.CAST_UNSIGNED, V.REG_32) -> fix_u32
		| (V.CAST_UNSIGNED, V.REG_16) -> fix_u16
		| (V.CAST_UNSIGNED, V.REG_8)  -> fix_u8
		| (V.CAST_UNSIGNED, V.REG_1)  -> fix_u1
		| (V.CAST_SIGNED,   V.REG_32) -> fix_s32
		| (V.CAST_SIGNED,   V.REG_16) -> fix_s16
		| (V.CAST_SIGNED,   V.REG_8)  -> fix_s8
		| (V.CAST_SIGNED,   V.REG_1)  -> fix_s1
		| _ -> failwith "unhandled cast kind in concretize_inner"
	    in
	      expand bits
	| _ -> self#choose_conc_offset_cached ty e

    method private concretize ty e =
      dt#start_new_query;
      let v = self#concretize_inner ty e in
	dt#count_query;
	v

    val mutable sink_read_count = 0L

    method private check_cond cond_e = 
      dt#start_new_query_binary;
      let choices = ref None in 
	self#restore_path_cond
	  (fun () ->
	     let b = self#extend_pc_random cond_e false in
	       choices := dt#check_last_choices;
	       dt#count_query;
	       ignore(b));
	!choices

    method private region_expr e =
      if !opt_check_for_null then
	(match
	   self#check_cond (V.BinOp(V.EQ, e, V.Constant(V.Int(V.REG_32, 0L))))
	 with
	   | Some true -> Printf.printf "Can be null.\n"
	   | Some false -> Printf.printf "Cannot be null.\n"
	   | None -> Printf.printf "Can be null or non-null\n";
	       infl_man#maybe_measure_influence_deref e);
      dt#start_new_query;
      let (cbases, coffs, eoffs, syms) = classify_terms e form_man in
	if !opt_trace_sym_addr_details then
	  (Printf.printf "Concrete base terms: %s\n"
	     (String.concat " "
		(List.map (Printf.sprintf "0x%08Lx") cbases));
	   Printf.printf "Concrete offset terms: %s\n"
	     (String.concat " "
		(List.map (Printf.sprintf "0x%08Lx") coffs));
	   Printf.printf "Offset expression terms: %s\n"
	     (String.concat " "
		(List.map V.exp_to_string eoffs));
	   Printf.printf "Ambiguous symbol terms: %s\n"
	     (String.concat " "
		(List.map V.exp_to_string syms)));
	let cbase = List.fold_left Int64.add 0L cbases in
	let (base, off_syms) = match (cbase, syms) with
	  | (0L, []) -> raise NullDereference
	  | (0L, [v]) -> (Some(self#region_for v), [])
	  | (0L, vl) ->
	      let (bvar, rest_vars) =
		let (known_regions, not_known) =
		  List.partition (fun e -> self#is_region_base e) vl
		in
		  match known_regions with
		    | [v] -> (v, not_known)
		    | _ -> 
			select_one vl
			  (fun () -> self#random_case_split
			     !opt_trace_decisions)
	      in
		if !opt_trace_sym_addrs then
		  Printf.printf "Choosing %s as the base address\n"
		    (V.exp_to_string bvar);
		(Some(self#region_for bvar), rest_vars)
	  | (off, vl) ->
	      (Some 0, vl)
	in
	let (region, offset) =
	  (match base with
	     | Some r
		 when List.exists (fun (r', _) -> r = r') sink_regions ->
		 let (r', size) =
		   List.find (fun (r', _) -> r = r') sink_regions in
		   Printf.printf "Ignoring access to sink region\n";
		   (let sat_dir = ref false in
		      self#restore_path_cond
			(fun () ->
			   sat_dir := self#extend_pc_random
			     (V.BinOp(V.LT, e,
				      V.Constant(V.Int(V.REG_32, size))))
			     false);
		      if !sat_dir = true then
			Printf.printf "Can be in bounds.\n"
		      else
			Printf.printf "Can be out of bounds.\n");
		   sink_read_count <- Int64.add sink_read_count 0x10L;
		   (None, sink_read_count)
	     | _ ->
		 let coff = List.fold_left Int64.add 0L coffs in
		 let offset = Int64.add (Int64.add cbase coff)
		   (match (eoffs, off_syms) with
		      | ([], []) -> 0L
		      | (el, vel) -> 
			  (self#concretize_inner V.REG_32
			     (sum_list (el @ vel)))) in
		   (base, (fix_u32 offset)))
	in
	  dt#count_query;
	  (region, offset)

    method private eval_addr_exp_region_conc_path e =
      let term_is_known_base = function
	| V.Lval(V.Temp(var)) -> form_man#known_region_base var
	| _ -> false
      in
      let terms = split_terms e form_man in
      let (known_bases, rest) =
	List.partition term_is_known_base terms in
	match known_bases with
	  | [] ->
	      let a = form_man#eval_expr e in
		if !opt_trace_sym_addrs then
		  Printf.printf "Computed concrete value 0x%08Lx\n" a;
		if !opt_solve_path_conditions then
		  (let cond = V.BinOp(V.EQ, e,
				      V.Constant(V.Int(V.REG_32, a)))
		   in
		   let sat = self#extend_pc_known cond false true in
		     assert(sat));
		(Some 0, a)
	  | [V.Lval(V.Temp(var)) as vexp] ->
	      let sum = sum_list rest in
	      let a = form_man#eval_expr sum in
	      let a_const = V.Constant(V.Int(V.REG_32, a)) in
		if !opt_trace_sym_addrs then
		  Printf.printf
		    "Computed concrete offset %s + 0x%08Lx\n" 
		    (V.var_to_string var) a;
		if !opt_solve_path_conditions && 
		  (sum <> a_const)
		then
		  (let cond = V.BinOp(V.EQ, sum, a_const) in
		   let sat = self#extend_pc_known cond false true in
		     assert(sat));
		(Some(self#region_for vexp), a)
	  | [_] -> failwith "known_base invariant failure"
	  | _ -> failwith "multiple bases"

    method eval_addr_exp_region exp =
      let v = self#eval_int_exp_simplify exp in
	try
	  (Some 0, D.to_concrete_32 v)
	with NotConcrete _ ->
	  let e = D.to_symbolic_32 v in
	  let eip = self#get_eip in
	    if !opt_trace_sym_addrs then
	      Printf.printf "Symbolic address %s @ (0x%Lx)\n"
		(V.exp_to_string e) eip;
	    if !opt_concrete_path then
	      self#eval_addr_exp_region_conc_path e
	    else
	      self#region_expr e
		  
    (* Because we override handle_{load,store}, this should only be
       called for jumps. *)
    method eval_addr_exp exp =
      let (r, addr) = self#eval_addr_exp_region exp in
	match r with
	  | Some 0 -> addr
	  | Some r_num -> raise SymbolicJump
	  | None -> raise SymbolicJump

    method get_word_var_concretize reg do_influence name : int64 =
      let v = self#get_int_var (Hashtbl.find reg_to_var reg) in
      try (D.to_concrete_32 v)
      with NotConcrete _ ->
	let e = D.to_symbolic_32 v in
	  if do_influence then 
	    (Printf.printf "Measuring symbolic %s influence..." name;
	     infl_man#measure_point_influence name e);
	  self#concretize V.REG_32 e

    method load_word_concretize addr do_influence name =
      let v = self#load_word addr in
      try (D.to_concrete_32 v)
      with NotConcrete _ ->
	let e = D.to_symbolic_32 v in
	  if do_influence then 
	    (Printf.printf "Measuring symbolic %s influence..." name;
	     infl_man#measure_point_influence name e);
	  self#concretize V.REG_32 e

    method load_short_concretize addr do_influence name =
      let v = self#load_short addr in
      try (D.to_concrete_16 v)
      with NotConcrete _ ->
	let e = D.to_symbolic_16 v in
	  if do_influence then 
	    (Printf.printf "Measuring symbolic %s influence..." name;
	     infl_man#measure_point_influence name e);
	  Int64.to_int (self#concretize V.REG_16 e)

    method load_byte_concretize addr do_influence name =
      let v = self#load_byte addr in
      try (D.to_concrete_8 v)
      with NotConcrete _ ->
	let e = D.to_symbolic_8 v in
	  if do_influence then 
	    (Printf.printf "Measuring symbolic %s influence..." name;
	     infl_man#measure_point_influence name e);
	  Int64.to_int (self#concretize V.REG_8 e)

    method private maybe_concretize_binop op v1 v2 ty1 ty2 =
      let conc t v =
	match t with
	  | V.REG_1 ->
	      (try ignore(D.to_concrete_1 v); v
	       with NotConcrete _ ->
		 (D.from_concrete_1
		    (Int64.to_int
		       (self#concretize t (D.to_symbolic_1 v)))))
	  | V.REG_8 ->
	      (try ignore(D.to_concrete_8 v); v
	       with NotConcrete _ ->
		 (D.from_concrete_8
		    (Int64.to_int
		       (self#concretize t (D.to_symbolic_8 v)))))
	  | V.REG_16 ->
	      (try ignore(D.to_concrete_16 v); v
	       with NotConcrete _ ->
		 (D.from_concrete_16
		    (Int64.to_int
		       (self#concretize t (D.to_symbolic_16 v)))))
	  | V.REG_32 ->
	      (try ignore(D.to_concrete_32 v); v
	       with NotConcrete _ ->
		 (D.from_concrete_32
		    (self#concretize t (D.to_symbolic_32 v))))
	  | V.REG_64 ->
	      (try ignore(D.to_concrete_64 v); v
	       with NotConcrete _ ->
		 (D.from_concrete_64
		    (self#concretize t (D.to_symbolic_64 v))))
	  | _ -> failwith "Bad type in maybe_concretize_binop"
      in
	match op with
	  | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD 
		when !opt_concretize_divisors
	      -> (v1, (conc ty2 v2))
	  | _ -> (v1, v2)

    method private store_byte_region  r addr b =
      (self#region r)#store_byte  addr b
    method private store_short_region r addr s =
      (self#region r)#store_short addr s
    method private store_word_region  r addr w =
      (self#region r)#store_word  addr w
    method private store_long_region  r addr l =
      (self#region r)#store_long  addr l

    method private load_byte_region  r addr = (self#region r)#load_byte  addr
    method private load_short_region r addr = (self#region r)#load_short addr
    method private load_word_region  r addr = (self#region r)#load_word  addr
    method private load_long_region  r addr = (self#region r)#load_long  addr

    method private query_bitwidth e ty =
      let rec loop min max =
	assert(min <= max);
	if min = max then
	  min
	else
	  let mid = (min + max) / 2 in
	  let mask = Int64.shift_right_logical (-1L) (64-mid) in
	  let cond_e = V.BinOp(V.LE, e, V.Constant(V.Int(ty, mask))) in
	  let in_bounds = self#check_cond cond_e in
	    if !opt_trace_tables then
	      Printf.printf "(%s) < 2**%d: %s\n" (V.exp_to_string e) mid
		(match in_bounds with
		   | Some true -> "valid"
		   | Some false -> "unsat"
		   | None -> "invalid" (* though satisfiable *));
	    if in_bounds = Some true then
	      loop min mid
	    else
	      loop (mid + 1) max
      in
      let max_wd = V.bits_of_width ty in
      let wd = loop 0 max_wd in
	if !opt_trace_tables then
	  Printf.printf "Bit width based on queries is %d\n" wd;
	wd
      
    val tables = Hashtbl.create 101

    val table_trees_cache = Hashtbl.create 101

    method private maybe_table_load addr_e ty =
      let e = D.to_symbolic_32 (self#eval_int_exp_simplify addr_e) in
      let (cbases, coffs, eoffs, syms) = classify_terms e form_man in
	let cbase = List.fold_left Int64.add 0L cbases in
	  if cbase = 0L then
	    None
	  else
	    let cloc = Int64.add cbase (List.fold_left Int64.add 0L coffs) in
	    let off_exp = sum_list (eoffs @ syms) in
	    let fast_wd = narrow_bitwidth form_man off_exp in
	    let wd_opt =
	      if fast_wd = 0 then
		None
	      else if !opt_table_limit = 0 then
		None
	      else if fast_wd > !opt_table_limit then
		let slow_wd = self#query_bitwidth off_exp V.REG_32 in
		  assert(slow_wd <= fast_wd);
		  if slow_wd > !opt_table_limit then
		    (if !opt_trace_tables then
		       Printf.printf
			 ("Load with base %08Lx, offset %s of size 2**%d "
			  ^^ "is not a table\n")
			 cloc (V.exp_to_string off_exp) slow_wd;
		     None)
		  else
		    Some slow_wd
	      else
		Some fast_wd
	    in
	      match wd_opt with None -> None | Some wd ->
		(let load_ent addr = match ty with
		   | V.REG_8  -> (self#region (Some 0))#load_byte  addr
		   | V.REG_16 -> (self#region (Some 0))#load_short addr
		   | V.REG_32 -> (self#region (Some 0))#load_word  addr
		   | V.REG_64 -> (self#region (Some 0))#load_long  addr
		   | _ -> failwith "Unexpected type in maybe_table_load" 
		 in
		 let table = map_n
		   (fun i -> load_ent (Int64.add cloc (Int64.of_int i)))
		   (1 lsl wd - 1) in
		 let table_num = try
		   Hashtbl.find tables table with
		     | Not_found ->
			 let i = Hashtbl.length tables in
			   Hashtbl.replace tables table i;
			   if !opt_trace_tables then
			     (Printf.printf "Table %d is: " i;
			      List.iter
				(fun v -> Printf.printf "%s "
				   (match ty with
				      | V.REG_1  -> D.to_string_1  v
				      | V.REG_8  -> D.to_string_8  v
				      | V.REG_16 -> D.to_string_16 v
				      | V.REG_32 -> D.to_string_32 v
				      | V.REG_64 -> D.to_string_64 v
				      | _ -> failwith "Can't happen"))
				table;
			      Printf.printf "\n");
			   i
		 in
		   if !opt_trace_tables then
		     Printf.printf
		       "Load with base %08Lx, size 2**%d is table %d"
		       cloc wd table_num;
		   let v = try
		     let v = 
		       Hashtbl.find table_trees_cache (table_num, off_exp) in
		       if !opt_trace_tables then
			 (Printf.printf " (hit cache)\n";
			  flush stdout);
		       v
		   with
		     | Not_found ->
			 Printf.printf "\n";
			 flush stdout;
			 let v = lookup_tree form_man off_exp wd ty table in
			   Hashtbl.replace table_trees_cache
			     (table_num, off_exp) v;
			   v
		   in
		     Some v)

    method private handle_load addr_e ty =
      match self#maybe_table_load addr_e ty with
	| Some v -> (v, ty)
	| None ->
      let (r, addr) = self#eval_addr_exp_region addr_e in
      let v =
	(match ty with
	   | V.REG_8  -> form_man#simplify8  (self#load_byte_region  r addr)
	   | V.REG_16 -> form_man#simplify16 (self#load_short_region r addr)
	   | V.REG_32 -> form_man#simplify32 (self#load_word_region  r addr)
	   | V.REG_64 -> form_man#simplify64 (self#load_long_region  r addr)
	   | _ -> failwith "Unsupported memory type") in
	(if !opt_trace_loads then
	  (Printf.printf "Load from %s "
	     (match r with
		| None -> "sink"
		| Some 0 -> "conc. mem"
		| Some r_num -> "region " ^ (string_of_int r_num));
	   Printf.printf "%08Lx = %s" addr (D.to_string_32 v);
	   (if !opt_use_tags then
	      Printf.printf " (%Ld @ %08Lx)" (D.get_tag v) location_id);
	   Printf.printf "\n"));
	if r = Some 0 && (Int64.abs (fix_s32 addr)) < 4096L then
	  raise NullDereference;
	(v, ty)

    method private push_cond_prefer_true cond_v = 
      try
	if (D.to_concrete_1 cond_v) = 1 then
	  (true, Some true)
	else
	  (false, Some false)
      with
	  NotConcrete _ ->
	    let e = D.to_symbolic_1 cond_v in
	      (dt#start_new_query_binary;
	       let b = self#extend_pc_known e true true in
	       let choices = dt#check_last_choices in
		 dt#count_query;
		 (b, choices))

    method private handle_store addr_e ty rhs_e =
      let (r, addr) = self#eval_addr_exp_region addr_e and
	  value = self#eval_int_exp_simplify rhs_e in
	if r = Some 0 && (Int64.abs (fix_s32 addr)) < 4096L then
	  raise NullDereference;
	if !opt_trace_stores then
	  if not (ty = V.REG_8 && r = None) then
	    (Printf.printf "Store to %s "
	       (match r with
		  | None -> "sink"
		  | Some 0 -> "conc. mem"
		  | Some r_num -> "region " ^ (string_of_int r_num));
	     Printf.printf "%08Lx = %s" addr (D.to_string_32 value);
	     (if !opt_use_tags then
		Printf.printf " (%Ld @ %08Lx)" (D.get_tag value) location_id);
	     Printf.printf "\n");
	(match (!opt_target_region_start, r, ty) with
	   | (Some from, Some 0, V.REG_8) ->
	       if addr >= from &&
		 addr < (Int64.add from
			   (Int64.of_int
			      (String.length !opt_target_region_string)))
	       then
		   let offset = Int64.to_int (Int64.sub addr from) in
		   let c = (!opt_target_region_string).[offset] in
		     if !opt_trace_target then
		       Printf.printf
			 "Store to target string offset %d: %s (vs '%c'): "
			 offset (D.to_string_32 value) c;
		     let cond_v = D.eq8 value (D.from_concrete_8 (Char.code c))
		     in
		     let (b, choices) = self#push_cond_prefer_true cond_v in
		       if !opt_trace_target then
			 Printf.printf "%s, %b\n"
			   (match choices with
			      | Some true -> "known equal"
			      | Some false -> "known mismatch"
			      | None -> "possible") b;
		       if not b then raise DisqualifiedPath;
		       if !opt_target_guidance <> 0.0 then
			 dt#set_heur offset;
		       if !opt_finish_on_target_match && b &&
			 offset = (String.length !opt_target_region_string) - 1
		       then
			 raise LastIteration
	   | _ -> ());
	(match ty with
	   | V.REG_8 -> self#store_byte_region r addr value
	   | V.REG_16 -> self#store_short_region r addr value
	   | V.REG_32 -> self#store_word_region r addr value
	   | V.REG_64 -> self#store_long_region r addr value
	   | _ -> failwith "Unsupported type in memory move")

    method concretize_misc =
      if !opt_arch = X86 then
	let var = Hashtbl.find reg_to_var R_DFLAG in
	let d = self#get_int_var var in
	  try ignore(D.to_concrete_32 d)
	  with NotConcrete _ ->
	    let e = D.to_symbolic_32 d in
	      if e <> V.Unknown("uninit") then
		self#set_int_var var
		  (D.from_concrete_32 
		     (self#concretize V.REG_32 e))

    method make_sink_region varname size =
      self#add_sink_region
	(D.to_symbolic_32 (form_man#fresh_symbolic_32 varname)) size

    val mutable gdt_base_var = D.uninit
    val mutable ldt_base_var = D.uninit

    method make_x86_segtables_symbolic =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	gdt_base_var <- form_man#fresh_region_base "GDT";
	ldt_base_var <- form_man#fresh_region_base "LDT";
	reg R_GDT gdt_base_var;
	reg R_LDT ldt_base_var

    method store_word_special_region which addr v =
      let vexp = match which with
	| R_GDT -> D.to_symbolic_32 gdt_base_var
	| R_LDT -> D.to_symbolic_32 ldt_base_var
	| _ -> failwith "Unknown special region"
      in
      let region = self#region_for vexp in
	self#store_word_region (Some region) addr (D.from_concrete_32 v)

    method reset () =
      spfm#reset ();
      List.iter (fun gm -> gm#clear ()) regions;
      Hashtbl.clear concrete_cache
  end
end

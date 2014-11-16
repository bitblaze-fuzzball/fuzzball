(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine

open Exec_domain
open Exec_exceptions
open Exec_options
open Frag_simplify
open Frag_marshal

module VarWeak = Weak.Make(VarByInt)

(* Unlike Vine_util.list_unique, this preserves order (keeping the
   first occurrence) which is important because the temps have to retain
   a topological ordering. *)
let list_unique l = 
  let h = Hashtbl.create 10 in
  let rec loop = function
    | [] -> []
    | e :: el ->
	if Hashtbl.mem h e then
	  loop el
	else
	  (Hashtbl.replace h e ();
	   e :: (loop el))
  in
    (loop l)

let cf_eval e =
  match Vine_opt.constant_fold (fun _ -> None) e with
    | V.Constant(V.Int(_, _)) as c -> c
    | e ->
	Printf.printf "Left with %s\n" (V.exp_to_string e);
	failwith "cf_eval failed in eval_expr"

let conjoin l =
  match l with
    | [] -> V.exp_true
    | e :: el -> List.fold_left (fun a b -> V.BinOp(V.BITAND, a, b)) e el
	
let disjoin l =
  match l with
    | [] -> V.exp_false
    | e :: el -> List.fold_left (fun a b -> V.BinOp(V.BITOR, a, b)) e el

let xorjoin l =
  match l with
    | [] -> V.exp_false
    | e :: el -> List.fold_left (fun a b -> V.BinOp(V.XOR, a, b)) e el

module FormulaManagerFunctor =
  functor (D : Exec_domain.DOMAIN) ->
struct
  (* This has to be outside the class because I want it to have
     polymorphic type. *)
  let if_expr_temp form_man var fn_t else_val else_fn =
    let box = ref else_val in
      form_man#if_expr_temp_unit var
	(fun e -> 
	   match e with
	     | Some (e) -> box := fn_t e
	     | None -> (else_fn var)
	);
      !box

  (* This function captures a kind of structure used for a function
     that would naturally be written as recursive over the structure of
     expressions, but wants to recuse into the definitions of temporary
     variables. It's important to memoize such a function over the
     temporaries, since otherwise a given temporary could appear
     exponentially many times in the traversal for a deep expression. We
     also pass the size of the variable when it's reused, since that's
     useful information that may not otherwise be visible nearby. It
     might also be worth caching between calls to this function, though
     that's less critical. As with if_expr_temp this has a polymorphic
     type, though in the motivating examples the result of the traversal
     was always an int.
  *)
  let map_expr_temp form_man e f combine =
    let cache = V.VarHash.create 101 in
    let rec recurse e =
      let rec maybe_recurse e var wd =
	try
	  V.VarHash.find cache var
	with
	    Not_found ->
	      let box = ref None in
		form_man#if_expr_temp_unit var
		  (function
		     | Some (e') ->
			 let res = f recurse e' in
			   V.VarHash.replace cache var res;
			   box := Some (combine wd res)
		     | None -> box := Some (f recurse e));
		match !box with
		  | Some res -> res
		  | None -> failwith "Empty box invariant failure"
      in
	match e with
	  | V.Lval(V.Temp((_, _,  V.REG_1) as var)) -> maybe_recurse e var  1
	  | V.Lval(V.Temp((_, _,  V.REG_8) as var)) -> maybe_recurse e var  8
	  | V.Lval(V.Temp((_, _, V.REG_16) as var)) -> maybe_recurse e var 16
	  | V.Lval(V.Temp((_, _, V.REG_32) as var)) -> maybe_recurse e var 32
	  | V.Lval(V.Temp((_, _, V.REG_64) as var)) -> maybe_recurse e var 64
	  | _ -> f recurse e
    in
      f recurse e

  class formula_manager = object(self)
    val input_vars = Hashtbl.create 30

    method private fresh_symbolic_var str ty =
      try Hashtbl.find input_vars str with
	  Not_found ->
	    Hashtbl.replace input_vars str (V.newvar str ty);
	    Hashtbl.find input_vars str

    method private fresh_symbolic_vexp str ty =
      V.Lval(V.Temp(self#fresh_symbolic_var str ty))

    method private fresh_symbolic str ty =
      let v = D.from_symbolic (self#fresh_symbolic_vexp str ty) in
	if !opt_use_tags then
	  Printf.printf "Symbolic %s is %Ld\n" str (D.get_tag v);
	v

    method fresh_symbolic_1  s = self#fresh_symbolic s V.REG_1
    method fresh_symbolic_8  s = self#fresh_symbolic s V.REG_8
    method fresh_symbolic_16 s = self#fresh_symbolic s V.REG_16
    method fresh_symbolic_32 s = self#fresh_symbolic s V.REG_32
    method fresh_symbolic_64 s = self#fresh_symbolic s V.REG_64

    method get_input_vars = Hashtbl.fold (fun s v l -> v :: l) input_vars []

    val region_base_vars = Hashtbl.create 30

    method fresh_region_base s =
      assert(not (Hashtbl.mem region_base_vars s));
      let var = self#fresh_symbolic_var s V.REG_32 in
	Hashtbl.replace region_base_vars s var;
	D.from_symbolic (V.Lval(V.Temp(var)))

    method known_region_base ((_,s,_):V.var) =
      Hashtbl.mem region_base_vars s

    val region_vars = Hashtbl.create 30

    method private fresh_symbolic_mem ty str addr =
      let v = try Hashtbl.find region_vars str with
	  Not_found ->
	    Hashtbl.replace region_vars str
	      (V.newvar str (V.TMem(V.REG_32, V.Little)));
	    Hashtbl.find region_vars str
      in
	D.from_symbolic
	  (V.Lval(V.Mem(v, V.Constant(V.Int(V.REG_32, addr)), ty)))

    method fresh_symbolic_mem_1  = self#fresh_symbolic_mem V.REG_1
    method fresh_symbolic_mem_8  = self#fresh_symbolic_mem V.REG_8
    method fresh_symbolic_mem_16 = self#fresh_symbolic_mem V.REG_16
    method fresh_symbolic_mem_32 = self#fresh_symbolic_mem V.REG_32
    method fresh_symbolic_mem_64 = self#fresh_symbolic_mem V.REG_64

    method input_dl =
      (Hashtbl.fold (fun k v l -> v :: l) input_vars []) @
      (Hashtbl.fold (fun k v l -> v :: l) region_vars [])

    val seen_concolic = Hashtbl.create 30
    val valuation = Hashtbl.create 30

    method private make_concolic ty str v =
      let var =
	(if Hashtbl.mem seen_concolic (str, 0L, ty) then
	   let var = Hashtbl.find seen_concolic (str, 0L, ty) in
	   let old_val = Hashtbl.find valuation var in
	     if v <> old_val then
	       if !opt_trace_unexpected then
		 Printf.printf
		   "Value mismatch: %s was 0x%Lx and then later 0x%Lx\n"
		   str old_val v;
	     var
	 else 
	   (let new_var = self#fresh_symbolic str ty in
	      Hashtbl.replace seen_concolic (str, 0L, ty) new_var;
	      new_var))
      in
	if !opt_trace_taint then
	  Printf.printf "Valuation %s = 0x%Lx:%s\n"
	    str v (V.type_to_string ty);
	Hashtbl.replace valuation var v;
	var

    method make_concolic_8  s v = self#make_concolic V.REG_8  s(Int64.of_int v)
    method make_concolic_16 s v = self#make_concolic V.REG_16 s(Int64.of_int v)
    method make_concolic_32 s v = self#make_concolic V.REG_32 s v
    method make_concolic_64 s v = self#make_concolic V.REG_64 s v

    method fresh_region_base_concolic s v =
      assert(not (Hashtbl.mem region_base_vars s));
      let var = self#fresh_symbolic_var s V.REG_32 in
	Hashtbl.replace region_base_vars s var;
	ignore(self#make_concolic_32 s v);
	D.from_symbolic (V.Lval(V.Temp(var)))

    method make_concolic_mem_8 str addr v_int =
      let v = Int64.of_int v_int in
      let var =
	(if Hashtbl.mem seen_concolic (str, addr, V.REG_8) then
	   let var = Hashtbl.find seen_concolic (str, addr, V.REG_8) in
	   let old_val = Hashtbl.find valuation var in
	     if v <> old_val then
	       if !opt_trace_unexpected then
		 Printf.printf
		   "Value mismatch: %s:0x%Lx was 0x%Lx and then later 0x%Lx\n"
		   str addr old_val v;
	     var
	 else 
	   (let new_var = self#fresh_symbolic_mem V.REG_8 str addr in
	      Hashtbl.replace seen_concolic (str, addr, V.REG_8) new_var;
	      new_var))
      in
	if !opt_trace_taint then
	  Printf.printf "Byte valuation %s:0x%Lx = 0x%Lx\n"
	    str addr v;
	Hashtbl.replace valuation var v;
	(match !input_string_mem_prefix with
	   | None -> input_string_mem_prefix := Some (str ^ "_byte_")
	   | _ -> ());
	max_input_string_length :=
	  max !max_input_string_length (1 + Int64.to_int addr);
	var

    method private mem_var region_str ty addr =
      let ty_str = (match ty with
		      | V.REG_8 -> "byte"
		      | V.REG_16 -> "short"
		      | V.REG_32 -> "word"
		      | V.REG_64 -> "long"
		      | _ -> failwith "Bad size in mem_var")
      in
      let name = Printf.sprintf "%s_%s_0x%08Lx" region_str ty_str addr
      in
	self#fresh_symbolic_var name ty

    val mutable mem_byte_vars = V.VarHash.create 30

    method private mem_var_byte region_str addr =
      let var = self#mem_var region_str V.REG_8 addr in
	V.VarHash.replace mem_byte_vars var ();
	var

    method private mem_axioms_short region_str addr svar =
      let bvar0 = self#mem_var_byte region_str addr and
	  bvar1 = self#mem_var_byte region_str (Int64.add addr 1L) in
	[svar, D.to_symbolic_16
	   (D.assemble16 (D.from_symbolic (V.Lval(V.Temp(bvar0))))
	      (D.from_symbolic (V.Lval(V.Temp(bvar1)))))]

    method private mem_axioms_word region_str addr wvar =
      let svar0 = self#mem_var region_str V.REG_16 addr and
	  svar1 = self#mem_var region_str V.REG_16 (Int64.add addr 2L) in
	[wvar, D.to_symbolic_32
	   (D.assemble32 (D.from_symbolic (V.Lval(V.Temp(svar0))))
	      (D.from_symbolic (V.Lval(V.Temp(svar1)))))]
	@ (self#mem_axioms_short region_str addr svar0)
	@ (self#mem_axioms_short region_str (Int64.add addr 2L) svar1)

    method private mem_axioms_long region_str addr lvar =
      let wvar0 = self#mem_var region_str V.REG_32 addr and
	  wvar1 = self#mem_var region_str V.REG_32 (Int64.add addr 4L) in
	[lvar, D.to_symbolic_64
	   (D.assemble32 (D.from_symbolic (V.Lval(V.Temp(wvar0))))
	      (D.from_symbolic (V.Lval(V.Temp(wvar1)))))]
	@ (self#mem_axioms_word region_str addr wvar0)
	@ (self#mem_axioms_word region_str (Int64.add addr 4L) wvar1)

    val mutable mem_axioms = V.VarHash.create 30

    method private add_mem_axioms region_str ty addr =
      let var = self#mem_var region_str ty addr in
	if ty = V.REG_8 then
	  V.VarHash.replace mem_byte_vars var ()
	else
	  let al = (match ty with
		      | V.REG_8  -> failwith "Unexpected REG_8"
		      | V.REG_16 -> self#mem_axioms_short region_str addr var
		      | V.REG_32 -> self#mem_axioms_word region_str addr var
		      | V.REG_64 -> self#mem_axioms_long region_str addr var
		      | _ -> failwith "Unexpected type in add_mem_axioms") in
	    List.iter
	      (fun (lhs, rhs) -> V.VarHash.replace mem_axioms lhs rhs)
	      al;
	    assert(V.VarHash.mem mem_axioms var);

    method private rewrite_mem_expr e =
      match e with
	| V.Lval(V.Mem((_,region_str,ty1),
		       V.Constant(V.Int(V.REG_32, addr)), ty2))
	  -> (self#add_mem_axioms region_str ty2 addr;
	      V.Lval(V.Temp(self#mem_var region_str ty2 addr)))
	| _ -> failwith "Bad expression in rewrite_mem_expr"

    method rewrite_for_solver e =
      let rec loop e =
	match e with
	  | V.BinOp(op, e1, e2) -> V.BinOp(op, (loop e1), (loop e2))
	  | V.UnOp(op, e1) -> V.UnOp(op, (loop e1))
	  | V.Constant(_) -> e
	  | V.Lval(V.Temp(_)) -> e
	  | V.Lval(V.Mem(_, _, _)) -> self#rewrite_mem_expr e
	  | V.Name(_) -> e
	  | V.Cast(kind, ty, e1) -> V.Cast(kind, ty, (loop e1))
	  | V.Unknown(_) -> e
	  | V.Let(V.Temp(v), e1, e2) ->
	      V.Let(V.Temp(v), (loop e1), (loop e2))
	  | V.Let(V.Mem(_,_,_), _, _) ->	      
	      failwith "Unexpected memory let in rewrite_for_solver"
	  | V.Ite(ce, te, fe) ->
	      V.Ite((loop ce), (loop te), (loop fe))
      in
	loop e

    method get_mem_axioms =
      let of_type ty ((n,s,ty'),e) = (ty = ty') in
      let l = V.VarHash.fold
	(fun lhs rhs l -> (lhs, rhs) :: l) mem_axioms [] in
      let shorts = List.filter (of_type V.REG_16) l and
	  words  = List.filter (of_type V.REG_32) l and
	  longs  = List.filter (of_type V.REG_64) l in
	shorts @ words @ longs

    method get_mem_bytes =
      V.VarHash.fold (fun v _ l -> v :: l) mem_byte_vars []

    method reset_mem_axioms = 
      V.VarHash.clear mem_byte_vars;
      V.VarHash.clear mem_axioms

    method private with_saved_mem_axioms f =
      let old_mbv = mem_byte_vars and
	  old_max = mem_axioms in
	mem_byte_vars <- V.VarHash.create 30;
	mem_axioms <- V.VarHash.create 30;
	let r = f () in
	  mem_byte_vars <- old_mbv;
	  mem_axioms <- old_max;
	  r

    method private eval_var lv =
      let d = D.from_symbolic (V.Lval lv) in
	match lv with
	  | V.Mem(mem_var, V.Constant(V.Int(_, addr)), V.REG_8) ->
	      if not (Hashtbl.mem valuation d) then
		Printf.printf "Unexpected symbolic byte %s\n"
		  (V.lval_to_string lv);
	      assert(Hashtbl.mem valuation d);
	      D.from_concrete_8 (Int64.to_int (Hashtbl.find valuation d))
	  | V.Mem(mem_var, V.Constant(V.Int(_, addr)), V.REG_16) ->
	      if Hashtbl.mem valuation d then
		D.from_concrete_16 (Int64.to_int (Hashtbl.find valuation d))
	      else
		D.assemble16
		  (self#eval_var
		     (V.Mem(mem_var, V.Constant(V.Int(V.REG_32, addr)),
			    V.REG_8)))
		  (self#eval_var
		     (V.Mem(mem_var,
			    V.Constant(V.Int(V.REG_32, 
					     (Int64.add 1L addr))),
			    V.REG_8)))
	  | V.Mem(mem_var, V.Constant(V.Int(_, addr)), V.REG_32) ->
	      if Hashtbl.mem valuation d then
		D.from_concrete_32 (Hashtbl.find valuation d)
	      else
		D.assemble32
		  (self#eval_var
		     (V.Mem(mem_var, V.Constant(V.Int(V.REG_32, addr)),
			    V.REG_16)))
		  (self#eval_var
		     (V.Mem(mem_var, V.Constant(V.Int(V.REG_32, 
						      (Int64.add 2L addr))),
			    V.REG_16)))
	  | V.Mem(mem_var, V.Constant(V.Int(_, addr)), V.REG_64) ->
	      if Hashtbl.mem valuation d then
		D.from_concrete_64 (Hashtbl.find valuation d)
	      else
		D.assemble64
		  (self#eval_var
		     (V.Mem(mem_var, V.Constant(V.Int(V.REG_32, addr)),
			    V.REG_32)))
		  (self#eval_var
		     (V.Mem(mem_var, V.Constant(V.Int(V.REG_32, 
						      (Int64.add 4L addr))),
			    V.REG_32)))
	  | V.Temp(_, _, V.REG_8) ->
	      assert(Hashtbl.mem valuation d);
	      D.from_concrete_8 (Int64.to_int (Hashtbl.find valuation d))
	  | V.Temp(_, _, V.REG_16) ->
	      assert(Hashtbl.mem valuation d);
	      D.from_concrete_16 (Int64.to_int (Hashtbl.find valuation d))
	  | V.Temp(_, _, V.REG_32) ->
	      assert(Hashtbl.mem valuation d);
	      D.from_concrete_32 (Hashtbl.find valuation d)
	  | V.Temp(_, _, V.REG_64) ->
	      assert(Hashtbl.mem valuation d);
	      D.from_concrete_64 (Hashtbl.find valuation d)

	  | _ -> failwith "unexpected lval expr in eval_var"

    (* subexpression cache *)
    val subexpr_to_temp_var_info = Hashtbl.create 1001
    val temp_var_num_to_subexpr = Hashtbl.create 1001
    val mutable temp_var_num = 0

    val temp_var_num_evaled = Hashtbl.create 1001

    method eval_expr e =
      let rec loop e =
	match e with
	  | V.BinOp(op, e1, e2) -> cf_eval (V.BinOp(op, loop e1, loop e2))
	  | V.UnOp(op, e1) -> cf_eval (V.UnOp(op, loop e1))
	  | V.Cast(op, ty, e1) -> cf_eval (V.Cast(op, ty, loop e1))
	  | V.Lval(V.Mem(_, _, ty) as lv) ->
	      let d = self#eval_var lv in
	      let v = match ty with
		| V.REG_8  -> Int64.of_int (D.to_concrete_8  d)
		| V.REG_16 -> Int64.of_int (D.to_concrete_16 d)
		| V.REG_32 -> D.to_concrete_32 d
		| V.REG_64 -> D.to_concrete_64 d
		| _ -> failwith "Unexpected type in eval_expr"
	      in
		V.Constant(V.Int(ty, v))
	  | V.Constant(V.Int(_, _)) -> e
	  | V.Lval(V.Temp(n,s,t))
	      when Hashtbl.mem temp_var_num_to_subexpr n ->
	      (try Hashtbl.find temp_var_num_evaled n
	       with
		 | Not_found ->
		     let (e_enc, _) = Hashtbl.find temp_var_num_to_subexpr n in
		     let e' = loop (decode_exp e_enc)
		     in
		       Hashtbl.replace temp_var_num_evaled n e';
		       e')
	  | V.Lval(V.Temp(n,s,ty) as lv) ->
	      let d = self#eval_var lv in
	      let v = match ty with
		| V.REG_8  -> Int64.of_int (D.to_concrete_8  d)
		| V.REG_16 -> Int64.of_int (D.to_concrete_16 d)
		| V.REG_32 -> D.to_concrete_32 d
		| V.REG_64 -> D.to_concrete_64 d
		| _ -> failwith "Unexpected type in eval_expr"
	      in
		V.Constant(V.Int(ty, v))
	  | V.Ite(ce, te, fe) -> cf_eval (V.Ite(loop ce, loop te, loop fe))
	  | V.Let(_, _, _)
	  | V.Unknown(_)
	  | V.Name(_)
	  | V.Constant(V.Str(_))
	    ->
	      Printf.printf "Can't evaluate %s\n" (V.exp_to_string e);
	      failwith "Unexpected expr in eval_expr"
      in
	match loop e with
	  | V.Constant(V.Int(_, i64)) -> i64
	  | e ->
	      Printf.printf "Left with %s\n" (V.exp_to_string e);
	      failwith "Constant invariant failed in eval_expr"

    method concolic_eval_1 d =
      Int64.to_int (self#eval_expr (D.to_symbolic_1 d))

    method concolic_eval_8 d =
      Int64.to_int (self#eval_expr (D.to_symbolic_8 d))

    method concolic_eval_16 d =
      Int64.to_int (self#eval_expr (D.to_symbolic_16 d))

    method concolic_eval_32 d =
      self#eval_expr (D.to_symbolic_32 d)

    method concolic_eval_64 d =
      self#eval_expr (D.to_symbolic_64 d)

    method private eval_var_from_ce ce lv =
      match lv with
	| V.Temp(_, s, ty) ->
	    let v = try List.assoc s ce 
	    with Not_found ->
	      0L 
	      (* Printf.printf "Missing var %s in counterexample\n" s;
	      List.iter (fun (s,v) -> Printf.printf "  %s = 0x%Lx\n" s v) ce;
	      failwith "eval_var_from_ce failed on missing value" *)
	    in
	      V.Constant(V.Int(ty, v))
	| _ ->
	    Printf.printf "Bad lvalue: %s\n" (V.lval_to_string lv);
	    failwith "Unhandled lvalue type in eval_var_from_ce"	    

    method eval_expr_from_ce ce e =
      let memo = Hashtbl.create 100 in
      let rec loop e =
	match e with
	  | V.BinOp(op, e1, e2) -> cf_eval (V.BinOp(op, loop e1, loop e2))
	  | V.UnOp(op, e1) -> cf_eval (V.UnOp(op, loop e1))
	  | V.Cast(op, ty, e1) -> cf_eval (V.Cast(op, ty, loop e1))
	  | V.Constant(V.Int(_, _)) -> e
	  | V.Lval(V.Temp(n,s,t))
	      when Hashtbl.mem temp_var_num_to_subexpr n ->
	      (try Hashtbl.find memo n
	       with
		 | Not_found ->
		     let (e_enc, _) = Hashtbl.find temp_var_num_to_subexpr n in
		     let e' = loop (decode_exp e_enc)
		     in
		       Hashtbl.replace memo n e';
		       e')
	  | V.Lval(V.Mem(_, _, _)) -> loop (self#rewrite_mem_expr e)
	  | V.Lval(V.Temp(memvar))
	      when V.VarHash.mem mem_axioms memvar
		->
	      loop (V.VarHash.find mem_axioms memvar)
	  | V.Lval(lv) -> self#eval_var_from_ce ce lv
	  | V.Ite(ce, te, fe) -> cf_eval (V.Ite(loop ce, loop te, loop fe))
	  | V.Let(_, _, _)
	  | V.Unknown(_)
	  | V.Name(_)
	  | V.Constant(V.Str(_))
	    ->
	      Printf.printf "Can't evaluate %s\n" (V.exp_to_string e);
	      failwith "Unexpected expr in eval_expr_from_ce"
      in
	match loop e with
	  | V.Constant(V.Int(_, i64)) -> i64
	  | e ->
	      Printf.printf "Left with %s\n" (V.exp_to_string e);
	      failwith "Constant invariant failed in eval_expr"
      

    val temp_var_num_has_loop_var = Hashtbl.create 1001

    method has_loop_var d = 
      let rec loop e =
	match e with
	  | V.BinOp(op, e1, e2) -> (loop e1) || (loop e2)
	  | V.UnOp(op, e1) -> loop e1
	  | V.Cast(op, ty, e1) -> loop e1
	  | V.Lval(V.Mem(_, _, _)) -> false
	  | V.Constant(V.Int(_, _)) -> false
	  | V.Lval(V.Temp(n,s,t))
	      when Hashtbl.mem temp_var_num_to_subexpr n ->
	      (try Hashtbl.find temp_var_num_has_loop_var n
	       with
		 | Not_found ->
		     let (e_enc, _) = Hashtbl.find temp_var_num_to_subexpr n in
		     let b = loop (decode_exp e_enc)
		     in
		       Hashtbl.replace temp_var_num_has_loop_var n b;
		       b)
	  | V.Lval(V.Temp(_,s,_)) ->
	      String.length s >= 3 && String.sub s 0 3 = "LTC"
	  | V.Ite(ce, te, fe) -> (loop ce) || (loop te) || (loop fe)
	  | V.Let(_, _, _)
	  | V.Unknown(_)
	  | V.Name(_)
	  | V.Constant(V.Str(_))
	    ->
	      failwith "Unexpected expr in has_loop_var"
      in
	loop (D.to_symbolic_32 d)

    val use_weak = false
    val temp_vars_weak = VarWeak.create 1001
    val temp_vars_unweak = Hashtbl.create 1001

    method private lookup_temp_var (temp_num, var_num, ty) =
      let var = (var_num, "t" ^ (string_of_int temp_num), ty) in
	if use_weak then
	  VarWeak.find temp_vars_weak var
	else
	  Hashtbl.find temp_vars_unweak var

    method private make_temp_var e ty =
      let cleanup_temp_var (n, s, t) =
	let (e_enc, _) = Hashtbl.find temp_var_num_to_subexpr n in
	  Hashtbl.remove temp_var_num_to_subexpr n;
	  Hashtbl.remove subexpr_to_temp_var_info e_enc;
	  Frag_marshal.free_var (n,s,t)
      in
      let (e_enc, used_vars) = encode_exp e in
	try
	  self#lookup_temp_var
	    (Hashtbl.find subexpr_to_temp_var_info e_enc)
	with Not_found ->
	  let temp_num = temp_var_num in
	  let s = "t" ^ (string_of_int temp_num) in
	    temp_var_num <- temp_var_num + 1;
	    let var = V.newvar s ty in
	    let (var_num,_,_) = var in
	    let var_info = (temp_num, var_num, ty) in
	      Gc.finalise cleanup_temp_var var;
 	      Hashtbl.replace subexpr_to_temp_var_info e_enc
		var_info;
 	      Hashtbl.replace temp_var_num_to_subexpr var_num
		(e_enc, used_vars);
	      if use_weak then
		VarWeak.add temp_vars_weak var
	      else
		Hashtbl.add temp_vars_unweak var var;
	      if !opt_trace_temps_encoded then
		Printf.printf "%s = %s\n" s (encode_printable_exp e);
	      if !opt_trace_temps then
		Printf.printf "%s = %s\n" s (V.exp_to_string e);
	      var

    method private simplify (v:D.t) ty =
      D.inside_symbolic
	(fun e ->
	   let e' = simplify_fp e in
	     (* We're supposed to simplify expressions as we build
		them, so something is going wrong if they get way to big
		at once: *)
	     (* assert(expr_size e' < 1000); *)
	     if expr_size e' < 10 then
	       e'
	     else
	       V.Lval(V.Temp(self#make_temp_var e' ty))
	) v

    method private tempify (v:D.t) ty =
      D.inside_symbolic
	(fun e ->
	   let e' = simplify_fp e in
	     V.Lval(V.Temp(self#make_temp_var e' ty))
	) v

    method simplify1  e = self#simplify e V.REG_1
    method simplify8  e = self#simplify e V.REG_8
    method simplify16 e = self#simplify e V.REG_16
    method simplify32 e = self#simplify e V.REG_32
    method simplify64 e = self#simplify e V.REG_64

    method simplify_with_callback f (v:D.t) ty =
      D.inside_symbolic
	(fun e ->
	   let e2 = simplify_fp e in
	     match e2 with
	       | V.Constant(_) -> e2
	       | _ -> 
		   (match (f e2 ty) with
		      | Some e3 -> e3
		      | None ->
			  (if expr_size e2 < 10 then
			     e2
			   else
			     V.Lval(V.Temp(self#make_temp_var e2 ty))))
	) v

    method make_ite cond_v ty v_true v_false =
      let cond_v'  = self#tempify  cond_v  V.REG_1 and
	  v_true'  = self#simplify v_true  ty      and
	  v_false' = self#simplify v_false ty
      in
	if v_true' = v_false' then
	  v_true'
	else
	  let func =
	    match ty with
	      | V.REG_1  -> D.ite1
	      | V.REG_8  -> D.ite8
	      | V.REG_16 -> D.ite16
	      | V.REG_32 -> D.ite32
	      | V.REG_64 -> D.ite64
	      | _ -> failwith "Unexpected type in make_ite"
	  in
	    func cond_v' v_true' v_false'

    method private lookup_tree e bits ty expr_list =
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
	    self#make_ite (D.from_symbolic cond_e) ty
	      (self#lookup_tree e (bits - 1) ty half_two)
	      (self#lookup_tree e (bits - 1) ty expr_list)


    val table_trees_cache = Hashtbl.create 101

    method private table_lookup_cached table table_num idx_exp0 idx_wd ty =
      let idx_ty = Vine_typecheck.infer_type_fast idx_exp0 in
      let idx_v = self#tempify (D.from_symbolic idx_exp0) idx_ty in
      let idx_exp = match idx_ty with
	| V.REG_1  -> D.to_symbolic_1  idx_v
	| V.REG_8  -> D.to_symbolic_8  idx_v
	| V.REG_16 -> D.to_symbolic_16 idx_v
	| V.REG_32 -> D.to_symbolic_32 idx_v
	| V.REG_64 -> D.to_symbolic_64 idx_v
	| _ -> failwith "Unexpected type in make_table_lookup"
      in
      let remake =
	let v = self#lookup_tree idx_exp idx_wd ty table in
	let v' = self#tempify v ty
	in
	  if !opt_trace_tables then
	    (Printf.printf "Select from table %d at %s is %s\n"
	       table_num (V.exp_to_string idx_exp) (D.to_string_64 v');
	     flush stdout);
	  if table_num <> -1 then
	    Hashtbl.replace table_trees_cache (table_num, idx_exp) v';
	  v'
      in
	try
	  let v =
	    if table_num = -1 then
	      remake
	    else
	      (let v = Hashtbl.find table_trees_cache (table_num, idx_exp) in
		 Printf.printf "Hit table cache\n";
		 v)
	  in
	    if !opt_trace_tables then
	      (Printf.printf "Select from table %d at %s is %s\n"
		 table_num (V.exp_to_string idx_exp) (D.to_string_64 v);
	       flush stdout);
	    v
	with
	  | Not_found ->
	      remake

    val tables = Hashtbl.create 101
    val tables_cache_limit = 1000000L

    method private save_table table ty =
      let num_cached = Int64.of_int (Hashtbl.length tables) and
	  size = Int64.of_int (List.length table)
      in
      let prod = Int64.mul num_cached size
      in
	if prod > tables_cache_limit then
	  (-1, true)
	else
	  try
	    (Hashtbl.find tables table, false)
	  with
	    | Not_found ->
		let i = Hashtbl.length tables in
		  Hashtbl.replace tables table i;
		  (i, true)

    method private print_table table ty i =
      Printf.printf "Table %d is: " i;
      let cnt = ref 0 in
	List.iter
	  (fun v ->
	     incr cnt;
	     if !cnt < 2048 (* 100 *) then
	       Printf.printf "%s "
		 (match ty with
		    | V.REG_1  -> D.to_string_1  v
		    | V.REG_8  -> D.to_string_8  v
		    | V.REG_16 -> D.to_string_16 v
		    | V.REG_32 -> D.to_string_32 v
		    | V.REG_64 -> D.to_string_64 v
		    | _ -> failwith "Can't happen"))
	  table;
	if !cnt > 2048 then
	  Printf.printf "...";
	Printf.printf "\n"

    (* Bitvectors can be seen as vectors or polynomials over the field
       GF(2), which consists of 0 and 1 with XOR as addition and AND as
       multiplication. In this setting, a function is linear if f(a XOR
       b) = f(a) XOR f(b). Functions that are linear in this sense are
       used in a number of places in error-correcting codes and
       symmetric-key cryptography. Some bitvector SMT solvers can reason
       effectively over the properties of such functions because their
       SAT solvers can do bit-level gaussian elimination. However these
       operations are often implemented in software using lookup tables,
       which can keep the solvers from recognizing the linear
       structure. So here we do the conversion for them, converting
       lookup tables that happen to implement a linear function into a
       more directly linear expression. Table lookup is equivalent to
       taking the XOR for one value for each 1 bit in the index. *)
    method private table_check_gf2 table idx_wd ty =
      let build_from_spine spine n =
	let rec loop x l n =
	  match l with
	    | [] -> x
	    | m :: rest ->
		let next =
		  if (n land 1) = 1 then
		    (Int64.logxor x m)
		  else
		    x
		in
		  loop next rest (n lsr 1)
	in
	  loop 0L spine n
      in
      let rec check_loop spine table =
	let rec loop l i =
	  match l with
	    | [] -> true
	    | x :: rest ->
		let x' = build_from_spine spine i in
		  if x <> x' then
		    false
		  else
		    loop rest (i + 1)
	in
	  loop table 0
      in
      let conc v =
	match ty with
	  | V.REG_1  -> Int64.of_int (D.to_concrete_1  v)
	  | V.REG_8  -> Int64.of_int (D.to_concrete_8  v)
	  | V.REG_16 -> Int64.of_int (D.to_concrete_16 v)
	  | V.REG_32 -> D.to_concrete_32 v
	  | V.REG_64 -> D.to_concrete_64 v
	  | _ -> failwith "Unexpected type in table_check_gf2"
      in
	try
	  assert(List.length table = (1 lsl idx_wd));
	  let table_conc = List.map conc table in
	  let spine = Vine_util.mapn
	    (fun i -> List.nth table_conc (1 lsl i)) (idx_wd - 1)
	  in
	    assert(List.length spine = idx_wd);
	    if check_loop spine table_conc then
	      (if !opt_trace_tables then
		 (Printf.printf "Detected GF(2) linear operator with spine:\n";
		  List.iter (fun n -> Printf.printf " 0x%Lx" n) spine;
		  Printf.printf "\n");
	       Some spine)
	    else
	      None
	with
	  | NotConcrete(_) -> None

    method private make_gf2_operator spine ty idx_exp0 =
      let rec term_loop l e n =
	match l with
	  | [] -> []
	  | m :: rest ->
	      let shift_amt = Int64.of_int n in
	      let bit_e = V.Cast(V.CAST_LOW, V.REG_1,
				 V.BinOp(V.RSHIFT, e,
					 V.Constant(V.Int(V.REG_8,
							  shift_amt))))
	      in
	      let wide_e = V.Cast(V.CAST_SIGNED, ty, bit_e) in
	      let term = V.BinOp(V.BITAND, wide_e,
				 V.Constant(V.Int(ty, m)))
	      in
		term :: (term_loop rest e (n+1))
      in
      let idx_ty = Vine_typecheck.infer_type_fast idx_exp0 in
      let idx_v = self#tempify (D.from_symbolic idx_exp0) idx_ty in
      let idx_exp = match idx_ty with
	| V.REG_1  -> D.to_symbolic_1  idx_v
	| V.REG_8  -> D.to_symbolic_8  idx_v
	| V.REG_16 -> D.to_symbolic_16 idx_v
	| V.REG_32 -> D.to_symbolic_32 idx_v
	| V.REG_64 -> D.to_symbolic_64 idx_v
	| _ -> failwith "Unexpected type in make_gf2_operator"
      in
      let terms = term_loop spine idx_exp 0 in
      let e = xorjoin terms in
      let v = D.from_symbolic e in
      let v' = self#tempify v ty in
	if !opt_trace_tables then
	  (Printf.printf "Select from GF(2) table at %s is %s\n"
	     (V.exp_to_string idx_exp) (D.to_string_64 v');
	   flush stdout);
	v'

    method make_table_lookup table idx_exp idx_wd ty =
      let (table_num, is_new) = self#save_table table ty in
	if !opt_trace_tables then
	  (if table_num = -1 then
	     Printf.printf " is uncached\n"
	   else
	     Printf.printf " is table %d\n" table_num);
	match self#table_check_gf2 table idx_wd ty with
	  | Some spine ->
	      self#make_gf2_operator spine ty idx_exp
	  | None ->
	      let v =
		self#table_lookup_cached table table_num idx_exp idx_wd ty
	      in
		if is_new && !opt_trace_tables then
		  self#print_table table ty table_num;
		v

    method if_expr_temp_unit (n,_,_) (fn_t: V.exp option  -> unit) =
      (* The slightly weird structure here is because we *don't*
	 want to catch a Not_found thrown by decode_exp. *)
      match
	(try
	   let (e_enc, _) = Hashtbl.find temp_var_num_to_subexpr n in
	     Some e_enc
	 with Not_found -> None)
      with
	| Some e_enc -> (fn_t (Some(decode_exp e_enc)))
	| None -> (fn_t None)
	
    (* This was originally designed to be polymorphic in the return
       type of f, and could be made so again as with if_expr_temp *)
    method walk_temps (f : (V.var -> V.exp -> (V.var * V.exp))) exp =
      let h = V.VarHash.create 21 in
      let temps = ref [] in
      let nontemps_h = V.VarHash.create 21 in
      let nontemps = ref [] in
      let rec walk = function
	| V.BinOp(_, e1, e2) -> walk e1; walk e2
	| V.UnOp(_, e1) -> walk e1
	| V.Constant(_) -> ()
	| V.Lval(V.Temp(var)) ->
	    if not (V.VarHash.mem h var) then
	      (let fn_t = (fun e ->
			     V.VarHash.replace h var ();
			     walk e;
			     temps := (f var e) :: !temps) in
	       let else_fn =
		 (fun v -> (* v is not a temp *)
		    if not (V.VarHash.mem nontemps_h var) then
		      (V.VarHash.replace nontemps_h var ();
		       nontemps := var :: !nontemps)) in
		 if_expr_temp self var fn_t () else_fn)
	| V.Lval(V.Mem(_, e1, _)) -> walk e1
	| V.Name(_) -> ()
	| V.Cast(_, _, e1) -> walk e1
	| V.Unknown(_) -> ()
	| V.Let(_, e1, e2) -> walk e1; walk e2 
	| V.Ite(ce, te, fe) -> walk ce; walk te; walk fe
      in
	walk exp;
	((List.rev !nontemps), (List.rev !temps))

    method collect_for_solving u_temps conds val_e =
      let val_expr = self#rewrite_for_solver val_e in
      let cond_expr = self#rewrite_for_solver
	(conjoin (List.rev conds)) in
      let (nts1, ts1) = self#walk_temps (fun var e -> (var, e)) cond_expr in
      let (nts2, ts2) = self#walk_temps (fun var e -> (var, e)) val_expr in
      let (nts3, ts3) = List.fold_left 
	(fun (ntl, tl) (lhs, rhs) ->
	   let (nt, t) = self#walk_temps (fun var e -> (var, e)) rhs in
	     (nt @ ntl, t @ tl))
	([], []) u_temps in
      let temps = 
	List.map (fun (var, e) -> (var, self#rewrite_for_solver e))
	  (list_unique (ts1 @ ts2 @ ts3 @ u_temps)) in
      let i_vars =
	list_unique (nts1 @ nts2 @ nts3 @ self#get_mem_bytes) in
      let m_axioms = self#get_mem_axioms in
      let m_vars = List.map (fun (v, _) -> v) m_axioms in
      let assigns = m_axioms @ temps in
      let decls = Vine_util.list_difference i_vars m_vars in
      let inputs_in_val_expr = i_vars 
      in
	(decls, assigns, cond_expr, val_expr, inputs_in_val_expr)

    method one_cond_for_solving cond seen_hash =
      let (all_decls, all_assigns, cond_expr) =
	self#with_saved_mem_axioms
	  (fun _ ->
	     let cond_expr = self#rewrite_for_solver cond in
	     let (nts, ts) =
	       self#walk_temps (fun var e -> (var, e)) cond_expr in
	     let temps =
	       List.map
		 (fun (var, e) -> (var, self#rewrite_for_solver e)) ts in
	     let i_vars = (list_unique (nts @ self#get_mem_bytes)) in
	     let m_axioms = self#get_mem_axioms in
	     let m_vars = List.map (fun (v, _) -> v) m_axioms in
	     let assigns = m_axioms @ temps in
	     let decls = Vine_util.list_difference i_vars m_vars in
	       (decls, assigns, cond_expr))
      in
      let new_decls = List.filter
	(fun v -> not (V.VarHash.mem seen_hash v)) all_decls in
      let new_assigns = List.filter
	(fun (v,_) -> not (V.VarHash.mem seen_hash v)) all_assigns in
      let new_vars = new_decls @ (List.map (fun (v,_) -> v) new_assigns) in
	List.iter (fun v -> V.VarHash.replace seen_hash v ()) new_vars;
	(new_decls, new_assigns, cond_expr, new_vars)

    method measure_size =
      let (input_ents, input_nodes) =
	(Hashtbl.length input_vars, Hashtbl.length input_vars) in
      let (rb_ents, rb_nodes) =
	(Hashtbl.length region_base_vars, Hashtbl.length region_base_vars) in
      let (rg_ents, rg_nodes) =
	(Hashtbl.length region_vars, Hashtbl.length region_vars) in
      let sc_ents = Hashtbl.length seen_concolic in
      let (bv_ents, bv_nodes) =
	(Hashtbl.length valuation, Hashtbl.length valuation) in
      let (se2t_ents, se2t_nodes) = 
	(Hashtbl.length subexpr_to_temp_var_info,
	 Hashtbl.length subexpr_to_temp_var_info) in
      let mbv_ents = V.VarHash.length mem_byte_vars in
      let sum_expr_sizes k v sum = sum + expr_size v in
      let (ma_ents, ma_nodes) =
	(V.VarHash.length mem_axioms,
	 V.VarHash.fold sum_expr_sizes mem_axioms 0) in
      let sum_lengths k (v,_) sum = sum + String.length v in
      let (t2se_ents, t2se_bytes) =
	(Hashtbl.length temp_var_num_to_subexpr,
	 Hashtbl.fold sum_lengths temp_var_num_to_subexpr 0) in
      let te_ents = Hashtbl.length temp_var_num_evaled in
      let tw_ents = VarWeak.count temp_vars_weak in
      let tu_ents = Hashtbl.length temp_vars_unweak in
      let ttree_ents = Hashtbl.length table_trees_cache in
      let ttree_nodes = Hashtbl.fold
	(fun _ v s -> s + D.measure_size v)
	table_trees_cache 0
      in
      let tables_ents = Hashtbl.length tables in
      let tables_nodes = Hashtbl.fold
	(fun k _ s ->
	   s + List.length k) tables 0
      in
	Printf.printf "input_vars has %d entries\n" input_ents;
	Printf.printf "region_base_vars has %d entries\n" rb_ents;
	Printf.printf "region_vars has %d entries\n" rg_ents;
	Printf.printf "seen_concolic has %d entries\n" sc_ents;
	Printf.printf "valuation has %d entries\n" bv_ents;
	Printf.printf "subexpr_to_temp_var has %d entries\n" se2t_ents;
	Printf.printf "mem_byte_vars has %d entries\n" mbv_ents;
	Printf.printf "mem_axioms has %d entries and %d nodes\n"
	  ma_ents ma_nodes;
	Printf.printf "temp_var_num_to_subexpr has %d entries and %d bytes\n"
	  t2se_ents t2se_bytes;
	Printf.printf "temp_vars_weak has %d entries\n" tw_ents;
	Printf.printf "temp_vars_unweak has %d entries\n" tu_ents;
	Printf.printf "table_trees_cache has %d entries and %d nodes\n"
	  ttree_ents ttree_nodes;
	Printf.printf "tables has %d entries and %d nodes\n"
	  tables_ents tables_nodes;
	(input_ents + rb_ents + rg_ents + sc_ents + bv_ents + se2t_ents +
	   mbv_ents + ma_ents + t2se_ents + te_ents + ttree_ents + tables_ents,
	 input_nodes + rb_nodes + rg_nodes + bv_nodes + se2t_nodes +
	   ma_nodes + ttree_nodes + tables_nodes)
  end
end

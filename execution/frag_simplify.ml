(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module V = Vine;;

open Exec_exceptions;;

let rec constant_fold_rec e =
  match e with
    | V.BinOp(op, e1, e2) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.BinOp(op, constant_fold_rec e1, constant_fold_rec e2))
    | V.UnOp(op, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.UnOp(op, constant_fold_rec e))
    | V.Cast(op, ty, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.Cast(op, ty, constant_fold_rec e))
    | V.Lval(lv) -> V.Lval(constant_fold_rec_lv lv)
    | _ -> e
and constant_fold_rec_lv lv =
  match lv with
    | V.Mem(v, e, ty) -> V.Mem(v, constant_fold_rec e, ty)
    | _ -> lv

let rec simplify_rec e =
  match e with
    | V.BinOp(V.EQ, V.BinOp(V.PLUS, e1, (V.Constant(_) as c)),
	      V.Constant(V.Int(ty, 0L))) ->
	V.BinOp(V.EQ, (simplify_rec e1), (simplify_rec (V.UnOp(V.NEG, c))))
    | V.BinOp(op, e1, e2) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.BinOp(op, simplify_rec e1, simplify_rec e2))
    | V.UnOp(op, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.UnOp(op, simplify_rec e))
    | V.Cast(op, ty, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.Cast(op, ty, simplify_rec e))
    | V.Lval(lv) -> V.Lval(simplify_rec_lv lv)
    | _ -> e
and simplify_rec_lv lv =
  match lv with
    | V.Mem(v, e, ty) -> V.Mem(v, simplify_rec e, ty)
    | _ -> lv

let rec expr_size e =
  match e with
    | V.BinOp(_, e1, e2) -> 1 + (expr_size e1) + (expr_size e2)
    | V.UnOp(_, e1) -> 1 + (expr_size e1)
    | V.Constant(_) -> 1
    | V.Lval(V.Temp(_)) -> 2
    | V.Lval(V.Mem(_, e1, _)) -> 2 + (expr_size e1)
    | V.Name(_) -> 1
    | V.Cast(_, _, e1) -> 1 + (expr_size e1)
    | V.Unknown(_) -> 1
    | V.Let(V.Temp(_), e1, e2) -> 2 + (expr_size e1) + (expr_size e2)
    | V.Let(V.Mem(_,e0,_), e1, e2) ->
	2 + (expr_size e0) + (expr_size e1) + (expr_size e2)

let rec stmt_size = function
  | V.Jmp(e) -> 1 + (expr_size e)
  | V.CJmp(e1, e2, e3) -> 1 + (expr_size e1) + (expr_size e2) + (expr_size e3)
  | V.Move(lv, e) -> 1 + (expr_size (V.Lval(lv))) + (expr_size e)
  | V.Special(_) -> 1
  | V.Label(_) -> 1
  | V.ExpStmt(e) -> 1 + (expr_size e)
  | V.Comment(_) -> 1
  | V.Block(dl, sl) -> 1 + (List.length dl) +
      (List.fold_left (+) 0 (List.map stmt_size sl))
  | V.Function(_, _, dl, _, st_o) ->
      3 + (List.length dl) +
	(match st_o with None -> 0 | Some st -> stmt_size st)
  | V.Return(None) -> 1
  | V.Return(Some e) -> (1+ expr_size e)
  | V.Call(lv_o, e1, e_l) -> 1 +
      (match lv_o with None -> 0 | Some(lv) -> expr_size (V.Lval(lv)))
      + (expr_size e1) + (List.fold_left (+) 0 (List.map expr_size e_l))
  | V.Attr(st, _) -> 1 + (stmt_size st)
  | V.Assert(e) -> 1 + (expr_size e)
  | V.Halt(e) -> 1 + (expr_size e)

let cfold_exprs (dl, sl) =
  let sl' = List.map
    (fun st -> match st with
       | V.CJmp(e, l1, l2) -> V.CJmp(constant_fold_rec e, l1, l2)
       | V.Move(lval, e) ->
	   V.Move(constant_fold_rec_lv lval, constant_fold_rec e)
       | V. ExpStmt(e) -> 
	   V.ExpStmt(constant_fold_rec e)
       | _ -> st)
    sl
  in
    (dl, sl')

let rec cfold_with_type e =
  match e with
    | V.UnOp(op, e1) ->
	let (e1', ty1) = cfold_with_type e1 in
	  (V.UnOp(op, e1'), ty1)
    | V.BinOp(op, e1, e2) ->
	let (e1', ty1) = cfold_with_type e1 and
	    (e2', ty2) = cfold_with_type e2 in
	let ty =
	  (match op with
	     | V.PLUS | V.MINUS | V.TIMES
	     | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
	     | V.BITAND | V.BITOR | V.XOR
		 -> assert(ty1 = ty2); ty1
	     | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		 -> ty1
	     | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		 -> assert(ty1 = ty2); V.REG_1)
	in
	  (V.BinOp(op, e1', e2'), ty)
    | V.Constant(V.Int(ty, _)) -> (e, ty)
    | V.Constant(V.Str(_)) -> (e, V.TString)
    | V.Lval(V.Temp(_,_,ty)) -> (e, ty)
    | V.Lval(V.Mem(mem,e1,ty)) ->
	let (e1', _) = cfold_with_type e1 in
	  (V.Lval(V.Mem(mem,e1',ty)), ty)
    | V.Name(_) -> (e, V.addr_t)
    | V.Cast(kind, ty, e1) ->
	let (e1', ty1) = cfold_with_type e1 in
	let e' = if ty = ty1 then e1' else V.Cast(kind, ty, e1') in
	  (e', ty)
    | V.Unknown("rdtsc") -> (e, V.REG_64)
    | V.Unknown("Unknown: Dirty") ->
	raise IllegalInstruction
    | V.Unknown(s) ->
	raise (Simplify_failure ("unhandled unknown "^s^" in cfold_with_type"))
    | V.Let(V.Temp(t), e1, e2) ->
	let (e1', _) = cfold_with_type e1 and
	    (e2', ty2) = cfold_with_type e2 in
	  (V.Let(V.Temp(t), e1', e2), ty2)
    | V.Let(V.Mem(mem,e0,mem_ty), e1, e2) ->
	let (e0', _) = cfold_with_type e0 and
	    (e1', _) = cfold_with_type e1 and
	    (e2', ty2) = cfold_with_type e2 in
	  (V.Let(V.Mem(mem,e0',mem_ty), e1', e2), ty2)

let cfold_exprs_w_type (dl, sl) =
  let fold e =
    match cfold_with_type e with (e', _) -> e'
  in
  let sl' = List.map
    (fun st -> match st with
       | V.CJmp(e, l1, l2) -> V.CJmp(fold e, l1, l2)
       | V.Move(V.Temp(v), e) -> V.Move(V.Temp(v), fold e)
       | V.Move(V.Mem(mem, e1, ty), e2) ->
	   V.Move(V.Mem(mem, fold e1, ty), fold e2)
       | V. ExpStmt(e) -> V.ExpStmt(fold e)
       | _ -> st)
    sl
  in
    (dl, sl')

let rm_unused_stmts sl =
  List.filter
    (fun st -> match st with
       | V.Special("call") -> false
       | V.Special("ret") -> false
       | V.Move(V.Temp(_,"R_CC_OP",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP1",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP2",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_NDEP",_),_) -> false
       | V.Move(V.Temp(_,("R_PF"|"R_AF"),_),_)
	   when !Exec_options.opt_omit_pf_af -> false
       | _ -> true)
    sl

let uncond_jmps sl =
  List.map
    (fun st -> match st with
       | V.CJmp(V.Constant(V.Int(V.REG_1, 1L)), l1, l2) -> V.Jmp(l1)
       | V.CJmp(V.Constant(V.Int(V.REG_1, 0L)), l1, l2) -> V.Jmp(l2)
       | _ -> st
    ) sl

let rec rm_sequential_jmps sl =
  match sl with
    | V.Jmp(V.Name(lab1)) :: V.Label(lab2) :: rest when lab1 = lab2
	-> V.Label(lab2) :: rm_sequential_jmps rest
    | st :: rest -> st :: rm_sequential_jmps rest
    | [] -> []

let rm_unused_labels sl =
  let exp_labels e =
    match e with
      | V.Name(l) -> [l]
      | _ -> []
  in
  let used_labels = List.concat
    (List.map
       (fun st -> match st with
	  | V.Jmp(e) -> exp_labels e
	  | V.CJmp(cond, e1, e2) -> (exp_labels e1) @ (exp_labels e2)
	  | _ -> [])
       sl)
  in
  let is_eip l = (String.length l > 5) && (String.sub l 0 5) = "pc_0x" in
    (* List.iter print_string used_labels; *)
    List.filter
      (fun st -> match st with
	 | V.Label(l) when not (List.mem l used_labels) &&
	     (not (is_eip l))
	     -> false
	 | _ -> true)
      sl

let rec count_uses_e var e =
  match e with
    | V.BinOp(_, e1, e2) -> (count_uses_e var e1) + (count_uses_e var e2)
    | V.UnOp(_, e) -> count_uses_e var e
    | V.Cast(_, _, e) -> count_uses_e var e
    | V.Lval(lv) -> count_uses_lv var lv
    | V.Let(lv, e1, e2) ->
	(count_uses_lv var lv) + (count_uses_e var e1) + (count_uses_e var e2)
    | _ -> 0
and count_uses_lv var lv =
  match lv with
    | V.Temp(v) -> (if v = var then 1 else 0)
    | V.Mem(v, e, _) -> (if v = var then 1 else 0) + (count_uses_e var e)

let count_temp_uses (dl, sl) =
  let count_uses_sl var =
    List.fold_left (+) 0
      (List.map 
	 (fun st -> match st with
	    | V.CJmp(e1, l1, l2) -> 
		(count_uses_e var e1) + (count_uses_e var l1)
		+ (count_uses_e var l2)
	    | V.Jmp(lab) -> count_uses_e var lab
	    | V.Move(V.Temp(var1), e) when var1 = var
		-> (count_uses_e var e)
	    | V.Move(lval, e) -> (count_uses_lv var lval)
		+ (count_uses_e var e)
	    | V.ExpStmt(e) -> count_uses_e var e
	    | V.Assert(e) -> count_uses_e var e
	    | _ -> 0)
	 sl)
  in
    List.map count_uses_sl dl

let rm_unused_vars (dl, sl) =
  let rm_defs var sl =
    List.filter
      (fun st -> match st with
	 | V.Move(V.Temp(v), _) when v = var -> false
	 | _ -> true)
      sl
  in
  let rec partition2 pred l1 l2 =
    match (l1, l2) with
      | ((item :: r1), (obj :: r2)) ->
	  let (rt, rf) = partition2 pred r1 r2 in
	  if pred obj then
	    (item :: rt, rf)
	  else
	    (rt, item :: rf)
      | ([], []) -> ([], [])
      | _ -> failwith "Mismatched lengths in partition2"
  in
  let uses = count_temp_uses (dl, sl) in
    (* print_string "{";
       List.iter print_int uses;
       print_string "}\n"; *)
  let (dl', to_rm) = partition2 (fun n -> n != 0) dl uses in
  let sl' = List.fold_right rm_defs to_rm sl in
    (dl', sl')

let copy_const_prop (dl, sl) =
  let map = V.VarHash.create 10 in
  let use_counts = List.fold_left2 
    (fun hash key value -> V.VarHash.add hash key value; hash)
    (V.VarHash.create 10) dl (count_temp_uses (dl, sl)) in
  let rec replace_uses_e expr =
    match expr with
      | V.BinOp(op, e1, e2) ->
	  V.BinOp(op, replace_uses_e e1, replace_uses_e e2)
      | V.UnOp(op, e) ->
	  V.UnOp(op, replace_uses_e e)
      | V.Cast(op, ty, e) ->
	  V.Cast(op, ty, replace_uses_e e)
      | V.Lval(V.Temp(v)) when V.VarHash.mem map v ->
	  V.VarHash.find map v
      | V.Lval(V.Mem(v, idx, ty)) ->
	  V.Lval(V.Mem(v, replace_uses_e idx, ty))
      | V.Let(V.Temp(v), e1, e2) ->
	  assert(not (V.VarHash.mem map v));
	  V.Let(V.Temp(v), (replace_uses_e e1), (replace_uses_e e2))
      | V.Let(V.Mem(v, idx, mem_ty), e1, e2) ->
	  assert(not (V.VarHash.mem map v));
	  V.Let(V.Mem(v, (replace_uses_e idx), mem_ty),
		(replace_uses_e e1), (replace_uses_e e2))
      | _ -> expr
  in
  let replace_uses st =
    match st with
      | V.CJmp(e1, l1, l2) -> 
	  V.CJmp(replace_uses_e e1, replace_uses_e l1, replace_uses_e l2)
      | V.Jmp(lab) ->
	  V.Jmp(replace_uses_e lab)
      | V.Move(V.Temp(var), e) -> 
	  V.Move(V.Temp(var), replace_uses_e e)
      | V.Move(V.Mem(var, idx, ty), e) -> 
	  V.Move(V.Mem(var, replace_uses_e idx, ty), replace_uses_e e)
      | V.ExpStmt(e) ->
	  V.ExpStmt(replace_uses_e e)
      | V.Assert(e) ->
	  V.Assert(replace_uses_e e)
      | _ -> st
  in
  let find_or_zero vh key = try V.VarHash.find vh key with Not_found -> 0
  in
  let invalidate_uses vh bad_var =
    V.VarHash.iter
      (fun lhs rhs ->
	 if (count_uses_e bad_var rhs) != 0 then	   
	   (V.VarHash.remove vh lhs))
      vh
  in
  let rec loop sl =
    match sl with
      | [] -> []
      | st :: rest ->
	  let st' = replace_uses st in
	    (match st' with
	       | V.Move(V.Temp(lhs_v), rhs)
		 -> invalidate_uses map lhs_v;
		   V.VarHash.remove map lhs_v;
	       | V.Move(V.Mem(var, _, _), rhs)
		 -> invalidate_uses map var;
	       | _ -> ());
	    (match st' with
	       | V.CJmp(_, _, _)
	       | V.Jmp(_)
	       | V.Label(_)
		 -> V.VarHash.clear map
	       | V.Move(V.Temp(lhs_v), V.Lval(V.Temp(rhs_v)))
		 -> V.VarHash.replace map lhs_v (V.Lval(V.Temp(rhs_v)));
		   (if List.mem rhs_v dl then
		      V.VarHash.replace map rhs_v (V.Lval(V.Temp(lhs_v))))
	       | V.Move(V.Temp(lhs_v), rhs)
		   when (find_or_zero use_counts lhs_v) = 1
		     -> V.VarHash.replace map lhs_v rhs
	       | V.Move(V.Temp(lhs_v), (V.Constant(_) as const))
		   -> V.VarHash.replace map lhs_v const
	       | _ -> ());
	    st' :: (loop rest)
  in
    (dl, loop sl)

let rec rm_set_used_e hash e =
  match e with
    | V.BinOp(_, e1, e2) -> (rm_set_used_e hash e1); (rm_set_used_e hash e2)
    | V.UnOp(_, e) -> rm_set_used_e hash e
    | V.Cast(_, _, e) -> rm_set_used_e hash e
    | V.Lval(lv) -> rm_set_used_lv hash lv
    | V.Let(lv, e1, e2) ->
	(rm_set_used_lv hash lv) ;
	(rm_set_used_e hash e1);
	(rm_set_used_e hash e2)
    | _ -> ()
and rm_set_used_lv hash lv =
  match lv with
    | V.Temp(v) -> Hashtbl.remove hash v
    | V.Mem(v, e, _) -> Hashtbl.remove hash v; (rm_set_used_e hash e)

let rm_dead_assigns (dl, sl) =
  ignore(dl);
  let dead = Hashtbl.create 31 in
  let rec loop sl_r =
    match sl_r with
      | [] -> []
      | st :: rest ->
	  match st with
	    | V.Move(V.Temp(lhs_v), rhs) ->
		if Hashtbl.mem dead lhs_v then
		  (loop rest)
		else
		  (Hashtbl.replace dead lhs_v ();
		   rm_set_used_e dead rhs;
		   st :: (loop rest))
	    | V.Move(V.Mem(_, addr_e, _), rhs) ->
		rm_set_used_e dead addr_e;
		rm_set_used_e dead rhs;
		st :: (loop rest)
	    | V.Jmp(_)
	    | V.CJmp(_, _, _)
	    | V.Label(_) ->
		Hashtbl.clear dead;
		st :: (loop rest)
	    | V.ExpStmt(e)
	    | V.Assert(e)
	    | V.Halt(e) ->
		rm_set_used_e dead e;
		st :: (loop rest)
	    | V.Block(dl1, sl1) ->
		let st' = V.Block(dl1, List.rev (loop (List.rev sl1))) in
		  st' :: (loop rest)
	    | V.Special(_)
	    | V.Comment(_) ->
		st :: (loop rest)
	    | _ -> failwith "Unhandled stmt type in rm_dead_assigns"
  in
    (dl, List.rev (loop (List.rev sl)))

let peephole_patterns (dl, sl) =
  let rec loop sl =
    match sl with
      (* No-op assignment *)
      | V.Move(V.Temp(r1_1), V.Lval(V.Temp(r1_2))) :: rest
	  when r1_1 = r1_2
	    -> loop rest
      (* Usually lets us avoid a temp in a push: *)
      | V.Move(V.Temp(t1_1), (V.BinOp(op, V.Lval(_), V.Constant(_)) as e1)) ::
	  V.Move(V.Temp(r1), V.Lval(V.Temp(t1_2))) ::
	  V.Move(V.Mem(mem, V.Lval(V.Temp(t1_3)), ty), e2) :: rest
	  when List.mem t1_1 dl && t1_1 = t1_2 && t1_2 = t1_3 &&
	    not (List.mem r1 dl)
	    ->
	  V.Move(V.Temp(t1_1), e1) ::
	    V.Move(V.Temp(r1), e1) ::
	    V.Move(V.Mem(mem, V.Lval(V.Temp(r1)), ty), e2) ::
	    loop rest
      (* Avoid a temp in a pop: *)
      | V.Move(V.Temp(t1_1),
	       (V.Lval(V.Mem(_, V.Lval(V.Temp(r1_1)), _)) as load)) ::
	  (V.Move(V.Temp(r1_2), V.BinOp(op, V.Lval(V.Temp(r1_3)),
					V.Constant(c)))
	     as update)::
	       V.Move(V.Temp(r2), V.Lval(V.Temp(t1_2))) :: rest
	       when List.mem t1_1 dl && t1_1 = t1_2 &&
		 not (List.mem r1_1 dl) && r1_1 = r1_2 && r1_2 = r1_3 &&
		 not (List.mem r2 dl)
	       ->
	  V.Move(V.Temp(t1_1), load) ::
	    V.Move(V.Temp(r2), load) ::
	    update ::
	    loop rest
      (* Make both target addrs visible in a Cjmp *)
      | V.CJmp(e, V.Name(l1), V.Name(l2)) ::
	  V.Label(l2') ::
	  V.Jmp(V.Name(l3)) :: rest
	  when l2 = l2' ->
	  V.CJmp(e, V.Name(l1), V.Name(l3)) :: (loop rest)
      | st :: rest -> st :: loop rest
      | [] -> []
  in
    (dl, loop sl)

let split_divmod (dl, sl) =
  let new_dl = ref [] in
  let vars = V.VarHash.create 5 in
  let rec walk = function
    | V.Cast(V.CAST_HIGH, V.REG_32, V.Lval(V.Temp(t1)))
	when V.VarHash.mem vars t1 ->
	let (_, mod_var) = V.VarHash.find vars t1 in
	  V.Lval(V.Temp(mod_var))
    | V.Cast(V.CAST_LOW, V.REG_32, V.Lval(V.Temp(t1)))
	when V.VarHash.mem vars t1 ->
	let (div_var, _) = V.VarHash.find vars t1 in
	  V.Lval(V.Temp(div_var))
    | V.BinOp(op, e1, e2) -> V.BinOp(op, (walk e1), (walk e2))
    | V.UnOp(op, e1) -> V.UnOp(op, (walk e1))
    | V.Let(lv, e1, e2) -> V.Let(lv, (walk e1), (walk e2))
    | other -> other
  in
  let rec stmt_loop sl =
      match sl with
	| V.Move(V.Temp((_, t1_str, V.REG_64) as t1),
		 V.BinOp(V.BITOR,
			 V.BinOp(V.LSHIFT,
				 V.Cast(V.CAST_UNSIGNED, V.REG_64,
					V.Cast(V.CAST_LOW, V.REG_32,
					       V.BinOp(V.SMOD, num1, den1))),
				 V.Constant(V.Int(V.REG_32, 32L))),
			 V.Cast(V.CAST_UNSIGNED, V.REG_64,
				V.Cast(V.CAST_LOW, V.REG_32,
				       V.BinOp(V.SDIVIDE, num2, den2)))))
	  :: rest
	    when num1 = num2 && den1 = den2 ->
	    let div_var = V.newvar (t1_str ^ "_div") V.REG_32 and
		mod_var = V.newvar (t1_str ^ "_mod") V.REG_32 in
	      V.VarHash.replace vars t1 (div_var, mod_var);
	      new_dl := div_var :: mod_var :: !new_dl;
	      (V.Move(V.Temp(div_var),
		      V.Cast(V.CAST_LOW, V.REG_32,
			     V.BinOp(V.SDIVIDE, num2, den2)))) ::
		(V.Move(V.Temp(mod_var),
			V.Cast(V.CAST_LOW, V.REG_32,
			       V.BinOp(V.SMOD, num1, den1)))) ::
		(stmt_loop rest)
	| V.Move(lv, rhs) :: rest ->
	    V.Move(lv, (walk rhs)) :: (stmt_loop rest)
	| V.ExpStmt(e) :: rest ->  V.ExpStmt(walk e) :: (stmt_loop rest)
	| V.Assert(e) :: rest -> V.Assert(walk e) :: (stmt_loop rest)
	| st :: rest -> st :: stmt_loop rest
	| [] -> []
  in
    (dl @ !new_dl, stmt_loop sl)

let lets_to_moves (dl, sl) =
  let rec expr_loop = function
    | V.BinOp(op, e1, e2) ->
	let (bl1, e1') = expr_loop e1 in
	let (bl2, e2') = expr_loop e2 in
	  (bl1 @ bl2, (V.BinOp(op, e1', e2')))
    | V.UnOp(op, e1) ->
	let (bl1, e1') = expr_loop e1 in
	  (bl1, V.UnOp(op, e1'))
    | V.Constant(_) as e -> ([], e)
    | V.Lval(V.Temp(_)) as e -> ([], e)
    | V.Lval(V.Mem(v, e1, ty)) ->
	let (bl1, e1') = expr_loop e1 in
	  (bl1, V.Lval(V.Mem(v, e1', ty)))
    | V.Name(_) as e -> ([], e)
    | V.Cast(ct, ty, e1) ->
	let (bl1, e1') = expr_loop e1 in
	  (bl1, (V.Cast(ct, ty, e1')))
    | V.Unknown(_) as e -> ([], e)
    | V.Let(V.Temp(v), e1, e2) ->
	let (bl1, e1') = expr_loop e1 in
	let (bl2, e2') = expr_loop e2 in
	  (bl1 @ [v, e1'] @ bl2, e2')
    | V.Let(V.Mem(_,_,_), _, _) ->
	failwith "Let mem unhandled in lets_to_moves"
  in
  let rec stmt_loop = function
    | st :: rest ->
	let (bl, st') = match st with
	  | V.Jmp(e1) ->
	      let (bl1, e1') = expr_loop e1 in
		(bl1, V.Jmp(e1'))
	  | V.CJmp(e1, e2, e3) ->
	      let (bl1, e1') = expr_loop e1 and
		  (bl2, e2') = expr_loop e2 and
		  (bl3, e3') = expr_loop e3 in
		(bl1 @ bl2 @ bl3, V.CJmp(e1', e2', e3'))
	  | V.Move((V.Temp(_) as lhs), e1) ->
	      let (bl1, e1') = expr_loop e1 in
		(bl1, V.Move(lhs, e1'))
	  | V.Move(V.Mem(v, e1, m_ty), e2) ->
	      let (bl1, e1') = expr_loop e1 and
		  (bl2, e2') = expr_loop e2 in
		(bl1 @ bl2, V.Move(V.Mem(v, e1', m_ty), e2'))
	  | V.Special(_) as st ->
	      ([], st)
	  | V.Label(_) as st ->
	      ([], st)
	  | V.ExpStmt(e1) ->
	      let (bl1, e1') = expr_loop e1 in
		(bl1, V.ExpStmt(e1'))
	  | V.Comment(_) as st ->
	      ([], st)
	  | V.Block(dl1, sl1) ->
	      let (dl1', sl1') = stmt_loop sl1 in
		([], V.Block(dl1 @ dl1', sl1))
	  | V.Assert(e1) ->
	      let (bl1, e1') = expr_loop e1 in
		(bl1, V.Assert(e1'))
	  | _ -> failwith "Unhandled stmt type in lets_to_moves"
	in
	let dl' = List.map (fun (a,_) -> a) bl in
	let sl' = List.map (fun (v,e) -> V.Move(V.Temp(v), e)) bl in
	let (dl2, sl2) = stmt_loop rest in
	  (dl' @ dl2, sl' @ (st' :: sl2))
    | [] -> ([], [])
  in
  let (dl', sl') = stmt_loop sl in
    (dl @ dl', sl')

let simplify_frag (orig_dl, orig_sl) =
  (* V.pp_program print_string (orig_dl, orig_sl); *)
  let (dl, sl) = (orig_dl, orig_sl) in
  let (dl, sl) = lets_to_moves (dl, sl) in
  let sl = rm_unused_stmts sl in
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
  let (dl, sl) = rm_dead_assigns (dl, sl) in
  let (dl, sl) = split_divmod (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in 
  let (dl, sl) = cfold_exprs_w_type (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = peephole_patterns (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in 
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in 
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl', sl') = (dl, sl) in
    (* Vine_typecheck.typecheck (dl' @ Asmir.x86_regs, sl'); *)
    (* V.pp_program print_string (orig_dl, orig_sl);
       V.pp_program print_string (dl', sl');
       V.pp_program print_string (copy_prop (dl', sl')); *)
    (dl', sl')

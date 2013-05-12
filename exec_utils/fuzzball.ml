(*
 Owned and copyright BitBlaze, 2007,2009. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

let usage = "trans_eval [options]* file.ir\n"
let infile = ref ""
let infile_set = ref false
let arg_name s = infile := s; infile_set := true

let new_memory () =
  Array.init 0x100000 (fun _ -> None)

let memory_store_byte mem addr b =
  let page = Int64.to_int (Int64.shift_right addr 12) and
      idx = Int64.to_int (Int64.logand addr 0xfffL) in
  let page_str = match mem.(page) with
    | Some page_str -> page_str
    | None ->
	let new_page = String.make 4096 '\x00' in
	  mem.(page) <- Some new_page;
	  new_page
  in
    page_str.[idx] <- Char.chr b

let memory_load_byte mem addr =
  let page = Int64.to_int (Int64.shift_right addr 12) and
      idx = Int64.to_int (Int64.logand addr 0xfffL) in
  let page_str = match mem.(page) with
    | Some page_str -> page_str
    | None ->
	let new_page = String.make 4096 '\x00' in
	  mem.(page) <- Some new_page;
	  new_page
  in
   Char.code page_str.[idx]

let nofix x = x

let fix_u ty x =
  (match ty with
     | V.REG_1 -> Int64.logand x 0x1L
     | V.REG_8 -> Int64.logand x 0xffL
     | V.REG_16 -> Int64.logand x 0xffffL
     | V.REG_32 -> Int64.logand x 0xffffffffL
     | V.REG_64 -> x
     | _ -> failwith "Bad int type in fix_u")

let fix_s ty x =
  (match ty with
     | V.REG_1 -> Int64.shift_right (Int64.shift_left x 63) 63
     | V.REG_8 -> Int64.shift_right (Int64.shift_left x 56) 56
     | V.REG_16 -> Int64.shift_right (Int64.shift_left x 48) 48
     | V.REG_32 -> Int64.shift_right (Int64.shift_left x 32) 32
     | V.REG_64 -> x
     | _ -> failwith "Bad int type in fix_s")

let bool64 f = fun a b -> if (f a b) then 1L else 0L

class frag_machine () = object(self)
  val mem = new_memory ()
  val reg_store = V.VarHash.create 100
  val temps = V.VarHash.create 100
  val mutable frag = ([], [])
  val mutable insns = []

  method init_prog (dl, sl) =
    List.iter (fun v -> V.VarHash.add reg_store v 0L) dl;
    self#set_frag (dl, sl);
    let result = self#run () in
      match result with
	| "fallthrough" -> ()
	| _ -> failwith "Initial program should fall through"

  method set_frag (dl, sl) =
    frag <- (dl, sl);
    V.VarHash.clear temps;
    List.iter (fun v -> V.VarHash.add temps v 0L) dl;
    insns <- sl

  method store_byte addr b =
    memory_store_byte mem addr (Int64.to_int b)

  method store_short addr s =
    memory_store_byte mem addr (Int64.to_int (Int64.logand 0xFFL s));
    memory_store_byte mem (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right s 8)))

  method store_word addr w =
    memory_store_byte mem addr (Int64.to_int (Int64.logand 0xFFL w));
    memory_store_byte mem (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 8)));
    memory_store_byte mem (Int64.add addr 2L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 16)));
    memory_store_byte mem (Int64.add addr 3L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 24)))

  method store_long addr l =
    memory_store_byte mem addr (Int64.to_int (Int64.logand 0xFFL l));
    memory_store_byte mem (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 8)));
    memory_store_byte mem (Int64.add addr 2L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 16)));
    memory_store_byte mem (Int64.add addr 3L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 24)));
    memory_store_byte mem (Int64.add addr 4L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 32)));
    memory_store_byte mem (Int64.add addr 5L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 40)));
    memory_store_byte mem (Int64.add addr 6L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 48)));
    memory_store_byte mem (Int64.add addr 7L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 56)))

  method load_byte addr =
    Int64.of_int (memory_load_byte mem addr)

  method load_short addr =
    let b1 = Int64.of_int (memory_load_byte mem addr)
    and b2 = Int64.of_int (memory_load_byte mem (Int64.add addr 1L))
    in
      Int64.logor b1 (Int64.shift_left b2 8)

  method load_word addr =
    let b1 = Int64.of_int (memory_load_byte mem addr)
    and b2 = Int64.of_int (memory_load_byte mem (Int64.add addr 1L))
    and b3 = Int64.of_int (memory_load_byte mem (Int64.add addr 2L))
    and b4 = Int64.of_int (memory_load_byte mem (Int64.add addr 3L))
    in
      Int64.logor 
	(Int64.logor b1 (Int64.shift_left b2 8))
	(Int64.logor (Int64.shift_left b3 16) (Int64.shift_left b4 24))

  method load_long addr =
    let b1 = Int64.of_int (memory_load_byte mem addr)
    and b2 = Int64.of_int (memory_load_byte mem (Int64.add addr 1L))
    and b3 = Int64.of_int (memory_load_byte mem (Int64.add addr 2L))
    and b4 = Int64.of_int (memory_load_byte mem (Int64.add addr 3L))
    and b5 = Int64.of_int (memory_load_byte mem (Int64.add addr 4L))
    and b6 = Int64.of_int (memory_load_byte mem (Int64.add addr 5L))
    and b7 = Int64.of_int (memory_load_byte mem (Int64.add addr 6L))
    and b8 = Int64.of_int (memory_load_byte mem (Int64.add addr 7L))
    in
      Int64.logor
	(Int64.logor 
	   (Int64.logor b1 (Int64.shift_left b2 8))
	   (Int64.logor (Int64.shift_left b3 16) (Int64.shift_left b4 24)))
	(Int64.logor 
	   (Int64.logor (Int64.shift_left b5 32) (Int64.shift_left b6 40))
	   (Int64.logor (Int64.shift_left b7 48) (Int64.shift_left b8 56)))

  method get_int_var ((_,_,ty) as var) =
    fix_u ty (self#eval_int_exp (V.Lval(V.Temp(var))))

  method eval_int_exp_ty exp =
    match exp with
      | V.BinOp(op, e1, e2) ->
	  let (v1, ty1) = self#eval_int_exp_ty e1 and
	      (v2, ty2) = self#eval_int_exp_ty e2 in
	  let ty = 
	    (match op with
	       | V.PLUS | V.MINUS | V.TIMES
	       | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
	       | V.BITAND | V.BITOR | V.XOR
		   -> assert(ty1 = ty2); ty1
	       | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		   -> ty1
	       | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		   -> assert(ty1 = ty2); V.REG_1) in
	  let (func, fix1, fix2) =
	    (match op with
	       | V.PLUS -> (Int64.add, nofix, nofix)
	       | V.MINUS -> (Int64.sub, nofix, nofix)
	       | V.TIMES -> (Int64.mul, nofix, nofix)
	       | V.DIVIDE -> (Vine_util.int64_udiv, fix_u ty, fix_u ty)
	       | V.SDIVIDE -> (Int64.div, fix_s ty, fix_s ty)
	       | V.MOD -> (Vine_util.int64_urem, fix_u ty, fix_u ty)
	       | V.SMOD -> (Int64.rem, fix_s ty, fix_s ty)
	       | V.LSHIFT -> ((fun v a -> Int64.shift_left v
				 (Int64.to_int a)), nofix, fix_u ty2)
	       | V.RSHIFT -> ((fun v a -> Int64.shift_right_logical v
				 (Int64.to_int a)), fix_u ty1, fix_u ty2)
	       | V.ARSHIFT -> ((fun v a -> Int64.shift_right v
				  (Int64.to_int a)), fix_s ty1, fix_u ty2)
	       | V.BITAND -> (Int64.logand, nofix, nofix)
	       | V.BITOR -> (Int64.logor, nofix, nofix)
	       | V.XOR -> (Int64.logxor, nofix, nofix)
	       | V.EQ -> (bool64 (=), fix_u ty1, fix_u ty1)
	       | V.NEQ -> (bool64 (<>), fix_u ty1, fix_u ty1)
	       | V.LT -> (bool64 (fun a b -> Vine_util.int64_ucompare
				    a b < 0), fix_u ty1, fix_u ty1)
	       | V.LE -> (bool64 (fun a b -> Vine_util.int64_ucompare
				    a b <= 0), fix_u ty1, fix_u ty1)
	       | V.SLT -> (bool64 (<), fix_s ty1, fix_s ty1)
	       | V.SLE -> (bool64 (<=), fix_s ty1, fix_s ty1))
	  in
	    (func (fix1 v1) (fix2 v2)), ty
      | V.UnOp(op, e1) ->
	  let (v1, ty1) = self#eval_int_exp_ty e1 and
	      func = 
	    (match op with
	       | V.NEG -> Int64.neg
	       | V.NOT -> Int64.lognot)
	  in
	    (func v1), ty1
      | V.Constant(V.Int(ty, i)) -> i, ty
      | V.Lval(V.Temp((_,_,ty) as var)) ->
	  (try
	     (V.VarHash.find reg_store var), ty
	   with
	     | Not_found ->
		 (try 
		    (V.VarHash.find temps var), ty
		  with
		    | Not_found -> V.pp_var print_string var; 
			failwith "Unknown variable"))
      | V.Lval(V.Mem(memv, idx, ty)) ->
	  let addr = fix_u V.REG_32 (self#eval_int_exp idx) in
	  let v =
	    (match ty with
	       | V.REG_8 -> self#load_byte addr
	       | V.REG_16 -> self#load_short addr
	       | V.REG_32 -> self#load_word addr
	       | V.REG_64 -> self#load_long addr
	       | _ -> failwith "Unsupported memory type") in
	    (v, ty)
      | V.Cast(kind, ty, e) ->
	  let (v1, ty1) = self#eval_int_exp_ty e in
	  let v =
	    match kind with
	      | V.CAST_UNSIGNED -> fix_u ty1 v1
	      | V.CAST_SIGNED -> fix_s ty1 v1
	      | V.CAST_LOW -> fix_u ty v1
	      | V.CAST_HIGH ->
		  let shift =
		    (match (ty1, ty) with
		       | (t_1, t_2) when t_1 = t_2 -> 0
		       | (V.REG_64, V.REG_1)  -> 63
		       | (V.REG_64, V.REG_8)  -> 56
		       | (V.REG_64, V.REG_16) -> 48
		       | (V.REG_64, V.REG_32) -> 32
		       | (V.REG_32, V.REG_1)  -> 31
		       | (V.REG_32, V.REG_8)  -> 24
		       | (V.REG_32, V.REG_16) -> 16
		       | (V.REG_16, V.REG_1)  -> 15
		       | (V.REG_16, V.REG_8)  -> 8
		       | (V.REG_8,  V.REG_1)  -> 7
		       | (_, _) -> failwith "Bad types in CAST_HIGH") in
		    Int64.shift_right v1 shift
	  in
	    (v, ty)
      | _ -> failwith "Unsupported (or non-int) expr type in eval_int_exp_ty"
	  
  method eval_int_exp exp =
    let (v, _) = self#eval_int_exp_ty exp in
      v

  method eval_bool_exp exp =
    if (Int64.logand (self#eval_int_exp exp) 0x1L) = 1L then true else false
      
  method eval_label_exp e =
    match e with
      | V.Name(lab) -> lab
      | _ ->
	  let addr = fix_u V.REG_32 (self#eval_int_exp e) in
	    Printf.sprintf "pc_0x%Lx" addr

  method jump lab =
    let rec find_label lab sl =
      match sl with
	| [] -> None
	| V.Label(l) :: rest when l = lab -> Some sl
	| st :: rest -> find_label lab rest
    in
    let (_, sl) = frag in
      match find_label lab sl with
	| None -> lab
	| Some sl ->
	    self#run_sl sl
	      
  method run_sl sl =
    match sl with
      | [] -> "fallthrough"
      | st :: rest ->
	  (match st with
	     | V.Jmp(l) -> self#jump (self#eval_label_exp l)
	     | V.CJmp(cond, l1, l2) ->
		 let cond_v = self#eval_bool_exp cond in
		   if cond_v then
		     self#jump (self#eval_label_exp l1)
		   else
		     self#jump (self#eval_label_exp l2)
	     | V.Move(V.Temp(v), e) ->
		 let value = self#eval_int_exp e in
		   (try
		      ignore(V.VarHash.find reg_store v);
		      V.VarHash.replace reg_store v value
		    with
			Not_found ->
			  V.VarHash.replace temps v value);
		   self#run_sl rest
	     | V.Move(V.Mem(memv, idx_e, ty), rhs_e) ->
		 let addr = fix_u V.REG_32 (self#eval_int_exp idx_e) and
		     value = self#eval_int_exp rhs_e in
		   (match ty with
		      | V.REG_8 -> self#store_byte addr value
		      | V.REG_16 -> self#store_short addr value
		      | V.REG_32 -> self#store_word addr value
		      | V.REG_64 -> self#store_long addr value
		      | _ -> failwith "Unsupported type in memory move");
		   self#run_sl rest
	     | V.Special("int 0x80") ->
		 failwith "No system calls yet"
	     | V.Special(_) -> failwith "Unknown special in run_sl"
	     | V.Label(_) -> self#run_sl rest
	     | V.ExpStmt(e) ->
		 let v = self#eval_int_exp e in
		   ignore(v);
		   self#run_sl rest
	     | V.Comment(_) -> self#run_sl rest
	     | V.Block(_,_) -> failwith "Block unsupported"
	     | V.Function(_,_,_,_,_) -> failwith "Function unsupported"
	     | V.Return(_) -> failwith "Return unsupported"
	     | V.Call(_,_,_) -> failwith "Call unsupported"
	     | V.Attr(st, _) -> self#run_sl (st :: rest)
	     | V.Assert(e) ->
		 let v = self#eval_bool_exp e in
		   assert(v);
		   self#run_sl rest
	     | V.Halt(e) ->
		 let v = self#eval_int_exp e in
		   Printf.sprintf "halt_%Ld" v)

  method run () = self#run_sl insns

end

(* class fake_frag_machine ((dl, sl) as prog) = object(self)
  val ce = new Vine_ceval.concrete_evaluator prog
  val mem_var = List.find (fun (i, s, t) -> s = "mem") dl

  method init_prog prog' =
    assert(prog == prog');
    ignore(ce#run ());
    ()
      
  method get_int_var var =
    match ce#eval_exp (V.Lval(V.Temp(var))) with
      | Vine_ceval.Int(_, i) -> i
      | _ -> failwith "Bad value in get_int_var"

  method load_byte addr =
    let b_val = ce#eval_exp (V.Lval(V.Mem(mem_var,
					  V.Constant(V.Int(V.REG_32, addr)),
					  V.REG_8))) in
      match b_val with
	| Vine_ceval.Int(V.REG_8, i) -> i
	| _ -> failwith "Memory values must be reg8_t"

  method load_word addr =
    let b_val = ce#eval_exp (V.Lval(V.Mem(mem_var,
					  V.Constant(V.Int(V.REG_32, addr)),
					  V.REG_32))) in
      match b_val with
	| Vine_ceval.Int(V.REG_32, i) -> i
	| _ -> failwith "Unexpected type in memory load"

  method set_frag prog =
    ce#restart prog

  method run () =
    try
      ignore(ce#run ()); "fallthrough"
    with
      | Vine_eval.NoSuchLabel(s) -> s

end *)

let fresh_addr = 
  let addr = ref 0x50000000L in
    fun size ->
      let ret = !addr in
	addr := Int64.add !addr size;
	addr := Int64.logand 0xffff_ffff_ffff_f000L
	  (Int64.add !addr 0x0fffL); (* page align *)
	ret

let the_break = ref 0x08100000L

let trans_cache = Hashtbl.create 100000 

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
    | V.Unknown(_) -> (e, V.REG_32)
    | V.Let(_) -> failwith "unhandled let in cfold_with_type"

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
       (*| V.Move(V.Temp(_,"R_CC_OP",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP1",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP2",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_NDEP",_),_) -> false*)
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
    (* List.iter print_string used_labels; *)
    List.filter
      (fun st -> match st with
	 | V.Label(l) when not (List.mem l used_labels) -> false
	 | _ -> true)
      sl

let rec count_uses_e var e =
  match e with
    | V.BinOp(_, e1, e2) -> (count_uses_e var e1) + (count_uses_e var e2)
    | V.UnOp(_, e) -> count_uses_e var e
    | V.Cast(_, _, e) -> count_uses_e var e
    | V.Lval(lv) -> count_uses_lv var lv
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

let contains hash ent =
  try
    V.VarHash.find hash ent;
    true;
  with
      Not_found -> false

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
      | V.Lval(V.Temp(v)) when contains map v ->
	  V.VarHash.find map v
      | V.Lval(V.Mem(v, idx, ty)) ->
	  V.Lval(V.Mem(v, replace_uses_e idx, ty))
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
      | st :: rest -> st :: loop rest
      | [] -> []
  in
    (dl, loop sl)

let simplify_frag (orig_dl, orig_sl) =
  let (dl, sl) = (orig_dl, orig_sl) in
  let sl = rm_unused_stmts sl in
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
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

let unix_fds = Array.make 1024 None
let () = Array.set unix_fds 0 (Some Unix.stdin)
let () = Array.set unix_fds 1 (Some Unix.stdout)
let () = Array.set unix_fds 2 (Some Unix.stderr)
let fresh_fd () =
  let rec loop i = match unix_fds.(i) with
    | None -> i
    | Some _ -> loop (i + 1)
  in loop 0

let get_fd vt_fd =
  match unix_fds.(vt_fd) with
    | Some fd -> fd
    | None -> raise
	(Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))

let errno err =
  match err with
    | Unix.E2BIG -> 7
    | Unix.EACCES -> 13
    | Unix.EAGAIN -> 11
    | Unix.EBADF -> 9
    | Unix.EBUSY -> 16
    | Unix.ECHILD -> 10
    | Unix.EDEADLK -> 35
    | Unix.EDOM -> 33
    | Unix.EEXIST -> 17
    | Unix.EFAULT -> 14
    | Unix.EFBIG -> 27
    | Unix.EINTR -> 4
    | Unix.EINVAL -> 22
    | Unix.EIO -> 5
    | Unix.EISDIR -> 21
    | Unix.EMFILE -> 24
    | Unix.EMLINK -> 31
    | Unix.ENAMETOOLONG -> 36
    | Unix.ENFILE -> 23
    | Unix.ENODEV -> 19
    | Unix.ENOENT -> 2
    | Unix.ENOEXEC -> 8
    | Unix.ENOLCK -> 37
    | Unix.ENOMEM -> 12
    | Unix.ENOSPC -> 28
    | Unix.ENOSYS -> 38
    | Unix.ENOTDIR -> 20
    | Unix.ENOTEMPTY ->	39
    | Unix.ENOTTY -> 25
    | Unix.ENXIO -> 6
    | Unix.EPERM -> 1
    | Unix.EPIPE -> 32
    | Unix.ERANGE -> 34
    | Unix.EROFS -> 30
    | Unix.ESPIPE -> 29
    | Unix.ESRCH -> 3
    | Unix.EXDEV -> 18
    | Unix.EWOULDBLOCK -> 11
    | Unix.EINPROGRESS -> 115
    | Unix.EALREADY -> 114
    | Unix.ENOTSOCK -> 88
    | Unix.EDESTADDRREQ -> 89
    | Unix.EMSGSIZE -> 90
    | Unix.EPROTOTYPE -> 91
    | Unix.ENOPROTOOPT -> 92
    | Unix.EPROTONOSUPPORT -> 93
    | Unix.ESOCKTNOSUPPORT -> 94
    | Unix.EOPNOTSUPP -> 95
    | Unix.EPFNOSUPPORT -> 96
    | Unix.EAFNOSUPPORT -> 97
    | Unix.EADDRINUSE -> 98
    | Unix.EADDRNOTAVAIL -> 99
    | Unix.ENETDOWN -> 100
    | Unix.ENETUNREACH -> 101
    | Unix.ENETRESET -> 102
    | Unix.ECONNABORTED -> 103
    | Unix.ECONNRESET -> 104
    | Unix.ENOBUFS -> 105
    | Unix.EISCONN -> 106
    | Unix.ENOTCONN -> 107
    | Unix.ESHUTDOWN -> 108
    | Unix.ETOOMANYREFS -> 109
    | Unix.ETIMEDOUT -> 110
    | Unix.ECONNREFUSED -> 111
    | Unix.EHOSTDOWN -> 112
    | Unix.EHOSTUNREACH -> 113
    | Unix.ELOOP -> 40
    | Unix.EOVERFLOW -> 75
    | Unix.EUNKNOWNERR(i) -> i

let rec runloop (*ce*) fm eip_var eip mem_var gpr_vars asmir_gamma =
  let load_byte addr = Int64.to_int (fm#load_byte addr) in
  let load_word addr = fm#load_word addr in
  let lea base i step off =
    Int64.add base (Int64.add (Int64.mul (Int64.of_int i) (Int64.of_int step))
		      (Int64.of_int off))
  in
  let read_reg32 var = fm#get_int_var var in
  let read_cstr addr =
    let rec bytes_loop i =
      let b = load_byte (Int64.add addr (Int64.of_int i)) in
	if b = 0 then [] else b :: bytes_loop (i + 1)
    in
      String.concat ""
	(List.map (fun b -> String.make 1 (Char.chr b))
	   (bytes_loop 0))
  in
  let read_buf addr len =
    Array.init len
      (fun i -> Char.chr (load_byte (Int64.add addr (Int64.of_int i))))
  in
  let mk_store_word base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
    V.Move(V.Mem(mem_var, V.Constant(V.Int(V.REG_32, addr)), V.REG_32),
	   V.Constant(V.Int(V.REG_32, v)))
  in
  let mk_store_byte base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
    V.Move(V.Mem(mem_var, V.Constant(V.Int(V.REG_8, addr)), V.REG_8),
	   V.Constant(V.Int(V.REG_8, v)))
  in
  let mk_zero_region base len =
    Vine_util.mapn
      (fun i -> mk_store_byte base i 0L) (len - 1)
  in
  let mk_store_str base idx str =
    Vine_util.mapn
      (fun i -> mk_store_byte (Int64.add base idx)
	 i (Int64.of_int (Char.code str.[i])))
      (String.length str - 1)
  in
  let mk_store_cstr base idx str =
    (mk_store_str base idx str)
      @ [mk_store_byte (Int64.add base idx) (String.length str) 0L]
  in
  let mk_put_reg var v =
    V.Move(V.Temp(var), V.Constant(V.Int(V.REG_32, fix_u V.REG_32 v)))
  in
  let mk_put_reg16 var v =
    V.Move(V.Temp(var), V.Constant(V.Int(V.REG_16, fix_u V.REG_16 v)))
  in
  let (eax_var, ebx_var, ecx_var, edx_var, esi_var, edi_var,
       esp_var, ebp_var, eflags_var, gs_var, gdt_var) =
    match gpr_vars with
      | [a; b; c; d; si; di; sp; bp; fl; gs; gd]
	-> (a, b, c, d, si, di, sp, bp, fl, gs, gd)
      | _ -> failwith "Bad length for gpr_vars"
  in
  let decode_insn eip =
    let insn_bytes = Array.init 16
      (fun i -> Char.chr (load_byte (Int64.add eip (Int64.of_int i))))
    in
    let asmp = Libasmir.byte_insn_to_asmp
      Libasmir.Bfd_arch_i386 eip insn_bytes in
    let sl = Asmir.asm_addr_to_vine asmir_gamma asmp eip in
      Libasmir.free_asm_program asmp;
      match sl with 
	| [V.Block(dl', sl')] -> (dl', sl')
	| _ -> failwith "expected asm_addr_to_vine to give single block"
  in
  let label_to_eip s =
    let len = String.length s in
    let hex = String.sub s 3 (len - 3) in (* remove "pc_" *)
      Int64.of_string hex
  in
  let rec last l =
    match l with
      | [e] -> e
      | a :: r -> last r
      | [] -> failwith "Empty list in last"
  in
  let rec decode_insns eip k first =
    if k = 0 then ([], []) else
      let (dl, sl) = decode_insn eip in
	if
	  List.exists (function V.Special("int 0x80") -> true | _ -> false) sl
	then
	  (* Make a system call be alone in its basic block *)
	  if first then (dl, sl) else ([], [])
	else
	  match last (rm_unused_stmts sl) with
	    | V.Jmp(V.Name(lab)) ->
		let next_eip = label_to_eip lab in
		let (dl', sl') = decode_insns next_eip (k - 1) false in
		  (dl @ dl', sl @ sl')
	    | _ -> (dl, sl) (* end of basic block, e.g. indirect jump *)
  in
  let decode_insns_cached eip =
    try
      Hashtbl.find trans_cache eip
    with
	Not_found ->
	  Hashtbl.add trans_cache eip
	    (simplify_frag (decode_insns eip 10 true));
	  Hashtbl.find trans_cache eip
  in
  let print_gprs () =
    Printf.printf "eax:%08Lx ebx:%08Lx ecx:%08Lx edx:%08Lx\n"
      (read_reg32 eax_var) (read_reg32 ebx_var) 
      (read_reg32 ecx_var) (read_reg32 edx_var);
    Printf.printf "esi:%08Lx edi:%08Lx esp:%08Lx ebp:%08Lx\n"
      (read_reg32 esi_var) (read_reg32 edi_var) 
      (read_reg32 esp_var) (read_reg32 ebp_var);
    Printf.printf "eip:%08Lx eflags:%08Lx\n"
      eip (read_reg32 eflags_var)
  in
  (* Remove "unknown" statments it seems safe to ignore *)
  let remove_known_unknowns sl =
    List.filter
      (function
	 | V.Move(_, V.Unknown("CCall: x86g_create_fpucw")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_check_fldcw")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_create_mxcsr")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_check_ldmxcsr")) -> false
	 | V.Move(_, V.Unknown("Unknown: GetI")) -> false
	     (* e.g., FPU load *)
	 | V.Move(_, V.Unknown("Floating point binop")) -> false
	 | V.Move(_, V.Unknown("Floating point triop")) -> false
	 | V.Move(_, V.Unknown("floatcast")) -> false
	 | V.ExpStmt(V.Unknown("Unknown: PutI")) -> false
	     (* e.g., FPU store *)
	 | V.ExpStmt(V.Unknown("Unknown: Dirty")) -> false
	     (* XXX too broad? covers rdtsc *)
	 | _ -> true)
      sl
  in
  (* Note that this has some side-effects immediately, and others via
     rewriting the statement list. *)
  let run_and_replace_syscall sl =
    match sl with
      | [V.Label(_); V.Comment(c1); V.Special("int 0x80");
	 V.Label(_); V.Jmp(e)]
      | [V.Comment(c1); V.Move(_, _); V.Special("int 0x80");
	 V.Jmp(e)]
      | [V.Comment(c1); V.Special("int 0x80"); V.Jmp(e)]
	->
	  (let syscall_num = Int64.to_int (read_reg32 eax_var) in
	   let do_write fd bytes count =
	     let success = [(mk_put_reg eax_var (Int64.of_int count))] in
	       try (match fd with
		      | 1 -> Array.iter print_char bytes; success
		      | _ ->
			  let str = Array.fold_left (^) ""
			    (Array.map (String.make 1) bytes)
			  in
			    match Unix.write (get_fd fd) str
			      0 (String.length str)
			    with
			      | i when i = count -> success
			      | _ -> raise (Unix.Unix_error(Unix.EINTR,"","")))
	       with
		 | Unix.Unix_error(err, _, _) ->
		     [(mk_put_reg eax_var (Int64.of_int ~-(errno err)))]
	   in
	   let do_unix_read fd addr count =
	     let total_read = ref 0 in
	     let rec loop left a =
	       if (left <= 0) then [] else
		 let chunk = if (left < 16384) then left else 16384 in
		 let str = String.create chunk in
		 let num_read = Unix.read fd str 0 chunk in
		   total_read := !total_read + num_read;
		   (mk_store_str a 0L (String.sub str 0 num_read)) ::
		     (loop (left - chunk) (Int64.add a (Int64.of_int chunk)))
	     in
	     let stores = List.concat (loop count addr) in
	       (stores, !total_read)
	   in
	   let mk_setup_tcb_seg new_ent base limit =
	     let new_gs = Int64.logor (Int64.shift_left new_ent 3) 3L in
	     let new_gdt = 0x60000000L in
	     let descr = Int64.add new_gdt (Int64.shift_left new_ent 3) in
	       [(mk_put_reg gdt_var new_gdt);
		(mk_put_reg16 gs_var new_gs);
		(mk_store_word descr 0 (Int64.logand limit 0xffL));
		(mk_store_word descr 1 (Int64.logand 
					  (Int64.shift_right limit 8) 0xffL));
		(mk_store_word descr 2 (Int64.logand base 0xffL));
		(mk_store_word descr 3 (Int64.logand
					  (Int64.shift_right base 8) 0xffL));
		(mk_store_word descr 4 (Int64.logand
					  (Int64.shift_right base 16) 0xffL));
		(mk_store_word descr 5 0xf3L); (* pres., ring 3, app, r/w a *)
		(mk_store_word descr 6 (Int64.logor 0xc0L
					  (Int64.shift_right limit 16)));
	          (* page-gran limit, 32-bit, high nibble of limit *)
		(mk_store_word descr 7 (Int64.logand
					  (Int64.shift_right base 24) 0xffL))]
	   in
	   let oc_kind_to_mode kind = match kind with
	     | Unix.S_REG  -> 0o0100000
	     | Unix.S_DIR  -> 0o0040000
	     | Unix.S_CHR  -> 0o0020000
	     | Unix.S_BLK  -> 0o0060000
	     | Unix.S_LNK  -> 0o0120000
	     | Unix.S_FIFO -> 0o0010000
	     | Unix.S_SOCK -> 0o0140000
	   in
	   let mk_write_oc_statbuf addr oc_buf =
	     let dev = Int64.of_int oc_buf.Unix.st_dev and
		 ino = Int64.of_int oc_buf.Unix.st_ino and
		 mode = Int64.of_int
	       (oc_buf.Unix.st_perm lor 
		  (oc_kind_to_mode oc_buf.Unix.st_kind)) and
		 nlink = Int64.of_int oc_buf.Unix.st_nlink and
		 uid = Int64.of_int oc_buf.Unix.st_uid and
		 gid = Int64.of_int oc_buf.Unix.st_gid and
		 rdev = Int64.of_int oc_buf.Unix.st_rdev and
		 size = Int64.of_int oc_buf.Unix.st_size and
		 atime = Int64.of_float oc_buf.Unix.st_atime and
		 mtime = Int64.of_float oc_buf.Unix.st_mtime and
		 ctime = Int64.of_float oc_buf.Unix.st_ctime and
		 blksize = 4096L and
		 blocks = Int64.of_int (oc_buf.Unix.st_size/4096)
	     in
	       [(mk_store_word addr 0 dev);
		(mk_store_word addr 4 0L); (* high bits of dev *)
		(mk_store_word addr 12 ino); (* 32-bit inode *)
		(mk_store_word addr 16 mode);
		(mk_store_word addr 20 nlink);
		(mk_store_word addr 24 uid);
		(mk_store_word addr 28 gid);
		(mk_store_word addr 32 rdev);
		(mk_store_word addr 36 0L); (* high bits of rdev *)
		(mk_store_word addr 44 size);
		(mk_store_word addr 48 0L); (* high bits of size *)
		(mk_store_word addr 52 blksize);
		(mk_store_word addr 56 blocks);
		(mk_store_word addr 64 atime);
		(mk_store_word addr 68 0L); (* atime nanosecs *)
		(mk_store_word addr 72 mtime);
		(mk_store_word addr 76 0L); (* mtime naonsecs *)
		(mk_store_word addr 80 ctime);
		(mk_store_word addr 84 0L); (* ctime nanosecs *)
		(mk_store_word addr 89 ino); (* low bits of 64-bit inode *)
		(mk_store_word addr 92 0L); (* high bits of 64-bit inode *)
	       ]
	   in
	   let new_sl =
	     (match syscall_num with
		| 3 -> (* read *)		    
		    let fd    = Int64.to_int (read_reg32 ebx_var) and
			buf   = read_reg32 ecx_var and
			count = Int64.to_int (read_reg32 edx_var) in
		      Printf.printf "read(%d, 0x%08Lx, %d)\n" fd buf count;
		      let str = String.create count in
		      let num_read = Unix.read (get_fd fd) str 0 count in
			(mk_store_str buf 0L (String.sub str 0 num_read)) @
			  [(mk_put_reg eax_var (Int64.of_int num_read))]
		| 4 -> (* write *)
		    let fd    = Int64.to_int (read_reg32 ebx_var) and
			buf   = read_reg32 ecx_var and
			count = Int64.to_int (read_reg32 edx_var) in
		      Printf.printf "write(%d, 0x%08Lx, %d)\n" fd buf count;
		    let bytes = read_buf buf count
		    in do_write fd bytes count
		| 5 -> (* open *)
		    let path_buf = read_reg32 ebx_var and
			flags    = Int64.to_int (read_reg32 ecx_var) and
			mode     = Int64.to_int (read_reg32 edx_var) in
		    let path = read_cstr path_buf in
		    let oc_flags =
		      (if (flags land 0x3) = 0 then [Unix.O_RDONLY] else []) @
			(if (flags land 0x3)= 1 then [Unix.O_WRONLY] else []) @
			(if (flags land 0x3) = 2 then [Unix.O_RDWR] else []) @
			(if(flags land 0o4000)!=0 then[Unix.O_NONBLOCK]else[])@
			(if (flags land 0o2000)!=0 then[Unix.O_APPEND]else[]) @
			(if (flags land 0o100)!=0 then [Unix.O_CREAT] else[]) @
			(if (flags land 0o1000)!=0 then [Unix.O_TRUNC]else[]) @
			(if (flags land 0o200)!=0 then [Unix.O_EXCL] else []) @
			(if (flags land 0o10000)!=0 then [Unix.O_SYNC] else [])
		    in
		      Printf.printf "open(\"%s\", 0x%x, 0o%o)\n"
			path flags mode;
		      (try
			 let oc_fd = Unix.openfile path oc_flags mode and
			     vt_fd = fresh_fd () in
			   Array.set unix_fds vt_fd (Some oc_fd);
			   [(mk_put_reg eax_var (Int64.of_int vt_fd))]
		       with
			 | Unix.Unix_error(err, _, _) ->
			     [(mk_put_reg eax_var
				 (Int64.of_int ~-(errno err)))])
		| 6 -> (* close *)
		    let fd = Int64.to_int (read_reg32 ebx_var) in
		      Printf.printf "close(%d)\n" fd;
		      (try
			 Unix.close (get_fd fd);
			 Array.set unix_fds fd None;
			 [(mk_put_reg eax_var 0L)] (* success *)
		       with
			 | Unix.Unix_error(err, _, _) ->
			     [(mk_put_reg eax_var
				 (Int64.of_int ~-(errno err)))])
		| 13 -> (* time *)
		    let addr = read_reg32 ebx_var in
		      Printf.printf "time(0x%08Lx)\n" addr;
		      let time = Int64.of_float (Unix.time ()) in
			if addr != 0L then
			  [(mk_store_word addr 0 time)] else []
			    @
			    [(mk_put_reg eax_var time)]
		| 33 -> (* access *)
		    let path_buf = read_reg32 ebx_var and
			mode     = Int64.to_int (read_reg32 ecx_var) in
		    let path = read_cstr path_buf in
		    let oc_mode =
		      (if   (mode land 0x7)= 0 then [Unix.F_OK] else []) @
			(if (mode land 0x1)!=0 then [Unix.X_OK] else []) @
			(if (mode land 0x2)!=0 then [Unix.W_OK] else []) @
			(if (mode land 0x4)!=0 then [Unix.R_OK] else []) 
		    in
		      Printf.printf "access(\"%s\", 0x%x)\n"
			path mode;
		      (try
			 Unix.access path oc_mode;
			 [(mk_put_reg eax_var 0L)]
		       with
			 | Unix.Unix_error(err, _, _) ->
			     [(mk_put_reg eax_var
				 (Int64.of_int ~-(errno err)))])
		| 43 -> (* times *)
		    let addr = read_reg32 ebx_var in
		      Printf.printf "times(0x%08Lx)\n" addr;
		      let float_to_clocks f =
			Int64.of_float (f *. 100.0) in
		      let pt = Unix.times () in
		      let ut = float_to_clocks (pt.Unix.tms_utime) and
			  st = float_to_clocks (pt.Unix.tms_stime) and
			  cut = float_to_clocks (pt.Unix.tms_cutime) and
			  cst = float_to_clocks (pt.Unix.tms_cstime) in
			Printf.printf "times: %Ld %Ld %Ld %Ld\n"
			  ut st cut cst;
			[mk_store_word addr 0 ut] @
			  [mk_store_word addr 4 st] @			 
			  [mk_store_word addr 8 cut] @
			  [mk_store_word addr 12 cst] @
			  [(mk_put_reg eax_var (Int64.add ut st))]
		| 45 -> (* brk *)
		    let addr = read_reg32 ebx_var in
		      Printf.printf "brk(0x%08Lx)\n" addr;
		      if addr < !the_break then
			()
		      else
			the_break := addr;
		      [(mk_put_reg eax_var !the_break)]
		| 54 -> (* ioctl *)
		    let fd   = Int64.to_int (read_reg32 ebx_var) and
			req  = read_reg32 ecx_var and
			argp = read_reg32 edx_var in
		      Printf.printf "ioctl(%d, 0x%Lx, 0x%08Lx)\n" fd req argp;
		      (match req with
			 | 0x5401L -> 
			     (* let attrs = Unix.tcgetattr (get_fd fd) in *)
			     failwith "Unhandled TCGETS ioctl"
			 | _ -> failwith "Unknown ioctl")
		| 85 -> (* readlink *)
		    let path_buf = read_reg32 ebx_var and
			out_buf  = read_reg32 ecx_var and
			buflen   = Int64.to_int (read_reg32 edx_var) in
		    let path = read_cstr path_buf in
		      Printf.printf "readlink(\"%s\", 0x%08Lx, %d)\n"
			path out_buf buflen;
		      let real = Unix.readlink path in
		      let written = min buflen (String.length real) in
			(mk_store_str out_buf 0L (String.sub real 0 written))
			  @
			  [(mk_put_reg eax_var (Int64.of_int written))]
		| 91 -> (* munmap *)
		    let addr = read_reg32 ebx_var and
			len  = read_reg32 ecx_var in
		      Printf.printf "munmap(0x%08Lx, %Ld)\n" addr len;
		      (* treat as no-op *)
		      [(mk_put_reg eax_var 0L)]
		| 122 -> (* uname *)
		    let buf = read_reg32 ebx_var in
		      Printf.printf "uname(0x%08Lx)\n" buf;
		      (List.concat
			 (List.map2
			    (fun i str ->
			       (mk_store_cstr buf (Int64.mul 65L i) str))
			    [0L; 1L; 2L; 3L; 4L; 5L]
			    ["Linux"; (* sysname *)
			     "amuro"; (* nodename *)
			     "2.6.26-2-amd64"; (* release *)
			     "#1 SMP Fri Mar 27 04:02:59 UTC 2009"; (*version*)
			     "i686"; (* machine *)
			     "cs.berkeley.edu" (* domain *)
			    ]))
		      @ [(mk_put_reg eax_var 0L)] (* success *)
		| 125 -> (* mprotect *)
		    let addr = read_reg32 ebx_var and
			len  = read_reg32 ecx_var and
			prot = read_reg32 edx_var in
		      Printf.printf "mprotect(0x%08Lx, %Ld, %Ld)\n"
			addr len prot;
		      (* treat as no-op *)
		      [(mk_put_reg eax_var 0L)]
		| 146 -> (* writev *)
		    let fd  = Int64.to_int (read_reg32 ebx_var) and
			iov = read_reg32 ecx_var and
			cnt = Int64.to_int (read_reg32 edx_var) in
		      Printf.printf "writev(%d, 0x%08Lx, %d)\n" fd iov cnt;
		      let bytes =
			Array.concat
			  (Vine_util.mapn
			     (fun i -> read_buf
				(load_word (lea iov i 8 0)) (* iov_base *)
				(Int64.to_int (load_word (lea iov i 8 4))))
			                                    (* iov_len *)
			     (cnt - 1)) in
			do_write fd bytes (Array.length bytes)
		| 174 -> (* rt_sigaction *)
		    let signum = Int64.to_int (read_reg32 ebx_var) and
			newbuf = read_reg32 ecx_var and
			oldbuf = read_reg32 edx_var and
			setlen = Int64.to_int (read_reg32 esi_var) in
		      Printf.printf "rt_sigaction(%d, 0x%08Lx, 0x%08Lx, %d)\n"
			signum newbuf oldbuf setlen;
		      let setold = if oldbuf = 0L then [] else
			let (action, mask_low, mask_high, flags) =
			  match signum with
			    | 8 (* SIGFPE *) -> (0L, 0L, 0L, 0L)
			    | _ -> failwith
				"Unhandled old signal in rt_sigaction";
			in
			  [(mk_store_word oldbuf 0 action);
			   (mk_store_word oldbuf 4 flags);
			   (mk_store_word oldbuf 8 0L); (* restorer *)
			   (mk_store_word oldbuf 12 mask_low);
			   (mk_store_word oldbuf 12 mask_high)]
		      in
			setold @ [(mk_put_reg eax_var 0L)] (* success *)
		| 175 -> (* rt_sigprocmask *)
		    let how    = Int64.to_int (read_reg32 ebx_var) and
			newset = read_reg32 ecx_var and
			oldset = read_reg32 edx_var and
			setlen = Int64.to_int (read_reg32 esi_var) in
		      Printf.printf "rt_sigprocmask(%d, 0x%08Lx, 0x%08Lx, %d)\n"
			how newset oldset setlen;
		      let setold = if oldset = 0L then [] else
			failwith "Can't report old mask" in
			setold @ [(mk_put_reg eax_var 0L)] (* success *)
		| 191 -> (* ugetrlimit *)
		    let rsrc = Int64.to_int (read_reg32 ebx_var) and
			buf  = read_reg32 ecx_var in
		      Printf.printf "ugetrlimit(%d, 0x%08Lx)\n" rsrc buf;
		      [(mk_store_word buf 0 0xffffffffL); (* infinity *)
		       (mk_store_word buf 4 0xffffffffL); (* infinity *)
		       (mk_put_reg eax_var 0L)] (* success *)
		| 192 -> (* mmap2 *)
		    let addr     = read_reg32 ebx_var and
			length   = read_reg32 ecx_var and
			prot     = read_reg32 edx_var and
			flags    = read_reg32 esi_var and
			fd       = Int64.to_int (read_reg32 edi_var) and
			pgoffset = Int64.to_int (read_reg32 ebp_var) in
		      Printf.printf "mmap2(0x%08Lx, %Ld, 0x%Lx, 0x%0Lx, %d, %d)\n"
			addr length prot flags fd pgoffset;
		      let do_read addr = 
			let len = Int64.to_int length in
			let old_loc = Unix.lseek (get_fd fd) 0 Unix.SEEK_CUR in
			let _ = Unix.lseek (get_fd fd)
			  (4096*pgoffset) Unix.SEEK_SET in
			let (stores, nr) = do_unix_read (get_fd fd) addr len in
			let _ = Unix.lseek (get_fd fd)
			  old_loc Unix.SEEK_SET in
			  Printf.printf "Read %d bytes\n" nr;
			  (stores, addr)
		      in
			let (sl, ret) =
			  match (addr, length, prot, flags, fd, pgoffset) with
			    | (0L, _, 0x3L (* PROT_READ|PROT_WRITE *),
			       0x22L (* MAP_PRIVATE|MAP_ANONYMOUS *), -1, 0)
			      ->
				let fresh = fresh_addr length in
				  ((mk_zero_region fresh
				      (Int64.to_int length)),
				   fresh)
			    | (_, _, 0x3L (* PROT_READ|PROT_WRITE *),
			       0x32L (* MAP_PRIVATE|FIXED|ANONYMOUS *), -1, 0)
			      -> ((mk_zero_region addr (Int64.to_int length)),
				  addr)
			    | (0L, _, 
			       (0x1L|0x5L) (* PROT_READ|PROT_EXEC *),
			       (0x802L|0x2L) (* MAP_PRIVATE|MAP_DENYWRITE *),
			       _, _) ->
				let dest_addr = fresh_addr length in
				  do_read dest_addr
			    | (_, _, 0x3L (* PROT_READ|PROT_WRITE *),
			       0x812L (* MAP_DENYWRITE|PRIVATE|FIXED *),
			       _, _) ->
				do_read addr
			    | _ -> failwith "Unhandled mmap operation"
			in
			  mk_put_reg eax_var ret :: sl
		| 195 -> (* stat64 *)
		    let path_buf = read_reg32 ebx_var and
			buf_addr = read_reg32 ecx_var in
		    let path = read_cstr path_buf in
		      Printf.printf "stat64(\"%s\", 0x%08Lx)\n" path buf_addr;
		      (try
			 let oc_buf = Unix.stat path in
			   (mk_write_oc_statbuf buf_addr oc_buf) @
			     [(mk_put_reg eax_var 0L)] (* success *)
		       with
			 | Unix.Unix_error(err, _, _) ->
			     [(mk_put_reg eax_var
				 (Int64.of_int ~-(errno err)))])
		| 197 -> (* fstat64 *)
		    let fd = Int64.to_int (read_reg32 ebx_var) and
			buf_addr = read_reg32 ecx_var in
		      Printf.printf "fstat64(%d, 0x%08Lx)\n" fd buf_addr;
		      let oc_buf = Unix.fstat (get_fd fd) in
			(mk_write_oc_statbuf buf_addr oc_buf) @
			  [(mk_put_reg eax_var 0L)] (* success *)
		| 199 -> (* getuid32 *)
		    Printf.printf "getuid32()\n";
		    [(mk_put_reg eax_var (Int64.of_int (Unix.getuid ())))]
		| 200 -> (* getgid32 *)
		    Printf.printf "getgid32()\n";
		    [(mk_put_reg eax_var (Int64.of_int (Unix.getgid ())))]
		| 201 -> (* geteuid32 *)
		    Printf.printf "geteuid32()\n";
		    [(mk_put_reg eax_var (Int64.of_int (Unix.geteuid ())))]
		| 202 -> (* getegid32 *)
		    Printf.printf "getegid32()\n";
		    [(mk_put_reg eax_var (Int64.of_int (Unix.getegid ())))]
		| 240 -> (* futex *)
		    let uaddr    = read_reg32 ebx_var and
			op       = Int64.to_int (read_reg32 ecx_var) and
			value    = read_reg32 edx_var and
			timebuf  = read_reg32 esi_var and
			uaddr2   = read_reg32 edi_var and
			val3     = read_reg32 ebp_var in
		      Printf.printf "futex(0x%08Lx, %d, %Ld, 0x%08Lx, 0x%08Lx, %Ld)\n"
			uaddr op value timebuf uaddr2 val3;
		      let ret = 
			match (op, value) with
			  | (129 (* FUTEX_WAKE_PRIVATE *), 1L) ->
			      0L (* never anyone to wake *)
			  | _ -> failwith "Unhandled futex operation"
		      in
			[(mk_put_reg eax_var ret)]
		| 243 -> (* set_thread_area *)
		    let uinfo = read_reg32 ebx_var in
		      Printf.printf "set_thread_area(0x%08Lx)\n" uinfo;
		      let old_ent = Int64.to_int (load_word (lea uinfo 0 0 0))
		      and
			  base    = load_word (lea uinfo 0 0 4) and
			  limit   = load_word (lea uinfo 0 0 8) in
			Printf.printf "set_thread_area({entry: %d, base: %Lx, limit: %Ld})\n" old_ent base limit;
			(match (old_ent, base, limit) with
			   | (-1, _, _) ->
			       let new_ent = 12L in
				 (mk_setup_tcb_seg new_ent base limit) @
				   [(mk_store_word uinfo 0 new_ent);
				    (mk_put_reg eax_var 0L)] (* success *)
			   | _ -> failwith "Unhandled args to set_thread_area")
		| 252 -> (* exit_group *)
		    let status = read_reg32 ebx_var in
		      Printf.printf "exit_group(%Ld)\n" status;
		      [V.Halt(V.Constant(V.Int(V.REG_32, status)))]
		| 258 -> (* set_tid_address *)
		    let addr = read_reg32 ebx_var in
		      Printf.printf "set_tid_address(0x08%Lx)\n" addr;
		      let pid = Unix.getpid () in
			[(mk_put_reg eax_var (Int64.of_int pid))]
		| 311 -> (* set_robust_list *)
		    let addr = read_reg32 ebx_var and
			len  = read_reg32 ecx_var in
		      Printf.printf "set_robust_list(0x08%Lx, %Ld)\n" addr len;
		      [(mk_put_reg eax_var 0L)] (* success *)
		| _ ->
		    Printf.printf "Unhandled system call %d\n" syscall_num;
		    [V.Jmp(V.Name("abort_unk_syscall"))])
	      in
	     [V.Comment(c1)] @ new_sl @ [V.Jmp(e)])
      | _ -> sl
  in
  let (dl, sl) = decode_insns_cached eip in
  let prog = (dl, (remove_known_unknowns
		     (run_and_replace_syscall sl))) in
    (* Libasmir.print_disasm_rawbytes Libasmir.Bfd_arch_i386 eip insn_bytes;
    print_string "\n"; *)
    (* Printf.printf "EIP is %08Lx\n" eip; *)
    (* Printf.printf "Watchpoint val is %02x\n" (load_byte 0x501650e8L); *)
    (* Printf.printf ("Insn bytes are %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n") (load_byte eip)
      (load_byte (Int64.add eip (Int64.of_int 1)))
      (load_byte (Int64.add eip (Int64.of_int 2)))
      (load_byte (Int64.add eip (Int64.of_int 3)))
      (load_byte (Int64.add eip (Int64.of_int 4)))
      (load_byte (Int64.add eip (Int64.of_int 5)))
      (load_byte (Int64.add eip (Int64.of_int 6)))
      (load_byte (Int64.add eip (Int64.of_int 7)))
      (load_byte (Int64.add eip (Int64.of_int 8)))
      (load_byte (Int64.add eip (Int64.of_int 9)))
      (load_byte (Int64.add eip (Int64.of_int 10)))
      (load_byte (Int64.add eip (Int64.of_int 11)))
      (load_byte (Int64.add eip (Int64.of_int 12)))
      (load_byte (Int64.add eip (Int64.of_int 13)))
      (load_byte (Int64.add eip (Int64.of_int 14)))
      (load_byte (Int64.add eip (Int64.of_int 15))); *)
    (* print_gprs (); *)
    (* V.pp_program print_string prog; *)
    fm#set_frag prog;
    flush stdout;
    let s = fm#run () in
    let new_eip = label_to_eip s in
      if new_eip = 0L then failwith "Jump to 0" else
	((*Printf.printf "Next EIP will be %Lx\n" new_eip;*)
	  (*print_gprs ();*)
	  runloop fm eip_var new_eip mem_var gpr_vars asmir_gamma)
      (*| Vine_eval.EvalError("Cannot evaluate special(\"int 0x80\");\n ")
	-> (let syscall_num = Int64.to_int (read_reg32 eax_var) in
	      Printf.printf "Unhandled system call %d\n" syscall_num) *)

let main argc argv = 
  let speclist = Vine_parser.defspecs in 
  let speclist = [] @ speclist in 
    Arg.parse speclist arg_name usage;
    if(!infile_set = false) then  (
      Arg.usage speclist usage; exit(-1)
    );
    let prog = (
      let p = Vine_parser.parse_file !infile in 
      let () = if !Vine_parser.flag_typecheck then
	Vine_typecheck.typecheck p else () in 
	p
    ) in 
    let () = if !Vine_parser.flag_pp then 
      Vine.pp_program (print_string) prog in
    let (dl, sl) = prog in
    let eip_var = List.find (fun (i, s, t) -> s = "R_EIP") dl in
    let mem_var = List.find (fun (i, s, t) -> s = "mem") dl in
    let gpr_vars = List.map
      (fun name -> List.find (fun (i, s, t) -> s = name) dl)
      ["R_EAX"; "R_EBX"; "R_ECX"; "R_EDX";
       "R_ESI"; "R_EDI"; "R_ESP"; "R_EBP"; "EFLAGS";
       "R_GS"; "R_GDT"] in
    let asmir_gamma = Asmir.gamma_create mem_var dl in
    let fm = new frag_machine () in
    (* let fm = new fake_frag_machine prog in *)
      fm#init_prog prog;
      let eip = fm#get_int_var eip_var
      in
	runloop fm eip_var eip mem_var gpr_vars asmir_gamma
;;

main (Array.length Sys.argv) Sys.argv;;

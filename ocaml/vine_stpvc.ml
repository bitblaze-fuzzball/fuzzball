(**
   Translation from vine expressions to expressions inside libstp and 
   vice versa. 

   The purpose of this module is to build expressions inside libstp, which can
   then be queried, asserted, simplified, or used to build other expressions.
   
   @author Ivan Jager, Pongsin Poosankam
 *)

open Stpvc
open Vine
open ExtList


module VH = Vine.VarHash

type ctx = Stpvc.exp VH.t
type revctx = (string, Vine.var) Hashtbl.t

(* note: this is different than Vine.is_not_memory, because arrays are
   true here versus false there. *)
let is_not_mem (_,_,t) =
  match (unwind_type t) with
      TMem _ -> false
    | _ -> true


(** Convert a type to STP.
    @param vc The STP validity checker the result will be used in.
    @param t The VinE type to be translated.
    @return the STP equivalent of the given VinE type.
*)
let rec typ_to_stp vc t =
  match t with
      REG_1
    | REG_8
    | REG_16
    | REG_32
    | REG_64 ->
	Stpvc.bitvector_t vc (bits_of_width t)
    | Array(t1,_) ->
	(* let t2 = idx_size_to_type i in  *)
	Stpvc.array_t vc (typ_to_stp vc REG_64) (typ_to_stp vc t1)
    | _ -> raise (Invalid_argument "Unsupported type in STP")

(** Create an empty context for expression translation. *)
let empty_ctx () = VH.create 8

(** Create a context containing the given declarations.
    @param vc The STP validity checker the result will be used in.
    @param decls The list of declarations.
    @return the new context.
*)
let new_ctx vc decls =
  let ctx = VH.create (List.length decls) in
  let () = List.iter 
    (fun ((num,name,t) as n) ->
       VH.add ctx n (e_var vc (name^"_"^string_of_int num) (typ_to_stp vc t)))
    decls
  in
    ctx

(** Convert a [ctx] into a [revctx]. The [revctx] can be used in {!stp_to_vine}
    to get back the original variable names. *)
let rev_ctx ctx =
  let rc = Hashtbl.create (VH.length ctx) in
    VH.iter (fun k v -> Hashtbl.add rc (get_name v) k) ctx;
    rc
    

(** Build an STP expression for the given VinE variable. The effect is
    similar to constructing a Temp expression with the variable and
    passing that to vine_var_to_stp, but it saves some steps including
    a call to STP's simplifier. Avoiding the simplifier can be good if
    you are going to use the variable for a counter-example lookup.
    @param vc The STP validity checker the result will be used in. The VC passed
    here must be the same as the one used to create the context.
    @param ctx The context created by new_ctx.
    @param v The variable to translate.
    @return the translated STP expression.
*)
let vine_var_to_stp vc ctx v = VH.find ctx v

(** Build an STP expression for the given VinE expression
    @param vc The STP validity checker the result will be used in. The VC passed
    here must be the same as the one used to create the context. (We could
    just include it in the context.)
    @param ctx The context created by new_ctx.
    @param e The expression to translate.
    @return the translated STP expression.
*)
let rec vine_to_stp vc ctx e =
  let rec tr e =
    match e with
	UnOp(NOT, e1) -> let (s1, ty) = tr e1 in (e_bvnot vc s1), ty
      | UnOp(NEG, e1) -> let (s1, ty) = tr e1 in (e_bvneg vc s1), ty
      | BinOp(bop,e1,e2) ->
	  let (s1, ty1) = tr e1 and
	      (s2, ty2) = tr e2 in
	  let w = bits_of_width ty1 in
	    (* w2 is only needed for the shift hack *)
	  let w2 = bits_of_width ty2 in
	  let tobv f = (fun vc x y -> (e_boolbv vc (f vc x y))) in
	  let ty = match bop with
	    | EQ | NEQ | LT | LE | SLT | SLE -> REG_1
	    | _ -> ty1
	  in
	  let constr = match bop with
	      PLUS -> e_bvplus vc w
	    | MINUS -> e_bvminus vc w
	    | TIMES -> e_bvmult vc w
	    | DIVIDE -> e_bvdiv vc w
	    | SDIVIDE -> e_sbvdiv vc w
	    | MOD -> e_bvmod vc w
	    | SMOD -> e_sbvmod vc w
	    | BITAND -> e_bvand vc
	    | BITOR -> e_bvor vc
	    | XOR -> e_bvxor vc
	    | EQ -> tobv e_eq vc
	    | NEQ -> (fun x y -> e_boolbv vc (e_not vc (e_eq vc x y)))
	    | LT -> tobv e_bvlt vc 
	    | LE -> tobv e_bvle vc
	    | SLT -> tobv e_bvslt vc
	    | SLE -> tobv e_bvsle vc
	    | LSHIFT -> (fun x y -> e_bvshiftleft vc w x w2 y)
	    | RSHIFT -> (fun x y -> e_bvshiftright vc w x w2 y)
	    | ARSHIFT -> (fun x y ->e_bvshiftright_arith vc w x w2 y)
	  in
	    (constr s1 s2), ty
      | Constant(Int(t, v)) -> (e_bv_of_int64 vc (bits_of_width t) v), t
      | Constant _ -> 
	  raise (Invalid_argument "Only constant integers supported")
      | Cast(ct,t,e1) ->
	  let (s1, t1) = tr e1 in
	  let (w, w1) = (bits_of_width t, bits_of_width t1) in
	    (match ct with
		 CAST_SIGNED    -> e_bvsextend vc w s1
	       | CAST_UNSIGNED  ->
		   if w = w1 then s1 else
		     e_bvconcat vc (e_bv_of_int vc (w-w1) 0) s1
	       | CAST_LOW       -> e_bvextract vc s1 (w-1) 0
	       | CAST_HIGH      -> e_bvextract vc s1 (w1-1) (w1-w)
	    ), t
      | Let(Temp _, _, _) -> 
	  tr_let [] e
      | Let(Mem(memname,_,_), _, _) when is_not_mem memname ->
	  tr_let [] e
      | Let(Mem(_,_,_), _, _) ->
	  failwith "Unsupported let-mem in vine_stpvc"
      | Lval(Temp ((n,s,t) as v)) ->
	  (VH.find ctx v), t
      | Lval(Mem(memname,a,t)) when is_not_mem memname->
	  let a' = Cast(CAST_UNSIGNED,REG_64, a) in
	  let (s_a, _) = tr a' in
	    (e_read vc (VH.find ctx memname) s_a), t
      | Lval(Mem(_,_,_)) ->
	  failwith "Unsupported memory lvalue in vine_stpvc"
      | Name _ -> failwith "vine_stpvc: translation from Name unsupported"
(*      | Phi _ -> failwith "vine_stpvc: Phi unsupported" *)
      | Ite(ce, te, fe) ->
	  let (s1, ty1) = tr ce and
	      (s2, ty2) = tr te and
	      (s3, ty3) = tr fe in
	    assert(ty1 = REG_1);
	    assert(ty2 = ty3);
	    let cond_s = e_eq vc s1 (e_bv_of_int vc 1 1) in
	      (e_ite vc cond_s s2 s3), ty2
      | Unknown _ -> failwith "vine_stpvc: Unknown unsupported"
  and tr_let to_remove e =
    match e with
	Let(Temp n, e1, e2) ->
	  let (s1, _) = tr e1 in
	    VH.add ctx n s1;
	    tr_let (n::to_remove) e2
      | Let(Mem(memname,a,_), e1, e2) when is_not_mem memname->
          let mem = VH.find ctx memname in
          let a' = Cast(CAST_UNSIGNED,REG_64, a) in
	  let (s_a, _) = tr a' in
	  let (s1, _) = tr e1 in
          let newmem = e_write vc mem s_a s1 in
          let () = VH.add ctx memname newmem in
	    tr_let (memname::to_remove) e2
      | _ -> 
	let ret = tr e in
	  List.iter (VH.remove ctx) to_remove;
	  ret
  in
  let (s, _) = tr e in
    Stpvc.e_simplify vc s

(**
   [type_of_width w] return the smallest VinE type that can hold variable of
   [w] bits.
*)
let type_of_width = function
    1 -> REG_1
  | x when (<=) x 8 -> REG_8
  | x when (<=) x 16 -> REG_16
  | x when (<=) x 32 -> REG_32
  | x when (<=) x 64 -> REG_64
  | _ -> failwith "BVLength is greater than 64 bits"
      
(**
   [kind_str e] return a string associated to [e].
*)
let kind_str e = 
  match get_kind e with
      Libstp.UNDEFINED -> "UNDEFINED"
    | Libstp.SYMBOL -> "SYMBOL"
    | Libstp.BVCONST -> "BVCONST"
    | Libstp.BVNEG -> "BVNEG"
    | Libstp.BVCONCAT -> "BVCONCAT"
    | Libstp.BVOR -> "BVOR"
    | Libstp.BVAND -> "BVAND"
    | Libstp.BVXOR -> "BVXOR"
    | Libstp.BVNAND -> "BVNAND"
    | Libstp.BVNOR -> "BVNOR"
    | Libstp.BVXNOR -> "BVXNOR"
    | Libstp.BVEXTRACT -> "BVEXTRACT"
    | Libstp.BVLEFTSHIFT -> "BVLEFTSHIFT"
    | Libstp.BVRIGHTSHIFT -> "BVRIGHTSHIFT"
    | Libstp.BVSRSHIFT -> "BVSRSHIFT"
    | Libstp.BVVARSHIFT -> "BVVARSHIFT"
    | Libstp.BVPLUS -> "BVPLUS"
    | Libstp.BVSUB ->"BVSUB"
    | Libstp.BVUMINUS ->"BVUMINUS"
    | Libstp.BVMULTINVERSE -> "BVMULTINVERSE"
    | Libstp.BVMULT -> "BVMULT"
    | Libstp.BVDIV -> "BVDIV"
    | Libstp.BVMOD -> "BVMOD"
    | Libstp.SBVDIV -> "SBVDIV"
    | Libstp.SBVMOD -> "SBVMOD"
    | Libstp.BVSX -> "BVSX"
    | Libstp.BOOLVEC -> "BOOLVEC"
    | Libstp.ITE -> "ITE"
    | Libstp.BVGETBIT -> "BVGETBIT"
    | Libstp.BVLT -> "BVLT"
    | Libstp.BVLE -> "BVLE"
    | Libstp.BVGT -> "BVGT"
    | Libstp.BVGE -> "BVGE"
    | Libstp.BVSLT -> "BVSLT"
    | Libstp.BVSLE -> "BVSLE"
    | Libstp.BVSGT -> "BVSGT"
    | Libstp.BVSGE -> "BVSGE"
    | Libstp.EQ -> "EQ"
    | Libstp.NEQ -> "NEQ"
    | Libstp.FALSE -> "FALSE"
    | Libstp.TRUE -> "TRUE"
    | Libstp.NOT -> "NOT"
    | Libstp.AND -> "AND"
    | Libstp.OR -> "OR"
    | Libstp.NAND -> "NAND"
    | Libstp.NOR -> "NOR"
    | Libstp.XOR -> "XOR"
    | Libstp.IFF -> "IFF"
    | Libstp.IMPLIES -> "IMPLIES"
    | Libstp.READ -> "READ"
    | Libstp.WRITE -> "WRITE"
    | Libstp.ARRAY -> "ARRAY"
    | Libstp.BITVECTOR -> "BITVECTOR"
    | Libstp.BOOLEAN -> "BOOLEAN"
	
(**
   [stp_to_type stp_exp] returns an equivalent VinE type for [stp_exp].
   
   @return an equivalent VinE type of the given STP expression
*)
let stp_to_type e =
  match Stpvc.get_type e with
      Libstp.BOOLEAN_TYPE -> REG_1
    | Libstp.BITVECTOR_TYPE -> type_of_width (Stpvc.get_vwidth e)
    | Libstp.ARRAY_TYPE -> (* STP array has at most 1-D *)
	let iwidth = Int64.of_int (Stpvc.get_iwidth e) in
	let vtype = type_of_width (Stpvc.get_vwidth e) in
	  Array(vtype, iwidth)
	    (* FIXME: iwidth is wrong *)
    | Libstp.UNKNOWN_TYPE -> failwith "stp_to_type: unknown type"

(**
   [letize exp] letizes the sub-nodes in STP expression [exp].

   FIXME: Should look at the NodeLetVarMap and get the proper names,
   rather than calling everything "stplet".

   @return reverse-ordered letize node list.
*)
let letize exp =
  let seen_hash = Hashtbl.create 17 in
  let let_hash = Hashtbl.create 17 in
  let let_nodes = ref [] in
  let rec do_letize e =
    match get_kind e with
	Libstp.SYMBOL | Libstp.BVCONST | Libstp.FALSE | Libstp.TRUE -> ()
      | _ -> 
	  let e_id = get_id e in
	    if (not (Hashtbl.mem let_hash e_id))
	    then (
	      if (Hashtbl.mem seen_hash e_id)
	      then (
		let letvar = newvar "stplet" (stp_to_type e) in
		  Hashtbl.add let_hash e_id ();
		  let_nodes := (letvar, e) :: !let_nodes
	      )
	      else (
		Hashtbl.add seen_hash e_id ();
		let children = List.init (get_degree e) 
		  (fun x -> get_child e x) in
		  List.iter (fun node -> do_letize node) children
	      )
	    )
  in
    do_letize exp;
    !let_nodes

(* strips the _number off a string *)
let stripnum1 = Str.replace_first (Str.regexp "_[0-9]+$") ""
    
(** Build a VinE expression for the given STP expression.

    @param exp The expression to translate.
    @return the translated VinE expression.
*)
let stp_to_vine ?(strip_nums=false) ?(ctx=Hashtbl.create 17) exp =
  let let_hash = Hashtbl.create 17 in
  let gamma = ctx in (* variable name hash *)
  let stp_to_var e = (* premise: e is var *)
    let vname = get_name e in
      try (Hashtbl.find gamma vname)
      with Not_found ->
	let vname = if strip_nums then stripnum1 vname else vname in
	let nvar = newvar vname (stp_to_type e) in
	  Hashtbl.add gamma vname nvar;
	  nvar
  in
  let rec stp_to_memvar e =
    try Hashtbl.find let_hash (get_id e)
    with Not_found -> 
      match get_kind e with
	  Libstp.SYMBOL ->
	    stp_to_var e
	| Libstp.WRITE ->
	    (* FIXME: This doesn't seem right. --aij *)
	    stp_to_memvar (get_child e 0)
	| _ -> failwith "stp_to_exp: Unexpected type of memory variable."
  in
  let is_zero e = (* return true if e is 0 *)
    match get_kind e with
	Libstp.BVCONST -> (int64_of_e e) = 0L
      | _ -> false
  in
  let rec extract front back stp_e =
    match get_kind stp_e with
	Libstp.BVEXTRACT ->
	  let nested_exp = get_child stp_e 0 in
	  let nested_back = int_of_e (get_child stp_e 2) in
	    extract (front + nested_back) (back + nested_back) nested_exp
      | _ ->
	  let ve = stp_to_exp stp_e in
	  let vet = Vine_typecheck.infer_type None ve in
	    match (vet, front, back) with
		(REG_64, 63, 0)
	      | (REG_32, 31, 0) 
	      | (REG_16, 15, 0)
	      | (REG_8, 7, 0)
	      | (REG_1, 0, 0) -> ve
	      | (_, 31, 0) -> Cast(CAST_LOW, REG_32, ve)
	      | (_, 15, 0) -> Cast(CAST_LOW, REG_16, ve)
	      | (_, 7, 0) -> Cast(CAST_LOW, REG_8, ve)
	      | (_, 0, 0) -> Cast(CAST_LOW, REG_1, ve)
	      | (REG_32, 31, 16) -> Cast(CAST_HIGH, REG_16, ve)
	      | (REG_32, 31, 24) -> Cast(CAST_HIGH, REG_8, ve)
	      (* why not add this?
		 | (REG_32, 31, 31) -> Cast(CAST_HIGH, REG_1, ve) *)
	      | (REG_32, 23, 16) -> Cast(CAST_LOW, REG_8,
					Cast(CAST_HIGH,REG_16, ve))
	      | (REG_32, 15, 8) -> Cast(CAST_HIGH, REG_8,
				       Cast(CAST_LOW,REG_16, ve))
	      | (REG_16, 15, 8) -> Cast(CAST_HIGH, REG_8, ve)
	      | _ ->
		  let rec bitmask front cur =
		    if (front < back) then Int64.shift_left cur back
		    else bitmask (front-1)
		      (Int64.logor (Int64.shift_left cur 1) Int64.one)
		  in
		    BinOp(RSHIFT, BinOp(
		      BITAND, ve, Constant(
			Int (vet, bitmask front Int64.zero))),
			 Constant(Int (REG_8,Int64.of_int back)))
  and stp_to_exp e =
    try Lval(Temp(Hashtbl.find let_hash (get_id e)))
    with Not_found ->
      let ops () = List.init (get_degree e) (fun x -> x) in
      let unop_to_vine t = UnOp(t, stp_to_exp (get_child e 0)) in
      let binop_to_vine t = function
	  hd::tl -> List.fold_left 
	    (fun a b -> BinOp(t, a, stp_to_exp (get_child e b)))
	    (stp_to_exp (get_child e hd)) tl
	| [] -> failwith "stp_to_exp: BinOp with no operands"
      in
      let unsigned_up_type ve =
	let new_t = stp_to_type e in
	let old_t = Vine_typecheck.infer_type None ve in
	  if old_t = new_t then ve else Cast(CAST_UNSIGNED, new_t, ve)
      in
      let up_type ve =
	let new_t = stp_to_type e in
	let old_t = Vine_typecheck.infer_type None ve in
	  if old_t = new_t then ve else Cast(CAST_SIGNED, new_t, ve)
      in
      let down_type ve =
	let new_t = stp_to_type e in
	let old_t = Vine_typecheck.infer_type None ve in
	  if old_t = new_t then ve else Cast(CAST_LOW, new_t, ve)
      in
      let stp_to_array mem_var nested_e mem_e =
	let rec get_let nested_e mem_e =
	  try 
	    if Hashtbl.find let_hash (get_id mem_e) = mem_var then
	      nested_e
	    else
	      failwith "stp_to_array: Inconsistent let variables."
	  with Not_found ->
	    match get_kind mem_e with
		Libstp.SYMBOL ->
		  nested_e 
	      | Libstp.WRITE ->
		  let m = get_child mem_e 0 in
		  let loc = stp_to_exp (get_child mem_e 1) in
		  let store = stp_to_exp (get_child mem_e 2) in
		    (
		      match mem_var with
			  (_,_,Array((t1,_))) ->
			    get_let (Let(Mem(mem_var,loc,t1),store,nested_e)) m
			| _ -> failwith "Invalid mem var"
		    )
	      | _ -> failwith "stp_to_exp: Unexpected type of memory variable."
	in
	  get_let nested_e mem_e
      in
      let result_e =
	match get_kind e with
	    Libstp.SYMBOL -> Lval(Temp(stp_to_var e))
	  | Libstp.BVCONST -> 
	      Constant( Int (stp_to_type e,int64_of_e e))
	  | Libstp.BVNEG | Libstp.NOT -> unop_to_vine NOT
	  | Libstp.BVUMINUS -> unop_to_vine NEG
	  | Libstp.OR | Libstp.BVOR -> binop_to_vine BITOR (ops ())
	  | Libstp.AND | Libstp.BVAND -> binop_to_vine BITAND (ops ())
	  | Libstp.XOR | Libstp.BVXOR -> binop_to_vine XOR (ops ())
	  | Libstp.BVPLUS -> binop_to_vine PLUS (ops ())
	  | Libstp.BVSUB -> binop_to_vine MINUS (ops ())
	  | Libstp.BVMULT -> binop_to_vine TIMES (ops ())
	  | Libstp.BVDIV -> binop_to_vine DIVIDE (ops ())
	  | Libstp.BVMOD -> binop_to_vine MOD (ops ())
	  | Libstp.SBVDIV -> binop_to_vine SDIVIDE (ops ())
	  | Libstp.SBVMOD -> binop_to_vine SMOD (ops ())
	  | Libstp.BVLT -> binop_to_vine LT (ops ())
	  | Libstp.BVLE -> binop_to_vine LE (ops ())
	  | Libstp.BVGT -> binop_to_vine LT (List.rev (ops ()))
	  | Libstp.BVGE -> binop_to_vine LE (List.rev (ops ()))
	  | Libstp.BVSLT -> binop_to_vine SLT (ops ())
	  | Libstp.BVSLE -> binop_to_vine SLE (ops ())
	  | Libstp.BVSGT -> binop_to_vine SLT (List.rev (ops ()))
	  | Libstp.BVSGE -> binop_to_vine SLE (List.rev (ops ()))
	  | Libstp.EQ -> binop_to_vine EQ (ops ())
	  | Libstp.NEQ -> binop_to_vine NEQ (ops ())
	  | Libstp.FALSE -> exp_false
	  | Libstp.TRUE -> exp_true
	  | Libstp.NAND | Libstp.BVNAND -> 
	      UnOp(NOT, binop_to_vine BITAND (ops ()))
	  | Libstp.NOR | Libstp.BVNOR -> 
	      UnOp(NOT, binop_to_vine BITOR (ops ()))
	  | Libstp.IFF | Libstp.BVXNOR -> UnOp(NOT, binop_to_vine XOR (ops ()))
	  | Libstp.IMPLIES -> (* right associative *)
	      (match List.rev (List.init (get_degree e) (fun x -> x)) with
		  [] -> failwith "stp_to_exp: IMPLES with no operands"
		| h::t -> List.fold_left (fun a b ->
		    BinOp(BITOR, UnOp(NOT, stp_to_exp (get_child e b)), a))
		    (stp_to_exp (get_child e h)) t)
	  | Libstp.BVLEFTSHIFT -> unsigned_up_type (binop_to_vine LSHIFT (ops ()))
	  | Libstp.BVRIGHTSHIFT -> down_type (binop_to_vine RSHIFT (ops ()))
	  | Libstp.BVSX -> up_type (stp_to_exp (get_child e 0))
	  | Libstp.BVEXTRACT ->
	      let front = int_of_e (get_child e 1) in
	      let back = int_of_e (get_child e 2) in
		down_type (extract front back (get_child e 0))
	  | Libstp.BVGETBIT ->
	      let loc = int_of_e (get_child e 1) in
		down_type (extract loc loc (get_child e 0))
	  | Libstp.READ ->
	      let m = get_child e 0 in
	      let loc = stp_to_exp (get_child e 1) in
	      let memvar = stp_to_memvar m in
		( match memvar with
		    (_,_,Array(t1,_)) ->
		      stp_to_array memvar (Lval(Mem(memvar,loc,t1))) m
		  | _ -> failwith "Bad mem var" )
	  | Libstp.WRITE ->
	      let memvar = stp_to_memvar (get_child e 0) in
		stp_to_array memvar (Lval(Temp memvar)) e
	  | Libstp.BVCONCAT -> (
	      let e1' = get_child e 0 in
	      let e2' = get_child e 1 in
		match (is_zero e1', is_zero e2') with
		    (true, true) -> Constant( Int(stp_to_type e, 0L))
		  | (true, false) -> 
		      let e2 = stp_to_exp e2' in
			unsigned_up_type e2
		  | (false, true) ->
		      let e1 = stp_to_exp e1' in
		      let bvl2 = get_bvlength e2' in
			BinOp(LSHIFT, unsigned_up_type e1, 
			     Constant(Int (REG_8, Int64.of_int bvl2)))
		  | (false, false) ->
		      let e1 = stp_to_exp e1' in
		      let e2 = stp_to_exp e2' in
		      let bvl2 = get_bvlength e2' in
			BinOp(BITOR, BinOp(
			  LSHIFT, unsigned_up_type e1, Constant(
			    Int (REG_8,Int64.of_int bvl2))), 
			     unsigned_up_type e2)
	    )
	  | Libstp.ITE -> (* boolean is always 1 bit *)
	      let e1 = get_child e 1 in
	      let e2 = get_child e 2 in
	      let true_e = stp_to_exp e1 in
	      let false_e = stp_to_exp e2 in
	      let cond = stp_to_exp (get_child e 0) in
		(match (true_e, false_e) with
		    (Constant(Int(REG_1, 1L)),Constant(Int(REG_1, 0L))) -> 
		      cond
		  | (Constant(Int(REG_1, 0L)),Constant(Int(REG_1, 1L))) -> 
		      UnOp(NOT, cond)
		  | _ ->
		      let typ = Vine_typecheck.infer_type None true_e in
			exp_ite cond typ true_e false_e
		)
	  | Libstp.ARRAY ->
	      let m = get_child e 0 in
	      let loc = stp_to_exp (get_child e 1) in
	      let memvar = stp_to_memvar m in
		( match memvar with 
                      (_,_,Array(t1,_)) -> Lval(Mem(memvar, loc, t1))
                    | _ -> failwith "Bad mem var"
                )
	  | Libstp.BITVECTOR ->
	      failwith "vine_stpvc: translation from BITVECTOR unsupported"
	  | Libstp.BOOLEAN ->
	      failwith "vine_stpvc: translation from BOOLEAN unsupported"
	  | Libstp.UNDEFINED -> 
	      failwith "vine_stpvc: translation from UNDEFINED unsupported"
	  | Libstp.BVSRSHIFT ->
	      failwith "vine_stpvc: translation from BVSRSHIFT unsupported"
	  | Libstp.BVVARSHIFT ->
	      failwith "vine_stpvc: translation from BVVARSHIFT unsupported"
	  | Libstp.BVMULTINVERSE ->
	      failwith "vine_stpvc: translation from BVMULTINVERSE unsupported"
	  | Libstp.BOOLVEC ->
	      failwith "vine_stpvc: translation from BOOLVEC unsupported"
      in
	result_e
  in
  let let_nodes = List.rev (letize exp) in
  let let_pairs = List.rev_map (fun (lvar, lsexp) ->
    let lvexp = stp_to_exp lsexp in
      Hashtbl.add let_hash (get_id lsexp) lvar;
      (lvar, lvexp)) let_nodes
  in
    List.fold_left (fun a (lvar, lvexp) -> Let(Temp lvar, lvexp, a))
      (stp_to_exp exp) let_pairs

(**
   VinE interface to vc_simplify

   Note: There is no need to create a new validity checker and a new context
   everytime you simplify an expression.

   @param vc The STP validity checker. The result will be used in. 
   The VC passed
   here must be the same as the one used to create the context. (We could
   just include it in the context.) 
   @param ctx The context created by new_ctx.
   @param e The expression to translate.
   @return the simplified VinE expression.
*)
let vc_simplify vc ctx e =
  let stp_expr = vine_to_stp vc ctx e in
  let vine_expr = stp_to_vine stp_expr in
    vine_expr
	
(* These should go elsewhere, if they are still being used 
   

(** takes in a C++ expression and return an STP expression. You can
    then query STP directly with the handle.
    @param vc - a validity context
    @param e - The passed in C++ handle (via libasmir stub)
    @return - Libasmir.expr stp handle and the validity checker
*)
let cpp_exp_to_stp_handle (vc:Libstp.vc) (e:Libasmir.exp) : bool = 
  let g = Asmir.gamma_create Asmir.x86_mem Asmir.x86_regs in
  let e = Asmir.tr_exp g e in 
  let ctx = new_ctx vc (Vine.get_req_ctx e) in
  let stp_exp = vine_to_stp vc ctx e in 
    Stpvc.query vc (Stpvc.e_not vc
			  (Stpvc.e_bvbitextract vc stp_exp 0))
;;

(** takes in a C++ expression and return an STP expression. You can
    then query STP directly with the handle.
    @param vc  a validity context
    @param e The passed in C++ handle (via libasmir stub)
    @return  Libasmir.expr stp handle and the validity checker
*)
let cpp_exp_to_stp_deend_handle (vc:Libstp.vc) (e:Libasmir.exp) : bool = 
  let g = Asmir.gamma_create Asmir.x86_mem Asmir.x86_regs in
  let e = Asmir.tr_exp g e in 
  let vis =
    new Vine_memory2array.memory2array_visitor
      Asmir.x86_mem Vine_memory2array.LittleEndian Asmir.x86_mem in
  let e' = (Vine.exp_accept vis e) in
  let ctx = new_ctx vc (Vine.get_req_ctx e') in
  let stp_exp = vine_to_stp vc ctx e' in 
  let query = (Stpvc.e_not vc (Stpvc.e_bvbitextract vc stp_exp 0)) in 
  let oc = open_out "foo.txt" in 
  let () = Stp.to_file oc e' in
  let () = close_out oc in 
      Stpvc.query vc (Stpvc.e_not vc
			 (Stpvc.e_bvbitextract vc stp_exp 0))
;;

(** takes in a C++ expression and return an STP expression. You can
    then query STP directly with the handle.
    @param vc  a validity context
    @param e  The passed in C++ handle (via libasmir stub)
    @return  Libasmir.expr stp handle and the validity checker
*)
let cpp_exp_to_stp_string (vc:Libstp.vc) (e:Libasmir.exp) : string = 
  let e = Asmir.tr_exp e in 
  let vis = new Vine_memory2array.memory2array_visitor (
      "mem", Vine_memory2array.LittleEndian, Vine.Temp("mem", Vine.Array(REG_32, REG_8))) in
  let e' = (Vine.exp_accept vis e) in
  let ctx = new_ctx vc (Vine.get_req_ctx e') in
  let stp_exp = vine_to_stp vc ctx e' in 
  (*
  let query = (Stpvc.e_not vc (Stpvc.e_bvbitextract vc stp_exp 0)) in 
  let oc = open_out "foo.txt" in 
  let () = Stp.to_file oc e' in
  let () = close_out oc in 
    Stpvc.query vc (Stpvc.e_not vc
			  (Stpvc.e_bvbitextract vc stp_exp 0))
    *)
  Stp.to_string e'
;;

(** takes in a C++ expression and return an STP expression. You can
    then query STP directly with the handle.
    @param vc  a validity context
    @param e  The passed in C++ handle (via libasmir stub)
    @return  Libasmir.expr stp handle and the validity checker
    @filename Filename to write STP file to
*)
let cpp_exp_to_stp_file (e:Libasmir.exp) (f:string) (q:int): int = 
  let e = Asmir.tr_exp e in 
  let vis = new Vine_memory2array.memory2array_visitor (
      "mem", Vine_memory2array.LittleEndian, Vine.Temp("mem", Vine.Array(REG_32, REG_8))) in
  let e' = (Vine.exp_accept vis e) in
  let fd= open_out f in
  let () = Stp.to_file fd e' in
  let () = if q=1 then Printf.fprintf fd "\nQUERY FALSE;\nCOUNTEREXAMPLE;\n%!"
	    else () in
  (*let () = Printf.fprintf fd "\nQUERY FALSE;\nCOUNTEREXAMPLE;\n%!" in*)
  close_out fd; 1
;;

(** 
    @param e The passed in C++ handle (via libasmir stub)
    @return  boolean true iff for all value to variables, false otherwise 
*)
let query_stp_cpp_exp (e:Libasmir.exp) : bool =
  let e = Asmir.tr_exp e in 
  let vc = create_validity_checker () in
  let ctx = new_ctx vc (Vine.get_req_ctx e) in
  let stp_exp = vine_to_stp vc ctx e in 
    Stpvc.query vc (Stpvc.e_not vc
			  (Stpvc.e_bvbitextract vc stp_exp 0))
;;


(** register a callback to invoke query_stp_exp via C *)    
let _ = Callback.register "query_stp_cpp_exp" query_stp_cpp_exp;;

(** register a callback to invoke c_exp_to_stp_handle via C *)    
let _ = Callback.register "cpp_exp_to_stp_handle" cpp_exp_to_stp_handle;;

(** register a callback to invoke c_exp_to_stp_handle via C *)    
let _ = Callback.register "cpp_exp_to_stp_deend_handle" cpp_exp_to_stp_deend_handle;;

(** register a callback to invoke c_exp_to_stp_handle via C *)    
let _ = Callback.register "cpp_exp_to_stp_string" cpp_exp_to_stp_string;;

(** register a callback to invoke c_exp_to_stp_handle via C *)    
let _ = Callback.register "cpp_exp_to_stp_file" cpp_exp_to_stp_file;;

*)

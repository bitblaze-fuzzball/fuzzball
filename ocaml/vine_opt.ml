(** 
    Functions to optimize vine expressions.
    
    Basically, constant_fold will only perform constant folding, whereas
    simplify() will also perform alpha substitution.
    
    Let me know if you use this, because I might be changing the
    interface slightly.

    @author Ivan Jager
 *)

open ExtList
open Vine
open Vine_util

module D = Debug.Make(struct let name = "Vine_opt" and default=`NoDebug end)
open D

module VH = Vine.VarHash


type alias = MayAlias | DoesAlias | PartialAlias | NoAlias


(* some helper functions *)


(* drop high bits *)
let to64 v =
    match v with
	Int(t,i) ->   let bits = 64 - bits_of_width t in
	  Int64.shift_right_logical (Int64.shift_left i bits) bits
      | _ -> raise (Invalid_argument "to64 is only for integers")

(* sign extend to 64 bits*)
let tos64 v =
    match v with
	Int(t,i) ->   let bits = 64 - bits_of_width t in
	  Int64.shift_right (Int64.shift_left i bits) bits
      | _ -> raise (Invalid_argument "tos64 is only for integers")
  
(* shifting by more than the number of bits or by negative values
 * will be the same as shifting by the number of bits. *)
let toshift max t v =
  let i = to64  v in
    if i <= Int64.of_int max && i >= 0L
    then Int64.to_int i
    else
      (prerr_endline("Warning: shifting "^string_of_int max^"-bit value by "
		    ^Int64.to_string i);
       max)

(* "cast" an int64 to a value *)
let to_val t v =
  let mask = Int64.shift_right_logical (-1L) (64-bits_of_width t) in
    Constant(Int(t,Int64.logand mask v))


(* flatten an expression to make it easier to rearange binops
 * Note: Should only be used with associative binops. *)
let rec flatten_binop op exp =
  match exp with
      BinOp(o, e1, e2) when o = op ->
	flatten_binop op e1 @ flatten_binop op e2
    | _ -> [exp]



(* exp_eq : exp -> exp -> bool option *)
(** Check if two expression are equal

    Note: When this function says two expressions are equivalent, it means they
    are equivalent if they appear in the same context or equivalent contexts.

    FIXME: This function could be more accurate if it also took the two
    contexts as arguments. (Eg, when e1 references x, and e2 references y, but
    in e1's context x is the same as the y is in e2's context.

    @return Some true if e1 \equiv e2,
    Some false, if e1 is never = e2,
    or None if we can't determine it yet. *)
let exp_eq e1 e2 =
  (* FIXME: canonicalize expressions so that they can be compared better *)
  match (e1, e2) with
      (Constant(v1), Constant(v2)) when v1 = v2 -> Some true
    | (Constant _, Constant _) -> Some false
    | _ when e1 = e2 -> Some true
    | _ -> None
    
let flip_bool e =
  match e with
    | UnOp(NOT, e) -> e
    | _ -> UnOp(NOT, e)
      
(** Performs simple contstant folding without recursing.
    (ie. only on the toplevel expression)
    Use constant_simplify instead if you want to also rearange expressions.
    @param ctx The context is a function from lval to exp option. When it
    return some, the lval will be replaced by the returned expression. Be
    careful to make sure that said expression does not reference any shadowed
    variables.
    @param e The expression to preform constant folding on.
    @return an equivalent, but usually smaller expression.
*)
let rec constant_fold ctx e =
  match e with
      BinOp(MINUS, e1, e2) ->
	constant_fold ctx (BinOp(PLUS, e1, constant_fold ctx (UnOp(NEG, e2))))

    | BinOp(bop, Constant(Int(t,_) as v1), Constant(Int(t2,_) as v2)) ->
	(* let () = assert (t = t2) in *)
	(* let _ = Vine_typecheck.infer_type None e in *)
	  (match bop with
	      PLUS -> to_val t (Int64.add (to64 v1) (to64 v2))
	    | MINUS -> to_val t (Int64.sub (to64 v1) (to64 v2))
	    | TIMES -> to_val t (Int64.mul (to64  v1) (to64  v2))
	    | BITAND -> to_val t (Int64.logand (to64  v1) (to64  v2))
	    | BITOR -> to_val t (Int64.logor (to64  v1) (to64  v2))
	    | XOR -> to_val t (Int64.logxor (to64  v1) (to64  v2))
	    | EQ ->
		exp_bool((to64 v1) = (to64 v2))
	    | NEQ ->
		exp_bool((to64 v1) <> (to64 v2))

(* FIXME: Enabling these seems to break stuff.
   Do they still? Someone uncommented them.
 *)
	    | LSHIFT ->
		to_val t (Int64.shift_left
			    (to64 v1) 
			    (toshift (bits_of_width t) t2 v2) )
	    | RSHIFT ->
		to_val t (Int64.shift_right_logical
			    (to64 v1)
			    (toshift (bits_of_width t) t2 v2) )
	    | ARSHIFT ->
		to_val t (Int64.shift_right
			    (tos64  v1)
			    (toshift (bits_of_width t) t2 v2) )



   (* Int64.div rounds towards zero. What do we want? *)
	    | DIVIDE -> to_val t (int64_udiv (tos64  v1) (tos64 v2))
	    | SDIVIDE -> to_val t (Int64.div (tos64 v1) (tos64  v2))
	    | MOD -> to_val t (int64_urem (tos64 v1) (tos64 v2))
	    | SMOD -> to_val t (Int64.rem (tos64 v1) (tos64 v2))
	    | SLT -> exp_bool(tos64  v1 < tos64 v2)
	    | SLE -> exp_bool(tos64  v1 <= tos64  v2)
	    | LT -> exp_bool(int64_ucompare (to64 v1) (to64 v2) < 0)
	    | LE -> exp_bool(int64_ucompare (to64 v1) (to64 v2) <= 0)

	    (* NB: Vine_ceval assumes this covers all operators. *)
	 )
    (*  some identities *)
    | BinOp(BITAND, _, (Constant(Int(_, 0L)) as c)) ->
	c
    | BinOp(BITAND, x, Constant(v)) when tos64 v = -1L ->
	x
    | BinOp(BITOR, x, (Constant(Int(_, 0L)))) ->
	x
    | BinOp(BITOR, _, (Constant(v) as c)) when tos64 v = -1L ->
	c
    | BinOp(XOR, x, (Constant(Int(_, 0L)))) ->
	x
    | BinOp(PLUS, x, (Constant(Int(_, 0L)))) ->
	x
    | BinOp(LSHIFT, x, (Constant(Int(_, 0L)))) ->
	x
    | BinOp(RSHIFT, x, (Constant(Int(_, 0L)))) ->
	x
    | BinOp(ARSHIFT, x, (Constant(Int(_, 0L)))) ->
	x
    | BinOp(TIMES, x, (Constant(Int(_, 1L)))) ->
	x
    | BinOp((DIVIDE|SDIVIDE), x, (Constant(Int(_, 1L)))) ->
	x
    | BinOp((MOD|SMOD), x, (Constant(Int(ty, 1L)))) ->
	Constant(Int(ty, 0L))
    | UnOp(op, Constant(Int(t,_) as v)) ->
	(match op with
	    NEG -> to_val t (Int64.neg (to64  v))
	  | NOT -> to_val t (Int64.lognot (to64  v))
	)
    | UnOp(NOT, BinOp(PLUS, e1, Constant(Int(_, -1L)))) ->
	UnOp(NEG, e1)
    | UnOp(NEG, UnOp(NEG, x)) ->
	x
    | UnOp(NOT, UnOp(NOT, x)) ->
	x

    | Cast(ct, t2, Constant(Int(t,_) as v)) ->
	let bits1 = bits_of_width t in
	let bits = bits_of_width t2 in
	(match ct with
	    CAST_UNSIGNED ->
	      to_val t2 (to64  v)
	  | CAST_SIGNED ->
	      to_val t2 (tos64  v)
	  | CAST_HIGH ->
     	      to_val t2
		(Int64.shift_right 
		    (Int64.logand (to64  v)
			(Int64.shift_left (-1L) (bits1-bits)) )
		    (bits1-bits) )
	  | CAST_LOW ->
	      to_val t2
		(Int64.logand (to64  v)
		    ((Int64.lognot(Int64.shift_left (-1L) bits))) )
	)
    | Cast(ct2, w2, Cast(ct1, w1, e1)) when ct1 = ct2 ->
	Cast(ct1, w2, e1) (* no redundant double casts *)
    (* Redundant widening inside narrowing *)
    | Cast(CAST_LOW, t2, Cast((CAST_SIGNED|CAST_UNSIGNED), t1, e))
	when (let w_e = bits_of_width (Vine_typecheck.infer_type None e) and
		  w_2 = bits_of_width t2 in
		w_2 <= w_e) ->
	Cast(CAST_LOW, t2, e)
    (* Widening cast followed by narrowing cast with same type *)
    | Cast(CAST_LOW,t2,Cast(CAST_UNSIGNED,_,e)) 
	when ((Vine_typecheck.infer_type None e) = t2) -> e
    | Cast(CAST_LOW,t2,Cast(CAST_SIGNED,_,e)) 
	when ((Vine_typecheck.infer_type None e) = t2) -> e
    (* Widening followed by narrowing equivalent to narrower widening *)
    | Cast(CAST_LOW, t1, Cast((CAST_SIGNED|CAST_UNSIGNED) as cast_ty, t2, e))
	when (let w1 = bits_of_width t1 and
		  w3 = bits_of_width (Vine_typecheck.infer_type None e) in
		w3 <= w1) ->
	Cast(cast_ty, t1, e)
    (* High cast selects only the zero bits from of an unsigned cast *)
    | Cast(CAST_HIGH, t1, Cast(CAST_UNSIGNED, t2, e3))
	when (let w1 = bits_of_width t1 and
		  w2 = bits_of_width t2 and
		  w3 = bits_of_width (Vine_typecheck.infer_type None e3) in
		w1 <= (w2 - w3))
	  -> Constant(Int(t1, 0L))
    (* Boolean -> integer -> boolean conversion with == 0 *)
    | BinOp(EQ, Cast((CAST_SIGNED|CAST_UNSIGNED), _, e),
	    Constant(Int(_, 0L)))
	when ((Vine_typecheck.infer_type None e) = REG_1)
      -> (flip_bool e)
    (* Cast followed by AND operation that cancels effect of cast *)
    | BinOp(BITAND,Cast(CAST_UNSIGNED, REG_32, e),Constant(Int(REG_32,0xffL))) 
	when ((Vine_typecheck.infer_type None e) = REG_8) ->
	  Cast(CAST_UNSIGNED, REG_32, e)
    | BinOp(BITAND,Cast(CAST_UNSIGNED, REG_32, e),Constant(Int(REG_32,0xffffL))) 
	when ((Vine_typecheck.infer_type None e) = REG_16) ->
	  Cast(CAST_UNSIGNED, REG_32, e)
    | BinOp(BITAND,Cast(CAST_SIGNED, REG_32, e),Constant(Int(REG_32,0xffL))) 
	when ((Vine_typecheck.infer_type None e) = REG_8) ->
	  Cast(CAST_UNSIGNED, REG_32, e)
    | BinOp(BITAND,Cast(CAST_SIGNED, REG_32, e),Constant(Int(REG_32,0xffffL))) 
	when ((Vine_typecheck.infer_type None e) = REG_16) ->
	  Cast(CAST_UNSIGNED, REG_32, e)
    (* Cast followed by left shift followed by unnecessary AND operation *)
    | BinOp(BITAND,(BinOp(LSHIFT,Cast(CAST_UNSIGNED, REG_32, e),
	Constant(Int(REG_8,0x8L)))),Constant(Int(REG_32,0xff00L))) 
	when ((Vine_typecheck.infer_type None e) = REG_8) ->
	  BinOp(LSHIFT,Cast(CAST_UNSIGNED, REG_32, e),
	    Constant(Int(REG_32,0x8L))) 
    | BinOp(BITAND,(BinOp(LSHIFT,Cast(CAST_UNSIGNED, REG_32, e),
	Constant(Int(REG_8,0x10L)))),Constant(Int(REG_32,0xff0000L))) 
	when ((Vine_typecheck.infer_type None e) = REG_8) ->
	  BinOp(LSHIFT,Cast(CAST_UNSIGNED, REG_32, e),
	    Constant(Int(REG_32,0x10L))) 
    | BinOp(BITAND,(BinOp(LSHIFT,Cast(CAST_UNSIGNED, REG_32, e),
	Constant(Int(REG_8,0x18L)))),Constant(Int(REG_32,0xff000000L))) 
	when ((Vine_typecheck.infer_type None e) = REG_8) ->
	  BinOp(LSHIFT,Cast(CAST_UNSIGNED, REG_32, e),
	    Constant(Int(REG_32,0x18L))) 
    (* Redundat cast and AND *)
    | Cast(CAST_LOW, REG_8, BinOp(BITAND, e, Constant(Int(_,0xffL)))) ->
	Cast(CAST_LOW, REG_8, e)
    | Cast(CAST_LOW, REG_16, BinOp(BITAND, e, Constant(Int(_,0xffffL)))) ->
	Cast(CAST_LOW, REG_16, e)
    | Cast(CAST_LOW, REG_32, BinOp(BITAND, e, Constant(Int(_,0xffffffffL)))) ->
	Cast(CAST_LOW, REG_32, e)
    (* A more complex way of writing sign extension *)
    | BinOp(BITOR, Cast(CAST_UNSIGNED, REG_64, e1),
	    BinOp(LSHIFT,
		  Cast(CAST_UNSIGNED, REG_64,
		       BinOp(ARSHIFT, e2, Constant(Int(_, 31L)))),
		  Constant(Int(_, 32L))))
	when e1 = e2 && ((Vine_typecheck.infer_type None e1) = REG_32)
	  -> Cast(CAST_SIGNED, REG_64, e1)
    | BinOp(BITOR, Cast(CAST_UNSIGNED, REG_32, e1),
	    BinOp(LSHIFT,
		  Cast(CAST_UNSIGNED, REG_32,
		       BinOp(ARSHIFT, e2, Constant(Int(_, 15L)))),
		  Constant(Int(_, 16L))))
	when e1 = e2 && ((Vine_typecheck.infer_type None e1) = REG_16)
	  -> Cast(CAST_SIGNED, REG_32, e1)
    | BinOp(BITOR, Cast(CAST_UNSIGNED, REG_16, e1),
	    BinOp(LSHIFT,
		  Cast(CAST_UNSIGNED, REG_16,
		       BinOp(ARSHIFT, e2, Constant(Int(_, 7L)))),
		  Constant(Int(_, 8L))))
	when e1 = e2 && ((Vine_typecheck.infer_type None e1) = REG_8)
	  -> Cast(CAST_SIGNED, REG_16, e1)
    | Cast(ct, ty, e) when (Vine_typecheck.infer_type None e) = ty ->
	e
    | Lval l ->
	(match ctx l with
	    Some c -> c
	   | None -> e
	)
    | Ite(((Constant(Int(_,_))) as cond), e1, e2) ->
	if (bool_of_const cond) then e1 else e2
    | Ite(cond, e1, e1') when e1 = e1' -> e1
    (* AND / OR with itself *)
    | BinOp(BITOR, x, y)
    | BinOp(BITAND, x, y)
	when x = y ->
	x
    | BinOp((EQ|LE|SLE), x, y)
	when x = y ->
	exp_true 
    | BinOp((NEQ|LT|SLT), x, y)
	when x = y ->
	exp_false
    (* Repeated shifts of the same type *)
    | BinOp(LSHIFT, BinOp(LSHIFT, x, Constant(Int(ty1, s1))),
	    Constant(Int(ty2, s2)))
	when ty1 = ty2 && s1 >= 0L && s2 >= 0L ->
	BinOp(LSHIFT, x, (Constant(Int(ty1, (Int64.add s1 s2)))))
    | BinOp(RSHIFT, BinOp(RSHIFT, x, Constant(Int(ty1, s1))),
	    Constant(Int(ty2, s2)))
	when ty1 = ty2 && s1 >= 0L && s2 >= 0L ->
	BinOp(RSHIFT, x, (Constant(Int(ty1, (Int64.add s1 s2)))))
    | BinOp(ARSHIFT, BinOp(ARSHIFT, x, Constant(Int(ty1, s1))),
	    Constant(Int(ty2, s2)))
	when ty1 = ty2 && s1 >= 0L && s2 >= 0L ->
	BinOp(ARSHIFT, x, (Constant(Int(ty1, (Int64.add s1 s2)))))
    (* byte & 0xffffff00 = 0 *)
    | BinOp(BITAND,
	    Cast(CAST_UNSIGNED, REG_32, e),
	    Constant(Int(REG_32, 0xffffff00L)))
	when (Vine_typecheck.infer_type None e) = REG_8 ->
	Constant(Int(REG_32, 0L))
    (* byte >> amt = 0  when amt >= 8 *)
    | BinOp(RSHIFT,
	    Cast(CAST_UNSIGNED, REG_32, e),
	    Constant(Int(_, amt)))
	when (Vine_typecheck.infer_type None e) = REG_8 && amt >= 8L ->
	Constant(Int(REG_32, 0L))
    (* (c ? k : k + 1) = (k + (int)c) *)
    | BinOp(BITOR,
	    BinOp(BITAND, Cast(CAST_SIGNED, REG_32, c1),
		  Constant(Int(REG_32, k1))),
	    BinOp(BITAND, UnOp(NOT, Cast(CAST_SIGNED, REG_32, c2)),
		  Constant(Int(REG_32, k2))))
	when c1 = c2 && k2 = (Int64.add k1 1L) ->
	BinOp(PLUS, Constant(Int(REG_32, k1)),
	      Cast(CAST_UNSIGNED, REG_32, c1))
    (* x + -x = 0 *)
    | BinOp(PLUS, x, UnOp(NEG, y)) when x = y ->
	Constant(Int((Vine_typecheck.infer_type None x), 0L))
    (* x + x = x << 1 *)
    | BinOp(PLUS, x, y) when x = y ->
	BinOp(LSHIFT, x, Constant(Int(REG_8, 1L)))
    (* x + (x << 1) = 3 * x *)
    | BinOp(PLUS, x, BinOp(LSHIFT, y, Constant(Int(_, 1L))))
	when x = y ->
	BinOp(TIMES,
	      Constant(Int((Vine_typecheck.infer_type None x), 3L)),
	      x)
    (* x + (x << 2) = 5 * x *)
    | BinOp(PLUS, x, BinOp(LSHIFT, y, Constant(Int(_, 2L))))
	when x = y ->
	BinOp(TIMES,
	      Constant(Int((Vine_typecheck.infer_type None x), 5L)),
	      x)
    (* x + -x + y  = y *)
    | BinOp(PLUS, x, BinOp(PLUS, UnOp(NEG, y), z)) when x = y ->
	z
    (* x & !x = 0 *)
    | BinOp(BITAND, x, UnOp(NOT, y)) when x = y ->
	Constant(Int((Vine_typecheck.infer_type None x), 0L))
    (* (x & y) | (x & !y) == x *)
    | BinOp(BITOR, BinOp(BITAND, x1, y1),
	    BinOp(BITAND, x2, (UnOp(NOT, y2)))) when x1 = x2 && y1 = y2 ->
	x1
    (* x | (!x & y) == x | y *)
    | BinOp(BITOR, x1,
	    BinOp(BITAND, UnOp(NOT, x2), y)) when x1 = x2 ->
	BinOp(BITOR, x1, y)
    (* x ^ x = 0 *)
    | BinOp(XOR, x, y) when x = y ->
	Constant(Int((Vine_typecheck.infer_type None x), 0L))
    (* x ^ x ^ y = y *)
    | BinOp(XOR, x, BinOp(XOR, y, z)) when x = y ->
	z
    (* x + c = 0 -> x = -c, for c constant *)
    | BinOp(EQ, BinOp(PLUS, x, (Constant(_) as c)),
	    Constant(Int(ty, 0L))) ->
	BinOp(EQ, x, (constant_fold ctx (UnOp(NEG, c))))
    | _ -> e (* leave other expressions as they are *)


(** Performs constant folding, and canonicalizes commutative, associative
    operations to have constants on the right and be right associated. *)
let constant_fold_more ctx e =
  match e with
    | BinOp(bop, e1, e2) ->
	(match bop with
	    (* canonicalize commutative and associative binops so that constants
	       are on the right, and expressions are right associative *)
	    (* FIXME: check that the operands are not floats *)
	    (* associative and commutative binops, only on integer types *)
	    PLUS | TIMES | BITAND | BITOR | XOR
		->
		  let sort_fun a b =
		    match (a,b) with
			(Constant _, Constant _) -> compare a b
		      | (Constant _, _) -> -1
		      | (_, Constant _) -> 1
		      | _ -> compare a b
		  in
		  let l = flatten_binop bop e in
		    (match List.stable_sort sort_fun l with
		       | (x::xs) ->
			  List.fold_left
			    (fun x y -> constant_fold ctx (BinOp(bop, y, x)))
			    x xs
		      | _ -> failwith "flatten_binop returned []."
		    )
	  | EQ ->
	      (match exp_eq e1 e2 with
		  Some b -> exp_bool b
		| None -> constant_fold ctx e)
	  | NEQ ->
	      (match exp_eq e1 e2 with
		  Some b -> exp_bool (not b)
		| None -> constant_fold ctx e)
	  | _ -> constant_fold ctx e
	)
    | _ -> constant_fold ctx e

(* This was ok for arrays, but doesn't isn't really sufficient for memories.
   Eg, what should it return for x[y]:reg32_t x[y+1]:reg8_t
(** Check if two memory references alias each other *)
let rec mem_alias m1 m2 =
  let is_array_var (_,_,t) = 
    match (unwind_type t) with
	Array _ -> true
      | _ -> false
  in
  match (m1,m2) with
    | Mem(m1, a1, t1), Mem(m2,a2,t2) 
	when is_array_var m1 && is_array_var m2 -> 
	if not(Var.equal m1 m2) then NoAlias else
	(match exp_eq a1 a2 with
	    Some true -> DoesAlias
	  | Some false  -> NoAlias 
	  | None -> MayAlias
	)
    |  (Mem(m1, a1,t1), Mem(m2 ,a2,t2)) ->
	if (Var.equal m1 m2) && t1 = t2 then DoesAlias
	  else MayAlias
    | _ -> invalid_arg "mem alias requires Mem expressions"

and  substitute_mem enew mfor ein =
  let freemems = freemems_exp enew in
  let subvis =
    object
      inherit nop_vine_visitor
      method visit_exp e =
	match e with
	    Lval(Mem _ as t) when t = mfor ->
	      ChangeTo enew
	  | Let(Mem _ as t, _, _) when mem_alias t mfor <> NoAlias ->
	      SkipChildren (* be conservative *)
	  | Let(Mem _ as t, _, _)
	      when List.exists (fun x->mem_alias t x <> NoAlias) freemems ->
	      SkipChildren (* be conservative *)
	  | _ -> DoChildren
    end
  in
    exp_accept subvis ein
*)
      
let splitname = function
    Temp name  -> (name,None)
  | Mem(name,index,_) -> (name, Some index)


(** Returns a list of variable names that are shadowed within the given
    expression *)
let find_shadowed_vars e =
  let vis = object (self)
    inherit nop_vine_visitor
    val ctx = Hashtbl.create 10
    val mutable found = []

    method shadowed = found
    method visit_exp = function
	Let(l,e1,e2) ->
	  let (name,_) =  splitname l in
	    found <- name::found;
	    DoChildren
      | _ -> DoChildren
  end
  in
    ignore(exp_accept vis e);
    List.unique(vis#shadowed)

let find_freevars e =
  List.map (function Mem(v,_,_) | Temp v -> v) (freerefs_exp e)


(* Helper function for count_bindings and count_refs *)
let add_to_refcounts counts l =
      let (name,idx) = splitname l in
      let delta (dir,indir) = match idx with
	  Some _ -> (dir, indir+1)
	| None -> (dir+1, indir)
      in
      let count =
	try VH.find counts name
	with Not_found -> (0,0)
      in
	VH.replace counts name (delta count)
  

let count_bindings e =
  let bindings = VH.create 100 in
  let vis = object
    inherit nop_vine_visitor
    method visit_binding (l,e) =
      add_to_refcounts bindings l;
      DoChildren
  end in
    ignore(exp_accept vis e);
    bindings

let count_refs e =
  let refs = VH.create 100 in
  let vis = object
    inherit nop_vine_visitor
    method visit_rlvalue l =
      add_to_refcounts refs l;
      DoChildren
  end in
    ignore(exp_accept vis e);
    refs


class simplifying_visitor simplify (subst_single: lvalue -> exp -> bool) =
  let simplify_alvalue =
    let vis = object
      inherit nop_vine_visitor
      method visit_exp e = ChangeTo(simplify e)
    end
    in
      alvalue_accept vis
  in
object (self)
  inherit nop_vine_visitor

  (* variables that we can force substitution on as soon as we see them,
     because they only appear once in the entire expression, and aren't
     shadowed. Others may require more analysis. *)
  val forced_vars = VH.create 64

  method compute_forced e =
    VH.clear forced_vars;
    let refs = count_refs e in
    let binds = count_bindings e in
      VH.iter
	(fun name -> function 
	     (1,0) -> (
	       if try VH.find refs name = (1,0) with Not_found -> false
	       then VH.add forced_vars name ()
	       else ()
	     )
	   | (dir,indir) ->
	       dprintf
		 "Not marking '%s' for substitution yet, because it is bound in %d + %d places."
		 (var_to_string name) dir indir
	)
	binds
  (* context containing bound variables *)
  val ctx = VH.create 64

  (* Extend the context.
     Returns a function to put the old context back, and wrap a Let
     around it's argument if needed, in additon to visiting the
     subexpressions. *)
  method extend lval e1 =
    let (name,idx) = splitname lval in
    let refcount = ref 0 in
    let indref = ref 0 in (* number of indirect references *)
    let force =  idx = None && VH.mem forced_vars name  in
    let (lval,e1) = 
      if force then (alvalue_accept self lval, exp_accept self e1)
      else (lval, e1)
    in
    let unextend x =
      VH.remove ctx name;
      match (!refcount, !indref, force) with
	  (0,0,false) -> (* drop let expression *)
	    dprintf "Dropping unused binding of '%s'" (lval_to_string lval);
	    x
	| (0,0,true) ->
	    x
	| (1,0,false)
	    when subst_single lval e1  &&
	      not(list_does_intersect (find_freevars e1) (find_shadowed_vars x))
	      ->
	    (* force substitution *)
	    (* This can't be forced when variables are shadowed,
	       in particular, note that memory is very often shadowed
	    *)
	    let () = dprintf "Forcing substitution of '%s' with 1 ref" (lval_to_string lval) in
	    let (name,idx) = splitname(alvalue_accept self lval) in
	    let e1 = exp_accept self e1 in
	    let () = VH.add ctx name (idx, e1, refcount, indref, true) in
	    let res = exp_accept self x in
	      VH.remove ctx name;
	      res
	| (_,_,false) ->
	    Let(alvalue_accept self lval, exp_accept self e1, x)
	| (r,ir,true) -> 
	    Printf.printf "Forced, but got back %d %d\n%!" r ir;
	    failwith "Forced substitution, but refcount not 0"
    in
      VH.add ctx name (idx, e1, refcount, indref, force);
      unextend

  (* look up a replacement for the lval, and mark it/them as being referenced
     so that they are not dropped *)
  method lookup lval =
    let ((_,_,t) as name, index) = splitname lval in
    let relevant_ctx = VH.find_all ctx name in
      (* for debugging
    let () = print_string("Looking up '"^name^"' this relevant context:\n") in
    let () =
      List.iter (fun (idx,e1,drop) ->
		   print_string("  ["^ (match idx with None -> ""
					 | Some e -> exp_to_string e)
				^"] "^(if !drop then "drop" else "keep")^ "\n"))
	relevant_ctx
    in
      *)
      (* If lval is a temp, we need to flag the temp itself, as well as any
	 assignments into indices in it. (in the case that is an array).
	 If lval is a mem, we need to flag all assignments that may be aliases,
	 as well as the array itself. *)
    let rec find_subst res l = match l with
      | [] -> res
      | (idx,e1,refcount,indref,force)::ls ->
	  let get_result () =
	    match res with
		None ->
		  (match e1 with
		       Constant _ -> Some(Some e1)
		    (* | Some(oldctx, Temp _ as new_t) -> new_t  FIXME: alpha vary *)
		     | _ when force ->
			 dprintf "Forced substitution of %s" (lval_to_string lval);
			 Some(Some e1)
		     | _ ->
			 inc refcount;
			 Some None
		  )
	      | x -> x
	  in
	  match (idx,index) with
	      (None,None) -> 
		get_result();
	    | (None,Some _) ->
		inc indref;
		res (* Stop looking *)
	    | (Some idx, Some index) ->
		(* if both have indices, then we can only substitute if
		   we are sure this value wasn't shadowed by a subsequent
		   write. *)
		(* for now, we only attempt alias analysis on Arrays. For
		   memories, we always assume they may be aliased. *)
		(match (exp_eq idx index, unwind_type t) with
		     (Some true, Array _) ->
		       find_subst (get_result()) ls
		   | (Some false, Array _) ->
		       find_subst res ls
		   | (None, Array _)
		   | (_, TMem _) ->
		       let res = match res with
			   (* This assignment may or may not shadow
			      previous ones, so we can't substitute
			      unless we found one earlier. *)
			   None -> Some None
			 | x -> x
		       in
			 inc indref;
			 find_subst res ls
		   | _ ->
		       failwith "unexpected type with indices"
		)
	    | (Some idx, None) -> inc indref; find_subst res ls
    in
      match find_subst None relevant_ctx with
	  Some x -> x
	| None -> None
(* Eww, that was overly long and complicated, and it doesn't even handle cases
   like "let m[1] = 1 in let m2 = m in m2[1]" *)

  (* Whether we are at the toplevel of the expression *)
  val mutable at_toplevel = true

  method visit_exp e =
    let getres() = match e with
	Let(l, e1, e2) ->
	  (* these simplifications must not use self, because that would
	     artificially increase the reference count for our context *)
	  let e1 = simplify e1 in
	  let l = simplify_alvalue l in
	    (* drop the let binding *)
	    (* can't use ChangeDoChildrenPost, because we want to visit
	     * e2, not only it's children *)
	  let unextend = self#extend l e1 in
	    ChangeTo(unextend(exp_accept self e2))
      | _ ->
	  ChangeDoChildrenPost(e, constant_fold_more (self#lookup))
    in
      if at_toplevel
      then (at_toplevel <- false;
	    self#compute_forced e;
	    let res = getres() in
	      at_toplevel<-true;
	      res
	   )
      else getres()

  method visit_binding _ = SkipChildren (* already done in visit_exp *)
end

(** This should maybe not be exposed, but is used in vine_indepclauses.ml *)
let rec simplify_rec subst_more e =
  let passes = ref 0 in
  let simpler1 x =
    inc passes;
    if !passes > 2
    then dprintf "Going on to simplification pass #%d" !passes;
    exp_accept (new simplifying_visitor (simplify_rec subst_more) subst_more) x
  in
  let rec simpler x =
    let x' = simpler1 x in
      (* FIXME: this comparison is slow. Having a function that returs None
       * when it doesn't change anything might be better. *)
      if x = x' 
      then
	x
      else (
	(* prerr_endline "Note: Simplified to: ";
	pp_exp prerr_string x';	prerr_newline();
	prerr_endline "Note: Simplifying again..."; *)
	simpler x')
  in
    simpler e

(** Simplify an expression as much as possible.
    This function performs all supported simplification steps on the given
    expression.
    @return the simplified expression.
*)
let simplify = simplify_rec (fun _ _ -> true)


(** Like [simplify], but does not perform the slower variable substitution.
    Alpha varying the expression before calling this function will likely result
    in better simplification.
*)
let simplify_faster = simplify_rec (fun _ _ -> false)

(** Simplifies all subexpressions inside a statement *)
let simplify_stmt =
  let stmt_simplifier = object
    inherit nop_vine_visitor
    method visit_exp e =
      ChangeTo(simplify e)
  end
  in
  stmt_accept (stmt_simplifier :> vine_visitor)

(** make expressions bigger by doing back-substitution *)
let coalesce_stmts stmts maxtimes = 
  (* this is broken. It should be doing substitution backwards. *)
  stmts
(*
  let vis = 
object(self)
  inherit nop_vine_visitor
    
  val mutable ctx = Hashtbl.create 13
  val subst_cntr_ctx = Hashtbl.create 13

  method clear_subst_cntr_ctx () = 
    Hashtbl.clear subst_cntr_ctx

  method clear_ctx () = 
    Hashtbl.clear ctx

  method visit_exp e = 
    match e with
	Let(lv,e1,e2) ->
	  ChangeDoChildrenPost(e, 
			       (fun x -> Hashtbl.remove ctx lv; x))
      | Lval((Temp(v) as lv)) when Vine.is_not_memory v  ->  (
	    try
	      let (e':exp) = Hashtbl.find ctx lv in 
	      let cntr = 
		try
		  Hashtbl.find subst_cntr_ctx lv 
		with
		    Not_found -> 0
	      in
		if cntr < maxtimes then (
		  Hashtbl.replace subst_cntr_ctx lv (cntr+1);
		  ChangeTo(e') 
		) else SkipChildren

	    with
		Not_found ->  SkipChildren
	)
      | _ -> DoChildren
	  
  method visit_binding ((lv,e) as b) =
    ChangeDoChildrenPost(b, (fun x -> Hashtbl.add ctx lv e; x) )
    
  method insert_stmt_to_ctx = function
      Move(lv, e) -> Hashtbl.replace ctx lv e
    | _ -> ()
	
  method visit_stmt s = 
    ChangeDoChildrenPost(s, (fun s -> self#insert_stmt_to_ctx s; s))
      
end
  in
  List.map 
    (fun s -> 
       vis#clear_subst_cntr_ctx;
       let s' = stmt_accept vis s in 
	 simplify_stmt s' 
    ) stmts
*)

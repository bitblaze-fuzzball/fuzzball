(**
    Translation from Vine expressions to SMT-LIB2 format.
 *)
(* Modeled after stp.ml, originally by Ivan Jager *)

open Vine
open Printf
module VH = Vine.VarHash
module List = ExtList.List

module D = Debug.Make(struct let name = "SMT" and default=`NoDebug end)
open D

let is_not_memory t =
  match unwind_type t with
      TMem _ -> false
    | _ -> true

let smtlib2_rm = function
  | Vine_util.ROUND_NEAREST           -> "RNE"
  | Vine_util.ROUND_NEAREST_AWAY_ZERO -> "RNA"
  | Vine_util.ROUND_POSITIVE          -> "RTP"
  | Vine_util.ROUND_NEGATIVE          -> "RTN"
  | Vine_util.ROUND_ZERO              -> "RTZ"

(** Class for printing out SMT-LIB2 syntax for an expression. *)
class vine_smtlib_printer puts =
  let rec type2s = function
    | REG_1 -> "Bool"
    | t when is_integer_type t ->
	"(_ BitVec "^string_of_int(Vine.bits_of_width t)^")"
    | Array(t2,i) ->
        failwith("Bitvector arrays not supported for SMT-LIB2 translation")
    | x ->
	failwith("Unsupported type for SMT-LIB translation: "^type_to_string x)

  in
  let unique_names = Hashtbl.create 1001 in
  let rename_var name =
    let need_bars = ref false
    and new_name = ref "" in
      for i = 0 to (String.length name) - 1 do
	match name.[i] with
	  | '_' -> new_name := !new_name ^ "-"
	  | '-' -> new_name := !new_name ^ "_"; need_bars := true
	  | _ -> new_name := !new_name ^ (Char.escaped name.[i])
      done;
      if !need_bars then
	"|" ^ !new_name ^ "|"
      else
	!new_name
  in
  let var2s (num,name,_) =
    let first = try Hashtbl.find unique_names name with
	Not_found -> Hashtbl.add unique_names name num; num
    in
      if first = num then
	rename_var name
      else
	rename_var (name^"_"^(string_of_int num))
  in
object (self)
    (* variables already used in this output (needed for mems) *)
  val used_vars = Hashtbl.create 57
    (* map vine var to var we are using *)
  val g = VH.create 57
  val mutable unknown_counter = 0

  method private extend2 v s =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s ();
    dprintf "Extending %s -> %s" (var2s v) s;
    VH.add g v s
  method private unextend v =
    dprintf "Unextending %s" (var2s v);
    VH.remove g v
  method private tr_var v =
    let v' =
    try VH.find g v
    with Not_found -> (* free variable *)
      var2s v
    in
      dprintf "Translating %s -> %s" (var2s v) v';
      v'

  method declare_var ((_,_,t) as v) =
    puts(sprintf "(declare-fun %s () %s)\n" (var2s v) (type2s t))

  method declare_var_value ((_,_,t) as v) e =
    let def = self#translate_exp e in
      puts(sprintf "(declare-fun %s () %s)\n" (var2s v) (type2s t));
      puts(sprintf "(assert (= %s " (var2s v));
      puts(def);
      puts "))\n"

  method private declare_freevars e =
    let fvs = get_req_ctx e in
      dprintf "%d free variables\n%!" (List.length fvs);
      List.iter self#declare_var fvs

  method private unwrap_fp_exp ty s =
    if String.length s > 3 && String.sub s 0 3 = "bfp" then
      (* Cheat: this is the result of a wrap_fp_exp conversion (below),
	 so we know the unwraped variable name. *)
      String.sub s 1 ((String.length s) - 1)
    else
      match ty with
	| REG_32 -> "((_ to_fp 8 24) " ^ s ^ ")"
	| REG_64 -> "((_ to_fp 11 53) " ^ s ^ ")"
	| _ -> failwith "Unsupported float size in unwrap_fp_exp"

  (* Z3 has a non-standard "to_ieee_bv" function we could use here,
     but the standard recommends a more cumbersome indirect
     expression that requires introducing a new variable. *)
  val mutable fp_counter = 1
  method private wrap_fp_exp ty s =
    let varname = "fp" ^ (string_of_int fp_counter) in
    let bvarname = "b" ^ varname in
    let (fp_sz, bv_ty) = match ty with
      | REG_32 -> ("8 24", "(_ BitVec 32)")
      | REG_64 -> ("11 53", "(_ BitVec 64)")
      | _ -> failwith "Unsupported float size in wrap_fp_exp"
    in
    let fp_ty = "(_ FloatingPoint " ^ fp_sz ^ ")" in
    let to_fp = "(_ to_fp " ^ fp_sz ^ ")" in
      puts(sprintf "(declare-fun %s () %s)\n" varname fp_ty);
      puts(sprintf "(assert (= %s %s))\n" varname s);
      puts(sprintf "(declare-fun %s () %s)\n" bvarname bv_ty);
      puts(sprintf "(assert (= %s (%s %s)))\n" varname to_fp bvarname);
      fp_counter <- fp_counter + 1;
      bvarname

  method private translate_exp e =
  let rec tr_exp e =
    match e with
	Constant(v) ->
	  (match v with
	     | Int(REG_1, i) ->
		 if bool_of_const e then "true" else "false"
	     | Int(t,i) ->
		let (format, mask) = match (Vine.unwind_type t) with
		  | REG_8  -> (format_of_string "#x%02Lx", 0xffL)
		  | REG_16 -> (format_of_string "#x%04Lx", 0xffffL)
		  | REG_32 -> (format_of_string "#x%08Lx", 0xffffffffL)
		  | REG_64 -> (format_of_string "#x%016Lx",
			       0xffffffffffffffffL)
		  | _ ->
		      raise (Invalid_argument 
			       "Only constant integers supported")
		in
		let maskedval = Int64.logand i mask
		in
		  sprintf format maskedval
	     | _ ->
		 raise (Invalid_argument
			  "Only integer constant types supported")
	  )
      | Lval(Temp v) ->
	  self#tr_var v
      | Lval(Mem(((_,_,m_ty)), idx,t)) when is_not_memory t ->
	  failwith "Memory access translation to SMT-LIB2 not supported"
      | Lval(Mem _) ->
	  raise (Invalid_argument "Memory type not handled")
      | Let(Temp(v), rhs, body) ->
	  let v_s = self#tr_var v and
	      rhs_s = tr_exp rhs and
	      body_s = tr_exp body in
	    "(let ((" ^ v_s ^ " " ^ rhs_s ^ ")) " ^ body_s ^ ")"
      | Let(Mem(_,_,_), _, _) ->
	  failwith "Memory let expression translation to SMT-LIB2 not supported"
      | UnOp(uop, e1) ->
	  let pre = match (uop, (Vine_typecheck.infer_type_fast e1)) with
	    | (_, REG_1) -> "(not "
	    | (NEG, _)   -> "(bvneg "
	    | (NOT, _)   -> "(bvnot "
	  in
	    pre ^ (tr_exp e1) ^ ")"
      | FUnOp(uop, rm, e1) ->
	  let ty1 = Vine_typecheck.infer_type_fast e1 and
	      op = match uop with
		| FNEG -> ignore(rm); "fp.neg" (* no rounding needed *)
	  in
	    self#wrap_fp_exp ty1
	      ("(" ^ op ^ " " ^ (self#unwrap_fp_exp ty1 (tr_exp e1)) ^ ")")
      | FBinOp(bop, rm, e1, e2) ->
	  let ty1 = Vine_typecheck.infer_type_fast e1 in
	  let (op, is_pred, need_rm) = match bop with
	    | FPLUS   -> ("fp.add", None,       true)
	    | FMINUS  -> ("fp.sub", None,       true)
	    | FTIMES  -> ("fp.mul", None,       true)
	    | FDIVIDE -> ("fp.div", None,       true)
	    | FEQ     -> ("fp.eq",  Some true,  false)
	    | FNEQ    -> ("fp.eq",  Some false, false)
	    | FLT     -> ("fp.lt",  Some true,  false)
	    | FLE     -> ("fp.leq", Some true,  false)
	  in
	  let core = 
	      "(" ^ op ^ " " ^
	      (if need_rm then (smtlib2_rm rm ^ " ") else "") ^
		(self#unwrap_fp_exp ty1 (tr_exp e1)) ^ " " ^
		(self#unwrap_fp_exp ty1 (tr_exp e2)) ^
		")"
	  in
	    (match is_pred with
	       | None -> self#wrap_fp_exp ty1 core
	       | Some true -> core
	       | Some false -> "(not " ^ core ^ ")")
      | FCast(ct, rm, ty2, e1) ->
	  let ty1 = Vine_typecheck.infer_type_fast e1 in
	  let (fp_in, fp_out) = match ct with
	    | (CAST_SFLOAT|CAST_UFLOAT) -> (false, true)
	    | (CAST_SFIX|CAST_UFIX) -> (true, false)
	    | (CAST_FWIDEN|CAST_FNARROW) -> (true, true)
	  in
	  let rm_s = smtlib2_rm rm in
	  let sz2 = string_of_int (bits_of_width ty2) in
	  let op = match (ct, ty2) with
	    | (CAST_SFLOAT,  REG_32) -> "(_ to_fp 8 24)"
	    | (CAST_SFLOAT,  REG_64) -> "(_ to_fp 11 53)"
	    | (CAST_UFLOAT,  REG_32) -> "(_ to_fp_unsigned 8 24)"
	    | (CAST_UFLOAT,  REG_64) -> "(_ to_fp_unsigned 11 53)"
	    | (CAST_FWIDEN,  REG_64) -> "(_ to_fp 11 53)"
	    | (CAST_FNARROW, REG_32) -> "(_ to_fp 8 24)"
	    | (CAST_SFIX,    _)      -> "(_ fp.to_sbv " ^ sz2 ^ ") "
	    | (CAST_UFIX,    _)      -> "(_ fp.to_ubv " ^ sz2 ^ ") "
	    | _ -> failwith "Unsupported FCast combination"
	  in
	  let core =
	    ("(" ^ op ^ " " ^ rm_s ^ " " ^
	       (if fp_in then
		  self#unwrap_fp_exp ty1 (tr_exp e1)
		else
		  (tr_exp e1)) ^
	       ")")
	  in
	    if fp_out then
	      self#wrap_fp_exp ty2 core
	    else
	      core
      | (BinOp(BITOR,
	       (Cast(CAST_UNSIGNED, REG_16, e1)
	       | BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_16, e1),
		       Constant(Int(_, 0L)))),
	       BinOp(LSHIFT,
		     Cast(CAST_UNSIGNED, REG_16, e2),
		     Constant(Int(_, 8L))))
	| BinOp(BITOR,
		BinOp(LSHIFT,
		      Cast(CAST_UNSIGNED, REG_16, e2),
		      Constant(Int(_, 8L))),
		Cast(CAST_UNSIGNED, REG_16, e1)))
	  when (Vine_typecheck.infer_type_fast e1) = REG_8
	    && (Vine_typecheck.infer_type_fast e2) = REG_8
	    ->
	  "(concat " ^ (tr_exp e2) ^ " " ^ (tr_exp e1) ^ ")"
      | (BinOp(BITOR,
	       Cast(CAST_UNSIGNED, REG_32, e1),
	       BinOp(LSHIFT,
		     Cast(CAST_UNSIGNED, REG_32, e2),
		     Constant(Int(_, 16L))))
	| BinOp(BITOR,
		BinOp(LSHIFT,
		      Cast(CAST_UNSIGNED, REG_32, e2),
		      Constant(Int(_, 16L))),
		Cast(CAST_UNSIGNED, REG_32, e1)))
	  when (Vine_typecheck.infer_type_fast e1) = REG_16
	    && (Vine_typecheck.infer_type_fast e2) = REG_16
	    ->
	  "(concat " ^ (tr_exp e2) ^ " " ^ (tr_exp e1) ^ ")"
      | (BinOp(BITOR,
	       Cast(CAST_UNSIGNED, REG_64, e1),
	       BinOp(LSHIFT,
		     Cast(CAST_UNSIGNED, REG_64, e2),
		     Constant(Int(_, 32L))))
	| BinOp(BITOR,
		BinOp(LSHIFT,
		      Cast(CAST_UNSIGNED, REG_64, e2),
		      Constant(Int(_, 32L))),
		Cast(CAST_UNSIGNED, REG_64, e1)))
	  when ((Vine_typecheck.infer_type_fast e1) = REG_32
	      && (Vine_typecheck.infer_type_fast e2) = REG_32)
	    ->
	  "(concat " ^ (tr_exp e2) ^ " " ^ (tr_exp e1) ^ ")"
      | (BinOp(
	  BITOR,
	  BinOp(
	    BITOR,
	    BinOp(
	      BITOR,
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e1),
		    Constant(Int(_, 0L))),
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e2),
		    Constant(Int(_, 8L)))),
	    BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e3),
		  Constant(Int(_, 16L)))),
	  BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e4),
		Constant(Int(_, 24L))))
	| BinOp(
	    BITOR,
	    BinOp(
	      BITOR,
	      Cast(CAST_UNSIGNED, REG_32, e1),
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e2),
		    Constant(Int(_, 8L)))),
	    BinOp(
	      BITOR,
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e3),
		    Constant(Int(_, 16L))),
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e4),
		    Constant(Int(_, 24L)))))
	| BinOp(
	    BITOR, Cast(CAST_UNSIGNED, REG_32, e1),
	    BinOp(
	      BITOR,
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e2),
		    Constant(Int(_, 8L))),
	      BinOp(
		BITOR,
		BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e3),
		      Constant(Int(_, 16L))),
		BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_32, e4),
		      Constant(Int(_, 24L)))))))
	  when (Vine_typecheck.infer_type_fast e1) = REG_8
	    && (Vine_typecheck.infer_type_fast e2) = REG_8
	    && (Vine_typecheck.infer_type_fast e3) = REG_8
	    && (Vine_typecheck.infer_type_fast e4) = REG_8
	  ->
	  "(concat (concat " ^ (tr_exp e4) ^ " " ^ (tr_exp e3) ^ ") " ^
	    "(concat " ^ (tr_exp e2) ^ " " ^ (tr_exp e1) ^ "))"
      | BinOp(
	  BITOR,
	  BinOp(
	    BITOR,
	    BinOp(
	      BITOR,
	      BinOp(
		BITOR,
		BinOp(
		  BITOR,
		  BinOp(
		    BITOR,
		    BinOp(
		      BITOR,
		      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e1),
			    Constant(Int(_, 0L))),
		      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e2),
			    Constant(Int(_, 8L)))),
		    BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e3),
			  Constant(Int(_, 16L)))),
		  BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e4),
			Constant(Int(_, 24L)))),
		BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e5),
		      Constant(Int(_, 32L)))),
	      BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e6),
		    Constant(Int(_, 40L)))),
	    BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e7),
		  Constant(Int(_, 48L)))),
	  BinOp(LSHIFT, Cast(CAST_UNSIGNED, REG_64, e8),
		Constant(Int(_, 56L))))
	  when (Vine_typecheck.infer_type_fast e1) = REG_8
	    && (Vine_typecheck.infer_type_fast e2) = REG_8
	    && (Vine_typecheck.infer_type_fast e3) = REG_8
	    && (Vine_typecheck.infer_type_fast e4) = REG_8
	    && (Vine_typecheck.infer_type_fast e5) = REG_8
	    && (Vine_typecheck.infer_type_fast e6) = REG_8
	    && (Vine_typecheck.infer_type_fast e7) = REG_8
	    && (Vine_typecheck.infer_type_fast e8) = REG_8
	    ->
	  "(concat (concat (concat " ^ (tr_exp e8) ^ " " ^ (tr_exp e7) ^ ") " ^
	    "(concat " ^ (tr_exp e6) ^ " " ^ (tr_exp e5) ^ ")) " ^
	    "(concat (concat " ^ (tr_exp e4) ^ " " ^ (tr_exp e3) ^ ") " ^
	    "(concat " ^ (tr_exp e2) ^ " " ^ (tr_exp e1) ^ ")))"
      | BinOp(BITOR,
	      BinOp(BITAND, Cast(CAST_SIGNED, ty1, cond1), x),
	      BinOp(BITAND, UnOp(NOT, Cast(CAST_SIGNED, ty2, cond2)), y))
      | BinOp(BITOR,
	      BinOp(BITAND, x, Cast(CAST_SIGNED, ty1, cond1)),
	      BinOp(BITAND, UnOp(NOT, Cast(CAST_SIGNED, ty2, cond2)), y))
      | BinOp(BITOR,
	      BinOp(BITAND, Cast(CAST_SIGNED, ty1, cond1), x),
	      BinOp(BITAND, y, UnOp(NOT, Cast(CAST_SIGNED, ty2, cond2))))
      | BinOp(BITOR,
	      BinOp(BITAND, x, Cast(CAST_SIGNED, ty1, cond1)),
	      BinOp(BITAND, y, UnOp(NOT, Cast(CAST_SIGNED, ty2, cond2))))
	  when ty1 = ty2 && cond1 = cond2 &&
	    (Vine_typecheck.infer_type_fast cond1) = REG_1
	    ->
	  "(ite " ^ (tr_exp cond1) ^ " " ^ (tr_exp x) ^ " " ^
	    (tr_exp y) ^ ")"
      | Ite(cond, x, y) ->
	  "(ite " ^ (tr_exp cond) ^ " " ^ (tr_exp x) ^ " " ^
	    (tr_exp y) ^ ")"
      | BinOp(bop, e1, e2) ->
	  let t = Vine_typecheck.infer_type_fast e1 in
	  let (pre,mid,post) = match (bop, t) with
	    | (PLUS, REG_1)    -> ("(xor ", " ", ")")
	    | (PLUS, _)        -> ("(bvadd ", " ", ")")
	    | (MINUS, REG_1)   -> ("(not (xor ", " ", "))")
	    | (MINUS, _)       -> ("(bvsub ", " ", ")")
	    | (TIMES, REG_1)   -> ("(and ", " ", ")")
	    | (TIMES, _)       -> ("(bvmul ", " ", ")")
	    | (DIVIDE, REG_1)  -> ("(not (xor ", " ", "))")
	    | (DIVIDE, _)      -> ("(bvudiv ", " ", ")")
	    | (SDIVIDE, REG_1) -> ("(not (xor ", " ", "))")
	    | (SDIVIDE, _)     -> ("(bvsdiv ", " ", ")")
	    | (MOD, REG_1)     -> ("(not (xor ", " ", "))")
	    | (MOD, _)         -> ("(bvurem ", " ", ")")
	    | (SMOD, REG_1)    -> ("(not (xor ", " ", "))")
	    | (SMOD, _)        -> ("(bvsrem ", " ", ")") (* FIXME: bvsmod or bvsrem? *)
	    | (BITAND, REG_1)  -> ("(and ", " ", ")")
	    | (BITAND, _)      -> ("(bvand ", " ", ")")
	    | (BITOR, REG_1)   -> ("(or ", " ", ")")
	    | (BITOR, _)       -> ("(bvor ", " ", ")")
	    | (XOR, REG_1)     -> ("(xor ", " ", ")")
	    | (XOR, _)         -> ("(bvxor ", " ", ")")
	    | (EQ, REG_1)      -> ("(not (xor ", " ", "))")
	    | (EQ, _)          -> ("(= ", " ", ")")
	    | (NEQ, REG_1)     -> ("(xor ", " ", ")")
	    | (NEQ, _)         -> ("(not (= ", " ", "))")
	    | (LT, REG_1)      -> ("(and (not ", ") ", ")")
	    | (LT, _)          -> ("(bvult ", " ", ")")
	    | (LE, REG_1)      -> ("(or (not ", ") ", ")")
	    | (LE, _)          -> ("(bvule ", " ", ")")
	    | (SLT, REG_1)     -> ("(and ", " (not ", "))")
	    | (SLT, _)         -> ("(bvslt ", " ", ")")
	    | (SLE, REG_1)     -> ("(or ", " (not ", "))")
	    | (SLE, _)         -> ("(bvsle ", " ", ")")
	    | (LSHIFT, _)      -> ("(bvshl ", " ", ")")
	    | (ARSHIFT, _)     -> ("(bvashr ", " ", ")")
	    | (RSHIFT, _)      -> ("(bvlshr ", " ", ")")
	  and e2 = match bop with
	    | LSHIFT | ARSHIFT | RSHIFT ->
	      (* SMT-LIB requires both shift operands to have the same size *)
	      let t = Vine_typecheck.infer_type None e1
	      and t2 = Vine_typecheck.infer_type None e2 in
	      let b = bits_of_width t and b2 = bits_of_width t2 in
	        if b > b2 then Cast(CAST_UNSIGNED, t, e2)
		else if b < b2 then Cast(CAST_LOW, t, e2)
		else e2
	    | _ ->
	        e2
	  in
	    pre ^ (tr_exp e1) ^ mid ^ (tr_exp e2) ^ post
      | Cast(CAST_LOW, REG_1,
	     (BinOp(ARSHIFT, e1, Constant(Int(REG_8, k))))) ->
	  let bit = string_of_int (Int64.to_int k) in
	  let extract = "((_ extract " ^ bit ^ " " ^ bit ^ ")" in
	    "(= #b1 " ^ extract ^ (tr_exp e1) ^ "))"
      | Cast(ct,t, e1) ->
	  let make_zeros k =
	    if k mod 4 = 0 then
	      "#x" ^ (String.make (k / 4) '0')
	    else
	      "#b" ^ (String.make k '0')
	  in
	  let make_ones k =
	    if k mod 4 = 0 then
	      "#x" ^ (String.make (k / 4) 'f')
	    else
	      "#b" ^ (String.make k '1')
	  in
	  let make_one k =
	    if k mod 4 = 0 then
	      (make_zeros (k - 4)) ^ "1"
	    else
	      (make_zeros (k - 1)) ^ "1"
	  in
	  let t1 = Vine_typecheck.infer_type_fast e1 in
	  let (bits, bits1) = (bits_of_width t, bits_of_width t1) in
	  let (pre,post) = match (ct, t1, t) with
	    | (_, t_1, t_2) when t_1 = t_2 ->
		("","")
	    | (CAST_SIGNED, REG_1, _) ->
		"(ite ", " " ^ (make_ones bits) ^
		   " " ^ (make_zeros bits) ^ ")"
	    | (CAST_SIGNED, _, _)  ->
		("((_ sign_extend " ^ string_of_int(bits-bits1) ^ ") ", ")")
	    | (CAST_LOW, _, REG_1) ->
		("(= #b1 ((_ extract "^string_of_int(bits - 1)^" 0) ", "))")
	    | (CAST_LOW, _, _) ->
		("((_ extract "^string_of_int(bits - 1)^" 0) ", ")")
	    | (CAST_HIGH, _, REG_1) ->
		("(= #b1 ((_ extract "^string_of_int(bits1-1)^" "^
		   string_of_int(bits1-bits)^") ", "))")
	    | (CAST_HIGH, _, _) ->
		("((_ extract "^string_of_int(bits1-1)^" "^
		   string_of_int(bits1-bits)^") ", ")")
	    | (CAST_UNSIGNED, REG_1, _) ->
		("(ite ", " " ^ (make_one bits) ^
		   " " ^ (make_zeros bits) ^ ")")
	    | (CAST_UNSIGNED, _, _)  ->
		("(concat "^(make_zeros (bits-bits1))^" ", ")")
	  in
	    pre ^ (tr_exp e1) ^ post
      | Unknown s ->
	  let s =
	    "unknown_"^string_of_int unknown_counter^" %" ^ s ^ "\n"
	  in
	    unknown_counter <- unknown_counter + 1;
	    s
      | Name _ -> raise (Invalid_argument "Names should be here")
  in
    tr_exp e

  method assert_exp e =
    let s = self#translate_exp e in
      puts "(assert ";
      puts s;
      puts ")\n"

end

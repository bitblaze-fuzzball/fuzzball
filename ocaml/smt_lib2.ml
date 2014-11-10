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

(* Translate a variable to conform to SMT-LIB2's syntax *)
let transvar s = s

let is_not_memory t =
  match unwind_type t with
      TMem _ -> false
    | _ -> true


let rec repeat = function 0 -> (fun _ -> ())
  | n -> (fun f -> ( f(); repeat (n-1) f))

let width_cvt t1 t2 e =
  let wd1 = Vine.bits_of_width t1 and
      wd2 = Vine.bits_of_width t2 in
    if wd1 = wd2 then
      e
    else if wd1 < wd2 then
      Cast(CAST_UNSIGNED, t2, e)
    else
      Cast(CAST_LOW, t2, e)

let index_type ty =
  match ty with
    | TMem(t, _) -> t
    | Array(_, sz) when sz <= 2L -> REG_1
    | Array(_, sz) when sz <= 256L -> REG_8
    | Array(_, sz) when sz <= 65536L -> REG_16
    | Array(_, sz) when sz <= 0x1000000000L -> REG_32
    | Array(_, _) -> REG_64
    | _ -> failwith "Nonindexable type in index_type"

let rec collect_bindings = function
    | Let(lv, rhs, e2) ->
	let (bl1, body) = collect_bindings e2 in
	  ((lv, rhs) :: bl1,  body)
    | e -> ([], e)

(* Like List.iter, but pass flags to allow f to treat the first and
   last items specially. *)
let iter_first_last f list =
  let rec loop = function
    | [] -> failwith "List length invariant failure in iter_first_last"
    | [e] -> f e false true
    | e :: r -> f e false false; loop r
  in
    match list with
      | [] -> ()
      | [e] -> f e true true
      | e :: r -> f e true false; (loop r)

(** Visitor for printing out SMT-LIB2 syntax for an expression. *)
class vine_smtlib_print_visitor puts =
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
  inherit nop_vine_visitor
    (* variables already used in this output (needed for mems) *)
  val used_vars = Hashtbl.create 57
    (* map vine var to var we are using *)
  val g = VH.create 57
  val mutable unknown_counter = 0

  method extend2 v s =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s ();
    dprintf "Extending %s -> %s" (var2s v) s;
    VH.add g v s
  method unextend v =
    dprintf "Unextending %s" (var2s v);
    VH.remove g v
  method tr_var v =
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
    puts(sprintf "(declare-fun %s () %s)\n" (var2s v) (type2s t));
    puts(sprintf "(assert (= %s " (var2s v));
    ignore(exp_accept (self :> vine_visitor) e);
    puts "))\n"

  method declare_freevars e =
    let fvs = get_req_ctx e in
      dprintf "%d free variables\n%!" (List.length fvs);
      List.iter self#declare_var fvs


  method visit_exp e =
    match e with
	Constant(v) ->
	  (match v with
	     | Int(REG_1, i) ->
		 if bool_of_const e then puts "true" else puts "false"
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
		  puts(sprintf format maskedval)
	     | _ ->
		 raise (Invalid_argument
			  "Only constant integer types supported")
	  );
	  SkipChildren
      | Lval(Temp v) ->
	  puts(self#tr_var v);
	  SkipChildren
      | Lval(Mem(((_,_,m_ty)), idx,t)) when is_not_memory t ->
	  failwith "Memory access translation to SMT-LIB2 not supported"
      | Lval(Mem _) ->
	  raise (Invalid_argument "Memory type not handled")

      | Let(_,_,_) ->
	  failwith "Let expression translation to SMT-LIB2 not supported"
      | UnOp(uop, e1) ->
	  let () = match (uop, (Vine_typecheck.infer_type_fast e1)) with
	    | (_, REG_1) -> puts "(not "
	    | (NEG, _)   -> puts "(bvneg "
	    | (NOT, _)   -> puts "(bvnot "
	  in
	    ChangeDoChildrenPost(e, fun x ->(puts ")";x))
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
	  (puts "(concat ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts ")";
	   SkipChildren)
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
	  (puts "(concat ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts ")";
	   SkipChildren)
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
	  (puts "(concat ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts ")";
	   SkipChildren)
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
	  (puts "(concat (concat ";
	   ignore(exp_accept (self :> vine_visitor) e4);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e3);
	   puts ") (concat ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts "))";
	   SkipChildren)
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
	  (puts "(concat (concat (concat ";
	   ignore(exp_accept (self :> vine_visitor) e8);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e7);
	   puts ") (concat ";
	   ignore(exp_accept (self :> vine_visitor) e6);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e5);
	   puts ")) (concat (concat ";
	   ignore(exp_accept (self :> vine_visitor) e4);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e3);
	   puts ") (concat ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts ")))";
	   SkipChildren)
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
	  (puts "(ite ";
	   ignore(exp_accept (self :> vine_visitor) cond1);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) x);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) y);
	   puts ")";
	   SkipChildren);
      | Ite(cond, x, y) ->
	  (puts "(ite ";
	   ignore(exp_accept (self :> vine_visitor) cond);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) x);
	   puts " ";
	   ignore(exp_accept (self :> vine_visitor) y);
	   puts ")";
	   SkipChildren);
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
	      let () = puts pre in
	      let _ = exp_accept (self :> vine_visitor) e1 in
	      let () = puts  mid in
	      let _ =  exp_accept (self :> vine_visitor) e2 in
	      let () = puts post in
		SkipChildren
      | Cast(CAST_LOW, REG_1,
	     (BinOp(ARSHIFT, e1, Constant(Int(REG_8, k))))) ->
	  let bit = string_of_int (Int64.to_int k) in
	  let extract = "((_ extract " ^ bit ^ " " ^ bit ^ ")" in
	    puts ("(= #b1 " ^ extract);
	    ignore(exp_accept (self :> vine_visitor) e1);
	    puts "))";
	    SkipChildren
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
		   string_of_int(bits1-bits)^")", ")")
	    | (CAST_UNSIGNED, REG_1, _) ->
		("(ite ", " " ^ (make_one bits) ^
		   " " ^ (make_zeros bits) ^ ")")
	    | (CAST_UNSIGNED, _, _)  ->
		("(concat "^(make_zeros (bits-bits1))^" ", ")")
	  in
	  let () = puts pre in
	  ChangeDoChildrenPost(e, fun x-> puts post; x)
      | Unknown s ->
	  puts ("unknown_"^string_of_int unknown_counter^" %");
	  puts s;
	  puts "\n";
	  unknown_counter <- unknown_counter + 1;
	  DoChildren
      | Name _ -> raise (Invalid_argument "Names should be here")

(*
      | Name of string
      | Phi of string * exp list
      | Let of lvalue * exp * exp
*)

end

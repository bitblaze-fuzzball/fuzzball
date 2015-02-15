(** Translations to STP
    Translation from VinE expressions to STP.
    See the to_string, to_file, and print functions.
    @author Ivan Jager
 *)

open Vine
open Printf
module VH = Vine.VarHash
module List = ExtList.List 

module D = Debug.Make(struct let name = "STP" and default=`NoDebug end)
open D

(* Translate a variable to conform to CVC's syntax *)
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

(** Visitor for printing out CVCL sytax for an expression. *)
class vine_cvcl_print_visitor puts =
  let rec type2s = function
    | REG_1 -> "BOOLEAN"
    | t when is_integer_type t ->
	"BV("^string_of_int(Vine.bits_of_width t)^")"
    | Array(t2,i) as ty ->
	let width_s = string_of_int (Vine.bits_of_width (index_type ty)) in
	  "ARRAY BITVECTOR("^width_s^") OF "^type2s t2
    | x ->
	failwith("Unsupported type for translation to STP: "^type_to_string x)
	
  in
  let unique_names = Hashtbl.create 1001 in
  let var2s (num,name,_) =
    let first = try Hashtbl.find unique_names name with
	Not_found -> Hashtbl.add unique_names name num; num
    in
      if first = num then
	name
      else
	name^"_"^string_of_int num
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
    puts(sprintf "%s : %s;\n" (var2s v) (type2s t))

  method declare_var_value ((_,_,t) as v) e = 
    puts(sprintf "%s : %s = " (var2s v) (type2s t));
    ignore(exp_accept (self :> vine_visitor) e);
    puts ";\n"

  method declare_freevars e =
    let () = puts "% free variables: \n" in
    let fvs = get_req_ctx e in 
    let () = dprintf "%d free variables\n%!" (List.length fvs) in 
    let () = List.iter self#declare_var fvs in
      puts "% end free variables.\n\n\n"



  method visit_exp e =
    match e with
	Constant(v) ->
	  (match v with
	     | Int(REG_1, i) ->
		 if bool_of_const e then puts "TRUE" else puts "FALSE"
	     | Int(t,i) ->
		let (format, mask) = match (Vine.unwind_type t) with
		  | REG_8  -> (format_of_string "0h%02Lx", 0xffL)
		  | REG_16 -> (format_of_string "0h%04Lx", 0xffffL)
		  | REG_32 -> (format_of_string "0h%08Lx", 0xffffffffL)
		  | REG_64 -> (format_of_string "0h%016Lx",
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
      | Lval(Mem(((_,_,m_ty) as m), idx,t)) when is_not_memory t ->
	  let idx_old_ty = Vine_typecheck.infer_type_fast idx in
	  let adj_idx = width_cvt idx_old_ty (index_type m_ty) idx in
	    puts (self#tr_var m^"[");
	    ignore(exp_accept self adj_idx);
	    puts "]";
	    SkipChildren
      | Lval(Mem _) ->
	  raise (Invalid_argument "Memory type not handled")

      | Let(_,_,_) ->
	  let (bl, e2) = collect_bindings e in
	    puts "(LET ";
	    iter_first_last
	      (fun (l,e1) is_first is_last ->
		 if not is_first then
		   puts "    ";
		 (match l with
		    | Temp v ->
			(* v isn't allowed to shadow anything *)
			let s = var2s v in
			  puts s;
			  puts " = ";
			  ignore(exp_accept (self :> vine_visitor) e1);
			  self#extend2 v s
		    | Mem(((_,_,m_ty) as m),addr,t) when is_not_memory t  -> 
			let oldmem = self#tr_var m in
			  (* m is shadowing the old m *)
			let newmem = var2s (renewvar m) in
			let addr_old_ty =
			  Vine_typecheck.infer_type_fast addr in
			let adj_addr = width_cvt addr_old_ty
			  (index_type m_ty) addr
			in
			  puts(newmem^" = ("^oldmem^" WITH [");
			  ignore(exp_accept (self :> vine_visitor) adj_addr);
			  puts "] := ";
			  ignore(exp_accept (self :> vine_visitor) e1);
			  puts ")";
			  self#extend2 m newmem
		    | Mem _ ->
			raise (Invalid_argument				 
				 "Memory type not handled in STP"));
		 if not is_last then
		   puts ",\n"
	      ) bl;
	    puts "\nIN\n";
	    ignore(exp_accept (self :> vine_visitor) e2);
	    List.iter
	      (fun (l,_) ->
		 (match l with
		    | Temp v -> self#unextend v
		    | Mem(m,_,_) -> self#unextend m)) bl;
	    puts ")";
	    SkipChildren
      | UnOp(uop, e1) ->
	  let () = match (uop, (Vine_typecheck.infer_type_fast e1)) with
	    | (_, REG_1) -> puts "(NOT "
	    | (NEG, _)   -> puts "BVUMINUS("
	    | (NOT, _)   -> puts "(~"
	  in
	    ChangeDoChildrenPost(e, fun x ->(puts ")";x))
      | (* Eww, the << operator in stp seems to want a constant int on the right,
	   rather than a bitvector *)
	  BinOp(LSHIFT, e1, Constant(Int(_, i))) ->
	  if i = 0L then let _ = exp_accept (self :> vine_visitor) e1 in SkipChildren (* STP barfs on 0 *)
	  else
	  let  t = Vine_typecheck.infer_type_fast e1 in
	  let () = puts("((") in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts(" << "^Int64.to_string i^")["^string_of_int(bits_of_width t -1)^":0])")
	  in
	    SkipChildren
      | BinOp(RSHIFT, e1, Constant(Int(_, i))) -> (* Same sort of deal :( *)
	  if i = 0L then let _ = exp_accept (self :> vine_visitor) e1 in SkipChildren (* STP barfs on 0 *)
	  else
	  (* let t = Vine_typecheck.infer_type_fast e1 in *)
	  let () = puts "(" in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts(" >> "^Int64.to_string i^")")
	  in
	    SkipChildren
      | BinOp(ARSHIFT, e1, Constant(Int(_, i))) -> (* Same sort of deal :( *)
	  if i = 0L then let _ = exp_accept (self :> vine_visitor) e1 in SkipChildren (* STP barfs on 0 *)
	  else
	  let t = Vine_typecheck.infer_type_fast e1 in
	  let bits = bits_of_width t in
	  let endpt = min (Int64.to_int i) (bits - 1) in
	  let () = puts "SX(" in
	  let _ = exp_accept (self :> vine_visitor) e1 in
	  let () = puts("[" ^ (string_of_int (bits - 1)) ^ 
			  ":" ^ (string_of_int endpt) ^ "], "^
			  (string_of_int bits) ^ ")")
	  in
	    SkipChildren
      | BinOp(bop, e1, e2) when bop = LSHIFT || bop = RSHIFT || bop = ARSHIFT ->
	  let t2 = Vine_typecheck.infer_type_fast e2 in
	  let const n = Constant(Int(t2,Int64.of_int n)) in
	  let put_one n = ignore(exp_accept (self :> vine_visitor) (BinOp(bop, e1, const n))) in
	  let rec put_all n =
	    if n < 64 then
	      (puts " IF ";
	       ignore(exp_accept (self :> vine_visitor) e2);
	       puts " = ";
	       ignore(exp_accept (self :> vine_visitor) (const n));
	       puts " THEN ");
	    put_one n;
	    if n < 64 then
	      (puts " ELSE ";
	       put_all (n+1);
	      puts " ENDIF ")
	  in
	    put_all 0; 
	    SkipChildren
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
	  (puts "(";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " @ ";
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
	  (puts "(";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " @ ";
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
	  (puts "(";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " @ ";
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
	  (puts "(";
	   ignore(exp_accept (self :> vine_visitor) e4);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e3);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts ")";
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
	  (puts "(";
	   ignore(exp_accept (self :> vine_visitor) e8);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e7);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e6);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e5);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e4);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e3);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e2);
	   puts " @ ";
	   ignore(exp_accept (self :> vine_visitor) e1);
	   puts ")";
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
	  (puts "IF ";
	   ignore(exp_accept (self :> vine_visitor) cond1);
	   puts " THEN ";
	   ignore(exp_accept (self :> vine_visitor) x);
	   puts " ELSE ";
	   ignore(exp_accept (self :> vine_visitor) y);
	   puts " ENDIF";
	   SkipChildren);
      | Ite(cond, x, y) ->
	  (puts "IF ";
	   ignore(exp_accept (self :> vine_visitor) cond);
	   puts " THEN ";
	   ignore(exp_accept (self :> vine_visitor) x);
	   puts " ELSE ";
	   ignore(exp_accept (self :> vine_visitor) y);
	   puts " ENDIF";
	   SkipChildren);
      | BinOp(bop, e1, e2) ->
	  let t = Vine_typecheck.infer_type_fast e1 in
	  let bits = if is_integer_type t then  bits_of_width t else -1 in
	  let sw = string_of_int bits in
	  let (pre,mid,post,is_assoc) = match (bop, t) with
	    | (PLUS, REG_1)    -> ("(", " XOR ", ")", true)
	    | (PLUS, _)        -> ("BVPLUS("^sw^", ", ",", ")", true)
	    | (MINUS, REG_1)   -> ("(", " <=> ", ")", false)
	    | (MINUS, _)       -> ("BVSUB("^sw^", ", ",", ")", false)
	    | (TIMES, REG_1)   -> ("(", " AND ", ")", true)
	    | (TIMES, _)       -> ("BVMULT("^sw^", ", ",", ")", false)
	    | (DIVIDE, REG_1)  -> ("(", " <=> ", ")", false)
	    | (DIVIDE, _)      -> ("BVDIV("^sw^", ", ",", ")", false)
	    | (SDIVIDE, REG_1) -> ("(", " <=> ", ")", false)
	    | (SDIVIDE, _)     -> ("SBVDIV("^sw^", ", ",", ")", false)
	    | (MOD, REG_1)     -> ("(", " <=> ", ")", false)
	    | (MOD, _)         -> ("BVMOD("^sw^", ", ",", ")", false)
	    | (SMOD, REG_1)    -> ("(", " <=> ", ")", false)
	    | (SMOD, _)        -> ("SBVMOD("^sw^", ", ",", ")", false)
	    | (BITAND, REG_1)  -> ("(", " AND ", ")", true)
	    | (BITAND, _)      -> ("(", " & ", ")", true)
	    | (BITOR, REG_1)   -> ("(", " OR ", ")", true)
	    | (BITOR, _)       -> ("(", " | ", ")", true)
	    | (XOR, REG_1)     -> ("(", " XOR ", ")", true)
	    | (XOR, _)         -> ("BVXOR(", ", ", ")", false)
	    | (EQ, REG_1)      -> ("(", " <=> ", ")", false)
	    | (EQ, _)          -> ("(", " = ", ")", false)
	    | (NEQ, REG_1)     -> ("(", " XOR ", ")", false)
	    | (NEQ, _)         -> ("(", " /= ", ")", false)
	    | (LT, REG_1)      -> ("((NOT ", ") AND ", ")", false)
	    | (LT, _)          -> ("BVLT(", ", ", ")", false)
	    | (LE, REG_1)      -> ("((NOT ", ") OR ", ")", false)
	    | (LE, _)          -> ("BVLE(", ", ", ")", false)
	    | (SLT, REG_1)     -> ("(", " AND (NOT ", "))", false)
	    | (SLT, _)         -> ("BVSLT(", ", ", ")", false)
	    | (SLE, REG_1)     -> ("(", " OR (NOT ", "))", false)
	    | (SLE, _)         -> ("BVSLE(", ", ", ")", false)
	    | (LSHIFT, _)
	    | (ARSHIFT, _)
	    | (RSHIFT, _) ->
		failwith "shifts should have been handled by a different case"
	  in
	    if is_assoc then
	      let terms = Vine_opt.flatten_binop bop e in
	      let newline = if List.length terms > 10 then "\n" else "" in
	      match terms with
		| first :: rest ->
		    assert(rest <> []);
		    puts pre;
		    ignore(exp_accept (self :> vine_visitor) first);
		    List.iter (fun a -> puts newline; puts mid;
				 ignore(exp_accept (self :> vine_visitor) a))
		      rest;
		    puts post;
		    SkipChildren
		 | [] -> failwith "flatten_binop failure";
	    else
	      let () = puts pre in
	      let _ = exp_accept (self :> vine_visitor) e1 in
	      let () = puts  mid in
	      let _ =  exp_accept (self :> vine_visitor) e2 in
	      let () = puts post in
		SkipChildren
      | Cast(CAST_LOW, REG_1,
	     (BinOp(ARSHIFT, e1, Constant(Int(REG_8, k))))) ->
	  let bit = string_of_int (Int64.to_int k) in
	  let extract = "[" ^ bit ^ ":" ^ bit ^ "]" in
	    puts "(0b1 = ";
	    ignore(exp_accept (self :> vine_visitor) e1);
	    puts extract;
	    puts ")";
	    SkipChildren
      | Cast(ct,t, e1) ->
	  let make_zeros k = 
	    if k mod 4 = 0 then
	      "0h" ^ (String.make (k / 4) '0')
	    else
	      "0b" ^ (String.make k '0')
	  in
	  let make_ones k = 
	    if k mod 4 = 0 then
	      "0h" ^ (String.make (k / 4) 'f')
	    else
	      "0b" ^ (String.make k '1')
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
		("IF ", " THEN " ^ (make_ones bits) ^
		   " ELSE " ^ (make_zeros bits) ^ " ENDIF")
	    | (CAST_SIGNED, _, _)  ->
		("SX(",", "^string_of_int bits^")")
	    | (CAST_LOW, _, REG_1) ->
		("(0b1 = ", "["^string_of_int(bits - 1)^":0])")
	    | (CAST_LOW, _, _) ->
		("(", "["^string_of_int(bits - 1)^":0])")
	    | (CAST_HIGH, _, REG_1) ->
		("(0b1 = ", "["^string_of_int(bits1-1)^":"^
		   string_of_int(bits1-bits)^"])")
	    | (CAST_HIGH, _, _) ->
		("(", "["^string_of_int(bits1-1)^":"^
		   string_of_int(bits1-bits)^"])")
	    | (CAST_UNSIGNED, REG_1, _) ->
		("IF ", " THEN " ^ (make_one bits) ^
		   " ELSE " ^ (make_zeros bits) ^ " ENDIF")
	    | (CAST_UNSIGNED, _, _)  ->
		("("^(make_zeros (bits-bits1))^" @ ", ")")
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
      | FUnOp(_, _, _)
      | FBinOp(_, _, _, _)
      | FCast(_, _, _, _)
	-> raise (Invalid_argument "STP does not support floating point")

(*
      | Name of string
      | Phi of string * exp list
      | Let of lvalue * exp * exp
*)

end

(** Translates an expression into STP and feeds it to the given print function.
    
    The output of this function consists of an ASSERTion that the given
    expression is true.
*)
let put_cvcl puts e =
  let vis = new vine_cvcl_print_visitor puts in
  let () = dprintf "making declarations\n%!" in 
  let () = vis#declare_freevars e in
  let () = puts "ASSERT(\n" in
  let () = dprintf "exp-accept freevars \n%!" in 
  let _ = exp_accept (vis :> vine_visitor) e in
    puts ");\n"

(** Like put_cvcl, but the STP code is retured in a string *)
let to_string e =
  let buf = Buffer.create 16 in
  let () = put_cvcl (Buffer.add_string buf) e in
    Buffer.contents buf

(** Like put_cvcl, but the STP code is printed to standard out *)
let print = put_cvcl print_string

(** Like put_cvcl, but the STP code is written to a channel *)
let to_file fd =
  put_cvcl (output_string fd)


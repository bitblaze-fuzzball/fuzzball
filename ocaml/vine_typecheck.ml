(**
   Type inference and typechecker for the VinE IR. See the documentation
   for the complete semantics.
   @author Ivan Jager
   @author David Brumley
*)

open Vine;;
open Vine_util;;
module VM = VarMap;;
module List=ExtList.List;;

module Label =
struct
  type t = Vine.label
  let compare = String.compare
end

module LMap = Map.Make(Label);;

let lmap_keys lm = LMap.fold (fun k d a -> k :: a) lm []

module D = Debug.Make(struct let name = "typecheck" and default=`Debug end)
open D

(** by default, during typechecking make sure jump targets within a
    function are to labels within the function. This can be turned
    off, but then it's unclear what the semantics are of functions.
    Turn off with care...it make break things like the evaluator.
    
    This does slow things down slightly.  For the blaster.trace.ir
    (which is about 500000 lines) in
    the regression suite, the typecheck time with this flag as true is
    about 14 seconds.  When this flag is false, the time is about 11
    seconds.  
 *)
let typecheck_jmp_targets = ref true;;

type ctx = Vine.typ VM.t
type gamma = ctx option


let rec tint t = 
  match (unwind_type t) with
      REG_1 | REG_8 | REG_16 | REG_32 | REG_64 -> true
    | _ -> false


let rec tcompat (t1:typ) (t2:typ) : bool = 
  let t1' = unwind_type t1 in 
  let t2' = unwind_type t2 in    
    t1' = t2'

let gamma_create () : gamma = Some(VM.empty) 

let gamma_extend gamma (v:var) (t:typ) = 
    match gamma with
      None -> None
    | Some(g) -> Some(VM.add v t g) 
;;


let gamma_find gamma v : typ option =
  match gamma with
      None -> None
    | Some(ctxt) -> (
	try 
	  Some(VM.find v ctxt)
	with
	    Not_found ->
	      raise (TypeError(var_to_string v^" not declared"))
      )


let gamma_check gamma v t : typ = 
  match (gamma_find gamma v) with
      None -> t
    | Some(t') when t <> t' -> 
	raise (TypeError("type mismatch: "^
			   var_to_string v^
			   ":"^(type_to_string t')^
			   " but declared as "^
			   (type_to_string t)))
    | _ -> t
;;




module AlmostDeclSet = 
  Set.Make(struct type t = int * string let compare = compare end)

(** verify that decl list lst has each variable id,name only once 
    @param lst the list of variables
    @return unit. 
    @raise TypeError if the variable is declared more than once.
*)
let check_unique_decls (lst:decl list) = 
  (* FIXME: this may not make a lot of sense given what the parser will
     be doing later *)
  let _ = List.fold_left 
    (fun acc (x,name,t) ->
       if (AlmostDeclSet.mem (x,name) acc)  then
         raise (TypeError(name^" redeclared in same scope"))
       else
         AlmostDeclSet.add (x,name) acc) 
    AlmostDeclSet.empty
    lst 
  in
  ()
;;

let rec typecheck_value (v:value) : typ = 
  match v with
    | Int(t, i) ->
	let b = bits_of_width t in
	  (* shift left by 64 is undefined, and b >=1 always *)
	let mask = Int64.shift_left (-2L) (b-1) in
	  if Int64.logand mask i = 0L
	    || Int64.logand (Int64.shift_right mask 1) (Int64.lognot i) = 0L (* negative numbers ok too*)
	  then
	    t
	  else
	    (pwarn ("Constant value outside range: "
		    ^Int64.to_string i^":"^type_to_string t);
	     t)
    | Str _  -> TString

and typecheck_lval names gamma (lv:lvalue)  = 
  match lv with
      Temp((_,_,t) as v) -> 
	let t' = gamma_check gamma v t in 
	  (names,t')
    | Mem((_,_,t) as v, e, t') -> 
	let vt = gamma_check  gamma v t   in 
	let (names,et) = typecheck_exp names gamma e in
	  (match vt with
	       Array(t1, i) when (tint et) && t1 = t' -> 
		 (names,t1)
	     | Array(t1,i) when (tint et) ->
		 raise 
		   (TypeError 
		      (Printf.sprintf 
			 ("Array elements declared type %s, "^^
			    "but used here as %s.")
			 (type_to_string t1)
			 (type_to_string t')
		      )
		   )
	     | Array(t1,i)  ->
		 raise (TypeError "Array index not an integer")
	     | TMem(t, _) when t = et -> (names,t')
	     | _ -> raise (TypeError(var_to_string v^" index type mismatch"))
	  )


and typecheck_exp names gamma (e:exp)  = 
  match e with
      Constant(v) -> (names,typecheck_value  v)
    | Lval(lv) -> typecheck_lval names gamma lv
    | UnOp(uop, e') -> 
	let (n,t) = typecheck_exp names gamma e'  in 
	  if (tint t) then (n,t) else
	    raise (TypeError("unop requires integer type"))
    | BinOp(bop, e1, e2) -> (
	let (names,t1) = typecheck_exp names gamma e1 in 
	let (names,t2) = typecheck_exp names gamma e2 in 
	match bop with
	    PLUS | MINUS | TIMES | DIVIDE | SDIVIDE 
	  | MOD | SMOD | BITAND | BITOR | XOR 
		when tcompat t1 t2 && tint t1
		  ->
	      (names,t1)
	  | LSHIFT | RSHIFT | ARSHIFT
		when tint t1 && tint t2
		  ->
	      (names,t1)
	  | EQ | NEQ
		 when tcompat t1 t2
		   ->
	      (names,REG_1)
	  | LT | LE | SLT | SLE
		 when tint t1 && tcompat t1 t2
		  ->
	      (names,REG_1)
	  | _ ->
	      let msg = (binop_to_string bop)^" incompatible with operands ("
		^type_to_string t1^" and "^type_to_string t2^")"
	      in
		raise (TypeError(msg))
      )
    | Ite(cond_e, true_e, false_e) ->
	let (names, cond_t) = typecheck_exp names gamma cond_e in
	  (if cond_t <> REG_1 then
	     let msg = "Condition in ? : should be of type REG_1, not "
	       ^ (type_to_string cond_t)
	     in
	       raise (TypeError(msg)));
	  let (names, true_t) = typecheck_exp names gamma true_e in
	  let (names, false_t) = typecheck_exp names gamma false_e in
	    if not (tcompat true_t false_t) then
	      let msg = "Second and third args to ? : must have " ^
		"compatible type, not " ^ (type_to_string true_t) ^ " and " ^
		(type_to_string false_t)
	      in
	       raise (TypeError(msg))
	    else
	      (names, true_t)
    | Unknown _ -> raise (TypeError("Cannot typecheck unknown's"))
    | Cast(ct, t, e') -> ( 
	let (names,t1) = typecheck_exp names gamma e' in
	let castbits = Vine.bits_of_width t in
	let ebits = (
	  match t1 with 
	      REG_1  -> 1  
	    | REG_8  -> 8
	    | REG_16 -> 16
	    | REG_32 -> 32
	    | REG_64 -> 64 
	    | _ -> raise (TypeError("nonsensical cast"))
	) in 
	  match ct with
	      CAST_UNSIGNED when castbits >= ebits -> (names,t) (* valid when
							   widening *)
	    | CAST_SIGNED when castbits >= ebits -> (names,t) (* valid when
							 widening *)
	    | CAST_HIGH when castbits <= ebits -> (names,t) (* valid when
						       narrowing *)
	    | CAST_LOW when castbits <= ebits -> (names,t) (* valid when
						      narrowing *)
	    | _ -> raise (TypeError("nonsensical cast"))
      )
    | Name(l) -> 
	if !typecheck_jmp_targets then (
	if LMap.mem l names then
	  (names,Vine.addr_t)
	else (LMap.add l None names, Vine.addr_t)
	) else 
	  (names,Vine.addr_t)
    | Let(lv, e1, e2) ->  
	let (names, e1t) = typecheck_exp names gamma e1 
	in 
	let gamma = (match lv with
			 Temp((_,_,t) as var) -> 
			   gamma_extend gamma var t
		       | _ -> gamma
		    )
	in
	let (names,lvt) = typecheck_lval names gamma lv in 
	  if not (tcompat lvt e1t) then 
	    raise (TypeError("Invalid let declaration"))
	  else (
	    let gamma = 
	      match lv with
		  Temp(v) -> gamma_extend gamma v e1t 
		| _ -> gamma
	    in
	    let (names, t) = typecheck_exp names gamma e2 in 
	      (names, t)
	  )
;;




let rec typecheck_stmt scope omega names labels sigma gamma s  = 
  let err m = 
    raise (TypeError("Type error: "^m^"\n at"^(stmt_to_string s)))
  in
  let retcheck t omega = 
    match omega with
	None when scope = 0 -> ()
      | t' when t' = t ->  ()
      | _ -> 
	  err "return type does not match declared type"
  in
  let decls_to_types dl = 
    List.map (fun (_,_,t) -> t) dl
  in
  let check_labels_and_names l sigma labels names =  (
    if !typecheck_jmp_targets then (
    try
      let nlst = lmap_keys names in 
      let l' = List.find
	(fun n ->
	   not (LMap.mem n labels) &&
	     not (LMap.mem n sigma)
	) nlst in
      let msg = Printf.sprintf ("%s label used but"^^
				  " undefined in "^^
				  "function %s")
	l' l in
	raise (TypeError msg)
    with
	Not_found -> ()
    )
    else () 
  ) in 
  match s with
    | Block(dl, sl) -> 
	check_unique_decls dl;
	let gamma' = gamma in 
	let sigma' = sigma in 
	let gamma = List.fold_left 
	  (fun g ((_,_,t) as var) -> gamma_extend g var t) gamma dl in 
	let (names, labels, _, _) = 
	  (List.fold_left 
	     (fun (names,labels,sigma,gamma)  s -> 
		typecheck_stmt (scope+1) omega names labels sigma gamma s) 
	     (names, labels, sigma,gamma) sl) in
	  (names,labels, sigma', gamma')
    | Jmp(e) ->	(
	let (names,t) = typecheck_exp names gamma e 
	in
	  match t with
	      t' when t' = addr_t -> (names,labels,sigma,gamma)
	    | _ -> err "Invalid jmp target"
      )

    | CJmp(e1, e2, e3) -> (
	let names,t1 = typecheck_exp names gamma e1 in 
	let names,t2 = typecheck_exp names gamma e2 in 
	let names,t3 = typecheck_exp names gamma e3 in 
	  match (t1,t2,t3) with
	      (REG_1, x1, x2) when x1 = addr_t && x2 = addr_t -> 
		(names, labels, sigma, gamma)
	    | _ -> err "CJmp typing error"
      )
    | Move(lv, e) -> 
	let (names,t1) = typecheck_lval names gamma lv in 
	let (names,t2) = typecheck_exp names gamma e in 
	  if (tcompat t1 t2) then (names,labels,sigma,gamma) else
	    err "Invalid assignment"
    | Special _ -> (names,labels,sigma,gamma)
    | Label(l) -> 
	if (!typecheck_jmp_targets) then (
	  if LMap.mem l labels then
	    (names, labels, sigma, gamma)
	  else 
	    (names,LMap.add l None labels, sigma,gamma)
	) else
	  (names, labels, sigma, gamma)
    | ExpStmt(e) -> 
	let names,_ = typecheck_exp names gamma e in 
	  (names,labels,sigma,gamma)
    | Comment _ -> (names,labels,sigma,gamma)
    | Return(Some(e)) -> 
	let (names,t) = typecheck_exp names gamma e in 
	  retcheck (Some(t)) omega;
	  (names,labels,sigma, gamma)
    | Return(None) -> 
	retcheck None omega;
	(names,labels,sigma,gamma)
    | Function(l,topt,dl,b,blockopt) ->
	let tfun = TFun(topt, decls_to_types dl, b) in 
	let () = if scope <> 0 then 
	  err "Cannot declare or define function outside global scope"
	else () in
	let sigma = LMap.add l tfun sigma in 
	let gamma' = List.fold_left 
	  (fun gamma ((_,_,t) as v) -> gamma_extend gamma v t ) gamma dl 
	in 
	  (
	    match blockopt with 
		None -> ()
	      | Some(s') -> (
		  let (names,labels, sigma', _) = 
		    typecheck_stmt scope topt 
		      LMap.empty LMap.empty sigma gamma' s'
		  in
		    if sigma <> sigma' then
		      err "Function declared inside another function";
		    check_labels_and_names l sigma labels names
		)
	  );
	  (names,labels,sigma,gamma)
    | Call(lvo,e, args) ->

	let (actualtypes,names) = 
	  List.fold_left
	    (fun (tacc,nacc) arg -> 
	       let (n,t) = typecheck_exp nacc gamma arg in 
		 (t::tacc,n)
	    ) ([],names) args in
	let (names,rettype) = (
	  match lvo with 
	      None -> (names,None)
	    | Some(lv) -> let (n,t) = typecheck_lval names gamma lv in
		(n,Some(t))
	) in
	let tfun = (
	  match e with
	      Name(l) -> (
		(* direct call *)
		try 
		  LMap.find l sigma
		with
		    Not_found -> 
		      err ("Direct call to unknown function "^l)
	      )
	    | _ -> (* indirect call. assume function of right type *)
		TFun(rettype,actualtypes, true)
	) in 
	  (match tfun with
	       TFun(rettype, actualtypes, _) -> 
		 (names,labels,sigma,gamma)
	     | _ -> 
		 raise (TypeError "invalid call")
	  )
    | Halt(e) ->
	let names,_ = typecheck_exp names gamma e in 
	  (labels, names, sigma, gamma)
    | Assert(e) ->
	let names,_ = typecheck_exp names gamma e in 
	  (labels, names, sigma, gamma)
    | Attr(s', a) -> 
	try
	  typecheck_stmt scope omega names labels sigma gamma s'
	with
	    TypeError m -> 
	      Printf.eprintf "%s: %s\n%!" (attr_to_string a) m;
	      err "Type Error"
;;


let typecheck (dl,sl) = 
  let gamma = List.fold_left (fun g ((v,i,t) as var) ->
				gamma_extend g var t) 
    (gamma_create ()) dl
  in
    ignore(
      List.fold_left 
	(fun (names,labels,sigma,gamma) s ->
	    typecheck_stmt 0 None names labels sigma gamma s) 
	(LMap.empty, LMap.empty ,LMap.empty, gamma) sl)
      



let infer_type gamma e = 
  let (_, t) = typecheck_exp LMap.empty gamma e in 
    t
;;


let infer_type_fast e =
  let rec loop e =
    match e with
      | Constant(Int(ty, _)) -> ty
      | Constant(Str(_)) -> TString
      | Lval(Temp((_,_,t))) -> t
      | Lval(Mem(_,_,t)) -> t
      | UnOp(_, e1) -> loop e1
      | BinOp((EQ|NEQ|LT|LE|SLT|SLE), _, _) -> REG_1
      | BinOp((LSHIFT|RSHIFT|ARSHIFT), e1, _) -> loop e1
      | BinOp((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|MOD|SMOD|BITAND|BITOR|XOR),
	      e1, e2) ->
	  loop (if Random.bool () then e1 else e2)
      | Unknown(_) -> raise (TypeError("Cannot typecheck unknowns"))
      | Cast(_, t, _) -> t
      | Name(_) -> Vine.addr_t
      | Let(_, _, e2) -> loop e2
      | Ite(_, e1, e2) ->
	  loop (if Random.bool () then e1 else e2)
  in
    loop e
;;

(*   let _ = List.fold_left *)
(*       (fun gamma x -> *)
(* 	 match x with *)
(* 	     Function((_,n,TFun(rettype,_,false)),args,stmts) -> *)
(* 	       let () = vis#clear_labels () in  *)
(* 	       let () = List.iter ( *)
(* 		 fun s -> ignore(stmt_accept vis s)) stmts in  *)
(* 	       let labels = Some(vis#get_labels ()) in  *)
(* 		 List.fold_left *)
(* 		   (fun gamma s -> *)
(* 		      typecheck_stmt rettype labels gamma s; *)
(* 		   ) gamma stmts; *)
(* 	   | Function((_,n,_),_,_) -> *)
(* 	       raise (TypeError ("Invalid  function type for "^n)) *)
(* 	   | GVar((_,_,t) as var) -> gamma_extend gamma var t *)
(*       ) (gamma_create ()) dl in () *)



(*    | Function(v, t, dl, false, Some(Block(_,blksl) as blk)) ->  (
	if scope <> 0 then 
	  err "Function definition not in scope 0";


	check_unique_decls dl; (* Hmm. this may be more stringent than c,
				  but seems like a good idea. We make
				  sure each variable name declared in the
				  signature is unique. 
				  -djb *)
	List.iter (gamma_extend gamma) dl;
	if (not (last_static_stmt_is_return blksl)) then 
	  raise (TypeError("Last statement in a function must be a return"));
	try
	  let newlabels = Hashtbl.create 13 in 
	    typecheck_stmt (scope+1) sigma (ref t)
	      newlabels gamma blk;
	    if !typecheck_jmp_targets then 
	      H.iter (fun akey avalue -> 
		if avalue = Used then 
		  raise (TypeError(v^": jump to unknown label "^akey)) )
		newlabels 
	    else
	      ()
	with
	    x -> List.iter (gamma_free gamma) dl;
	      raise x
      ) 
    | Function(v,t,dl, b, None) -> ( 
	if scope <> global_scope then
	  raise (TypeError("Declaration  for '"^v^"' outside global scope not allowed"));
	extend_sigma v t dl b;
	()
      ) *)



(*
  (
	try 
	  let (rt, formals, _) = H.find sigma l in
	    match lvo with
		Some lv
		  when  Some(typecheck_lval labels gamma lv) <> rt ->
		    raise (TypeError("call lval not same as return type"))
	      | _ -> 
		  List.iter2
		    (fun arg ((_,_,ft) as fv) ->
		       let argtype = typecheck_exp labels gamma arg in 
			 if not (tcompat argtype ft) then 
			   raise (TypeError("argument for "^var_to_string fv^
					      " doesn't match formal type"))
		    ) args formals
	  with 
	      Not_found -> 
		raise (TypeError("Function "^l^" not declared."))
      )
*)

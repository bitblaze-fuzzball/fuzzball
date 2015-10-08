(*
  Copyright (C) BitBlaze, 2009-2011. All rights reserved.
*)

module V = Vine;;

open Exec_options;;
open Query_engine;;

class stpvc_engine = object(self)
  inherit query_engine

  val vc = Stpvc.create_validity_checker ()
  val mutable ctx = None
  val mutable free_vars = []
  val mutable temp_vars = []
  val mutable eqns = []
  val mutable conds = []

  val mutable asserts = []
  val mutable the_query = None

  method private ctx =
    match ctx with
      | Some c -> c
      | None -> failwith "Missing ctx in stpvc_engine"

  method start_query =
    Libstp.vc_push vc    

  method add_free_var v =
    free_vars <- v :: free_vars

  method add_temp_var v =
    temp_vars <- v :: temp_vars

  method private ensure_ctx =
    match ctx with
      | Some c -> ()
      | None -> 
	  ctx <- Some(Vine_stpvc.new_ctx vc (free_vars @ temp_vars))

  method assert_eq var rhs =
    eqns <- (var, rhs) :: eqns

  method add_condition e =
    conds <- e :: conds;
    
  val mutable ctx_stack = []

  method push =
    ctx_stack <- (free_vars, temp_vars, eqns, conds) :: ctx_stack

  method pop =
    match ctx_stack with
      | (free_vars', temp_vars', eqns', conds') :: rest ->
	  free_vars <- free_vars';
	  temp_vars <- temp_vars';
	  eqns <- eqns';
	  conds <- conds';
	  ctx_stack <- rest
      | [] -> failwith "Context underflow in stpvc_engine#pop"

  method private real_assert_eq (var, rhs) =
    let form = V.BinOp(V.EQ, V.Lval(V.Temp(var)), rhs) in
    (* Printf.printf "Asserting %s\n" (V.exp_to_string form); *)
    let f = Stpvc.e_bvbitextract vc (Vine_stpvc.vine_to_stp vc self#ctx form) 0
    in
      Stpvc.do_assert vc f;
      asserts <- f :: asserts

  method private real_prepare =
    ctx <- Some(Vine_stpvc.new_ctx vc (List.rev (temp_vars @ free_vars)));
    List.iter self#real_assert_eq (List.rev eqns)

  method query qe =
    self#real_prepare;
    let conj = List.fold_left
      (fun es e -> V.BinOp(V.BITAND, e, es)) qe (List.rev conds)
    in
    let s = (Stpvc.e_simplify vc
	       (Stpvc.e_not vc
		  (Stpvc.e_bvbitextract vc
		     (Vine_stpvc.vine_to_stp vc self#ctx conj) 0)))
    in
    (* Printf.printf "STP formula is %s\n" (Stpvc.to_string s);
       flush stdout; *)
    let result = Stpvc.query vc s in
    let ce_list = if result then [] else
      (* Strategy 1: getTrueCounterExample. A custom interface that Vine
	 has patched into STP for a while, but I don't know the reason
	 why it's desirable. Probably better to avoid.
      List.map
	(fun (sym, stp_exp) ->
	   ((Stpvc.to_string sym), (fun () -> (Stpvc.int64_of_e stp_exp))))
	(Stpvc.get_true_counterexample vc) *)

      (* Strategy 2: getWholeCounterExample. An interface that's closer to
	 STP's internal data structures. If a variable is unconstrained,
	 it will just be mapped to a SYMBOL, so we can drop it from the
	 C.E. This worked fine for many versions of STP, but recently
	 we've run into trouble when an optimization has caused this to
	 return a BVCONCAT when only some of the bits in an expression
	 are constrained. Potentially useful information but we don't
	 always know how to deal with it.
      let wce = Stpvc.get_whole_counterexample vc in
	List.map (fun (var, e) -> (var, Stpvc.int64_of_e e))
	  (List.filter (fun (var, e) ->
			  (Libstp.getExprKind e) = Libstp.BVCONST)
	     (List.map
		(fun ((n,s,t) as var) ->
		   let var_s = Vine_stpvc.vine_var_to_stp vc self#ctx var
		   in
		     (s, (Stpvc.get_term_from_counterexample vc var_s wce)))
		free_vars)) *)

      (* Strategy 3: getCounterExample. This always returns a concrete
         value; if the variable is unconstrained, it will just return a
         constant 0. I believe this is the interface KLEE uses
         exclusively.
      List.map
	(fun ((n,s,t) as var) ->
	    let var_e = V.Lval(V.Temp(var)) in
	    let var_s = Vine_stpvc.vine_to_stp vc self#ctx var_e in
	    let val_s = Stpvc.get_counterexample vc var_s in
	      assert((Libstp.getExprKind val_s) = Libstp.BVCONST);
	      (s, Stpvc.int64_of_e val_s))
	free_vars; *)

      (* Strategy 4: first try getWholeCounterExample, but fallback to
	 getCounterExample if the value is neither completely
	 unconstrained nor completely constrained. *)
      let wce = Stpvc.get_whole_counterexample vc in
	List.map (function
		    | Some x -> x
		    | _ -> failwith "Filter invariant failure")
	  (List.filter (function Some _ -> true | None -> false)
	     (List.map
		(fun ((n,s,t) as var) ->
		   let var_s = Vine_stpvc.vine_var_to_stp vc self#ctx var in
		   let term = Stpvc.get_term_from_counterexample vc var_s wce
		   in
		     match Libstp.getExprKind term with
		       | Libstp.SYMBOL -> None
		       | Libstp.BVCONST ->
			   Some (s, Stpvc.int64_of_e term)
		       | _ ->
			   let cterm = Stpvc.get_counterexample vc var_s in
			     assert((Libstp.getExprKind cterm)
				    =  Libstp.BVCONST);
			     Some (s, Stpvc.int64_of_e cterm))
		free_vars))
    in
      the_query <- Some s;
      ((Some result), ce_from_list ce_list)

  val mutable filenum = 0

  method after_query save_results =
    if save_results || !opt_save_solver_files then
      (filenum <- filenum + 1;
       let fname = "fuzz-stpvc-" ^ (string_of_int filenum) ^ ".stp" in
       let oc = open_out fname in
	 List.iter (fun a -> Printf.fprintf oc "ASSERT(%s);\n"
		      (Stpvc.to_string a)) asserts;
	 (match the_query with
	   | None -> ()
	   | Some q -> Printf.fprintf oc "QUERY(%s);\n"
	       (Stpvc.to_string q));
	 close_out oc;
	 Printf.printf "Saved STP commands in %s\n%!" fname);
    asserts <- [];
    the_query <- None;
    Libstp.vc_clearDecls vc;
    Libstp.vc_pop vc;
    Libstp.vc_push vc;
    Libstp.vc_pop vc;

  method reset =
    ctx <- None;
    free_vars <- [];
    temp_vars <- [];
    eqns <- [];
    conds <- []

  method push_vc = Libstp.vc_push vc
    
  method get_vc = vc
      
end

(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
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
    let ce = if result then [] else
      (*
      List.map
	(fun (sym, stp_exp) ->
	   ((Stpvc.to_string sym), (fun () -> (Stpvc.int64_of_e stp_exp))))
	(Stpvc.get_true_counterexample vc) *)
      let wce = Stpvc.get_whole_counterexample vc in
	List.map (fun (var, e) -> (var, Stpvc.int64_of_e e))
	  (List.filter (fun (var, e) ->
			  (Libstp.getExprKind e) = Libstp.BVCONST)
	     (List.map
		(fun ((n,s,t) as var) ->
		   let var_e = V.Lval(V.Temp(var)) in
		   let var_s = Vine_stpvc.vine_to_stp vc self#ctx var_e
		   in
		     (s, (Stpvc.get_term_from_counterexample vc var_s wce)))
		free_vars))
    in
      the_query <- Some s;
      ((Some result), ce)

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

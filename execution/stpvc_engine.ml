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

  val mutable asserts = []
  val mutable the_query = None

  method private ctx =
    match ctx with
      | Some c -> c
      | None -> failwith "Missing ctx in stpvc_engine"

  method prepare free_vars_a temp_vars =
    Libstp.vc_push vc;
    free_vars <- free_vars_a;
    ctx <- Some(Vine_stpvc.new_ctx vc (free_vars_a @ temp_vars))

  method assert_eq var rhs =
    let form = V.BinOp(V.EQ, V.Lval(V.Temp(var)), rhs) in
    (* Printf.printf "Asserting %s\n" (V.exp_to_string form); *)
    let f = Stpvc.e_bvbitextract vc (Vine_stpvc.vine_to_stp vc self#ctx form) 0
    in
      Stpvc.do_assert vc f;
      asserts <- f :: asserts

  method query e =
    let s = (Stpvc.e_simplify vc
	       (Stpvc.e_not vc
		  (Stpvc.e_bvbitextract vc
		     (Vine_stpvc.vine_to_stp vc self#ctx e) 0)))
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

  method unprepare save_results =
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
    ctx <- None;
    Libstp.vc_push vc;
    Libstp.vc_pop vc;

  method push_vc = Libstp.vc_push vc
    
  method get_vc = vc
      
end

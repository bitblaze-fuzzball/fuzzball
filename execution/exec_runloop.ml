(*
  Copyright (C) BitBlaze, 2009-2011. All rights reserved.
*)

module V = Vine

open Exec_veritesting
open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common 
open Exec_veritesting

let call_replacements fm last_eip eip =
  let ret_reg = match !opt_arch with
    | X86 -> R_EAX
    | X64 -> R_RAX
    | ARM -> R0
  in
  let canon_eip eip =
    match !opt_arch with
      | X86 -> eip
      | X64 -> eip
      | ARM -> Int64.logand 0xfffffffeL eip (* undo Thumb encoding *)
  in
  let lookup targ l =
    List.fold_left
      (fun ret (addr, retval) -> 
	 if ((canon_eip addr) = (canon_eip targ)) then Some (retval) else ret)
      None l
  in
    match ((lookup eip      !opt_skip_func_addr),
	   (lookup eip      !opt_skip_func_addr_symbol),
	   (lookup eip      !opt_skip_func_addr_region),
	   (lookup last_eip !opt_skip_call_addr),
	   (lookup last_eip !opt_skip_call_addr_symbol),
	   (lookup last_eip !opt_skip_call_addr_region))
    with
      | (None, None, None, None, None, None) -> None
      | (Some sfa_val, None, None, None, None, None) ->
	  Some (fun () -> fm#set_word_var ret_reg sfa_val)
      | (None, Some sfas_sym, None, None, None, None) ->
	  Some (fun () -> fm#set_word_reg_fresh_symbolic ret_reg sfas_sym)
      | (None, None, Some sfar_sym, None, None, None) ->
	  Some (fun () -> fm#set_word_reg_fresh_region ret_reg sfar_sym)
      | (None, None, None, Some cfa_val, None, None) ->
	  Some (fun () -> fm#set_word_var ret_reg cfa_val)
      | (None, None, None, None, Some cfas_sym, None) ->
	  Some (fun () -> fm#set_word_reg_fresh_symbolic ret_reg cfas_sym)
      | (None, None, None, None, None, Some cfar_sym) ->
	  Some (fun () -> fm#set_word_reg_fresh_region ret_reg cfar_sym)
      | _ -> failwith "Contradictory replacement options"

let loop_detect = Hashtbl.create 1000
    
let decode_insns_cached fm gamma eip =
  let decode_call _ = decode_insns fm gamma eip !opt_bb_size in
  let (decls, stmts) as return =
    match (find_veritesting_region fm gamma eip !opt_bb_size) with
    | None -> with_trans_cache eip decode_call
    | Some progn -> progn in
  if false
  then (Printf.printf "Printing statement list:\n";
	List.iter (fun s -> V.stmt_to_channel stdout s) stmts;
	Printf.printf "End statement list\n";
	Printf.printf "Printing decls:\n";
	List.iter (fun s -> V.decl_to_channel stdout s; Printf.printf "\n") decls;
	Printf.printf "End decls\n");
  return


let runloop (fm : fragment_machine) eip asmir_gamma until =
  let rec loop last_eip eip =
    (let old_count =
       (try
	  Hashtbl.find loop_detect eip
	with Not_found ->
	  Hashtbl.replace loop_detect eip 1L;
	  1L)
     in
       Hashtbl.replace loop_detect eip (Int64.succ old_count);
       if old_count > !opt_iteration_limit then raise TooManyIterations);
    let (dl, sl) as prog = decode_insns_cached fm asmir_gamma eip in
    let prog' = match call_replacements fm last_eip eip with
	| None -> prog
	| Some thunk ->
	    thunk ();
	    let fake_ret = match (!opt_arch, (Int64.logand eip 1L)) with
	      | (X86, _) -> [|'\xc3'|] (* ret *)
	      | (X64, _) -> [|'\xc3'|] (* ret *)
	      | (ARM, 0L) -> [|'\x1e'; '\xff'; '\x2f'; '\xe1'|] (* bx lr *)
	      | (ARM, 1L) -> [|'\x70'; '\x47'|] (* bx lr (Thumb) *)
	      | (ARM, _) -> failwith "Can't happen (logand with 1)"
	    in
	      decode_insn asmir_gamma eip fake_ret
      in
	if !opt_trace_insns
	then print_insns eip prog' None '\n';
	if !opt_trace_ir
	then V.pp_program print_string prog';
	fm#set_frag prog';
	(* flush stdout; *)
	let new_eip = label_to_eip (fm#run ()) in
	match (new_eip, until) with
	| (e1, until_fn) when until_fn e1 -> () (* halt recursion when until is true *)
	| (0L, _) -> raise JumpToNull           (* new eip is null *)
	| _ -> loop eip new_eip                 (* keep going *)
  in
    Hashtbl.clear loop_detect;
    loop (0L) eip

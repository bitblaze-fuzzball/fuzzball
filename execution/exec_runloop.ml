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
  let (ret_reg, set_reg_conc, set_reg_sym, set_reg_fresh) =
    match !opt_arch with
    | X86 -> (R_EAX, fm#set_word_var, fm#set_word_reg_symbolic,
	      fm#set_word_reg_fresh_symbolic)
    | X64 -> (R_RAX, fm#set_long_var, fm#set_long_reg_symbolic,
	      fm#set_long_reg_fresh_symbolic)
    | ARM -> (R0, fm#set_word_var, fm#set_word_reg_symbolic,
	      fm#set_word_reg_fresh_symbolic)
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
	   (lookup last_eip !opt_skip_call_addr_symbol_once),
	   (lookup last_eip !opt_skip_call_addr_region))
    with
      | (None, None, None, None, None, None, None) -> None
      | (Some sfa_val, None, None, None, None, None, None) ->
	  Some (fun () -> set_reg_conc ret_reg sfa_val)
      | (None, Some sfas_sym, None, None, None, None, None) ->
	  Some (fun () -> ignore(set_reg_fresh ret_reg sfas_sym))
      | (None, None, Some sfar_sym, None, None, None, None) ->
	  Some (fun () -> fm#set_reg_fresh_region ret_reg sfar_sym)
      | (None, None, None, Some cfa_val, None, None, None) ->
	  Some (fun () -> set_reg_conc ret_reg cfa_val)
      | (None, None, None, None, Some cfas_sym, None, None) ->
	  Some (fun () -> ignore(set_reg_fresh ret_reg cfas_sym))
      | (None, None, None, None, None, Some cfaso_sym, None) ->
	  Some (fun () -> set_reg_sym ret_reg cfaso_sym)
      | (None, None, None, None, None, None, Some cfar_sym) ->
	  Some (fun () -> fm#set_reg_fresh_region ret_reg cfar_sym)
      | _ -> failwith "Contradictory replacement options"

let loop_detect = Hashtbl.create 1000
    
let decode_insns_cached fm gamma eip =
  let decode_call _ = decode_insns fm gamma eip !opt_bb_size
  and veritest_call _ = find_veritesting_region fm gamma eip !opt_bb_size in
  let (decls, stmts) as return =
    match (some_none_trans_cache eip veritest_call) with
    | None -> with_trans_cache eip decode_call
    | Some progn -> progn in
  if false then
    begin
      Printf.eprintf "Printing statement list:\n";
      List.iter (fun s -> V.stmt_to_channel stdout s) stmts;
      Printf.eprintf "End statement list: %d\n" (List.length stmts);
      Printf.eprintf "Printing decls:\n";
      List.iter (fun s -> V.decl_to_channel stdout s; Printf.eprintf "\n") decls;
      Printf.eprintf "End decls\n";
      flush stdout
    end;
  return


let runloop (fm : fragment_machine) eip asmir_gamma until =
  let rec loop last_eip eip is_final_loop num_insns_executed =
    (match fm#maybe_switch_proc eip with
       | Some eip' ->
	   clear_trans_cache (); (* drastic, better to make tagged *)
	   loop (0L) eip' false num_insns_executed
       | None -> ());
    (* Currently disabled: advance the reset point if we've got symbolic
       data but we haven't yet branched on it. *)
    if false && fm#before_first_branch && fm#started_symbolic then (
	fm#make_snap ();
	fm#set_start_eip eip);
    (let old_count =
       (try
	  Hashtbl.find loop_detect eip
	with Not_found ->
	  Hashtbl.replace loop_detect eip 1L;
	  1L)
     in
       Hashtbl.replace loop_detect eip (Int64.succ old_count);
       (match !opt_iteration_limit_enforced with
       | Some lim -> if old_count > lim then raise TooManyIterations
       | _ -> ()););
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
	let new_label = fm#run () in
	let new_eip = 
	  try 
	    label_to_eip (new_label)
	  with Failure s -> failwith (Printf.sprintf "Couldn't decode eip in runloop: %s" s) in
	  if is_final_loop then
	    Printf.eprintf "End jump to: %Lx\n" new_eip
	  else if num_insns_executed = !opt_insn_limit then
	    Printf.eprintf "Stopping after %Ld insns\n" !opt_insn_limit
	  else
	    match (new_eip, until, !opt_trace_end_jump) with
	      | (e1, _, Some e2) when e1 = e2 ->
                  loop eip new_eip true (Int64.succ num_insns_executed)
		(* halt execution next time if we hit -trace-end-jump *)
	      | (e1, until_fn, _) when until_fn e1 -> ()
		(* halt execution when until_fn returns true *)
	      | (0L, _, _) -> raise JumpToNull (* next eip is 0, stop *)
	      | _ -> loop eip new_eip false (Int64.succ num_insns_executed)
                (* otherwise keep going *)
  in
    Hashtbl.clear loop_detect;
    loop (0L) eip false 1L

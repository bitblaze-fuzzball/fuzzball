(*
  Copyright (C) BitBlaze, 2009-2011. All rights reserved.
*)

module V = Vine

open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

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

let decode_insn_at fm gamma eipT =
  try
    let insn_addr = match !opt_arch with
      | ARM ->
	  (* For a Thumb instruction, change the last bit to zero to
	     find the real instruction address *)
	  if Int64.logand eipT 1L = 1L then
	    Int64.logand eipT (Int64.lognot 1L)
	  else
	    eipT
      | _ -> eipT
    in
    (* The number of bytes read must be long enough to cover any
       single instruction. Naively you might think that 16 bytes
       would be enough, but Valgrind client requests count as single
       instructions for VEX, and they can be up to 19 bytes on
       x86-64 and 20 bytes on ARM. *)
    let bytes = Array.init 20
      (fun i -> Char.chr (fm#load_byte_conc
			    (Int64.add insn_addr (Int64.of_int i))))
    in
    let prog = decode_insn gamma eipT bytes in
      prog
  with
      NotConcrete(_) ->
	Printf.printf "Jump to symbolic memory 0x%08Lx\n" eipT;
	raise IllegalInstruction

let rec last l =
  match l with
    | [e] -> e
    | a :: r -> last r
    | [] -> failwith "Empty list in last"

let rec decode_insns fm gamma eip k first =
  if k = 0 then ([], []) else
    let (dl, sl) = decode_insn_at fm gamma eip in
      if
	List.exists (function V.Special("int 0x80") -> true | _ -> false) sl
      then
	(* Make a system call be alone in its basic block *)
	if first then (dl, sl) else ([], [])
      else
	match last (rm_unused_stmts sl) with
	  | V.Jmp(V.Name(lab)) when lab <> "pc_0x0" ->
	      let next_eip = label_to_eip lab in
	      let (dl', sl') = decode_insns fm gamma next_eip (k - 1) false in
		(dl @ dl', sl @ sl')
	  | _ -> (dl, sl) (* end of basic block, e.g. indirect jump *)

let bb_size = 1

let decode_insns_cached fm gamma eip =
  with_trans_cache eip (fun () -> decode_insns fm gamma eip bb_size true)

let rec runloop (fm : fragment_machine) eip asmir_gamma until =
  let rec loop last_eip eip is_final_loop num_insns_executed =
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
    let (dl, sl) = decode_insns_cached fm asmir_gamma eip in
    let prog = (dl, sl) in
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
	if !opt_trace_insns then
	  print_insns eip prog' None '\n';
	if !opt_trace_ir then
	  V.pp_program print_string prog';
	fm#set_frag prog';
	(* flush stdout; *)
	let new_eip = label_to_eip (fm#run ()) in
	  if is_final_loop then
	    Printf.printf "End jump to: %Lx\n" new_eip
	  else if num_insns_executed = !opt_insn_limit then
	    Printf.printf "Stopping after %Ld insns\n" !opt_insn_limit
	  else
	    match (new_eip, until, !opt_trace_end_jump) with
	      | (e1, e2, Some e3) when e1 = e3 ->
	          loop eip new_eip true (Int64.succ num_insns_executed)
	      | (e1, e2, _) when e2 e1 -> ()
	      | (0L, _, _) -> raise JumpToNull
	      | _ -> loop eip new_eip false (Int64.succ num_insns_executed)
  in
    Hashtbl.clear loop_detect;
    loop (0L) eip false 1L

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

let rec last l =
  match l with
  | [e] -> e
  | a :: r -> last r
  | [] -> failwith "Empty list in last"


let rec has_special = function
  | [] -> false
  | hd::tl ->
    match hd with
    | V.Special("int 0x80") -> true
    | _ -> has_special tl


let tuple_push (dl,sl) (dl',sl') = dl::dl', sl::sl'

let decode_insns fm gamma starting_eip k =
  let bottom = ([] , []) in
  let rec decode_insns_int eip remaining =
    if remaining = 0
    then bottom (* base case -- exceeded maximum block size *)
    else let (_,sl) as cur_tup = decode_insn_at fm gamma eip in
	 if has_special sl
	 then
	   if (remaining = k) (* this is the first recursive call *)
	   then tuple_push cur_tup bottom (* base case -- first inst is a system call *)
	   else bottom
	 else match last (rm_unused_stmts sl) with
	 | V.Jmp(V.Name(lab)) when lab <> "pc_0x0" ->
	   let next_eip = label_to_eip lab
	   and remaining' = remaining - 1 in
	   tuple_push cur_tup (decode_insns_int next_eip remaining')
	 | _ -> tuple_push cur_tup bottom in (* end of basic block, e.g. indirect jump *)
  let decl_list_list, statement_list_list = decode_insns_int starting_eip k in
  List.concat decl_list_list,
  List.concat statement_list_list
    
let decode_insns_cached fm gamma eip =
  let decode_call _ = decode_insns fm gamma eip !opt_bb_size in

  (** Uncomment me if you want to play with the veritesting region identification code.
      Comment me out again if you want to be able to run fuzzball. **)
  
  (* ignore(find_veritesting_region_m2 fm gamma eip !opt_bb_size); *)
  with_trans_cache eip decode_call

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

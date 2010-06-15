(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module V = Vine

open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

let call_replacements fm eip =
  let eaxreplace = List.fold_left
    (fun ret (addr, retval) -> 
       if (addr = eip) then Some (retval) else ret)
    None !opt_skip_call_addr in
    match eaxreplace with 
      | Some(x) -> Some (fun () -> fm#set_word_var R_EAX x)
      | None ->
	  let eax_sym = List.fold_left
	    (fun ret (addr, symname) -> 
	       if (addr = eip) then Some symname else ret)
	    None !opt_skip_call_addr_symbol in
	    match eax_sym with 
	      | Some symname ->
		  Some (fun () -> fm#set_word_reg_fresh_symbolic R_EAX symname)
	      | _ -> None

let loop_detect = Hashtbl.create 1000

let decode_insn_at fm gamma eip =
  try
    let bytes = Array.init 16
      (fun i -> Char.chr (fm#load_byte_conc (Int64.add eip (Int64.of_int i))))
    in
    let prog = decode_insn gamma eip bytes in
      prog
  with
      NotConcrete(_) ->
	Printf.printf "Jump to symbolic memory 0x%08Lx\n" eip;
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
  let rec loop eip =
    (let old_count =
       (try
	  Hashtbl.find loop_detect eip
	with Not_found ->
	  Hashtbl.replace loop_detect eip 1L;
	  1L)
     in
       Hashtbl.replace loop_detect eip (Int64.succ old_count);
       if old_count > !opt_iteration_limit then raise TooManyIterations);
    let (dl, sl) = decode_insns_cached fm asmir_gamma eip in
    let prog = (dl, sl) in
      (* Libasmir.print_disasm_rawbytes Libasmir.Bfd_arch_i386 eip insn_bytes;
	 print_string "\n"; *)
      if !opt_trace_registers then
	fm#print_x86_regs;
      if !opt_trace_eip then
	Printf.printf "EIP is 0x%08Lx\n" eip;
      fm#set_eip eip;
      (* Printf.printf "EFLAGSREST is %08Lx\n" (fm#get_word_var EFLAGSREST);*)
      fm#watchpoint;
      let prog' = match call_replacements fm eip with
	| None -> prog
	| Some thunk ->
	    thunk ();
	    decode_insn asmir_gamma eip [|'\xc3'|] (* fake "ret" *)
      in
	if !opt_trace_insns then
	  print_insns eip prog' None '\n';
	if !opt_trace_ir then
	  V.pp_program print_string prog';
	fm#set_frag prog';
	fm#run_eip_hooks;
	(* flush stdout; *)
	let new_eip = label_to_eip (fm#run ()) in
	  match (new_eip, until) with
	    | (e1, e2) when e2 e1 -> ()
	    | (0L, _) -> raise JumpToNull
	    | _ -> loop new_eip
  in
    Hashtbl.clear loop_detect;
    loop eip

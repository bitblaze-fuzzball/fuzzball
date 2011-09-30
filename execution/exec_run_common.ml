(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;
module FM = Fragment_machine;;

open Exec_exceptions;;
open Exec_options;;

let decode_insn asmir_gamma eip insn_bytes =
  (* It's important to flush buffers here because VEX will also
     print error messages to stdout, but its buffers are different from
     OCaml's. *)
  flush stdout;
  let arch = asmir_arch_of_execution_arch !opt_arch in
  let sl = Asmir.asm_bytes_to_vine asmir_gamma arch eip insn_bytes in
    match sl with 
      | [V.Block(dl', sl')] ->
	  if !opt_trace_orig_ir then
	    V.pp_program print_string (dl', sl');
	  (dl', sl')
      | _ -> failwith "expected asm_addr_to_vine to give single block"

let label_to_eip s =
  let len = String.length s in
  let hex = String.sub s 3 (len - 3) in (* remove "pc_" *)
  let eip = Int64.of_string hex in
    eip

let known_unknowns = (
  let h = Hashtbl.create 11 in
    Hashtbl.replace h "Unknown: GetI" ();
    Hashtbl.replace h "NegF64" ();
    Hashtbl.replace h "Floating point binop" ();
    Hashtbl.replace h "Floating point triop" ();
    Hashtbl.replace h "floatcast" ();
    Hashtbl.replace h "CCall: x86g_create_fpucw" ();
    Hashtbl.replace h "CCall: x86g_calculate_FXAM" ();
    Hashtbl.replace h "CCall: x86g_check_fldcw" ();
    h)

(* Disable "unknown" statments it seems safe to ignore *)
let noop_known_unknowns (dl, sl) = 
  (dl,
   List.map
     (function
	| V.Move((V.Temp(_,_,ty) as lhs),
		 V.Unknown(msg)) when Hashtbl.mem known_unknowns msg -> 
	    V.Move(lhs, V.Constant(V.Int(ty, 0L)))
	| V.ExpStmt(V.Unknown("Unknown: PutI")) ->
	    V.Comment("Unknown: PutI")
	| s -> s) sl)

let trans_cache = Hashtbl.create 100001

let with_trans_cache (eip:int64) fn =
  (match !opt_translation_cache_size with
     | Some limit ->
	 if Hashtbl.length trans_cache > limit then
	   Hashtbl.clear trans_cache
     | None -> ());
  try
    Hashtbl.find trans_cache eip
  with
      Not_found ->
	let (dl, sl) = (fn ()) in
	  Hashtbl.add trans_cache eip
	    (Frag_simplify.simplify_frag (noop_known_unknowns (dl, sl)));
	  Hashtbl.find trans_cache eip

let skip_strings = 
  (let h = Hashtbl.create 2 in
     Hashtbl.replace h "NoOp" ();
     Hashtbl.replace h "x86g_use_seg_selector" ();
     h)

let print_insns start_eip (_, sl) insn_num endl =
  let eip = ref (Some start_eip) in
  let print_eip () = 
    match (insn_num, !eip) with
      | (Some i, Some pc) -> Printf.printf "%10Ld %08Lx: " i pc; eip := None
      | (None, Some pc) -> Printf.printf "%08Lx: " pc; eip := None
      | (Some _, None) -> Printf.printf "                     "
      | (None, None) -> Printf.printf "          "
  in
    List.iter
      (function
	 | V.Comment(s) ->
	     if not (Hashtbl.mem skip_strings s) &&
               ((String.length s < 13) ||
                  (String.sub s 0 13) <> "eflags thunk:")
	     then
	       (print_eip();
		Printf.printf "%s%c" s endl)
	 | V.Label(lab) ->
	     if (String.length lab > 5) &&
	       (String.sub lab 0 5) = "pc_0x" then
		 eip := Some (label_to_eip lab)
	 | _ -> ()
      )
      sl

let run_one_insn fm gamma eip bytes =
  let (dl, sl) = with_trans_cache eip (fun () -> decode_insn gamma eip bytes)
  in
  let prog = (dl, sl) in
    if !opt_trace_eip then
      Printf.printf "EIP is 0x%08Lx\n" eip;
    fm#set_eip eip;
    if !opt_trace_registers then
      fm#print_regs;
    fm#watchpoint;
    if !opt_trace_insns then
      print_insns eip prog None '\n';
    if !opt_trace_ir then
      V.pp_program print_string prog;
    fm#set_frag prog;
    fm#run_eip_hooks;
    (* flush stdout; *)
    let next_lab = fm#run () in
      if !opt_trace_registers then
	fm#print_regs;
      label_to_eip next_lab


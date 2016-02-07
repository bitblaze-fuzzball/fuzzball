(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;
module FM = Fragment_machine;;

open Exec_exceptions;;
open Exec_options;;

let match_faked_insn eip insn_bytes =
  match (!opt_arch,
	 !opt_nop_system_insns || (!opt_x87_entry_point <> None)) with
    | (_,  false) -> None 
    | (ARM, true) -> None (* nothing implemented *)
    | (X64, true) -> None (* nothing implemented *)
    | (X86, true) ->
	(* Pretend to decode, but just to a no-op or a trap, some
	   system instructions that a VEX-based LibASMIR doesn't handle. *)
	(assert((Array.length insn_bytes) >= 1);
	 let maybe_byte i =
	   if (Array.length insn_bytes) <= i then
	     -1
	   else
	     Char.code (insn_bytes.(i))
	 in
	 let b0 = Char.code (insn_bytes.(0)) and
	     b1 = maybe_byte 1 and
	     b2 = maybe_byte 2 and
	     b3 = maybe_byte 3 in
	 let modrm_reg b = (b lsr 3) land 0x7 in
	 let modrm_has_sib = function
	   | 0x04 | 0x0c | 0x14 | 0x1c | 0x24 | 0x2c | 0x34 | 0x3c
	   | 0x44 | 0x4c | 0x54 | 0x5c | 0x64 | 0x6c | 0x74 | 0x7c
	   | 0x84 | 0x8c | 0x94 | 0x9c | 0xa4 | 0xac | 0xb4 | 0xbc
	       -> true
	   | _ -> false
	 in
	 (* Compute the total number of bytes used by the ModR/M byte,
	    the SIB byte if present, and the displacement they specify if
	    present, given the values of the ModR/M byte and the SIB byte
	    (if present) *)
	 let modrm_len mr sib = 
	   1 + 
	     (if 0x40 <= mr && mr <= 0x7f then 1
	      else if 0x80 <= mr && mr <= 0xbf then 4
	      else if mr >= 0xc0 then 0
	      else if modrm_has_sib mr then
		1 + (if sib land 0x7 = 5 then 4 else 0)
	      else if (mr land 7) = 5 then 4
	      else 0)
	 in
	 let (comment, maybe_len) =
	   match (b0, b1,
		  !opt_nop_system_insns, (!opt_x87_entry_point <> None))
	   with
	     | (0x0f, 0x00, true, _) when (modrm_reg b2) = 0 ->
		 ("sldt", 2 + (modrm_len b2 b3))
	     | (0x0f, 0x00, true, _) when (modrm_reg b2) = 1 ->
		 ("str", 2 + (modrm_len b2 b3))
	     | (0x0f, 0x01, true, _) when (modrm_reg b2) = 0 ->
		 ("sgdt", 2 + (modrm_len b2 b3))
	     | (0x0f, 0x01, true, _) when (modrm_reg b2) = 1 ->
		 ("sidt", 2 + (modrm_len b2 b3))
	     | (0x0f, 0x0b, true, _) -> ("ud2",          2)
	     | (0x0f, 0x20, true, _) -> ("mov from %cr", 3)
	     | (0x0f, 0x21, true, _) -> ("mov from %db", 3)
	     | (0x0f, 0x22, true, _) -> ("mov to %cr",   3)
	     | (0x0f, 0x23, true, _) -> ("mov to %db",   3)
	     | (0xcd, 0x2d, true, _) -> ("int $0x2d",   -1)
		 (* Windows Debug Service Handler *)
	     | (0xf4, _, true, _)    -> ("hlt",         -1)
	     | (0xfa, _, true, _)    -> ("cli",          1)
	     | (0xfb, _, true, _)    -> ("sti",          1)
	     | ((0xd8|0xd9|0xda|0xdb|0xdc|0xdd|0xde|0xdf), _, _, true) ->
		 ("x87 FPU insn", -2)
	     | _ -> ("", 0)
	 in
	 let maybe_sl =
	   match maybe_len with
	     | 0 -> None
	     | -1 -> Some [V.Special("trap")]
	     | -2 -> Some [V.Special("x87 emulator trap")]
	     | len ->
		 let new_eip = Int64.add eip (Int64.of_int len) in
		   Some [V.Jmp(V.Name(V.addr_to_label new_eip))]
	 in
	   match maybe_sl with
	     | None -> None
	     | Some sl -> Some [V.Block([], [V.Label(V.addr_to_label eip);
					     V.Comment("fake " ^ comment)] @
					  sl)]
	)

let decode_insn asmir_gamma eip insn_bytes =
  (* It's important to flush buffers here because VEX will also
     print error messages to stdout, but its buffers are different from
     OCaml's. *)
  flush stdout;
  let arch = asmir_arch_of_execution_arch !opt_arch in
  let sl = match match_faked_insn eip insn_bytes with
    | Some sl -> sl
    | _ -> Asmir.asm_bytes_to_vine asmir_gamma arch eip insn_bytes
  in
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
    Hashtbl.replace h "CCall: x86g_create_fpucw" (); (* s/b handled now *)
    Hashtbl.replace h "CCall: x86g_calculate_FXAM" ();
    Hashtbl.replace h "CCall: x86g_check_fldcw" (); (* s/b handled now *)
    Hashtbl.replace h "loadF80" ();
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
	| V.ExpStmt(V.Unknown("Unknown: loadF80")) ->
	    V.Comment("Unknown: loadF80")
	| V.ExpStmt(V.Unknown("Unknown: storeF80")) ->
	    V.Comment("Unknown: storeF80")
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
	     if FM.comment_is_insn s then
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


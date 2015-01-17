(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine
module FM = Fragment_machine
module FS = Frag_simplify

open Exec_exceptions
open Exec_options


let match_faked_insn eip insn_bytes =
  let fake_system = !opt_nop_system_insns and
      fake_x87 = !opt_x87_entry_point <> None
  in
  let fake_any = fake_system || fake_x87 in
  match (!opt_arch, fake_any) with
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
	   match (b0, b1, fake_system, fake_x87) with
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

let mem_bytemap = Hashtbl.create 100001	(*each memory byte -> correspoding trans_cache entry*)
let invalid_list = Hashtbl.create 100	(*if remove KEY from trans_cache, also remove VALUE*)

(*Fill two hashtbls, mem_bytemap and invalid_list, while decoding each asm instruction*)
let with_mem_bytemap (s_addr:int64) (size:int) (trans_addr:int64) =
	let handle_each_byte addr =  
	if Hashtbl.mem mem_bytemap addr then(
		let prev = Hashtbl.find mem_bytemap addr in
		Hashtbl.replace mem_bytemap addr trans_addr;
		if not (Hashtbl.mem invalid_list trans_addr) then(
			Hashtbl.add invalid_list trans_addr prev;
			(*Printf.printf "[invalid_list] Add 0x%08Lx 0x%08Lx\n" trans_addr prev*))
		)
	else
		Hashtbl.add mem_bytemap addr trans_addr
	in	 
	for offset = 0 to size do
		let byte_addr = Int64.add s_addr (Int64.of_int offset) in
		handle_each_byte byte_addr 
	done	
	
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
    with_mem_bytemap eip (Array.length insn_bytes) eip;
    match sl with 
      | [V.Block(dl', sl')] ->
	  if !opt_trace_orig_ir then
	    V.pp_program print_string (dl', sl');
	  (dl', sl')
      | _ -> failwith "expected asm_addr_to_vine to give single block"

let label_to_eip s =
  if (s.[0] = 'p') && (s.[1] = 'c') && (s.[2] = '_')
  then
    let len = String.length s in
    let hex = String.sub s 3 (len - 3) in (* remove "pc_" *)
    let eip = Int64.of_string hex in
    eip
  else failwith (Printf.sprintf "label_to_eip: |%s| isn't of the expected form pc_<hex>" s)

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
    Hashtbl.replace h "register type (I64)" ();
    Hashtbl.replace h "register type (F32)" ();
    Hashtbl.replace h "register type (F64)" ();
    Hashtbl.replace h "register type (I128)" ();
    Hashtbl.replace h "register type (V128)" ();
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

let clear_trans_cache () =
  match !opt_translation_cache_size with
  | Some limit ->
    if Hashtbl.length trans_cache > limit then
      Hashtbl.clear trans_cache
  | None -> ()

let with_trans_cache (eip:int64) fn =
  clear_trans_cache ();
  try
    Hashtbl.find trans_cache eip
  with
      Not_found ->
	let (dl, sl) = (fn ()) in
	let to_add = Frag_simplify.simplify_frag (noop_known_unknowns (dl, sl)) in
	Hashtbl.add trans_cache eip to_add;
	to_add

let some_none_trans_cache (eip:int64) fn =
  clear_trans_cache ();
  try
    Some (Hashtbl.find trans_cache eip)
  with
      Not_found ->
	begin
	  match (fn ()) with
	  | None -> None
	  | Some (dl,sl) ->
	    begin
	      let to_add = Frag_simplify.simplify_frag (noop_known_unknowns (dl, sl)) in
	      Hashtbl.add trans_cache eip to_add;
	      Some to_add
	    end
	end



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

(* Here's an example of the pattern we're matching: 
 804a75d:       0f b7 45 f6             movzwl -0xa(%ebp),%eax ; w0 (past)
 804a761:       f2 0f 2a c0             cvtsi2sd %eax,%xmm0    ; w1
 804a765:       f2 0f 10 0d 30 b9 04    movsd  0x804b930,%xmm1 ; w2, w3
 804a76c:       08 
 804a76d:       f2 0f 5e c1             divsd  %xmm1,%xmm0     ; w4
 804a771:       f2 0f 2c c0             cvttsd2si %xmm0,%eax   ; w5 *)
let fancy_faked_insn eip fm gamma =
  (* It happens that all the chunks of bytes we need to load for this
     matching process are 4 bytes long, so this FM method works well.
     It's a little weird, though, that the "words" are generally not
     naturally aligned, and the byte order reads backwards. *) 
  let read4 addr = fm#load_word_conc addr in
  let fake_sse = !opt_sse_emulator = Some "punt" in
  if not fake_sse then None else
    let w1 = read4 eip in
      if w1 <> 0xc02a0ff2L then None else
	let w0 = read4 (Int64.sub eip  4L) and
	    w2 = read4 (Int64.add eip  4L) and
	    w3 = read4 (Int64.add eip  8L) and
	    w4 = read4 (Int64.add eip 12L) and
	    w5 = read4 (Int64.add eip 16L) in
	  if (Int64.logand w0 0xffffL) = 0xb70fL &&
	    w2 = 0x0d100ff2L && w4 = 0xc15e0ff2L && w5 = 0xc02c0ff2L then
	      let (known, num, denom) = match fm#load_long_conc w3 with
		| 0x3fe8000000000000L -> (true, 4L, 3L) (* 0.75 *)
		| _ ->
		    (false, 0L, 0L)
	      in
		if not known then None else
		  let eax_v = Asmir.gamma_lookup gamma "R_EAX" and
		      num_c = V.Constant(V.Int(V.REG_32, num)) and
		      denom_c = V.Constant(V.Int(V.REG_32, denom)) in
		  let new_eip = Int64.add eip 20L in
		  let sl =
		    [V.Label(V.addr_to_label eip);
		     V.Comment("fake special SSE int/float divide");
		     V.Move(V.Temp(eax_v),
			    V.BinOp(V.SDIVIDE, 
				    V.BinOp(V.TIMES,
					    V.Lval(V.Temp(eax_v)), num_c),
				    denom_c));
		     V.Jmp(V.Name(V.addr_to_label new_eip))]
		  in
		    Some (([], sl), 20)
	  else
	    None

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
    let faked_info = fancy_faked_insn eipT fm gamma in
      match faked_info with
	| Some (faked_prog, faked_len) ->
	    with_mem_bytemap eipT faked_len eipT;
	    faked_prog
	| None ->
	    let bytes = Array.init 16
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
	 else match last (FS.rm_unused_stmts sl) with
	 | V.Jmp(V.Name(lab)) when lab <> "pc_0x0" ->
	   let next_eip = label_to_eip lab
	   and remaining' = remaining - 1 in
	   tuple_push cur_tup (decode_insns_int next_eip remaining')
	 | _ -> tuple_push cur_tup bottom in (* end of basic block, e.g. indirect jump *)
  let decl_list_list, statement_list_list = decode_insns_int starting_eip k in  
  List.concat decl_list_list,
  List.concat statement_list_list

(*Erase trans cache entry indexed by addr and all related entries indicated by invalid_list*)
let rec erase_trans_cache addr = 
	(*Printf.printf "Size of invalid_list : %d\n" (Hashtbl.length invalid_list);*)
	Hashtbl.remove trans_cache addr;
	(*Printf.printf "Erase Trans_cache 0x%08Lx\n" addr;*)
	if Hashtbl.mem invalid_list addr then(
		let prev_addr = Hashtbl.find invalid_list addr in 
		(*Printf.printf "		0x%08Lx should be erased next\n" prev_addr;*)
		Hashtbl.remove invalid_list addr;
		erase_trans_cache prev_addr)
	

let add_remove_hook fm =
	let hook s_addr size =
		for offset = 0 to size do
			(*Hashtbl.remove mem_bytemap (Int64.of_int byte);*)
			let byte = Int64.add s_addr (Int64.of_int offset) in
			if Hashtbl.mem mem_bytemap byte then (
				let addr = Hashtbl.find mem_bytemap byte in
				erase_trans_cache addr;)
		done
	in
	fm#add_extra_store_hook hook
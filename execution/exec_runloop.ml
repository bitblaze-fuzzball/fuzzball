(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;
open Frag_simplify;;
open Fragment_machine;;

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
		  Some (fun () -> fm#set_word_reg_symbolic R_EAX symname)
	      | _ -> None

let trans_cache = Hashtbl.create 100000 

let loop_detect = Hashtbl.create 1000

let rec runloop (fm : fragment_machine) eip asmir_gamma until =
  let load_byte addr = fm#load_byte_conc addr in
  let decode_insn eip insn_bytes =
    (* It's important to flush buffers here because VEX will also
       print error messages to stdout, but its buffers are different from
       OCaml's. *)
    flush stdout;
    let asmp = Libasmir.byte_insn_to_asmp
      Libasmir.Bfd_arch_i386 eip insn_bytes in
    let sl = Asmir.asm_addr_to_vine asmir_gamma asmp eip in
      Libasmir.free_asm_program asmp;
      match sl with 
	| [V.Block(dl', sl')] -> (dl', sl')
	| _ -> failwith "expected asm_addr_to_vine to give single block"
  in
  let decode_insn_at eip =
    try
      let bytes = Array.init 16
	(fun i -> Char.chr (load_byte (Int64.add eip (Int64.of_int i))))
      in
      let prog = decode_insn eip bytes in
	if !opt_trace_orig_ir then
	  V.pp_program print_string prog;
	prog
    with
	NotConcrete(_) ->
	  Printf.printf "Jump to symbolic memory 0x%08Lx\n" eip;
	  raise IllegalInstruction
  in
  let label_to_eip s =
    let len = String.length s in
    let hex = String.sub s 3 (len - 3) in (* remove "pc_" *)
    let eip = Int64.of_string hex in
      eip
  in
  let rec last l =
    match l with
      | [e] -> e
      | a :: r -> last r
      | [] -> failwith "Empty list in last"
  in
  let rec decode_insns eip k first =
    if k = 0 then ([], []) else
      let (dl, sl) = decode_insn_at eip in
	if
	  List.exists (function V.Special("int 0x80") -> true | _ -> false) sl
	then
	  (* Make a system call be alone in its basic block *)
	  if first then (dl, sl) else ([], [])
	else
	  match last (rm_unused_stmts sl) with
	    | V.Jmp(V.Name(lab)) when lab <> "pc_0x0" ->
		let next_eip = label_to_eip lab in
		let (dl', sl') = decode_insns next_eip (k - 1) false in
		  (dl @ dl', sl @ sl')
	    | _ -> (dl, sl) (* end of basic block, e.g. indirect jump *)
  in
  (* Disable "unknown" statments it seems safe to ignore *)
  let noop_known_unknowns (dl, sl) = 
    (dl,
     List.map
       (function
	  | V.Move((V.Temp(_,_,ty) as lhs),
		   V.Unknown("Unknown: GetI"|"Floating point binop"|
				 "Floating point triop"|"floatcast"|
				     "CCall: x86g_create_fpucw"|"CCall: x86g_calculate_FXAM"|
					 "CCall: x86g_check_fldcw"))
	    -> V.Move(lhs, V.Constant(V.Int(ty, 0L)))
	  | V.ExpStmt(V.Unknown("Unknown: PutI")) 
	    -> V.Comment("Unknown: PutI")
	  | s -> s) sl)
  in
  let decode_insns_cached eip =
    try
      Hashtbl.find trans_cache eip
    with
	Not_found ->
	  let (dl, sl) = (decode_insns eip 1 true) in
	    Hashtbl.add trans_cache eip
	      (simplify_frag  (noop_known_unknowns (dl, sl)));
	    Hashtbl.find trans_cache eip
  in
  let print_insns start_eip (_, sl) =
    let eip = ref (Some start_eip) in
    let print_eip () = 
      match !eip with
	| Some i -> Printf.printf "%08Lx: " i; eip := None
	| None -> Printf.printf "          "
    in
      List.iter
	(function
	   | V.Comment(s) ->
	       if s <> "NoOp" &&
		 ((String.length s < 13) ||
		    (String.sub s 0 13) <> "eflags thunk:") then
		   (print_eip();
		    Printf.printf "%s\n" s)
	   | V.Label(lab) ->
	       if (String.length lab > 5) &&
		 (String.sub lab 0 5) = "pc_0x" then
		    eip := Some (label_to_eip lab)
	   | _ -> ()
	)
	sl
  in
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
    let (dl, sl) = decode_insns_cached eip in
    let prog = (dl, sl) in
      (* Libasmir.print_disasm_rawbytes Libasmir.Bfd_arch_i386 eip insn_bytes;
	 print_string "\n"; *)
      (* fm#print_x86_regs; *)
      if !opt_trace_eip then
	Printf.printf "EIP is 0x%08Lx\n" eip;
      fm#set_eip eip;
      (* Printf.printf "EFLAGSREST is %08Lx\n" (fm#get_word_var EFLAGSREST);*)
      fm#watchpoint;
      (* Printf.printf ("Insn bytes are %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n") (load_byte eip)
	 (load_byte (Int64.add eip (Int64.of_int 1)))
	 (load_byte (Int64.add eip (Int64.of_int 2)))
	 (load_byte (Int64.add eip (Int64.of_int 3)))
	 (load_byte (Int64.add eip (Int64.of_int 4)))
	 (load_byte (Int64.add eip (Int64.of_int 5)))
	 (load_byte (Int64.add eip (Int64.of_int 6)))
	 (load_byte (Int64.add eip (Int64.of_int 7)))
	 (load_byte (Int64.add eip (Int64.of_int 8)))
	 (load_byte (Int64.add eip (Int64.of_int 9)))
	 (load_byte (Int64.add eip (Int64.of_int 10)))
	 (load_byte (Int64.add eip (Int64.of_int 11)))
	 (load_byte (Int64.add eip (Int64.of_int 12)))
	 (load_byte (Int64.add eip (Int64.of_int 13)))
	 (load_byte (Int64.add eip (Int64.of_int 14)))
	 (load_byte (Int64.add eip (Int64.of_int 15))); *)
      let prog' = match call_replacements fm eip with
	| None -> prog
	| Some thunk ->
	    thunk ();
	    decode_insn eip [|'\xc3'|] (* fake "ret" *)
      in
	if !opt_trace_insns then
	  print_insns eip prog';
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

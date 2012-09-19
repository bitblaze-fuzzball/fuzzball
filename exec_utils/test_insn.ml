(*
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

let opt_thumb = ref false

module SRFM = Sym_region_frag_machine.SymRegionFragMachineFunctor
  (Symbolic_domain.SymbolicDomain)

let parse_hex_to_bytes str =
  if String.length str = 4 && (String.sub str 0 2) = "0x" then
    [int_of_string str]
  else if String.length str = 6 && (String.sub str 0 2) = "0x" then
    let short = int_of_string str in
    let b1 = short land 0xff and
	b2 = (short lsr 8) land 0xff
    in
      [b1; b2] (* i.e., little endian *)
  else if String.length str = 10 && (String.sub str 0 2) = "0x" then
    let word = Int64.of_string str in
    let b1 = Int64.to_int (Int64.logand word 0xffL) and
	b2 = Int64.to_int (Int64.logand (Int64.shift_right word 8) 0xffL) and
	b3 = Int64.to_int (Int64.logand (Int64.shift_right word 16) 0xffL) and
	b4 = Int64.to_int (Int64.logand (Int64.shift_right word 24) 0xffL)
    in
      [b1; b2; b3; b4] (* i.e., little endian *)
  else
    failwith "Args should look like 0x90, 0xcafe, or 0xfeedface"

let main argv = 
  let bytes_arg = ref [] in
    Arg.parse
      (Arg.align (Exec_set_options.cmdline_opts
		  @ Exec_set_options.concrete_state_cmdline_opts
		  @ Exec_set_options.symbolic_state_cmdline_opts
		  @ Options_solver.solver_cmdline_opts
		  @ Exec_set_options.trace_replay_cmdline_opts
		  @ 
		  [("-thumb", Arg.Set(opt_thumb),
		    " ARM instruction is a Thumb instruction")]
		 ))
      (fun s -> bytes_arg := !bytes_arg @ (parse_hex_to_bytes s))
      "test_insn [options]* 0xfe 0xed 0x42 ...\n";
    let dt = ((new Linear_decision_tree.linear_decision_tree)
		:> Decision_tree.decision_tree) in
    let fm = ((new SRFM.sym_region_frag_machine dt)
	      :> Fragment_machine.fragment_machine) in
    let dl = Asmir.decls_for_arch (Exec_options.asmir_arch ()) in
    let asmir_gamma = Asmir.gamma_create 
      (List.find (fun (i, s, t) -> s = "mem") dl) dl
    in
      fm#init_prog (dl, []);
      Exec_set_options.apply_cmdline_opts_early fm dl;
      Exec_set_options.apply_cmdline_opts_nonlinux fm;
      Options_solver.apply_solver_cmdline_opts fm;
      Exec_set_options.apply_cmdline_opts_late fm;
      if !opt_thumb then assert(!Exec_options.opt_arch = Exec_options.ARM);
      let bytes_l = List.map Char.chr !bytes_arg in
      let code_addr = Int64.logor 0x08048000L 
	(if !opt_thumb then 1L else 0L)
      in
      let bytes_a = Array.of_list bytes_l in
	Array.iteri
	  (fun i b -> fm#store_byte_conc (Int64.add code_addr (Int64.of_int i))
	     (Char.code b)) bytes_a;
	let next_eip = 
	  Exec_run_common.run_one_insn fm asmir_gamma code_addr bytes_a in
	  if !Exec_options.opt_trace_eip then
	    Printf.printf "Next instruction would be at 0x%08Lx\n" next_eip
;;

main Sys.argv;;

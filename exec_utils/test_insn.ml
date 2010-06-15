(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module SRFM = Sym_region_frag_machine.SymRegionFragMachineFunctor
  (Symbolic_domain.SymbolicDomain)

let main argv = 
  let bytes_arg = ref [] in
    Arg.parse
      (Arg.align (Exec_set_options.cmdline_opts
		  @ Exec_set_options.concrete_state_cmdline_opts
		  @ Exec_set_options.symbolic_state_cmdline_opts
		  @ Options_solver.solver_cmdline_opts
		  @ Exec_set_options.trace_replay_cmdline_opts))
      (fun s -> bytes_arg := (int_of_string s) :: !bytes_arg)
      "test_insn [options]* 0xfe 0xed 0x42 ...\n";
    let dt = ((new Linear_decision_tree.linear_decision_tree)
		:> Decision_tree.decision_tree) in
    let fm = ((new SRFM.sym_region_frag_machine dt)
	      :> Fragment_machine.fragment_machine) in
    let dl = Asmir.decls_for_arch Asmir.arch_i386 in
    let asmir_gamma = Asmir.gamma_create 
      (List.find (fun (i, s, t) -> s = "mem") dl) dl
    in
      fm#init_prog (dl, []);
      Exec_set_options.apply_cmdline_opts_early fm dl;
      Exec_set_options.apply_cmdline_opts_nonlinux fm;
      Options_solver.apply_solver_cmdline_opts fm;
      Exec_set_options.apply_cmdline_opts_late fm;
      let bytes_l = List.map Char.chr (List.rev !bytes_arg) in
      let code_addr = 0x08048000L in
      let bytes_a = Array.of_list bytes_l in
	Array.iteri
	  (fun i b -> fm#store_byte_conc (Int64.add code_addr (Int64.of_int i))
	     (Char.code b)) bytes_a;
	if !Exec_options.opt_trace_registers then
	  fm#print_x86_regs;
	Exec_runloop.runloop fm code_addr asmir_gamma (fun _ -> true);
	if !Exec_options.opt_trace_registers then
	  fm#print_x86_regs;
;;

main Sys.argv;;

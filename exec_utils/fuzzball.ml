(*
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module FM = Fragment_machine

module SRFM = Sym_region_frag_machine.SymRegionFragMachineFunctor
  (Symbolic_domain.SymbolicDomain)
module SRFMT = Sym_region_frag_machine.SymRegionFragMachineFunctor
  (Tagged_domain.TaggedDomainFunctor(Symbolic_domain.SymbolicDomain))

module SPFM = Sym_path_frag_machine.SymPathFragMachineFunctor
  (Symbolic_domain.SymbolicDomain)
module SPFMT = Sym_path_frag_machine.SymPathFragMachineFunctor
  (Tagged_domain.TaggedDomainFunctor(Symbolic_domain.SymbolicDomain))

module IM = Exec_influence.InfluenceManagerFunctor
  (Symbolic_domain.SymbolicDomain)
module IMT = Exec_influence.InfluenceManagerFunctor
  (Tagged_domain.TaggedDomainFunctor(Symbolic_domain.SymbolicDomain))

let main argv = 
  Arg.parse
    (Arg.align (Exec_set_options.cmdline_opts
		@ Options_linux.linux_cmdline_opts
		@ State_loader.state_loader_cmdline_opts
		@ Exec_set_options.concrete_state_cmdline_opts
		@ Exec_set_options.symbolic_state_cmdline_opts	
		@ Exec_set_options.concolic_state_cmdline_opts	
		@ Exec_set_options.explore_cmdline_opts
		@ Exec_set_options.tags_cmdline_opts
		@ Exec_set_options.fuzzball_cmdline_opts
		@ Options_solver.solver_cmdline_opts
		@ Exec_set_options.influence_cmdline_opts))
    (fun arg -> Exec_set_options.set_program_name arg)
    "fuzzball [options]* program\n";
  let dt = ((new Binary_decision_tree.binary_decision_tree)
	    :> Decision_tree.decision_tree)#init in
  let (fm, infl_man) = if !Exec_options.opt_use_tags then
    (let srfm = new SRFMT.sym_region_frag_machine dt in
     let spfm = (srfm :> SPFMT.sym_path_frag_machine) in
     let im = new IMT.influence_manager spfm in
       srfm#set_influence_manager im;
       ((srfm :> FM.fragment_machine),
	(im :> Exec_no_influence.influence_manager)))
  else
    (let srfm = new SRFM.sym_region_frag_machine dt in
     let spfm = (srfm :> SPFM.sym_path_frag_machine) in
     let im = new IM.influence_manager spfm in
       srfm#set_influence_manager im;
       ((srfm :> FM.fragment_machine),
	(im :> Exec_no_influence.influence_manager)))
  in
  let dl = Asmir.decls_for_arch (Exec_options.asmir_arch ()) in
  let asmir_gamma = Asmir.gamma_create 
    (List.find (fun (i, s, t) -> s = "mem") dl) dl
  in
    fm#init_prog (dl, []);
    Exec_set_options.default_on_missing := (fun fm -> fm#on_missing_symbol);
    Exec_set_options.apply_cmdline_opts_early fm dl;
    Options_linux.apply_linux_cmdline_opts fm;
    Options_solver.apply_solver_cmdline_opts fm;
    State_loader.apply_state_loader_cmdline_opts fm;
    Exec_set_options.apply_cmdline_opts_late fm;
    let symbolic_init = Exec_set_options.make_symbolic_init fm infl_man in
    let (start_addr, fuzz_start) = Exec_set_options.decide_start_addrs () in
      Exec_fuzzloop.fuzz start_addr fuzz_start
	!Exec_options.opt_fuzz_end_addrs fm asmir_gamma symbolic_init
	(fun _ -> ());
      ()
;;

main Sys.argv;;

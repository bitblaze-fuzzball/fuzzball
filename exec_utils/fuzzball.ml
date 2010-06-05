(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module SRFM = Sym_region_frag_machine.SymRegionFragMachineFunctor
  (Symbolic_domain.SymbolicDomain)
module SRFMT = Sym_region_frag_machine.SymRegionFragMachineFunctor
  (Tagged_domain.TaggedDomainFunctor(Symbolic_domain.SymbolicDomain))

let main argv = 
  Arg.parse
    (Arg.align Exec_set_options.cmdline_opts)
    (* (fun arg -> prog := Vine_parser.parse_file arg) *)
    (fun arg -> Exec_set_options.set_program_name arg)
    "fuzzball [options]* program\n";
  let fm = if !Exec_options.opt_use_tags then
    new SRFMT.sym_region_frag_machine
  else
    new SRFM.sym_region_frag_machine
  in
  let dl = Asmir.decls_for_arch Asmir.arch_i386 in
  let asmir_gamma = Asmir.gamma_create 
    (List.find (fun (i, s, t) -> s = "mem") dl) dl
  in
    fm#init_prog (dl, []);
    Exec_set_options.apply_cmdline_opts fm dl;
    let symbolic_init = Exec_set_options.make_symbolic_init fm in
    (* replay_trace fm asmir_gamma s *)
    let (start_addr, fuzz_start) = Exec_set_options.decide_start_addrs () in
      Exec_fuzzloop.fuzz start_addr fuzz_start
	!Exec_options.opt_fuzz_end_addrs fm asmir_gamma symbolic_init
;;

main Sys.argv;;

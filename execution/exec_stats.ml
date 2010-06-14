(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

let check_memory_size () =
  let chan = open_in "/proc/self/status" in
    for i = 1 to 11 do ignore(input_line chan) done;
    let line = input_line chan in 
      close_in chan;
      assert((String.sub line 0 7) = "VmSize:");
      String.sub line 7 ((String.length line) - 7)

let check_memory_usage (fm:Fragment_machine.fragment_machine) trans_cache =
  let tc_size =
    Hashtbl.fold
      (fun k (dl, sl) s -> s + (Frag_simplify.stmt_size (V.Block(dl,sl))))
      trans_cache 0 in
  let (mem_ents, mem_nodes, mem_conc) = fm#measure_mem_size in
  let (form_ents, form_nodes) = fm#measure_form_man_size in
  let (reg_nodes, temps_nodes) = fm#measure_size in
    Printf.printf "Translation cache has %d entries, %d nodes\n"
      (Hashtbl.length trans_cache) tc_size;
    Printf.printf "Memory has %d entries, %d nodes, %d concrete bytes\n"
      mem_ents mem_nodes mem_conc;
    Printf.printf
      "Frag. machine using %d nodes in registers, %d nodes in temps\n"
      reg_nodes temps_nodes;
    Printf.printf "Formula manager has %d entries, %d nodes\n"
      form_ents form_nodes;
    Printf.printf "Total counted size is %d nodes\n"
      (mem_nodes + reg_nodes + temps_nodes + tc_size + form_nodes);
    Printf.printf "/proc size is %s\n" (check_memory_size ());
    flush stdout;
    Gc.print_stat stdout

(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;
open Sym_path_frag_machine;;

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
  let dt_nodes = fm#measure_dt_size in
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
    Printf.printf "Decision tree (on disk) has %d nodes\n" dt_nodes;
    Printf.printf "Total counted size is %d nodes\n"
      (mem_nodes + reg_nodes + temps_nodes + tc_size + form_nodes);
    Printf.printf "/proc size is %s\n" (check_memory_size ());
    flush stdout;
    Gc.print_stat stdout

let final_check_memory_usage () =
  Gc.full_major ();
  Gc.compact ();
  Printf.printf "After final collection:\n";
  Printf.printf "/proc size is %s\n" (check_memory_size ());
  flush stdout;
  Gc.print_stat stdout

let last_dt_print_time = ref 0.0

let print_tree fm =
  let now = Unix.gettimeofday () in
  let interval = match !opt_save_decision_tree_interval with
    | Some i -> i | None -> failwith "missing interval in print_tree" in
    if now -. !last_dt_print_time > interval then
      let chan = open_out "fuzz.tree" in
	fm#print_tree chan;
	close_out chan;
	last_dt_print_time := Unix.gettimeofday ()

let periodic_stats fm at_end force = 
  if !opt_save_decision_tree_interval <> None || force then
    print_tree fm;
  if !opt_gc_stats || force then
    check_memory_usage fm Exec_run_common.trans_cache;
  if !opt_gc_stats || force then
    Gc.print_stat stdout;
  if (!opt_solver_stats && at_end) || force then
    (Printf.printf "Solver returned satisfiable %Ld time(s)\n" !solver_sats;
     Printf.printf "Solver returned unsatisfiable %Ld time(s)\n"
       !solver_unsats;
     Printf.printf "Solver failed %Ld time(s)\n" !solver_fails)

let add_periodic_hook fm period =
  let insn_count = ref 0L in
  let hook fm eip =
    insn_count := Int64.succ !insn_count;
    if Int64.rem !insn_count period = 0L then
      (Printf.printf "%Ld instructions executed\r" !insn_count;
       periodic_stats fm false false;
       flush stdout)
  in
    fm#add_extra_eip_hook hook


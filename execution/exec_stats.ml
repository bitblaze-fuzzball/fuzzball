(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;
open Sym_path_frag_machine;;

module V = Vine;;

let check_memory_size () =
  let chan = open_in "/proc/self/status" in
  let try_read () =
    try Some (input_line chan)
    with End_of_file -> None in
  let rec get_mem () = match try_read () with
    | Some s ->
      if (String.sub s 0 7) = "VmSize:"
      then String.sub s 7 ((String.length s) - 7)
      else get_mem ()
    | None -> failwith "Couldn't find memory consumption in /proc/self/status!" in
  get_mem ()


let check_memory_usage (fm:Fragment_machine.fragment_machine) trans_cache =
  let tc_size =
    Hashtbl.fold
      (fun k (dl, sl) s -> s + (Frag_simplify.stmt_size (V.Block(dl,sl))))
      trans_cache 0 in
  let (mem_ents, mem_nodes, mem_conc) = fm#measure_mem_size in
  let (form_ents, form_nodes) = fm#measure_form_man_size in
  let dt_nodes = fm#measure_dt_size in
  let (reg_nodes, temps_nodes) = fm#measure_size in
    Printf.eprintf "Translation cache has %d entries, %d nodes\n"
      (Hashtbl.length trans_cache) tc_size;
    Printf.eprintf "Memory has %d entries, %d nodes, %d concrete bytes\n"
      mem_ents mem_nodes mem_conc;
    Printf.eprintf
      "Frag. machine using %d nodes in registers, %d nodes in temps\n"
      reg_nodes temps_nodes;
    Printf.eprintf "Formula manager has %d entries, %d nodes\n"
      form_ents form_nodes;
    Printf.eprintf "Decision tree (on disk) has %d nodes\n" dt_nodes;
    Printf.eprintf "Total counted size is %d nodes\n"
      (mem_nodes + reg_nodes + temps_nodes + tc_size + form_nodes);
    Printf.eprintf "/proc size is %s\n" (check_memory_size ());
    flush stdout;
    Gc.print_stat stdout

let final_check_memory_usage () =
  Gc.full_major ();
  Gc.compact ();
  Printf.eprintf "After final collection:\n";
  Printf.eprintf "/proc size is %s\n" (check_memory_size ());
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
    (Printf.eprintf "Solver returned satisfiable %Ld time(s)\n" !solver_sats;
     Printf.eprintf "Solver returned unsatisfiable %Ld time(s)\n"
       !solver_unsats;
     Printf.eprintf "Solver timed out (treated as unsat) %Ld time(s)\n"
       !solver_fake_unsats;
     Printf.eprintf "Solver failed %Ld time(s)\n" !solver_fails)

let add_periodic_hook fm period =
  let insn_count = ref 0L in
  let hook fm eip =
    insn_count := Int64.succ !insn_count;
    if Int64.rem !insn_count period = 0L then
      (Printf.eprintf "%Ld instructions executed\r" !insn_count;
       periodic_stats fm false false;
       flush stdout)
  in
    fm#add_extra_eip_hook hook


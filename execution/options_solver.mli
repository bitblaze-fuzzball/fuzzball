(*
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val solver_cmdline_opts : (string * Arg.spec * string) list

val solvers_table : 
  (string, (string -> Query_engine.query_engine option)) Hashtbl.t

val construct_solver : string -> Query_engine.query_engine

val apply_solver_cmdline_opts : Fragment_machine.fragment_machine -> unit

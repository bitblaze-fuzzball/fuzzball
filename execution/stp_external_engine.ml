(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;
open Query_engine;;
open Stpvc_engine;;

let map_lines f chan =
  let results = ref [] in
    (try while true do
       match (f (input_line chan)) with
	 | Some x -> results := x :: !results
	 | None -> ()
     done
     with End_of_file -> ());
    List.rev !results

let parse_counterex line =
  if line = "Invalid." then
    None
  else
    (assert((String.sub line 0 8) = "ASSERT( ");
     assert((String.sub line ((String.length line) - 4) 4) = "  );");
     let trimmed = String.sub line 8 ((String.length line) - 12) in
     let eq_loc = String.index trimmed '=' in
     let lhs = String.sub trimmed 0 eq_loc and
	 rhs = (String.sub trimmed (eq_loc + 1)
		  ((String.length trimmed) - eq_loc - 1)) in
       assert((String.sub lhs ((String.length lhs) - 2) 2) = "  ");
       let varname = String.sub lhs 0 ((String.length lhs) - 2) in
       let value =
	 let len = String.length rhs in
	   (Int64.of_string
	      (if len >= 6 && (String.sub rhs 0 5) = " 0hex" then
		 ("0x" ^ (String.sub rhs 5 (len - 5)))
	       else if len >= 4 && (String.sub rhs 0 3) = " 0x" then
		 ("0x" ^ (String.sub rhs 3 (len - 3)))
	       else if len >= 4 && (String.sub rhs 0 3) = " 0b" then
		 ("0b" ^ (String.sub rhs 3 (len - 3)))
	       else
		 failwith "Failed to parse value in counterexample"))
       in
	 Some (varname, value))

class stp_external_engine fname = object(self)
  inherit query_engine

  val mutable chan = None
  val mutable visitor = None
  val mutable filenum = 0
  val mutable curr_fname = fname

  method private get_fresh_fname = 
    filenum <- filenum + 1;
    curr_fname <- (Printf.sprintf "%s-%d" fname filenum);
    if !opt_trace_solver then
      Printf.printf "Creating STP file: %s.stp\n" curr_fname;
    curr_fname

  method private chan =
    match chan with
      | Some c -> c
      | None -> failwith "Missing output channel in stp_external_engine"

  method private visitor =
    match visitor with
      | Some v -> v
      | None -> failwith "Missing visitor in stp_external_engine"

  method prepare free_vars temp_vars =
    let fname = self#get_fresh_fname in
      chan <- Some(open_out (fname ^ ".stp"));
      visitor <- Some(new Stp.vine_cvcl_print_visitor
			(output_string self#chan));
      List.iter self#visitor#declare_var free_vars

  method assert_eq var rhs =
    try
      self#visitor#declare_var_value var rhs
    with
      | V.TypeError(err) ->
	  Printf.printf "Typecheck failure on %s: %s\n"
	    (V.exp_to_string rhs) err;
	  failwith "Typecheck failure in assert_eq"

  method query e =
    output_string self#chan "QUERY(NOT ";
    ignore(V.exp_accept (self#visitor :> V.vine_visitor) e);
    output_string self#chan ");\n";
    output_string self#chan "COUNTEREXAMPLE;\n";
    close_out self#chan;
    chan <- None;
    let timeout_opt = match !opt_solver_timeout with
      | Some s -> "-g " ^ (string_of_int s) ^ " "
      | None -> ""
    in
    let cmd = !opt_stp_path ^ " " ^ timeout_opt ^ curr_fname 
      ^ ".stp >" ^ curr_fname ^ ".stp.out" in
      if !opt_trace_solver then
	Printf.printf "Solver command: %s\n" cmd;
      flush stdout;
      let rcode = Sys.command cmd in
      let results = open_in (curr_fname ^ ".stp.out") in
	if rcode <> 0 then
	  (Printf.printf "STP died with result code %d\n" rcode;
	   (match rcode with 
	      | 131 -> raise (Signal "QUIT")
	      | _ -> ());
	   ignore(Sys.command ("cat " ^ curr_fname ^ ".stp.out"));
	   (None, []))
	else
	  let result_s = input_line results in
	  let first_assert = (String.sub result_s 0 6) = "ASSERT" in
	  let result = match result_s with
	    | "Valid." -> Some true
	    | "Timed Out." -> Printf.printf "STP timeout\n"; None
	    | "Invalid." -> Some false
	    | _ when first_assert -> Some false
	    | _ -> failwith "Unexpected first output line"
	  in
	  let first_assign = if first_assert then
	    [(match parse_counterex result_s with
		| Some ce -> ce | None -> failwith "Unexpected parse failure")]
	  else
	    [] in
	  let ce = map_lines parse_counterex results in
	    close_in results;
	    (result, first_assign @ ce)

  method unprepare save_results =
    if save_results then
      Printf.printf "STP query and results are in %s.stp and %s.stp.out\n"
	curr_fname curr_fname
    else if not !opt_save_solver_files then
      (Sys.remove (curr_fname ^ ".stp");
       Sys.remove (curr_fname ^ ".stp.out"));
    visitor <- None
end

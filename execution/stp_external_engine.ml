(*
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
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
     assert((String.sub line ((String.length line) - 3) 3) = " );");
     let trimmed = String.sub line 8 ((String.length line) - 11) in
     let eq_loc = String.index trimmed '=' in
     let lhs = String.sub trimmed 0 eq_loc and
	 rhs = (String.sub trimmed (eq_loc + 1)
		  ((String.length trimmed) - eq_loc - 1)) in
       assert((String.sub lhs ((String.length lhs) - 1) 1) = " "
	   || (String.sub lhs ((String.length lhs) - 1) 1) = "<");
       let lhs_rtrim =
	 if (String.sub lhs ((String.length lhs) - 2) 1) = " " then
	   2 else 1
       in
       let rhs_rtrim =
	 if (String.sub rhs ((String.length rhs) - 1) 1) = " " then
	   1 else 0
       in
       let varname = String.sub lhs 0 ((String.length lhs) - lhs_rtrim) in
       let value =
	 let rhs' = String.sub rhs 0 ((String.length rhs) - rhs_rtrim) in
	 let len = String.length rhs' in
	   (Int64.of_string
	      (if len >= 6 && (String.sub rhs' 0 5) = " 0hex" then
		 ("0x" ^ (String.sub rhs' 5 (len - 5)))
	       else if len >= 4 && (String.sub rhs' 0 3) = " 0x" then
		 ("0x" ^ (String.sub rhs' 3 (len - 3)))
	       else if len >= 4 && (String.sub rhs' 0 3) = " 0b" then
		 ("0b" ^ (String.sub rhs' 3 (len - 3)))
	       else if rhs' = ">FALSE" then
		 "0"
	       else if rhs' = ">TRUE" then
		 "1"
	       else
		 failwith "Failed to parse value in counterexample"))
       in
	 Some (varname, value))

class stp_external_engine fname = object(self)
  inherit query_engine

  val mutable chan = None
  val mutable visitor = None
  val mutable temp_dir = None
  val mutable filenum = 0
  val mutable curr_fname = fname

  method private get_temp_dir =
    match temp_dir with
      | Some t -> t
      | None ->
	  let rec loop num =
	    let name = Printf.sprintf "fuzzball-tmp-%d" num in
	      if Sys.file_exists name then
		loop (num + 1)
	      else
		(Unix.mkdir name 0o777;
		 temp_dir <- Some name;
		 name)
	  in
	    loop 1
		  
  method private get_fresh_fname = 
    let split_limbs n m =
      let rec loop n =
	if n < m then
	  [n]
	else
	  (n mod m) :: loop (n / m)
      in
	loop n
    in
    let make_dirs parent limbs =
      let rec loop p l =
	match l with
	  | [] -> p
	  | n :: rest ->
	      let dir = p ^ "/" ^ Printf.sprintf "%03d" n in
		if not (Sys.file_exists dir) then
		  Unix.mkdir dir 0o777;
		loop dir rest
      in
	loop parent limbs
    in
    let dir = self#get_temp_dir in
      filenum <- filenum + 1;
      let (low, rest) = match split_limbs filenum 1000 with
	| (low :: rest) -> (low, rest)
	| _ -> failwith "Non-empty list invariant failure in get_fresh_fname"
      in
      let dir' = make_dirs dir (List.rev rest) in
        ignore(low);
	curr_fname <- (Printf.sprintf "%s/%s-%d" dir' fname filenum);
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

  val mutable free_vars = []
  val mutable eqns = []
  val mutable conds = []

  method start_query =
    ()

  method add_free_var var =
    free_vars <- var :: free_vars
 
  method private real_add_free_var var =
    self#visitor#declare_var var

  method add_temp_var var =
    ()

  method assert_eq var rhs =
    eqns <- (var, rhs) :: eqns;

  method add_condition e =
    conds <- e :: conds

  val mutable ctx_stack = []

  method push =
    ctx_stack <- (free_vars, eqns, conds) :: ctx_stack

  method pop =
    match ctx_stack with
      | (free_vars', eqns', conds') :: rest ->
	  free_vars <- free_vars';
	  eqns <- eqns';
	  conds <- conds';
	  ctx_stack <- rest
      | [] -> failwith "Context underflow in stp_external_engine#pop"

  method private real_assert_eq (var, rhs) =
    try
      self#visitor#declare_var_value var rhs
    with
      | V.TypeError(err) ->
	  Printf.printf "Typecheck failure on %s: %s\n"
	    (V.exp_to_string rhs) err;
	  failwith "Typecheck failure in assert_eq"

  method private real_prepare =
    let fname = self#get_fresh_fname in
      chan <- Some(open_out (fname ^ ".stp"));
      visitor <- Some(new Stp.vine_cvcl_print_visitor
			(output_string self#chan));
      List.iter self#real_add_free_var (List.rev free_vars);
      List.iter self#real_assert_eq (List.rev eqns);

  method query qe =
    self#real_prepare;
    output_string self#chan "QUERY(NOT (";
    let conj = List.fold_left
      (fun es e -> V.BinOp(V.BITAND, e, es)) qe (List.rev conds)
    in
      (let visitor = (self#visitor :> V.vine_visitor) in
       let rec loop = function
	 | V.BinOp(V.BITAND, e1, e2) ->
	     loop e1;
	     output_string self#chan "\n  AND ";
	     loop e2
	 | e ->
	     ignore(V.exp_accept visitor e)
       in
	 loop conj);
    output_string self#chan "));\n";
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
	      | 127 ->
		  if !opt_stp_path = "stp" then
		    Printf.printf
		      "Perhaps you should set the -stp-path option?\n"
		  else if String.contains !opt_stp_path '/' &&
		    not (Sys.file_exists !opt_stp_path)
		  then
		    Printf.printf "The file %s does not appear to exist\n"
		      !opt_stp_path
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

  method after_query save_results =
    if save_results then
      Printf.printf "STP query and results are in %s.stp and %s.stp.out\n"
	curr_fname curr_fname
    else if not !opt_save_solver_files then
      (Sys.remove (curr_fname ^ ".stp");
       Sys.remove (curr_fname ^ ".stp.out"))

  method reset =
    visitor <- None;
    free_vars <- [];
    eqns <- [];
    conds <- []
end

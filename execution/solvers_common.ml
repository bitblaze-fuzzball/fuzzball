(*
  Based on stp_external_engine.ml, which bears the following notice:
  Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

type external_solver_type =
  | STP_CVC
  | STP_SMTLIB2
  | CVC4
  | BOOLECTOR
  | Z3

let map_lines f chan =
  let results = ref [] in
    (try while true do
       match (f (input_line chan)) with
	 | Some x -> results := x :: !results
	 | None -> ()
     done
     with End_of_file -> ());
    List.rev !results

let smtlib_rename_var name =
  let new_name = ref "" in
    for i = 0 to (String.length name) - 1 do
      match name.[i] with
        | '_' -> new_name := !new_name ^ "-"
        | '-' -> new_name := !new_name ^ "_"
	| '|' -> ()
        | _ -> new_name := !new_name ^ (Char.escaped name.[i])
    done;
    !new_name

type maybe_ce_result =
  | No_CE_here
  | End_of_CE
  | Assignment of string * int64

let parse_stp_ce e_s_t line =
  if line = "sat" then
    (assert(e_s_t = STP_SMTLIB2);
     End_of_CE)
  else if line = "Invalid." then
    (assert(e_s_t = STP_CVC);
     End_of_CE)
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
       let varname_raw = String.sub lhs 0 ((String.length lhs) - lhs_rtrim) in
       let varname = match e_s_t with
	 | STP_SMTLIB2 -> smtlib_rename_var varname_raw
	 | _ -> varname_raw
       in
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
	 Assignment (varname, value))

let parse_cvc4_ce line =
  if line = ")" then
    End_of_CE
  else if line = "(model" then
    No_CE_here
  else
    (assert((String.sub line 0 12) = "(define-fun ");
     assert((String.sub line ((String.length line) - 1) 1) = ")");
     let trimmed1 = String.sub line 12 ((String.length line) - 13) in
     let var_end = String.index trimmed1 ' ' in
     let varname = String.sub trimmed1 0 var_end in
     let trimmed2 = String.sub trimmed1 (var_end + 4)
       ((String.length trimmed1) - (var_end + 4))
     in
       assert(((String.sub trimmed2 0 4) = "Bool") ||
		 ((String.sub trimmed2 0 10) = "(_ BitVec "));
       let trimmed3 =
	 if (String.sub trimmed2 0 4) = "Bool" then
	   String.sub trimmed2 5 ((String.length trimmed2) - 5)
	 else
	   let type_end = String.index trimmed2 ')' in
	     String.sub trimmed2 (type_end + 2)
	       ((String.length trimmed2) - (type_end + 2))
       in
         assert(((String.sub trimmed3 0 4) = "true") ||
		   ((String.sub trimmed3 0 5) = "false") ||
		   ((String.sub trimmed3 0 5) = "(_ bv"));
	 let value = Int64.of_string
	   (if (String.sub trimmed3 0 4) = "true" then
	      "1"
	    else if (String.sub trimmed3 0 5) = "false" then
	      "0"
	    else
              let trimmed4 = String.sub trimmed3 5
		((String.length trimmed3) - 5)
	      in
	      let val_end = String.index trimmed4 ' ' in
	        String.sub trimmed4 0 val_end)
	 in
	   Assignment ((smtlib_rename_var varname), value))

let parse_btor_ce line =
  let sp = String.index line ' ' in
  let varname_raw = String.sub line 0 sp and
      bits = String.sub line (sp + 1) ((String.length line) - sp - 1)
  in
    Assignment ((smtlib_rename_var varname_raw),
		(Int64.of_string ("0b" ^ bits)))

(* Z3's models are similar to CVC4's as S-expressions, but they're
   split over lines differently, so we can't use a simple one-ce-per-line
   parsing strategy. In particular we need to remember a variable name
   from a previous line before we see the value in a later one. The
   parameter "v" is used for that. *)
let parse_z3_ce_line s v =
  match (s, v) with
    | ("(model ", None) -> (No_CE_here, None)
    | (")", None) -> (End_of_CE, None)
    | (s, None) when String.length s > 14
	&& String.sub s 0 14 = "  (define-fun " ->
	let trim1 = String.sub s 14 ((String.length s) - 14) in
	let var_end = String.index trim1 ' ' in
	let varname = String.sub trim1 0 var_end in
	  (No_CE_here, Some (smtlib_rename_var varname))
    | (s, None) when String.length s > 7
	&& String.sub s 0 7 = "(error "
	->
	let l = String.length s in
	  if String.sub s (l - 24) 24 = "model is not available\")" then
	    (No_CE_here, None)
	  else
	    (Printf.printf "Z3 error: %s\n" s;
	     failwith "Unexpected error in parse_z3_ce_lines")
    | (s, Some varname) ->
	let l = String.length s in
	  assert(l > 4 && String.sub s 0 4 = "    ");
	  assert(String.sub s (l - 1) 1 = ")");
	  let trim = String.sub s 4 (l - 5) in
	  let v =
	    match trim with
	      | "false" -> 0L
	      | "true" -> 1L
	      | "#b0" -> 0L
	      | "#b1" -> 1L
	      | t when String.length t > 2 && String.sub t 0 2 = "#x" ->
		  let l2 = (String.length t) in
		    Int64.of_string ("0x" ^ (String.sub t 2 (l2 - 2)))
	      | _ ->
		  Printf.printf "Value parse failure on <%s>\n" trim;
		  failwith "Unhandled value case in parse_z3_ce_lines"
	  in
	    (Assignment (varname, v), None)
    | (s, _) ->
	  Printf.printf "Parse failure on <%s>\n" s;
	  failwith "Unhandled loop case in parse_z3_ce_line"

let parse_ce e_s_t =
  match e_s_t with
    | (STP_CVC|STP_SMTLIB2) -> parse_stp_ce e_s_t
    | CVC4 -> parse_cvc4_ce
    | Z3 -> failwith "Z3 unsupported in parse_ce"
    | BOOLECTOR -> parse_btor_ce

let create_temp_dir prefix =
  let rec loop num =
    let name = Printf.sprintf "%s-%d" prefix num in
      if Sys.file_exists name then
	loop (num + 1)
      else
	(Unix.mkdir name 0o777;
	 name)
  in
    loop 1

let pick_fresh_fname dir fname filenum =
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
    let (low, rest) = match split_limbs filenum 1000 with
      | (low :: rest) -> (low, rest)
      | _ -> failwith "Non-empty list invariant failure in pick_fresh_fname"
    in
    let dir' = make_dirs dir (List.rev rest) in
      ignore(low);
      Printf.sprintf "%s/%s-%d" dir' fname filenum

module V = Vine
module VOpt = Vine_opt

open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

type pointer_statements = {
  eip : int64;
  test : V.exp option;
  mutable vine_stmts : V.stmt list;
  mutable vine_decls : V.decl list;
}
  
type breakloop = {
  source_eip : int64;
  dest_eip : int64;
}

type veritesting_data = 
| BreakLoop of breakloop         (* loops in program *)
| InternalLoop of int64          (* loops in vine's decoding of an instruction *)
| Return of int64
| Halt of int64
| FunCall of int64
| SysCall of int64
| Special of int64
| SearchLimit of int64
| Instruction of pointer_statements


type node = {
  data : veritesting_data;
  mutable parent : node list;
  mutable children : node list;
}

type vineLabel =
| X86eip of int64
| Internal of int

exception UnexpectedEIP

let make_exit parent =
  match parent.data with
  | Instruction pdata ->
    let child = {data = SearchLimit (Int64.add Int64.one pdata.eip);
		 parent = [parent];
		 children = [];} in
    parent.children <- child::parent.children;
    Some child
  | _ -> None


let veritesting_data_to_string = function
  | BreakLoop at  -> (Printf.sprintf "BreakLoop From 0x%LX To 0x%LX"
			at.source_eip at.dest_eip)
  | InternalLoop at -> Printf.sprintf "Instruction @ 0x%LX has self loop" at
  | SysCall at -> Printf.sprintf "SysCall @ 0x%LX" at
  | Special at -> Printf.sprintf "Special Call @ 0x%LX" at
  | Return at -> Printf.sprintf "Return @ 0x%LX" at
  | Halt at ->  Printf.sprintf "Halt @ 0x%LX" at
  | FunCall at ->  Printf.sprintf "Fun. Call @ 0x%LX" at
  | SearchLimit at ->  Printf.sprintf "Search Limit @ 0x%LX" at
  | Instruction i -> (Printf.sprintf "@ 0x%LX" i.eip)


let node_to_string (node : node) =
  veritesting_data_to_string node.data


let decode_eip str =
  if (str.[0] = 'L' && str.[1] = '_')
  then (let num = String.sub str 2 ((String.length str) - 2) in
	Internal (int_of_string num))
  else X86eip (label_to_eip str)


let walk_to_label target_label tail =
  let rec walk_to_label_help target_label = function
    | [] -> None
    | V.ExpStmt(V.Name(label))::tail ->
      begin
        match decode_eip label with
        | X86eip(_) -> raise UnexpectedEIP
        | Internal(ilabel) ->
          if (target_label = ilabel)
	  then Some tail
          else walk_to_label_help target_label tail
      end
    | _::tail ->
      walk_to_label_help target_label tail in 
  match (decode_eip target_label) with
  | X86eip(_) -> raise UnexpectedEIP
  | Internal(ilabel) -> walk_to_label_help ilabel tail


let equal (a : veritesting_data) (b : veritesting_data) =
  let rec check_vine_stmts = function
    | [], [] -> true
    | _::_, []
    | [], _::_ -> false
    | hd1::tl1, hd2::tl2 ->
      hd1 = hd2 && check_vine_stmts (tl1,tl2) in
  match (a,b) with
  | BreakLoop i1, BreakLoop i2 -> ((i1.source_eip = i2.source_eip)
				   && (i1.dest_eip = i2.dest_eip))
  | InternalLoop i1 , InternalLoop i2
  | Return i1, Return i2
  | Halt i1, Halt i2
  | SysCall i1, SysCall i2
  | Special i1, Special i2
  | SearchLimit i1, SearchLimit i2
  | FunCall i1, FunCall i2 -> i1 = i2
  | Instruction i1, Instruction i2 ->
    (i1.eip = i2.eip) && check_vine_stmts (i1.vine_stmts, i2.vine_stmts)
  | _, _ -> false


let ordered_p (a : veritesting_data) (b : veritesting_data) =
  match a,b with
  | Instruction i1, Instruction i2 -> i1.eip < i2.eip
  | _, Instruction _ ->  false
  | Instruction _, _ -> true
  | _ , _ -> true

let ordered_p (a : node) (b : node) =  ordered_p a.data b.data
    

let key (a : node) = a.data

let check_cycle (lookfor : node) (root : node) =
  let me = key lookfor in 
  let check_this_gen found_cycle child =
    if not found_cycle
    then (equal me (key child))
    else found_cycle in
  let rec check_cycle_int next =
    (List.fold_left check_this_gen false next.children)
  || List.fold_left
      (fun accum child ->
	if accum
	then accum
	else check_cycle_int child) false next.parent in
  check_cycle_int root

(**
   Case 1: No Labels, No Jumps.  
           Just a straight away leading to a single node and no children

   Case 2: Something I can't encode as part of the statement list.
           Just the entering eip to the non-decodable region is what we have to capture.

   Case 3: x86 LABEL -- STATEMENTS BEFORE LABEL :: REST 
                        create the current node based on statements before label and handed in eip.
                        recur, creating the single child node starting at eip LABEL

   Case 4: JMP to x86  -- STATEMENTS INCLUDING JMP :: REST
                          discard rest -- you can't reach it. Statements up to and including jmp become this node.
                          singleton child starting at jmp label.

   Case 5: JMP to internal -- try to walk to the label in the current statement list. 
                              If it doesn't exist, there's a cycle, abort.
                              Otherwise, keep going beyond this case to another
                              terminating case.

   Case 6: CJMP to x86, x86 -- two external jumps
   Case 7: CJMP to int, int -- two internal jumps
   CASE 8: CJMP to x86, int -- a mixed jump
**)
    
let expand fm gamma (node : veritesting_data) =
  match node with
  | InternalLoop _
  | BreakLoop _
  | SysCall _
  | Return _
  | Halt _
  | Special _
  | FunCall _
  | SearchLimit _ -> [] (* these are non child-bearing nodes *)
  | Instruction i1 ->
    (* the first member is the declaration list *)
    let decls, statement_list = decode_insn_at fm gamma i1.eip in
  (* right now the legality check is going to catch self loops inside
     of the statement list, so I'm not looking for them here.
     Instead, we're just collecting exit points for the region
     and statement lists executed up until that point. *)
    let rec walk_statement_list current_eip stmts = function
      | [] ->
	(match current_eip with
	| Some eip -> [ Instruction {eip = eip;
				     test = None;
				     vine_stmts = stmts;
				     vine_decls = decls}]
	| None -> failwith "Don't know what eip to associate with an instruction.")
      | stmt::tl ->
	begin
	  match stmt with
          | V.Jmp(V.Name(label)) ->
	    begin
	      match decode_eip label with
	    (* this captures the wrong thing entirely.  The
	       statements preceeding a jump are not the statements
	       associated with the jump, so setting the eip value
	       for the statement list at the jump is insane. *)
	    | X86eip value -> [Instruction {eip = value;
					    test = None;
					    vine_stmts = stmts;
					    vine_decls = decls;}]
	    | Internal value -> 
	      (* if we walk off, it should be because the label is
		 behind us iff labels are only valid local to a vine
		 decoding *)
	      begin
		match walk_to_label label tl with
		| None -> [InternalLoop i1.eip]
		| Some next -> walk_statement_list current_eip (stmt::stmts) next
	      end
	    end
          | V.CJmp(test, V.Name(true_lab), V.Name(false_lab)) ->
	    begin
	      (** These internal jumps are wrong because we aren't capturing the path conditions that
		  they're a part of. JTT 9-8-2014 *)
	      match (decode_eip true_lab), (decode_eip false_lab) with
	      | X86eip tval, X86eip fval ->
		[ Instruction {eip = tval;
			       test = Some test;
			       vine_stmts = V.ExpStmt test :: stmts;
			       vine_decls = decls;};
		  Instruction {eip = fval;
			       test = Some (V.UnOp (V.NOT, test));
			       vine_stmts = stmts;
			       vine_decls = decls;}]
	      | X86eip tval, Internal fval ->
		(Instruction {eip = tval;
			      test = Some test;
			      vine_stmts = V.ExpStmt test :: stmts;
			      vine_decls = decls}) ::
		  (match walk_to_label false_lab tl with
		  | None -> [InternalLoop i1.eip]
		  | Some next -> walk_statement_list current_eip (stmt::stmts) next)
	      | Internal tval, X86eip fval ->
		(Instruction {eip = fval;
			      test = Some (V.UnOp (V.NOT, test));
			      vine_stmts = V.ExpStmt (V.UnOp (V.NOT, test)) :: stmts;
			      vine_decls = decls }) ::
		  (match walk_to_label true_lab tl with
		  | None -> [InternalLoop i1.eip]
		  | Some next -> walk_statement_list current_eip (stmt::stmts) next)
	      | Internal tval, Internal fval ->
		begin
		  let next = walk_statement_list current_eip (stmt::stmts) in
		  match (walk_to_label true_lab tl), (walk_to_label false_lab tl) with
		  | None, None -> [InternalLoop i1.eip; InternalLoop i1.eip;]
		  | Some rem, None
		  | None, Some rem -> InternalLoop i1.eip :: (next rem)
		  | Some r1, Some r2 -> List.rev_append (next r1) (next r2)
		end
	    end
	      (* Why One?  -*)
	  | V.Label(l)
	    -> (match (decode_eip l) with
	    | X86eip eip -> (assert (None = current_eip); (* this label shows which EIP controls this block *)
			     walk_statement_list (Some eip) (stmt::stmts) tl)
	    | Internal name -> walk_statement_list current_eip (stmt::stmts) tl)
	    
	  | V.Special("int 0x80") -> [SysCall i1.eip;]
	  | V.Special _ -> [Special i1.eip]
	  | V.Return _ -> [Return i1.eip]
	  | V.Call _ -> [FunCall i1.eip]
	  | V.Halt _  -> [Halt i1.eip]
	  | _ -> walk_statement_list current_eip (stmt::stmts) tl
	end in
      walk_statement_list None [] statement_list


let rec print_region ?offset:(offset = 1) node =
  (for i=1 to offset do Printf.printf "\t" done);
  Printf.printf "%s\n" (node_to_string node);
  List.iter (print_region ~offset:(offset + 1)) node.children

let make_root fm gamma eip =
  let decode_call _ = decode_insns fm gamma eip 1 in
  let decls, stmts = with_trans_cache eip decode_call in
  Instruction { eip = eip;
		test = None;
		vine_stmts = stmts;
		vine_decls = decls; }

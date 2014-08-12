
module V = Vine

open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

type veritesting_region =
| Statement of V.stmt
| Exit of V.stmt option (* None for the bottoming out case *)
| Branch of (V.exp * veritesting_region list) list

type veritesting_data = {
  entry_point : int64; (* eip *)
  vine_stmts : V.stmt list; (* What did I do in this node *)
}

type 'a node = {
  data : 'a;
  parent : 'a node;
  mutable children : 'a node list;
}

type vineLabel =
| X86eip of int64
| Internal of int

exception VineLoop
exception UnexpectedEIP
exception SysCall
exception Return
exception FunCall
exception Halt

let decode_eip str =
  if (str.[0] = 'L' && str.[1] = '_')
  then (let num = String.sub str 2 ((String.length str) - 2) in
 Internal (int_of_string num))
  else X86eip (label_to_eip str)

let check_label label seen_labels = 
  match decode_eip label with 
  | X86eip(_) -> true
  | Internal(ilabel) -> 
    if (List.mem ilabel seen_labels)
    then false
    else true


let walk_to_label target_label tail =
  let rec walk_to_label_help target_label = function
    | [] -> raise VineLoop
    | V.ExpStmt(V.Name(label))::tail ->
      begin
        match decode_eip label with
        | X86eip(_) -> raise UnexpectedEIP
        | Internal(ilabel) ->
          if (target_label = ilabel)
	  then tail
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
  (a.entry_point = b.entry_point)
  && check_vine_stmts (a.vine_stmts, b.vine_stmts)

let key a = a

let expand fm gamma (node : veritesting_data) =
  let _, statement_list = decode_insn_at fm gamma node.entry_point in
  (* right now the legality check is going to catch self loops inside
     of the statement list, so I'm not looking for them here.
     Instead, we're just collecting exit points for the region
     and statement lists executed up until that point. *)
  let rec walk_statement_list accum = function
    | [] -> [{ entry_point = Int64.add node.entry_point Int64.one;
	      (* next instruction *)
	       vine_stmts = accum }]
    | stmt::tl ->
      begin
	match stmt with
        | V.Jmp(V.Name(label)) ->
	  begin
	    match decode_eip label with
	    | X86eip value -> [{ entry_point = value;
				  vine_stmts = accum }]
	    | Internal value -> 
	      (* if we walk off, it should be because the label is behind us iff labels are only valid local to a vine decoding *)
	      walk_statement_list (stmt::accum) (walk_to_label label tl)
	  end
        | V.CJmp(test, V.Name(true_lab), V.Name(false_lab)) ->
	  begin
	    match (decode_eip true_lab), (decode_eip false_lab) with
	    | X86eip tval, X86eip fval ->
	      [{entry_point = tval; vine_stmts = stmt::accum};
	       {entry_point = fval; vine_stmts = stmt::accum}]
	    | X86eip tval, Internal fval ->
	      {entry_point = tval; vine_stmts = stmt::accum}::
		(walk_statement_list (stmt::accum) (walk_to_label false_lab tl))
	    | Internal tval, X86eip fval ->
	      {entry_point = fval; vine_stmts = stmt::accum}::
		(walk_statement_list (stmt::accum) (walk_to_label true_lab tl))
	    | Internal tval, Internal fval ->
	      let next = walk_statement_list (stmt::accum) in
	      (List.append
		 (next (walk_to_label true_lab tl))
		 (next (walk_to_label false_lab tl)))
	  end
	| V.Special("int 0x80") -> raise SysCall
	| V.Return _ -> raise Return
	| V.Call _ -> raise FunCall
	| V.Halt _  -> raise Halt
	| _ -> walk_statement_list (stmt::accum) tl
      end in
  try
    (* note that this isn't tail recursive because it's in a try block *)
    walk_statement_list [] statement_list
  with 
  | UnexpectedEIP -> raise UnexpectedEIP
  | _ -> [] (* non-veritesting region, no children *)


let breadth_first_search ?max_it:(max_it = max_int) root expansion key =
  let closed = Hashtbl.create 100
  and key node = key node.data in
  let add_child parent accum data =
    let child = 
      { data = data;
	parent = parent;
	children = []; } in
    if (Hashtbl.mem closed (key child))
    then accum
    else (Hashtbl.add closed (key child) child;
	  child::accum) in
  let expansion node =
    assert(node.children = []); (* not yet expanded *)
    node.children <-
      List.fold_left (add_child node) [] (expansion node.data);
    node.children in
  let rec loop it = function 
    | [] -> ()
    | head::tail ->
      if (it > max_it)
      then ()
      else loop (it + 1) (tail@(expansion head)) in
  let rec root_node =
    { data = root;
      parent = root_node;
      children = []; } in 
  Hashtbl.add closed (key root_node) root_node;
  loop 0 [root_node];
  root_node

let find_veritesting_region_m2 fm gamma starting_eip max_depth =
  let root_of_region = 
    breadth_first_search ~max_it:max_depth
      { entry_point = starting_eip;
	vine_stmts = [];}
      (expand fm gamma)
      key in
  let rec print_region ?offset:(offset = 1) node =
    (for i=1 to offset do Printf.printf "\t" done);
    Printf.printf "%Li\n" node.data.entry_point;
    List.iter (print_region ~offset:(offset + 1)) node.children in
  print_region root_of_region
      

let find_veritesting_region fm gamma starting_eip k =
  let rec find_veritesting_region_int eip remaining =
    let rec walk_sl = function
      | [] -> []
      | hd::tl ->
	Printf.printf "eip %LX: %s\n" eip (V.stmt_to_string hd);
	flush stdout;
	match hd with
	| V.Jmp(V.Name(lab)) when lab <> "pc_0x0" ->
	  (* hd is the last member of an old statement list,
	     lab defines the begining of a new statement list. *)
	  (Statement hd)::(find_veritesting_region_int (label_to_eip lab) (remaining - 1))
	| V.CJmp(test, V.Name(true_lab), V.Name(false_lab)) ->
	  let ntrue = V.Name true_lab
	  and nfalse = V.Name false_lab in
	  (Printf.printf "Conditional Jump -- True: %s False: %s \n"
	     (V.exp_to_string ntrue) (V.exp_to_string nfalse);
	   let remaining' = remaining - 1 in
	   let true_child = ntrue, find_veritesting_region_int (label_to_eip true_lab) remaining'
	   and false_child = nfalse, find_veritesting_region_int (label_to_eip false_lab) remaining' in

	   (Statement hd)::
	     [Branch 
		 [true_child;
		  false_child]])
	| V.Jmp _ as smt -> (Printf.printf "Exiting Veritesting Region -- JMP to pc_0x0\n";
			     [Exit (Some smt)])
	    
	| V.CJmp (_,_,_) as smt -> (Printf.printf "Exiting Veritesting region -- CJMP we can't handle.\n";
			    [Exit (Some smt)])
	| V.Special("int 0x80") as smt -> (Printf.printf "Exiting Veritesting region -- System Call\n";
				    [Exit (Some smt)])
	| V.Return _ as smt-> (Printf.printf "Exiting Veritesting Region -- Return\n";
		       [Exit (Some smt)])
	| V.Call _ as smt-> (Printf.printf "Exiting Veritesting Region -- Call\n";
		     [Exit (Some smt)])
	| V.Halt _ as smt-> (Printf.printf "Exiting Verittesting Region -- Halt\n";
		     [Exit (Some smt)])
	| _ -> (Statement hd)::(walk_sl tl)
    in
    if remaining = 0
    then [Exit None]
    else let (_,sl) = decode_insn_at fm gamma eip in
	 walk_sl sl in
  find_veritesting_region_int starting_eip k

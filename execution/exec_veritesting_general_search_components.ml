module V = Vine

open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

type 'a common = {
  eip : int64;
  parent : 'a;
}

type 'a minimal = {
  m_core : 'a common ;
  mutable next : 'a option;
}

type 'a progn = {
  p_core : 'a minimal;
  decls : V.decl list;
  stmts : V.stmt list;
}

type 'a branch = {
  b_core : 'a common;
  test : V.exp;
  mutable true_child : 'a;
  mutable false_child: 'a;
}

type 'a finished_type =
| ExternalLoop of int64   (* instruction jumps to ancestor eip *)
| InternalLoop of int64   (* instruction contains cycle in statement list *)
| Return of int64         (* statement list had a return, exit to the eip containing that statement *)
| Halt of int64           (* as above, with halt. *)
| FunCall of int64        (* etc. *)
| SysCall of int64
| Special of int64
| SearchLimit of int64    (* search went too deep, halt with the next instruction being this EIP *)
| Branch of 'a branch     (* encode the test and continuations *)
| Segment of 'a progn     (* completely interpreted segment *)

type key_type =
| HaltingEIP of int64
| UndecodedEIP of int64
| RawEIP of int64
| BranchingEIP of int64
| CompletedEIP of int64 

type veritesting_node =
| Undecoded of veritesting_node minimal
| Raw of veritesting_node progn
| Completed of veritesting_node finished_type


type vineLabel =
| X86eip of int64
| Internal of int

exception UnexpectedEIP

let key (ft : veritesting_node finished_type) =
  (** uniquely identify a node *)
  match ft with
  | ExternalLoop eip
  | InternalLoop eip
  | Return eip
  | Halt eip
  | FunCall eip
  | SysCall eip
  | Special eip
  | SearchLimit eip -> HaltingEIP eip
  | Branch b -> BranchingEIP (Int64.neg b.b_core.eip) (* don't want it to colide with prev. block *)
  | Segment progn -> CompletedEIP progn.p_core.m_core.eip


let key (node : veritesting_node) =
(** uniquely identify a node *)
  match node with
  | Undecoded m -> UndecodedEIP m.m_core.eip
  | Raw progn -> RawEIP progn.p_core.m_core.eip
  | Completed ft -> key ft

let eip_of_node (node : veritesting_node) =
  match (key node) with
  | UndecodedEIP eip
  | RawEIP eip
  | CompletedEIP eip
  | BranchingEIP eip
  | HaltingEIP eip -> eip


let equal (a : veritesting_node) (b : veritesting_node) =
  (** quick test to see if nodes are equivalent *)
  match a,b with
  | Undecoded a, Undecoded b -> a.m_core.eip = b.m_core.eip
  | Raw aprgn, Raw bprgn -> aprgn.p_core.m_core.eip = bprgn.p_core.m_core.eip
  | Completed _, Completed _ -> (key a) = (key b)
  | _, _ -> false


let finished_type_to_string = function
| ExternalLoop eip -> (Printf.sprintf "ExternalLoop @ 0x%Lx" eip)
| InternalLoop eip -> (Printf.sprintf "InternalLoop @ 0x%Lx" eip)
| Return eip -> (Printf.sprintf "Return @ 0x%Lx" eip)
| Halt eip -> (Printf.sprintf "Halt @ 0x%Lx" eip)
| FunCall eip -> (Printf.sprintf "FunCall @ 0x%Lx" eip)
| SysCall eip -> (Printf.sprintf "SysCall @ 0x%Lx" eip)
| Special eip -> (Printf.sprintf "Special @ 0x%Lx" eip)
| SearchLimit eip -> (Printf.sprintf "SearchLimit @ 0x%Lx" eip)
| Branch b -> (Printf.sprintf "Branch @ 0x%Lx" b.b_core.eip)
| Segment s -> (Printf.sprintf "Code Segment @ 0x%Lx" s.p_core.m_core.eip)


let node_to_string = function
  | Undecoded a -> (Printf.sprintf "Undecoded Region @ 0x%Lx" a.m_core.eip)
  | Raw progn -> (Printf.sprintf "Raw Region @ 0x%Lx" progn.p_core.m_core.eip)
  | Completed c -> finished_type_to_string c


let decode_eip str =
  (** helper function converting a string into the logically correct value *)
  if (str.[0] = 'L' && str.[1] = '_')
  then (let num = String.sub str 2 ((String.length str) - 2) in
	Internal (int_of_string num))
  else X86eip (label_to_eip str)


let ordered_p (a : veritesting_node finished_type) (b : veritesting_node finished_type) =
  match a,b with
  | _, _ -> true


let ordered_p (a : veritesting_node) (b : veritesting_node) =
  match a,b with
  | Undecoded aeip, Undecoded beip -> aeip < beip
  | Raw aprgn, Raw bprgn -> aprgn.p_core.m_core.eip < bprgn.p_core.m_core.eip
  | Completed ac, Completed bc -> ordered_p ac bc
  | _, _ -> true


let rec print_region_ft ?offset:(offset = 1) (node : veritesting_node finished_type) =
  match node with
  | Branch b ->
    begin
      print_region ~offset:(offset+1) b.true_child;
      print_region ~offset:(offset+1) b.false_child
    end
  | Segment progn -> (*  this isn't very promising *)
    (match progn.p_core.next with
    | Some child -> print_region ~offset:(offset+1) child
    | None -> ())
  | _ -> ()


and print_region ?offset:(offset = 1) (node : veritesting_node) =
  (for i=1 to offset do Printf.printf "\t" done);
  Printf.printf "%s\n" (node_to_string node);
  match node with
  | Undecoded _
  | Raw _ -> ()
  | Completed c -> print_region_ft ~offset c


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


let complete_node (raw_node : veritesting_node) (child : veritesting_node)
    (sl : V.stmt list) (dl: V.decl list) =
  assert(sl != []);
  (match raw_node with
  | Raw progn -> 
    begin
     (* stitch the completed child into the code segment. *)
      let closed_variant =
	Completed
	  (Segment {p_core = {m_core = {eip = (eip_of_node raw_node);
					parent = raw_node;};
			      next = Some child;};
		    stmts = (List.rev sl);
		    decls = dl;}) in
      progn.p_core.next <- Some closed_variant
    end
  | _ -> failwith "Should only be able to call complete node on a raw_node");
  child


let add_exit progn next =
  assert (None = progn.p_core.next);
  progn.p_core.next <- Some next;
  next


let rec truncate_node = function
  (** If you're doing a truncated expand of a veritesting node, this is
      the way you tell the search that the area beyond this point was
      intentionally not considered because it was too far afield from
      the root. *)
  | Undecoded m -> m.next <- Some (Completed (SearchLimit m.m_core.eip))
  | Raw progn -> progn.p_core.next <- Some (Completed (SearchLimit progn.p_core.m_core.eip))
  | Completed ft -> match ft with
    | Segment progn -> progn.p_core.next <- Some (Completed (SearchLimit progn.p_core.m_core.eip))
    | Branch b -> 
      begin
	truncate_node b.true_child;
	truncate_node b.false_child
      end
    | _ -> () (* everything else is a terminal anyway. You'd never expand it. *)


let children = function
  (** Standardized function for getting the successors of a veritesting
      node after it has been generated *)
  | Undecoded m ->
    (match m.next with
    |Some s -> [s]
    | _ -> [])
  | Raw progn ->
    (match progn.p_core.next with
    | Some s -> [s]
    | _ -> [])
  | Completed ft ->
    (match ft with
    | Segment progn ->
      (match progn.p_core.next with 
      | Some s -> [s]
      | _ -> [])
    | Branch b -> [b.true_child; b.false_child]
    | _ -> [])

let consider_statements (node : veritesting_node) = 
  let progn = match node with
    |Raw progn -> progn
    | _ -> failwith "only for raw nodes" in
  let rec wsl accum = function
    | [] -> Completed (Segment progn)
    | stmt::rest ->
      begin
	match stmt with
	| V.Special("int 0x80") -> add_exit progn (Completed (SysCall progn.p_core.m_core.eip))
	| V.Special _ ->           add_exit progn (Completed (Special progn.p_core.m_core.eip))
	| V.Return _ ->            add_exit progn (Completed (Return progn.p_core.m_core.eip))
	| V.Call _ ->              add_exit progn (Completed (FunCall progn.p_core.m_core.eip))
	| V.Halt _  ->             add_exit progn (Completed (Halt progn.p_core.m_core.eip))
	| V.Label(l) ->
	  begin
	    match (decode_eip l) with
	    (* update this node to finished.  It's stmt list is just the current accum.
	       then, generate the raw child, which is an undecoded of the target eip *)  
	    | X86eip eip ->
	      if progn.p_core.m_core.eip = eip
	      then wsl (stmt::accum) rest (* this is this eip, no jumping. *)
	      else
	      (* this eip = nodes eip, skip it? *)
		complete_node node (Undecoded 
				      {m_core = {eip = eip;
						 parent = node;};
				       next = None;}) accum progn.decls
	    | Internal _ -> wsl accum rest (* drop internal labels on the floor *)
	  end
        | V.Jmp(V.Name(l)) ->
	  begin
	    match (decode_eip l) with
	    | X86eip eip -> complete_node node (Undecoded {m_core = {eip = eip;
								     parent = node;};
							   next = None;}) accum progn.decls
	    | Internal _ ->
	      match walk_to_label l rest with
	      | None -> add_exit progn (Completed (InternalLoop progn.p_core.m_core.eip))
	      | Some rest' -> wsl accum rest'
	  end 
        | V.CJmp(test, V.Name(true_lab), V.Name(false_lab)) ->
	  begin
	    match (decode_eip true_lab, decode_eip false_lab) with
	    | X86eip teip, X86eip feip -> 
	      let rec branch = Completed (Branch { test = test;
						   b_core = {eip = progn.p_core.m_core.eip;
							     parent = node;};
						   true_child =  Undecoded {m_core = {eip = teip; 
										      parent = branch;};
									    next = None;};
						   false_child = Undecoded {m_core = {eip = feip;
										      parent = branch;};
									    next = None;}}) in
	      complete_node node branch accum progn.decls
	    | X86eip teip, Internal flabel ->
	      begin
		match walk_to_label false_lab rest with
		| None -> add_exit progn (Completed (InternalLoop progn.p_core.m_core.eip))
		| Some rest' ->
		  begin
		    let rec branch = 
		      Completed (Branch { test = test;
					  b_core = {eip = progn.p_core.m_core.eip;
						    parent = node;};
					  true_child = Undecoded {m_core = {eip = teip;
									    parent = branch;};
								  next = None};
					  false_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip; (* dangerous!  should be the label. *)
										 parent = branch;};
								       next = None;};
							     decls = [];
							     stmts = rest';}}) in
		    complete_node node branch accum progn.decls
		  end
	      end
	    | Internal tlabel, X86eip feip ->
	      begin
		match walk_to_label true_lab rest with
		| None -> add_exit progn (Completed (InternalLoop progn.p_core.m_core.eip))
		| Some rest' ->
		  begin
		    let rec branch = Completed (Branch { test = test;
							 b_core = {eip = progn.p_core.m_core.eip;
								   parent = node;};
							 true_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip; (* dangerous!  should be the label. *)
											       parent = branch;};
										     next = None;};
									   decls = [];
									   stmts = rest';};
						     false_child = Undecoded {m_core = {eip = feip;
											parent = branch};
									      next = None}}) in
		    complete_node node branch accum progn.decls
		  end
	      end
	    | Internal tlabel, Internal flabel ->
	      begin
		match (walk_to_label true_lab rest), (walk_to_label false_lab rest) with
		| Some traw, Some fraw ->
		  let rec branch = Completed (Branch {test = test;
						      b_core = {eip = progn.p_core.m_core.eip;
								parent = node;};
						      true_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip;
											    parent = branch;};
										  next = None;};
									decls = [];
									stmts = traw};
						      false_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip;
											     parent = branch;};
										   next = None};
									 decls = [];
									 stmts = fraw}}) in
		  complete_node node branch accum progn.decls
		| _ -> add_exit progn (Completed (InternalLoop progn.p_core.m_core.eip))
	      end
	  end
	| _ -> wsl (stmt::accum) rest
	  end
  in
  wsl [] progn.stmts


let generate_children fm gamma = function
  | Undecoded a as node->
    begin
      assert(None = a.next);
      let decls, stmts = 
	(with_trans_cache a.m_core.eip
	   (fun () -> decode_insns fm gamma a.m_core.eip !opt_bb_size)) in
      let child = Raw { p_core = {m_core = {eip = a.m_core.eip;
					    parent = node;};
				  next = None;};
			decls = decls;
			stmts = stmts} in
      a.next <- Some child;
      [child]
    end
  | Raw progn as node -> [consider_statements node] (* this has to replace the current node with the appropriate structure? *)
  | Completed ft ->
    begin
      match ft with
	| ExternalLoop _
	| InternalLoop _ 
	| Return _
	| Halt _
	| FunCall _
	| SysCall _
	| Special _
	| SearchLimit _
	| Segment _ -> [] (* maybe this is progn.next *)
	| Branch b -> [b.true_child; b.false_child]
    end

let expand fm gamma (node : veritesting_node) =
  generate_children fm gamma node


let make_root eip = 
  let rec root = Undecoded {m_core = {eip = eip;
				      parent = root;};
			    next = None;} in
  root

let parent = function
    (* this is dangerous-- clearly some of the leaves have parents,
       but since we only really use parent pointers for cycle checking,
       it doesn't matter if we return their actual parent, or none. If
       we're testing them, they're the lookfor value, and they can't
       occur anywhere else along their ancestors as they are defined to
       be terminal. *)
    | Undecoded a -> Some a.m_core.parent
    | Raw progn -> Some progn.p_core.m_core.parent
    | Completed c ->
      (match c with
      | ExternalLoop a
      | InternalLoop a
      | Return a
      | Halt a
      | FunCall a
      | SysCall a
      | Special a
      | SearchLimit a -> None
      | Branch b -> Some b.b_core.parent 
      | Segment s -> Some s.p_core.m_core.parent)

let check_cycle (lookfor : veritesting_node) (root : veritesting_node) =
  let rec walk_parents (current : veritesting_node) =
    if (lookfor == current)
    then true
    else (let parent = parent current in
	  match parent with
	  | None -> true
	  | Some p ->
	    if current == p
	    then false
	    else walk_parents p) in
  walk_parents root

let replace_child current_node replacement =
  match (parent current_node) with
  | None -> ()
  | Some p ->
    match p with
    | Undecoded a -> a.next <- Some replacement
    | Raw a -> a.p_core.next <- Some replacement
    | Completed c ->
      (match c with
      | ExternalLoop _
      | InternalLoop _
      | Return _
      | Halt _
      | FunCall _
      | SysCall _
      | Special _
      | SearchLimit _ -> failwith "Illegal parent type!"
      | Segment s -> s.p_core.next <- Some replacement
      | Branch b ->
	(if current_node == b.true_child
	 then b.true_child <- replacement
	 else if b.false_child == current_node
	 then b.false_child <- replacement
	 else failwith "current_node isn't a child to its parent?"))


let successor = function
    | Undecoded a -> a.next
    | Raw progn -> progn.p_core.next
    | Completed c ->
      (match c with
      | ExternalLoop _
      | InternalLoop _
      | Return _
      | Halt _
      | FunCall _
      | SysCall _
      | Special _
      | SearchLimit _ -> None
      | Branch b -> None (* this is a lie *)
      | Segment s -> s.p_core.next)
  

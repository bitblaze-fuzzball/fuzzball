module V = Vine
open Exec_exceptions
open Exec_options
open Frag_simplify
open Fragment_machine
open Exec_run_common

type 'a common = {
  eip : int64;
  mutable parent : 'a;
  depth : int;
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


let depth = function
    | Undecoded a -> a.m_core.depth
    | Raw progn -> progn.p_core.m_core.depth
    | Completed c ->
      (match c with
      | ExternalLoop a
      | InternalLoop a
      | Return a
      | Halt a
      | FunCall a
      | SysCall a
      | Special a
      | SearchLimit a -> ~-1 (*JTT HACK HACK HACK *)
      | Branch b -> b.b_core.depth
      | Segment s -> s.p_core.m_core.depth)


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


let replace_parent target = function
  | Undecoded a -> a.m_core.parent <- target
  | Raw progn -> progn.p_core.m_core.parent <- target
    | Completed c ->
      (match c with
      | ExternalLoop a
      | InternalLoop a
      | Return a
      | Halt a
      | FunCall a
      | SysCall a
      | Special a
      | SearchLimit a -> ()
      | Branch b -> b.b_core.parent <- target
      | Segment s -> s.p_core.m_core.parent <- target)


let complete_node
    (raw_node : veritesting_node)
    (child : veritesting_node)
    (sl : V.stmt list)
    (dl: V.decl list) =
  assert(sl != []);
  (match raw_node with
  | Raw progn -> 
    begin
     (* stitch the completed child into the code segment. *)
      let closed_variant =
	Completed
	  (Segment {p_core = {m_core = {eip = (eip_of_node raw_node);
					parent = raw_node;
					depth = depth raw_node; };
			      next = Some child;};
		    stmts = (List.rev sl);
		    decls = dl;}) in
      progn.p_core.next <- Some closed_variant;
      replace_parent closed_variant child;
(*    Printf.eprintf "Completing %s with %s\n"
      (node_to_string raw_node) (node_to_string closed_variant); *)
      closed_variant
    end
  | _ -> failwith "Should only be able to call complete node on a raw_node")(*;
  child*)


let add_exit progn next =
  assert (None = progn.p_core.next);
  progn.p_core.next <- Some next;
  next


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
    begin
(*    Printf.eprintf "Changing successor of %s from %s to %s\n"
      (node_to_string p) (node_to_string current_node) (node_to_string replacement); *)
    match p with
    | Undecoded a -> 
      begin
	match a.next with 
	| None -> failwith "Called replace_child on a node with no child."
	| Some next ->
	  begin
	 (*Printf.eprintf "Old next: %s\n" (node_to_string next); *)
	    if(next == current_node) then
	      a.next <- Some replacement
	    else failwith "replace_child: parent of current not current"
	  end
      end
    | Raw a -> 
      begin
	match a.p_core.next with
	| None -> failwith "Called replace_child on a node with no child."
	| Some next ->
	  begin
	    (* Printf.eprintf "Old next: %s\n" (node_to_string next);*)
	    if (next == current_node) then
	      a.p_core.next <- Some replacement
	    else failwith "replace_child: parent of current not current"
	   end
      end
    | Completed c ->
      begin
	match c with
	| ExternalLoop _
	| InternalLoop _
	| Return _
      | Halt _
      | FunCall _
      | SysCall _
      | Special _
      | SearchLimit _ -> failwith "Illegal parent type!"
      | Segment s ->
	begin
	  match s.p_core.next with
	  | None -> failwith "Called replace_child on a node with no child."
	  | Some next ->
	    begin
	    (* Printf.eprintf "Old next: %s\n" (node_to_string next);*)
	      assert(next == current_node);
	      s.p_core.next <- Some replacement
	    end
	end
      | Branch b ->
	(if current_node == b.true_child
	 then b.true_child <- replacement
	 else if b.false_child == current_node
	 then b.false_child <- replacement
	 else failwith "current_node isn't a child to its parent?")
      end
    end


let rec truncate_node = function
  (** If you're doing a truncated expand of a veritesting node, this is
      the way you tell the search that the area beyond this point was
      intentionally not considered because it was too far afield from
      the root. *)
  | Undecoded m -> m.next <- Some (Completed (SearchLimit m.m_core.eip))
  | Raw progn -> progn.p_core.next <- Some (Completed (SearchLimit progn.p_core.m_core.eip))
  | Completed ft as node -> match ft with
    | Segment progn -> (replace_child node (Completed (SearchLimit progn.p_core.m_core.eip)))
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

let set_successor value = function 
    | Undecoded a -> a.next <- Some value
    | Raw progn -> progn.p_core.next <- Some value
    | Completed c ->
      (match c with
      | ExternalLoop _
      | InternalLoop _
      | Return _
      | Halt _
      | FunCall _
      | SysCall _
      | Special _
      | SearchLimit _
      | Branch _ -> failwith "setting successor on a node with no natural successor."
      | Segment s -> s.p_core.next <- Some value)

let consider_statements (node : veritesting_node) = 
  let progn = match node with
    |Raw progn -> progn
    | _ -> failwith "only for raw nodes" in
  let rec wsl accum = function
    | [] -> let comp = 
	      Completed (Segment {p_core = {m_core = {eip = (eip_of_node node);
						      parent = node;
						      depth = depth node; };
					    next = None;};
				  stmts = List.rev accum;
				  decls = progn.decls;}) in
	    set_successor comp node;
	    comp
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
		begin
		  (* Printf.eprintf "Completing %s" (node_to_string node);*)
		  complete_node node (Undecoded 
					{m_core = {eip = eip;
						   parent = node;
						   depth = depth node;};
					 next = None;}) accum progn.decls
		end
	    | Internal _ -> wsl accum rest (* drop internal labels on the floor *)
	  end
        | V.Jmp(V.Name(l)) ->
	  begin
	    match (decode_eip l) with
	    | X86eip eip -> (complete_node
			       node
			       (Undecoded {m_core = {eip = eip;
						     parent = node;
						     depth = 1 + (depth node);};
					   next = None;}) accum progn.decls)
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
							     parent = node;
							     depth = depth node;};
						   true_child =  Undecoded {m_core = {eip = teip; 
										      parent = branch;
										      depth = 1 + (depth node);};
									    next = None;};
						   false_child = Undecoded {m_core = {eip = feip;
										      parent = branch;
										      depth = 1 + (depth node);};
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
						    parent = node;
						    depth = depth node;};
					  true_child = Undecoded {m_core = {eip = teip;
									    parent = branch;
									    depth = 1 + (depth node);};
								  next = None};
					  false_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip;
										 (* dangerous!  should be the label. *)
										 parent = branch;
										 depth = 1 + (depth node);};
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
								   parent = node;
								   depth = depth node;};
							 true_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip;
											       (* dangerous!  should be the label. *)
											       parent = branch;
											       depth = 1 + (depth node);};
										     next = None;};
									   decls = [];
									   stmts = rest';};
						     false_child = Undecoded {m_core = {eip = feip;
											parent = branch;
											depth = 1 + (depth node);};
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
								parent = node;
								depth = depth node;};
						      true_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip;
											    parent = branch;
											    depth = 1 + (depth node);};
										  next = None;};
									decls = [];
									stmts = traw};
						      false_child = Raw {p_core = {m_core = {eip = progn.p_core.m_core.eip;
											     parent = branch;
											     depth = 1 + (depth node);};
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
  (* Printf.eprintf "Considering %s\n" (node_to_string node);
     List.iter (Vine.stmt_to_channel stderr) progn.stmts;
     let ret = wsl [] progn.stmts in
     Printf.eprintf "Done, result was %s.\n" (node_to_string ret);
     ret
  *)
  wsl [] progn.stmts

let generate_children fm gamma = function
  | Undecoded a as node->
    begin
      assert(None = a.next);
      let decls, stmts = 
	(with_trans_cache a.m_core.eip
	   (fun () -> decode_insns fm gamma a.m_core.eip !opt_bb_size)) in
      let child = Raw { p_core = {m_core = {eip = a.m_core.eip;
					    parent = node;
					    depth = depth node;};
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
	| SearchLimit _ -> []
	| Segment p ->
	  (match p.p_core.next with
	  | Some n -> [n]
	  | None -> [] )
	| Branch b -> [b.true_child; b.false_child]
    end

let expand fm gamma (node : veritesting_node) =
  generate_children fm gamma node


let make_root eip = 
  let rec root = Undecoded {m_core = {eip = eip;
				      parent = root;
				      depth = 0;};
			    next = None;} in
  root

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

let successors = function
    | Undecoded a ->
      (match a.next with
      | Some s -> [s]
      | None -> [])
    | Raw progn ->
      (match progn.p_core.next with
	Some s -> [s]
      | None -> [])
    | Completed c ->
      (match c with
      | ExternalLoop _
      | InternalLoop _
      | Return _
      | Halt _
      | FunCall _
      | SysCall _
      | Special _
      | SearchLimit _ -> []
      | Branch b -> [b.true_child; b.false_child]
      | Segment s ->
	(match s.p_core.next with
	| Some x -> [x]
	| None -> []))

let do_offset ch offset =
  for i = 0 to offset
  do
    Printf.fprintf ch "\t";
  done

let node_to_ch ch offset n =
  do_offset ch offset;
  Printf.fprintf ch "%s\n" (node_to_string n)

let node_and_stmts ch offset n =
  let indent _ = do_offset ch offset in
  indent ();
  Printf.fprintf ch "%s\n" (node_to_string n);
  match n with
  | Completed (Segment s) ->
    List.iter (fun s -> indent(); Vine.stmt_to_channel ch s) s.stmts;
    List.iter (fun d -> indent(); Vine.decl_to_channel ch d) s.decls
  | _ -> ()

let rec print_tree_opt ?(print_fn = (node_to_ch stderr)) ?(offset = 0) = function
  | None -> ()
  | Some node ->
    begin
      print_fn offset node;
      match node with
      | Undecoded a -> print_tree_opt ~print_fn ~offset a.next
      | Raw progn -> print_tree_opt ~print_fn ~offset progn.p_core.next
      | Completed c ->
	(match c with
	| ExternalLoop _
	| InternalLoop _
	| Return _
	| Halt _
	| FunCall _
	| SysCall _
	| Special _
	| SearchLimit _ -> ()
	| Branch b ->
	  begin
	    print_tree_opt ~print_fn ~offset:(offset+1) (Some b.true_child); 
	    print_tree_opt ~print_fn ~offset:(offset+1) (Some b.false_child)
	  end 
	| Segment s -> print_tree_opt ~print_fn ~offset s.p_core.next)
    end

let print_tree node = print_tree_opt (Some node)

let print_tree_statements node =
  print_tree_opt ~print_fn:(node_and_stmts stderr) (Some node)

let rec find_endpoints node =
  match successors node with
  | [] -> [node]
  | [a] -> find_endpoints a
  | [a;b] -> (find_endpoints a) @ (find_endpoints b)
  | _ -> failwith "Successors should return 0, 1 or 2 children. I saw more!"

let rec check_uniqueness = function
    | [] -> None
    | [ep] -> Some ep
    | fst::snd::tl ->
      if (equal fst snd) then
	check_uniqueness (snd::tl) else
	None

let unique_endpoint node = 
  check_uniqueness (find_endpoints node)

(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;

type decision_tree_node = {
  mutable parent : decision_tree_node option;
  (* None: unexplored; Some None: unsat; Some Some n: sat *)
  mutable f_child : decision_tree_node option option;
  mutable t_child : decision_tree_node option option;
  mutable all_seen : bool;
  mutable query_children : int option;
  mutable query_counted : bool;
  mutable ident : int }

let next_dt_ident = ref 1

let ident_to_node_table = Array.init 1024 (fun _ -> None)

let new_dt_node the_parent =
  next_dt_ident := !next_dt_ident + 1;
  let i = !next_dt_ident in
  let i3 = i land 1023 and
      i2 = (i asr 10) land 1023 and
      i1 = i asr 20
  in
  let t2 = match ident_to_node_table.(i1) with
    | Some t -> t
    | None ->
	let t = Array.init 1024 (fun _ -> None) in
	  ident_to_node_table.(i1) <- Some t; t
  in
  let t3 = match t2.(i2) with
    | Some t -> t
    | None ->
	let t = Array.init 1024 (fun _ -> None) in
	  t2.(i2) <- Some t; t
  in
  let node =
    {parent = the_parent;
     f_child = None; t_child = None;
     query_children = None; query_counted = false;
     all_seen = false; ident = i}
  in
    t3.(i3) <- Some node;
    node

let ident_to_node i =
  let i3 = i land 1023 and
      i2 = (i asr 10) land 1023 and
      i1 = i asr 20
  in
    match ident_to_node_table.(i1) with
      | Some t2 ->
	  (match t2.(i2) with
	     | Some t3 ->
		 (match t3.(i3) with
		    | Some node -> node
		    | _ -> failwith "Node missing (t3)")
	     | _ -> failwith "Node sub-table missing (t2)")
      | None -> failwith "Node sub-table missing (t1)"

(* This hash algorithm is FNV-1a,
   c.f. http://www.isthe.com/chongo/tech/comp/fnv/index.html *)
let hash_round h x =
  let h' = Int32.logxor h (Int32.of_int x) in
    Int32.mul h' (Int32.of_int 0x1000193)

class binary_decision_tree = object(self)
  inherit Decision_tree.decision_tree

  val root = new_dt_node None
  val mutable cur = new_dt_node None (* garbage *)
  val mutable cur_query = new_dt_node None (* garbage *)
  val mutable depth = 0
  val mutable path_hash = Int64.to_int32 0x811c9dc5L
  val mutable iteration_count = 0
  val mutable randomness = Random.State.make [|!opt_random_seed; 0|]

  method init =
    root.query_children <- Some 0;
    cur <- root;
    cur_query <- root;
    if !opt_trace_decision_tree then
      Printf.printf "DT: Initialized.\n";
    (self :> Decision_tree.decision_tree)

  method reset =
    cur <- root;
    cur_query <- root;
    depth <- 0;
    path_hash <- Int64.to_int32 0x811c9dc5L;
    iteration_count <- iteration_count + 1;
    if !opt_trace_randomness then
      Printf.printf "Initializing random state as %08x\n" iteration_count;
    randomness <- Random.State.make [|!opt_random_seed; iteration_count|]

  method get_hist =
    let child_is kid n =
      match kid with
	| Some (Some n') -> n' == n
	| _ -> false
    in
    let last_choice n p =
      if child_is p.f_child n then
	false
      else if child_is p.t_child n then
	true
      else
	failwith "Parent invariant failure in get_hist"
    in
    let rec loop n =
      match n.parent with
	| None -> []
	| Some p -> (last_choice n p) :: loop p
    in
      loop cur

  method get_hist_str = 
    String.concat ""
      (List.map (fun b -> if b then "1" else "0") (List.rev self#get_hist));

  method private get_hist_queries =
    let kid n b =
      match (n.f_child, n.t_child, b) with
	| (Some(Some k), _, false) -> k
	| (_, Some(Some k), true) -> k
	| _ -> failwith "missing kid in get_hist_queries"
    in
    let get_query n h =
      let rec loop q n h =
	if n.query_children <> None then
	  (q, h, n)
	else
	  match h with
	    | [] -> (q, h, n)
	    | first :: rest -> loop (first :: q) (kid n first) rest
      in
      assert(n.query_children <> None);
	match h with
	  | [] -> ([], [], n)
	  | first :: rest -> 
	      let (l, h', n') = loop [first] (kid n first) rest in
		((List.rev l), h', n')
    in
    let rec outer_loop l n h =
      match h with
	| [] -> l
	| _ -> let (ql, h', n') = get_query n h in
	    outer_loop (ql :: l) n' h'
    in
    let hist = List.rev self#get_hist in
    List.rev (outer_loop [] root hist)

  method get_hist_str_queries = 
    String.concat "-"
      (List.map
	 (fun q -> String.concat ""
	    (List.map (fun b -> if b then "1" else "0") q))
	 self#get_hist_queries)

  method get_hist_str_bracketed = 
    String.concat ""
      (List.map
	 (fun q -> let s = String.concat ""
	    (List.map (fun b -> if b then "1" else "0") q) in
	    if String.length s = 1 then s else "[" ^ s ^ "]")
	 self#get_hist_queries)

  method get_depth = depth

  method add_kid b =
    if !opt_trace_decision_tree then
      Printf.printf "DT: Adding %B child to %d\n" b cur.ident;
    assert(not cur.all_seen);
    match (b, cur.f_child, cur.t_child) with
      | (false, Some(Some kid), _)
      | (true,  _, Some(Some kid)) -> () (* already there *)
      | (false, None, _) ->
	  let new_kid = new_dt_node (Some cur) in
	    cur.f_child <- Some (Some new_kid)
      | (true,  _, None) ->
	  let new_kid = new_dt_node (Some cur) in
	    cur.t_child <- Some (Some new_kid)
      | (false, Some None, _)
      | (true,  _, Some None) ->
	  failwith "Tried to extend an unsat branch"

  method start_new_query =
    assert(cur.query_children <> None);
    cur_query <- cur

  method start_new_query_binary =
    self#start_new_query;
    match cur_query.query_children with
      |	None | Some 0 | Some 1 | Some 2 -> ()
      | _ -> failwith "Too many children in start_new_query_binary"

  method count_query =
    let rec finish_internal_nodes n top =
      if !opt_trace_decision_tree then
	Printf.printf "DT: Finish internal nodes at %d (%B)\n" n.ident top;
      if n.query_children = None || top then
	((match n.f_child with
	    | Some(Some kid) -> finish_internal_nodes kid false
	    | Some None -> ()
	    | None -> n.f_child <- Some None);
	 (match n.t_child with
	    | Some(Some kid) -> finish_internal_nodes kid false
	    | Some None -> ()
	    | None -> n.t_child <- Some None);
	 self#maybe_mark_all_seen_node n)
    in
      if !opt_trace_decision_tree then
	Printf.printf "DT: count_query at %d (q %d)" cur.ident cur_query.ident;
      if cur.ident <> cur_query.ident then 
	(if cur.query_children = None then
	   cur.query_children <- Some 0;
	 (match cur_query.query_children with
	    | None -> failwith "Count_query outside a query"
	    | Some k when not cur.query_counted ->
		if !opt_trace_decision_tree then	
		  Printf.printf " -> %d" (k+1);
		assert(k < !opt_query_branch_limit);
 		cur_query.query_children <- Some (k + 1);
		cur.query_counted <- true
	    | Some k -> ());
	 if !opt_trace_decision_tree then	
	   Printf.printf "\n";
	 match cur_query.query_children with 
	   | Some k when k >= !opt_query_branch_limit ->
	       finish_internal_nodes cur_query true
	   | _ -> ())
      else
	if !opt_trace_decision_tree then	
	  Printf.printf "\n"

  method extend b =
    if !opt_trace_decision_tree then
      Printf.printf "DT: Extending with %B at %d\n" b cur.ident;
    self#add_kid b;
    path_hash <- hash_round path_hash (if b then 49 else 48);
    (let h = Int32.to_int path_hash in
      (if !opt_trace_randomness then
	 Printf.printf "Setting random state to %08x\n" h;
       randomness <- Random.State.make [|!opt_random_seed; h|]));
    (match (b, cur.f_child, cur.t_child) with
       | (false, Some(Some kid), _) -> cur <- kid
       | (true,  _, Some(Some kid)) -> cur <- kid
       | (false, None, _)
       | (true,  _, None)
       | (false, Some None, _)
       | (true,  _, Some None) ->
	   failwith "Add_kid failed in extend");
    depth <- depth + 1;
    if (Int64.of_int depth) > !opt_path_depth_limit then
      raise DeepPath

  method set_iter_seed i =
    path_hash <- hash_round path_hash i

  method random_bit =
    let b = Random.State.bool randomness in
      if !opt_trace_randomness then
	Printf.printf "Flipping a coin to get %B\n" b;
      b

  method random_float =
    let f = Random.State.float randomness 1.0 in
      if !opt_trace_randomness then
	Printf.printf "Flipping a floating coin to get %f\n" f;
      f

  method record_unsat b =
    match (b, cur.f_child, cur.t_child) with
      | (false, None, _) ->
	  cur.f_child <- Some None
      | (true,  _, None) ->
	  cur.t_child <- Some None
      | (false, Some None, _)
      | (true,  _, Some None) -> () (* already recorded *)
      | (false, Some (Some _), _)
      | (true,  _, Some (Some _)) ->
	  failwith "Trying to make sat branch unsat in record_unsat"

  method try_extend (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) (random_bit_gen : unit -> bool) =
    let known b = 
      non_try_func b;
      self#extend b;
      (b, (trans_func b))
    in
    let known_check b =
      let c = trans_func b in
	if try_func b c then
	  (self#extend b; (b, c))
	else
	  failwith "Unexpected unsat in try_extend"
    in
    let try_or_boring b =
      let c = trans_func b in
	if try_func b c then
	  (self#extend b; (b, c))
	else
	  (self#record_unsat b;
	   self#mark_all_seen_node cur;
	   known (not b))
    in
    let try_both () =
      let b = random_bit_gen () in
      let c = trans_func b and
	  c' = trans_func (not b) in
	if try_func b c then
	  (if try_func (not b) c' then
	     self#add_kid (not b)
	   else
	     self#record_unsat (not b);
	   self#extend b;
	   (b, c))
        else
	  (self#record_unsat b;
	   if try_func (not b) c' then
	     (self#extend (not b);
	      ((not b), c'))
	   else
	     failwith "Both branches unsat in try_extend")
    in
      assert(not cur.all_seen);
      let limited = match cur_query.query_children with 
	| Some k when k >= !opt_query_branch_limit -> true
	| _ -> false
      in
      if !opt_trace_decision_tree then	
	Printf.printf "try_extend at %d\n" cur.ident;
      match (cur.f_child, cur.t_child, limited) with
	| (Some(Some f_kid), Some(Some t_kid), _) ->
	    (match (f_kid.all_seen, t_kid.all_seen) with
	       | (true, true) -> 
		   if cur.all_seen then
		     known (random_bit_gen ())
		   else
		     failwith "all_seen invariant failure"
	       | (false, true) -> known false
	       | (true, false) -> known true
	       | (false, false) -> known (random_bit_gen ()))
	| (Some(Some f_kid), Some None, _) ->
	    assert(not f_kid.all_seen);
	    known false
	| (Some None, Some(Some t_kid), _) ->
	    assert(not t_kid.all_seen);
	    known true
	| (Some None, Some None, _) -> failwith "Unsat node in try_extend"
	| (Some(Some f_kid), None, false) ->
	    if f_kid.all_seen then
	      try_or_boring true
	    else
	      try_both ()
	| (Some(Some f_kid), None, true) -> known false
	| (None, Some(Some t_kid), false) ->
	    if t_kid.all_seen then
	      try_or_boring false
	    else
	      try_both ()
	| (None, Some(Some t_kid), true) -> known true
	| (None, Some None, _) -> known_check false
	| (Some None, None, _) -> known_check true
	| (None, None, false) ->
	    try_both ()
	| (None, None, true) ->
	    failwith "Limited/unknown invariant failure in try_extend"

  method try_extend_memoryless (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) (random_bit_gen : unit -> bool) =
    let known b = 
      non_try_func b;
      self#extend b;
      (b, (trans_func b))
    in
    let try_branch b c field fn = 
      match field with
	| (Some(Some f_kid)) -> known b
	| Some None -> raise KnownPath
	| None ->
	    if try_func b c then
	      (self#extend b; (b, c))
	    else
	      (self#record_unsat b; (fn b))
    in
    if (random_bit_gen ()) then
      try_branch false (trans_func false) cur.f_child
	(fun _ -> try_branch true (trans_func true) cur.t_child
	   (fun _ -> failwith "Both branches unsat in try_extend"))
    else
      try_branch true (trans_func true) cur.t_child
	(fun _ -> try_branch false (trans_func false) cur.f_child
	   (fun _ -> failwith "Both branches unsat in try_extend"))

  method private mark_all_seen_node node =
    let mark n =
      (* check the invariant that a node can only be marked all_seen
	 if all of its children are. *)
      (match n.f_child with
	 | Some(Some kid) -> assert(kid.all_seen);
	 | _ -> ());
      (match n.t_child with
	 | Some(Some kid) ->
	     if not kid.all_seen then
	       (Printf.printf "all_seen invariant failure at node %d\n"
		  n.ident;
		self#print_tree stdout;
	       assert(kid.all_seen))
	 | _ -> ());
      n.all_seen <- true
    in
    let rec mark_internal_nodes n =
      (match n.f_child with
	 | Some(Some kid) when
	     kid.query_children = None && not kid.all_seen ->
	     mark_internal_nodes kid
	 | _ -> ());
      (match n.t_child with
	 | Some(Some kid) when
	     kid.query_children = None && not kid.all_seen ->
	     mark_internal_nodes kid
	 | _ -> ());
      mark n
    in
    let rec internal_nodes_check n top =
      if n.all_seen then
	true
      else if n.query_children <> None && not top then
	false
      else if
	(match n.f_child with
	   | Some(Some kid) -> internal_nodes_check kid false
	   | _ -> true)
	  &&
	(match n.t_child with
	   | Some(Some kid) -> internal_nodes_check kid false
	   | _ -> true)
      then
	(mark n; true)
      else
	false
    in
    let rec query_parent n =
      match n.query_children with
	| Some k -> n
	| None ->
	    match n.parent with
	      | Some p -> query_parent p
	      | _ -> failwith "Missing query node parent"
    in
    let rec loop n = 
      mark n;
      match n.parent with
	| None -> ()
	| Some p ->
	    (match (p.all_seen, p.t_child, p.f_child) with
	       | (false, Some(Some f_kid), Some(Some t_kid))
		   when f_kid.all_seen && t_kid.all_seen ->
		   loop p
	       | (false, Some(Some f_kid), Some None)
		   when f_kid.all_seen ->
		   loop p
	       | (false, Some None, Some(Some t_kid))
		   when t_kid.all_seen ->
		   loop p
	       | _ -> ());
	    let q_parent = query_parent p in
	      match q_parent.query_children with
		| Some k when k >= !opt_query_branch_limit ->
		    if k > !opt_query_branch_limit then
		      (Printf.printf "Node %d has excessive count %d\n"
			 q_parent.ident k;
		       assert(k = !opt_query_branch_limit)
		      );
		    if internal_nodes_check q_parent true then
		      loop q_parent
		| _ -> ()
    in
      if !opt_trace_decision_tree then	
	Printf.printf "DT: Mark_all_seen at %d\n" node.ident;
      loop node

  method private maybe_mark_all_seen_node n =
    if !opt_trace_decision_tree then	
      Printf.printf "DT: maybe_mark_all_seen_node at %d\n" n.ident;
    if (match n.f_child with
	  | None -> false
	  | Some None -> true
	  | Some(Some kid) -> kid.all_seen)
      &&
	  (match n.t_child with
	     | None -> false
	     | Some None -> true
	     | Some(Some kid) -> kid.all_seen)
    then
      self#mark_all_seen_node n

  method mark_all_seen = self#mark_all_seen_node cur

  method try_again_p = not root.all_seen

  method check_last_choices =
    match cur.parent with
      | None -> failwith "Missing parent in check_last_choices"
      | Some p ->
	  (match (p.f_child, p.t_child) with
	     | (Some(Some _), Some (Some _)) -> None
	     | (Some(Some _), Some None) -> Some false
	     | (Some None, Some(Some _)) -> Some true
	     | (Some None, Some None) ->
		 failwith "Parent invariant failure in check_last_choices"
	     | (None, _)
	     | (_, None)
		 -> failwith "Unexplored parent in check_last_choices")

  method have_choice =
    let result = 
      match (cur.f_child, cur.t_child) with
	| (None, _)
	| (_, None)
	  -> true
	| (Some(Some fkid), Some (Some tkid)) -> 
	    (match fkid.all_seen, tkid.all_seen with
	       | (true, true) -> true
	       | (false, false) -> true
	       | (true, false)
	       | (false, true) -> false)
	| (Some(Some _), Some None)
	| (Some None, Some(Some _))
	  -> false
	| (Some None, Some None) ->
	    failwith "Feasibility invariant failure in have_choice"
    in
      if !opt_trace_decision_tree then
	Printf.printf "DT: at %d, have_choice is %b\n" cur.ident result;
      result

  method cur_ident =
    cur.ident

  method is_live_ident i =
    let node = ident_to_node i in
      not node.all_seen

  method cur_can_reach_ident i =
    let rec loop n =
      match n.parent with
	| None -> None
	| Some p when p == cur ->
	    (match (cur.f_child, cur.t_child) with
	       | (Some Some f_kid, _) when f_kid == n -> Some false
	       | (_, Some Some t_kid) when t_kid == n -> Some true
	       | _ -> 
		   failwith "Parent invariant failure in cur_can_reach_ident")
	| Some p -> loop p
    in
    let dir = loop (ident_to_node i)
    in
      dir

  method print_tree chan =
    let kid_to_string mmn =
      match mmn with
	| None -> "unknown"
	| Some None -> "none"
	| Some(Some kid) -> string_of_int kid.ident
    in
    let print_node n =
      Printf.fprintf chan "%d: " n.ident;
      Printf.fprintf chan "%s " (kid_to_string n.f_child);
      Printf.fprintf chan "%s " (kid_to_string n.t_child);
      Printf.fprintf chan "%s " (if n.all_seen then "*" else "?");
      Printf.fprintf chan "%s " (kid_to_string (Some n.parent));
      Printf.fprintf chan "%s\n"
	(match n.query_children with
	   | None -> "[]"
	   | Some k -> "[" ^ (string_of_int k) ^ "]");
      
    in
    let rec loop n =
      print_node n;
      (match n.f_child with
	 | Some(Some kid) -> loop kid
	 | _ -> ());
      (match n.t_child with
	 | Some(Some kid) -> loop kid
	 | _ -> ());
    in
      loop root
end

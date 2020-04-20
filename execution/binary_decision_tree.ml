(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;
open Exec_assert_minder;;

type decision_tree_node = {
  (* Only the root has parent = None *)
  mutable parent : dt_node_ref option;

  (* None: unexplored; Some None: unsat; Some Some n: sat *)
  mutable f_child : dt_node_ref option option;
  mutable t_child : dt_node_ref option option;

  (* Initially false. If true, the full subtree rooted at this node
     has been completely explored, and so shouldn't be revisited. *)
  mutable all_seen : bool;

  (* In the presence of multi-way choices like pointer concretization,
     we need a partial subtree of multiple nodes to represent a
     3-or-more-way decision. This leads to an abstraction of a multi-way
     tree built on top of the underlying binary tree. The binary tree
     nodes that represent the roots of the multi-way partial subtrees
     have query_children = Some n, while all the other binary tree nodes
     have query_children = None. The value of n in Some n is the number
     of multi-way tree children that have been explored. *)
  mutable query_children : int option;

  (* Initially false. Set to true at the point a node is counted as
     one of the query_children (see above) of a multi-way choice. *)
  mutable query_counted : bool;

  mutable heur_min : int;
  mutable heur_max : int;

  (* Numeric identifier of a decision tree node. We use these instead
     of pointers as "dt_node_ref"s so that the tree can use a more
     compact representation. It serves as a sequential index into either
     an in-memory array of string representations or an on-disk file. *)
  mutable ident : int;

  mutable eip_loc : int64 }
and
  dt_node_ref = int

let bool_to_string b =
  if b
  then "1"
  else "0" 

(* Mask to restrict an OCaml "int" to be at most 32 bits. This is a
   no-op on a 32-bit system, but not on a 64-bit system. And it's a
   little tricky to write, becuse the code has to compile correctly on
   either kind of system. *)
let mask_32bits_int =
  if 0x7fffffff = -1 then
    0x7fffffff
  else
    Int64.to_int 0xffffffffL

let dt_node_to_string n =
  let parent_int = match n.parent with
    | None -> 0
    | Some pr -> pr in
  let kid_to_int = function
    | None -> 0x7fffffff
    | Some None -> 0
    | Some (Some k) -> k
  in
  let f_child_int = kid_to_int n.f_child and
      t_child_int = kid_to_int n.t_child in
  let query_children_int = match n.query_children with
    | None -> -1
    | Some c -> c
  in
  let flags_int = (if n.all_seen then 1 else 0) +
    (if n.query_counted then 2 else 0)
  in
  let s = Printf.sprintf "%08x%c%08x%08x%08x%08x%04x%016Lx"
    (parent_int land mask_32bits_int) (Char.chr (0x40 + flags_int))
    (f_child_int land mask_32bits_int) (t_child_int land mask_32bits_int)
    (n.heur_min land mask_32bits_int) (n.heur_max land mask_32bits_int)
    (query_children_int land 0xffff) n.eip_loc in
  g_assert (String.length s = 61) 100 "Binary_decision_tree.dt_node_to_string";
    s

let string_to_dt_node ident_arg s =
  g_assert(String.length s = 61) 100 "Binary_decision_tree.string_to_dt_node";
  let parent_str = String.sub s 0 8 and
      flags_char = s.[8] and
      f_child_str = String.sub s 9 8 and
      t_child_str = String.sub s 17 8 and
      heur_min_str = String.sub s 25 8 and
      heur_max_str = String.sub s 33 8 and
      query_children_str = String.sub s 41 4 and
      eip_loc_str = String.sub s 45 16
  in
  let int_of_hex_string s = int_of_string ("0x" ^ s) in
  let parent_int = int_of_hex_string parent_str and
      flags_int = (Char.code flags_char) - 0x40 and
      f_child_int = int_of_hex_string f_child_str and
      t_child_int = int_of_hex_string t_child_str and
      heur_min_int = int_of_hex_string heur_min_str and
      heur_max_int = int_of_hex_string heur_max_str and
      query_children_int = int_of_hex_string query_children_str and
      eip_loc_i64 = Int64.of_string ("0x" ^ eip_loc_str)
  in
  let parent_o = match parent_int with
    | 0 -> None
    | i -> Some i
  in
  let kid_to_oo = function
    | 0x7fffffff -> None
    | 0 -> Some None
    | i -> Some (Some i)
  in
  let f_child_oo = kid_to_oo f_child_int and
      t_child_oo = kid_to_oo t_child_int in
  let query_children_o = match query_children_int with
    | 0xffff -> None
    | i -> Some i
  in
  let maybe_negative_one i =
    if i land mask_32bits_int = mask_32bits_int then
      -1
    else i
  in
  let all_seen_bool = (flags_int land 1) <> 0 and
      query_counted_bool = (flags_int land 2) <> 0
  in
    {parent = parent_o;
     f_child = f_child_oo; t_child = t_child_oo;
     query_children = query_children_o; query_counted = query_counted_bool;
     heur_min = maybe_negative_one heur_min_int;
     heur_max = maybe_negative_one heur_max_int;
     all_seen = all_seen_bool; ident = ident_arg; eip_loc = eip_loc_i64}

let next_dt_ident = ref 0

let ident_to_node_table = Array.make 1024 None

let nodes_fd_or = ref None

let nodes_fd () =
  g_assert(!opt_decision_tree_use_file) 100 "Binary_decision_tree.nodes_fd";
  match !nodes_fd_or with
    | Some fd -> fd
    | None ->
	let fd = Unix.openfile "fuzzball.tree"
	  [Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT] 0o666
	in
	  nodes_fd_or := Some fd;
	  fd

let ident_to_node i =
  if !opt_decision_tree_use_file then
    ((let off = Unix.lseek (nodes_fd ()) (54 * (i-1)) Unix.SEEK_SET in
	g_assert(off = 54 * (i-1)) 100 "Binary_decision_tree.ident_to_node");
     let buf = String.create 54 in
     let len = Unix.read (nodes_fd ()) buf 0 54 in
       g_assert(len = 54) 100 "Binary_decision_tree.ident_to_node";
       g_assert(String.sub buf 45 1 = "\n") 100 "Binary_decision_tree.ident_to_node";
       string_to_dt_node i (String.sub buf 0 53))
  else
    let i3 = i land 1023 and
	i2 = (i asr 10) land 1023 and
	i1 = i asr 20
    in
      match ident_to_node_table.(i1) with
	| Some t2 ->
	    (match t2.(i2) with
	       | Some t3 ->
		   string_to_dt_node i t3.(i3)
	       | _ -> failwith "Node sub-table missing (t2)")
	| None -> failwith "Node sub-table missing (t1)"

let get_dt_node (dnr : dt_node_ref) : decision_tree_node =
  ident_to_node dnr

let ref_dt_node (n : decision_tree_node) : dt_node_ref =
  n.ident

let get_parent n =
  match n.parent with
    | None -> None
    | Some nr -> Some (get_dt_node nr)

let get_f_child n =
  match n.f_child with
    | None -> None
    | Some None -> Some None
    | Some (Some nr) -> Some (Some (get_dt_node nr))

let get_t_child n =
  match n.t_child with
    | None -> None
    | Some None -> Some None
    | Some (Some nr) -> Some (Some (get_dt_node nr))

let print_node chan n =
  let kid_to_string mmn =
    match mmn with
      | None -> "unknown"
      | Some None -> "none"
      | Some(Some kid) -> string_of_int kid.ident
  in
    Printf.fprintf chan "%d: " n.ident;
    Printf.fprintf chan "%s " (kid_to_string (get_f_child n));
    Printf.fprintf chan "%s " (kid_to_string (get_t_child n));
    Printf.fprintf chan "%s " (if n.all_seen then "*" else "?");
    Printf.fprintf chan "%s " (kid_to_string (Some (get_parent n)));
    if n.heur_min <= n.heur_max then
      Printf.fprintf chan "(%d %d) " n.heur_min n.heur_max
    else
      Printf.fprintf chan "(X) ";
    Printf.fprintf chan "at 0x%08Lx " n.eip_loc;
    Printf.fprintf chan "%s\n"
      (match n.query_children with
	 | None -> "[]"
	 | Some k -> "[" ^ (string_of_int k) ^ "]")

let update_dt_node n =
  (* assert(n = string_to_dt_node n.ident (dt_node_to_string n)); *)
  if !opt_trace_decision_tree then
    (Printf.eprintf "DT: Writing back node %d\n" n.ident;
     print_node stdout n);
  if !opt_decision_tree_use_file then
    let i = n.ident in 
      (let off = Unix.lseek (nodes_fd ()) (54 * (i-1)) Unix.SEEK_SET in
	 g_assert(off = 54 * (i-1)) 100 "Binary_decision_tree.update_dt_node");
      let len = Unix.write (nodes_fd ()) (dt_node_to_string n ^ "\n") 0 54 in
	g_assert(len = 54) 100 "Binary_decision_tree.update_dt_node";
  else
    let i3 = n.ident land 1023 and
	i2 = (n.ident asr 10) land 1023 and
	i1 = n.ident asr 20
    in
      match ident_to_node_table.(i1) with
	| Some t2 ->
	    (match t2.(i2) with
	       | Some t3 ->
		   t3.(i3) <- dt_node_to_string n
	      | _ -> failwith "Node sub-table missing (t2)")
	| None -> failwith "Node sub-table missing (t1)"
	  
let new_dt_node the_parent =
  next_dt_ident := !next_dt_ident + 1;
  let i = !next_dt_ident in
    if not !opt_decision_tree_use_file then
      (let i3 = i land 1023 and
	   i2 = (i asr 10) land 1023 and
	   i1 = i asr 20
       in
       let t2 = match ident_to_node_table.(i1) with
	 | Some t -> t
	 | None ->
	     let t = Array.create 1024 None in
	       ident_to_node_table.(i1) <- Some t; t
       in
       let t3 = match t2.(i2) with
	 | Some t -> t
	 | None ->
	     let t = Array.create 1024 "" in
	       t2.(i2) <- Some t; t
       in
	 ignore(i3);
	 ignore(t3));
    let node =
      {parent = (match the_parent with
		   | None -> None
		   | Some nr -> Some (ref_dt_node nr));
       f_child = None; t_child = None;
       query_children = None; query_counted = false;
       heur_min = 0x3fffffff; heur_max = -1;
       all_seen = false; ident = i; eip_loc = 0L}
    in
      update_dt_node node;
      node

let put_parent n p =
  n.parent <-
    (match p with
      | None -> None
      | Some p' -> Some (ref_dt_node p'));
  update_dt_node n
	  
let put_f_child n k =
  n.f_child <-
    (match k with
       | None -> None
       | Some None -> Some None
       | Some (Some k') -> Some (Some (ref_dt_node k')));
  update_dt_node n

let put_t_child n k =
  n.t_child <-
    (match k with
       | None -> None
       | Some None -> Some None
       | Some (Some k') -> Some (Some (ref_dt_node k')));
  update_dt_node n

let put_eip_loc n loc =
  n.eip_loc <- loc;
  update_dt_node n

(* This hash algorithm is FNV-1a,
   c.f. http://www.isthe.com/chongo/tech/comp/fnv/index.html *)
let hash_round h x =
  let h' = Int32.logxor h (Int32.of_int x) in
    Int32.mul h' (Int32.of_int 0x1000193)

class binary_decision_tree = object(self)
  inherit Decision_tree.decision_tree

  val seen = Hashtbl.create 100

  val root_ident = (new_dt_node None).ident

  (* "cur" is the core changing state that points to the decision tree
     node corresponding to the current point on some execution path. On a
     typical path, it will start by traversing a path through the
     existing tree down from the root, and then it points to the newly
     created nodes on the new segment of a path. To satisfy OCaml's type
     system and intialization rules, we have to first initialize this
     field referring to a dummy node that will never be used. *)
  val mutable cur = new_dt_node None (* this initial value is garbage *)

  (* "cur_query" is similar to "cur", but it corresponds to the
     multi-choice structure of the tree, so it trails "cur" and skips
     over nodes with query_children = None. *)
  val mutable cur_query = new_dt_node None (* garbage *)

  val mutable depth = 0
  val mutable path_hash = Int64.to_int32 0x811c9dc5L
  val mutable iteration_count = 0
  val mutable randomness = Random.State.make [|!opt_random_seed; 0|]
  val mutable best_heur = -1
  val mutable cur_heur = -1

  method init =
    let root = get_dt_node root_ident in
      root.query_children <- Some 0;
      update_dt_node root;
      cur <- root;
      Hashtbl.replace seen cur.eip_loc cur;
      cur_query <- root;
      if !opt_trace_decision_tree then
	Printf.eprintf "DT: Initialized.\n";
(*
      let module Log = 
	    (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in
      Log.trace (Yojson_logger.LazyJson
		   (lazy
		      (`Assoc [
			"function", `String "init";
			"node", `Int cur.ident;
		      ])
		   )
      );
*)
      (self :> Decision_tree.decision_tree)

  method reset =
    cur <- get_dt_node root_ident;
    Hashtbl.replace seen cur.eip_loc cur;
    cur_query <- get_dt_node root_ident;
    cur_heur <- -1;
    depth <- 0;
    path_hash <- Int64.to_int32 0x811c9dc5L;
    iteration_count <- iteration_count + 1;
    Hashtbl.clear seen;
    if !opt_trace_randomness then
      Printf.eprintf "Initializing random state as %08x\n" iteration_count;
    randomness <- Random.State.make [|!opt_random_seed; iteration_count|]

  method get_hist =
    let child_is kid n =
      match kid with
	| Some (Some n') -> n'.ident = n.ident
	| _ -> false
    in
    let last_choice n p =
      if child_is (get_f_child p) n then
	false
      else if child_is (get_t_child p) n then
	true
      else
	failwith "Parent invariant failure in get_hist"
    in
    let rec loop n =
      match get_parent n with
	| None -> []
	| Some p -> (last_choice n p) :: loop p
    in
      loop cur

  method get_hist_str = 
    let append accum next = (bool_to_string next) ^ accum in
    List.fold_left append "" self#get_hist


  method private get_hist_queries =
    let kid n b =
      match ((get_f_child n), (get_t_child n), b) with
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
      g_assert(n.query_children <> None) 100 "Binary_decision_tree.get_hist_queries";
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
    List.rev (outer_loop [] (get_dt_node root_ident) hist)

  method get_hist_str_queries = 
    String.concat "-"
      (List.map
	 (fun q -> String.concat "" 
	    (List.map bool_to_string q))
	 self#get_hist_queries)

  method get_hist_str_bracketed = 
    String.concat ""
      (List.map
	 (fun q -> let s = String.concat ""
	    (List.map bool_to_string q) in
	    if String.length s = 1 then s else "[" ^ s ^ "]")
	 self#get_hist_queries)

  method get_depth = depth

  method private forget_unsat b =
    if !opt_trace_decision_tree then
      Printf.eprintf "DT: Forgetting unsat of %B child to %d\n" b cur.ident;
    (if b then put_t_child else put_f_child) cur None

  method add_kid b =
    if !opt_trace_decision_tree then
      Printf.eprintf "DT: Adding %B child to %d\n" b cur.ident;
    g_assert(not cur.all_seen) 100 "Binary_decision_tree.add_kid";
    let status =
    (match (b, (get_f_child cur), (get_t_child cur)) with
      | (false, Some(Some kid), _)
      | (true,  _, Some(Some kid)) ->
	"child already exists"
      | (false, None, _) ->
	let new_kid = new_dt_node (Some cur) in
	put_f_child cur (Some (Some new_kid));
	  "adding false child";
      | (true,  _, None) ->
	let new_kid = new_dt_node (Some cur) in
	put_t_child cur (Some (Some new_kid));
	"adding true child"
      | (false, Some None, _)
      | (true,  _, Some None) ->
	failwith "Tried to extend an unsat branch") in
    let module Log = 
	  (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in

    let childId = match status with
      | "child already exists" -> -1
      | "adding false child" ->
	(match cur.f_child with
	| Some Some cid -> cid
	| _ -> -1)
      | "adding true child" ->
	(match cur.t_child with
	| Some Some cid -> cid
	| _ -> -1)
      | _ -> failwith "Didn't match status string, shouldn't happen" in

    Log.trace (Yojson_logger.LazyJson
		 (lazy
		    (`Assoc [
		      "function", `String "add_kid";
		      "node", `Int cur.ident; 
		      "childId", `Int childId;
		      "childBranch", `Bool b;
		      "status", `String status;
		    ])
		 )
    ) 	  

  (* Calls to start_new_query(_binary)? and count_query should
     alternate along each execution path, delimiting the one or more
     binary choices that are treated as part of one multi-way choice. *)
  method start_new_query =
    (* A failure of the following assertion generally means that you
       did start_new_query, then created some new decision tree nodes,
       then called start_new_query again. You need to call count_query
       after the first decisions, and before calling start_new_query
       again. *)
    g_assert(cur.query_children <> None) 100 "Binary_decision_tree.start_new_query";
    if !opt_trace_decision_tree then
      Printf.eprintf "DT: New query, updating cur_query to cur %d\n" cur.ident;
    cur_query <- cur

  method start_new_query_binary =
    self#start_new_query;
    match cur_query.query_children with
      |	None | Some 0 | Some 1 | Some 2 -> ()
      | Some n ->
	  Printf.eprintf "Current query node is %d with %d children\n"
	    cur_query.ident n;
          self#print_dot;
	  failwith "Too many children in start_new_query_binary"

  method count_query =
    let rec finish_internal_nodes n top =
      if !opt_trace_decision_tree then
	Printf.eprintf "DT: Finish internal nodes at %d (%B)\n" n.ident top;
      if n.query_children = None || top then
	((match get_f_child n with
	    | Some(Some kid) -> finish_internal_nodes kid false
	    | Some None -> ()
	    | None -> put_f_child n (Some None));
	 (match get_t_child n with
	    | Some(Some kid) -> finish_internal_nodes kid false
	    | Some None -> ()
	    | None -> put_t_child n (Some None));
	 self#maybe_mark_all_seen_node n)
    in
      if !opt_trace_decision_tree then
	Printf.eprintf "DT: count_query at %d (q %d)" cur.ident cur_query.ident;
      if cur.ident <> cur_query.ident then 
	(if cur.query_children = None then
	   cur.query_children <- Some 0;
	 (match cur_query.query_children with
	    | None -> failwith "Count_query outside a query"
	    | Some k when not cur.query_counted ->
		if !opt_trace_decision_tree then	
		  Printf.eprintf " -> %d" (k+1);
		g_assert(k < !opt_query_branch_limit) 100 "Binary_decision_tree.count_query";
 		cur_query.query_children <- Some (k + 1);
		update_dt_node cur_query;
		cur.query_counted <- true
	    | Some k -> ());
	 update_dt_node cur;
	 if !opt_trace_decision_tree then	
	   Printf.eprintf "\n";
	 match cur_query.query_children with 
	   | Some k when k >= !opt_query_branch_limit ->
	       finish_internal_nodes cur_query true
	   | _ -> ())
      else
	if !opt_trace_decision_tree then	
	  Printf.eprintf "\n"

  method extend b =
    let module Log = 
	  (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in
    Log.trace (Yojson_logger.LazyJson
		 (lazy
		    (`Assoc [
		      "function", `String "extend";
		      "node", `Int cur.ident;
		      "heuristic_min", `Int cur.heur_min;
		      "heuristic_max", `Int cur.heur_max;
		      "child", `Bool b;
		    ])
		 )
    );
    if !opt_trace_decision_tree then
      Printf.eprintf "DT: Extending with %B at %d\n" b cur.ident;
    self#add_kid b;
    path_hash <- hash_round path_hash (if b then 49 else 48);
    (let h = Int32.to_int path_hash in
       (if !opt_trace_randomness then
	  Printf.eprintf "Setting random state to %08x\n" h;
	randomness <- Random.State.make [|!opt_random_seed; h|]));
    (match (b, get_f_child cur, get_t_child cur) with
       | (false, Some(Some kid), _) -> cur <- kid
       | (true,  _, Some(Some kid)) -> cur <- kid
       | (false, None, _)
       | (true,  _, None)
       | (false, Some None, _)
       | (true,  _, Some None) ->
	 failwith "Add_kid failed in extend");
    Hashtbl.replace seen cur.eip_loc cur;
    depth <- depth + 1;
    if (Int64.of_int depth) > !opt_path_depth_limit then
      raise DeepPath

  method set_iter_seed i =
    path_hash <- hash_round path_hash i

  method random_bit =
    let b = Random.State.bool randomness in
      if !opt_trace_randomness then
	Printf.eprintf "Flipping a coin to get %B\n" b;
      b

  method random_float =
    let f = Random.State.float randomness 1.0 in
      if !opt_trace_randomness then
	Printf.eprintf "Flipping a floating coin to get %f\n" f;
      f

  method random_byte =
    let b = Random.State.int randomness 256 in
      if !opt_trace_randomness then
	Printf.eprintf "Rolling a 256-sided die to get %d\n" b;
      b

  method random_word =
    let i = Random.State.int64 randomness 0x100000000L in
      if !opt_trace_randomness then
	Printf.eprintf "Flipping a 32-bit coin to get %Ld\n" i;
      i

  method record_unsat b =
    match (b, get_f_child cur, get_t_child cur) with
      | (false, None, _) ->
	  put_f_child cur (Some None)
      | (true,  _, None) ->
	  put_t_child cur (Some None)
      | (false, Some None, _)
      | (true,  _, Some None) -> () (* already recorded *)
      | (false, Some (Some _), _)
      | (true,  _, Some (Some _)) ->
	  failwith "Trying to make sat branch unsat in record_unsat"

  method try_extend (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) (random_bit_gen : unit -> bool)
    both_fail_func eip_loc =
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
	     let b' = both_fail_func b in
	       self#forget_unsat b';
	       self#extend b';
	       (b', trans_func b'))
    in
      g_assert(not cur.all_seen) 100 "Binary_decision_tree.try_extend";
      if cur.eip_loc <> 0L && cur.eip_loc <> eip_loc then
	(* For a discussion of why we have this check, and how to interpret
	   the .%04Lx values, see the comment before SPFM.eip_ident *)
	(Printf.eprintf
	   ("Decision tree inconsistency: node " ^^
	      "%d was 0x%08Lx.%04Lx and then %08Lx.%04Lx\n")
	   cur.ident
	   (Int64.shift_right cur.eip_loc 16) (Int64.logand cur.eip_loc 0xffffL)
	   (Int64.shift_right eip_loc 16) (Int64.logand eip_loc 0xffffL);
	 if Hashtbl.mem seen eip_loc then
	   Printf.eprintf "However, I have previously seen a node with that eip.\n"
	 else
	   (Printf.eprintf "And I've never seen a node with that eip.\n"); self#print_dot);
      g_assert (cur.eip_loc = 0L || cur.eip_loc = eip_loc) 100
	"Binary_decision_tree.try_extend 2";
      put_eip_loc cur eip_loc;
      let limited = match cur_query.query_children with 
	| Some k when k >= !opt_query_branch_limit -> true
	| _ -> false
      in
      if !opt_trace_decision_tree then	
	Printf.eprintf "try_extend at %d\n" cur.ident;
      match (get_f_child cur, get_t_child cur, limited) with
	| (Some(Some f_kid), Some(Some t_kid), _) ->
	    (match (f_kid.all_seen, t_kid.all_seen) with
	       | (true, true) -> 
		   if cur.all_seen then
		     known (random_bit_gen ())
		   else
		     (Printf.eprintf "Bug: kids %d and %d are all_seen, but not parent %d\n"
			f_kid.ident t_kid.ident cur.ident;
		      failwith "all_seen invariant failure")
	       | (false, true) -> known false
	       | (true, false) -> known true
	       | (false, false) -> known (random_bit_gen ()))
	| (Some(Some f_kid), Some None, _) ->
	    g_assert(not f_kid.all_seen) 100 "Binary_decision_tree.try_extend";
	    known false
	| (Some None, Some(Some t_kid), _) ->
	    g_assert(not t_kid.all_seen) 100 "Binary_decision_tree.try_extend";
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
      try_branch false (trans_func false) (get_f_child cur)
	(fun _ -> try_branch true (trans_func true) (get_t_child cur)
	   (fun _ -> failwith "Both branches unsat in try_extend"))
    else
      try_branch true (trans_func true) (get_t_child cur)
	(fun _ -> try_branch false (trans_func false) (get_f_child cur)
	   (fun _ -> failwith "Both branches unsat in try_extend"))

  method private mark_all_seen_node node =
    let mark n =
      (* check the invariant that a node can only be marked all_seen
	 if all of its children are. *)
      (match get_f_child n with
	 | Some(Some kid) ->
	     if not kid.all_seen then
	       (Printf.eprintf "all_seen invariant failure: parent %d is all seen, but not true child %d%!\n"
		  n.ident kid.ident;
		self#print_tree stdout;
		self#print_dot;
		g_assert(kid.all_seen) 100 "Binary_decision_tree.mark_all_seen_node");
	 | _ -> ());
      (match get_t_child n with
	 | Some(Some kid) ->
	     if not kid.all_seen then
	       (Printf.eprintf "all_seen invariant failure: parent %d is all seen, but not true child %d%!\n"
		  n.ident kid.ident;
		self#print_tree stdout;
		self#print_dot;
	       g_assert(kid.all_seen) 100 "Binary_decision_tree.mark_all_seen_node")
	 | _ -> ());
      n.all_seen <- true;
      if !opt_trace_decision_tree then	
	Printf.eprintf "Marking node %d as all_seen\n" n.ident;
      update_dt_node n
    in
    let rec internal_nodes_check n top =
      if n.all_seen then
	true
      else if n.query_children <> None && not top then
	false
      else if
	(match get_f_child n with
	   | Some(Some kid) -> internal_nodes_check kid false
	   | _ -> true)
	  &&
	(match get_t_child n with
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
	    match get_parent n with
	      | Some p -> query_parent p
	      | _ -> failwith "Missing query node parent"
    in
    let rec loop n = 
      mark n;
      match get_parent n with
	| None -> ()
	| Some p ->
	    (match (p.all_seen, (get_t_child p), (get_f_child p)) with
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
		      (Printf.eprintf "Node %d has excessive count %d\n"
			 q_parent.ident k;
		       g_assert(k = !opt_query_branch_limit) 100 "Binary_decision_tree.mark_all_seen_node"
		      );
		    if internal_nodes_check q_parent true then
		      loop q_parent
		| _ -> ()
    in
      if !opt_trace_decision_tree then	
	Printf.eprintf "DT: Mark_all_seen at %d\n" node.ident;
      loop node

  method private maybe_mark_all_seen_node n =
    if !opt_trace_decision_tree then	
      Printf.eprintf "DT: maybe_mark_all_seen_node at %d\n" n.ident;
    if (match get_f_child n with
	  | None -> false
	  | Some None -> true
	  | Some(Some kid) -> kid.all_seen)
      &&
	  (match get_t_child n with
	     | None -> false
	     | Some None -> true
	     | Some(Some kid) -> kid.all_seen)
    then
      self#mark_all_seen_node n

  method set_heur i =
(*    let module Log = 
	  (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in
    Log.trace (Yojson_logger.LazyJson
		 (lazy
		    (`Assoc [
		      "function", `String "set_heur";
		      "oldValue", `Int cur_heur;
		      "newValue", `Int i;
		    ])
		 )
    );
*)
    cur_heur <- i;
    if i > best_heur then
      best_heur <- i;
    self#propagate_heur cur

  method private propagate_heur node =
    let rec loop n = 
      if (cur_heur < n.heur_min || cur_heur > n.heur_max) then
	(n.heur_min <- min cur_heur n.heur_min;
	 n.heur_max <- max cur_heur n.heur_max;
	 update_dt_node n;
	 if !opt_trace_decision_tree then	
	   Printf.eprintf "DT: propagate_heur %d (%d %d) at %d\n" cur_heur
	     n.heur_min n.heur_max n.ident;
	 match get_parent n with
	   | None -> ()
	   | Some p -> loop p);
    in
      loop node

  method heur_preference =
    let setH = ref false and
	fMin = ref (-1) and
	fMax = ref (-1) and
	tMin = ref (-1) and
	tMax = ref (-1) in
(*    let module Log = 
	  (val !Loggers.fuzzball_bdt_json : Yojson_logger.JSONLog) in
*)
    let preference = (
    match (get_f_child cur, get_t_child cur) with
      | (Some Some kid_f, Some Some kid_t) ->
	  let f_min = kid_f.heur_min and
	      f_max = kid_f.heur_max and
	      t_min = kid_t.heur_min and
	      t_max = kid_t.heur_max in
	  setH := true;
	  fMin := f_min;
	  fMax := f_max;
	  tMin := t_min;
	  tMax := t_max;
	    if !opt_trace_guidance then
	      Printf.eprintf
		"Heuristic choice between F[%d, %d] and T[%d, %d] (cur %d)\n"
		f_min f_max t_min t_max cur_heur;
	    if f_min > f_max || t_min > t_max then
	      (* Only one side explored, no basis to choose *)
	      None
	    else if !opt_target_guidance >= 2.0 then
	      if !opt_target_guidance = 2.0 && cur_heur <= 1 then
		(* At 2.0, don't apply guidance before we have any
		   estimate of the value of this path *)
		None
	      (* Only prefer branches that lead to the best state(s)
		 we've ever seen *)
	      else if f_max = best_heur && f_max > t_max then
		Some false
	      else if t_max = best_heur && t_max > f_max then
		Some true
	      else
		None
	    else if f_max <> t_max then
	      ( (*Printf.eprintf "Preference based on max\n"; *)
	       Some (t_max > f_max))
	    (* else if f_min <> t_min then
	      ( (*Printf.eprintf "Preference based on min\n"; *)
	       Some (t_min > f_min)) *)
	    else
	      None
      | (Some Some _, None) -> Some false
      | (None, Some Some _) -> Some true
      | _ -> None
    ) in
(*    Log.trace (Yojson_logger.LazyJson
		 (lazy
		    (`Assoc [
		      "function", `String "heur_preference";
		      "consideredH", `Bool !setH;
		      "falseKid", `Assoc [
			"min", `Int !fMin;
			"max", `Int !fMax;
		      ];
		      "trueKid", `Assoc [
			"min", `Int !tMin;
			"max", `Int !tMax;
		      ];
		      "preference_opt", `Variant ("preference", 
						  (match preference with
						  | Some p -> Some  (`Bool p)
						  | None -> None
						  ));
		     ]
		    )
		 )
    );
*)
    preference

  method mark_all_seen =
    self#mark_all_seen_node cur;
    self#propagate_heur cur

  method try_again_p = not (get_dt_node root_ident).all_seen

  method check_last_choices =
    match get_parent cur with
      | None -> failwith "Missing parent in check_last_choices"
      | Some p ->
	  (match ((get_f_child p), (get_t_child p)) with
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
      match ((get_f_child cur), (get_t_child cur)) with
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
	Printf.eprintf "DT: at %d, have_choice is %b\n" cur.ident result;
      result

  method cur_ident =
    cur.ident

  method is_live_ident i =
    let node = ident_to_node i in
      not node.all_seen

  method cur_can_reach_ident i =
    let rec loop n =
      match get_parent n with
	| None -> None
	| Some p when p == cur ->
	    (match ((get_f_child cur), (get_t_child cur)) with
	       | (Some Some f_kid, _) when f_kid == n -> Some false
	       | (_, Some Some t_kid) when t_kid == n -> Some true
	       | _ -> 
		   failwith "Parent invariant failure in cur_can_reach_ident")
	| Some p -> loop p
    in
    let dir = loop (ident_to_node i)
    in
      dir

  method measure_size =
    !next_dt_ident    

  method print_dot = 
    let rec viz_node fd n =  
      let id = n.ident in
      let eip = Int64.shift_right_logical n.eip_loc 16 in
      let ident = Int64.logand 0x000000000000ffffL n.eip_loc in 
      let style = 
        (if (Int64.logand ident 0xc000L) = 0xc000L then "style=filled fillcolor=yellow"
         else "")
      in
        Printf.fprintf fd "%d [label=\"%d:0x%Lx (0x%Lx)\" %s];\n" id id eip ident style;
        (match get_f_child n with 
           | Some(Some f) -> (Printf.fprintf fd "%d -> %d [label = \"false\"];\n" id f.ident ;viz_node fd f)
           | _ -> ());
        (match get_t_child n with 
           | Some(Some t) -> (Printf.fprintf fd "%d -> %d [label = \"true\"];\n" id t.ident ;viz_node fd t)
           | _ -> ());
    in
      if not (!opt_save_decision_tree_dot = "") then
        (let fd = open_out !opt_save_decision_tree_dot in
           Printf.fprintf fd "digraph G {\n";
           let root = get_dt_node root_ident in
             viz_node fd root;
             Printf.fprintf fd "}\n";
             close_out fd)      

  method print_tree chan =
    let rec loop n =
      print_node chan n;
      (match get_f_child n with
	 | Some(Some kid) -> loop kid
	 | _ -> ());
      (match get_t_child n with
	 | Some(Some kid) -> loop kid
	 | _ -> ());
    in
      loop (get_dt_node root_ident)
end

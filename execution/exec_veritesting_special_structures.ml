module V = Vine

open Exec_veritesting_general_search_components

exception NotDag


let find_linear_region ?maxdepth:(maxdepth = 10) root expansion =
  (* Note, there must always be a linear veritesting region of at least
     one node in length.*)
  let add_child parent child =
    if check_cycle child parent
    then None
    else 
      match child with
      | Completed c ->
	(match c with
	| Branch b -> (truncate_node parent;
		       None)
	| _ -> Some child)
      | _ -> Some child in
  let expand node =
    let children = expansion node in
    match children with
    | [child] -> add_child node child
    | []
    | _::_ -> (truncate_node node; None) in
  let rec loop node =
    if (depth node) = maxdepth
    then (truncate_node node;
	  Some root)
    else
      match expand node with
      | None -> Some root
      | Some next -> loop next in
  loop root


let detect_diamond ?max_depth:(max_depth = 100) root expansion =
  (* Find a diamond of at most size max_depth.  If that structure
     exists, return its root. Otherwise, return none.*)
  let closed = Hashtbl.create 100 in
  let add_child parent accum child =
    try
      let prev = Hashtbl.find closed (key child) in
(*      Printf.eprintf "Found additional path to %Lx\n" (eip_of_node child);
	Printf.eprintf "%s vs %s\n" (node_to_string prev) (node_to_string child);
	Printf.eprintf "Truncating Child."; *)
      replace_child child prev;
      truncate_node prev;
(*      Printf.eprintf "Checking cycles\n"; *)
      if (check_cycle child parent) then
	raise NotDag else
	accum
    with Not_found -> child::accum in
  let expansion node =
    if not (Hashtbl.mem closed (key node)) then
      begin
	Hashtbl.add closed (key node) node;
	if (depth node) < max_depth then
	  begin
	    let children = List.fold_left (add_child node) [] (expansion node) in
(*	    Printf.eprintf "%Lx generates " (eip_of_node node);
	    List.iter (fun c -> Printf.eprintf "%Lx\t" (eip_of_node c)) children;
	    Printf.eprintf "\n";   *)
	    children
	  end
	else []
      end 
    else
      begin
(*	Printf.eprintf "Truncating %s\n" (node_to_string node); *)
	truncate_node node;
	[]
      end
  in
  let rec loop saw_branch = function 
    | [] ->
      begin
(*      Printf.eprintf "Open list has 0 elements.\n";
	print_tree root; *)
	match unique_endpoint root with
	| Some terminal -> (* Here we establish the shallowest terminal in the search *)
	  begin
(*	    Printf.eprintf "I think %s is the terminal.\n" (node_to_string terminal);  *)
	    truncate_node terminal;
	    Some root
	 end
	| None -> None
      end
    | head::tail ->
      begin
      let to_add = expansion head in
      loop (saw_branch || (List.length to_add) = 2)
	(List.rev_append tail to_add)end
  in
  loop false [root]


(* perhaps better idea for diamond: label all edges by branch count, then when you find a matching 
   node (same branch numbers && same EIP), you're done. regress up both sides for common root. 
   otherwise, as it is now, the common root will _always_ have to be the root we're searching from. *)

(* currently we cannot find > two paths that lead to same spot; just two. *)
   

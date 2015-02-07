module V = Vine

open Exec_veritesting_general_search_components


let find_linear_region ?maxdepth:(maxdepth = 100) root expansion =
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
  let rec loop depth node =
    if (depth = maxdepth)
    then (truncate_node node)
    else
      match expand node with
      | None -> ()
      | Some next -> 
	(match next with
	| Undecoded _ -> loop (depth + 1) next
	| _ -> loop depth next) in
  loop ~-1 root;
  Some root

let detect_diamond ?max_depth:(max_depth = 2) root expansion =
  (* Find a diamond of at most size max_depth.  If that structure
     exists, return its root. Otherwise, return none.*)
  let closed = Hashtbl.create 100 in
  let add_child parent accum child =
    try
      let prev = Hashtbl.find closed (key child) in
      replace_child child prev;
      if (check_cycle child parent)
      then accum (* this isn't DAG any more, do I want to bail? *)
      else accum
    with Not_found ->
      (Hashtbl.add closed (key child) child;
       child::accum) in
  let expansion node =
    let children = List.fold_left (add_child node) [] (expansion node)in
    assert (2 >= List.length children);
    children in
  let rec loop it = function 
    | [] -> None
    | head::tail as openlist ->
      if (it = max_depth)
      then
	begin
	  if ((List.length openlist) = 1)
	  then Some root
	  else None
	end
      else (match head with
      | Undecoded _ -> loop (it + 1) (List.rev_append tail (expansion head))
      | _ -> loop it (List.rev_append tail (expansion head))) in
  Hashtbl.add closed (key root) root;
  loop 0 [root]

let detect_diamond_lopsided ?max_depth:(max_depth = 2) root expansion =
  (* Find a diamond of at most size max_depth.  If that structure
     exists, return its root. Otherwise, return none. *)
  let closed = Hashtbl.create 100 in
  let add_child parent accum child =
    try
      let prev = Hashtbl.find closed (key child) in
      replace_child child prev;
      if (check_cycle child parent)
      then accum (* this isn't DAG any more, do I want to bail? *)
      else accum
    with Not_found ->
      (Hashtbl.add closed (key child) child;
       child::accum) in
  let expansion node =
    let children = List.fold_left (add_child node) [] (expansion node) in
    assert (2 >= List.length children);
    children in
  let rec loop it = function 
    | [] -> None
    | head::tail as openlist ->
      if (it = max_depth)
      then
	begin
	  if ((List.length openlist) = 1)
	  then Some root
	  else None
	end
      else if ((List.length openlist) > 2)
      then None (* Branches means this is not a clean diamond. *)
      else if ((Hashtbl.mem closed (key head)))
      then
        begin
          let same_eip_node = Hashtbl.find closed (key head) in
          match same_eip_node with
          | Completed c ->
             (match c with
              | Segment s -> 
                 s.p_core.next <- None;
                 ();
              (*| Branch b ->
                 () (* JDB: FIXME: I think we might want something here...*) *)
              | _ -> ());
             Some root (* Lopsided diamond *)
          | _ -> Some root
        end
      else (match head with
      | Undecoded _ -> loop (it + 1) (List.rev_append tail (expansion head))
      | _ -> loop it (List.rev_append tail (expansion head))) in
  Hashtbl.add closed (key root) root;
  loop 0 [root]

(* perhaps better idea for diamond: label all edges by branch count, then when you find a matching 
   node (same branch numbers && same EIP), you're done. regress up both sides for common root. 
   otherwise, as it is now, the common root will _always_ have to be the root we're searching from. *)

(* currently we cannot find > two paths that lead to same spot; just two. *)
   

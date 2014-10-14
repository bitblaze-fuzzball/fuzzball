open Exec_veritesting_general_search_components

let depth_first_search ?max_depth:(max_depth = max_int) root expansion =
  let closed = Hashtbl.create 100 in
  let add_child parent accum data =
    let child = 
      { data = data;
	parent = [parent];
	children = []; } in
    try
      let prev = Hashtbl.find closed (key child) in
      if (check_cycle child parent)
      then accum (* this isn't DAG any more, do I want to bail? *)
      else
	begin
	  prev.parent <- parent::prev.parent;
	  accum
	end
    with Not_found ->
      (Hashtbl.add closed (key child) child;
       child::accum) in
  let expansion node =
    assert(node.children = []); (* not yet expanded *)
    node.children <-
      List.fold_left (add_child node) [] (expansion node.data);
    assert (2 >= List.length node.children);
    node.children in
  let rec loop depth node =
    if (depth > max_depth)
    then ()
    else List.iter (loop (depth + 1)) (expansion node) in
  let rec root_node =
    { data = root;
      parent = [];
      children = []; } in 
  Hashtbl.add closed (key root_node) root_node;
  loop 0 root_node;
  root_node

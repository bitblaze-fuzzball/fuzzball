open Exec_veritesting_general_search_components

let breadth_first_search ?max_it:(max_it = max_int) root expansion =
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
    node.children in
  let rec loop it = function 
    | [] -> ()
    | head::tail ->
      if (it > max_it)
      then ()
      else loop (it + 1) (List.rev_append tail (expansion head)) in
  let rec root_node =
    { data = root;
      parent = [];
      children = []; } in 
  Hashtbl.add closed (key root_node) root_node;
  loop 0 [root_node];
  root_node

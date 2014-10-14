module V = Vine
module VOpt = Vine_opt

open Exec_veritesting_general_search_components


let find_linear_region ?maxdepth:(maxdepth = 2) root expansion =
  (* Note, there must always be a linear veritesting region of at least
     one node in length.*)
  let closed = Hashtbl.create 100 in
  let add_child parent data =
    let child = 
      { data = data;
	parent = [parent];
	children = []; } in
    if Hashtbl.mem closed (key child)
    then None (* the parent is the exit, as it is the last node before a cycle *)
    else begin (* the child can be processed *)
      assert([] = parent.children);
      parent.children <- [child];
      Some child
    end in
  let expand node =
    assert(node.children = []);
    let children = expansion node.data in
    match children with
    | [child] -> add_child node child
    | []
    | _::_ -> None in
  let rec loop depth node =
    if (depth = maxdepth)
    then make_exit node
    else
      match expand node with
      | None -> None
      | Some next -> loop (depth + 1) next in
  let rec root_node =
    { data = root;
      parent = [];
      children = []; } in 
  ignore(loop 0 root_node);
  Some root_node


let detect_diamond ?max_depth:(max_depth = 2) root expansion =
  (* Find a diamond of at most size max_depth.  If that structure
     exists, return its root. Otherwise, return none.*)
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
  let rec root_node =
    { data = root;
      parent = [];
      children = []; } in 
  let rec loop it = function 
    | [] -> None
    | head::tail as openlist ->
      if (it = max_depth)
      then
	begin
	  if ((List.length openlist) = 1)
	  then Some root_node
	  else None
	end
      else loop (it + 1) (List.rev_append tail (expansion head)) in
  Hashtbl.add closed (key root_node) root_node;
  loop 0 [root_node]

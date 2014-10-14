open Exec_veritesting_general_search_components

let breadth_first_search ?max_it:(max_it = max_int) root expansion =
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
    | [] -> ()
    | head::tail ->
      if (it > max_it)
      then ()
      else loop (it + 1) (List.rev_append tail (expansion head)) in
  Hashtbl.add closed (key root) root;
  loop 0 [root];
  root

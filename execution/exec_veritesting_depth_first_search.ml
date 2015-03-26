open Exec_veritesting_general_search_components
open Exec_assert_minder

let depth_first_search ?max_depth:(max_depth = max_int) root expansion =
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
    g_assert (2 >= List.length children) 100 "Exec_veritesting_depth_first_search";
    children in
  let rec loop depth node =
    if (depth > max_depth)
    then ()
    else List.iter (loop (depth + 1)) (expansion node) in
  Hashtbl.add closed (key root) root;
  loop 0 root;
  root

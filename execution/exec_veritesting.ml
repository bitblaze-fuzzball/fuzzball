open Exec_veritesting_general_search_components
module BFS = Exec_veritesting_breadth_first_search
module DFS = Exec_veritesting_depth_first_search
module V = Vine
module Encode = Exec_encode_veritesting_region

type supported_searches =
| BFS
| DFS

let find_veritesting_region search fm gamma starting_eip max_depth =
  let root_of_region =
    match search with
    | BFS ->
      BFS.breadth_first_search ~max_it:max_depth
        (Instruction { eip = starting_eip;
  		       vine_stmts = []; })
	(expand fm gamma)
    | DFS ->
      DFS.depth_first_search ~max_depth:max_depth
        (Instruction { eip = starting_eip;
  		       vine_stmts = []; })
	(expand fm gamma)
  in
  let rec print_region ?offset:(offset = 1) node =
    (for i=1 to offset do Printf.printf "\t" done);
    Printf.printf "%s\n" (node_to_string node);
    List.iter (print_region ~offset:(offset + 1)) node.children in
  print_region root_of_region;
  Printf.printf "\n";
  V.stmt_to_channel stdout (V.ExpStmt (Encode.build_equations root_of_region));
  Printf.printf "\n"

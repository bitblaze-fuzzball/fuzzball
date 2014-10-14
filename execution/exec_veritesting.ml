open Exec_veritesting_general_search_components
open Exec_veritesting_breadth_first_search

type supported_searches = BFS

let find_veritesting_region search fm gamma starting_eip max_depth =
  let root_of_region =
    (match search with
    | BFS ->
      breadth_first_search ~max_it:max_depth
        { entry_point = starting_eip;
  	vine_stmts = [];}
        (expand fm gamma)
        key) in
  let rec print_region ?offset:(offset = 1) node =
    (for i=1 to offset do Printf.printf "\t" done);
    Printf.printf "%Li\n" node.data.entry_point;
    List.iter (print_region ~offset:(offset + 1)) node.children in
  print_region root_of_region
open Exec_veritesting_general_search_components
open Exec_run_common

module BFS = Exec_veritesting_breadth_first_search
module DFS = Exec_veritesting_depth_first_search
module V = Vine
module Encode = Exec_encode_veritesting_region
module VSS = Exec_veritesting_special_structures

type supported_searches =
| BFS
| DFS
| Linear
| Diamond of int


let rec print_region ?offset:(offset = 1) node =
  (for i=1 to offset do Printf.printf "\t" done);
  Printf.printf "%s\n" (node_to_string node);
  List.iter (print_region ~offset:(offset + 1)) node.children

let make_root fm gamma eip =
  let decode_call _ = decode_insns fm gamma eip 1 in
  let decls, stmts = with_trans_cache eip decode_call in
  Instruction { eip = eip;
		test = None;
		vine_stmts = stmts;
		vine_decls = decls; }

let find_veritesting_region search fm gamma starting_eip max_depth =
  let search_root =  make_root fm gamma starting_eip
  and expand = expand fm gamma in
  let root_of_region =
    match search with
    | BFS ->
      Some (BFS.breadth_first_search ~max_it:max_depth
	      search_root expand)
    | DFS ->
      Some (DFS.depth_first_search ~max_depth:max_depth
	      search_root expand)
    | Linear -> VSS.find_linear_region search_root expand
    | Diamond size -> VSS.detect_diamond ~max_depth:size search_root expand
  in

  match root_of_region with
  | None -> None
  | Some root_of_region ->
    print_region root_of_region;
    Printf.printf "\n";
    let decls, stmts = Encode.encode_region root_of_region in
    List.iter (fun s -> V.stmt_to_channel stdout s) stmts;
    Printf.printf "\n";
    Some (decls, stmts)
      

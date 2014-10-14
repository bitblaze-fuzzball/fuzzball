open Exec_veritesting_general_search_components
open Exec_run_common

module EO = Exec_options
module BFS = Exec_veritesting_breadth_first_search
module DFS = Exec_veritesting_depth_first_search
module V = Vine
module Encode = Exec_encode_veritesting_region
module VSS = Exec_veritesting_special_structures


let find_veritesting_region fm gamma starting_eip max_depth =
  let search_root =  make_root starting_eip
  and expand = expand fm gamma in
  let root_of_region =
    match !EO.opt_veritesting with
    | EO.NoVeritesting -> None
    | EO.BFS ->
      Some (BFS.breadth_first_search ~max_it:max_depth
	      search_root expand)
    | EO.DFS ->
      Some (DFS.depth_first_search ~max_depth:max_depth
	      search_root expand)
    | EO.Linear -> VSS.find_linear_region search_root expand
    | EO.Diamond size -> VSS.detect_diamond ~max_depth:size search_root expand
  in
  
  match root_of_region with
  | None -> None
  | Some root_of_region ->
    print_region root_of_region;
    let stmts, decls = Encode.encode_region root_of_region in
    Printf.printf "Printing statement list:\n";
    List.iter (fun s -> V.stmt_to_channel stdout s) stmts;
    Printf.printf "End statement list\n";
    Some (decls, stmts)

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
    | EO.Diamond size -> 
      begin
(*	Printf.eprintf "Detecting Diamond Region of size %i\n" size;
*)
	VSS.detect_diamond ~max_depth:size search_root expand
      end
  in
  
  match root_of_region with
  | None -> None
  | Some root_of_region ->
    (*try*)
    begin
(*      Printf.eprintf "Detected region, encoding starting from %s...\n" (node_to_string root_of_region);*)
      Encode.encode_region root_of_region
    end

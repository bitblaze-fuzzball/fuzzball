module V = Vine
module VOpt = Vine_opt
module Search = Exec_veritesting_general_search_components

(*

  Just a couple of notes on the translation:

  It will likely be faster if we go bottom up (that is, exits first).
  Common subexpression caching works best with that direction of an
  approach.  It's akin to dynamic programming.

  For truly big regions, we could blow the stack in build equations.
  Rewriting the code with a heap based stack insted of the stack being
  the call stack should be easy enough.

*)


let no_data = [],[]

let make_exit eip =
  [V.Jmp 
      (V.Name
	    (Printf.sprintf "pc_0x%Lx" eip))],
  []

let data_of_ft (ft : Search.veritesting_node Search.finished_type) =
  match ft with
  | Search.ExternalLoop eip
  | Search.InternalLoop eip
  | Search.Return eip
  | Search.Halt eip
  | Search.FunCall eip
  | Search.SysCall eip
  | Search.Special eip
  | Search.SearchLimit eip -> make_exit eip
  | Search.Branch _ -> failwith "stub" 
  | Search.Segment s -> s.Search.stmts, s.Search.decls


let data_of_node (n : Search.veritesting_node) =
  match n with
  | Search.Undecoded _
  | Search.Raw _ -> no_data
  | Search.Completed ft -> data_of_ft ft


let build_simplest_equations root =
  let stmt_accum = ref []
  and decl_accum = ref [] in
  let rec internal node =
    let stmts,decls as this_data = data_of_node node in
    stmt_accum := stmts::!stmt_accum;
    decl_accum := decls::!decl_accum;
    match Search.successor node with
    | None -> ()
    | Some s -> internal s in
  internal root;
  List.concat (List.rev !stmt_accum), List.concat (List.rev !decl_accum)
  

let encode_region root =
  build_simplest_equations root
  

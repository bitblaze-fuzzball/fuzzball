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

type branch_move = {
  var : V.lvalue;
  true_val : V.exp option;
  false_val : V.exp option;
}

exception BranchMerge of string

let branch_move_to_ite (bmove : branch_move) test =
  match bmove.true_val, bmove.false_val with
  | Some tv, Some fv ->
    V.Move (bmove.var, (V.Ite (test, tv, fv)))
  | Some tv, None -> V.Move (bmove.var, (V.Ite (test, tv, V.Lval bmove.var)))
  | None, Some fv -> V.Move (bmove.var, (V.Ite (test, V.Lval bmove.var, fv)))
  | None, None -> raise (BranchMerge "branch_move_to_ite: Move isn't in either branch. What am I merging?")


let merge_diamond test true_path false_path =
  (*we're looking for the moves. *)
  let stmts = Hashtbl.create 100
  and decls = (snd true_path) @ (snd false_path)
  and kv_to_v _ value accum = (branch_move_to_ite value test)::accum in
  let rec stmt_helper side = function
    | [] -> ()
    | hd::tl ->
      begin
	(match hd with 
	| V.Move (lvalue, exp) ->
	  begin
	    try
	      let prev = Hashtbl.find stmts lvalue in
	      if side then
		Hashtbl.replace stmts lvalue {var = lvalue; true_val = Some exp; false_val = prev.false_val;} else
		Hashtbl.replace stmts lvalue {var = lvalue; false_val = Some exp; true_val = prev.false_val;}
	    with Not_found ->
	      if side then
		Hashtbl.add stmts lvalue {var = lvalue; true_val = Some exp; false_val = None;} else
		Hashtbl.add stmts lvalue {var = lvalue; true_val = None; false_val = Some exp;}
	  end
	| _ -> ());
	stmt_helper side tl
      end in
  stmt_helper true (fst true_path);
  stmt_helper false (fst false_path);
  Hashtbl.fold kv_to_v stmts [], decls

                                   
(** I'd really rather not have the next three functions be mutually
    recursive, but we need to be able to get data from regions that
    are part of a branching structure being considered for
    veritesting. There's no performance hit (that I know of), it's
    just needlessly complicated. *)
let rec recover_diamond (ft : Search.veritesting_node Search.finished_type) =
  (* Printf.eprintf "Recovering diamond...\n"; *)
  let tuple_append (dl,sl) (dl',sl') = dl@dl', sl@sl' in
  let rec helper true_node false_node true_accum false_accum =
(*  Printf.eprintf "True: %s\tFalse: %s\n" (Search.node_to_string true_node) (Search.node_to_string false_node);*)
    if (Search.eip_of_node true_node) = (Search.eip_of_node false_node) then
      begin
	(* Printf.eprintf "Diamond converges at %Lx\n" (Search.eip_of_node true_node);*)
	true_accum, false_accum, true_node
      end else 
      begin
	match Search.successor true_node, Search.successor false_node with
	| Some t, Some f ->
	  helper t f 
	    (tuple_append (data_of_node true_node) true_accum)
	    (tuple_append (data_of_node false_node) false_accum)
	| Some t, None -> helper t false_node (tuple_append (data_of_node true_node) true_accum) false_accum
	| None, Some f -> helper true_node f true_accum (tuple_append (data_of_node false_node) false_accum)
	| _ -> raise (BranchMerge ":recover_diamond: Diamond didn't rejoin")
      end in
  match ft with
  | Search.Branch b ->
    let true_accum, false_accum, merge_point = helper b.Search.true_child b.Search.false_child ([],[]) ([],[]) in
    tuple_append (merge_diamond b.Search.test true_accum false_accum) (make_exit (Search.eip_of_node merge_point))
  | _ -> raise (BranchMerge "recover_diamond: not a branch, how did you call this?")


and data_of_ft (ft : Search.veritesting_node Search.finished_type) =
  match ft with
  | Search.ExternalLoop eip
  | Search.InternalLoop eip
  | Search.Return eip
  | Search.Halt eip
  | Search.FunCall eip
  | Search.SysCall eip
  | Search.Special eip
  | Search.SearchLimit eip -> make_exit eip
  | Search.Branch _ -> 
    begin
    (* Printf.eprintf "Merging branch...\n"; *)
    recover_diamond ft
    end
  | Search.Segment s -> s.Search.stmts, s.Search.decls


and data_of_node (n : Search.veritesting_node) =
  match n with
  | Search.Undecoded _
  | Search.Raw _ -> no_data
  | Search.Completed ft -> data_of_ft ft


let build_simplest_equations root =
  let stmt_accum = ref []
  and decl_accum = ref [] in
  let rec internal node =
    let stmts,decls = data_of_node node in
    stmt_accum := stmts::!stmt_accum;
    decl_accum := decls::!decl_accum;
    match Search.successor node with
    | None -> ()
    | Some s -> 
      begin
	(* Printf.eprintf "%s -> %s\n" (Search.node_to_string node) (Search.node_to_string s);*)
      internal s
      end in
  internal root;
  let stmts = List.concat (List.rev !stmt_accum)
  and decls = List.concat (List.rev !decl_accum) in
  (*
    Printf.eprintf "Beginning of translation:\n";
    List.iter (fun s -> Vine.stmt_to_channel stderr s) stmts;
    Printf.eprintf "End of translation\n";
    flush stderr;
  *)
  (* JTT - 12/2/15 this is horrible
     However, if there is only one statement in the list, it's the statement
     that jumps to the next / current instruction.  This causes an infinite loop.
     
     Ideally, we'd notice this before doing any encoding -- there aren't going to be any segments
     in a trace producing such a statement list.
  *)
  if (List.length stmts) = 1 then
    None else
    Some (decls, stmts)


let encode_region root =
  (* Printf.eprintf "\n\nEncoding Veritesting Region:\n"; *)
  (* Search.print_tree root;*)
  build_simplest_equations root    

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

let exp_of_lvalue = function
  | V.Temp (id, name, typ) ->
    (* This is probably wrong, but it's a positing of what to do when a move on a loacl variable appears in one branch and not
       the other.
       (Printf.eprintf "I did something dangerous with a local var.\n";
       match typ with 
       | V.REG_1 -> Vine.exp_true
       | V.REG_8
       | V.REG_16
       | V.REG_32
       | V.REG_64 -> (Vine.const_of_int typ 0)
       | V.TString
       | V.TMem _
       | V.TFun _
       | V.Array _
       | V.TAttr _ -> failwith "I give up.")*)
    raise (BranchMerge "exp_of_lvalue: There was a move on a local variable unique to one branch; unsure how to merge.")
  | V.Mem (var, exp, typ) -> exp


let branch_move_to_ite (bmove : branch_move) test =
  match bmove.true_val, bmove.false_val with
  | Some tv, Some fv ->
    V.Move (bmove.var, (V.Ite (test, tv, fv)))
  | Some tv, None -> V.Move (bmove.var, (V.Ite (test, tv, exp_of_lvalue bmove.var)))
  | None, Some fv -> V.Move (bmove.var, (V.Ite (test, exp_of_lvalue bmove.var, fv)))
  | _ -> raise (BranchMerge "branch_move_to_ite: Don't know what to do when move isn't in both branches.")


let merge_diamond test (true_path, false_path) =
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
	      if side
	      then Hashtbl.replace stmts lvalue {var = lvalue; true_val = Some exp; false_val = prev.false_val;}
	      else Hashtbl.replace stmts lvalue {var = lvalue; false_val = Some exp; true_val = prev.false_val;}
	    with Not_found ->
	      if side
	      then Hashtbl.add stmts lvalue {var = lvalue; true_val = Some exp; false_val = None;}
	      else Hashtbl.add stmts lvalue {var = lvalue; true_val = None; false_val = Some exp;}
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
(*  Printf.eprintf "Recovering diamond...\n";*)
  let tuple_append (dl,sl) (dl',sl') = dl@dl', sl@sl' in
  let rec helper true_node false_node true_accum false_accum =
    if Search.equal true_node false_node
    then (Printf.eprintf "Diamond converges at %Lx\n" (Search.eip_of_node true_node);
	  true_accum, false_accum)
    else 
      begin
	let next_true = Search.successor true_node
	and next_false = Search.successor false_node in
	match next_true, next_false with
	| Some t, Some f ->
	  helper t f 
	    (tuple_append (data_of_node true_node) true_accum)
	    (tuple_append (data_of_node false_node) false_accum)
	| Some t, None -> helper t false_node (tuple_append (data_of_node true_node) true_accum) false_accum
	| None, Some f -> helper true_node f true_accum (tuple_append (data_of_node false_node) false_accum)
	| _ -> raise (BranchMerge ":recover_diamond: Diamond didn't rejoin")
      end in
  match ft with
  | Search.Branch b -> merge_diamond b.Search.test (helper b.Search.true_child b.Search.false_child ([],[]) ([],[]))
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
  | Search.Branch _ -> (Printf.eprintf "Merging branch...\n"; recover_diamond ft)
  | Search.Segment s -> s.Search.stmts, s.Search.decls


and data_of_node (n : Search.veritesting_node) =
  match n with
  | Search.Undecoded _
  | Search.Raw _ ->
    begin
      Printf.eprintf "%s has no associated data, moving to next node\n" (Search.node_to_string n);
      no_data
    end
  | Search.Completed ft -> data_of_ft ft


let build_simplest_equations root =
  let stmt_accum = ref []
  and decl_accum = ref [] in
  let rec internal node =
    let stmts,decls = data_of_node node in
    stmt_accum := stmts::!stmt_accum;
    decl_accum := decls::!decl_accum;
    match Search.successor node with
    | None -> Printf.eprintf "%s has no successors, returning.\n" (Search.node_to_string node);
    | Some s -> 
      begin
	Printf.eprintf "%s -> %s\n" (Search.node_to_string node) (Search.node_to_string s);
      internal s
      end in
  internal root;
  let stmts = List.concat (List.rev !stmt_accum)
  and decls = List.concat (List.rev !decl_accum) in
  (*List.iter (fun s -> Vine.stmt_to_channel stderr s) stmts;*)
  decls, stmts



let encode_region root =
  Printf.eprintf "\n\nEncoding Veritesting Region:\n";
  Search.print_tree_statements root;
  if true (* set to false to turn on the guards around veritesting *)
  then
    Some (build_simplest_equations root)
  else
  try
    Some (build_simplest_equations root)
  with _ -> None
    

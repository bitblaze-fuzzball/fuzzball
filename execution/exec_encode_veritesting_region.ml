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


type exp_of_stmt_return =
| Nothing
| Expression of V.exp
| ContextUpdate of V.lvalue * V.exp

let cached_statments = Hashtbl.create 50
and common_expressions = Hashtbl.create 50
and region_decls = Hashtbl.create 50


let clear_caches _ =
  (** Some of the work can be saved between calls to encode the
      regions (I think).  In particular, translations of statement
      lists should be equivalent between all calls to walk statement
      list.  Contexts may mess this up.

      At any rate, repeatedly clearing the hashtables rather than
      reconstructing them for every call to encode region is cheaper
      because we don't have to regrow the tables every time.  **)
  Hashtbl.clear cached_statments;
  Hashtbl.clear common_expressions;
  Hashtbl.clear region_decls


let update_context context lvalue assignment =
  let rec helper = function 
    | [] -> [lvalue, assignment] (* hit the end without update, add. *)
    | hd::tl ->
      if (fst hd) = lvalue       (* update this context *)
      then (lvalue, assignment) :: tl
      else hd :: (helper tl) in  (* not our target, handle remaining list *)
  helper context


let rec context_of_decl_list context = function
    | [] -> []
    | decl::tl ->
      (context_of_decl_list
	 (update_context context (V.Temp decl) (V.Name "Foo"))
	 tl)


let rec handle_context context = function
  | V.Let (lvalue, e1, e2) -> handle_context (update_context context lvalue e1) e2
  | V.Lval lvalue -> List.assoc lvalue context
  | V.BinOp (typ, e1, e2) -> V.BinOp (typ,
				      (handle_context context e1),
				      (handle_context context e2))
  | V.UnOp (typ , e1) -> V.UnOp (typ, (handle_context context e1))
  | exp -> exp


let rec exp_of_stmt context = function
  | V.Jmp exp 
  | V.ExpStmt exp 
  | V.Assert exp
  | V.Halt exp -> Expression exp
  | V.Label label -> Expression (V.Name label)
  | V.Special string -> failwith "Don't know what to do with magic"
  | V.Move (lval, exp) -> ContextUpdate (lval, exp)
  | V.Comment string -> Nothing
  | V.Block (decls, stmts) -> Expression 
    (walk_statement_list (context_of_decl_list context decls) stmts)
  | V.Attr (stmt, _) -> exp_of_stmt context stmt
  | _ -> assert false

and walk_statement_list context list =
  VOpt.simplify (wsl_int context list)

and wsl_int context = function 
  | [] ->  V.exp_true
  | head::tail ->
    begin
      match exp_of_stmt context head with
      | Nothing -> wsl_int context tail
      | Expression exp ->  V.BinOp (V.BITAND, exp, wsl_int context tail)
      | ContextUpdate (lval, exp) ->
	begin
	  wsl_int (update_context context lval exp) tail
	end 
    end
    

let stmts_of_data = function
  (** most vine statements just break out of the veritesting region, so
      we encode these as the JMP statement to the first instruction
     that ended the veritesting region. *)
  | Search.SysCall eip
  | Search.InternalLoop eip
  | Search.Return eip
  | Search.Halt eip
  | Search.Special eip
  | Search.SearchLimit eip
  | Search.FunCall eip -> let str = Printf.sprintf "pc_0x%Lx" eip in
			  [V.Jmp (V.Name str) ]
  | Search.Instruction pointer_statements -> pointer_statements.Search.vine_stmts
  | Search.BreakLoop eip -> failwith "BreakLoop stmts_of_data: stub"

let decls_of_data = function 
  | Search.Instruction pointer_statements -> pointer_statements.Search.vine_decls
  | _ -> []

let add_declarations (node : Search.node) =
  let rec inner = function
    | [] -> ()
    | hd::tl ->
      begin
	Hashtbl.replace region_decls (V.key hd) hd;
	inner tl
      end in
  inner (decls_of_data node.Search.data)

let walk_statements context id (node : Search.node) =
  try
    Hashtbl.find cached_statments id
  with Not_found ->
    add_declarations node;
    let node_data = node.Search.data in
    let to_add = walk_statement_list context (stmts_of_data node_data) in
    Hashtbl.add cached_statments id to_add;
    (* do the substitution at the last moment *)
    (* (handle_context context to_add) *)
    to_add

let get_decl_list key value accum = value::accum


let encode_region ?context:(context = []) (root : Search.node) =
  let rec encode_region_int node =
    add_declarations node;
    match node.Search.children with 
    | [] -> [stmts_of_data node.Search.data]
    | [one] -> (stmts_of_data node.Search.data) :: (encode_region_int one)
    | head::tail -> failwith "Cant' handle branches at the moment." in
  let stmt_list = List.concat (encode_region_int root) in
  let ret_decl = Hashtbl.fold get_decl_list region_decls [] in
  ret_decl, stmt_list

let build_equations ?context:(context = []) (root : Search.node) =
  let rec be_int context node =
    let id = (Search.key node) in
    try
      Hashtbl.find common_expressions id
    with Not_found ->
      match node.Search.children with
      | [] -> 
	begin 
	  let to_add = VOpt.simplify (walk_statements context id node) in
	  Hashtbl.add common_expressions id to_add;
	  to_add
	end
      | [singleton] ->
	begin
	  let to_add = VOpt.simplify (V.BinOp (V.BITAND,
					       walk_statements context id node, 
					       be_int context singleton)) in
	  Hashtbl.add common_expressions id to_add;
	  to_add
	end
      | head::tail ->
	begin
	  let to_add = VOpt.simplify (V.BinOp (V.BITAND, 
					       walk_statements context id node,
					       walk_child_list node.Search.children)) in
	  Hashtbl.add common_expressions id to_add;
	  to_add
	end
  and walk_child_list = function
    | [one] -> be_int context one
    | [one; two] -> V.BinOp (V.BITOR,
			     be_int context one,
			     be_int context two)
    | _ -> failwith "assert 0 < |child_list| < 3 failed" in
  let ret_stmt = be_int context root
  and ret_decl = Hashtbl.fold get_decl_list region_decls [] in
  Hashtbl.clear common_expressions; (* can't keep common subexpressions between calls. *)
  Hashtbl.clear region_decls; (* probably want to feed the minimum decl list *)
  ret_decl, ret_stmt

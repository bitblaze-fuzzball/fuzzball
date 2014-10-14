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


let clear_caches _ =
  (** Some of the work can be saved between calls to encode the
      regions (I think).  In particular, translations of statement
      lists should be equivalent between all calls to walk statement
      list.  Contexts may mess this up.

      At any rate, repeatedly clearing the hashtables rather than
      reconstructing them for every call to encode region is cheaper
      because we don't have to regrow the tables every time.  **)
  Hashtbl.clear cached_statments;
  Hashtbl.clear common_expressions


let substitute_context context lvalue assignment =
  let rec helper = function 
    | [] -> [lvalue, assignment] (* hit the end without update, add. *)
    | hd::tl ->
      if (fst hd) = lvalue       (* update this context *)
      then (lvalue, assignment) :: tl
      else hd :: (helper tl) in  (* not our target, handle remaining list *)
  helper context


let rec handle_context context = function
  | V.Let (lvalue, e1, e2) -> handle_context (substitute_context context lvalue e1) e2
  | V.Lval lvalue -> List.assoc lvalue context
  | exp -> exp


let rec exp_of_stmt = function
  | V.Jmp exp 
  | V.ExpStmt exp 
  | V.Assert exp
  | V.Halt exp -> Expression exp
  | V.Label label -> Expression (V.Name label)
  | V.Special string ->failwith "Don't know what to do with magic"
  | V.Move (lval, exp) -> ContextUpdate (lval, exp)
  | V.Comment string -> Nothing
  | V.Block (decls, stmts) -> Nothing (* failwith "punt for later" *)
  | V.Attr (stmt, _) -> exp_of_stmt stmt
  | _ -> assert false


let stmts_of_data = function
  | Search.SysCall pointer_statements
  | Search.Instruction pointer_statements -> pointer_statements.Search.vine_stmts
  | Search.InternalLoop _
  | Search.Return _
  | Search.Halt _
  | Search.FunCall _
  | Search.BreakLoop _ -> []


let rec walk_statement_list context id node =
  try
    Hashtbl.find cached_statments id
  with Not_found ->
    let to_add = VOpt.simplify (wsl_int context (stmts_of_data node.Search.data)) in
    Hashtbl.add cached_statments id to_add;
    to_add
and wsl_int context = function 
  | [] ->  V.exp_true
  | head::tail ->
    begin
      match exp_of_stmt head with
      | Nothing -> wsl_int context tail
      | Expression exp ->  V.BinOp (V.BITAND, exp, wsl_int context tail)
      | ContextUpdate (lval, exp) ->
	begin
	  wsl_int (substitute_context context lval exp) tail
	end 
    end


let build_equations ?context:(context = []) (root : Search.node) =
  let rec be_int context node =
    let id = (Search.key node) in
    try
      Hashtbl.find common_expressions id
    with Not_found ->
      match node.Search.children with
      | [] -> 
	begin 
	  let to_add = VOpt.simplify (walk_statement_list context id node) in
	  Hashtbl.add common_expressions id to_add;
	  to_add
	end
      | [singleton] ->
	begin
	  let to_add = VOpt.simplify (V.BinOp (V.BITAND,
					       walk_statement_list context id node, 
					       be_int context singleton)) in
	  Hashtbl.add common_expressions id to_add;
	  to_add
	end
      | head::tail ->
	begin
	  let to_add = VOpt.simplify (V.BinOp (V.BITAND, 
					       walk_statement_list context id node,
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
  let ret = be_int context root in
  Hashtbl.clear common_expressions; (* can't keep common subexpressions between calls. *)
  ret

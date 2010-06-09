(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module FormulaManagerFunctor :
  functor (D : Exec_domain.DOMAIN) ->
sig
  class formula_manager : object
    method input_dl : Vine.var list

    method fresh_symbolic_1  : string -> D.t
    method fresh_symbolic_8  : string -> D.t
    method fresh_symbolic_16 : string -> D.t
    method fresh_symbolic_32 : string -> D.t
    method fresh_symbolic_64 : string -> D.t

    method get_input_vars : Vine.var list

    method fresh_region_base : string -> D.t

    method known_region_base : Vine.var -> bool

    method fresh_symbolic_mem_1  : string -> int64 -> D.t
    method fresh_symbolic_mem_8  : string -> int64 -> D.t
    method fresh_symbolic_mem_16 : string -> int64 -> D.t
    method fresh_symbolic_mem_32 : string -> int64 -> D.t
    method fresh_symbolic_mem_64 : string -> int64 -> D.t

    method make_concolic_mem_8 : string -> int64 -> int -> D.t

    method rewrite_for_solver : Vine.exp -> Vine.exp

    method get_mem_axioms : (Vine.var * Vine.exp) list

    method get_mem_bytes : Vine.var list
      
    method reset_mem_axioms : unit

    method eval_expr : Vine.exp -> int64

    method simplify1  : D.t -> D.t
    method simplify8  : D.t -> D.t
    method simplify16 : D.t -> D.t
    method simplify32 : D.t -> D.t
    method simplify64 : D.t -> D.t

    method if_expr_temp_unit : Vine.var -> (Vine.exp option -> unit) -> unit

    method walk_temps : (Vine.var -> Vine.exp -> (Vine.var * Vine.exp)) ->
      Vine.exp -> (Vine.var list * (Vine.var * Vine.exp) list)

    method conjoin : Vine.exp list -> Vine.exp
    method disjoin : Vine.exp list -> Vine.exp

    method collect_for_solving : (Vine.var * Vine.exp) list ->
      Vine.exp list -> Vine.exp ->
      (Vine.var list * (Vine.var * Vine.exp) list * Vine.exp * 
	 Vine.exp * Vine.var list)

    method measure_size : (int * int)
  end

  val if_expr_temp : formula_manager -> Vine.var ->
    (Vine.exp -> 'a) -> 'a -> (Vine.var -> unit) -> 'a
end

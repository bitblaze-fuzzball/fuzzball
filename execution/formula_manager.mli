(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val conjoin : Vine.exp list -> Vine.exp
val disjoin : Vine.exp list -> Vine.exp

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

    method make_concolic_8  : string -> int   -> D.t
    method make_concolic_16 : string -> int   -> D.t
    method make_concolic_32 : string -> int64 -> D.t
    method make_concolic_64 : string -> int64 -> D.t

    method fresh_region_base_concolic : string -> int64 -> D.t

    method make_concolic_mem_8 : string -> int64 -> int -> D.t

    method rewrite_for_solver : Vine.exp -> Vine.exp

    method get_mem_axioms : (Vine.var * Vine.exp) list

    method get_mem_bytes : Vine.var list
      
    method reset_mem_axioms : unit

    method eval_expr : Vine.exp -> int64

    method eval_expr_from_ce : (string * int64) list -> Vine.exp -> int64

    method concolic_eval_1  : D.t -> int
    method concolic_eval_8  : D.t -> int
    method concolic_eval_16 : D.t -> int
    method concolic_eval_32 : D.t -> int64
    method concolic_eval_64 : D.t -> int64

    method has_loop_var : D.t -> bool

    method simplify1  : D.t -> D.t
    method simplify8  : D.t -> D.t
    method simplify16 : D.t -> D.t
    method simplify32 : D.t -> D.t
    method simplify64 : D.t -> D.t

    method simplify_with_callback :
      (Vine.exp -> Vine.typ -> Vine.exp option) -> D.t -> Vine.typ -> D.t

    method make_ite : D.t -> Vine.typ -> D.t -> D.t -> D.t

    method make_table_lookup : (D.t list) -> Vine.exp -> int -> Vine.typ -> D.t

    method if_expr_temp_unit : Vine.var -> (Vine.exp option -> unit) -> unit

    method walk_temps : (Vine.var -> Vine.exp -> (Vine.var * Vine.exp)) ->
      Vine.exp -> (Vine.var list * (Vine.var * Vine.exp) list)

    method collect_for_solving : (Vine.var * Vine.exp) list ->
      Vine.exp list -> Vine.exp ->
      (Vine.var list * (Vine.var * Vine.exp) list * Vine.exp * 
	 Vine.exp * Vine.var list)

    method one_cond_for_solving : Vine.exp -> unit Vine.VarHash.t ->
      Vine.var list * (Vine.var * Vine.exp) list * Vine.exp *
        Vine.var list

    method measure_size : (int * int)
  end

  val if_expr_temp : formula_manager -> Vine.var ->
    (Vine.exp -> 'a) -> 'a -> (Vine.var -> unit) -> 'a

  val map_expr_temp : formula_manager -> Vine.exp ->
    ((Vine.exp -> 'a) -> Vine.exp -> 'a) ->
    (int -> 'a -> 'a) -> 'a
end

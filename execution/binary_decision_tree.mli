(*
  Copyright (C) BitBlaze, 2009-2012. All rights reserved.
*)

class binary_decision_tree : object
  inherit Decision_tree.decision_tree

  method init : Decision_tree.decision_tree
  method reset : unit
  method get_hist : bool list
  method get_hist_str : string
  method get_hist_str_queries : string
  method get_hist_str_bracketed : string
  method get_depth : int
  method add_kid : bool -> unit
  method start_new_query : unit
  method start_new_query_binary : unit
  method count_query : unit
  method extend : bool -> unit
  method set_iter_seed : int -> unit
  method random_bit : bool
  method random_float : float
  method record_unsat : bool -> unit

  method try_extend : (bool -> Vine.exp) ->
    (bool -> Vine.exp -> bool) -> (bool -> unit) -> (unit -> bool) -> int64
    -> (bool * Vine.exp)

  method try_extend_memoryless : (bool -> Vine.exp) ->
    (bool -> Vine.exp -> bool) -> (bool -> unit) -> (unit -> bool) 
    -> (bool * Vine.exp)

  method set_heur : int -> unit
  method heur_preference : bool option
  method mark_all_seen : unit

  method check_last_choices : bool option
  method have_choice : bool
  method try_again_p : bool

  method cur_ident : int
  method is_live_ident : int -> bool
  method cur_can_reach_ident : int -> bool option

  method measure_size : int
  method print_tree : out_channel -> unit
end

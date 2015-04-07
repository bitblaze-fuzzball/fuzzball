(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine

class virtual decision_tree = object
  method virtual init : decision_tree
  method virtual reset : unit
  method virtual get_hist : bool list
  method virtual get_hist_str : string
  method virtual get_hist_str_queries : string
  method virtual get_hist_str_bracketed : string
  method virtual get_depth : int
  method virtual add_kid : bool -> unit
  method virtual start_new_query : unit
  method virtual start_new_query_binary : unit
  method virtual count_query : unit
  method virtual extend : bool -> unit
  method virtual set_iter_seed : int -> unit
  method virtual random_bit : bool
  method virtual random_float : float
  method virtual record_unsat : bool -> unit

  method virtual try_extend : (bool -> Vine.exp) ->
    (bool -> Vine.exp -> bool) -> (bool -> unit) -> (unit -> bool) ->
    (bool -> bool) -> int64 -> (bool * Vine.exp)

  method virtual try_extend_memoryless : (bool -> V.exp) ->
    (bool -> V.exp -> bool) -> (bool -> unit) -> (unit -> bool) 
    -> (bool * V.exp)

  method virtual set_heur : int -> unit
  method virtual heur_preference : bool option
  method virtual mark_all_seen : unit

  method virtual check_last_choices : bool option
  method virtual have_choice : bool
  method virtual try_again_p : bool

  method virtual measure_size : int
  method virtual print_tree : out_channel -> unit
end

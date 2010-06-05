(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

class decision_tree : object
  method init : decision_tree
  method reset : unit
  method get_hist : bool list
  method get_hist_str : string
  method get_hist_queries : bool list list 
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
  method record_unsat : bool -> unit

  method try_extend : (bool -> Vine.exp) ->
    (bool -> Vine.exp -> bool) -> (bool -> unit) -> (unit -> bool) 
    -> (bool * Vine.exp)

  method try_extend_memoryless : (bool -> Vine.exp) ->
    (bool -> Vine.exp -> bool) -> (bool -> unit) -> (unit -> bool) 
    -> (bool * Vine.exp)

  method mark_all_seen : unit

  method try_again_p : bool

  method print_tree : out_channel -> unit
end

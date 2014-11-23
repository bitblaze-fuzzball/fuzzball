(*
  Copyright (C) BitBlaze, 2010-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_exceptions;;
open Exec_options;;

class linear_decision_tree = object(self)
  inherit Decision_tree.decision_tree

  val mutable depth = 0
  val mutable history = []

  method init = (self :> Decision_tree.decision_tree)

  method reset =
    history <- [];
    depth <- 0;

  method get_hist = history

  method get_hist_str = 
    String.concat ""
      (List.map (fun b -> if b then "1" else "0") (List.rev self#get_hist));

  method get_hist_str_queries = self#get_hist_str

  method get_hist_str_bracketed = self#get_hist_str

  method get_depth = depth

  method add_kid b = ()

  method start_new_query = ()

  method start_new_query_binary = ()

  method count_query = ()

  method extend b =
    history <- b :: history;
    depth <- depth + 1

  method set_iter_seed i = ()

  method random_bit = Random.bool ()

  method random_float = Random.float 1.0

  method record_unsat b = ()

  method try_extend (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) (random_bit_gen : unit -> bool) eip
    =
    let b = random_bit_gen () in
    let c = trans_func b and
	c' = trans_func (not b) in
    let r1 = try_func b c in
      assert(r1);
      ignore(try_func (not b) c');
      self#extend b;
      (b, c)

  method try_extend_memoryless (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) (random_bit_gen : unit -> bool) =
    let b = random_bit_gen () in
    let c = trans_func b in
    let r1 = try_func b c in
      assert(r1);
      self#extend b;
      (b, c)

  method set_heur i = ()
  method heur_preference = (None : bool option)
  method mark_all_seen = ()

  method try_again_p = false

  method check_last_choices =
    match history with
      | b :: _ -> Some b
      | [] -> failwith "Missing parent in check_last_choices"

  method have_choice = false

  method measure_size = depth

  method print_tree chan = ()
end

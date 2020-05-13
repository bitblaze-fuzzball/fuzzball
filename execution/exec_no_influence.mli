(*
  Copyright (C) 2010 Ensighta Security Inc.  All rights reserved.
*)

class virtual influence_manager : object
  method virtual take_measure_eip : Vine.exp list -> unit
  
  method virtual take_measure_expr : Vine.exp -> Vine.exp -> unit

  method virtual measure_influence_common : Vine.decl list
    -> (Vine.var * Vine.exp) list -> Vine.exp -> Vine.exp list -> float

  method virtual measure_influence : Vine.exp list -> float

  method virtual compute_multipath_influence : string -> unit

  method virtual compute_all_multipath_influence : unit

  method virtual store_symbolic_byte_influence  : int64 -> string -> unit
  method virtual store_symbolic_short_influence : int64 -> string -> unit
  method virtual store_symbolic_word_influence  : int64 -> string -> unit
  method virtual store_symbolic_long_influence  : int64 -> string -> unit

  method virtual maybe_periodic_influence : unit

  method virtual path_end_influence : unit

  method virtual measure_point_influence : string -> Vine.exp list -> unit

  method virtual maybe_measure_influence_deref : Vine.exp -> unit

  method virtual measure_influence_rep : unit

  method virtual measure_influence_expr : Vine.exp list -> unit

  method virtual disqualify_path : unit

  method virtual eip_hook : int64 -> unit

  method virtual finish_path : unit 

  method virtual reset : unit

  method virtual after_exploration : unit
end

class no_influence_manager : object
  method take_measure_eip : Vine.exp list -> unit

  method take_measure_expr : Vine.exp -> Vine.exp -> unit

  method measure_influence_common : Vine.decl list
    -> (Vine.var * Vine.exp) list -> Vine.exp -> Vine.exp list -> float

  method measure_influence : Vine.exp list -> float

  method compute_multipath_influence : string -> unit

  method compute_all_multipath_influence : unit

  method store_symbolic_byte_influence  : int64 -> string -> unit
  method store_symbolic_short_influence : int64 -> string -> unit
  method store_symbolic_word_influence  : int64 -> string -> unit
  method store_symbolic_long_influence  : int64 -> string -> unit

  method maybe_periodic_influence : unit

  method path_end_influence : unit

  method measure_point_influence : string -> Vine.exp list -> unit

  method maybe_measure_influence_deref : Vine.exp -> unit

  method measure_influence_rep : unit

  method measure_influence_expr : Vine.exp list -> unit

  method disqualify_path : unit

  method eip_hook : int64 -> unit

  method finish_path : unit 

  method reset : unit

  method after_exploration : unit
end

(*
  Copyright (C) 2010 Ensighta Security Inc.  All rights reserved.
*)

module V = Vine

class virtual influence_manager = object
  method virtual take_measure_eip : Vine.exp -> unit
  
  method virtual take_measure_expr : Vine.exp -> Vine.exp -> unit

  method virtual measure_influence_common : Vine.decl list
    -> (Vine.var * Vine.exp) list -> Vine.exp -> Vine.exp -> float

  method virtual measure_influence : Vine.exp -> float

  method virtual compute_multipath_influence : string -> unit

  method virtual compute_all_multipath_influence : unit

  method virtual store_symbolic_byte_influence  : int64 -> string -> unit
  method virtual store_symbolic_short_influence : int64 -> string -> unit
  method virtual store_symbolic_word_influence  : int64 -> string -> unit
  method virtual store_symbolic_long_influence  : int64 -> string -> unit

  method virtual maybe_periodic_influence : unit

  method virtual path_end_influence : unit

  method virtual measure_point_influence : string -> Vine.exp -> unit

  method virtual maybe_measure_influence_deref : Vine.exp -> unit

  method virtual measure_influence_rep : unit

  method virtual measure_influence_expr : Vine.exp -> unit

  method virtual disqualify_path : unit

  method virtual eip_hook : int64 -> unit

  method virtual finish_path : unit 

  method virtual reset : unit

  method virtual after_exploration : unit
end

class no_influence_manager = object(self)
  inherit influence_manager

  method take_measure_eip (e:V.exp) = ()
  method take_measure_expr (e1:V.exp) (e2:V.exp) = ()
  method measure_influence_common (dl:V.decl list)
    (a:(V.var * V.exp) list) (e1:V.exp) (e2:V.exp) = 0.0
  method measure_influence (e:V.exp) = 0.0
  method compute_multipath_influence (s:string) = ()
  method compute_all_multipath_influence = ()
  method store_symbolic_byte_influence  (a:int64) (s:string) = ()
  method store_symbolic_short_influence (a:int64) (s:string) = ()
  method store_symbolic_word_influence  (a:int64) (s:string) = ()
  method store_symbolic_long_influence  (a:int64) (s:string) = ()
  method maybe_periodic_influence = ()
  method path_end_influence = ()
  method measure_point_influence (s:string) (e:V.exp) = ()
  method maybe_measure_influence_deref (e:V.exp) = ()
  method measure_influence_rep = ()
  method measure_influence_expr (e:V.exp) = ()
  method disqualify_path = ()
  method eip_hook (i:int64) = ()
  method finish_path = () 
  method reset = ()
  method after_exploration = ()
end  

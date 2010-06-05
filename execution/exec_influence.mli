(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

type argparams_t = {
  mutable _tmp_name : string;
  mutable _get_val_bounds : bool;
  mutable _low_bound_to : float;
  mutable _sample_pts : int;
  mutable _xor_count : int;
  mutable _xor_seed : int;
  mutable _stp_file : string;
  mutable _ps_file : string;
} ;;

val get_influence : Vine.program -> argparams_t -> Vine.exp -> float

(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Query_engine;;
open Stpvc_engine;;

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

let get_influence (prog: Vine.program) (args: argparams_t) (q: V.exp) =
  0.0
;;

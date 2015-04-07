(*
  Copyright (C) BitBlaze 2014.  All rights reserved.
*)

(* This module contains functionality which is used by more than one
   of the query engines. *)

type external_solver_type =
  | STP_CVC
  | STP_SMTLIB2
  | CVC4
  | BOOLECTOR
  | Z3
  | MATHSAT

val map_lines : (string -> 'a option) -> in_channel -> ('a list)

val smtlib_rename_var : string -> string

type maybe_ce_result =
  | No_CE_here
  | End_of_CE
  | Assignment of string * int64 * bool

val parse_z3_ce_line : string -> string option
  -> (maybe_ce_result * string option)

val parse_mathsat_ce_line : string -> string option
  -> (maybe_ce_result * string option)

val parse_ce : external_solver_type -> string -> maybe_ce_result

val create_temp_dir : string -> string

val pick_fresh_fname : string -> string -> int -> string

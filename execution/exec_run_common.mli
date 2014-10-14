(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val decode_insn : Asmir.varctx -> int64 -> char array -> Vine.program

val label_to_eip : string -> int64

val trans_cache : (int64, Vine.program) Hashtbl.t

val with_trans_cache : int64 -> (unit -> Vine.program) -> Vine.program

val print_insns : int64 -> Vine.program -> int64 option -> char -> unit

val run_one_insn : Fragment_machine.fragment_machine -> Asmir.varctx -> int64
  -> char array -> int64

val decode_insn_at : Fragment_machine.fragment_machine -> Asmir.varctx -> int64 -> Vine.decl list * Vine.stmt list

val decode_insns : Fragment_machine.fragment_machine -> Asmir.varctx -> int64 -> int -> Vine.decl list * Vine.stmt list

val last : 'a list -> 'a

val has_special : Vine.stmt list -> bool

val tuple_push : 'a * 'b -> 'a list * 'b list -> 'a list * 'b list

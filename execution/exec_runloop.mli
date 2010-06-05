(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val trans_cache : (int64, Vine.program) Hashtbl.t

val runloop :
  < load_byte_conc : int64 -> int; run : unit -> string;
    run_eip_hooks : unit; set_eip : int64 -> unit;
    set_frag : Vine.decl list * Vine.stmt list -> unit;
    set_word_reg_symbolic : Fragment_machine.register_name ->
						string -> unit;
    set_word_var : Fragment_machine.register_name -> int64 -> unit;
    watchpoint : unit; .. >
  -> int64 -> Asmir.varctx -> (int64 -> bool) -> unit

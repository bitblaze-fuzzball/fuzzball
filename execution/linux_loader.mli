(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

type needed_fm =
    < 
      get_short_var : Fragment_machine.register_name -> int;
      load_word_conc : int64 -> int64;
      load_x86_user_regs : Temu_state.userRegs -> unit;
      set_short_var : Fragment_machine.register_name -> int -> unit;
      set_word_var : Fragment_machine.register_name -> int64 -> unit;
      store_byte_conc : int64 -> int -> unit;
      store_cstr : int64 -> int64 -> string -> unit;
      store_page_conc : int64 -> string -> unit;
      store_str : int64 -> int64 -> string -> unit;
      store_word_conc : int64 -> int64 -> unit; watchpoint : unit;
      zero_fill : int64 -> int -> unit;
      >

val load_dynamic_program : needed_fm -> string -> int64 -> bool -> bool ->
  (int64 * int64) list -> string list -> int64

val load_core : needed_fm -> string -> int64

val setup_tls_segment : needed_fm -> int64 -> int64 -> unit

val proc_identities : (int * int * int * int) option ref

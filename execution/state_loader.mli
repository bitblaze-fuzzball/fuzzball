(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val load_mem_state :
  < load_x86_user_regs : Temu_state.userRegs -> unit;
    store_byte_conc : int64 -> int -> unit;
    store_page_conc : int64 -> string -> unit; .. >
      -> string -> int64

(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

class linux_special_nonhandler : < print_x86_regs : unit; .. >
  -> object
  method handle_special : string -> Vine.stmt list option
end

class trap_special_nonhandler : _ -> object
  method handle_special : string -> Vine.stmt list option
end

class cpuid_special_handler :
  < get_word_var : Fragment_machine.register_name -> int64;
    set_word_var : Fragment_machine.register_name -> int64 -> unit; .. >
      ->
object
  method handle_special : string -> Vine.stmt list option
end

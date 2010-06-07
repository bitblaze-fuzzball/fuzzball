(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

class linux_special_nonhandler : Fragment_machine.fragment_machine -> object
  method handle_special : string -> Vine.stmt list option
end

class trap_special_nonhandler : Fragment_machine.fragment_machine -> object
  method handle_special : string -> Vine.stmt list option
end

class cpuid_special_handler : Fragment_machine.fragment_machine -> object
  method handle_special : string -> Vine.stmt list option
end

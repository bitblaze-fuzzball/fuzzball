(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
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

(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

class linux_special_nonhandler : Fragment_machine.fragment_machine -> object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
  method state_json : Yojson.Safe.json option
end

class trap_special_nonhandler : Fragment_machine.fragment_machine -> object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
  method state_json : Yojson.Safe.json option
end

class cpuid_special_handler : Fragment_machine.fragment_machine -> object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
  method state_json : Yojson.Safe.json option
end

class x87_emulator_special_handler : Fragment_machine.fragment_machine ->
object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
  method state_json : Yojson.Safe.json option
end

class sse_floating_point_punter : Fragment_machine.fragment_machine ->
object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
  method state_json : Yojson.Safe.json option
end

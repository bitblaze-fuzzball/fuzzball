class cgcos_special_handler : Fragment_machine.fragment_machine ->
object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
  method state_json : Yojson.Safe.json option
  method enablePointerManagementMemoryChecking : unit
end

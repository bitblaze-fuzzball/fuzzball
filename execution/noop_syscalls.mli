class noop_linux_special_handler : Fragment_machine.fragment_machine ->
object
  method handle_special : string -> Vine.stmt list option
  method make_snap : unit
  method reset : unit
end

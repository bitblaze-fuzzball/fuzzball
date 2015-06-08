
class pointer_management : object
  method set_reporter : ((string * Yojson.Safe.json) list -> unit) -> unit
  method add_alloc : int64 -> int64 -> unit
  method add_dealloc : int64 -> int64 -> unit
  method is_safe_read :  ?prov:Interval_tree.provenance -> int64 -> int64 -> bool
  method is_safe_write :   ?prov:Interval_tree.provenance -> int64 -> int64 -> bool
  method update_stack_end : int64 -> unit
  method find_read_prov : int64 -> int64 -> Interval_tree.provenance
  method make_snap : unit
  method reset : unit
end

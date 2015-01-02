class pointer_management : object
  method set_reporter : ((string * Yojson.Safe.json) list -> unit) -> unit
  method add_alloc : int64 -> int64 -> unit
  method add_dealloc : int64 -> int64 -> unit
  method is_safe_read : int64 -> int64 -> bool
  method is_safe_write : int64 -> int64 -> bool
  method update_stack_end : int64 -> unit
  method clear : unit
  method copy_tables : Interval_tree.element Interval_tree.IntervalMap.t -> Interval_tree.IntervalMap.key Interval_tree.IntervalMap.t -> unit
  method construct_deep_copy : pointer_management
(*  method add_alloc_dirty : int64 -> bool Int64Array.int64Array -> unit
  method add_dealloc_dirty : int64 -> int64 -> unit *)
end

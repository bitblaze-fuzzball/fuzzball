class pointer_management : object
  method add_alloc : int64 -> int64 -> unit
  method add_dealloc : int64 -> int64 -> unit
  method is_safe_access : int64 -> int64 -> bool
  method set_esp_lookup : (unit -> int64) -> unit
  method clear : unit
  method construct_deep_copy : pointer_management
  method add_alloc_dirty : int64 -> int64 -> unit
  method add_dealloc_dirty : int64 -> int64 -> unit
end

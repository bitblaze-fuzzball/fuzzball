(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

class virtual concrete_memory : object
  method virtual store_byte : Int64.t -> int -> unit
  method virtual maybe_load_byte : Int64.t -> int option
  method virtual clear : unit -> unit

  method measure_size : int

  method load_byte : int64 -> int 

  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit

  method store_page : int64 -> string -> unit

  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
    
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option
end

class concrete_string_memory : object
  inherit concrete_memory

  method store_byte : Int64.t -> int -> unit
  method maybe_load_byte : Int64.t -> int option
  method clear : unit -> unit
  method measure_size : int
  method load_byte : int64 -> int 
  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit
  method store_page : int64 -> string -> unit
  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option
end

class concrete_hash_memory : object
  inherit concrete_memory

  method store_byte : Int64.t -> int -> unit
  method maybe_load_byte : Int64.t -> int option
  method clear : unit -> unit
  method measure_size : int
  method load_byte : int64 -> int 
  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit
  method store_page : int64 -> string -> unit
  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option
end

class virtual concrete_memory_w_reset : object
  inherit concrete_memory

  method virtual store_byte : Int64.t -> int -> unit
  method virtual maybe_load_byte : Int64.t -> int option
  method virtual clear : unit -> unit
  method measure_size : int
  method load_byte : int64 -> int 
  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit
  method store_page : int64 -> string -> unit
  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option

  method virtual make_snap : unit -> unit
  method virtual reset : unit -> unit
end

class concrete_snapshot_memory :
  concrete_memory_w_reset -> concrete_memory_w_reset -> object
  inherit concrete_memory
    
  method store_byte : Int64.t -> int -> unit
  method maybe_load_byte : Int64.t -> int option
  method clear : unit -> unit
  method measure_size : int
  method load_byte : int64 -> int 
  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit
  method store_page : int64 -> string -> unit
  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option

  method make_snap : unit -> unit
  method reset : unit -> unit
end

class parallel_check_memory :
  concrete_memory_w_reset -> concrete_memory_w_reset -> object
  inherit concrete_memory

  method store_byte : Int64.t -> int -> unit
  method maybe_load_byte : Int64.t -> int option
  method clear : unit -> unit
  method measure_size : int
  method load_byte : int64 -> int 
  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit
  method store_page : int64 -> string -> unit
  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option

  method make_snap : unit -> unit
  method reset : unit -> unit
end

class string_maybe_memory : object
  inherit concrete_memory

  method store_byte : Int64.t -> int -> unit
  method maybe_load_byte : Int64.t -> int option
  method clear : unit -> unit
  method measure_size : int
  method load_byte : int64 -> int 
  method store_short : int64 -> int   -> unit
  method store_word  : int64 -> int64 -> unit
  method store_long  : int64 -> int64 -> unit
  method store_page : int64 -> string -> unit
  method load_short : int64 -> int
  method load_word  : int64 -> int64
  method load_long  : int64 -> int64
  method maybe_load_short : int64 -> int   option
  method maybe_load_word  : int64 -> int64 option
  method maybe_load_long  : int64 -> int64 option
end

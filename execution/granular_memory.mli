(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module GranularMemoryFunctor :
  functor (D : Exec_domain.DOMAIN) ->
sig
  val split64 : D.t -> (D.t * D.t)
  val split32 : D.t -> (D.t * D.t)
  val split16 : D.t -> (D.t * D.t)

  val endian_i : int -> int -> int

  type gran8 = Byte of D.t
	       | Absent8

  type gran16 = Short of D.t
		| Gran8s of gran8 * gran8
		| Absent16
      
  type gran32 = Word of D.t
		| Gran16s of gran16 * gran16
		| Absent32

  type gran64 = Long of D.t
		| Gran32s of gran32 * gran32
		| Absent64

  type missing_t = int -> int64 -> D.t

  val  gran8_get_byte  : gran8  -> missing_t -> int64 ->        (D.t * gran8)
  val gran16_get_byte  : gran16 -> missing_t -> int64 -> int -> (D.t * gran16)
  val gran32_get_byte  : gran32 -> missing_t -> int64 -> int -> (D.t * gran32)
  val gran64_get_byte  : gran64 -> missing_t -> int64 -> int -> (D.t * gran64)
  val gran16_get_short : gran16 -> missing_t -> int64 ->        (D.t * gran16)
  val gran32_get_short : gran32 -> missing_t -> int64 -> int -> (D.t * gran32)
  val gran64_get_short : gran64 -> missing_t -> int64 -> int -> (D.t * gran64)
  val gran32_get_word  : gran32 -> missing_t -> int64 ->        (D.t * gran32)
  val gran64_get_word  : gran64 -> missing_t -> int64 -> int -> (D.t * gran64)
  val gran64_get_long  : gran64 -> missing_t -> int64 ->        (D.t * gran64)

  val gran64_split : gran64 -> (gran32 * gran32)
  val gran32_split : gran32 -> (gran16 * gran16)
  val gran16_split : gran16 -> (gran8  * gran8)
	      
  val gran16_put_byte  : gran16 -> int -> D.t -> gran16
  val gran32_put_byte  : gran32 -> int -> D.t -> gran32
  val gran64_put_byte  : gran64 -> int -> D.t -> gran64
  val gran32_put_short : gran32 -> int -> D.t -> gran32
  val gran64_put_short : gran64 -> int -> D.t -> gran64
  val gran64_put_word  : gran64 -> int -> D.t -> gran64

  val gran8_to_string  : gran8  -> string
  val gran16_to_string : gran16 -> string
  val gran32_to_string : gran32 -> string
  val gran64_to_string : gran64 -> string

  val gran8_size  : gran8  -> int
  val gran16_size : gran16 -> int
  val gran32_size : gran32 -> int
  val gran64_size : gran64 -> int

  class virtual granular_memory : object
    method on_missing : missing_t -> unit

    method private virtual store_common_fast :
      int64 -> (gran64 -> int -> gran64) -> unit
      
    method private virtual with_chunk :
      int64 -> (gran64 -> int64 -> int -> D.t * gran64) -> D.t option

    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option

    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t

    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit

    method store_page : int64 -> string -> unit
	    
    method virtual clear : unit -> unit

    method virtual measure_size : int * int * int
  end
    
  class granular_page_memory : object
    inherit granular_memory

    method private store_common_fast :
      int64 -> (gran64 -> int -> gran64) -> unit
      
    method private with_chunk :
      int64 -> (gran64 -> int64 -> int -> D.t * gran64) -> D.t option

    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method store_page : int64 -> string -> unit
    method clear : unit -> unit
    method measure_size : int * int * int
  end

  class granular_sink_memory : object
    inherit granular_memory

    method private store_common_fast :
      int64 -> (gran64 -> int -> gran64) -> unit
      
    method private with_chunk :
      int64 -> (gran64 -> int64 -> int -> D.t * gran64) -> D.t option

    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method store_page : int64 -> string -> unit
    method clear : unit -> unit
    method measure_size : int * int * int
  end

  class granular_hash_memory : object
    inherit granular_memory

    method private store_common_fast :
      int64 -> (gran64 -> int -> gran64) -> unit
      
    method private with_chunk :
      int64 -> (gran64 -> int64 -> int -> D.t * gran64) -> D.t option

    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method store_page : int64 -> string -> unit
    method clear : unit -> unit
    method measure_size : int * int * int
  end

  class granular_snapshot_memory : granular_memory -> granular_memory ->
  object
    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method store_page : int64 -> string -> unit
    method clear : unit -> unit
    method measure_size : int * int * int

    method make_snap : unit -> unit
    method reset : unit -> unit
  end

  class granular_second_snapshot_memory :
    granular_snapshot_memory -> granular_memory ->
  object
    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method store_page : int64 -> string -> unit
    method clear : unit -> unit
    method measure_size : int * int * int

    method make_snap : unit -> unit
    method reset : unit -> unit

    method inner_make_snap : unit -> unit
  end
    
  class concrete_adaptor_memory : Concrete_memory.concrete_memory -> object
    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method clear : unit -> unit
    method measure_size : int * int * int
  end

  class concrete_maybe_adaptor_memory :
    Concrete_memory.concrete_memory ->
  object
    method on_missing : missing_t -> unit
    method maybe_load_byte  : int64 -> D.t option
    method maybe_load_short : int64 -> D.t option
    method maybe_load_word  : int64 -> D.t option
    method maybe_load_long  : int64 -> D.t option
    method load_byte  : int64 -> D.t
    method load_short : int64 -> D.t
    method load_word  : int64 -> D.t
    method load_long  : int64 -> D.t
    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit
    method store_page : int64 -> string -> unit
    method clear : unit -> unit
    method measure_size : int * int * int
  end
end

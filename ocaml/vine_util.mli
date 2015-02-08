val id : 'a -> 'a
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
val ( <@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val foldn : ('a -> int -> 'a) -> 'a -> int -> 'a
val mapn : (int -> 'a) -> int -> 'a list

val inc : int ref -> unit
val dec : int ref -> unit

val list_union : 'a list -> 'a list -> 'a list
val list_intersection : 'a list -> 'a list -> 'a list
val list_does_intersect : 'a list -> 'a list -> bool
val shortest_first : (int -> int -> 'a) -> 'b list -> 'c list -> 'a
val list_difference : 'a list -> 'a list -> 'a list
val list_subset : 'a list -> 'a list -> bool
val list_set_eq : 'a list -> 'a list -> bool

val union_find : ('a -> 'b list) -> 'a list -> 'a list list

val list_foldl : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val list_pop : 'a list ref -> 'a
val list_last : 'a list -> 'a
val list_partition_last : 'a list -> 'a list * 'a
val list_last_option : 'a list -> 'a option
val list_filter_some : ('a -> 'b option) -> 'a list -> 'b list
val list_find_some : ('a -> 'b option) -> 'a list -> 'b
val list_unique : 'a list -> 'a list
val list_map_some : ('a -> 'b option) -> 'a list -> 'b list
val list_join : ('a -> 'a -> 'a) -> 'a list -> 'a

val split_common_prefix : 'a list -> 'a list -> 'a list * 'a list * 'a list
val split_common_suffix : 'a list -> 'a list -> 'a list * 'a list * 'a list

val option_map : ('a -> 'b) -> 'a option -> 'b option
val apply_option : ('a -> 'a) option -> 'a -> 'a

val print_separated_list : ('a -> string) -> string -> 'a list -> string

module HashUtil :
  functor (H : Hashtbl.S) ->
    sig
      val hashtbl_eq : ?eq:('a -> 'a -> bool) -> 'a H.t -> 'a H.t -> bool
    end
val hashtbl_eq :
  ?eq:('a -> 'a -> bool) -> ('b, 'a) Hashtbl.t -> ('b, 'a) Hashtbl.t -> bool

val trim_newline : string -> string


val int64_udiv : int64 -> int64 -> int64
val int64_urem : int64 -> int64 -> int64
val int64_ucompare : int64 -> int64 -> int
val int64_umax : int64 -> int64 -> int64
val int64_umin : int64 -> int64 -> int64
val int64_urandom : int64 -> int64
val int64_u_to_float : int64 -> float
val int64_u_of_float : float -> int64
val int64_u_of_string : string -> int64

val run_with_remapped_fd :
  Unix.file_descr -> Unix.file_descr -> (unit -> 'a) -> 'a

type round_mode = | ROUND_NEAREST (* ties to even *)
		  | ROUND_NEAREST_AWAY_ZERO
		  | ROUND_POSITIVE
		  | ROUND_NEGATIVE
		  | ROUND_ZERO

val f32_neg : round_mode -> int32 -> int32
val f32_eq : round_mode -> int32 -> int32 -> bool
val f32_ne : round_mode -> int32 -> int32 -> bool
val f32_lt : round_mode -> int32 -> int32 -> bool
val f32_le : round_mode -> int32 -> int32 -> bool
val f32_add : round_mode -> int32 -> int32 -> int32
val f32_sub : round_mode -> int32 -> int32 -> int32
val f32_mul : round_mode -> int32 -> int32 -> int32
val f32_div : round_mode -> int32 -> int32 -> int32
val f32_rem : round_mode -> int32 -> int32 -> int32

val f64_neg : round_mode -> int64 -> int64
val f64_eq : round_mode -> int64 -> int64 -> bool
val f64_ne : round_mode -> int64 -> int64 -> bool
val f64_lt : round_mode -> int64 -> int64 -> bool
val f64_le : round_mode -> int64 -> int64 -> bool
val f64_add : round_mode -> int64 -> int64 -> int64
val f64_sub : round_mode -> int64 -> int64 -> int64
val f64_mul : round_mode -> int64 -> int64 -> int64
val f64_div : round_mode -> int64 -> int64 -> int64
val f64_rem : round_mode -> int64 -> int64 -> int64

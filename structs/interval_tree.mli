type interval = { low : int64; high : int64; }

val make_interval : int -> int -> interval

val make_interval_wcheck : int64 -> int64 -> interval

module IntervalMap :
  sig
    type key = interval
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

type key = interval
type 'a t = 'a IntervalMap.t

val empty : 'a t
val is_empty : 'a t -> bool
val mem : key -> 'a t -> bool
val add : key -> 'a -> 'a t -> 'a t
val singleton : key -> 'a -> 'a t
val remove : key -> 'a t -> 'a t
val merge :
  (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val iter : (key -> 'a -> unit) -> 'a t -> unit
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val cardinal : 'a t -> int


val find : key -> 'a t -> 'a

type claimStatus = Allocate | Deallocate

type element = { key : interval; at : claimStatus; }

exception AllocatingAllocated of (interval * interval)
exception ReadingUnwritten of interval
exception ReadingUnallocated of interval
exception DoubleFree of (interval * interval)
exception DeallocatingUnallocated of interval
exception DeallocationSizeMismatch of (interval * interval)
exception WritingUnallocated of interval

val optional_find : 'a IntervalMap.t -> IntervalMap.key -> 'a option

val remove_all : 'a IntervalMap.t -> IntervalMap.key -> 'a IntervalMap.t

val attempt_allocate :
  element IntervalMap.t -> IntervalMap.key -> element IntervalMap.t

val attempt_deallocate :
  element IntervalMap.t ->
  'a IntervalMap.t -> IntervalMap.key -> element IntervalMap.t * 'a IntervalMap.t

val attempt_read :
  'a IntervalMap.t -> interval IntervalMap.t -> IntervalMap.key -> unit

val attempt_write :
  element IntervalMap.t ->
  IntervalMap.key IntervalMap.t ->
  IntervalMap.key -> IntervalMap.key IntervalMap.t

val copy : 'a IntervalMap.t -> 'a IntervalMap.t

val print_interval : interval -> unit

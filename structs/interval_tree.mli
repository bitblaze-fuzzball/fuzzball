type provenance = Internal | External | Random | DontKnow

type interval = {
  low : int64;
  high : int64;
  mutable provenance : provenance;
  mutable accessed : int;
}

val prov_to_string : provenance -> string
val make_interval : ?prov:provenance -> int -> int -> interval
val make_interval_wcheck : ?prov:provenance -> int64 -> int64 -> interval
val intersects : interval -> interval -> bool
val abbuts : interval -> interval -> bool
val is_in : interval -> interval -> bool
val safe_merge : interval -> interval -> interval
val fast_merge : interval -> interval -> interval
val check_interval : interval -> unit
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
type claimStatus = Allocate | Deallocate
type element = { key : interval; at : claimStatus; }
type conflicting_intervals = {
  original_interval : interval;
  proposed_conflict : interval;
}
type multiconflict = {
  existing_regions : interval list;
  proposed : interval;
}
exception AllocatingAllocated of conflicting_intervals
exception ReadingUnwritten of interval
exception ReadingUnallocated of interval
exception DoubleFree of conflicting_intervals
exception DeallocatingUnallocated of interval
exception DeallocationSizeMismatch of conflicting_intervals
exception WriteBeforeAllocated of interval
exception WriteAfterDeallocated of interval
exception WriteBefore of conflicting_intervals
exception WriteAfter of conflicting_intervals
exception WriteAcross of conflicting_intervals
exception MultiWriteConflict of multiconflict
val optional_find : 'a IntervalMap.t -> IntervalMap.key -> 'a option
val remove_all : 'a IntervalMap.t -> IntervalMap.key -> 'a IntervalMap.t
val attempt_allocate :
  element IntervalMap.t -> IntervalMap.key -> element IntervalMap.t
val attempt_deallocate :
  element IntervalMap.t ->
  'a IntervalMap.t ->
  IntervalMap.key -> element IntervalMap.t * 'a IntervalMap.t
val attempt_read :
  'a IntervalMap.t -> interval IntervalMap.t -> IntervalMap.key -> unit
val copy : 'a IntervalMap.t -> 'a IntervalMap.t
val find_all : element IntervalMap.t -> IntervalMap.key -> interval list
val attempt_write :
  ?prov:provenance ->
  element IntervalMap.t ->
  IntervalMap.key IntervalMap.t ->
  IntervalMap.key -> IntervalMap.key * IntervalMap.key IntervalMap.t
val print_interval : interval -> unit

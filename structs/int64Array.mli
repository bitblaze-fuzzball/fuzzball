type 'a int64Array = { arrays : 'a array array; length : int64; }
val get : 'a int64Array -> int64 -> 'a
val set : 'a int64Array -> int64 -> 'a -> unit
val make : int64 -> 'a -> 'a int64Array
val create : int64 -> 'a -> 'a int64Array
val iter : 'a int64Array -> ('a -> 'b) -> unit
val iteri : 'a int64Array -> ('a -> int64 -> 'b) -> unit
val init : int64 -> (int64 -> int64) -> unit
val copy : int64 int64Array -> int64 int64Array
val mapi : 'a int64Array -> ('a -> int64) -> unit

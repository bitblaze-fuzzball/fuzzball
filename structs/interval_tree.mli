type interval = { low : int64; high : int64; }

type interval_node = {
  data : interval;
  mutable left : interval_node;
  mutable right : interval_node;
  mutable parent : interval_node;
  mutable red : bool;
  is_nil : bool;
}

exception IntersectingRange
exception ConsumedRange

type tree = {
  mutable root : interval_node;
  nil : interval_node; (* end sentinel *)
  mutable count : int;
}

(** give me the leftmost interval in the tree *)
val min : tree -> interval_node

(** make a new interval tree *)
val make_tree : unit -> tree

(** is the tree empty *)
val empty_p : tree -> bool

(** how many elements does the tree have *)
val count : tree -> int

(** find the interval that intersects this one. Raises not found if no interval matches*)
val find : tree -> interval -> interval

(** like find, but returns None / Some interval *)
val find_interval : tree -> interval -> interval option

(** fold across tree elements *)
val fold_left : ('a -> interval -> 'a) -> 'a -> tree -> 'a

(** map over tree elements *)
val map : (interval -> 'a) -> tree -> 'a list

(** iter over tree elements *)
val iter : (interval -> unit) -> tree -> unit

(** display an interval on channel *)
val print_interval : out_channel -> interval -> unit

(** display the tree on channel *)
val print_using : out_channel -> tree -> unit

(** insert the interval into the tree *)
val insert : tree -> interval -> unit

(** remove the interval from the tree, possibly splitting other intervals. *)
val remove_range : tree -> interval -> unit

(** is the second interval contained in the first? *)
val is_in : interval -> interval -> bool

(** clears the interval tree *)
val clear : tree -> unit

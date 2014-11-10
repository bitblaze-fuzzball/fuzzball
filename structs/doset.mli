(* $Id: doset.mli,v 1.1 2005/12/10 22:37:52 ruml Exp ruml $

   ordered set with destructive modification.

   An alternative to the standard library Set module, which is functional.
   Most operations are log time..
*)


type 'a t

type 'a node


val make_with : ?equals:(('a -> 'a -> bool) option) -> ('a -> 'a -> bool) -> 'a -> 'a t
  (** predicate should be true iff elements are in the desired order
    (including equality!). initial element is only used for initialization
    and is not added to the set.  Note: this initial element will never be
    garbage-collected! *)

val insert : 'a t -> 'a -> unit

val insert_node : 'a t -> 'a -> 'a node
  (** returns a node that can be used for deletion *)

val data : 'a node -> 'a

val delete : 'a t -> 'a node -> unit

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val map : ('a -> 'b) -> 'a t -> 'b list

val iter : ('a -> unit) -> 'a t -> unit
  (** in order from min to max.  tolerates any modifications during
      traversal - elements are copied to an intermediate array. *)

val raw_iter : ('a node -> unit) -> 'a t -> unit
  (** in order from min to max.  tolerates any modifications during
      traversal - elements are copied to an intermediate array. *)


(* following two iterators are in order *)

val unsafe_iter : ('a -> unit) -> 'a t -> unit
  (** tolerates only addition after current element.  Current element
      cannot be removed and elements added before will not be
      traversed.  *)

val unsafe_iter2 : ('a -> unit) -> 'a t -> unit
  (** tolerates only removal of current element. even additions after
      may or may not be seen. *)

val visit_interval : ('a -> bool) -> ('a -> bool) -> ('a node -> unit) -> 'a t -> unit
  (** [sats_lower], [sats_upper], [f].  evaluates [f] on nodes that are
      high enough to satisfy the lower bound and low enough to satisfy the
      upper bound.  Doesn't tolerate modification of the tree!  Order not
      specified. *)

val min : 'a t -> 'a node
  (** element is not removed *)

val find : 'a t -> 'a -> 'a
  (** returns an arbitrary element of the set that compares equal by the
    ordering predicate to the given object, or raises Not_found *)

val empty_p : 'a t -> bool

val count : 'a t -> int
  (** constant time *)

val print_using : out_channel -> (out_channel -> 'a -> unit) -> 'a t -> unit
  (** takes a function that prints an element.  indentation is already
    taken care of *)

val resort : 'a t -> unit

val clear : 'a t -> unit

val check : 'a t -> int -> unit
val check_no_count : 'a t -> unit

val print_tree : 'a t -> (out_channel -> 'a -> unit) -> unit

(** outside of usual interface *****)

val test1 : int -> int -> bool -> unit
val test2 : int -> int -> bool -> unit
val test3 : int -> int -> bool -> unit

val get_parent : 'a node -> 'a node
val get_right_child : 'a node -> 'a node
val get_left_child : 'a node -> 'a node
val get_nil : 'a t -> 'a node
(* EOF *)

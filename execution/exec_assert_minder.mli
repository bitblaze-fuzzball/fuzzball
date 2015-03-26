(** Keeps track of how many weird behaviors fuzzball has encountered during the run.
    Actually fails when too many have been added. *)

val reset : unit -> unit

val g_assert : bool -> int -> string -> unit

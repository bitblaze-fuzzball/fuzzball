(* logs table of indirect jump targts *)

(* I didn't want to expose the table, really, but I had to define the type
   to make the compiler happy *)
val assoc : (int64, (int64, int64) Hashtbl.t) Hashtbl.t

(* Target to output the addresses to *)
val output_loc : string ref

(* Add an element to the table *)
val add : int64 -> int64 -> unit

(* Clear the table (not really needed since I had to expose the table *)
val clear : unit -> unit

(* flush the association list to the location defined in output_loc *)
val flush : unit -> unit

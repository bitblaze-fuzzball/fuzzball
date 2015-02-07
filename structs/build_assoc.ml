(* builds an association list using a hashtable.  An association list
   is really just a list of lists, where the head of each list is a
   key pointing at it's associates.

   In fuzzball, we use this to build out an address table for indirect jumps.
*)

let add table key element =
  let prev =
    try 
      Hashtbl.find table key
    with Not_found ->
      Hashtbl.create 10 in
  Hashtbl.replace prev element element

let clear table = Hashtbl.clear table


let print table print_key print_associates =
  let printer key associates =
    print_key key;
    print_associates associates in
  Hashtbl.iter printer table

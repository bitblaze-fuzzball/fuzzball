(*
    Modified interval tree, taken from CLRS third edition -- pg 348-354
*)

type interval = {
  low  : int64;
  high : int64;
}

let make_interval low high =
  assert (high >= low);
  assert (low >= 0);
  { low = Int64.of_int low;
    high = Int64.of_int high;}

let make_interval_wcheck low high =
  assert (high >= low);
  { low = low;
    high = high;}

let intersects i1 i2 =
  (i1.low >= i2.low && i1.low <= i2.high) (* left endpoint i1 in i2 *)
  || (i1.high <= i2.high && i1.high >= i2.low) (* right of i1 in i2 *)
  || (i2.low >= i1.low && i2.low <= i1.high)      (* left of l2 in l1 *)
  || (i2.high <= i1.high && i2.high >= i1.low) (* right of l2 in l1 *)

let compare i1 i2 =
  if intersects i1 i2 then
    0
  else
    (let l1 = i1.low
    and l2 = i2.low in
     if l1 < l2
     then ~-1
     else 1)
      
let abbuts i1 i2 =
  i2.low = i1.high ||
  i2.high = i1.low ||
  i1.low = i2.high ||
  i1.high = i2.low

let is_in i1 i2 =
    i2.low >= i1.low &&
    i2.high <= i1.high

let safe_merge i1 i2 =
  assert (intersects i1 i2);
  make_interval_wcheck 
    (Pervasives.min i1.low i2.low)
    (Pervasives.max i1.high i2.high)

let fast_merge i1 i2 =
  make_interval_wcheck
    (Pervasives.min i1.low i2.low)
    (Pervasives.max i1.high i2.high)


let merge = safe_merge


let check_interval i =
  assert (i.low <= i.high)


let remove i1 i2 = (** order matters -- remove i2 from i1 *)
  (* cases --
     a overlapping, i2 to left
     b overlapping, i2 to right
     c overlapping, i2 consumes (empty range return)
     d overlapping, i1 consumes (two ranges come back
     e no intersection *)
  let save_high_1 = i1.high <= i2.high
  and save_high_2 = i2.high <= i1.high in
  let i1_in_i2 = i1.low >= i2.low && save_high_1
  and i2_in_i1 = i2.low >= i1.low && save_high_2
  and no_intersect = i1.high < i2.low || i2.high < i1.low
  and i1_left = i1.low < i2.low && i1.high >= i2.low && save_high_1
  and i2_left = i2.low < i1.low && i2.high >= i2.low && save_high_2 in
  if no_intersect
  then [i1]
  else if i1_in_i2
  then []
  else if i2_in_i1
  then
    (if i2.high = i1.high
     then [(make_interval_wcheck i1.low (Int64.sub i2.low Int64.one))]
     else if i2.low = i1.low
     then [(make_interval_wcheck (Int64.add i2.high Int64.one) i1.high)]
     else [(make_interval_wcheck i1.low (Int64.sub i2.low Int64.one));
	   (make_interval_wcheck (Int64.add i2.high Int64.one) i1.high);])
  else if i1_left
  then [(make_interval_wcheck i1.low (Int64.sub i2.low Int64.one))]
  else if i2_left
  then [(make_interval_wcheck (Int64.add i2.high Int64.one) i1.high)]
  else failwith "I don't think this code is reachable."


module IntervalMap =
  Map.Make(
    struct
      type t = interval
      let compare = compare
    end)

include IntervalMap

type claimStatus =
| Allocate  
| Deallocate

type element = {
  key : interval;
  at : claimStatus;
}

exception AllocatingAllocated of (interval * interval)
exception ReadingUnwritten of interval
exception ReadingUnallocated of interval
exception DoubleFree of (interval * interval)
exception DeallocatingUnallocated of interval
exception DeallocationSizeMismatch of (interval * interval)
exception WritingUnallocated of interval

let optional_find imap key =
  try
    Some (IntervalMap.find key imap)
  with Not_found -> None


let rec remove_all imap key =
  (* not tail recursive. Very dangerous *)
  try
    ignore (IntervalMap.remove key imap);
    remove_all imap key
  with Not_found -> ()


let rec attempt_allocate imap attempted_range =
  match optional_find imap attempted_range with
  | Some collision ->
    (match collision.at with
    | Deallocate ->
      (* I don't care that the range was deallocated. *)
      ignore (IntervalMap.remove collision.key imap);
      attempt_allocate imap attempted_range
    | Allocate -> raise (AllocatingAllocated (collision.key, attempted_range)))
  | None ->
    IntervalMap.add
      attempted_range
      { key = attempted_range;
	at = Allocate}
      imap


let attempt_deallocate alloc_map io_map attempted_range =
  (* needs both maps so it can remove a range from the written tree as well *)
  match optional_find alloc_map attempted_range with
  | Some collision ->
    (match collision.at with
    | Deallocate -> raise (DoubleFree (collision.key, attempted_range))
    | Allocate ->
      (if ((collision.key.low = attempted_range.low)
	   && (collision.key.high = attempted_range.high))
       then (ignore (IntervalMap.remove attempted_range alloc_map);
	     remove_all io_map attempted_range;
	     IntervalMap.add attempted_range { key = attempted_range; at = Deallocate;} alloc_map)
       else raise (DeallocationSizeMismatch (collision.key, attempted_range))))
  | None -> raise (DeallocatingUnallocated attempted_range)
      

let attempt_read alloc_map io_map attempted_range =
  match optional_find alloc_map attempted_range with
  | None -> raise (ReadingUnallocated attempted_range)
  | Some _ ->
    (match optional_find io_map attempted_range with
    | None -> raise (ReadingUnwritten attempted_range)
    | Some written_range ->
      if not (is_in attempted_range written_range)
      then raise (ReadingUnwritten attempted_range))

let attempt_write alloc_map io_map attempted_range =
    match optional_find alloc_map attempted_range with
  | None -> raise (WritingUnallocated attempted_range)
  | Some _ ->
    ((match optional_find io_map attempted_range with
    | None -> ()
    | Some _ -> remove_all io_map attempted_range);
     IntervalMap.add attempted_range attempted_range io_map)
      

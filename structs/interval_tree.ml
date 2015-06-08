(*
    Modified interval tree, taken from CLRS third edition -- pg 348-354
*)

type provenance =
| Internal
| External
| Random

let prov_to_string = function
  | Internal -> "Internal"
  | External -> "External"
  | Random -> "Random"

type interval = {
  low  : int64;
  high : int64;
  mutable provenance : provenance;
  mutable accessed : int;
}

let make_interval ?(prov = Internal) low high =
  assert (high >= low);
  assert (low >= 0);
  { low = Int64.of_int low;
    high = Int64.of_int high;
    provenance = prov;
    accessed = 0;
  }

let make_interval_wcheck ?(prov = Internal) low high =
  assert (high >= low);
  { low = low;
    high = high;
    provenance = prov;
    accessed = 0;}

let intersects i1 i2 =
  (i1.low >= i2.low && i1.low <= i2.high) (* left endpoint i1 in i2 *)
  || (i1.high <= i2.high && i1.high >= i2.low) (* right of i1 in i2 *)
  || (i2.low >= i1.low && i2.low <= i1.high)      (* left of l2 in l1 *)
  || (i2.high <= i1.high && i2.high >= i1.low) (* right of l2 in l1 *)

let compare i1 i2 =
  if intersects i1 i2 then
    0 else
    let l1 = i1.low
    and l2 = i2.low in
    if l1 < l2 then
      ~-1 else
      1
      
let abbuts i1 i2 =
  let i1i2_highlow = Int64.sub i2.low i1.high   (* i1 abbuts i2 to the left *)
  and i1i2_lowhigh = Int64.sub i1.low i2.high in (* i1 abbuts i2 to the right *)
    (Int64.one = i1i2_highlow) ||
    (Int64.one = i1i2_lowhigh)

let is_in i1 i2 =
    (* is i2 in i1 *)
  i2.low >= i1.low &&
    i2.high <= i1.high

let safe_merge i1 i2 =
  let ret = make_interval_wcheck ~prov:i1.provenance
    (Pervasives.min i1.low i2.low)
    (Pervasives.max i1.high i2.high) in
  assert (intersects ret i1);
  assert (intersects ret i2);
  assert ((intersects i1 i2) || (abbuts i1 i2));
  ret

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


let optional_find imap key =
  try
    Some (IntervalMap.find key imap)
  with Not_found -> None

let rec remove_all imap key =
  (* removes all keys that collide with the specified key *)
  let imap' = IntervalMap.remove key imap in
  if (IntervalMap.cardinal imap') = (IntervalMap.cardinal imap)
  then imap (* or imap' -- either is ok *)
  else remove_all imap' key
    


let rec attempt_allocate imap attempted_range =
  match optional_find imap attempted_range with
  | Some collision ->
    (match collision.at with
    | Deallocate ->
      (* I don't care that the range was deallocated. *)
      attempt_allocate (IntervalMap.remove collision.key imap) attempted_range
    | Allocate -> raise (AllocatingAllocated
			   {original_interval = collision.key;
			    proposed_conflict = attempted_range; }))
  | None ->
    IntervalMap.add
      attempted_range
      { key = attempted_range;
	at = Allocate}
      imap


let attempt_deallocate alloc_map io_map attempted_range =
  (* needs both maps so it can remove a range from the written tree as well
     returns alloc_map', io_map' *)
  match optional_find alloc_map attempted_range with
  | Some collision ->
    (match collision.at with
    | Deallocate -> raise (DoubleFree {original_interval = collision.key;
				       proposed_conflict = attempted_range;})
    | Allocate ->
      (if ((collision.key.low = attempted_range.low)
	   && (collision.key.high = attempted_range.high))
       then
	  (let alloc_map' = remove_all alloc_map attempted_range
	  and io_map' = remove_all io_map attempted_range in
	   (* writes are constrained not to fall across allocs *)
	   IntervalMap.add attempted_range
	     { key = attempted_range; at = Deallocate;} alloc_map',
	   io_map')
       else raise (DeallocationSizeMismatch {original_interval = collision.key;
					     proposed_conflict = attempted_range})))
  | None -> raise (DeallocatingUnallocated attempted_range)
      

let attempt_read alloc_map io_map attempted_range =
  match optional_find alloc_map attempted_range with
  | None -> raise (ReadingUnallocated attempted_range)
  | Some interval -> 
	(match interval.at with
	| Deallocate -> raise (ReadingUnallocated attempted_range)
	| Allocate -> 
		(match optional_find io_map attempted_range with
		| None -> raise (ReadingUnwritten attempted_range)
		| Some written_range ->
		if not (is_in written_range attempted_range)
		then raise (ReadingUnwritten attempted_range)
		else written_range.accessed <- 0)
			)


let copy imap =
  (* re-write with a fold *)
  let to_return = ref IntervalMap.empty in
  IntervalMap.iter (fun key element -> to_return := IntervalMap.add key element !to_return) imap;
  !to_return


let find_all base_imap key =
  let rec helper imap =
    match (optional_find imap key) with
    | None -> []
    | Some i -> i.key::(helper (IntervalMap.remove i.key imap)) in
  List.sort (fun a b -> if a.low <= b.low then ~-1 else 1) (helper (copy base_imap))


let attempt_write alloc_map io_map attempted_range =
  let prov = attempted_range.provenance in
    match optional_find alloc_map attempted_range with
  | None -> raise (WriteBeforeAllocated attempted_range)
  | Some interval ->
    (match interval.at with
    | Deallocate -> raise (WriteAfterDeallocated attempted_range)
    | Allocate ->
      (* write range in allocated range *)
      (if is_in interval.key attempted_range
       then
	  (let merge_range = {low = (Int64.sub attempted_range.low Int64.one);
			      high = (Int64.add attempted_range.high Int64.one);
			      provenance = prov;
			      accessed = 1; (* acccessed at least once, by this write *)} in
	   (match optional_find io_map merge_range with
	   | None -> (attempted_range.accessed <- 1;
		      attempted_range,
		      IntervalMap.add attempted_range attempted_range io_map)
	   (* no collision *)
	   | Some write ->
	     (let io_map' = remove_all io_map write
	     and to_add = safe_merge write attempted_range in
	      to_add.provenance <- prov;
	      to_add.accessed <- write.accessed + 1;
	      (* it's a write, set the access as appropriate *)
	      to_add, IntervalMap.add to_add to_add io_map')))
       else
	  (* really, this is a new kind of error, where we're writing
	     off the end / before the start of allocated memory. *)
	  begin
	    let conflicting_ranges = find_all alloc_map attempted_range in
	    match conflicting_ranges with
	    | [] -> assert false (* must be at least interval! *)
	    | [spill_range] ->
	      begin
	      let before = spill_range.low > attempted_range.low
	      and after = spill_range.high < attempted_range.high
	      and conflict_desc = { original_interval = spill_range;
				    proposed_conflict = attempted_range} in
	      if before && after
	      then raise (WriteAcross conflict_desc)
	      else if before
	      then raise (WriteBefore conflict_desc)
	      else if after
	      then raise (WriteAfter conflict_desc)
	      else assert false
	      end
	    | _ -> raise (MultiWriteConflict
			    { existing_regions = conflicting_ranges;
			      proposed = attempted_range; })
	  end

      ))


let print_interval i =
  Printf.eprintf "%LX to %LX\n" i.low i.high

(* Test Helper Functions *)

let make_random_interval max_val =
  let a = Random.int max_val
  and b = Random.int max_val in
  if a > b then
    make_interval b a else
    make_interval a b


let make_random_overlapping_intervals max_val =
  let a = Random.int max_val
  and b = Random.int max_val
  and c = Random.int max_val
  and d = Random.int max_val in
  let as_list = List.sort Pervasives.compare [a; b; c; d;] in
  let nth = List.nth as_list in
  [make_interval (nth 0) (nth 2);
   make_interval (nth 1) (nth 3);]


let make_random_disjoint_intervals max_val =
  let a = Random.int max_val
  and b = Random.int max_val
  and gap = (Random.int max_val) + 1
  and c = Random.int max_val
  and d = Random.int max_val in
  let as_list = List.sort Pervasives.compare [a; b; c; d;] in
  let nth = List.nth as_list in
  [make_interval (nth 0) (nth 1);
   make_interval ((nth 2) + gap) ((nth 3) + gap);]


let random_adjacent ?(max_size = 100) interval =
  let isize = Int64.of_int ((Random.int max_size) +  1)
  and flip = Random.bool () in
  if flip then
    (* we're before *)
    let hp = Int64.sub interval.low Int64.one
    and lp = Int64.sub interval.low isize in
    {low = lp;
     high = hp;
     provenance = Internal;
     accessed = 0;} else
    (* we're after *)
    let lp = Int64.add interval.high Int64.one
    and hp = Int64.add interval.high isize in
    {low = lp;
     high = hp;
     provenance = Internal;
     accessed = 0;}

(** Interval testing code **)

let test_intersect num_tests = 
  (* Generate num_tests guaranteed to overlap sets of intervals,
     assert they intersect *)
  for i = 1 to num_tests do
    begin
      begin
	match make_random_overlapping_intervals 10_000 with
	| [i1; i2] -> assert (intersects i1 i2)
	| _ -> failwith "Impossible"
      end;
      begin
	match make_random_disjoint_intervals 10_000 with
	| [i1; i2] -> assert(not (intersects i1 i2))
	| _ -> failwith "Impossible"
      end
    end
  done

let test_merge num_tests =
  for i = 1 to num_tests do
    match make_random_overlapping_intervals 10_000 with
    (* safe_merge contains assertion tests that i1 and i2 intersect merged region. *)
    | [i1; i2] -> ignore (safe_merge i1 i2)
    | _ -> failwith "Impossible"
  done

let test_abbuts num_tests =
  for i = 1 to num_tests do
    let base_interval = make_random_interval 10_000 in
    let adjacent = random_adjacent base_interval in
    assert (abbuts base_interval adjacent);
    assert (abbuts adjacent base_interval);
    match make_random_disjoint_intervals 10_000 with
    | [i1; i2] -> assert (not (abbuts i1 i2))
    | _ -> failwith "impossible"
  done

let check_intervals num_tests =
  (* checks intersection, makes sure there aren't false positives *)
  test_intersect num_tests;
  (* runs merge code, makes sure merging regions intersect their child regions *)
  test_merge num_tests;
  (* makes sure that abbuts is correct *)
  test_abbuts num_tests

(* Interval Tree tests *)

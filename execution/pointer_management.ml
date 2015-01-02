open Exec_exceptions

exception Safe
exception Overlapping_Alloc

let json_addr i64 = `String (Printf.sprintf "0x%08Lx" i64) 

class pointer_management = object(self)

(* This is where addresses start being handed out from in cgc_syscalls (1342177280)*)
val heap_start = 0x50000000L
(* in concrete_memory it appears that the heap is 0x0100001L long (1343225857) *)
val heap_end = 0x50100001L
(* Thi is where the stack is supposed to start *)
val stack_start = 0xbaaab000L
val mutable stack_end = 0xbaaab000L

val alloc_table = Hashtbl.create 100
val dealloc_table = Hashtbl.create 100
val stack_table = Hashtbl.create 100

method update_stack_end esp =
  if self#greater_than esp stack_end then (
    let removeList = Hashtbl.fold (fun addr _ accum ->
      if self#less_than addr stack_end then
	addr :: accum
      else
	accum
    ) stack_table [] in
    List.iter (fun addr -> Hashtbl.remove stack_table addr) removeList
  );
    stack_end <- esp

method private less_than a b =
  (Int64.compare a b) < 0

method private less_than_equal a b =
  (Int64.compare a b) <= 0

method private greater_than a b =
  (Int64.compare a b) > 0

method private greater_than_equal a b =
  (Int64.compare a b) >= 0

method private equal a b =
  (Int64.compare a b) = 0

method private not_equal a b =
  (Int64.compare a b) <> 0

method private is_contained v1 v2 r1 r2 =
  self#greater_than_equal v1 r1 && self#less_than_equal v2 r2

method private is_non_overlapping v1 v2 r1 r2 =
  (self#less_than v2 r1) || (self#greater_than v1 r2)

method private is_overlapping_not_contained v1 v2 r1 r2 =
  ((self#less_than v1 r1) && (self#greater_than v2 r1) && (self#less_than_equal v2 r2)) ||
    ((self#greater_than v1 r1) && (self#less_than_equal v1 r2) && (self#less_than v1 r1)) ||
    ((self#less_than v1 r1) && (self#greater_than v2 r2))

method private is_overlapping v1 v2 r1 r2 =
  self#is_contained v1 v2 r1 r2 &&
    self#is_overlapping_not_contained v1 v2 r1 r2

method add_alloc addr len = 
  let start_addr = addr
  and end_addr = Int64.add addr len in
  Hashtbl.iter
    (fun key set ->
      let start_addr2 = key
      (* Don't need the whole array here -- just the length of the allocation *)
      and end_addr2 = Int64.add key (Int64Array.length set) in  
      if self#is_overlapping start_addr end_addr start_addr2 end_addr2
      then (Printf.printf "\nNew block (%s - %s) overlaps with existing block (%s - %s)\n"
	      (Int64.to_string start_addr)
	      (Int64.to_string end_addr)
	      (Int64.to_string start_addr2)
	      (Int64.to_string end_addr2);
	    raise Overlapping_Alloc))
    alloc_table;
		
  Hashtbl.add alloc_table addr (Int64Array.make len false);

  let updated_deallocs =
    Hashtbl.fold (fun key value accum ->
      let start_addr2 = key
      and end_addr2 = Int64.add key value in
(* These cases are overly verbose with checking for clarity
   feel free to optimize the unnecessary checks away *)

      (* blocks are equal or new block contains all of old block
	 remove the old block entirely *)
      if self#is_contained start_addr2 end_addr2 start_addr end_addr then
	(key, Int64.zero) :: accum
      else (
						(* new block is contained within old block
						split the old block into two blocks *)
	if self#is_contained start_addr end_addr start_addr2 end_addr2 then (
	  let new_length1 = Int64.sub start_addr2 addr
	  and new_length2 = Int64.sub end_addr end_addr2 in
	  (key, new_length1) :: (end_addr2, new_length2) :: accum
	)
	else (
							(* new block contains start of old block
							   remove the head of old block and replace with new block *)
	  if self#less_than_equal start_addr2 start_addr &&
	    ((self#greater_than end_addr2 start_addr) && (self#less_than_equal end_addr2 end_addr)) then (
	      let new_length = Int64.sub end_addr end_addr2 in
	      (key, Int64.zero) :: (end_addr2, new_length) :: accum
	      )
	  else (
					  			(* old block contains start of new block
					  			   update the length of the old block *)
	    if (self#less_than_equal start_addr start_addr2) &&
	      ((self#greater_than end_addr start_addr2) && (self#less_than_equal end_addr end_addr2)) then (
		let new_length = Int64.sub start_addr2 start_addr in
		(key, new_length) :: accum
		)
	    else
	      accum
	  )
	)
      )
    ) dealloc_table [] in

  List.iter (fun (new_addr, new_len) ->
    if Hashtbl.mem dealloc_table new_addr then (
      if (self#equal new_len Int64.zero) then
	Hashtbl.remove dealloc_table new_addr
      else
	Hashtbl.replace dealloc_table new_addr new_len
    )
    else (
      if (self#equal new_len Int64.zero) then
	()
      else
	Hashtbl.add dealloc_table new_addr new_len
    )
  ) updated_deallocs

val mutable info_reporter = None
	  
method set_reporter (r : ((string * Yojson.Safe.json) list -> unit)) =
  info_reporter <- Some r
	    
method private report l =
  match info_reporter with
  | None -> ()
  | Some r -> r l

(* In this function we check several things, some of which might not
   be considered a weakness/vulnerability, we should check the decree spec:

		 1) Has this pointer been free previously?
		 2) Is this pointer currently pointing to the HEAD of a block of
		    previously allocated memory?
		 3) Does the deallocation length specified match the full size of
		    the block that was originally allocated? (Maybe they can do partial
		    deallocation? Or free multiple blocks at once?)
*)

method add_dealloc addr len =
	(* Has this pointer already been deallocated? *)
  if Hashtbl.mem dealloc_table addr then
    (self#report [("tag", (`String ":double-free"));
		  ("double-freed-addr", (json_addr addr))];
     raise Double_Free);

	(* Does this pointer point to memory that has been allocated? *)
  if not (Hashtbl.mem alloc_table addr) (* don't need the array for the alloc table here *)
  then raise Dealloc_Not_Alloc;

	(* This might not be "exception-worthy"
		 We need to check the spec to see how this function is allowed to
		 behave. Can you "under-free" or "over-free"? Would the block be
		 resized in the under-free case? Or in the over-free case, can you
		 free multiple blocks at once (I don't know how you could guarantee
		 contiguous memory from userspace, but maybe you can?)?
	*)
  let old_len = Int64Array.length (Hashtbl.find alloc_table addr) in (* don't need the array here *)
  if (self#not_equal len old_len)
  then raise Alloc_Dealloc_Length_Mismatch;

  Hashtbl.remove alloc_table addr;
  Hashtbl.add dealloc_table addr len

method is_safe_read addr len =
  let start_addr = addr
  and end_addr = Int64.add addr len
  and is_safe = ref false in
	
	(* fully in the heap *)
  if self#is_contained start_addr end_addr heap_start heap_end then (
		(* is it in an allocated block *)
    try
      Hashtbl.iter (fun key set ->
	let start_addr2 = key
	and end_addr2 = Int64.add key (Int64Array.length set) in
				(* fully contained within an allocated block *)
	if self#is_contained start_addr end_addr start_addr2 end_addr2 then (
	  let start_index = Int64.sub start_addr start_addr2 in
	  for offset = 0 to ((Int64.to_int len) - 1) do
	    if not (Int64Array.get set (Int64.add start_index (Int64.of_int offset)))
	    then raise Uninitialized_Memory;
	  done;
	  raise Safe)) alloc_table;
    with
    | Safe -> is_safe := true;)
  else (
		(* overlapping unsafe memory between heap and stack *)
    if (self#is_overlapping start_addr end_addr heap_end stack_end)  ||
      (* overlapping the heap but not contained *)
      (self#is_overlapping_not_contained start_addr end_addr heap_start heap_end) then
      is_safe := false
    else (
      if (self#is_contained start_addr end_addr stack_end stack_start) then (
	let start_addr_ref = ref start_addr in
	while (Int64.compare !start_addr_ref end_addr) <> 0 do
	  if not (Hashtbl.mem stack_table !start_addr_ref) then (
	    self#report [("tag", (`String ":uninit-read"));
			 ("subtag", (`String ":stack-missing"));
			 ("stack-start", (json_addr stack_start));
			 ("stack-end", (json_addr stack_end));
			 ("read-start", (json_addr start_addr));
			 ("read-len", (`Int (Int64.to_int len)));
			];
	    raise Uninitialized_Memory
	  );
	  start_addr_ref := Int64.add !start_addr_ref (Int64.one)
	done
      );
			(* otherwise we assume we're safe for now *)
      is_safe := true
    );
  );
  !is_safe

method is_safe_write addr len =
  let start_addr = addr
  and end_addr = Int64.add addr len
  and is_safe = ref false in
  
	(* fully in the heap *)
  if self#is_contained start_addr end_addr heap_start heap_end then (
		(* is it in an allocated block *)
    try
      Hashtbl.iter (fun key set ->
	let start_addr2 = key
	and end_addr2 = Int64.add key (Int64Array.length set) in
				(* fully contained within an allocated block *)
	if self#is_contained start_addr end_addr start_addr2 end_addr2 then (
	  let start_index = Int64.sub start_addr start_addr2 in
	  for offset = 0 to ((Int64.to_int len) - 1) do
	    Int64Array.set set (Int64.add start_index (Int64.of_int offset)) true
	  done;
	  raise Safe
	)
      ) alloc_table;
    with
    | Safe -> is_safe := true;
  )
  else (
		(* overlapping unsafe memory between heap and stack *)
    if (self#is_overlapping start_addr end_addr heap_end stack_end)  ||
			 (* overlapping the heap but not contained *)
      (self#is_overlapping_not_contained start_addr end_addr heap_start heap_end) then
      is_safe := false
    else (
      if (self#is_contained start_addr end_addr stack_end stack_start) then (
	let start_addr_ref = ref start_addr in
	while (Int64.compare !start_addr_ref end_addr) <> 0 do
	  Hashtbl.replace stack_table !start_addr_ref true;
	  start_addr_ref := Int64.add !start_addr_ref (Int64.one)
	done
      );
      (* otherwise we assume we're safe for now *)
      is_safe := true
    )
  );
  !is_safe
    
method clear =
	        (* Hashtbl.clear and Hashtbl.reset have the same
	           visible semantics; the difference is just a time/space
	        tradeoff. But Hashtbl.reset is a fairly recent
	        addition: sticking with clear retains compatibilty
	        with 3.x versions of OCaml. *)
  Hashtbl.clear alloc_table;
  Hashtbl.clear dealloc_table;
  
method add_alloc_dirty addr set =
  Hashtbl.add alloc_table addr set
    
method add_dealloc_dirty addr len =
  Hashtbl.add dealloc_table addr len

method construct_deep_copy =
  let copy = new pointer_management in
  Hashtbl.iter (fun key set ->
    copy#add_alloc_dirty key set
  ) alloc_table;
  Hashtbl.iter (fun key len ->
    copy#add_dealloc_dirty key len
  ) dealloc_table;
  copy

end

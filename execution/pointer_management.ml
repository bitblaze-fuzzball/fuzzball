open Exec_exceptions
open Exec_assert_minder

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
    (* allocate / deallocate range table *)
  val mutable assign_ranges = Interval_tree.IntervalMap.empty
    (* read / write range table *)
  val mutable io_ranges = Interval_tree.IntervalMap.empty

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
    flush stderr;
    if (len = Int64.zero) then
      (self#report [("tag", (`String ":zero-length-allocate"));
		    ("zero-length-allocate-addr", (json_addr addr))]) else
      begin
	let start_addr = addr
	and end_addr = Int64.add addr (Int64.sub len Int64.one) in
	let this_interval = { Interval_tree.low = start_addr;
			     Interval_tree.high = end_addr;
			     accessed = 0;} in
	begin
	  match !Exec_options.opt_big_alloc with
	  | None -> ()
	  | Some size ->
	    if len >= size
	    then self#report [("tag", (`String ":suspiciously-big-allocate"));
			      ("alloc size", (`String (Printf.sprintf "%Li" len)));
			      ("suspiciously-big-allocate-addr", (json_addr addr))]
	end;
	try
	  assign_ranges <- (Interval_tree.attempt_allocate
			      assign_ranges this_interval)
	with Interval_tree.AllocatingAllocated _ ->
	  raise Overlapping_Alloc
      end
	
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
    g_assert(len > Int64.zero) 100 "Pointer_management.add_dealloc";
    let start_addr = addr
    and end_addr = Int64.add addr (Int64.sub len Int64.one) in
    let this_interval = { Interval_tree.low = start_addr; Interval_tree.high = end_addr; Interval_tree.accessed = 0;} in
    try
      let assign_ranges', io_ranges' =
	Interval_tree.attempt_deallocate assign_ranges io_ranges this_interval in
      Printf.eprintf "Dealloc Range ";
      Interval_tree.print_interval this_interval;
      Printf.eprintf "\n";
      flush stderr;
      assign_ranges <- assign_ranges';
      io_ranges <- io_ranges'
    with
    | Interval_tree.DoubleFree _ ->
      (self#report [("tag", (`String ":double-free"));
		    ("double-freed-addr", (json_addr addr))];
       raise Double_Free)
    | Interval_tree.DeallocationSizeMismatch _ ->
      raise Alloc_Dealloc_Length_Mismatch
    | Interval_tree.DeallocatingUnallocated _ -> 
      raise Dealloc_Not_Alloc


  method is_safe_read addr len =
    g_assert(len > Int64.zero) 100 "Pointer_management.is_safe_read";
    let start_addr = addr
    and end_addr = Int64.add addr (Int64.sub len Int64.one) in
    let this_interval = {Interval_tree.low = start_addr;
			 Interval_tree.high = end_addr;
			 Interval_tree.accessed = 0;}
    and is_safe = ref false in
  (* fully in the heap *)
    if self#is_contained start_addr end_addr heap_start heap_end then (
      try (Interval_tree.attempt_read assign_ranges io_ranges this_interval;
	   is_safe := true) 
      with
      | Interval_tree.ReadingUnallocated _ -> is_safe := false
      | Interval_tree.ReadingUnwritten _ -> raise Uninitialized_Memory
    ) else (
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
    g_assert(len > Int64.zero) 100 "Pointer_management.is_safe_write";
    let start_addr = addr
    and end_addr = Int64.add addr (Int64.sub len Int64.one)
    and is_safe = ref false in
    let this_interval = {Interval_tree.low = start_addr; Interval_tree.high = end_addr; Interval_tree.accessed = 0} in
  (* fully in the heap *)
    if self#is_contained start_addr end_addr heap_start heap_end then (
    (* is it in an allocated block *)
      try
	let new_write, io_ranges' = 
	  Interval_tree.attempt_write assign_ranges io_ranges this_interval in
	io_ranges <- io_ranges';
	if new_write.Interval_tree.accessed > !Exec_options.opt_read_write_warn_ratio
	then self#report [("tag" , (`String ":suspicious-write"));
			  ("subtag", (`String ":write-before-read"));
			  ("write-start", (json_addr start_addr));
			  ("write-end", (json_addr end_addr));];
	is_safe := true
      with
      | Interval_tree.WriteBeforeAllocated _ -> 
	begin
	  self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":write-before-allocation"));
		      ("write-start", (json_addr start_addr));
		      ("write-end", (json_addr end_addr));];
	 is_safe := false
	end
      | Interval_tree.WriteAfterDeallocated _ -> 
	begin
	  self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":write-after-dealloc"));
		      ("write-start", (json_addr start_addr));
		      ("write-end", (json_addr end_addr));];
	 is_safe := false
	end
      | Interval_tree.WriteAcross conflict -> 
	begin
	  let legal_sub = conflict.Interval_tree.original_interval in
	  self#report [("tag", (`String ":unsafe-write"));
		       ("subtag", (`String ":write-across-allocated"));
		       ("write-start", (json_addr start_addr));
		       ("write-end", (json_addr end_addr));
		       ("legal-start", (json_addr legal_sub.Interval_tree.low));
		       ("legal-end", (json_addr legal_sub.Interval_tree.high));
		      ];
	is_safe := false
	end
      | Interval_tree.WriteBefore conflict -> 
	begin
	  let legal_sub = conflict.Interval_tree.original_interval in
	  self#report [("tag", (`String ":unsafe-write"));
		       ("subtag", (`String ":write-before-allocated"));
		       ("write-start", (json_addr start_addr));
		       ("write-end", (json_addr end_addr));
		       ("legal-start", (json_addr legal_sub.Interval_tree.low));
		       ("legal-end", (json_addr legal_sub.Interval_tree.high));
		      ];
	is_safe := false
	end
      | Interval_tree.WriteAfter conflict -> 
	begin
	  let legal_sub = conflict.Interval_tree.original_interval in
	  self#report [("tag", (`String ":unsafe-write"));
		       ("subtag", (`String ":write-after-allocated"));
		       ("write-start", (json_addr start_addr));
		       ("write-end", (json_addr end_addr));
		       ("legal-start", (json_addr legal_sub.Interval_tree.low));
		       ("legal-end", (json_addr legal_sub.Interval_tree.high));
		      ];
	is_safe := false
	end
      | Interval_tree.MultiWriteConflict conflict ->
	let existing_regions = conflict.Interval_tree.existing_regions in
	self#report [("tag", (`String ":unsafe-write"));
		     ("subtag", (`String ":write-across-multiple-allocs"));
		     ("write-start", (json_addr start_addr));
		     ("write-end", (json_addr end_addr));
		     ("segments-written-to",
		      (`Assoc
			  (List.map
			     (fun i ->
			    ("segment-touched",
			     `Assoc
				[("start_addr", (json_addr i.Interval_tree.low));
				 ("end_addr", (json_addr i.Interval_tree.high));])) 
			     existing_regions)));
		    ];
	is_safe := false
    ) else (
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
    assign_ranges <- Interval_tree.IntervalMap.empty;
    io_ranges <- Interval_tree.IntervalMap.empty

  method copy_tables ar ir =
    assign_ranges <- Interval_tree.copy ar;
    io_ranges <- Interval_tree.copy ir

  method construct_deep_copy =
    let copy = new pointer_management in
    copy#copy_tables assign_ranges io_ranges;
    copy

end

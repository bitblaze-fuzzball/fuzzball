open Exec_exceptions
open Exec_assert_minder

module IT = Interval_tree

exception Overlapping_Alloc

let json_addr i64 = `String (Printf.sprintf "0x%08Lx" i64) 

class pointer_management = object(self)

(* This is where addresses start being handed out from in cgc_syscalls (1342177280)*)
  val heap_start = 0x50000000L
(* The heap grows dynamically, but here's a reasonable bound: *)
  val heap_end = 0x60000001L
(* This is where the stack is supposed to start in Decree
   (the high address, because it grows down) *)
  val stack_start = 0xbaaab000L
  val mutable stack_end = 0xbaaab000L

  (* Conservative bounds on the area of the address space that holds the
     code and data loaded from the binary. In the future we should probably
     get this information exactly from decree_loader. *)
  val static_start = 0x08040000L
  val static_end   = 0x09000000L

  val max_addr = 0xffffffffL
    (* allocate / deallocate range table *)
  val mutable assign_ranges = IT.IntervalMap.empty
    (* read / write range table *)
  val mutable io_ranges = IT.IntervalMap.empty

  val mutable stack_table = Hashtbl.create 100

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
    self#is_contained v1 v2 r1 r2 ||
      self#is_overlapping_not_contained v1 v2 r1 r2

  method add_alloc addr len = 
    flush stderr;
    if (len = Int64.zero) then
      (self#report [("tag", (`String ":zero-length-allocate"));
		    ("zero-length-allocate-addr", (json_addr addr))]) else
      begin
	let start_addr = addr
	and end_addr = Int64.add addr (Int64.sub len Int64.one) in
	let this_interval = { IT.low = start_addr;
			     IT.high = end_addr;
			     IT.provenance = IT.Internal;
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
	  assign_ranges <- (IT.attempt_allocate
			      assign_ranges this_interval)
	with IT.AllocatingAllocated _ ->
	  (* Our allocate algorithm currently assigns locations
	     sequentially, so I don't think there's any way this could be
	     caused by a subject program bug. It has previously been
	     triggered by the pointer_management data not being properly
	     cleared/restored between paths. *)
	  Printf.printf "Overlapping alloc: 0x%08Lx+%Ld\n" addr len
	    (* raise Overlapping_Alloc *)
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
    let this_interval = { IT.low = start_addr;
			  IT.high = end_addr;
			  IT.provenance = IT.Internal;
			  IT.accessed = 0;} in
    try
      let assign_ranges', io_ranges' =
	IT.attempt_deallocate assign_ranges io_ranges this_interval in
      (*
	Printf.eprintf "Dealloc Range ";
	IT.print_interval this_interval;
	Printf.eprintf "\n";
      *)
      flush stderr;
      assign_ranges <- assign_ranges';
      io_ranges <- io_ranges'
    with
    | IT.DoubleFree _ ->
      (self#report [("tag", (`String ":double-free"));
		    ("double-freed-addr", (json_addr addr))];
       raise Double_Free)
    | IT.DeallocationSizeMismatch _ ->
	Printf.printf "Ignoring unsupported partial dealloc of 0x%Lx+0x%Lx\n"
	  addr len;
	(* We no longer treat this as indicating a program problem,
	   since it can happen legitimately, including in a specific CGC
	   malloc library with partially unmaps part of a larger allocation
	   to get an aligned block. Really we should update the interval
	   tree to support this too, but for now catching the exception
	   should be similar to treating the partial dealloc as a no-op,
	   which should be fairly safe. *)
	(* raise Alloc_Dealloc_Length_Mismatch *)
    | IT.DeallocatingUnallocated _ -> 
      raise Dealloc_Not_Alloc

  method find_read_prov addr len =
    let start_addr = addr
    and end_addr = Int64.add addr (Int64.sub len Int64.one) in
    let this_interval = {IT.low = start_addr;
			 IT.high = end_addr;
			 IT.provenance = IT.Internal;
			 IT.accessed = 0;} in
      match IT.optional_find io_ranges this_interval with
      | None -> IT.Internal
      | Some i -> i.IT.provenance
      
      
  method is_safe_read ?(prov = IT.Internal) addr len =
    g_assert(len > Int64.zero) 100 "Pointer_management.is_safe_read";
    let start_addr = addr
    and end_addr = Int64.add addr (Int64.sub len Int64.one) in
    let this_interval = {IT.low = start_addr;
			 IT.high = end_addr;
			 IT.provenance = prov;
			 IT.accessed = 0;}
    and is_safe = ref false in
  (* fully in the heap *)
    if self#is_contained start_addr end_addr heap_start heap_end then (
      try (IT.attempt_read assign_ranges io_ranges this_interval;
	   is_safe := true) 
      with
      | IT.ReadingUnallocated _ ->
	  self#report [("tag", (`String ":unsafe-read"));
		       ("subtag", (`String ":heap-unallocated"));
		       ("read-start", (json_addr start_addr));
		       ("read-len", (`Int (Int64.to_int len)));
		      ];
	  is_safe := false
      | IT.ReadingUnwritten _ -> raise Uninitialized_Memory (* _ is allocated interval that had not been written that we were trying to read from *)
    ) else (
    (* overlapping unsafe memory between heap and stack *)
      if (self#is_overlapping start_addr end_addr heap_end stack_end)  ||
      (* overlapping the heap but not contained *)
	(self#is_overlapping_not_contained start_addr end_addr heap_start heap_end) then
	is_safe := false
      else
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
	      (* Report this, but fall through to consider it safe,
		 because it's not on its own a vulnerability in the PoV
		 sense. *)
	      (* raise Uninitialized_Memory *)
	    );
	    start_addr_ref := Int64.add !start_addr_ref (Int64.one)
	  done;
	    is_safe := true
	)
	else if start_addr > 0xc1048000L && end_addr < 0xc1148000L then
	  (* Special memory range for the x87 emulator *)
	  is_safe := true
	else if end_addr > stack_start then
	  (self#report [("tag", (`String ":unsafe-read"));
			("subtag", (`String ":past-stack"));
			("stack-start", (json_addr stack_start));
			("stack-end", (json_addr stack_end));
			("read-start", (json_addr start_addr));
			("read-len", (`Int (Int64.to_int len)));
		       ];
	   is_safe := false)
	else if start_addr > static_end && end_addr < heap_start then
	  (self#report [("tag", (`String ":unsafe-read"));
			("subtag", (`String ":before-heap"));
			("read-start", (json_addr start_addr));
			("read-len", (`Int (Int64.to_int len)));
		       ];
	   is_safe := false)
	else if start_addr >= 4096L && end_addr < static_start then
	  (* Don't include the zero page here, since it should be
	     checked elsewhere when it's a problem. *)
	  (self#report [("tag", (`String ":unsafe-read"));
			("subtag", (`String ":before-program"));
			("read-start", (json_addr start_addr));
			("read-len", (`Int (Int64.to_int len)));
		       ];
	   is_safe := false)
	else
	  (* otherwise we assume we're safe for now *)
	  is_safe := true;
    );
      !is_safe

  method is_safe_write ?(prov = IT.Internal) addr len =
    g_assert(len > Int64.zero) 100 "Pointer_management.is_safe_write";
    let start_addr = addr
    and end_addr = Int64.add addr (Int64.sub len Int64.one)
    and is_safe = ref false in
    let this_interval = {IT.low = start_addr;
			 IT.high = end_addr;
			 IT.provenance = prov;
			 IT.accessed = 0} in
  (* fully in the heap *)
    if self#is_contained start_addr end_addr heap_start heap_end then (
    (* is it in an allocated block *)
      try
	let new_write, io_ranges' = 
	  IT.attempt_write assign_ranges io_ranges this_interval in
	g_assert(new_write.IT.provenance = prov)  100 "Pointer_management.is_safe_write";
	io_ranges <- io_ranges';
	if !Exec_options.opt_read_write_warn_ratio <> -1 &&
	  new_write.IT.accessed > !Exec_options.opt_read_write_warn_ratio
	then self#report [("tag" , (`String ":suspicious-write"));
			  ("subtag", (`String ":write-before-read"));
			  ("write-start", (json_addr start_addr));
			  ("write-end", (json_addr end_addr));];
	is_safe := true
      with
      | IT.WriteBeforeAllocated _ -> 
	begin
	  self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":write-before-allocation"));
		      ("write-start", (json_addr start_addr));
		      ("write-end", (json_addr end_addr));];
	 is_safe := false
	end
      | IT.WriteAfterDeallocated _ -> 
	begin
	  self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":write-after-dealloc"));
		      ("write-start", (json_addr start_addr));
		      ("write-end", (json_addr end_addr));];
	 is_safe := false
	end
      | IT.WriteAcross conflict -> 
	begin
	  let legal_sub = conflict.IT.original_interval in
	  self#report [("tag", (`String ":unsafe-write"));
		       ("subtag", (`String ":write-across-allocated"));
		       ("write-start", (json_addr start_addr));
		       ("write-end", (json_addr end_addr));
		       ("legal-start", (json_addr legal_sub.IT.low));
		       ("legal-end", (json_addr legal_sub.IT.high));
		      ];
	is_safe := false
	end
      | IT.WriteBefore conflict -> 
	begin
	  let legal_sub = conflict.IT.original_interval in
	  self#report [("tag", (`String ":unsafe-write"));
		       ("subtag", (`String ":write-before-allocated"));
		       ("write-start", (json_addr start_addr));
		       ("write-end", (json_addr end_addr));
		       ("legal-start", (json_addr legal_sub.IT.low));
		       ("legal-end", (json_addr legal_sub.IT.high));
		      ];
	is_safe := false
	end
      | IT.WriteAfter conflict -> 
	begin
	  let legal_sub = conflict.IT.original_interval in
	  self#report [("tag", (`String ":unsafe-write"));
		       ("subtag", (`String ":write-after-allocated"));
		       ("write-start", (json_addr start_addr));
		       ("write-end", (json_addr end_addr));
		       ("legal-start", (json_addr legal_sub.IT.low));
		       ("legal-end", (json_addr legal_sub.IT.high));
		      ];
	is_safe := false
	end
      | IT.MultiWriteConflict conflict ->
	let existing_regions = conflict.IT.existing_regions in
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
				[("start_addr", (json_addr i.IT.low));
				 ("end_addr", (json_addr i.IT.high));])) 
			     existing_regions)));
		    ];
	is_safe := false
    ) else (
      (* not in the heap cases *)
    (* overlapping unsafe memory between heap and stack *)
      if (self#is_overlapping start_addr end_addr heap_end stack_end)  ||
      (* overlapping the heap but not contained *)
	(self#is_overlapping_not_contained start_addr end_addr heap_start heap_end) then
	is_safe := false
      else if start_addr > 0xc1048000L && end_addr < 0xc1148000L then
	(* Special memory range for the x87 emulator *)
	is_safe := true
      else if end_addr > stack_start then
	(self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":past-stack"));
		      ("stack-start", (json_addr stack_start));
		      ("stack-end", (json_addr stack_end));
		      ("read-start", (json_addr start_addr));
		      ("read-len", (`Int (Int64.to_int len)));
		     ];
	 is_safe := false)
      else if start_addr > static_end && end_addr < heap_start then
	(self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":before-heap"));
		      ("read-start", (json_addr start_addr));
		      ("read-len", (`Int (Int64.to_int len)));
		     ];
	 is_safe := false)
      else if start_addr >= 4096L && end_addr < static_start then
	(* Don't include the zero page here, since it should be
	   checked elsewhere when it's a problem. *)
	(self#report [("tag", (`String ":unsafe-write"));
		      ("subtag", (`String ":before-program"));
		      ("read-start", (json_addr start_addr));
		      ("read-len", (`Int (Int64.to_int len)));
		     ];
	 is_safe := false)
      else
	if (self#is_contained start_addr end_addr stack_end stack_start) then (
		(
	  for i = 0 to (Int64.to_int len) - 1 do
	    Hashtbl.replace stack_table (Int64.add start_addr (Int64.of_int i))
	      true;
	  done
	);
		is_safe := true;
	)
	else
      (* otherwise we assume we're safe for now *)
	is_safe := true
    );
    !is_safe

  val mutable saved_assign_ranges = IT.IntervalMap.empty
  val mutable saved_io_ranges = IT.IntervalMap.empty
  val mutable saved_stack_table = Hashtbl.create 1
  val mutable saved_stack_end = 0L

  method make_snap =
    saved_assign_ranges <- assign_ranges;
    saved_io_ranges <- io_ranges;
    saved_stack_table <- stack_table;
    saved_stack_end <- stack_end;
    ()

  method reset =
    assign_ranges <- saved_assign_ranges;
    io_ranges <- saved_io_ranges;
    stack_table <- saved_stack_table;
    stack_end <- saved_stack_end;
    ()
      
end

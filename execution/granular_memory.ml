(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

open Exec_domain;;

module GranularMemoryFunctor =
  functor (D : DOMAIN) ->
struct
  let split64 l = ((D.extract_32_from_64 l 0), (D.extract_32_from_64 l 4))
  let split32 l = ((D.extract_16_from_32 l 0), (D.extract_16_from_32 l 2))
  let split16 l = ((D. extract_8_from_16 l 0), (D. extract_8_from_16 l 1))
    
  (* At the moment, there are still some calls to endian_i, but there
     are other places where endianness checking is missing, so assume
     little-endian for the time being.
     
     let endianness = V.Little
     
     let endian_i n k = 
     match endianness with
     | V.Little -> k
     | V.Big -> n - k  
  *)

  let endian_i n k = k


  type gran8 = Byte of D.t
	       | Absent8

  type gran16 = Short of D.t
		| Gran8s of gran8 * gran8
		| Absent16
      
  type gran32 = Word of D.t
		| Gran16s of gran16 * gran16
		| Absent32

  type gran64 = Long of D.t
		| Gran32s of gran32 * gran32
		| Absent64

  type missing_t = int -> int64 -> D.t

  let  gran8_get_byte  g8  missing addr =
    match g8 with
      | Byte l -> (l, g8)
      | Absent8 ->
	  let l = missing 8 addr in
	    (l, Byte l)

  let gran16_get_byte  g16 missing addr which =
    assert(which >= 0); assert(which < 2);
    match g16, Absent8, Absent8 with
      | Short(l),_,_ -> (D.extract_8_from_16 l (endian_i 2 which), g16)
      | Gran8s(g1, g2),_,_
      | Absent16, g1, g2 ->
	  if which < 1 then
	    let (l, g1') = gran8_get_byte g1 missing addr in
	      (l, Gran8s(g1', g2))
	  else
	    let (l, g2') = gran8_get_byte g2 missing (Int64.add addr 1L) in
	      (l, Gran8s(g1, g2'))
		
  let gran32_get_byte  g32 missing addr which =
    assert(which >= 0); assert(which < 4);
    match g32, Absent16, Absent16 with
      | Word(l),_,_ -> (D.extract_8_from_32 l (endian_i 4 which), g32)
      | Gran16s(g1, g2),_,_
      | Absent32, g1, g2 ->
	  if which < 2 then
	    let (l, g1') = gran16_get_byte g1 missing addr which in
	      (l, Gran16s(g1', g2))
	  else
	    let (l, g2') = gran16_get_byte g2 missing (Int64.add addr 2L) 
	      (which - 2) in
	      (l, Gran16s(g1, g2'))

  let gran64_get_byte  g64 missing addr which =
    assert(which >= 0); assert(which < 8);
    match g64, Absent32, Absent32 with
      | Long(l),_,_ -> (D.extract_8_from_64 l (endian_i 8 which), g64)
      | Gran32s(g1, g2),_,_
      | Absent64, g1, g2 ->
	  if which < 4 then
	    let (l, g1') = gran32_get_byte g1 missing addr which in
	      (l, Gran32s(g1', g2))
	  else
	    let (l, g2') = gran32_get_byte g2 missing (Int64.add addr 4L)
	      (which - 4) in
	      (l, Gran32s(g1, g2'))

  let gran16_get_short g16 missing addr =
    match g16 with
      | Short(l) -> (l, g16)
      | Gran8s(g1, g2) ->
	  let (b1, g1') = gran8_get_byte g1 missing addr and
	      (b2, g2') = gran8_get_byte g2 missing (Int64.add addr 1L) in
	    (D.reassemble16 b1 b2, Gran8s(g1', g2'))
      | Absent16 ->
	  let l = missing 16 addr in
	    (l, Short l)

  let gran32_get_short g32 missing addr which =
    assert(which = 0 || which = 2);
    match g32, Absent16, Absent16 with
      | Word(l),_,_ -> (D.extract_16_from_32 l (endian_i 4 which), g32)
      | Gran16s(g1, g2),_,_
      | Absent32, g1, g2 ->
	  if which < 2 then
	    let (l, g1') = gran16_get_short g1 missing addr in
	      (l, Gran16s(g1', g2))
	  else
	    let (l, g2') = gran16_get_short g2 missing (Int64.add addr 2L) in
	      (l, Gran16s(g1, g2'))

  let gran64_get_short g64 missing addr which =
    assert(which = 0 || which = 2 || which = 4 || which = 6);
    match g64, Absent32, Absent32 with
      | Long(l),_,_ -> (D.extract_16_from_64 l (endian_i 8 which), g64)
      | Gran32s(g1, g2),_,_
      | Absent64, g1, g2 ->
	  if which < 4 then
	    let (l, g1') = gran32_get_short g1 missing addr which in
	      (l, Gran32s(g1', g2))
	  else
	    let (l, g2') = gran32_get_short g2 missing (Int64.add addr 4L) 
	      (which - 4) in
	      (l, Gran32s(g1, g2'))
		
  let gran32_get_word  g32 missing addr =
    match g32 with
      | Word(l) -> (l, g32)
      | Gran16s(g1, g2) ->
	  let (s1, g1') = gran16_get_short g1 missing addr and
	      (s2, g2') = gran16_get_short g2 missing (Int64.add addr 2L) in
	    (D.reassemble32 s1 s2, Gran16s(g1', g2'))
      | Absent32 ->
	  let l = missing 32 addr in
	    (l, Word l)
	      
  let gran64_get_word  g64 missing addr which =
    assert(which = 0 || which = 4);
    match g64, Absent32, Absent32 with
      | Long(l),_,_ -> (D.extract_32_from_64 l (endian_i 8 which), g64)
      | Gran32s(g1, g2),_,_
      | Absent64, g1, g2 ->
	  if which < 4 then
	    let (l, g1') = gran32_get_word g1 missing addr in
	      (l, Gran32s(g1', g2))
	  else
	    let (l, g2') = gran32_get_word g2 missing (Int64.add addr 4L) in
	      (l, Gran32s(g1, g2'))

  let gran64_get_long  g64 missing addr  =
    match g64 with
      | Long(l) -> (l, g64)
      | Gran32s(g1, g2) ->
	  let (w1, g1') = gran32_get_word g1 missing addr and
	      (w2, g2') = gran32_get_word g2 missing (Int64.add addr 4L) in
	    (D.reassemble64 w1 w2, Gran32s(g1', g2'))
      | Absent64 -> 
	  let l = missing 64 addr in
	    (l, Long l)
	      
  let gran64_split g64 = 
    match g64 with
      | Gran32s(g1, g2) -> (g1, g2)
      | Long(l) -> let (w1, w2) = split64 l in (Word(w1), Word(w2))
      | Absent64 -> (Absent32, Absent32)

  let gran32_split g32 = 
    match g32 with
      | Gran16s(g1, g2) -> (g1, g2)
      | Word(l) ->
	  let (s1, s2) = split32 l in (Short(s1), Short(s2))
      | Absent32 -> (Absent16, Absent16)
	  
  let gran16_split g16 = 
    match g16 with
      | Gran8s(g1, g2) -> (g1, g2)
      | Short(l) ->
	  let (b1, b2) = split16 l in (Byte(b1), Byte(b2))
      | Absent16 -> (Absent8, Absent8)

  let gran16_put_byte g16 which b =
    assert(which = 0 || which = 1);
    let (g1, g2) = gran16_split g16 in
      if which < 1 then
	Gran8s(Byte(b), g2)
      else
	Gran8s(g1, Byte(b))

  let gran32_put_byte g32 which b =
    assert(which >= 0); assert(which < 4);
    let (g1, g2) = gran32_split g32 in
      if which < 2 then
	Gran16s((gran16_put_byte g1 which b), g2)
      else
	Gran16s(g1, (gran16_put_byte g2 (which - 2) b))
	  
  let gran64_put_byte g64 which b =
    assert(which >= 0); assert(which < 8);
    let (g1, g2) = gran64_split g64 in
      if which < 4 then
	Gran32s((gran32_put_byte g1 which b), g2)
      else
	Gran32s(g1, (gran32_put_byte g2 (which - 4) b))

  let gran32_put_short g32 which s =
    assert(which = 0 || which = 2);
    let (g1, g2) = gran32_split g32 in
      if which < 2 then
	Gran16s(Short(s), g2)
      else
	Gran16s(g1, Short(s))
	  
  let gran64_put_short g64 which s =
    assert(which = 0 || which = 2 || which = 4 || which = 6);
    let (g1, g2) = gran64_split g64 in
      if which < 4 then
	Gran32s((gran32_put_short g1 which s), g2)
      else
	Gran32s(g1, (gran32_put_short g2 (which - 4) s))

  let gran64_put_word g64 which w =
    assert(which = 0 || which = 4);
    let (g1, g2) = gran64_split g64 in
      if which < 4 then
	Gran32s(Word(w), g2)
      else
	Gran32s(g1, Word(w))

  let gran8_to_string g8 =
    match g8 with
      | Byte(b) -> D.to_string_8 b
      | Absent8 -> "__"

  let gran16_to_string g16 =
    match g16 with
      | Short(s) -> D.to_string_16 s
      | Gran8s(g1, g2) -> (gran8_to_string g1) ^ "|" ^ (gran8_to_string g2)
      | Absent16 -> "____"

  let gran32_to_string g32 =
    match g32 with
      | Word(w) -> D.to_string_32 w 
      | Gran16s(g1, g2) -> (gran16_to_string g1) ^ "|" ^ (gran16_to_string g2)
      | Absent32 -> "________"
	  
  let gran64_to_string g64 =
    match g64 with
      | Long(l) -> D.to_string_64 l
      | Gran32s(g1, g2) -> (gran32_to_string g1) ^ "|" ^ (gran32_to_string g2)
      | Absent64 -> "________________"

  let gran8_size g8 =
    match g8 with
      | Byte(b) -> D.measure_size b
      | Absent8 -> 1
	  
  let gran16_size g16 =
    match g16 with
      | Short(s) -> D.measure_size s
      | Gran8s(g1, g2) -> (gran8_size g1) + (gran8_size g2)
      | Absent16 -> 1

  let gran32_size g32 =
    match g32 with
      | Word(w) -> D.measure_size w
      | Gran16s(g1, g2) -> (gran16_size g1) + (gran16_size g2)
      | Absent32 -> 1

  let gran64_size g64 =
    match g64 with
      | Long(l) -> D.measure_size l
      | Gran32s(g1, g2) -> (gran32_size g1) + (gran32_size g2)
      | Absent64 -> 1


  class virtual granular_memory = object(self)
    val mutable missing : (int -> int64 -> D.t) =
      (fun _ -> failwith "Must call on_missing")
	
    method on_missing m = missing <- m
      
    method private virtual with_chunk : int64 ->
      (gran64 -> int64 -> int -> (D.t * gran64)) -> D.t option

    method private maybe_load_divided addr bits bytes load assemble =
      let mb0 = load addr and
	  mb1 = load (Int64.add addr bytes) in
	match (mb0, mb1) with
	  | (None, None) -> None
	  | _ ->
	      let b0 = (match mb0 with
			  | Some b -> b
			  | None -> (missing bits addr)) and
		  b1 = (match mb1 with
			  | Some b -> b
			  | None -> (missing bits (Int64.add addr bytes))) in
		Some (assemble b0 b1)

    method maybe_load_byte addr =
      self#with_chunk addr
	(fun chunk caddr which -> gran64_get_byte chunk missing caddr which)

    method maybe_load_short addr =
      if (Int64.logand addr 1L) = 0L then
	self#with_chunk addr
	  (fun chunk caddr which -> gran64_get_short chunk missing caddr which)
      else
	self#maybe_load_divided addr 8 1L self#maybe_load_byte D.reassemble16

    method maybe_load_word addr =
      if (Int64.logand addr 3L) = 0L then
	self#with_chunk addr
	  (fun chunk caddr which -> gran64_get_word chunk missing caddr which)
      else
	self#maybe_load_divided addr 16 2L self#maybe_load_short D.reassemble32

    method maybe_load_long addr =
      if (Int64.logand addr 7L) = 0L then
	self#with_chunk addr
	  (fun chunk caddr _ -> gran64_get_long chunk missing caddr)
      else
	self#maybe_load_divided addr 32 4L self#maybe_load_word D.reassemble64

    method load_byte addr =
      match self#maybe_load_byte addr with
	| Some b -> b
	| None ->
	    let b = missing 8 addr in 
	      self#store_byte addr b;
	      b

    method load_short addr =
      match self#maybe_load_short addr with
	| Some s -> s
	| None ->
	    let s = missing 16 addr in
	      self#store_short addr s;
	      s

    method load_word addr =
      match self#maybe_load_word addr with
	| Some w -> w
	| None ->
	    let w = missing 32 addr in
	      self#store_word addr w;
	      w

    method load_long addr =
      match self#maybe_load_long addr with
	| Some l -> l
	| None ->
	    let l = missing 64 addr in
	      self#store_long addr l;
	      l

    method private virtual store_common_fast : int64 ->
      (gran64 -> int -> gran64) -> unit

    method store_byte addr b =
      self#store_common_fast addr
	(fun chunk which -> gran64_put_byte chunk which b)
	
    method store_short addr s =
      if (Int64.logand addr 1L) = 0L then
	self#store_common_fast addr
	  (fun chunk which -> gran64_put_short chunk which s)
      else
	(* unaligned slow path *)
	let (b0, b1) = split16 s in
	  self#store_byte addr b0;
	  self#store_byte (Int64.add addr 1L) b1

    method store_word addr w =
      if (Int64.logand addr 3L) = 0L then
	self#store_common_fast addr
	  (fun chunk which -> gran64_put_word chunk which w)
      else
	(* unaligned slow path *)
	let (s0, s1) = split32 w in
	  self#store_short addr s0;
	  self#store_short (Int64.add addr 2L) s1

    method store_long addr l =
      if (Int64.logand addr 7L) = 0L then
	self#store_common_fast addr
	  (fun _ _ -> Long(l))
      else
	(* unaligned slow path *)
	let (w0, w1) = split64 l in
	  self#store_word addr w0;
	  self#store_word (Int64.add addr 4L) w1
	    
    method store_page addr p =
      (* We choose to store the page as longs here with the aim of
	 minimizing memory usage. *)
      for i = 0 to 511 do
	let bytes = String.sub p (8 * i) 8 in
	let c0 = Char.code bytes.[0] and
	    c1 = Char.code bytes.[1] and
	    c2 = Char.code bytes.[2] and
	    c3 = Char.code bytes.[3] and
	    c4 = Char.code bytes.[4] and
	    c5 = Char.code bytes.[5] and
	    c6 = Char.code bytes.[6] and
	    c7 = Char.code bytes.[7] in
	let s0 = c0 lor (c1 lsl 8) and
	    s1 = c2 lor (c3 lsl 8) and
	    s2 = c4 lor (c5 lsl 8) and
	    s3 = c6 lor (c7 lsl 8) in
	let w0 = Int64.logor (Int64.of_int s0)
	  (Int64.shift_left (Int64.of_int s1) 16) and
	    w1 = Int64.logor (Int64.of_int s2)
	  (Int64.shift_left (Int64.of_int s3) 16) in
	let long = Int64.logor w0 (Int64.shift_left w1 32) in
	  self#store_long (Int64.add addr (Int64.of_int (8 * i)))
	    (D.from_concrete_64 long);
      done
	    
    method virtual clear : unit -> unit

    method virtual measure_size : int * int * int

  (* method make_snap () = failwith "make_snap unsupported"; ()
     method reset () = failwith "reset unsupported"; () *)
  end
    
  class granular_page_memory = object(self)
    inherit granular_memory

    (* The extra page is a hacky way to not crash on address wrap-around *)
    val mem = Array.init 0x100001 (fun _ -> None)

    method private with_chunk addr fn =
      let page = Int64.to_int (Int64.shift_right addr 12) and
	  idx = Int64.to_int (Int64.logand addr 0xfffL) in
	match mem.(page) with
	  | None -> None
	  | Some page ->
	      let chunk_n = idx asr 3 and
		  which = idx land 0x7 in
	      let caddr = (Int64.sub addr (Int64.of_int which)) and
		  chunk = page.(chunk_n) in 
		match chunk with
		  | Absent64 -> None
		  | g64 ->
		      let (l, chunk') = fn page.(chunk_n) caddr which in
			page.(chunk_n) <- chunk';
			Some l

    method private get_page addr =
      let page_n = Int64.to_int (Int64.shift_right addr 12) in
	match mem.(page_n) with
	  | Some page -> page
	  | None ->
	      let new_page = Array.init 512 (fun _ -> Absent64) in
		mem.(page_n) <- Some new_page;
		new_page

    method private store_common_fast addr fn =
      let page = self#get_page addr and
	  idx = Int64.to_int (Int64.logand addr 0xfffL) in
      let chunk = idx asr 3 and
	  which = idx land 0x7 in
	page.(chunk) <- fn page.(chunk) which

    method private chunk_to_string addr =
      let page = self#get_page addr and
	  idx = Int64.to_int (Int64.logand addr 0xfffL) in
      let chunk = idx asr 3 in
	"[" ^ (gran64_to_string page.(chunk)) ^ "]"

    method clear () =
      Array.fill mem 0 0x100001 None

    method measure_size =
      let sum_some f ary =
	Array.fold_left
	  (fun n x -> n + match x with None -> 0 | Some(x') -> f x') 0 ary
      in
      let num_nodes =
	sum_some
	  (fun page -> Array.fold_left 
	     (fun n g64 -> n+ gran64_size g64) 0 page) mem
      in
      let num_entries = sum_some (fun page -> 512) mem in
	(num_entries, num_nodes, 0)
  end

  class granular_sink_memory = object(self)
    inherit granular_memory

    method private store_common_fast addr fn = ()
    method private with_chunk addr fn = None
    method clear () = ()
    method measure_size = (1, 0, 0)
  end

  class granular_hash_memory = object(self)
    inherit granular_memory

    val mem = Hashtbl.create 101

    method private with_chunk addr fn =
      let which = Int64.to_int (Int64.logand addr 0x7L) in
      let caddr = Int64.sub addr (Int64.of_int which) in
	try
	  let chunk = Hashtbl.find mem caddr in
	    match chunk with
	      | Absent64 -> None
	      | g64 -> 
		  let (l, chunk') = fn chunk caddr which in
		    Hashtbl.replace mem caddr chunk';
		    Some l
	with
	    Not_found -> None

    method private store_common_fast addr fn =
      let which = Int64.to_int (Int64.logand addr 0x7L) in
      let caddr = Int64.sub addr (Int64.of_int which) in
      let chunk = try
	Hashtbl.find mem caddr
      with Not_found ->
	Absent64
      in
	Hashtbl.replace mem caddr (fn chunk which)

    method private chunk_to_string addr =
      let caddr = Int64.logand addr (Int64.lognot 0x7L) in
      let chunk = Hashtbl.find mem caddr in
	"[" ^ (gran64_to_string chunk) ^ "]"

    method clear () =
      Hashtbl.clear mem

    method measure_size =
      let num_nodes = Hashtbl.fold (fun k v sum -> sum + gran64_size v) mem 0
      in
      let num_entries = Hashtbl.length mem in
	(num_nodes, num_entries, 0)
  end

  class granular_snapshot_memory
    (main:granular_memory) (diff:granular_memory) =
  object(self)
    val mutable have_snap = false

    method on_missing main_missing =
      main#on_missing main_missing;
      diff#on_missing
	(fun size addr ->
	   match size with
	     | 8 -> main#load_byte addr
	     | 16 -> main#load_short addr
	     | 32 -> main#load_word addr
	     | 64 -> main#load_long addr
	     | _ -> failwith "Bad size in missing")

    method store_byte addr b =
      if have_snap then
	diff#store_byte addr b
      else
	main#store_byte addr b

    method store_short addr s =
      if have_snap then
	diff#store_short addr s
      else
	main#store_short addr s

    method store_word addr w =
      if have_snap then
	diff#store_word addr w
      else
	main#store_word addr w

    method store_long addr l =
      if have_snap then
	diff#store_long addr l
      else
	main#store_long addr l

    method store_page addr p =
      if have_snap then
	diff#store_page addr p
      else
	main#store_page addr p

    method maybe_load_byte addr =
      if have_snap then
	match diff#maybe_load_byte addr with
	  | Some b -> Some b
	  | None -> main#maybe_load_byte addr
      else
	main#maybe_load_byte addr

    method load_byte addr =
      if have_snap then
	match diff#maybe_load_byte addr with
	  | Some b -> b
	  | None -> main#load_byte addr
      else
	main#load_byte addr

    method maybe_load_short addr =
      if have_snap then
	match diff#maybe_load_short addr with
	  | Some s -> Some s
	  | None -> main#maybe_load_short addr
      else
	main#maybe_load_short addr

    method load_short addr =
      if have_snap then
	match diff#maybe_load_short addr with
	  | Some s -> s
	  | None -> main#load_short addr
      else
	main#load_short addr

    method maybe_load_word addr =
      if have_snap then
	match diff#maybe_load_word addr with
	  | Some w -> Some w
	  | None -> main#maybe_load_word addr
      else
	main#maybe_load_word addr

    method load_word addr =
      if have_snap then
	match diff#maybe_load_word addr with
	  | Some w -> w
	  | None -> main#load_word addr
      else
	main#load_word addr

    method maybe_load_long addr =
      if have_snap then
	match diff#maybe_load_long addr with
	  | Some l -> Some l
	  | None -> main#maybe_load_long addr
      else
	main#maybe_load_long addr

    method load_long addr =
      if have_snap then
	match diff#maybe_load_long addr with
	  | Some l -> l
	  | None -> main#load_long addr
      else
	main#load_long addr

    method measure_size =
      let (ents_d, nodes_d, conc_d) = diff#measure_size in
      let (ents_m, nodes_m, conc_m) = main#measure_size in
	(ents_d + ents_m, nodes_d + nodes_m, conc_d + conc_m)

    method clear () = 
      diff#clear ();
      main#clear ()
	
    method make_snap () =
      have_snap <- true
	
    method reset () = 
      diff#clear (); ()
  end

  class granular_second_snapshot_memory
    (mem1_2:granular_snapshot_memory) (mem3:granular_memory) =
  object(self) 
    inherit granular_snapshot_memory (mem1_2 :> granular_memory) mem3
      
    method inner_make_snap () = mem1_2#make_snap ()
  end
    
  class concrete_adaptor_memory (mem:Concrete_memory.concrete_memory) =
  object(self)
    method on_missing (m:int -> int64 -> D.t) = ()

    method store_byte  addr b = mem#store_byte  addr (D.to_concrete_8 b)
    method store_short addr s = mem#store_short addr (D.to_concrete_16 s)
    method store_word  addr w = mem#store_word  addr (D.to_concrete_32 w)
    method store_long  addr l = mem#store_word  addr (D.to_concrete_64 l)

    method load_byte  addr = D.from_concrete_8 (mem#load_byte  addr)
    method load_short addr = D.from_concrete_16(mem#load_short addr)
    method load_word  addr = D.from_concrete_32(mem#load_word  addr)
    method load_long  addr = D.from_concrete_64(mem#load_long  addr)

    method maybe_load_byte  addr = match mem#maybe_load_byte addr with
      | None -> None | Some b -> Some(D.from_concrete_8 b)
    method maybe_load_short  addr = match mem#maybe_load_short addr with
      | None -> None | Some s -> Some(D.from_concrete_16 s)
    method maybe_load_word  addr = match mem#maybe_load_word addr with
      | None -> None | Some w -> Some(D.from_concrete_32 w)
    method maybe_load_long  addr = match mem#maybe_load_long addr with
      | None -> None | Some l -> Some(D.from_concrete_64 l)
  
    method measure_size = (0, 0, mem#measure_size)

    method clear () = mem#clear ()
  end

  class concrete_maybe_adaptor_memory
    (mem:Concrete_memory.concrete_memory) = object(self)
      val mutable missing : (int -> int64 -> D.t) =
	(fun _ -> failwith "Must call on_missing")

      method on_missing m = missing <- m

      method store_byte  addr b = mem#store_byte  addr (D.to_concrete_8 b)
      method store_short addr s = mem#store_short addr (D.to_concrete_16 s)
      method store_word  addr w = mem#store_word  addr (D.to_concrete_32 w)
      method store_long  addr l = mem#store_long  addr (D.to_concrete_64 l)
      method store_page  addr p = mem#store_page  addr p

      method maybe_load_byte  addr =
	match mem#maybe_load_byte addr with
	  | Some b -> Some(D.from_concrete_8 b)
	  | None -> None

      method private unmaybe mb addr = match mb with
	| None -> missing 8 addr
	| Some b -> D.from_concrete_8 b

      method maybe_load_short  addr =
	let mb0 = mem#maybe_load_byte addr and
	    mb1 = mem#maybe_load_byte (Int64.add addr 1L) in
	  match (mb0, mb1) with
	    | (None, None) -> None
	    | _ ->
		let b0 = self#unmaybe mb0 addr and
		    b1 = self#unmaybe mb1 (Int64.add addr 1L) in
		  Some(D.reassemble16 b0 b1)

      method maybe_load_word  addr =
	let mb0 = mem#maybe_load_byte addr and
	    mb1 = mem#maybe_load_byte (Int64.add addr 1L) and
	    mb2 = mem#maybe_load_byte (Int64.add addr 2L) and
	    mb3 = mem#maybe_load_byte (Int64.add addr 3L) in
	  match (mb0, mb1, mb2, mb3) with
	    | (None, None, None, None) -> None
	    | _ ->
		let b0 = self#unmaybe mb0 addr and
		    b1 = self#unmaybe mb1 (Int64.add addr 1L) and
		    b2 = self#unmaybe mb2 (Int64.add addr 2L) and
		    b3 = self#unmaybe mb3 (Int64.add addr 3L) in
		  Some(D.reassemble32 (D.reassemble16 b0 b1)
			 (D.reassemble16 b2 b3))

      method maybe_load_long  addr =
	let mb0 = mem#maybe_load_byte addr and
	    mb1 = mem#maybe_load_byte (Int64.add addr 1L) and
	    mb2 = mem#maybe_load_byte (Int64.add addr 2L) and
	    mb3 = mem#maybe_load_byte (Int64.add addr 3L) and
	    mb4 = mem#maybe_load_byte (Int64.add addr 4L) and
	    mb5 = mem#maybe_load_byte (Int64.add addr 5L) and
	    mb6 = mem#maybe_load_byte (Int64.add addr 6L) and
	    mb7 = mem#maybe_load_byte (Int64.add addr 7L) in
	  match (mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7) with
	    | (None, None, None, None, None, None, None, None) -> None
	    | _ ->
		let b0 = self#unmaybe mb0 addr and
		    b1 = self#unmaybe mb1 (Int64.add addr 1L) and
		    b2 = self#unmaybe mb2 (Int64.add addr 2L) and
		    b3 = self#unmaybe mb3 (Int64.add addr 3L) and
		    b4 = self#unmaybe mb4 (Int64.add addr 4L) and
		    b5 = self#unmaybe mb5 (Int64.add addr 5L) and
		    b6 = self#unmaybe mb6 (Int64.add addr 6L) and
		    b7 = self#unmaybe mb7 (Int64.add addr 7L) in
		  Some
		    (D.reassemble64
		       (D.reassemble32 (D.reassemble16 b0 b1)
			  (D.reassemble16 b2 b3))
		       (D.reassemble32 (D.reassemble16 b4 b5)
			  (D.reassemble16 b6 b7)))

      method load_byte  addr  = 
	match self#maybe_load_byte addr with
	  | Some b -> b
	  | None -> missing 8 addr

      method load_short addr  = 
	match self#maybe_load_short addr with
	  | Some s -> s
	  | None -> missing 16 addr

      method load_word  addr  = 
	match self#maybe_load_word addr with
	  | Some w -> w
	  | None -> missing 32 addr

      method load_long  addr  = 
	match self#maybe_load_long addr with
	  | Some l -> l
	  | None -> missing 64 addr

      method measure_size = (0, 0, mem#measure_size)

      method clear () = mem#clear ()
    end
end

(*
 Owned and copyright BitBlaze, 2007,2009. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

class virtual memory = object(self)
  method virtual store_byte : Int64.t -> int -> unit
  method virtual maybe_load_byte : Int64.t -> int option
  method virtual clear : unit -> unit

  method load_byte addr =
    match (self#maybe_load_byte addr) with
      | Some b -> b
      | None -> 0

  method store_short addr s =
    self#store_byte addr (s land 0xFF);
    self#store_byte (Int64.add addr 1L) ((s lsr 8) land 0xFF)

  method store_word addr w =
    self#store_byte addr (Int64.to_int (Int64.logand 0xFFL w));
    self#store_byte (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 8)));
    self#store_byte (Int64.add addr 2L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 16)));
    self#store_byte (Int64.add addr 3L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 24)))

  method store_long addr l =
    self#store_byte addr (Int64.to_int (Int64.logand 0xFFL l));
    self#store_byte (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 8)));
    self#store_byte (Int64.add addr 2L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 16)));
    self#store_byte (Int64.add addr 3L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 24)));
    self#store_byte (Int64.add addr 4L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 32)));
    self#store_byte (Int64.add addr 5L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 40)));
    self#store_byte (Int64.add addr 6L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 48)));
    self#store_byte (Int64.add addr 7L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 56)))

  method load_short addr =
    let b1 = self#load_byte addr
    and b2 = self#load_byte (Int64.add addr 1L)
    in
      b1 lor (b2 lsl 8)

  method maybe_load_short addr = Some (self#load_short addr)
  method maybe_load_word  addr = Some (self#load_word  addr)
  method maybe_load_long  addr = Some (self#load_long  addr)

  method load_word addr =
    let b1 = Int64.of_int (self#load_byte addr)
    and b2 = Int64.of_int (self#load_byte (Int64.add addr 1L))
    and b3 = Int64.of_int (self#load_byte (Int64.add addr 2L))
    and b4 = Int64.of_int (self#load_byte (Int64.add addr 3L))
    in
      Int64.logor 
	(Int64.logor b1 (Int64.shift_left b2 8))
	(Int64.logor (Int64.shift_left b3 16) (Int64.shift_left b4 24))

  method load_long addr =
    let b1 = Int64.of_int (self#load_byte addr)
    and b2 = Int64.of_int (self#load_byte (Int64.add addr 1L))
    and b3 = Int64.of_int (self#load_byte (Int64.add addr 2L))
    and b4 = Int64.of_int (self#load_byte (Int64.add addr 3L))
    and b5 = Int64.of_int (self#load_byte (Int64.add addr 4L))
    and b6 = Int64.of_int (self#load_byte (Int64.add addr 5L))
    and b7 = Int64.of_int (self#load_byte (Int64.add addr 6L))
    and b8 = Int64.of_int (self#load_byte (Int64.add addr 7L))
    in
      Int64.logor
	(Int64.logor 
	   (Int64.logor b1 (Int64.shift_left b2 8))
	   (Int64.logor (Int64.shift_left b3 16) (Int64.shift_left b4 24)))
	(Int64.logor 
	   (Int64.logor (Int64.shift_left b5 32) (Int64.shift_left b6 40))
	   (Int64.logor (Int64.shift_left b7 48) (Int64.shift_left b8 56)))
end

class string_memory = object(self)
  inherit memory

  (* The extra page is a hacky way to not crash on address wrap-around *)
  val mem = Array.init 0x100001 (fun _ -> None)

  method store_byte addr b =
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let page_str = match mem.(page) with
      | Some page_str -> page_str
      | None ->
	  let new_page = String.make 4096 '\x00' in
	    mem.(page) <- Some new_page;
	    new_page
    in
      page_str.[idx] <- Char.chr b

  method load_byte addr =
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let page_str = match mem.(page) with
      | Some page_str -> page_str
      | None ->
	  let new_page = String.make 4096 '\x00' in
	    mem.(page) <- Some new_page;
	    new_page
    in
      Char.code page_str.[idx]

  method maybe_load_byte addr = Some (self#load_byte addr)

  method clear () =
    Array.fill mem 0 0x100001 None
end

class hash_memory = object(self)
  inherit memory

  val mem = Hashtbl.create 1000

  method store_byte addr b =
    Hashtbl.replace mem addr b

  method maybe_load_byte addr =
    try
      Some (Hashtbl.find mem addr)
    with
	Not_found -> None

  method clear () =
    Hashtbl.clear mem
end

class snapshot_memory main diff = object(self)
  inherit memory
    
  val mutable have_snap = false

  method store_byte addr b =
    (if have_snap then diff else main)#store_byte addr b

  method maybe_load_byte addr =
    if have_snap then
      match diff#maybe_load_byte addr with
	| Some b -> Some b
	| None -> main#maybe_load_byte addr
    else
      main#maybe_load_byte addr

  method clear () = 
    diff#clear ();
    main#clear ()

  method make_snap () =
    have_snap <- true

  method reset () = 
    diff#clear ()
end

let endianness = V.Little

let endian_i n k = 
  match endianness with
    | V.Little -> k
    | V.Big -> n - k
  
let extract_8_from_64 l which =
  Int64.to_int (Int64.logand 0xFFL
		  (Int64.shift_right l (8 * (endian_i 8 which))))
        
let extract_8_from_32 l which =
  Int64.to_int (Int64.logand 0xFFL
		  (Int64.shift_right l (8 * (endian_i 4 which))))

let extract_8_from_16 l which =
  Int64.to_int (Int64.logand 0xFFL
		  (Int64.shift_right l (8 * (endian_i 2 which))))

let extract_16_from_64 l which =
  Int64.to_int (Int64.logand 0xFFFFL
		  (Int64.shift_right l (8 * (endian_i 8 which))))

let extract_16_from_32 l which =
  Int64.to_int (Int64.logand 0xFFFFL
		  (Int64.shift_right l (8 * (endian_i 4 which))))

let extract_32_from_64 l which =
  Int64.logand 0xFFFFFFFFL (Int64.shift_right l (8 * (endian_i 8 which)))

let split64 l =
  ((extract_32_from_64 l 0), (extract_32_from_64 l 4))

let split32 l =
  ((extract_16_from_32 l 0), (extract_16_from_32 l 2))

let split16 l =
  ((extract_8_from_16 l 0), (extract_8_from_16 l 1))

let assemble16 b1 b2 =
  match endianness with
    | V.Little -> b1 lor (b2 lsl 8)
    | V.Big    -> b2 lor (b1 lsl 8)

let assemble32 s1 s2 =
  match endianness with
    | V.Little -> Int64.logor (Int64.of_int s1)
	                      (Int64.shift_left (Int64.of_int s2) 16)
    | V.Big    -> Int64.logor (Int64.of_int s2)
	                      (Int64.shift_left (Int64.of_int s1) 16)

let assemble64 w1 w2 =
  match endianness with
    | V.Little -> Int64.logor w1 (Int64.shift_left w2 32)
    | V.Big    -> Int64.logor w2 (Int64.shift_left w1 32)

type gran8 = Byte of int64
  
type gran16 = Short of int64
	      | Gran8s of gran8 * gran8
		  
type gran32 = Word of int64
	      | Gran16s of gran16 * gran16
		    
type gran64 = Long of int64
	      | Gran32s of gran32 * gran32

let gran8_get_byte (Byte l) = Int64.to_int l

let gran16_get_byte g16 which =
  assert(which >= 0); assert(which < 2);
  match g16 with
    | Short(l) -> extract_8_from_16 l which
    | Gran8s(g1, g2) ->
	if which < 1 then
	  gran8_get_byte g1
	else
	  gran8_get_byte g2
		  
let gran32_get_byte g32 which =
  assert(which >= 0); assert(which < 4);
  match g32 with
    | Word(l) -> extract_8_from_32 l which
    | Gran16s(g1, g2) ->
	if which < 2 then
	  gran16_get_byte g1 which
	else
	  gran16_get_byte g2 (which - 2)

let gran64_get_byte g64 which =
  assert(which >= 0); assert(which < 8);
  match g64 with
    | Long(l) -> extract_8_from_64 l which
    | Gran32s(g1, g2) ->
	if which < 4 then
	  gran32_get_byte g1 which
	else
	  gran32_get_byte g2 (which - 4)

let gran16_get_short g16 =
  match g16 with
    | Short(l) -> Int64.to_int l
    | Gran8s(g1, g2) -> assemble16 (gran8_get_byte g1) (gran8_get_byte g2)

let gran32_get_short g32 which =
  assert(which = 0 or which = 2);
  match g32 with
    | Word(l) -> extract_16_from_32 l which
    | Gran16s(g1, g2) ->
	if which < 2 then
	  gran16_get_short g1
	else
	  gran16_get_short g2

let gran64_get_short g64 which =
  assert(which = 0 or which = 2 or which = 4 or which = 6);
  match g64 with
    | Long(l) -> extract_16_from_64 l which
    | Gran32s(g1, g2) ->
	if which < 4 then
	  gran32_get_short g1 which
	else
	  gran32_get_short g2 (which - 4)

let gran32_get_word g32 =
  match g32 with
    | Word(l) -> l
    | Gran16s(g1, g2) -> assemble32 (gran16_get_short g1) (gran16_get_short g2)

let gran64_get_word g64 which =
  assert(which = 0 or which = 4);
  match g64 with
    | Long(l) -> extract_32_from_64 l which
    | Gran32s(g1, g2) ->
	if which < 4 then
	  gran32_get_word g1
	else
	  gran32_get_word g2

let gran64_get_long g64 =
  match g64 with
    | Long(l) -> l
    | Gran32s(g1, g2) -> assemble64 (gran32_get_word g1) (gran32_get_word g2)

let gran64_split g64 = 
  match g64 with
    | Gran32s(g1, g2) -> (g1, g2)
    | Long(l) -> let (w1, w2) = split64 l in (Word(w1), Word(w2))

let gran32_split g32 = 
  match g32 with
    | Gran16s(g1, g2) -> (g1, g2)
    | Word(l) ->
	let (s1, s2) = split32 l in
	  (Short(Int64.of_int s1), Short(Int64.of_int s2))
	    
let gran16_split g16 = 
  match g16 with
    | Gran8s(g1, g2) -> (g1, g2)
    | Short(l) ->
	let (b1, b2) = split16 l in
	  (Byte(Int64.of_int b1),
	   Byte(Int64.of_int b2))

let gran16_put_byte g16 which b =
  assert(which = 0 or which = 1);
  let (g1, g2) = gran16_split g16 in
    if which < 1 then
      Gran8s(Byte(Int64.of_int b), g2)
    else
      Gran8s(g1, Byte(Int64.of_int b))

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
  assert(which = 0 or which = 2);
  let (g1, g2) = gran32_split g32 in
    if which < 2 then
      Gran16s(Short(Int64.of_int s), g2)
    else
      Gran16s(g1, Short(Int64.of_int s))

let gran64_put_short g64 which s =
  assert(which = 0 or which = 2 or which = 4 or which = 6);
  let (g1, g2) = gran64_split g64 in
    if which < 4 then
      Gran32s((gran32_put_short g1 which s), g2)
    else
      Gran32s(g1, (gran32_put_short g2 (which - 4) s))

let gran64_put_word g64 which w =
  assert(which = 0 or which = 4);
  let (g1, g2) = gran64_split g64 in
    if which < 4 then
      Gran32s(Word(w), g2)
    else
      Gran32s(g1, Word(w))

let gran8_to_string g8 =
  match g8 with
    | Byte(b) -> Printf.sprintf "%02Lx" b

let gran16_to_string g16 =
  match g16 with
    | Short(s) -> Printf.sprintf "%04Lx" s
    | Gran8s(g1, g2) -> (gran8_to_string g1) ^ "|" ^ (gran8_to_string g2)

let gran32_to_string g32 =
  match g32 with
    | Word(w) -> Printf.sprintf "%08Lx" w
    | Gran16s(g1, g2) -> (gran16_to_string g1) ^ "|" ^ (gran16_to_string g2)

let gran64_to_string g64 =
  match g64 with
    | Long(l) -> Printf.sprintf "%016Lx" l
    | Gran32s(g1, g2) -> (gran32_to_string g1) ^ "|" ^ (gran32_to_string g2)

type page = (gran64 option) array

class granular_memory = object(self)
  (* The extra page is a hacky way to not crash on address wrap-around *)
  val mem = Array.init 0x100001 (fun _ -> None)

  method maybe_load_byte addr =
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    match mem.(page) with
      | None -> None
      | Some page ->
	  let chunk = idx asr 3 and
	      which = idx land 0x7 in
	    match page.(chunk) with
	      | None -> None
	      | Some g64 -> Some (gran64_get_byte g64 which)

  method maybe_load_short addr =
    if (Int64.logand addr 1L) = 0L then
      ((* aligned fast path *)
	let page = Int64.to_int (Int64.shift_right addr 12) and
	    idx = Int64.to_int (Int64.logand addr 0xfffL) in
	  match mem.(page) with
	    | None -> None
	    | Some page ->
		let chunk = idx asr 3 and
		    which = idx land 0x7 in
		  match page.(chunk) with
		    | None -> None
		    | Some g64 -> Some (gran64_get_short g64 which))
    else
      (* unaligned slow path *)
      let mb0 = self#maybe_load_byte addr and
	  mb1 = self#maybe_load_byte (Int64.add addr 1L) in
	match (mb0, mb1) with
	  | (Some b0, Some b1) -> Some (assemble16 b0 b1)
	  | _ -> None

  method maybe_load_word addr =
    if (Int64.logand addr 3L) = 0L then
      ((* aligned fast path *)
	let page = Int64.to_int (Int64.shift_right addr 12) and
	    idx = Int64.to_int (Int64.logand addr 0xfffL) in
	  match mem.(page) with
	    | None -> None
	    | Some page ->
		let chunk = idx asr 3 and
		    which = idx land 0x7 in
		  match page.(chunk) with
		    | None -> None
		    | Some g64 -> Some (gran64_get_word g64 which))
    else
      (* unaligned slow path *)
      let ms0 = self#maybe_load_short addr and
	  ms1 = self#maybe_load_short (Int64.add addr 2L) in
	match (ms0, ms1) with
	  | (Some s0, Some s1) -> Some (assemble32 s0 s1)
	  | _ -> None

  method maybe_load_long addr =
    if (Int64.logand addr 7L) = 0L then
      ((* aligned fast path *)
	let page = Int64.to_int (Int64.shift_right addr 12) and
	    idx = Int64.to_int (Int64.logand addr 0xfffL) in
	  match mem.(page) with
	    | None -> None
	    | Some page ->
		let chunk = idx asr 3 in
		  match page.(chunk) with
		    | None -> None
		    | Some g64 -> Some (gran64_get_long g64))
    else
      (* unaligned slow path *)
      let mw0 = self#maybe_load_word addr and
	  mw1 = self#maybe_load_word (Int64.add addr 4L) in
	match (mw0, mw1) with
	  | (Some w0, Some w1) -> Some (assemble64 w0 w1)
	  | _ -> None

  method load_byte addr =
    match self#maybe_load_byte addr with
      | None -> 0
      | Some b -> b

  method load_short addr =
    match self#maybe_load_short addr with
      | None -> 0
      | Some s -> s

  method load_word addr =
    match self#maybe_load_word addr with
      | None -> 0L
      | Some w -> w

  method load_long addr =
    match self#maybe_load_long addr with
      | None -> 0L
      | Some l -> l

  method private get_page addr =
    let page_n = Int64.to_int (Int64.shift_right addr 12) in
      match mem.(page_n) with
	| Some page -> page
	| None ->
	    let new_page = Array.init 512 (fun _ -> None) in
	      mem.(page_n) <- Some new_page;
	      new_page

  method store_byte addr b =
    let page = self#get_page addr and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let chunk = idx asr 3 and
	which = idx land 0x7 in
    let old_chunk =
      (match page.(chunk) with
	 | None -> Long(0L)
	 | Some g64 -> g64) in
      page.(chunk) <- Some (gran64_put_byte old_chunk which b)

  method store_short addr s =
    if (Int64.logand addr 1L) = 0L then
      ((* aligned fast path *)
	let page = self#get_page addr and
	    idx = Int64.to_int (Int64.logand addr 0xfffL) in
	let chunk = idx asr 3 and
	    which = idx land 0x7 in
	let old_chunk =
	  (match page.(chunk) with
	     | None -> Long(0L)
	     | Some g64 -> g64) in
	  page.(chunk) <- Some (gran64_put_short old_chunk which s))
    else
      (* unaligned slow path *)
      let (b0, b1) = split16 (Int64.of_int s) in
	self#store_byte addr b0;
	self#store_byte (Int64.add addr 1L) b1

  method store_word addr w =
    if (Int64.logand addr 3L) = 0L then
      ((* aligned fast path *)
	let page = self#get_page addr and
	    idx = Int64.to_int (Int64.logand addr 0xfffL) in
	let chunk = idx asr 3 and
	    which = idx land 0x7 in
	let old_chunk =
	  (match page.(chunk) with
	     | None -> Long(0L)
	     | Some g64 -> g64) in
	  page.(chunk) <- Some (gran64_put_word old_chunk which w))
    else
      (* unaligned slow path *)
      let (s0, s1) = split32 w in
	self#store_short addr s0;
	self#store_short (Int64.add addr 2L) s1

  method store_long addr l =
    if (Int64.logand addr 7L) = 0L then
      ((* aligned fast path *)
	let page = self#get_page addr and
	    idx = Int64.to_int (Int64.logand addr 0xfffL) in
	let chunk = idx asr 3 in
	  page.(chunk) <- Some(Long(l)))
    else
      (* unaligned slow path *)
      let (w0, w1) = split64 l in
	self#store_word addr w0;
	self#store_word (Int64.add addr 4L) w1

  method private chunk_to_string addr =
    let page = self#get_page addr and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let chunk = idx asr 3 in
      match page.(chunk) with
	| None -> "[...]"
	| Some g64 -> "[" ^ (gran64_to_string g64) ^ "]"

  method sync_chunk addr thunk =
    (* Printf.printf "sync chunk %08Lx: " addr; *)
    let page = self#get_page addr and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let chunk = idx asr 3 in
      (match page.(chunk) with
	 | None -> page.(chunk) <- Some (Long(thunk ()))
	 | _ -> ())
	(* Printf.printf "%s\n" (self#chunk_to_string addr) *)

  method clear () =
    Array.fill mem 0 0x100001 None
end

class granular_snapshot_memory main diff = object(self)
  inherit memory

  val mutable have_snap = false

  method private sync_chunks addr1 addr2 =
    let sync_chunk addr =
      diff#sync_chunk addr
	(fun () -> main#load_long addr)
    in
    let chunk1 = Int64.logand addr1 (Int64.lognot 7L) and
	chunk2 = Int64.logand addr2 (Int64.lognot 7L) in
      sync_chunk chunk1;
      if chunk1 <> chunk2 then
	sync_chunk chunk2
      else ()

  method store_byte addr b =
    if have_snap then
      (self#sync_chunks addr addr;
       diff#store_byte addr b)
    else
      main#store_byte addr b

  method store_short addr s =
    if have_snap then
      (self#sync_chunks addr (Int64.add addr 1L);
       diff#store_short addr s)
    else
      main#store_short addr s

  method store_word addr w =
    if have_snap then
      (self#sync_chunks addr (Int64.add addr 3L);
       diff#store_word addr w)
    else
      main#store_word addr w

  method store_long addr l =
    if have_snap then
      (self#sync_chunks addr (Int64.add addr 7L);
       diff#store_long addr l)
    else
      main#store_long addr l

  method maybe_load_byte addr =
    if have_snap then
      match diff#maybe_load_byte addr with
	| Some b -> Some b
	| None -> main#maybe_load_byte addr
    else
      main#maybe_load_byte addr

  method maybe_load_short addr =
    if have_snap then
      match diff#maybe_load_short addr with
	| Some s -> Some s
	| None -> main#maybe_load_short addr
    else
      main#maybe_load_short addr

  method maybe_load_word addr =
    if have_snap then
      match diff#maybe_load_word addr with
	| Some w -> Some w
	| None -> main#maybe_load_word addr
    else
      main#maybe_load_word addr

  method maybe_load_long addr =
    if have_snap then
      match diff#maybe_load_long addr with
	| Some l -> Some l
	| None -> main#maybe_load_long addr
    else
      main#maybe_load_long addr

  method clear () = 
    diff#clear ();
    main#clear ()

  method make_snap () =
    have_snap <- true

  method reset () = 
    diff#clear (); ()
end

class parallel_check_memory mem1 mem2 = object(self)
  inherit memory

  method store_byte addr b =
    Printf.printf "mem[%08Lx]:b := %02x\n" addr b;
    mem1#store_byte addr b;
    mem2#store_byte addr b

  method store_short addr s =
    Printf.printf "mem[%08Lx]:s := %04x\n" addr s;
    mem1#store_short addr s;
    mem2#store_short addr s

  method store_word addr w =
    Printf.printf "mem[%08Lx]:w := %08Lx\n" addr w;
    mem1#store_word addr w;
    mem2#store_word addr w

  method store_long addr l =
    Printf.printf "mem[%08Lx]:l := %016Lx\n" addr l;
    mem1#store_long addr l;
    mem2#store_long addr l

  method load_byte addr =
    let b1 = mem1#load_byte addr and
	b2 = mem2#load_byte addr in
      if b1 = b2 then
	Printf.printf "mem[%08Lx] is %02x\n" addr b1
      else
	(Printf.printf "mem[%08Lx] mismatch %02x vs %02x\n" addr b1 b2;
	 failwith "Mismatch in load_byte");
      b1

  method load_short addr =
    let s1 = mem1#load_short addr and
	s2 = mem2#load_short addr in
      if s1 = s2 then
	Printf.printf "mem[%08Lx] is %04x\n" addr s1
      else
	(Printf.printf "mem[%08Lx] mismatch %04x vs %04x\n" addr s1 s2;
	 failwith "Mismatch in load_short");
      s1

  method load_word addr =
    let w1 = mem1#load_word addr and
	w2 = mem2#load_word addr in
      if w1 = w2 then
	Printf.printf "mem[%08Lx] is %08Lx\n" addr w1
      else
	(Printf.printf "mem[%08Lx] mismatch %08Lx vs %08Lx\n" addr w1 w2;
	 failwith "Mismatch in load_word");
      w1

  method load_long addr =
    let l1 = mem1#load_long addr and
	l2 = mem2#load_long addr in
      if l1 = l2 then
	Printf.printf "mem[%08Lx] is %016Lx\n" addr l1
      else
	(Printf.printf "mem[%08Lx] mismatch %016Lx vs %016Lx\n" addr l1 l2;
	 failwith "Mismatch in load_long");
      l1

  method maybe_load_byte addr = Some (self#load_byte addr)

  method clear     () = mem1#clear ();     mem2#clear ();
    Printf.printf "-------- clear --------\n"

  method make_snap () = mem1#make_snap (); mem2#make_snap ();
    Printf.printf "-------- make_snap --------\n"

  method reset     () = mem1#reset ();     mem2#reset ();
    Printf.printf "-------- reset --------\n"
end

let nofix x = x

let fix_u ty x =
  (match ty with
     | V.REG_1 -> Int64.logand x 0x1L
     | V.REG_8 -> Int64.logand x 0xffL
     | V.REG_16 -> Int64.logand x 0xffffL
     | V.REG_32 -> Int64.logand x 0xffffffffL
     | V.REG_64 -> x
     | _ -> failwith "Bad int type in fix_u")

let fix_s ty x =
  (match ty with
     | V.REG_1 -> Int64.shift_right (Int64.shift_left x 63) 63
     | V.REG_8 -> Int64.shift_right (Int64.shift_left x 56) 56
     | V.REG_16 -> Int64.shift_right (Int64.shift_left x 48) 48
     | V.REG_32 -> Int64.shift_right (Int64.shift_left x 32) 32
     | V.REG_64 -> x
     | _ -> failwith "Bad int type in fix_s")

let bool64 f = fun a b -> if (f a b) then 1L else 0L

let move_hash src dest =
  V.VarHash.clear dest;
  V.VarHash.iter (fun a b -> V.VarHash.add dest a b) src

class virtual special_handler = object(self)
  method virtual handle_special : string -> bool
end

class ['a] frag_machine () = object(self)
  (* val mem = new snapshot_memory (new string_memory) (new hash_memory) *)

  val mem = (new granular_snapshot_memory (new granular_memory)
	       (new granular_memory))

  (* val mem = new parallel_check_memory
    (new snapshot_memory (new string_memory) (new hash_memory))
    (new granular_snapshot_memory (new granular_memory)
       (new granular_memory)) *)

  val reg_store = V.VarHash.create 100
  val temps = V.VarHash.create 100
  val mutable frag = ([], [])
  val mutable insns = []

  val mutable snap = (V.VarHash.create 1, V.VarHash.create 1)

  method init_prog (dl, sl) =
    List.iter (fun v -> V.VarHash.add reg_store v 0L) dl;
    self#set_frag (dl, sl);
    let result = self#run () in
      match result with
	| "fallthrough" -> ()
	| _ -> failwith "Initial program should fall through"

  method set_frag (dl, sl) =
    frag <- (dl, sl);
    V.VarHash.clear temps;
    List.iter (fun v -> V.VarHash.add temps v 0L) dl;
    insns <- sl

  method get_x86_gprs () = 
    let dl = V.VarHash.fold (fun k v l -> k :: l) reg_store [] in
      List.map
	(fun name -> List.find (fun (i, s, t) -> s = name) dl)
	["R_EAX"; "R_EBX"; "R_ECX"; "R_EDX";
	 "R_ESI"; "R_EDI"; "R_ESP"; "R_EBP"; "EFLAGS";
	 "R_GS"; "R_GDT"]    

  method store_byte  addr b = mem#store_byte  addr (Int64.to_int b)
  method store_short addr s = mem#store_short addr (Int64.to_int s)
  method store_word  addr w = mem#store_word  addr w
  method store_long  addr l = mem#store_long  addr l

  method load_byte  addr = Int64.of_int (mem#load_byte  addr)
  method load_short addr = Int64.of_int (mem#load_short addr)
  method load_word  addr =               mem#load_word  addr
  method load_long  addr =               mem#load_long  addr

  method make_mem_snap () = mem#make_snap ()
  method mem_reset () = mem#reset ()

  method make_snap () =
    self#make_mem_snap ();
    snap <- (V.VarHash.copy reg_store, V.VarHash.copy temps)

  method reset () =
    self#mem_reset ();
    match snap with (r, t) ->
      move_hash r reg_store;
      move_hash t temps;

  val mutable special_handler_list = ([] : #special_handler list)

  method virtual add_special_handler : (#special_handler -> unit)

  method add_special_handler (h : 'a) : unit =
    special_handler_list <- h :: special_handler_list

  method handle_special str =
    try
      ignore(List.find (fun h -> (h#handle_special str)) special_handler_list);
      true
    with
	Not_found -> false

  method get_int_var_unfix ((_,_,ty) as var) =
    try
      V.VarHash.find reg_store var
    with
      | Not_found ->
	  (try 
	     V.VarHash.find temps var
	   with
	     | Not_found -> V.pp_var print_string var; 
		 failwith "Unknown variable")

  method get_int_var ((_,_,ty) as var) =
        fix_u ty (self#get_int_var_unfix var)

  method set_int_var ((_,_,ty) as var) value =
    try
      ignore(V.VarHash.find reg_store var);
      V.VarHash.replace reg_store var value
    with
	Not_found ->
	  V.VarHash.replace temps var value

  method eval_int_exp_ty exp =
    match exp with
      | V.BinOp(op, e1, e2) ->
	  let (v1, ty1) = self#eval_int_exp_ty e1 and
	      (v2, ty2) = self#eval_int_exp_ty e2 in
	  let ty = 
	    (match op with
	       | V.PLUS | V.MINUS | V.TIMES
	       | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
	       | V.BITAND | V.BITOR | V.XOR
		   -> assert(ty1 = ty2); ty1
	       | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		   -> ty1
	       | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		   -> assert(ty1 = ty2); V.REG_1) in
	  let (func, fix1, fix2) =
	    (match op with
	       | V.PLUS -> (Int64.add, nofix, nofix)
	       | V.MINUS -> (Int64.sub, nofix, nofix)
	       | V.TIMES -> (Int64.mul, nofix, nofix)
	       | V.DIVIDE -> (Vine_util.int64_udiv, fix_u ty, fix_u ty)
	       | V.SDIVIDE -> (Int64.div, fix_s ty, fix_s ty)
	       | V.MOD -> (Vine_util.int64_urem, fix_u ty, fix_u ty)
	       | V.SMOD -> (Int64.rem, fix_s ty, fix_s ty)
	       | V.LSHIFT -> ((fun v a -> Int64.shift_left v
				 (Int64.to_int a)), nofix, fix_u ty2)
	       | V.RSHIFT -> ((fun v a -> Int64.shift_right_logical v
				 (Int64.to_int a)), fix_u ty1, fix_u ty2)
	       | V.ARSHIFT -> ((fun v a -> Int64.shift_right v
				  (Int64.to_int a)), fix_s ty1, fix_u ty2)
	       | V.BITAND -> (Int64.logand, nofix, nofix)
	       | V.BITOR -> (Int64.logor, nofix, nofix)
	       | V.XOR -> (Int64.logxor, nofix, nofix)
	       | V.EQ -> (bool64 (=), fix_u ty1, fix_u ty1)
	       | V.NEQ -> (bool64 (<>), fix_u ty1, fix_u ty1)
	       | V.LT -> (bool64 (fun a b -> Vine_util.int64_ucompare
				    a b < 0), fix_u ty1, fix_u ty1)
	       | V.LE -> (bool64 (fun a b -> Vine_util.int64_ucompare
				    a b <= 0), fix_u ty1, fix_u ty1)
	       | V.SLT -> (bool64 (<), fix_s ty1, fix_s ty1)
	       | V.SLE -> (bool64 (<=), fix_s ty1, fix_s ty1))
	  in
	    (func (fix1 v1) (fix2 v2)), ty
      | V.UnOp(op, e1) ->
	  let (v1, ty1) = self#eval_int_exp_ty e1 and
	      func = 
	    (match op with
	       | V.NEG -> Int64.neg
	       | V.NOT -> Int64.lognot)
	  in
	    (func v1), ty1
      | V.Constant(V.Int(ty, i)) -> i, ty
      | V.Lval(V.Temp((_,_,ty) as var)) -> (self#get_int_var_unfix var), ty
      | V.Lval(V.Mem(memv, idx, ty)) ->
	  let addr = fix_u V.REG_32 (self#eval_int_exp idx) in
	  let v =
	    (match ty with
	       | V.REG_8 -> self#load_byte addr
	       | V.REG_16 -> self#load_short addr
	       | V.REG_32 -> self#load_word addr
	       | V.REG_64 -> self#load_long addr
	       | _ -> failwith "Unsupported memory type") in
	    (v, ty)
      | V.Cast(kind, ty, e) ->
	  let (v1, ty1) = self#eval_int_exp_ty e in
	  let v =
	    match kind with
	      | V.CAST_UNSIGNED -> fix_u ty1 v1
	      | V.CAST_SIGNED -> fix_s ty1 v1
	      | V.CAST_LOW -> fix_u ty v1
	      | V.CAST_HIGH ->
		  let shift =
		    (match (ty1, ty) with
		       | (t_1, t_2) when t_1 = t_2 -> 0
		       | (V.REG_64, V.REG_1)  -> 63
		       | (V.REG_64, V.REG_8)  -> 56
		       | (V.REG_64, V.REG_16) -> 48
		       | (V.REG_64, V.REG_32) -> 32
		       | (V.REG_32, V.REG_1)  -> 31
		       | (V.REG_32, V.REG_8)  -> 24
		       | (V.REG_32, V.REG_16) -> 16
		       | (V.REG_16, V.REG_1)  -> 15
		       | (V.REG_16, V.REG_8)  -> 8
		       | (V.REG_8,  V.REG_1)  -> 7
		       | (_, _) -> failwith "Bad types in CAST_HIGH") in
		    Int64.shift_right v1 shift
	  in
	    (v, ty)
      | _ -> failwith "Unsupported (or non-int) expr type in eval_int_exp_ty"
	  
  method eval_int_exp exp =
    let (v, _) = self#eval_int_exp_ty exp in
      v

  method eval_bool_exp exp =
    if (Int64.logand (self#eval_int_exp exp) 0x1L) = 1L then true else false
      
  method eval_label_exp e =
    match e with
      | V.Name(lab) -> lab
      | _ ->
	  let addr = fix_u V.REG_32 (self#eval_int_exp e) in
	    Printf.sprintf "pc_0x%Lx" addr

  method jump lab =
    let rec find_label lab sl =
      match sl with
	| [] -> None
	| V.Label(l) :: rest when l = lab -> Some sl
	| st :: rest -> find_label lab rest
    in
    let (_, sl) = frag in
      match find_label lab sl with
	| None -> lab
	| Some sl ->
	    self#run_sl sl
	      
  method run_sl sl =
    match sl with
      | [] -> "fallthrough"
      | st :: rest ->
	  (match st with
	     | V.Jmp(l) -> self#jump (self#eval_label_exp l)
	     | V.CJmp(cond, l1, l2) ->
		 let cond_v = self#eval_bool_exp cond in
		   if cond_v then
		     self#jump (self#eval_label_exp l1)
		   else
		     self#jump (self#eval_label_exp l2)
	     | V.Move(V.Temp(v), e) ->
		 self#set_int_var v (self#eval_int_exp e);
		 self#run_sl rest
	     | V.Move(V.Mem(memv, idx_e, ty), rhs_e) ->
		 let addr = fix_u V.REG_32 (self#eval_int_exp idx_e) and
		     value = self#eval_int_exp rhs_e in
		   (match ty with
		      | V.REG_8 -> self#store_byte addr value
		      | V.REG_16 -> self#store_short addr value
		      | V.REG_32 -> self#store_word addr value
		      | V.REG_64 -> self#store_long addr value
		      | _ -> failwith "Unsupported type in memory move");
		   self#run_sl rest
	     | V.Special(str) ->
		 if self#handle_special str then
		   self#run_sl rest
		 else
		   (Printf.printf "Unhandled special %s\n" str;
		    failwith "Unhandled special")
	     | V.Label(_) -> self#run_sl rest
	     | V.ExpStmt(e) ->
		 let v = self#eval_int_exp e in
		   ignore(v);
		   self#run_sl rest
	     | V.Comment(_) -> self#run_sl rest
	     | V.Block(_,_) -> failwith "Block unsupported"
	     | V.Function(_,_,_,_,_) -> failwith "Function unsupported"
	     | V.Return(_) -> failwith "Return unsupported"
	     | V.Call(_,_,_) -> failwith "Call unsupported"
	     | V.Attr(st, _) -> self#run_sl (st :: rest)
	     | V.Assert(e) ->
		 let v = self#eval_bool_exp e in
		   assert(v);
		   self#run_sl rest
	     | V.Halt(e) ->
		 let v = self#eval_int_exp e in
		   Printf.sprintf "halt_%Ld" v)

  method run () = self#run_sl insns

  method store_byte_idx base idx b =
    self#store_byte (Int64.add base (Int64.of_int idx)) b

  method store_str base idx str =
    for i = 0 to (String.length str - 1) do
      self#store_byte_idx (Int64.add base idx)
	i (Int64.of_int (Char.code str.[i]))
    done

  method store_cstr base idx str =
    self#store_str base idx str;
    self#store_byte_idx (Int64.add base idx) (String.length str) 0L

  method read_buf addr len =
    Array.init len
      (fun i -> Char.chr (mem#load_byte (Int64.add addr (Int64.of_int i))))

  method read_cstr addr =
    let rec bytes_loop i =
      let b = mem#load_byte (Int64.add addr (Int64.of_int i)) in
	if b = 0 then [] else b :: bytes_loop (i + 1)
    in
      String.concat ""
	(List.map (fun b -> String.make 1 (Char.chr b))
	   (bytes_loop 0))

end

(* class fake_frag_machine ((dl, sl) as prog) = object(self)
  val ce = new Vine_ceval.concrete_evaluator prog
  val mem_var = List.find (fun (i, s, t) -> s = "mem") dl

  method init_prog prog' =
    assert(prog == prog');
    ignore(ce#run ());
    ()
      
  method get_int_var var =
    match ce#eval_exp (V.Lval(V.Temp(var))) with
      | Vine_ceval.Int(_, i) -> i
      | _ -> failwith "Bad value in get_int_var"

  method load_byte addr =
    let b_val = ce#eval_exp (V.Lval(V.Mem(mem_var,
					  V.Constant(V.Int(V.REG_32, addr)),
					  V.REG_8))) in
      match b_val with
	| Vine_ceval.Int(V.REG_8, i) -> i
	| _ -> failwith "Memory values must be reg8_t"

  method load_word addr =
    let b_val = ce#eval_exp (V.Lval(V.Mem(mem_var,
					  V.Constant(V.Int(V.REG_32, addr)),
					  V.REG_32))) in
      match b_val with
	| Vine_ceval.Int(V.REG_32, i) -> i
	| _ -> failwith "Unexpected type in memory load"

  method set_frag prog =
    ce#restart prog

  method run () =
    try
      ignore(ce#run ()); "fallthrough"
    with
      | Vine_eval.NoSuchLabel(s) -> s

end *)

let trans_cache = Hashtbl.create 100000 

let rec constant_fold_rec e =
  match e with
    | V.BinOp(op, e1, e2) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.BinOp(op, constant_fold_rec e1, constant_fold_rec e2))
    | V.UnOp(op, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.UnOp(op, constant_fold_rec e))
    | V.Cast(op, ty, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.Cast(op, ty, constant_fold_rec e))
    | V.Lval(lv) -> V.Lval(constant_fold_rec_lv lv)
    | _ -> e
and constant_fold_rec_lv lv =
  match lv with
    | V.Mem(v, e, ty) -> V.Mem(v, constant_fold_rec e, ty)
    | _ -> lv

let cfold_exprs (dl, sl) =
  let sl' = List.map
    (fun st -> match st with
       | V.CJmp(e, l1, l2) -> V.CJmp(constant_fold_rec e, l1, l2)
       | V.Move(lval, e) ->
	   V.Move(constant_fold_rec_lv lval, constant_fold_rec e)
       | V. ExpStmt(e) -> 
	   V.ExpStmt(constant_fold_rec e)
       | _ -> st)
    sl
  in
    (dl, sl')

let rec cfold_with_type e =
  match e with
    | V.UnOp(op, e1) ->
	let (e1', ty1) = cfold_with_type e1 in
	  (V.UnOp(op, e1'), ty1)
    | V.BinOp(op, e1, e2) ->
	let (e1', ty1) = cfold_with_type e1 and
	    (e2', ty2) = cfold_with_type e2 in
	let ty =
	  (match op with
	     | V.PLUS | V.MINUS | V.TIMES
	     | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
	     | V.BITAND | V.BITOR | V.XOR
		 -> assert(ty1 = ty2); ty1
	     | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		 -> ty1
	     | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		 -> assert(ty1 = ty2); V.REG_1)
	in
	  (V.BinOp(op, e1', e2'), ty)
    | V.Constant(V.Int(ty, _)) -> (e, ty)
    | V.Constant(V.Str(_)) -> (e, V.TString)
    | V.Lval(V.Temp(_,_,ty)) -> (e, ty)
    | V.Lval(V.Mem(mem,e1,ty)) ->
	let (e1', _) = cfold_with_type e1 in
	  (V.Lval(V.Mem(mem,e1',ty)), ty)
    | V.Name(_) -> (e, V.addr_t)
    | V.Cast(kind, ty, e1) ->
	let (e1', ty1) = cfold_with_type e1 in
	let e' = if ty = ty1 then e1' else V.Cast(kind, ty, e1') in
	  (e', ty)
    | V.Unknown(_) -> failwith "unhandled unknown in cfold_with_type"
    | V.Let(_) -> failwith "unhandled let in cfold_with_type"

let cfold_exprs_w_type (dl, sl) =
  let fold e =
    match cfold_with_type e with (e', _) -> e'
  in
  let sl' = List.map
    (fun st -> match st with
       | V.CJmp(e, l1, l2) -> V.CJmp(fold e, l1, l2)
       | V.Move(V.Temp(v), e) -> V.Move(V.Temp(v), fold e)
       | V.Move(V.Mem(mem, e1, ty), e2) ->
	   V.Move(V.Mem(mem, fold e1, ty), fold e2)
       | V. ExpStmt(e) -> V.ExpStmt(fold e)
       | _ -> st)
    sl
  in
    (dl, sl')

let rm_unused_stmts sl =
  List.filter
    (fun st -> match st with
       | V.Special("call") -> false
       | V.Special("ret") -> false
       (*| V.Move(V.Temp(_,"R_CC_OP",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP1",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP2",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_NDEP",_),_) -> false*)
       | _ -> true)
    sl

let uncond_jmps sl =
  List.map
    (fun st -> match st with
       | V.CJmp(V.Constant(V.Int(V.REG_1, 1L)), l1, l2) -> V.Jmp(l1)
       | V.CJmp(V.Constant(V.Int(V.REG_1, 0L)), l1, l2) -> V.Jmp(l2)
       | _ -> st
    ) sl

let rec rm_sequential_jmps sl =
  match sl with
    | V.Jmp(V.Name(lab1)) :: V.Label(lab2) :: rest when lab1 = lab2
	-> V.Label(lab2) :: rm_sequential_jmps rest
    | st :: rest -> st :: rm_sequential_jmps rest
    | [] -> []

let rm_unused_labels sl =
  let exp_labels e =
    match e with
      | V.Name(l) -> [l]
      | _ -> []
  in
  let used_labels = List.concat
    (List.map
       (fun st -> match st with
	  | V.Jmp(e) -> exp_labels e
	  | V.CJmp(cond, e1, e2) -> (exp_labels e1) @ (exp_labels e2)
	  | _ -> [])
       sl)
  in
    (* List.iter print_string used_labels; *)
    List.filter
      (fun st -> match st with
	 | V.Label(l) when not (List.mem l used_labels) -> false
	 | _ -> true)
      sl

let rec count_uses_e var e =
  match e with
    | V.BinOp(_, e1, e2) -> (count_uses_e var e1) + (count_uses_e var e2)
    | V.UnOp(_, e) -> count_uses_e var e
    | V.Cast(_, _, e) -> count_uses_e var e
    | V.Lval(lv) -> count_uses_lv var lv
    | _ -> 0
and count_uses_lv var lv =
  match lv with
    | V.Temp(v) -> (if v = var then 1 else 0)
    | V.Mem(v, e, _) -> (if v = var then 1 else 0) + (count_uses_e var e)

let count_temp_uses (dl, sl) =
  let count_uses_sl var =
    List.fold_left (+) 0
      (List.map 
	 (fun st -> match st with
	    | V.CJmp(e1, l1, l2) -> 
		(count_uses_e var e1) + (count_uses_e var l1)
		+ (count_uses_e var l2)
	    | V.Jmp(lab) -> count_uses_e var lab
	    | V.Move(V.Temp(var1), e) when var1 = var
		-> (count_uses_e var e)
	    | V.Move(lval, e) -> (count_uses_lv var lval)
		+ (count_uses_e var e)
	    | V.ExpStmt(e) -> count_uses_e var e
	    | V.Assert(e) -> count_uses_e var e
	    | _ -> 0)
	 sl)
  in
    List.map count_uses_sl dl

let rm_unused_vars (dl, sl) =
  let rm_defs var sl =
    List.filter
      (fun st -> match st with
	 | V.Move(V.Temp(v), _) when v = var -> false
	 | _ -> true)
      sl
  in
  let rec partition2 pred l1 l2 =
    match (l1, l2) with
      | ((item :: r1), (obj :: r2)) ->
	  let (rt, rf) = partition2 pred r1 r2 in
	  if pred obj then
	    (item :: rt, rf)
	  else
	    (rt, item :: rf)
      | ([], []) -> ([], [])
      | _ -> failwith "Mismatched lengths in partition2"
  in
  let uses = count_temp_uses (dl, sl) in
    (* print_string "{";
       List.iter print_int uses;
       print_string "}\n"; *)
  let (dl', to_rm) = partition2 (fun n -> n != 0) dl uses in
  let sl' = List.fold_right rm_defs to_rm sl in
    (dl', sl')

let contains hash ent =
  try
    V.VarHash.find hash ent;
    true;
  with
      Not_found -> false

let copy_const_prop (dl, sl) =
  let map = V.VarHash.create 10 in
  let use_counts = List.fold_left2 
    (fun hash key value -> V.VarHash.add hash key value; hash)
    (V.VarHash.create 10) dl (count_temp_uses (dl, sl)) in
  let rec replace_uses_e expr =
    match expr with
      | V.BinOp(op, e1, e2) ->
	  V.BinOp(op, replace_uses_e e1, replace_uses_e e2)
      | V.UnOp(op, e) ->
	  V.UnOp(op, replace_uses_e e)
      | V.Cast(op, ty, e) ->
	  V.Cast(op, ty, replace_uses_e e)
      | V.Lval(V.Temp(v)) when contains map v ->
	  V.VarHash.find map v
      | V.Lval(V.Mem(v, idx, ty)) ->
	  V.Lval(V.Mem(v, replace_uses_e idx, ty))
      | _ -> expr
  in
  let replace_uses st =
    match st with
      | V.CJmp(e1, l1, l2) -> 
	  V.CJmp(replace_uses_e e1, replace_uses_e l1, replace_uses_e l2)
      | V.Jmp(lab) ->
	  V.Jmp(replace_uses_e lab)
      | V.Move(V.Temp(var), e) -> 
	  V.Move(V.Temp(var), replace_uses_e e)
      | V.Move(V.Mem(var, idx, ty), e) -> 
	  V.Move(V.Mem(var, replace_uses_e idx, ty), replace_uses_e e)
      | V.ExpStmt(e) ->
	  V.ExpStmt(replace_uses_e e)
      | V.Assert(e) ->
	  V.Assert(replace_uses_e e)
      | _ -> st
  in
  let find_or_zero vh key = try V.VarHash.find vh key with Not_found -> 0
  in
  let invalidate_uses vh bad_var =
    V.VarHash.iter
      (fun lhs rhs ->
	 if (count_uses_e bad_var rhs) != 0 then	   
	   (V.VarHash.remove vh lhs))
      vh
  in
  let rec loop sl =
    match sl with
      | [] -> []
      | st :: rest ->
	  let st' = replace_uses st in
	    (match st' with
	       | V.Move(V.Temp(lhs_v), rhs)
		   -> invalidate_uses map lhs_v;
		     V.VarHash.remove map lhs_v;
	       | _ -> ());
	    (match st' with
	       | V.CJmp(_, _, _)
	       | V.Jmp(_)
	       | V.Label(_)
		 -> V.VarHash.clear map
	       | V.Move(V.Temp(lhs_v), V.Lval(V.Temp(rhs_v)))
		 -> V.VarHash.replace map lhs_v (V.Lval(V.Temp(rhs_v)));
		   (if List.mem rhs_v dl then
		      V.VarHash.replace map rhs_v (V.Lval(V.Temp(lhs_v))))
	       | V.Move(V.Temp(lhs_v), rhs)
		   when (find_or_zero use_counts lhs_v) = 1
		     -> V.VarHash.replace map lhs_v rhs
	       | V.Move(V.Temp(lhs_v), (V.Constant(_) as const))
		   -> V.VarHash.replace map lhs_v const
	       | _ -> ());
	    st' :: (loop rest)
  in
    (dl, loop sl)

let peephole_patterns (dl, sl) =
  let rec loop sl =
    match sl with
      (* No-op assignment *)
      | V.Move(V.Temp(r1_1), V.Lval(V.Temp(r1_2))) :: rest
	  when r1_1 = r1_2
	    -> loop rest
      (* Usually lets us avoid a temp in a push: *)
      | V.Move(V.Temp(t1_1), (V.BinOp(op, V.Lval(_), V.Constant(_)) as e1)) ::
	  V.Move(V.Temp(r1), V.Lval(V.Temp(t1_2))) ::
	  V.Move(V.Mem(mem, V.Lval(V.Temp(t1_3)), ty), e2) :: rest
	  when List.mem t1_1 dl && t1_1 = t1_2 && t1_2 = t1_3 &&
	    not (List.mem r1 dl)
	    ->
	  V.Move(V.Temp(t1_1), e1) ::
	    V.Move(V.Temp(r1), e1) ::
	    V.Move(V.Mem(mem, V.Lval(V.Temp(r1)), ty), e2) ::
	    loop rest
      (* Avoid a temp in a pop: *)
      | V.Move(V.Temp(t1_1),
	       (V.Lval(V.Mem(_, V.Lval(V.Temp(r1_1)), _)) as load)) ::
	  (V.Move(V.Temp(r1_2), V.BinOp(op, V.Lval(V.Temp(r1_3)),
					V.Constant(c)))
	     as update)::
	       V.Move(V.Temp(r2), V.Lval(V.Temp(t1_2))) :: rest
	       when List.mem t1_1 dl && t1_1 = t1_2 &&
		 not (List.mem r1_1 dl) && r1_1 = r1_2 && r1_2 = r1_3 &&
		 not (List.mem r2 dl)
	       ->
	  V.Move(V.Temp(t1_1), load) ::
	    V.Move(V.Temp(r2), load) ::
	    update ::
	    loop rest
      | st :: rest -> st :: loop rest
      | [] -> []
  in
    (dl, loop sl)

let simplify_frag (orig_dl, orig_sl) =
  (* V.pp_program print_string (orig_dl, orig_sl); *)
  let (dl, sl) = (orig_dl, orig_sl) in
  let sl = rm_unused_stmts sl in
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
  let (dl, sl) = cfold_exprs (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in 
  let (dl, sl) = cfold_exprs_w_type (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = peephole_patterns (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in 
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in 
  let (dl, sl) = rm_unused_vars (dl, sl) in
 let (dl', sl') = (dl, sl) in
    (* Vine_typecheck.typecheck (dl' @ Asmir.x86_regs, sl'); *)
    (* V.pp_program print_string (orig_dl, orig_sl);
       V.pp_program print_string (dl', sl');
       V.pp_program print_string (copy_prop (dl', sl')); *)
    (dl', sl')

exception SimulatedExit of int64

class linux_special_handler fm =
  let (eax_var, ebx_var, ecx_var, edx_var, esi_var, edi_var,
       esp_var, ebp_var, eflags_var, gs_var, gdt_var) =
    match fm#get_x86_gprs () with
      | [a; b; c; d; si; di; sp; bp; fl; gs; gd]
	-> (a, b, c, d, si, di, sp, bp, fl, gs, gd)
      | _ -> failwith "Bad length for gpr_vars"
  in
  let put_reg var v = fm#set_int_var var v in
  let load_word addr = fm#load_word addr in
  let lea base i step off =
    Int64.add base (Int64.add (Int64.mul (Int64.of_int i) (Int64.of_int step))
		      (Int64.of_int off)) in
  let store_word base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_word addr v
  in
  let zero_region base len =
    for i = 0 to len -1 do fm#store_byte_idx base i 0L done
  in
object(self)
  val unix_fds = 
    let a = Array.make 1024 None in
      Array.set a 0 (Some Unix.stdin);
      Array.set a 1 (Some Unix.stdout);
      Array.set a 2 (Some Unix.stderr);
      a

  method fresh_fd () =
    let rec loop i = match unix_fds.(i) with
      | None -> i
      | Some _ -> loop (i + 1)
    in loop 0

  method get_fd vt_fd =
    match unix_fds.(vt_fd) with
      | Some fd -> fd
      | None -> raise
	  (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))

  method errno err =
    match err with
      | Unix.E2BIG -> 7
      | Unix.EACCES -> 13
      | Unix.EAGAIN -> 11
      | Unix.EBADF -> 9
      | Unix.EBUSY -> 16
      | Unix.ECHILD -> 10
      | Unix.EDEADLK -> 35
      | Unix.EDOM -> 33
      | Unix.EEXIST -> 17
      | Unix.EFAULT -> 14
      | Unix.EFBIG -> 27
      | Unix.EINTR -> 4
      | Unix.EINVAL -> 22
      | Unix.EIO -> 5
      | Unix.EISDIR -> 21
      | Unix.EMFILE -> 24
      | Unix.EMLINK -> 31
      | Unix.ENAMETOOLONG -> 36
      | Unix.ENFILE -> 23
      | Unix.ENODEV -> 19
      | Unix.ENOENT -> 2
      | Unix.ENOEXEC -> 8
      | Unix.ENOLCK -> 37
      | Unix.ENOMEM -> 12
      | Unix.ENOSPC -> 28
      | Unix.ENOSYS -> 38
      | Unix.ENOTDIR -> 20
      | Unix.ENOTEMPTY -> 39
      | Unix.ENOTTY -> 25
      | Unix.ENXIO -> 6
      | Unix.EPERM -> 1
      | Unix.EPIPE -> 32
      | Unix.ERANGE -> 34
      | Unix.EROFS -> 30
      | Unix.ESPIPE -> 29
      | Unix.ESRCH -> 3
      | Unix.EXDEV -> 18
      | Unix.EWOULDBLOCK -> 11
      | Unix.EINPROGRESS -> 115
      | Unix.EALREADY -> 114
      | Unix.ENOTSOCK -> 88
      | Unix.EDESTADDRREQ -> 89
      | Unix.EMSGSIZE -> 90
      | Unix.EPROTOTYPE -> 91
      | Unix.ENOPROTOOPT -> 92
      | Unix.EPROTONOSUPPORT -> 93
      | Unix.ESOCKTNOSUPPORT -> 94
      | Unix.EOPNOTSUPP -> 95
      | Unix.EPFNOSUPPORT -> 96
      | Unix.EAFNOSUPPORT -> 97
      | Unix.EADDRINUSE -> 98
      | Unix.EADDRNOTAVAIL -> 99
      | Unix.ENETDOWN -> 100
      | Unix.ENETUNREACH -> 101
      | Unix.ENETRESET -> 102
      | Unix.ECONNABORTED -> 103
      | Unix.ECONNRESET -> 104
      | Unix.ENOBUFS -> 105
      | Unix.EISCONN -> 106
      | Unix.ENOTCONN -> 107
      | Unix.ESHUTDOWN -> 108
      | Unix.ETOOMANYREFS -> 109
      | Unix.ETIMEDOUT -> 110
      | Unix.ECONNREFUSED -> 111
      | Unix.EHOSTDOWN -> 112
      | Unix.EHOSTUNREACH -> 113
      | Unix.ELOOP -> 40
      | Unix.EOVERFLOW -> 75
      | Unix.EUNKNOWNERR(i) -> i

  method put_errno err =
    put_reg eax_var (Int64.of_int ~-(self#errno err))

  val mutable next_fresh_addr = 0x50000000L

  method fresh_addr size = 
    let ret = next_fresh_addr in
      next_fresh_addr <- Int64.add next_fresh_addr size;
      next_fresh_addr <- Int64.logand 0xffff_ffff_ffff_f000L
	(Int64.add next_fresh_addr 0x0fffL); (* page align *)
      ret

  val the_break = ref 0x08100000L

  method do_write fd bytes count =
    (try
       (match fd with
	  | 1 -> Array.iter print_char bytes;
	      put_reg eax_var (Int64.of_int count)
	  | _ ->
	      let str = Array.fold_left (^) ""
		(Array.map (String.make 1) bytes)
	      in
		match Unix.write (self#get_fd fd) str 0 (String.length str)
		with
		  | i when i = count -> put_reg eax_var (Int64.of_int count)
		  | _ -> raise (Unix.Unix_error(Unix.EINTR, "", "")))
     with
       | Unix.Unix_error(err, _, _) -> self#put_errno err);
    ()

  method do_unix_read fd addr count =
    let rec loop left a =
      if (left <= 0) then 0 else
	let chunk = if (left < 16384) then left else 16384 in
	let str = String.create chunk in
	let num_read = Unix.read fd str 0 chunk in
	  fm#store_str a 0L (String.sub str 0 num_read);
	  num_read +
	    (loop (left - chunk) (Int64.add a (Int64.of_int chunk)))
    in
      loop count addr

  method setup_tcb_seg new_ent base limit =
    let new_gs = Int64.logor (Int64.shift_left new_ent 3) 3L in
    let new_gdt = 0x60000000L in
    let descr = Int64.add new_gdt (Int64.shift_left new_ent 3) in
      put_reg gdt_var new_gdt;
      put_reg gs_var new_gs;
      store_word descr 0 (Int64.logand limit 0xffL);
      store_word descr 1 (Int64.logand 
			    (Int64.shift_right limit 8) 0xffL);
      store_word descr 2 (Int64.logand base 0xffL);
      store_word descr 3 (Int64.logand
			    (Int64.shift_right base 8) 0xffL);
      store_word descr 4 (Int64.logand
			    (Int64.shift_right base 16) 0xffL);
      store_word descr 5 0xf3L; (* pres., ring 3, app, r/w a *)
      store_word descr 6 (Int64.logor 0xc0L
			    (Int64.shift_right limit 16));
      (* page-gran limit, 32-bit, high nibble of limit *)
      store_word descr 7 (Int64.logand
			    (Int64.shift_right base 24) 0xffL)
	
  method oc_kind_to_mode kind = match kind with
    | Unix.S_REG  -> 0o0100000
    | Unix.S_DIR  -> 0o0040000
    | Unix.S_CHR  -> 0o0020000
    | Unix.S_BLK  -> 0o0060000
    | Unix.S_LNK  -> 0o0120000
    | Unix.S_FIFO -> 0o0010000
    | Unix.S_SOCK -> 0o0140000

  method flags_to_oc_flags flags =
    (if (flags land 0x3) = 0        then [Unix.O_RDONLY]   else []) @
      (if (flags land 0x3)= 1       then [Unix.O_WRONLY]   else []) @
      (if (flags land 0x3) = 2      then [Unix.O_RDWR]     else []) @
      (if (flags land 0o4000) != 0  then [Unix.O_NONBLOCK] else []) @
      (if (flags land 0o2000) != 0  then [Unix.O_APPEND]   else []) @
      (if (flags land 0o100) != 0   then [Unix.O_CREAT]    else []) @
      (if (flags land 0o1000) != 0  then [Unix.O_TRUNC]    else []) @
      (if (flags land 0o200) != 0   then [Unix.O_EXCL]     else []) @
      (if (flags land 0o10000) != 0 then [Unix.O_SYNC]     else [])

  method write_oc_statbuf addr oc_buf =
    let dev = Int64.of_int oc_buf.Unix.st_dev and
	ino = Int64.of_int oc_buf.Unix.st_ino and
	mode = Int64.of_int (oc_buf.Unix.st_perm lor 
			       (self#oc_kind_to_mode oc_buf.Unix.st_kind)) and
	nlink = Int64.of_int oc_buf.Unix.st_nlink and
	uid = Int64.of_int oc_buf.Unix.st_uid and
	gid = Int64.of_int oc_buf.Unix.st_gid and
	rdev = Int64.of_int oc_buf.Unix.st_rdev and
	size = Int64.of_int oc_buf.Unix.st_size and
	atime = Int64.of_float oc_buf.Unix.st_atime and
	mtime = Int64.of_float oc_buf.Unix.st_mtime and
	ctime = Int64.of_float oc_buf.Unix.st_ctime and
	blksize = 4096L and
	blocks = Int64.of_int (oc_buf.Unix.st_size/4096)
    in
      store_word addr 0 dev;
      store_word addr 4 0L;       (* high bits of dev *)
      store_word addr 12 ino;     (* 32-bit inode *)
      store_word addr 16 mode;
      store_word addr 20 nlink;
      store_word addr 24 uid;
      store_word addr 28 gid;
      store_word addr 32 rdev;
      store_word addr 36 0L;      (* high bits of rdev *)
      store_word addr 44 size;
      store_word addr 48 0L;      (* high bits of size *)
      store_word addr 52 blksize;
      store_word addr 56 blocks;
      store_word addr 64 atime;
      store_word addr 68 0L;      (* atime nanosecs *)
      store_word addr 72 mtime;
      store_word addr 76 0L;      (* mtime naonsecs *)
      store_word addr 80 ctime;
      store_word addr 84 0L;      (* ctime nanosecs *)
      store_word addr 89 ino;     (* low bits of 64-bit inode *)
      store_word addr 92 0L;      (* high bits of 64-bit inode *)

  method sys_access path mode =
    let oc_mode =
      (if   (mode land 0x7)= 0 then [Unix.F_OK] else []) @
	(if (mode land 0x1)!=0 then [Unix.X_OK] else []) @
	(if (mode land 0x2)!=0 then [Unix.W_OK] else []) @
	(if (mode land 0x4)!=0 then [Unix.R_OK] else []) 
    in
      try
	Unix.access path oc_mode;
	put_reg eax_var 0L
      with
	| Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_brk addr =
    if addr < !the_break then
      ()
    else
      the_break := addr;
    put_reg eax_var !the_break;

  method sys_close fd =
    try
      Unix.close (self#get_fd fd);
      Array.set unix_fds fd None;
      put_reg eax_var 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_exit_group status =
    raise (SimulatedExit(status))

  method sys_futex uaddr op value timebuf uaddr2 val3 =
    let ret = 
      match (op, value) with
	| (129 (* FUTEX_WAKE_PRIVATE *), 1L) ->
	    0L (* never anyone to wake *)
	| _ -> failwith "Unhandled futex operation"
    in
      put_reg eax_var ret

  method sys_ugetrlimit rsrc buf =
    store_word buf 0 0xffffffffL; (* infinity *)
    store_word buf 4 0xffffffffL; (* infinity *)
    put_reg eax_var 0L (* success *)

  method sys_getgid32 () = 
    put_reg eax_var (Int64.of_int (Unix.getgid ()))

  method sys_getegid32 () = 
    put_reg eax_var (Int64.of_int (Unix.getegid ()))

  method sys_getuid32 () = 
    put_reg eax_var (Int64.of_int (Unix.getuid ()))

  method sys_geteuid32 () = 
    put_reg eax_var (Int64.of_int (Unix.geteuid ()))

  method sys_ioctl fd req argp =
    match req with
      | 0x5401L -> 
	  (* let attrs = Unix.tcgetattr (get_fd fd) in *)
	  failwith "Unhandled TCGETS ioctl"
      | _ -> failwith "Unknown ioctl"

  method sys_mmap2 addr length prot flags fd pgoffset =
    let do_read addr = 
      let len = Int64.to_int length in
      let old_loc = Unix.lseek (self#get_fd fd) 0 Unix.SEEK_CUR in
      let _ = Unix.lseek (self#get_fd fd) (4096*pgoffset) Unix.SEEK_SET in
      let nr = self#do_unix_read (self#get_fd fd) addr len in
      let _ = Unix.lseek (self#get_fd fd) old_loc Unix.SEEK_SET in
	(* assert(nr = len); *)
	addr
    in
    let ret =
      match (addr, length, prot, flags, fd, pgoffset) with
	| (0L, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x22L (* MAP_PRIVATE|MAP_ANONYMOUS *), -1, 0) ->
	    let fresh = self#fresh_addr length in
	      zero_region fresh (Int64.to_int length);
	      fresh
	| (_, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x32L (* MAP_PRIVATE|FIXED|ANONYMOUS *), -1, 0) ->
	    zero_region addr (Int64.to_int length);
	    addr
	| (0L, _, 
	   (0x1L|0x5L) (* PROT_READ|PROT_EXEC *),
	   (0x802L|0x2L) (* MAP_PRIVATE|MAP_DENYWRITE *), _, _) ->
	    let dest_addr = self#fresh_addr length in
	      do_read dest_addr
	| (_, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x812L (* MAP_DENYWRITE|PRIVATE|FIXED *), _, _) ->
	    do_read addr
	| _ -> failwith "Unhandled mmap operation"
    in
      put_reg eax_var ret

  method sys_mprotect addr len prot =
    (* treat as no-op *)
    put_reg eax_var 0L;

  method sys_munmap addr len =
    (* treat as no-op *)
    put_reg eax_var 0L

  method sys_open path flags mode =
    try
      let oc_flags = self#flags_to_oc_flags flags in
      let oc_fd = Unix.openfile path oc_flags mode and
	  vt_fd = self#fresh_fd () in
	Array.set unix_fds vt_fd (Some oc_fd);
	put_reg eax_var (Int64.of_int vt_fd)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_read fd buf count =
    let str = String.create count in
    let num_read = Unix.read (self#get_fd fd) str 0 count in
      fm#store_str buf 0L (String.sub str 0 num_read);
      put_reg eax_var (Int64.of_int num_read)

  method sys_readlink path out_buf buflen =
    let real = Unix.readlink path in
    let written = min buflen (String.length real) in
      fm#store_str out_buf 0L (String.sub real 0 written);
      put_reg eax_var (Int64.of_int written);

  method sys_set_robust_list addr len =
    put_reg eax_var 0L (* success *)

  method sys_set_thread_area uinfo =
    let old_ent = Int64.to_int (load_word (lea uinfo 0 0 0))
    and
	base  = load_word (lea uinfo 0 0 4) and
	limit = load_word (lea uinfo 0 0 8) in
      Printf.printf "set_thread_area({entry: %d, base: %Lx, limit: %Ld})\n"
	old_ent base limit;
      (match (old_ent, base, limit) with
	 | (-1, _, _) ->
	     let new_ent = 12L in
	       self#setup_tcb_seg new_ent base limit;
	       store_word uinfo 0 new_ent;
	       put_reg eax_var 0L (* success *)
	 | _ -> failwith "Unhandled args to set_thread_area")

  method sys_set_tid_address addr =
    let pid = Unix.getpid () in
      put_reg eax_var (Int64.of_int pid)

  method sys_rt_sigaction signum newbuf oldbuf setlen =
    (if oldbuf = 0L then () else
      let (action, mask_low, mask_high, flags) =
	match signum with
	  | 8 (* SIGFPE *) -> (0L, 0L, 0L, 0L)
	  | _ -> failwith
	      "Unhandled old signal in rt_sigaction";
      in
	store_word oldbuf 0 action;
	store_word oldbuf 4 flags;
	store_word oldbuf 8 0L; (* restorer *)
	store_word oldbuf 12 mask_low;
	store_word oldbuf 12 mask_high);
    put_reg eax_var 0L; (* success *)

  method sys_rt_sigprocmask how newset oldset setlen =
    (if oldset = 0L then () else
       failwith "Can't report old mask");
    put_reg eax_var 0L (* success *)

  method sys_stat64 path buf_addr =
    try
      let oc_buf = Unix.stat path in
	self#write_oc_statbuf buf_addr oc_buf;
	put_reg eax_var 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fstat64 fd buf_addr =
    let oc_buf = Unix.fstat (self#get_fd fd) in
      self#write_oc_statbuf buf_addr oc_buf;
      put_reg eax_var 0L (* success *)

  method sys_time addr =
    let time = Int64.of_float (Unix.time ()) in
      if addr != 0L then
	store_word addr 0 time else ();
      put_reg eax_var time

  method sys_times addr =
    let float_to_clocks f = Int64.of_float (f *. 100.0) in
    let pt = Unix.times () in
    let ut = float_to_clocks (pt.Unix.tms_utime) and
	st = float_to_clocks (pt.Unix.tms_stime) and
	cut = float_to_clocks (pt.Unix.tms_cutime) and
	cst = float_to_clocks (pt.Unix.tms_cstime) in
      (* Printf.printf "times: %Ld %Ld %Ld %Ld\n" ut st cut cst; *)
      store_word addr 0 ut;
      store_word addr 4 st;		 
      store_word addr 8 cut;
      store_word addr 12 cst;
      put_reg eax_var (Int64.add ut st)

  method sys_uname buf =
    List.iter2
      (fun i str ->
	 fm#store_cstr buf (Int64.mul 65L i) str)
      [0L; 1L; 2L; 3L; 4L; 5L]
      ["Linux"; (* sysname *)
       "amuro"; (* nodename *)
       "2.6.26-2-amd64"; (* release *)
       "#1 SMP Fri Mar 27 04:02:59 UTC 2009"; (*version*)
       "i686"; (* machine *)
       "cs.berkeley.edu" (* domain *)
      ];
    put_reg eax_var 0L (* success *)

  method sys_write fd bytes count =
    self#do_write fd bytes count

  method sys_writev fd iov cnt =
    let bytes =
      Array.concat
	(Vine_util.mapn
	   (fun i -> fm#read_buf
	      (load_word (lea iov i 8 0)) (* iov_base *)
	      (Int64.to_int (load_word (lea iov i 8 4))))
	   (* iov_len *)
	   (cnt - 1)) in
      self#do_write fd bytes (Array.length bytes)

  method handle_linux_syscall () =
    (let syscall_num = Int64.to_int (fm#get_int_var eax_var) and
	 ebx = fm#get_int_var ebx_var and
	 ecx = fm#get_int_var ecx_var and
	 edx = fm#get_int_var edx_var and
	 esi = fm#get_int_var esi_var and
	 edi = fm#get_int_var edi_var and
	 ebp = fm#get_int_var ebp_var in
       match syscall_num with
	 | 0 -> (* restart_syscall *)
	     failwith "Unhandled Linux system call restart_syscall (0)"
	 | 1 -> (* exit *)
	     failwith "Unhandled Linux system call exit (1)"
	 | 2 -> (* fork *)
	     failwith "Unhandled Linux system call fork (2)"
	 | 3 -> (* read *)		    
	     let fd    = Int64.to_int ebx and
		 buf   = ecx and
		 count = Int64.to_int edx in
	       Printf.printf "read(%d, 0x%08Lx, %d)" fd buf count;
	       self#sys_read fd buf count;
	 | 4 -> (* write *)
	     let fd    = Int64.to_int ebx and
		 buf   = ecx and
		 count = Int64.to_int edx in
	       Printf.printf "write(%d, 0x%08Lx, %d)\n" fd buf count;
	       let bytes = fm#read_buf buf count in
		 self#sys_write fd bytes count
	 | 5 -> (* open *)
	     let path_buf = ebx and
		 flags    = Int64.to_int ecx and
		 mode     = Int64.to_int edx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "open(\"%s\", 0x%x, 0o%o)" path flags mode;
	       self#sys_open path flags mode
	 | 6 -> (* close *)
	     let fd = Int64.to_int ebx in
	       Printf.printf "close(%d)" fd;
	       self#sys_close fd
	 | 7 -> (* waitpid *)
	     failwith "Unhandled Linux system call waitpid (7)"
	 | 8 -> (* creat *)
	     failwith "Unhandled Linux system call creat (8)"
	 | 9 -> (* link *)
	     failwith "Unhandled Linux system call link (9)"
	 | 10 -> (* unlink *)
	     failwith "Unhandled Linux system call unlink (10)"
	 | 11 -> (* execve *)
	     failwith "Unhandled Linux system call execve (11)"
	 | 12 -> (* chdir *)
	     failwith "Unhandled Linux system call chdir (12)"
	 | 13 -> (* time *)
	     let addr = ebx in
	       Printf.printf "time(0x%08Lx)" addr;
	       self#sys_time addr
	 | 14 -> (* mknod *)
	     failwith "Unhandled Linux system call mknod (14)"
	 | 15 -> (* chmod *)
	     failwith "Unhandled Linux system call chmod (15)"
	 | 16 -> (* lchown *)
	     failwith "Unhandled Linux system call lchown (16)"
	 | 17 -> (* break *)
	     failwith "Unhandled Linux system call break (17)"
	 | 18 -> (* oldstat *)
	     failwith "Unhandled Linux system call oldstat (18)"
	 | 19 -> (* lseek *)
	     failwith "Unhandled Linux system call lseek (19)"
	 | 20 -> (* getpid *)
	     failwith "Unhandled Linux system call getpid (20)"
	 | 21 -> (* mount *)
	     failwith "Unhandled Linux system call mount (21)"
	 | 22 -> (* umount *)
	     failwith "Unhandled Linux system call umount (22)"
	 | 23 -> (* setuid *)
	     failwith "Unhandled Linux system call setuid (23)"
	 | 24 -> (* getuid *)
	     failwith "Unhandled Linux system call getuid (24)"
	 | 25 -> (* stime *)
	     failwith "Unhandled Linux system call stime (25)"
	 | 26 -> (* ptrace *)
	     failwith "Unhandled Linux system call ptrace (26)"
	 | 27 -> (* alarm *)
	     failwith "Unhandled Linux system call alarm (27)"
	 | 28 -> (* oldfstat *)
	     failwith "Unhandled Linux system call oldfstat (28)"
	 | 29 -> (* pause *)
	     failwith "Unhandled Linux system call pause (29)"
	 | 30 -> (* utime *)
	     failwith "Unhandled Linux system call utime (30)"
	 | 31 -> (* stty *)
	     failwith "Unhandled Linux system call stty (31)"
	 | 32 -> (* gtty *)
	     failwith "Unhandled Linux system call gtty (32)"
	 | 33 -> (* access *)
	     let path_buf = ebx and
		 mode     = Int64.to_int ecx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "access(\"%s\", 0x%x)" path mode;
	       self#sys_access path mode
	 | 34 -> (* nice *)
	     failwith "Unhandled Linux system call nice (34)"
	 | 35 -> (* ftime *)
	     failwith "Unhandled Linux system call ftime (35)"
	 | 36 -> (* sync *)
	     failwith "Unhandled Linux system call sync (36)"
	 | 37 -> (* kill *)
	     failwith "Unhandled Linux system call kill (37)"
	 | 38 -> (* rename *)
	     failwith "Unhandled Linux system call rename (38)"
	 | 39 -> (* mkdir *)
	     failwith "Unhandled Linux system call mkdir (39)"
	 | 40 -> (* rmdir *)
	     failwith "Unhandled Linux system call rmdir (40)"
	 | 41 -> (* dup *)
	     failwith "Unhandled Linux system call dup (41)"
	 | 42 -> (* pipe *)
	     failwith "Unhandled Linux system call pipe (42)"
	 | 43 -> (* times *)
	     let addr = ebx in
	       Printf.printf "times(0x%08Lx)" addr;
	       self#sys_times addr
	 | 44 -> (* prof *)
	     failwith "Unhandled Linux system call prof (44)"
	 | 45 -> (* brk *)
	     let addr = ebx in
	       Printf.printf "brk(0x%08Lx)" addr;
	       self#sys_brk addr
	 | 46 -> (* setgid *)
	     failwith "Unhandled Linux system call setgid (46)"
	 | 47 -> (* getgid *)
	     failwith "Unhandled Linux system call getgid (47)"
	 | 48 -> (* signal *)
	     failwith "Unhandled Linux system call signal (48)"
	 | 49 -> (* geteuid *)
	     failwith "Unhandled Linux system call geteuid (49)"
	 | 50 -> (* getegid *)
	     failwith "Unhandled Linux system call getegid (50)"
	 | 51 -> (* acct *)
	     failwith "Unhandled Linux system call acct (51)"
	 | 52 -> (* umount2 *)
	     failwith "Unhandled Linux system call umount2 (52)"
	 | 53 -> (* lock *)
	     failwith "Unhandled Linux system call lock (53)"
	 | 54 -> (* ioctl *)
	     let fd   = Int64.to_int ebx and
		 req  = ecx and
		 argp = edx in
	       Printf.printf "ioctl(%d, 0x%Lx, 0x%08Lx)" fd req argp;
	       self#sys_ioctl fd req argp;
	 | 55 -> (* fcntl *)
	     failwith "Unhandled Linux system call fcntl (55)"
	 | 56 -> (* mpx *)
	     failwith "Unhandled Linux system call mpx (56)"
	 | 57 -> (* setpgid *)
	     failwith "Unhandled Linux system call setpgid (57)"
	 | 58 -> (* ulimit *)
	     failwith "Unhandled Linux system call ulimit (58)"
	 | 59 -> (* oldolduname *)
	     failwith "Unhandled Linux system call oldolduname (59)"
	 | 60 -> (* umask *)
	     failwith "Unhandled Linux system call umask (60)"
	 | 61 -> (* chroot *)
	     failwith "Unhandled Linux system call chroot (61)"
	 | 62 -> (* ustat *)
	     failwith "Unhandled Linux system call ustat (62)"
	 | 63 -> (* dup2 *)
	     failwith "Unhandled Linux system call dup2 (63)"
	 | 64 -> (* getppid *)
	     failwith "Unhandled Linux system call getppid (64)"
	 | 65 -> (* getpgrp *)
	     failwith "Unhandled Linux system call getpgrp (65)"
	 | 66 -> (* setsid *)
	     failwith "Unhandled Linux system call setsid (66)"
	 | 67 -> (* sigaction *)
	     failwith "Unhandled Linux system call sigaction (67)"
	 | 68 -> (* sgetmask *)
	     failwith "Unhandled Linux system call sgetmask (68)"
	 | 69 -> (* ssetmask *)
	     failwith "Unhandled Linux system call ssetmask (69)"
	 | 70 -> (* setreuid *)
	     failwith "Unhandled Linux system call setreuid (70)"
	 | 71 -> (* setregid *)
	     failwith "Unhandled Linux system call setregid (71)"
	 | 72 -> (* sigsuspend *)
	     failwith "Unhandled Linux system call sigsuspend (72)"
	 | 73 -> (* sigpending *)
	     failwith "Unhandled Linux system call sigpending (73)"
	 | 74 -> (* sethostname *)
	     failwith "Unhandled Linux system call sethostname (74)"
	 | 75 -> (* setrlimit *)
	     failwith "Unhandled Linux system call setrlimit (75)"
	 | 76 -> (* getrlimit *)
	     failwith "Unhandled Linux system call getrlimit (76)"
	 | 77 -> (* getrusage *)
	     failwith "Unhandled Linux system call getrusage (77)"
	 | 78 -> (* gettimeofday *)
	     failwith "Unhandled Linux system call gettimeofday (78)"
	 | 79 -> (* settimeofday *)
	     failwith "Unhandled Linux system call settimeofday (79)"
	 | 80 -> (* getgroups *)
	     failwith "Unhandled Linux system call getgroups (80)"
	 | 81 -> (* setgroups *)
	     failwith "Unhandled Linux system call setgroups (81)"
	 | 82 -> (* select *)
	     failwith "Unhandled Linux system call select (82)"
	 | 83 -> (* symlink *)
	     failwith "Unhandled Linux system call symlink (83)"
	 | 84 -> (* oldlstat *)
	     failwith "Unhandled Linux system call oldlstat (84)"
	 | 85 -> (* readlink *)
	     let path_buf = ebx and
		 out_buf  = ecx and
		 buflen   = Int64.to_int edx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "readlink(\"%s\", 0x%08Lx, %d)"
		 path out_buf buflen;
	       self#sys_readlink path out_buf buflen
	 | 86 -> (* uselib *)
	     failwith "Unhandled Linux system call uselib (86)"
	 | 87 -> (* swapon *)
	     failwith "Unhandled Linux system call swapon (87)"
	 | 88 -> (* reboot *)
	     failwith "Unhandled Linux system call reboot (88)"
	 | 89 -> (* readdir *)
	     failwith "Unhandled Linux system call readdir (89)"
	 | 90 -> (* mmap *)
	     failwith "Unhandled Linux system call mmap (90)"
	 | 91 -> (* munmap *)
	     let addr = ebx and
		 len  = ecx in
	       Printf.printf "munmap(0x%08Lx, %Ld)" addr len;
	       self#sys_munmap addr len
	 | 92 -> (* truncate *)
	     failwith "Unhandled Linux system call truncate (92)"
	 | 93 -> (* ftruncate *)
	     failwith "Unhandled Linux system call ftruncate (93)"
	 | 94 -> (* fchmod *)
	     failwith "Unhandled Linux system call fchmod (94)"
	 | 95 -> (* fchown *)
	     failwith "Unhandled Linux system call fchown (95)"
	 | 96 -> (* getpriority *)
	     failwith "Unhandled Linux system call getpriority (96)"
	 | 97 -> (* setpriority *)
	     failwith "Unhandled Linux system call setpriority (97)"
	 | 98 -> (* profil *)
	     failwith "Unhandled Linux system call profil (98)"
	 | 99 -> (* statfs *)
	     failwith "Unhandled Linux system call statfs (99)"
	 | 100 -> (* fstatfs *)
	     failwith "Unhandled Linux system call fstatfs (100)"
	 | 101 -> (* ioperm *)
	     failwith "Unhandled Linux system call ioperm (101)"
	 | 102 -> (* socketcall *)
	     failwith "Unhandled Linux system call socketcall (102)"
	 | 103 -> (* syslog *)
	     failwith "Unhandled Linux system call syslog (103)"
	 | 104 -> (* setitimer *)
	     failwith "Unhandled Linux system call setitimer (104)"
	 | 105 -> (* getitimer *)
	     failwith "Unhandled Linux system call getitimer (105)"
	 | 106 -> (* stat *)
	     failwith "Unhandled Linux system call stat (106)"
	 | 107 -> (* lstat *)
	     failwith "Unhandled Linux system call lstat (107)"
	 | 108 -> (* fstat *)
	     failwith "Unhandled Linux system call fstat (108)"
	 | 109 -> (* olduname *)
	     failwith "Unhandled Linux system call olduname (109)"
	 | 110 -> (* iopl *)
	     failwith "Unhandled Linux system call iopl (110)"
	 | 111 -> (* vhangup *)
	     failwith "Unhandled Linux system call vhangup (111)"
	 | 112 -> (* idle *)
	     failwith "Unhandled Linux system call idle (112)"
	 | 113 -> (* vm86old *)
	     failwith "Unhandled Linux system call vm86old (113)"
	 | 114 -> (* wait4 *)
	     failwith "Unhandled Linux system call wait4 (114)"
	 | 115 -> (* swapoff *)
	     failwith "Unhandled Linux system call swapoff (115)"
	 | 116 -> (* sysinfo *)
	     failwith "Unhandled Linux system call sysinfo (116)"
	 | 117 -> (* ipc *)
	     failwith "Unhandled Linux system call ipc (117)"
	 | 118 -> (* fsync *)
	     failwith "Unhandled Linux system call fsync (118)"
	 | 119 -> (* sigreturn *)
	     failwith "Unhandled Linux system call sigreturn (119)"
	 | 120 -> (* clone *)
	     failwith "Unhandled Linux system call clone (120)"
	 | 121 -> (* setdomainname *)
	     failwith "Unhandled Linux system call setdomainname (121)"
	 | 122 -> (* uname *)
	     let buf = ebx in
	       Printf.printf "uname(0x%08Lx)" buf;
	       self#sys_uname buf
	 | 123 -> (* modify_ldt *)
	     failwith "Unhandled Linux system call modify_ldt (123)"
	 | 124 -> (* adjtimex *)
	     failwith "Unhandled Linux system call adjtimex (124)"
	 | 125 -> (* mprotect *)
	     let addr = ebx and
		 len  = ecx and
		 prot = edx in
	       Printf.printf "mprotect(0x%08Lx, %Ld, %Ld)" addr len prot;
	       self#sys_mprotect addr len prot
	 | 126 -> (* sigprocmask *)
	     failwith "Unhandled Linux system call sigprocmask (126)"
	 | 127 -> (* create_module *)
	     failwith "Unhandled Linux system call create_module (127)"
	 | 128 -> (* init_module *)
	     failwith "Unhandled Linux system call init_module (128)"
	 | 129 -> (* delete_module *)
	     failwith "Unhandled Linux system call delete_module (129)"
	 | 130 -> (* get_kernel_syms *)
	     failwith "Unhandled Linux system call get_kernel_syms (130)"
	 | 131 -> (* quotactl *)
	     failwith "Unhandled Linux system call quotactl (131)"
	 | 132 -> (* getpgid *)
	     failwith "Unhandled Linux system call getpgid (132)"
	 | 133 -> (* fchdir *)
	     failwith "Unhandled Linux system call fchdir (133)"
	 | 134 -> (* bdflush *)
	     failwith "Unhandled Linux system call bdflush (134)"
	 | 135 -> (* sysfs *)
	     failwith "Unhandled Linux system call sysfs (135)"
	 | 136 -> (* personality *)
	     failwith "Unhandled Linux system call personality (136)"
	 | 137 -> (* afs_syscall *)
	     failwith "Unhandled Linux system call afs_syscall (137)"
	 | 138 -> (* setfsuid *)
	     failwith "Unhandled Linux system call setfsuid (138)"
	 | 139 -> (* setfsgid *)
	     failwith "Unhandled Linux system call setfsgid (139)"
	 | 140 -> (* _llseek *)
	     failwith "Unhandled Linux system call _llseek (140)"
	 | 141 -> (* getdents *)
	     failwith "Unhandled Linux system call getdents (141)"
	 | 142 -> (* _newselect *)
	     failwith "Unhandled Linux system call _newselect (142)"
	 | 143 -> (* flock *)
	     failwith "Unhandled Linux system call flock (143)"
	 | 144 -> (* msync *)
	     failwith "Unhandled Linux system call msync (144)"
	 | 145 -> (* readv *)
	     failwith "Unhandled Linux system call readv (145)"
	 | 146 -> (* writev *)
	     let fd  = Int64.to_int ebx and
		 iov = ecx and
		 cnt = Int64.to_int edx in
	       Printf.printf "writev(%d, 0x%08Lx, %d)" fd iov cnt;
	       self#sys_writev fd iov cnt
	 | 147 -> (* getsid *)
	     failwith "Unhandled Linux system call getsid (147)"
	 | 148 -> (* fdatasync *)
	     failwith "Unhandled Linux system call fdatasync (148)"
	 | 149 -> (* _sysctl *)
	     failwith "Unhandled Linux system call _sysctl (149)"
	 | 150 -> (* mlock *)
	     failwith "Unhandled Linux system call mlock (150)"
	 | 151 -> (* munlock *)
	     failwith "Unhandled Linux system call munlock (151)"
	 | 152 -> (* mlockall *)
	     failwith "Unhandled Linux system call mlockall (152)"
	 | 153 -> (* munlockall *)
	     failwith "Unhandled Linux system call munlockall (153)"
	 | 154 -> (* sched_setparam *)
	     failwith "Unhandled Linux system call sched_setparam (154)"
	 | 155 -> (* sched_getparam *)
	     failwith "Unhandled Linux system call sched_getparam (155)"
	 | 156 -> (* sched_setscheduler *)
	     failwith "Unhandled Linux system call sched_setscheduler (156)"
	 | 157 -> (* sched_getscheduler *)
	     failwith "Unhandled Linux system call sched_getscheduler (157)"
	 | 158 -> (* sched_yield *)
	     failwith "Unhandled Linux system call sched_yield (158)"
	 | 159 -> (* sched_get_priority_max *)
	     failwith "Unhandled Linux system call sched_get_priority_max (159)"
	 | 160 -> (* sched_get_priority_min *)
	     failwith "Unhandled Linux system call sched_get_priority_min (160)"
	 | 161 -> (* sched_rr_get_interval *)
	     failwith "Unhandled Linux system call sched_rr_get_interval (161)"
	 | 162 -> (* nanosleep *)
	     failwith "Unhandled Linux system call nanosleep (162)"
	 | 163 -> (* mremap *)
	     failwith "Unhandled Linux system call mremap (163)"
	 | 164 -> (* setresuid *)
	     failwith "Unhandled Linux system call setresuid (164)"
	 | 165 -> (* getresuid *)
	     failwith "Unhandled Linux system call getresuid (165)"
	 | 166 -> (* vm86 *)
	     failwith "Unhandled Linux system call vm86 (166)"
	 | 167 -> (* query_module *)
	     failwith "Unhandled Linux system call query_module (167)"
	 | 168 -> (* poll *)
	     failwith "Unhandled Linux system call poll (168)"
	 | 169 -> (* nfsservctl *)
	     failwith "Unhandled Linux system call nfsservctl (169)"
	 | 170 -> (* setresgid *)
	     failwith "Unhandled Linux system call setresgid (170)"
	 | 171 -> (* getresgid *)
	     failwith "Unhandled Linux system call getresgid (171)"
	 | 172 -> (* prctl *)
	     failwith "Unhandled Linux system call prctl (172)"
	 | 173 -> (* rt_sigreturn *)
	     failwith "Unhandled Linux system call rt_sigreturn (173)"
	 | 174 -> (* rt_sigaction *)
	     let signum = Int64.to_int ebx and
		 newbuf = ecx and
		 oldbuf = edx and
		 setlen = Int64.to_int esi in
	       Printf.printf "rt_sigaction(%d, 0x%08Lx, 0x%08Lx, %d)"
		 signum newbuf oldbuf setlen;
	       self#sys_rt_sigaction signum newbuf oldbuf setlen
	 | 175 -> (* rt_sigprocmask *)
	     let how    = Int64.to_int ebx and
		 newset = ecx and
		 oldset = edx and
		 setlen = Int64.to_int esi in
	       Printf.printf "rt_sigprocmask(%d, 0x%08Lx, 0x%08Lx, %d)"
		 how newset oldset setlen;
	       self#sys_rt_sigprocmask how newset oldset setlen
	 | 176 -> (* rt_sigpending *)
	     failwith "Unhandled Linux system call rt_sigpending (176)"
	 | 177 -> (* rt_sigtimedwait *)
	     failwith "Unhandled Linux system call rt_sigtimedwait (177)"
	 | 178 -> (* rt_sigqueueinfo *)
	     failwith "Unhandled Linux system call rt_sigqueueinfo (178)"
	 | 179 -> (* rt_sigsuspend *)
	     failwith "Unhandled Linux system call rt_sigsuspend (179)"
	 | 180 -> (* pread64 *)
	     failwith "Unhandled Linux system call pread64 (180)"
	 | 181 -> (* pwrite64 *)
	     failwith "Unhandled Linux system call pwrite64 (181)"
	 | 182 -> (* chown *)
	     failwith "Unhandled Linux system call chown (182)"
	 | 183 -> (* getcwd *)
	     failwith "Unhandled Linux system call getcwd (183)"
	 | 184 -> (* capget *)
	     failwith "Unhandled Linux system call capget (184)"
	 | 185 -> (* capset *)
	     failwith "Unhandled Linux system call capset (185)"
	 | 186 -> (* sigaltstack *)
	     failwith "Unhandled Linux system call sigaltstack (186)"
	 | 187 -> (* sendfile *)
	     failwith "Unhandled Linux system call sendfile (187)"
	 | 188 -> (* getpmsg *)
	     failwith "Unhandled Linux system call getpmsg (188)"
	 | 189 -> (* putpmsg *)
	     failwith "Unhandled Linux system call putpmsg (189)"
	 | 190 -> (* vfork *)
	     failwith "Unhandled Linux system call vfork (190)"
	 | 191 -> (* ugetrlimit *)
	     let rsrc = Int64.to_int ebx and
		 buf  = ecx in
	       Printf.printf "ugetrlimit(%d, 0x%08Lx)" rsrc buf;
	       self#sys_ugetrlimit rsrc buf
	 | 192 -> (* mmap2 *)
	     let addr     = ebx and
		 length   = ecx and
		 prot     = edx and
		 flags    = esi and
		 fd       = Int64.to_int edi and
		 pgoffset = Int64.to_int ebp in
	       Printf.printf "mmap2(0x%08Lx, %Ld, 0x%Lx, 0x%0Lx, %d, %d)"
		 addr length prot flags fd pgoffset;
	       self#sys_mmap2 addr length prot flags fd pgoffset
	 | 193 -> (* truncate64 *)
	     failwith "Unhandled Linux system call truncate64 (193)"
	 | 194 -> (* ftruncate64 *)
	     failwith "Unhandled Linux system call ftruncate64 (194)"
	 | 195 -> (* stat64 *)
	     let path_buf = ebx and
		 buf_addr = ecx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "stat64(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_stat64 path buf_addr
	 | 196 -> (* lstat64 *)
	     failwith "Unhandled Linux system call lstat64 (196)"
	 | 197 -> (* fstat64 *)
	     let fd = Int64.to_int ebx and
		 buf_addr = ecx in
	       Printf.printf "fstat64(%d, 0x%08Lx)" fd buf_addr;
	       self#sys_fstat64 fd buf_addr
	 | 198 -> (* lchown32 *)
	     failwith "Unhandled Linux system call lchown32 (198)"
	 | 199 -> (* getuid32 *)
	     Printf.printf "getuid32()";
	     self#sys_getuid32 ()
	 | 200 -> (* getgid32 *)
	     Printf.printf "getgid32()";
	     self#sys_getgid32 ()
	 | 201 -> (* geteuid32 *)
	     Printf.printf "geteuid32()";
	     self#sys_geteuid32 ()
	 | 202 -> (* getegid32 *)
	     Printf.printf "getegid32()";
	     self#sys_getegid32 ()
	 | 203 -> (* setreuid32 *)
	     failwith "Unhandled Linux system call setreuid32 (203)"
	 | 204 -> (* setregid32 *)
	     failwith "Unhandled Linux system call setregid32 (204)"
	 | 205 -> (* getgroups32 *)
	     failwith "Unhandled Linux system call getgroups32 (205)"
	 | 206 -> (* setgroups32 *)
	     failwith "Unhandled Linux system call setgroups32 (206)"
	 | 207 -> (* fchown32 *)
	     failwith "Unhandled Linux system call fchown32 (207)"
	 | 208 -> (* setresuid32 *)
	     failwith "Unhandled Linux system call setresuid32 (208)"
	 | 209 -> (* getresuid32 *)
	     failwith "Unhandled Linux system call getresuid32 (209)"
	 | 210 -> (* setresgid32 *)
	     failwith "Unhandled Linux system call setresgid32 (210)"
	 | 211 -> (* getresgid32 *)
	     failwith "Unhandled Linux system call getresgid32 (211)"
	 | 212 -> (* chown32 *)
	     failwith "Unhandled Linux system call chown32 (212)"
	 | 213 -> (* setuid32 *)
	     failwith "Unhandled Linux system call setuid32 (213)"
	 | 214 -> (* setgid32 *)
	     failwith "Unhandled Linux system call setgid32 (214)"
	 | 215 -> (* setfsuid32 *)
	     failwith "Unhandled Linux system call setfsuid32 (215)"
	 | 216 -> (* setfsgid32 *)
	     failwith "Unhandled Linux system call setfsgid32 (216)"
	 | 217 -> (* pivot_root *)
	     failwith "Unhandled Linux system call pivot_root (217)"
	 | 218 -> (* mincore *)
	     failwith "Unhandled Linux system call mincore (218)"
	 | 219 -> (* madvise *)
	     failwith "Unhandled Linux system call madvise (219)"
	 | 220 -> (* getdents64 *)
	     failwith "Unhandled Linux system call getdents64 (220)"
	 | 221 -> (* fcntl64 *)
	     failwith "Unhandled Linux system call fcntl64 (221)"
	 | 224 -> (* gettid *)
	     failwith "Unhandled Linux system call gettid (224)"
	 | 225 -> (* readahead *)
	     failwith "Unhandled Linux system call readahead (225)"
	 | 226 -> (* setxattr *)
	     failwith "Unhandled Linux system call setxattr (226)"
	 | 227 -> (* lsetxattr *)
	     failwith "Unhandled Linux system call lsetxattr (227)"
	 | 228 -> (* fsetxattr *)
	     failwith "Unhandled Linux system call fsetxattr (228)"
	 | 229 -> (* getxattr *)
	     failwith "Unhandled Linux system call getxattr (229)"
	 | 230 -> (* lgetxattr *)
	     failwith "Unhandled Linux system call lgetxattr (230)"
	 | 231 -> (* fgetxattr *)
	     failwith "Unhandled Linux system call fgetxattr (231)"
	 | 232 -> (* listxattr *)
	     failwith "Unhandled Linux system call listxattr (232)"
	 | 233 -> (* llistxattr *)
	     failwith "Unhandled Linux system call llistxattr (233)"
	 | 234 -> (* flistxattr *)
	     failwith "Unhandled Linux system call flistxattr (234)"
	 | 235 -> (* removexattr *)
	     failwith "Unhandled Linux system call removexattr (235)"
	 | 236 -> (* lremovexattr *)
	     failwith "Unhandled Linux system call lremovexattr (236)"
	 | 237 -> (* fremovexattr *)
	     failwith "Unhandled Linux system call fremovexattr (237)"
	 | 238 -> (* tkill *)
	     failwith "Unhandled Linux system call tkill (238)"
	 | 239 -> (* sendfile64 *)
	     failwith "Unhandled Linux system call sendfile64 (239)"
	 | 240 -> (* futex *)
	     let uaddr    = ebx and
		 op       = Int64.to_int ecx and
		 value    = edx and
		 timebuf  = esi and
		 uaddr2   = edi and
		 val3     = ebp in
	       Printf.printf "futex(0x%08Lx, %d, %Ld, 0x%08Lx, 0x%08Lx, %Ld)"
		 uaddr op value timebuf uaddr2 val3;
	       self#sys_futex uaddr op value timebuf uaddr2 val3
	 | 241 -> (* sched_setaffinity *)
	     failwith "Unhandled Linux system call sched_setaffinity (241)"
	 | 242 -> (* sched_getaffinity *)
	     failwith "Unhandled Linux system call sched_getaffinity (242)"
	 | 243 -> (* set_thread_area *)
	     let uinfo = ebx in
	       Printf.printf "set_thread_area(0x%08Lx)" uinfo;
	       self#sys_set_thread_area uinfo
	 | 244 -> (* get_thread_area *)
	     failwith "Unhandled Linux system call get_thread_area (244)"
	 | 245 -> (* io_setup *)
	     failwith "Unhandled Linux system call io_setup (245)"
	 | 246 -> (* io_destroy *)
	     failwith "Unhandled Linux system call io_destroy (246)"
	 | 247 -> (* io_getevents *)
	     failwith "Unhandled Linux system call io_getevents (247)"
	 | 248 -> (* io_submit *)
	     failwith "Unhandled Linux system call io_submit (248)"
	 | 249 -> (* io_cancel *)
	     failwith "Unhandled Linux system call io_cancel (249)"
	 | 250 -> (* fadvise64 *)
	     failwith "Unhandled Linux system call fadvise64 (250)"
	 | 252 -> (* exit_group *)
	     let status = ebx in
	       Printf.printf "exit_group(%Ld) (no return)\n" status;
	       self#sys_exit_group status
	 | 253 -> (* lookup_dcookie *)
	     failwith "Unhandled Linux system call lookup_dcookie (253)"
	 | 254 -> (* epoll_create *)
	     failwith "Unhandled Linux system call epoll_create (254)"
	 | 255 -> (* epoll_ctl *)
	     failwith "Unhandled Linux system call epoll_ctl (255)"
	 | 256 -> (* epoll_wait *)
	     failwith "Unhandled Linux system call epoll_wait (256)"
	 | 257 -> (* remap_file_pages *)
	     failwith "Unhandled Linux system call remap_file_pages (257)"
	 | 258 -> (* set_tid_address *)
	     let addr = ebx in
	       Printf.printf "set_tid_address(0x08%Lx)" addr;
	       self#sys_set_tid_address addr
	 | 259 -> (* timer_create *)
	     failwith "Unhandled Linux system call timer_create (259)"
	 | 268 -> (* statfs64 *)
	     failwith "Unhandled Linux system call statfs64 (268)"
	 | 269 -> (* fstatfs64 *)
	     failwith "Unhandled Linux system call fstatfs64 (269)"
	 | 270 -> (* tgkill *)
	     failwith "Unhandled Linux system call tgkill (270)"
	 | 271 -> (* utimes *)
	     failwith "Unhandled Linux system call utimes (271)"
	 | 272 -> (* fadvise64_64 *)
	     failwith "Unhandled Linux system call fadvise64_64 (272)"
	 | 273 -> (* vserver *)
	     failwith "Unhandled Linux system call vserver (273)"
	 | 274 -> (* mbind *)
	     failwith "Unhandled Linux system call mbind (274)"
	 | 275 -> (* get_mempolicy *)
	     failwith "Unhandled Linux system call get_mempolicy (275)"
	 | 276 -> (* set_mempolicy *)
	     failwith "Unhandled Linux system call set_mempolicy (276)"
	 | 277 -> (* mq_open *)
	     failwith "Unhandled Linux system call mq_open (277)"
	 | 283 -> (* kexec_load *)
	     failwith "Unhandled Linux system call kexec_load (283)"
	 | 284 -> (* waitid *)
	     failwith "Unhandled Linux system call waitid (284)"
	 | 286 -> (* add_key *)
	     failwith "Unhandled Linux system call add_key (286)"
	 | 287 -> (* request_key *)
	     failwith "Unhandled Linux system call request_key (287)"
	 | 288 -> (* keyctl *)
	     failwith "Unhandled Linux system call keyctl (288)"
	 | 289 -> (* ioprio_set *)
	     failwith "Unhandled Linux system call ioprio_set (289)"
	 | 290 -> (* ioprio_get *)
	     failwith "Unhandled Linux system call ioprio_get (290)"
	 | 291 -> (* inotify_init *)
	     failwith "Unhandled Linux system call inotify_init (291)"
	 | 292 -> (* inotify_add_watch *)
	     failwith "Unhandled Linux system call inotify_add_watch (292)"
	 | 293 -> (* inotify_rm_watch *)
	     failwith "Unhandled Linux system call inotify_rm_watch (293)"
	 | 294 -> (* migrate_pages *)
	     failwith "Unhandled Linux system call migrate_pages (294)"
	 | 295 -> (* openat *)
	     failwith "Unhandled Linux system call openat (295)"
	 | 296 -> (* mkdirat *)
	     failwith "Unhandled Linux system call mkdirat (296)"
	 | 297 -> (* mknodat *)
	     failwith "Unhandled Linux system call mknodat (297)"
	 | 298 -> (* fchownat *)
	     failwith "Unhandled Linux system call fchownat (298)"
	 | 299 -> (* futimesat *)
	     failwith "Unhandled Linux system call futimesat (299)"
	 | 300 -> (* fstatat64 *)
	     failwith "Unhandled Linux system call fstatat64 (300)"
	 | 301 -> (* unlinkat *)
	     failwith "Unhandled Linux system call unlinkat (301)"
	 | 302 -> (* renameat *)
	     failwith "Unhandled Linux system call renameat (302)"
	 | 303 -> (* linkat *)
	     failwith "Unhandled Linux system call linkat (303)"
	 | 304 -> (* symlinkat *)
	     failwith "Unhandled Linux system call symlinkat (304)"
	 | 305 -> (* readlinkat *)
	     failwith "Unhandled Linux system call readlinkat (305)"
	 | 306 -> (* fchmodat *)
	     failwith "Unhandled Linux system call fchmodat (306)"
	 | 307 -> (* faccessat *)
	     failwith "Unhandled Linux system call faccessat (307)"
	 | 308 -> (* pselect6 *)
	     failwith "Unhandled Linux system call pselect6 (308)"
	 | 309 -> (* ppoll *)
	     failwith "Unhandled Linux system call ppoll (309)"
	 | 310 -> (* unshare *)
	     failwith "Unhandled Linux system call unshare (310)"
	 | 311 -> (* set_robust_list *)
	     let addr = ebx and
		 len  = ecx in
	       Printf.printf "set_robust_list(0x08%Lx, %Ld)" addr len;
	       self#sys_set_robust_list addr len
	 | 312 -> (* get_robust_list *)
	     failwith "Unhandled Linux system call get_robust_list (312)"
	 | 313 -> (* splice *)
	     failwith "Unhandled Linux system call splice (313)"
	 | 314 -> (* sync_file_range *)
	     failwith "Unhandled Linux system call sync_file_range (314)"
	 | 315 -> (* tee *)
	     failwith "Unhandled Linux system call tee (315)"
	 | 316 -> (* vmsplice *)
	     failwith "Unhandled Linux system call vmsplice (316)"
	 | 317 -> (* move_pages *)
	     failwith "Unhandled Linux system call move_pages (317)"
	 | 318 -> (* getcpu *)
	     failwith "Unhandled Linux system call getcpu (318)"
	 | 319 -> (* epoll_pwait *)
	     failwith "Unhandled Linux system call epoll_pwait (319)"
	 | 320 -> (* utimensat *)
	     failwith "Unhandled Linux system call utimensat (320)"
	 | 321 -> (* signalfd *)
	     failwith "Unhandled Linux system call signalfd (321)"
	 | 322 -> (* timerfd_create *)
	     failwith "Unhandled Linux system call timerfd_create (322)"
	 | 323 -> (* eventfd *)
	     failwith "Unhandled Linux system call eventfd (323)"
	 | 324 -> (* fallocate *)
	     failwith "Unhandled Linux system call fallocate (324)"
	 | 325 -> (* timerfd_settime *)
	     failwith "Unhandled Linux system call timerfd_settime (325)"
	 | 326 -> (* timerfd_gettime *)
	     failwith "Unhandled Linux system call timerfd_gettime (326)"
	 | _ ->
	     Printf.printf "Unhandled system call %d\n" syscall_num;
	     failwith "Unhandled Linux system call");
    Printf.printf " = %Ld (0x%08Lx)\n"
      (fix_s V.REG_32 (fm#get_int_var eax_var)) (fm#get_int_var eax_var);
    flush stdout

  method handle_special str =
    match str with
      | "int 0x80" -> self#handle_linux_syscall (); true
      | _ -> false

end

let rec runloop fm eip_var eip mem_var asmir_gamma until =
  let load_byte addr = Int64.to_int (fm#load_byte addr) in
  let read_reg32 var = fm#get_int_var var in
  let (eax_var, ebx_var, ecx_var, edx_var, esi_var, edi_var,
       esp_var, ebp_var, eflags_var, gs_var, gdt_var) =
    match fm#get_x86_gprs () with
      | [a; b; c; d; si; di; sp; bp; fl; gs; gd]
	-> (a, b, c, d, si, di, sp, bp, fl, gs, gd)
      | _ -> failwith "Bad length for gpr_vars"
  in
  let decode_insn eip =
    let insn_bytes = Array.init 16
      (fun i -> Char.chr (load_byte (Int64.add eip (Int64.of_int i))))
    in
    let asmp = Libasmir.byte_insn_to_asmp
      Libasmir.Bfd_arch_i386 eip insn_bytes in
    let sl = Asmir.asm_addr_to_vine asmir_gamma asmp eip in
      Libasmir.free_asm_program asmp;
      match sl with 
	| [V.Block(dl', sl')] -> (dl', sl')
	| _ -> failwith "expected asm_addr_to_vine to give single block"
  in
  let label_to_eip s =
    let len = String.length s in
    let hex = String.sub s 3 (len - 3) in (* remove "pc_" *)
      Int64.of_string hex
  in
  let rec last l =
    match l with
      | [e] -> e
      | a :: r -> last r
      | [] -> failwith "Empty list in last"
  in
  let rec decode_insns eip k first =
    if k = 0 then ([], []) else
      let (dl, sl) = decode_insn eip in
	if
	  List.exists (function V.Special("int 0x80") -> true | _ -> false) sl
	then
	  (* Make a system call be alone in its basic block *)
	  if first then (dl, sl) else ([], [])
	else
	  match last (rm_unused_stmts sl) with
	    | V.Jmp(V.Name(lab)) ->
		let next_eip = label_to_eip lab in
		let (dl', sl') = decode_insns next_eip (k - 1) false in
		  (dl @ dl', sl @ sl')
	    | _ -> (dl, sl) (* end of basic block, e.g. indirect jump *)
  in
  let decode_insns_cached eip =
    try
      Hashtbl.find trans_cache eip
    with
	Not_found ->
	  Hashtbl.add trans_cache eip
	    (simplify_frag (decode_insns eip 10 true));
	  Hashtbl.find trans_cache eip
  in
  let print_gprs () =
    Printf.printf "eax:%08Lx ebx:%08Lx ecx:%08Lx edx:%08Lx\n"
      (read_reg32 eax_var) (read_reg32 ebx_var) 
      (read_reg32 ecx_var) (read_reg32 edx_var);
    Printf.printf "esi:%08Lx edi:%08Lx esp:%08Lx ebp:%08Lx\n"
      (read_reg32 esi_var) (read_reg32 edi_var) 
      (read_reg32 esp_var) (read_reg32 ebp_var);
    Printf.printf "eip:%08Lx eflags:%08Lx\n"
      eip (read_reg32 eflags_var)
  in
  (* Remove "unknown" statments it seems safe to ignore *)
  let remove_known_unknowns sl =
    List.filter
      (function
	 | V.Move(_, V.Unknown("CCall: x86g_create_fpucw")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_check_fldcw")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_create_mxcsr")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_check_ldmxcsr")) -> false
	 | V.Move(_, V.Unknown("Unknown: GetI")) -> false
	     (* e.g., FPU load *)
	 | V.Move(_, V.Unknown("Floating point binop")) -> false
	 | V.Move(_, V.Unknown("Floating point triop")) -> false
	 | V.Move(_, V.Unknown("floatcast")) -> false
	 | V.ExpStmt(V.Unknown("Unknown: PutI")) -> false
	     (* e.g., FPU store *)
	 | V.ExpStmt(V.Unknown("Unknown: Dirty")) -> false
	     (* XXX too broad? covers rdtsc *)
	 | _ -> true)
      sl
  in
  let rec loop eip =
    let (dl, sl) = decode_insns_cached eip in
    let prog = (dl, (remove_known_unknowns sl)) in
      (* Libasmir.print_disasm_rawbytes Libasmir.Bfd_arch_i386 eip insn_bytes;
	 print_string "\n"; *)
      (* Printf.printf "EIP is %08Lx\n" eip; *)
      (* Printf.printf "Watchpoint val is %02x\n" (load_byte 0x501650e8L); *)
      (* Printf.printf ("Insn bytes are %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n") (load_byte eip)
	 (load_byte (Int64.add eip (Int64.of_int 1)))
	 (load_byte (Int64.add eip (Int64.of_int 2)))
	 (load_byte (Int64.add eip (Int64.of_int 3)))
	 (load_byte (Int64.add eip (Int64.of_int 4)))
	 (load_byte (Int64.add eip (Int64.of_int 5)))
	 (load_byte (Int64.add eip (Int64.of_int 6)))
	 (load_byte (Int64.add eip (Int64.of_int 7)))
	 (load_byte (Int64.add eip (Int64.of_int 8)))
	 (load_byte (Int64.add eip (Int64.of_int 9)))
	 (load_byte (Int64.add eip (Int64.of_int 10)))
	 (load_byte (Int64.add eip (Int64.of_int 11)))
	 (load_byte (Int64.add eip (Int64.of_int 12)))
	 (load_byte (Int64.add eip (Int64.of_int 13)))
	 (load_byte (Int64.add eip (Int64.of_int 14)))
	 (load_byte (Int64.add eip (Int64.of_int 15))); *)
      (* print_gprs (); *)
      (* V.pp_program print_string prog; *)
      fm#set_frag prog;
      (* flush stdout; *)
      let s = fm#run () in
	if s = "halt_0" then () else
	  let new_eip = label_to_eip s in
	    match (new_eip, until) with
	      | (e1, Some e2) when e1 = e2 -> ()
	      | (0L, _) -> failwith "Jump to 0"
	      | _ -> loop new_eip
  in
    loop eip

let usage = "trans_eval [options]* file.ir\n"
let infile = ref ""
let infile_set = ref false
let arg_name s = infile := s; infile_set := true

let random_regex maxlen =
  let len = Random.int maxlen in
  let str = String.create len in
    for i = 0 to len - 1 do
      let c = match (Random.int 25) with
	| 0 -> 'a'
	| 1 -> 'b'
	| 2 -> 'c'
	| 3 -> '|'
	| 4 -> '+'
	| 5 -> '*'
	| 6 -> '('
	| 7 -> ')'
	| 8 -> '['
	| 9 -> ']'
	| 10 -> '{'
	| 11 -> '}'
	| 12 -> ','
	| 13 -> '0'
	| 14 -> '1'
	| 15 -> '2'
	| 16 -> '4'
	| 17 -> '^'
	| 18 -> '$'
	| 19 -> '?'
	| 20 -> '.'
	| 21 -> '\\'
	| 22 -> 'w'
	| 23 -> 'd'
	| 24 -> 's'
	| _ -> failwith "random integer too big"
      in
	str.[i] <- c
    done;
    str
    
let fuzz_pcre fm eip_var mem_var asmir_gamma =
  let eip = fm#get_int_var eip_var
  in
    runloop fm eip_var eip mem_var asmir_gamma (Some 0x08048656L);
    fm#make_snap (); Printf.printf "Took snapshot\n";
    let iter = ref 0L in
      while true do
	iter := Int64.add !iter 1L;
	let regex = random_regex 20 and
	    old_tcs = Hashtbl.length trans_cache in
	  Printf.printf "Iteration %Ld: %s\n" !iter regex;
	  fm#store_cstr 0x08063c20L 0L regex;
	  (try
	     runloop fm eip_var 0x08048656L mem_var asmir_gamma
	       (Some 0x080486d2L);
	   with
	     | Failure("Jump to 0") -> () (* equivalent of segfault *)
	     | SimulatedExit(_) -> ()
	  );
	  if (Hashtbl.length trans_cache - old_tcs > 0) then
	    Printf.printf "Coverage increased to %d with %s on %Ld\n"
	      (Hashtbl.length trans_cache) regex !iter
	  else ();
	  fm#reset ();
	  flush stdout
      done


let main argc argv = 
  let speclist = Vine_parser.defspecs in 
  let speclist = [] @ speclist in 
    Arg.parse speclist arg_name usage;
    if(!infile_set = false) then  (
      Arg.usage speclist usage; exit(-1)
    );
    let prog = (
      let p = Vine_parser.parse_file !infile in 
      let () = if !Vine_parser.flag_typecheck then
	Vine_typecheck.typecheck p else () in 
	p
    ) in 
    let () = if !Vine_parser.flag_pp then 
      Vine.pp_program (print_string) prog in
    let (dl, sl) = prog in
    let eip_var = List.find (fun (i, s, t) -> s = "R_EIP") dl in
    let mem_var = List.find (fun (i, s, t) -> s = "mem") dl in
    let asmir_gamma = Asmir.gamma_create mem_var dl in
    let fm = new frag_machine () in
    (* let fm = new fake_frag_machine prog in *)
      fm#init_prog prog;
      fm#add_special_handler (new linux_special_handler fm);
      fuzz_pcre fm eip_var mem_var asmir_gamma
      (* let eip = fm#get_int_var eip_var in
	runloop fm eip_var eip mem_var asmir_gamma None (* run until exit *) *)
;;

main (Array.length Sys.argv) Sys.argv;;

(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

open Exec_exceptions;;
open Exec_options;;

class virtual concrete_memory = object(self)
  method virtual store_byte : Int64.t -> int -> unit
  method virtual maybe_load_byte : Int64.t -> int option
  method virtual clear : unit -> unit

  method measure_size = 0

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

  method store_page addr pagestr =
    assert(Int64.logand addr 0xfffL = 0L);
    assert(String.length pagestr = 4096);
    for i = 0 to 4096 do
      self#store_byte (Int64.add addr (Int64.of_int i))
	(Char.code pagestr.[i])
    done

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

class virtual concrete_memory_w_reset = object(self)
  inherit concrete_memory

  method virtual make_snap : unit -> unit
  method virtual reset : unit -> unit
end

class concrete_string_memory = object(self)
  inherit concrete_memory

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

  method store_page addr newstr =
    assert(Int64.logand addr 0xfffL = 0L);
    assert(String.length newstr = 4096);
    let page = Int64.to_int (Int64.shift_right addr 12) in
      mem.(page) <- Some newstr

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

  method measure_size = 
    Array.fold_right
      (fun page c -> c + match page with None -> 0 | Some _ -> 4096)
      mem 0
end

class concrete_hash_memory = object(self)
  inherit concrete_memory

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

class concrete_snapshot_memory
  (main:concrete_memory_w_reset) (diff:concrete_memory_w_reset) = object(self)
  inherit concrete_memory_w_reset
    
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

class parallel_check_memory
  (mem1:concrete_memory_w_reset) (mem2:concrete_memory_w_reset)
  = object(self)
  inherit concrete_memory_w_reset

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

let all_present = String.make 512 '\xff'

class string_maybe_memory = object(self)
  inherit concrete_memory

  (* The extra page is a hacky way to not crash on address wrap-around *)
  val mem = Array.init 0x100001 (fun _ -> None)
  val bitmaps = Array.init 0x100001 (fun _ -> None)

  method private maybe_get_pages addr = 
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
      match (mem.(page), bitmaps.(page)) with
	| (Some page_str, Some bitmap) -> Some (page_str, bitmap, idx)
	| (None, None) -> None
	| _ -> failwith "mem vs. bitmaps inconsistency in string_maybe_memory"

  method private get_pages addr = 
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
      match (mem.(page), bitmaps.(page)) with
	| (Some page_str, Some bitmap) -> (page_str, bitmap, idx)
	| (None, None) ->
	    let new_page = String.make 4096 '\x00' and
		new_bitmap = String.make 512 '\x00' in
	      mem.(page) <- Some new_page;
	      bitmaps.(page) <- Some new_bitmap;
	      (new_page, new_bitmap, idx)
	| _ -> failwith "mem vs. bitmaps inconsistency in string_maybe_memory"

  method store_byte addr b =
    let (page_str, bitmap, idx) = self#get_pages addr in
      page_str.[idx] <- Char.chr b;
      let bit = 1 lsl (idx land 7) and
	  bidx = idx lsr 3 in
	bitmap.[bidx] <- (Char.chr ((Char.code bitmap.[bidx]) lor bit))
	
  method store_page addr newstr =
    assert(Int64.logand addr 0xfffL = 0L);
    assert(String.length newstr = 4096);
    let page = Int64.to_int (Int64.shift_right addr 12) in
      mem.(page) <- Some newstr;
      bitmaps.(page) <- Some all_present

  method maybe_load_byte addr =
    match (self#maybe_get_pages addr) with
      | None -> None
      | Some(page_str, bitmap, idx) ->
	  let bit = 1 lsl (idx land 7) and
	      bidx = idx lsr 3 in
	    if (Char.code bitmap.[bidx]) land bit = 0 then
	      None
	    else
	      Some (Char.code page_str.[idx])

  method load_byte addr =
    let (page_str, _, idx) = self#get_pages addr in
      Char.code page_str.[idx]

  method clear () =
    Array.fill mem 0 0x100001 None;
    Array.fill bitmaps 0 0x100001 None

  method measure_size = 
    (Array.fold_right
       (fun page c -> c + match page with None -> 0 | Some _ -> 4096)
       mem 0) +
    (Array.fold_right
       (fun page c -> c + match page with None -> 0 | Some _ -> 512)
       bitmaps 0)
end

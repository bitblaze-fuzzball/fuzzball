(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

let load_mem_ranges fm fname areas =
  let si = Temu_state.open_state fname in
    List.iter
      (fun (base,size) ->
	 let last = Int64.pred (Int64.add base size) in
	   List.iter
	     (fun (addr, ch) ->
		fm#store_byte_conc addr (Char.code ch))
	     (si#get_memrange base last))
      areas;
    Temu_state.close_state si

let load_mem_state 
    (fm :
     < load_x86_user_regs : Temu_state.userRegs -> unit;
       store_byte_conc : int64 -> int -> unit;
       store_page_conc : int64 -> string -> unit; .. >)
    fname =
  let ic = open_in fname in
  let i = IO.input_channel ic in
  let si = Temu_state.open_state fname in
    List.iter
      (fun blk ->
	 assert(Int64.logand blk#first 0xfffL = 0L);
	 assert(Int64.sub blk#last blk#first = 0xfffL);
	 LargeFile.seek_in ic blk#file_pos;
	 let page = IO.really_nread i 4096 in
	   fm#store_page_conc blk#first page)
      si#blocks;
    fm#load_x86_user_regs si#regs;
    let eip = Int64.of_int32 si#regs.Temu_state.eip in
      Temu_state.close_state si;
      eip

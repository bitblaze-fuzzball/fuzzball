open Exec_options
open Fragment_machine

module LL = Linux_loader

let read_ui32 i =
  Int64.logand 0xffffffffL (Int64.of_int32 (IO.read_real_i32 i))

let read_cgcef_header ic =
  let i = IO.input_channel ic in
  let ident = IO.really_nread i 9 in
    if ident <> "\x7fCGC\x01\x01\x01\x43\x01" then
      (Printf.printf "DECREE binary header mismatch: got %s\n" ident;
       failwith "DECREE binary header mismatch");
    ignore(IO.really_nread i 7); (* 'random values', e.g. "Merino\0" *)
    (* OCaml structure initialization isn't guaranteed to happen
       left to right, so we need to use a bunch of lets here: *)
    let eh_type = IO.read_ui16 i in
    let machine = IO.read_ui16 i in
    let version = read_ui32 i in
    let entry = read_ui32 i in
    let phoff = read_ui32 i in
    let shoff = read_ui32 i in
    let eh_flags = read_ui32 i in
    let ehsize = IO.read_ui16 i in
    let phentsize = IO.read_ui16 i in
    let phnum = IO.read_ui16 i in
    let shentsize = IO.read_ui16 i in
    let shnum = IO.read_ui16 i in
    let shstrndx = IO.read_ui16 i in
    let eh : LL.elf_header = {
      LL.eh_type = eh_type; LL.machine = machine; LL.version = version;
      LL.entry = entry; LL.phoff = phoff; LL.shoff = shoff;
      LL.eh_flags = eh_flags; LL.ehsize = ehsize; LL.phentsize = phentsize;
      LL.phnum = phnum; LL.shentsize = shentsize; LL.shnum = shnum;
      LL.shstrndx = shstrndx }
    in
      assert(eh_type = 2); (* Executable, in future may add core dumps *)
      assert(machine = 3); (* i386 *)
      assert(version = 1L);
      assert(eh_flags = 0L);
      assert(ehsize = 16 + 36);
      eh

let build_startup_state fm eh load_base =
  let esp = ref 0xbaaab000L in
    if !opt_trace_setup then
      Printf.printf "Initial ESP is 0x%08Lx\n" !esp;
    fm#set_word_var R_ESP !esp

let load_cb (fm : fragment_machine) fname load_base data_too do_setup extras =
  let ic = open_in (Linux_syscalls.chroot fname) in
  let eh = read_cgcef_header ic in
  let entry_point = ref eh.LL.entry in
  let extra_vaddr = match eh.LL.eh_type with
    | 2 -> 0L (* fixed-location executable *)
    | _ -> failwith "Unhandled CGCEF object type"
  in
    entry_point := eh.LL.entry;
    List.iter
      (fun phr ->
	 if phr.LL.ph_type = 1L then (* PT_LOAD *)
	   (if phr.LL.ph_flags = 5L && extra_vaddr = 0L then
	       (if (phr.LL.vaddr <> load_base) then
		   (Printf.eprintf "Decree_loader::load_cb: phr.vaddr neq load_base\t 0x%Lx <> 0x%Lx"
                                phr.LL.vaddr load_base));
(*               (if (phr.LL.vaddr <> load_base) then
                   (failwith (Printf.sprintf
                                "Decree_loader::load_cb: phr.vaddr neq load_base\t 0x%Lx <> 0x%Lx"
                                phr.LL.vaddr load_base))); *)
	    if data_too || (phr.LL.ph_flags <> 6L && phr.LL.ph_flags <> 7L) then
	      LL.load_segment fm ic phr extra_vaddr true)
	 else if phr.LL.memsz != 0L then
	   LL.load_segment fm ic phr extra_vaddr true;
	 List.iter
	   (fun (base, size) ->
	      if base >= phr.LL.vaddr && 
		base < (Int64.add phr.LL.vaddr phr.LL.memsz)
	      then
		(assert(Int64.add base size < Int64.add phr.LL.vaddr phr.LL.memsz);
		 LL.load_partial_segment fm ic phr base size))
	   extras)
      (LL.read_program_headers ic eh);
    close_in ic;
    if do_setup then
      build_startup_state fm eh load_base;
    !entry_point

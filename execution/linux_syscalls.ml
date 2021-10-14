(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

module V = Vine

open Exec_utils
open Exec_exceptions
open Exec_options
open Fragment_machine

(* This excessively-long file contains most of the machinery FuzzBALL
   uses to emulate the Linux system call interface. Primarily it does
   this by passing the syscalls through to the underlying OS (typically
   itself Linux) via OCaml's Unix module. Some calls that aren't
   supported by OCaml's Unix are faked in various ways, such as returning
   errors, doing nothing, or using external programs. However may
   less-commonly used system calls are not supported at all, and will
   cause FuzzBALL to die (patches welcome).

   Generally speaking, the implementation of a system call named "foo"
   is in a method named "sys_foo". After some utility functions and
   data structures, roughly the first half of the file is
   implementations of sys_* methods, roughly in alphabetical order but
   with some closely-related methods grouped. Then the second half of
   the file is the dispatch routine.
*)

let linux_initial_break = ref None

let linux_setup_tcb_seg (fm : fragment_machine) new_ent new_gdt base limit =
  let store_byte base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_byte_conc addr (Int64.to_int v)
  in
  let new_gs = (new_ent lsl 3) lor 3 in
  let descr = Int64.add new_gdt (Int64.of_int (new_ent lsl 3)) in
    fm#set_word_var R_GDT new_gdt;
    fm#set_short_var R_GS new_gs;
    store_byte descr 0 (Int64.logand limit 0xffL);
    store_byte descr 1 (Int64.logand 
			  (Int64.shift_right limit 8) 0xffL);
    store_byte descr 2 (Int64.logand base 0xffL);
    store_byte descr 3 (Int64.logand
			  (Int64.shift_right base 8) 0xffL);
    store_byte descr 4 (Int64.logand
			  (Int64.shift_right base 16) 0xffL);
    store_byte descr 5 0xf3L; (* pres., ring 3, app, r/w a *)
    store_byte descr 6 (Int64.logor 0xc0L
			  (Int64.shift_right limit 16));
    (* page-gran limit, 32-bit, high nibble of limit *)
    store_byte descr 7 (Int64.logand
			  (Int64.shift_right base 24) 0xffL)

let linux_set_up_arm_kuser_page (fm : fragment_machine) =
    (* See arch/arm/kernel/entry-armv.S in the Linux sources for details *)
    (* __kuser_memory_barrier: *)
    fm#store_word_conc 0xffff0fa0L 0xe12fff1eL; (* bx      lr *)
    fm#store_word_conc 0xffff0fa4L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fa8L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0facL 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fb0L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fb4L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fb8L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fbcL 0xe1a00000L; (* nop *)
    (* __kuser_cmpxchg: *)
    fm#store_word_conc 0xffff0fc0L 0xe5923000L; (* ldr     r3, [r2] *)
    fm#store_word_conc 0xffff0fc4L 0xe0533000L; (* subs    r3, r3, r0 *)
    fm#store_word_conc 0xffff0fc8L 0x05821000L; (* streq   r1, [r2] *)
    fm#store_word_conc 0xffff0fccL 0xe2730000L; (* rsbs    r0, r3, #0 *)
    fm#store_word_conc 0xffff0fd0L 0xe12fff1eL; (* bx      lr *)
    fm#store_word_conc 0xffff0fd4L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fd8L 0xe1a00000L; (* nop *)
    fm#store_word_conc 0xffff0fdcL 0xe1a00000L; (* nop *)
    (* __kuser_get_tls: *)
    fm#store_word_conc 0xffff0fe0L 0xe59f0008L; (* ldr  r0, [pc, #8] ; 0xff0 *)
    fm#store_word_conc 0xffff0fe4L 0xe12fff1eL; (* bx      lr *)
    fm#store_word_conc 0xffff0fe8L 0x0L; (* pad *)
    fm#store_word_conc 0xffff0fecL 0x0L; (* pad *)
    fm#store_word_conc 0xffff0ff0L 0x0L; (* __kuser_get_tls TLS pointer *)
    fm#store_word_conc 0xffff0ff4L 0x0L; (* pad *)
    fm#store_word_conc 0xffff0ff8L 0x0L; (* pad *)
    fm#store_word_conc 0xffff0ffcL 3L; (* __kuser_helper_version *)
    ()

let chroot s =
  if String.length s >= 1 && String.sub s 0 1 = "/" then
    (match !opt_chroot_path with
       | Some p -> p ^ s
       | None -> s)
  else
    s

type fd_extra_info = {
  mutable unix_fd : Unix.file_descr option;
  mutable snap_unix_fd : Unix.file_descr option;
  mutable dirp_offset : int;
  mutable readdir_eof: int;
  mutable fname : string;
  mutable snap_pos : int option;
  mutable is_symbolic : bool;
  mutable is_concolic : bool;
  mutable num_read_symbolic : int;
  mutable snap_num_read : int;
}

(* N.b. the argument order here is the opposite from D.assemble64,
   but matches the way the Linux kernel intefaces go *)
let assemble64 high low =
  Int64.logor (Int64.logand 0xffffffffL low) (Int64.shift_left high 32)

class linux_special_handler (fm : fragment_machine) =
  let put_reg = fm#set_word_var in
  let put_return =
    (match !opt_arch with
       | X86 -> put_reg R_EAX
       | X64 -> fm#set_long_var R_RAX
       | ARM -> put_reg R0)
  in
  let load_long addr =
    fm#load_long_concretize addr !opt_measure_influence_syscall_args
      "syscall arg"
  in
  let load_word addr =
    fm#load_word_concretize addr !opt_measure_influence_syscall_args
      "syscall arg"
  in
  let load_short addr =
    fm#load_short_concretize addr !opt_measure_influence_syscall_args
      "syscall arg"
  in
  let load_byte addr =
    fm#load_byte_concretize addr !opt_measure_influence_syscall_args
      "syscall arg"
  in
  let load_byte_or_q addr =
    try
      fm#load_byte_conc addr
    with
      | NotConcrete(_) ->
	  Char.code '?'
  in
  let read_buf addr len =
    if !opt_stop_on_symbolic_syscall_args then
      try
	fm#read_buf addr len (* Works for concrete values only *)
      with
	  NotConcrete(_) -> raise SymbolicSyscall
    else
      let lb =
	if !opt_skip_output_concretize then load_byte_or_q else load_byte
      in
      Array.init len
	(fun i -> Char.chr (lb (Int64.add addr (Int64.of_int i))))
  in
  let lea base i step off =
    Int64.add base (Int64.add (Int64.mul (Int64.of_int i) (Int64.of_int step))
		      (Int64.of_int off)) in
  let store_word base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_word_conc addr v
  in
  let store_short base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_short_conc addr v
  in
  let store_long base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_long_conc addr v
  in
  let zero_region base len =
    assert(len >= 0 && len <= 0x20000000); (* sanity check *)
    let limit = Int64.add base (Int64.of_int len) and
	i = ref base in
      assert(limit >= !i);
      while !i < limit && (Int64.logand !i 0xfffL) <> 0x0L do
	fm#store_byte_conc !i 0;
	i := Int64.succ !i
      done;
      while (Int64.logand !i 0xfffL) = 0L && (Int64.sub limit !i) >= 4096L do
	fm#store_page_conc !i (String.make 4096 '\000');
	i := Int64.add !i 4096L
      done;
      while !i < limit do
	fm#store_byte_conc !i 0;
	i := Int64.succ !i
      done;
      assert(!i = limit)
  in
  let string_of_char_array ca =
    let s = String.create (Array.length ca) in
      for i = 0 to (Array.length ca) - 1 do
	s.[i] <- ca.(i)
      done;
      s
  in
  let compare_fds fd1 fd2 error name =
    if fd1 = fd2 then (
      raise 
        (Unix.Unix_error
          (error, 
           name, "")));
  in
object(self)
  val fd_info =
    let a = Array.init 1024
              (fun _ -> { unix_fd = None;
			  snap_unix_fd = None;
                          dirp_offset = 0;
                          readdir_eof = 0;
                          fname = "";
                          snap_pos = None;
                          is_symbolic = false;
                          is_concolic = false;
                          num_read_symbolic = 0;
                          snap_num_read = 0})
    in
      a.(0).unix_fd <- (Some Unix.stdin);
      a.(1).unix_fd <- (Some Unix.stdout);
      a.(2).unix_fd <- (Some Unix.stderr);
      a

  method private fresh_fd () =
    let rec loop i =
      if i >= Array.length fd_info then
	raise (Unix.Unix_error(Unix.EMFILE, "Out of (virtual) file handles",
			       ""))
      else
	match fd_info.(i).unix_fd with
	  | None -> i
	  | Some _ -> loop (i + 1)
    in loop 0

  method private get_fd vt_fd =
    if vt_fd < 0 || vt_fd >= (Array.length fd_info) then
      raise (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))
    else
      match fd_info.(vt_fd).unix_fd with
	| Some fd -> fd
	| None -> raise
	    (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))

  method private clear_fd vt_fd =
    fd_info.(vt_fd).unix_fd <- None;
    fd_info.(vt_fd).dirp_offset <- 0;
    fd_info.(vt_fd).readdir_eof <- 0;
    fd_info.(vt_fd).fname <- "";
    fd_info.(vt_fd).is_symbolic <- false;
    fd_info.(vt_fd).is_concolic <- false;
    fd_info.(vt_fd).num_read_symbolic <- 0;

  method private new_fd vt_fd unix_fd =
    self#clear_fd vt_fd;
    fd_info.(vt_fd).unix_fd <- Some unix_fd

  method private copy_fd new_vt_fd new_unix_fd old_vt_fd =
    let old = fd_info.(old_vt_fd) in
    fd_info.(new_vt_fd).unix_fd <- Some new_unix_fd;
    fd_info.(new_vt_fd).dirp_offset <- old.dirp_offset;
    fd_info.(new_vt_fd).readdir_eof <- old.readdir_eof;
    fd_info.(new_vt_fd).fname <- old.fname;
    fd_info.(new_vt_fd).is_symbolic <- old.is_symbolic;
    fd_info.(new_vt_fd).is_concolic <- old.is_concolic;
    fd_info.(new_vt_fd).num_read_symbolic <- old.num_read_symbolic;

  val netlink_sim_sockfd = ref 1025
  val netlink_sim_seq = ref 0L
  val netlink_cnt = ref 0

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
    put_return (Int64.of_int ~-(self#errno err))

  val mutable next_fresh_addr = 0x50000000L

  method fresh_addr size = 
    let ret = next_fresh_addr in
      next_fresh_addr <- Int64.add next_fresh_addr size;
      next_fresh_addr <- Int64.logand 0xffff_ffff_ffff_f000L
	(Int64.add next_fresh_addr 0x0fffL); (* page align *)
      ret

  val the_break = ref None

  val mutable saved_next_fresh_addr = 0L
  val mutable saved_the_break = None

  method private save_memory_state =
    saved_next_fresh_addr <- next_fresh_addr;
    saved_the_break <- !the_break

  method private reset_memory_state =
    next_fresh_addr <- saved_next_fresh_addr;
    the_break := saved_the_break

  method string_create len =
    try String.create len
    with Invalid_argument(_ (* "String.create" *) )
	-> raise (Unix.Unix_error(Unix.EFAULT, "String.create", ""))

  (* Right now we always redirect the program's FDs 1 and 2 (stdout
     and stderr) to FuzzBALL's stdout. We might want to consider doing
     this more selectively (e.g., only if they're still pointing the same
     place as they did when the program started), or controlled by a
     command-line flag. *)
  method do_write fd bytes count =
    let str = string_of_char_array bytes in
    let strstr haystack needle =
      let needle_len = String.length needle in
      let found = ref false in
	for i = 0 to (String.length haystack) - needle_len do
	  if (String.sub haystack i needle_len) = needle then
	    found := true
	done;
	!found
    in
    (try
       (match !opt_prefix_out, fd with
	  | (Some prefix, (1|2)) ->
	      Printf.printf "[%s fd %d]: " prefix fd
	  | _ -> ());
       (match fd with
	  | (1|2) ->
	      Array.iter print_char bytes;
	      flush stdout;
	      put_return (Int64.of_int count)
	  | _ ->
	      let ufd = self#get_fd fd
	      in
		match Unix.write ufd str 0 count
		with
		  | i when i = count -> put_return (Int64.of_int count)
		  | _ -> raise (Unix.Unix_error(Unix.EINTR, "", "")));
       (match !opt_disqualify_on_message with
	  | Some msg ->
	      if strstr str msg then
		(if not (strstr str "\n") then print_char '\n';
		 raise DisqualifiedPath)
	  | _ -> ())
     with
       | Unix.Unix_error(err, _, _) -> self#put_errno err);
    ()

  method do_unix_read fd addr count =
    let rec loop left a =
      if (left <= 0) then 0 else
	let chunk = if (left < 4096) then left else 4096 in
	let str = self#string_create chunk in
	let num_read = Unix.read fd str 0 chunk in
	  if num_read = 4096 && (Int64.logand a 0xfffL) = 0L then
	    fm#store_page_conc a str
	  else
	    fm#store_str a 0L (String.sub str 0 num_read);
	  num_read +
	    (loop (left - chunk) (Int64.add a (Int64.of_int chunk)))
    in
      loop count addr
	
  method read_sockaddr addr addrlen =
    let family = load_short addr and
	buf = Int64.add 2L addr and
	len = addrlen - 2 in
      match family with
	| 1 -> let path = fm#read_cstr buf in
	    if path = "" then
	      (* I don't think OCaml can handle the Linux
		 "abstract namespace" extension. But sometimes (e.g.,
		 as used by the X libraries) the abstract name is
		 the same as a pathname that will also work, so try that.
	      *)
	      let path' = fm#read_cstr (Int64.add buf 1L)
	      in
		Unix.ADDR_UNIX(chroot path')
	    else
	      Unix.ADDR_UNIX(chroot path)
	| 2 -> 
	    assert(len = 6 || len = 14);
	    let port_be = load_short buf and
		addr_h  = load_byte (lea buf 0 0 2) and
		addr_mh = load_byte (lea buf 0 0 3) and
		addr_ml = load_byte (lea buf 0 0 4) and
		addr_l  = load_byte (lea buf 0 0 5) in
	    let port = ((port_be land 0xff) lsl 8) lor (port_be lsr 8) and
		addr = (Unix.inet_addr_of_string
			  (Printf.sprintf "%d.%d.%d.%d" addr_h addr_mh
			     addr_ml addr_l))
	    in
	      Unix.ADDR_INET(addr, port)
	| _ -> failwith "Unexpected sockaddr family"

  method write_sockaddr sockaddr_oc addr addrlen_ptr =
    let dotted_addr_to_dat str =
      let dot1 = String.index str '.' in
      let dot2 = String.index_from str (dot1 + 1) '.' in
      let dot3 = String.index_from str (dot2 + 1) '.' in
      let b1 = int_of_string (String.sub str 0 dot1) and
	  b2 = int_of_string (String.sub str (dot1 + 1) (dot2 - dot1 - 1)) and
	  b3 = int_of_string (String.sub str (dot2 + 1) (dot3 - dot2 - 1)) and
	  b4 = int_of_string (String.sub str (dot3 + 1)
				((String.length str) - dot3 - 1))
      in
	Printf.sprintf "%c%c%c%c" (Char.chr b1) (Char.chr b2)
	  (Char.chr b3) (Char.chr b4)
    in
    let (family, data) = 
      match sockaddr_oc with
	| Unix.ADDR_UNIX(path) ->
	    ("\001\000", path)
	| Unix.ADDR_INET(addr, port) ->
	    let port_dat = Printf.sprintf "%c%c"
	      (Char.chr (port lsr 8)) (Char.chr (port land 0xff)) and
		addr_str = Unix.string_of_inet_addr addr in
	    let addr_dat = dotted_addr_to_dat addr_str in
	      ("\002\000", port_dat ^ addr_dat)
    in
    let sa_data = family ^ data in
    let real_len = String.length sa_data in
    let buf_len = Int64.to_int (load_word addrlen_ptr) in
    let write_len = min real_len buf_len in
      fm#store_str addr 0L (String.sub sa_data 0 write_len);
      store_word addrlen_ptr 0 (Int64.of_int real_len);

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
      (if (flags land 0o4000) <> 0  then [Unix.O_NONBLOCK] else []) @
      (if (flags land 0o2000) <> 0  then [Unix.O_APPEND]   else []) @
      (if (flags land 0o100) <> 0   then [Unix.O_CREAT]    else []) @
      (if (flags land 0o1000) <> 0  then [Unix.O_TRUNC]    else []) @
      (if (flags land 0o200) <> 0   then [Unix.O_EXCL]     else []) @
      (if (flags land 0o10000) <> 0 then [Unix.O_SYNC]     else [])

  method private write_oc_statbuf_as_stat addr oc_buf =
    let dev = Int64.of_int oc_buf.Unix.st_dev and
	ino = Int64.of_int oc_buf.Unix.st_ino and
	mode = (oc_buf.Unix.st_perm lor 
		  (self#oc_kind_to_mode oc_buf.Unix.st_kind)) and
	nlink = oc_buf.Unix.st_nlink and
	uid = oc_buf.Unix.st_uid and
	gid = oc_buf.Unix.st_gid and
	rdev = Int64.of_int oc_buf.Unix.st_rdev and
	size = Int64.of_int oc_buf.Unix.st_size and
	atime = Int64.of_float oc_buf.Unix.st_atime and
	mtime = Int64.of_float oc_buf.Unix.st_mtime and
	ctime = Int64.of_float oc_buf.Unix.st_ctime and
	blksize = 4096L and
	blocks = Int64.of_int (oc_buf.Unix.st_size/4096)
    in
      store_word  addr  0 dev;
      store_word  addr  4 ino;     (* 32-bit inode *)
      store_short addr  8 mode;
      store_short addr 10 nlink;
      store_short addr 12 uid;
      store_short addr 14 gid;
      store_word  addr 16 rdev;
      store_word  addr 20 size; 
      store_word  addr 24 blksize;
      store_word  addr 28 blocks;
      store_word  addr 32 atime;
      store_word  addr 36 0L;      (* atime nanosecs *)
      store_word  addr 40 mtime;
      store_word  addr 44 0L;      (* mtime naonsecs *)
      store_word  addr 48 ctime;
      store_word  addr 52 0L;      (* ctime nanosecs *)

  method private write_oc_statbuf_as_x86_stat64 addr oc_buf =
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
      store_word addr 60 0L;      (* high bits of blocks *)
      store_word addr 64 atime;
      store_word addr 68 0L;      (* atime nanosecs *)
      store_word addr 72 mtime;
      store_word addr 76 0L;      (* mtime nanosecs *)
      store_word addr 80 ctime;
      store_word addr 84 0L;      (* ctime nanosecs *)
      store_word addr 88 ino;     (* low bits of 64-bit inode *)
      store_word addr 92 0L;      (* high bits of 64-bit inode *)

  method private write_oc_statbuf_as_x64_stat addr oc_buf =
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
      store_long addr   0 dev;
      store_long addr   8 ino;
      store_long addr  16 nlink;
      store_word addr  24 mode;
      store_word addr  28 uid;
      store_word addr  32 gid;
      (* 4 byte __pad0 *)
      store_long addr  40 rdev;
      store_long addr  48 size;
      store_long addr  56 blksize;
      store_long addr  64 blocks;
      store_long addr  72 atime;
      store_long addr  80 0L; (* atime nanosecs *)
      store_long addr  88 mtime;
      store_long addr  96 0L; (* mtime nanosecs *)
      store_long addr 104 ctime;
      store_long addr 112 0L; (* ctime nanosecs *)
      (* 24 bytes reserved *)

  (* Slightly different layout than the x86 version, because the
     8-byte values are 8- rather than 4-byte aligned *)
  method private write_oc_statbuf_as_arm_stat64 addr oc_buf =
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
      (* 4 bytes padding *)
      store_word addr 12 ino;     (* 32-bit inode *)
      store_word addr 16 mode;
      store_word addr 20 nlink;
      store_word addr 24 uid;
      store_word addr 28 gid;
      store_word addr 32 rdev;
      store_word addr 36 0L;      (* high bits of rdev *)
      (* 4 bytes explicit pad + 4 bytes alignment pad *)
      store_word addr 48 size;
      store_word addr 52 0L;      (* high bits of size *)
      store_word addr 56 blksize;
      store_word addr 64 blocks;
      store_word addr 68 0L;      (* high bits of blocks *)
      store_word addr 72 atime;
      store_word addr 76 0L;      (* atime nanosecs *)
      store_word addr 80 mtime;
      store_word addr 84 0L;      (* mtime naonsecs *)
      store_word addr 88 ctime;
      store_word addr 92 0L;      (* ctime nanosecs *)
      store_word addr 96 ino;     (* low bits of 64-bit inode *)
      store_word addr 100 0L;      (* high bits of 64-bit inode *)

  method private write_oc_statbuf_as_stat64 addr oc_buf =
    match !opt_arch with
      | X86 -> self#write_oc_statbuf_as_x86_stat64 addr oc_buf
      | X64 -> failwith "64-bit syscalls not supported"
      | ARM -> self#write_oc_statbuf_as_arm_stat64 addr oc_buf

  method private write_fake_statfs_buf addr =
    (* OCaml's Unix module doesn't provide an interface for this
       information, so we just make it up. *)
    let f_type   = 0x52654973L (* REISERFS_SUPER_MAGIC *) and
	f_bsize  = 4096L and
	f_blocks = 244182546L and
	f_bfree  = 173460244L and
	f_bavail = 173460244L and
	f_files  = 0L and
	f_ffree  = 0L and
	f_fsid_0 = 0L and
	f_fsid_1 = 0L and
	f_namelen = 255L and
	f_frsize = 4096L
    in
      store_word addr  0 f_type;
      store_word addr  4 f_bsize;
      store_word addr  8 f_blocks;
      store_word addr 12 f_bfree;
      store_word addr 16 f_bavail;
      store_word addr 20 f_files;
      store_word addr 24 f_ffree;
      store_word addr 28 f_fsid_0;
      store_word addr 32 f_fsid_1;
      store_word addr 36 f_namelen;
      store_word addr 40 f_frsize

  method private write_fake_statfs_x64_buf addr =
    (* OCaml's Unix module doesn't provide an interface for this
       information, so we just make it up. *)
    let f_type   = 0x52654973L (* REISERFS_SUPER_MAGIC *) and
	f_bsize  = 4096L and
	f_blocks = 244182546L and
	f_bfree  = 173460244L and
	f_bavail = 173460244L and
	f_files  = 0L and
	f_ffree  = 0L and
	f_fsid_0 = 0L and
	f_fsid_1 = 0L and
	f_namelen = 255L and
	f_frsize = 4096L and
	f_flags = 0L
    in
      store_long addr  0 f_type;
      store_long addr  8 f_bsize;
      store_long addr 16 f_blocks;
      store_long addr 24 f_bfree;
      store_long addr 32 f_bavail;
      store_long addr 40 f_files;
      store_long addr 48 f_ffree;
      store_long addr 56 f_fsid_0;
      store_long addr 64 f_fsid_1;
      store_long addr 72 f_namelen;
      store_long addr 80 f_frsize;
      store_long addr 88 f_flags

  method private write_fake_statfs64buf addr =
    (* OCaml's Unix module doesn't provide an interface for this
       information, so we just make it up. *)
    let f_type   = 0x52654973L (* REISERFS_SUPER_MAGIC *) and
	f_bsize  = 4096L and
	f_blocks = 244182546L and
	f_bfree  = 173460244L and
	f_bavail = 173460244L and
	f_files  = 0L and
	f_ffree  = 0L and
	f_fsid_0 = 0L and
	f_fsid_1 = 0L and
	f_namelen = 255L and
	f_frsize = 4096L
    in
      store_word addr  0 f_type;
      store_word addr  4 f_bsize;
      store_word addr  8 f_blocks;
      store_word addr 12 0L;     (* high word of f_blocks *)
      store_word addr 16 f_bfree;
      store_word addr 20 0L;     (* high word of f_bfree *)
      store_word addr 24 f_bavail;
      store_word addr 28 0L;     (* high word of f_bavail *)
      store_word addr 32 f_files;
      store_word addr 36 0L;     (* high word of f_files *)
      store_word addr 40 f_ffree;
      store_word addr 44 0L;     (* high word of f_ffree *)
      store_word addr 48 f_fsid_0;
      store_word addr 52 f_fsid_1;
      store_word addr 56 f_namelen;
      store_word addr 60 f_frsize
      (* offsets 64, 68, 72, 76, 80: f_spare[5] reserved *)

  (* This works for either a struct timeval or a struct timespec,
     depending on the resolution *)
  method private write_ftime_as_words ftime addr resolution =
    let fsecs = floor ftime in
    let secs = Int64.of_float fsecs and
	fraction = Int64.of_float (resolution *. (ftime -. fsecs)) in
      match !opt_arch with
	| (X86|ARM) ->
	    store_word addr 0 secs;
	    store_word addr 4 fraction
	| X64 ->
	    store_long addr 0 secs;
	    store_long addr 8 fraction

  method private read_words_as_ftime addr resolution =
    let (secs, frac) = match !opt_arch with
      | (X86|ARM) ->
	  (load_word (lea addr 0 0 0)), (load_word (lea addr 0 0 4))
      | X64 ->
	  (load_word (lea addr 0 0 0)), (load_long (lea addr 0 0 8))
    in
      (Int64.to_float secs) +. (Int64.to_float frac) /. resolution

  val mutable proc_identities = None

  method set_proc_identities id =
    proc_identities <- id

  method get_pid =
    match proc_identities with
      | Some (pid, _, _, _) -> pid
      | None -> Unix.getpid ()

  method get_ppid =
    match proc_identities with
      | Some (_, ppid, _, _) -> ppid
      | None -> Unix.getppid ()

  method get_pgrp =
    match proc_identities with
      | Some (_, _, pgrp, _) -> pgrp
      | None ->
	  (* Was: failwith "OCaml has no getpgrp()". This approximation
	     may work for at least some programs. *)
	  Unix.getpid ();

  method get_sid =
    match proc_identities with
      | Some (_, _, _, sid) -> sid
      | None -> failwith "OCaml has no getsid()"

  val symbolic_fnames = Hashtbl.create 11

  method add_symbolic_file s is_concolic =
    Hashtbl.replace symbolic_fnames s is_concolic

  method add_symbolic_fd fd is_concolic =
    fd_info.(fd).is_symbolic <- true;
    fd_info.(fd).is_concolic <- is_concolic;
    (* Set up a global variable so that the symbolic variables
       produced for input from this FD can be parsed by
       -trace-assigns-string. For this to work, this code has to match
       the way the variables are created later in fill_read_buf. *)
    let byte_suf = if is_concolic then "" else "byte_" in
    match (!input_string_mem_prefix, fd) with
    | (None, 0) -> input_string_mem_prefix := Some ("stdin_" ^ byte_suf)
    | (None, _) -> input_string_mem_prefix := Some ("file_" ^ byte_suf)
    | (Some _, _) -> ()

  method private save_sym_fd_positions = 
    Array.iteri
      (fun fd info ->
        if info.is_symbolic then
	  ((try
	      info.snap_pos <-
	        Some (Unix.lseek (self#get_fd fd) 0 Unix.SEEK_CUR)
	    with Unix.Unix_error(Unix.ESPIPE, "lseek", "") -> ());
           info.snap_num_read <- info.num_read_symbolic)
      ) fd_info

  method private reset_sym_fd_positions = 
    Array.iteri
      (fun fd info ->
        if info.is_symbolic then
          ((match info.snap_pos with
	   | Some pos -> ignore(Unix.lseek (self#get_fd fd) pos Unix.SEEK_SET)
	   | None -> ());
           info.num_read_symbolic <- info.snap_num_read)
      ) fd_info

  method private save_fds =
    Array.iteri
      (fun fd info ->
	 info.snap_unix_fd <- info.unix_fd
      ) fd_info

  method private reset_fds =
    Array.iteri
      (fun fd info ->
	 (if info.snap_unix_fd <> info.unix_fd then
	   match info.unix_fd with
	     | Some oc_fd -> Unix.close oc_fd
	     | None -> ());
	 info.unix_fd <- info.snap_unix_fd
      ) fd_info

  method make_snap = 
    self#save_sym_fd_positions;
    self#save_fds;
    self#save_memory_state

  method reset = 
    self#reset_sym_fd_positions;
    self#reset_fds;
    self#reset_memory_state

  method sys_access path mode =
    let oc_mode =
      (if   (mode land 0x7)= 0 then [Unix.F_OK] else []) @
	(if (mode land 0x1)<>0 then [Unix.X_OK] else []) @
	(if (mode land 0x2)<>0 then [Unix.W_OK] else []) @
	(if (mode land 0x4)<>0 then [Unix.R_OK] else [])
    in
      try
	Unix.access (chroot path) oc_mode;
	put_return 0L
      with
	| Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_bind sockfd addr addrlen =
    try
      if sockfd <> !netlink_sim_sockfd then (
        Unix.bind (self#get_fd sockfd) (self#read_sockaddr addr addrlen));
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_accept sockfd addr addrlen_ptr =
    try
      compare_fds !netlink_sim_sockfd sockfd 
        Unix.EOPNOTSUPP "Unsupported accept(2) on netlink socket fd";
      let (sockfd_oc,socka_oc) = Unix.accept (self#get_fd sockfd) and
          vt_fd = self#fresh_fd () in
      self#write_sockaddr socka_oc addr addrlen_ptr;
      self#new_fd vt_fd sockfd_oc;
      put_return (Int64.of_int vt_fd)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_brk addr =
    let cur_break = match !the_break with
      | Some b -> b
      | None ->
	  (let first_break = 
	     (match !linux_initial_break with
		| Some b -> b
		| None -> 0x08200000L (* vague guess *) )
	   in
	     the_break := Some first_break;
	     first_break)
    in
    let new_break = 
      if addr < cur_break then
	cur_break
      else 
	let size = Int64.sub addr cur_break in
	  if size < 1073741824L then
	    (fm#zero_fill cur_break (Int64.to_int size);
	     addr)
	  else
	    (* too big, refuse *)
	    cur_break
    in
      the_break := Some new_break;
      put_return new_break;

  method sys_capget hdrp datap =
    ignore(hdrp);
    ignore(datap);
    self#put_errno Unix.EFAULT

  method sys_chdir path =
    try
      Unix.chdir path;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fchdir fd =
    try
      let dirname = fd_info.(fd).fname in
	Unix.chdir dirname;
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_chmod path mode =
    try
      Unix.chmod (chroot path) mode;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fchmod fd mode =
    Unix.fchmod (self#get_fd fd) mode;
    put_return 0L (* success *)

  method sys_chown path user group =
    try
      Unix.chown path user group;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fchown32 fd user group =
    try
      Unix.fchown (self#get_fd fd) user group;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_clock_getres clkid timep =
    match clkid with
      | 1 -> (* CLOCK_MONOTONIC *)
	  store_word timep 0 0L;
	  store_word timep 4 1L; (* 1-nanosecond precision *)
	  put_return 0L
      | 0 -> (* CLOCK_REALTIME *)
	  store_word timep 0 0L;
	  store_word timep 4 1L; (* 1-nanosecond precision; is this right? *)
	  put_return 0L
      | _ -> self#put_errno Unix.EINVAL (* unsupported clock type *)

  method sys_clock_gettime clkid timep =
    match clkid with
      | 1 -> (* CLOCK_MONOTONIC *)
	  (* For Linux, this is pretty much time since boot, unless you're
	     changing the system's clock while running the program.
	     Pretend we were booted on 2010-03-18. *)
	  (self#write_ftime_as_words (Unix.gettimeofday () -. 1268959142.0)
	     timep 1e9);
	  put_return 0L
      | 0 -> (* CLOCK_REALTIME *)
	  self#write_ftime_as_words (Unix.gettimeofday ()) timep 1e9;
	  put_return 0L
      | _ -> self#put_errno Unix.EINVAL (* unsupported clock type *)

  method sys_close fd =
    try
      let oc_fd = self#get_fd fd in
      if (fd <> 1 && fd <> 2) then (
        if fd = !netlink_sim_sockfd then
          netlink_sim_sockfd := 1025
        else
          Unix.close oc_fd);
      self#clear_fd fd;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_connect sockfd addr addrlen =
    try
      compare_fds !netlink_sim_sockfd sockfd 
        Unix.ENOSYS "connect(2) on netlink socket fd not implemented";
      Unix.connect (self#get_fd sockfd) (self#read_sockaddr addr addrlen);
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_dup old_vt_fd =
    let old_oc_fd = self#get_fd old_vt_fd in
    let new_oc_fd = Unix.dup old_oc_fd in
    let new_vt_fd = self#fresh_fd () in
      self#copy_fd new_vt_fd new_oc_fd old_vt_fd;
      put_return (Int64.of_int new_vt_fd)

  method sys_dup2 old_vt_fd new_vt_fd =
    let old_oc_fd = self#get_fd old_vt_fd and
        new_oc_fd = self#get_fd new_vt_fd in
    (* We should probably recheck whether this does the right thing in
       the two cases of the new file descriptor number either being in
       use or not. The get_fd might fail if it is not in use, but if
       it is in use it might be best to close it. *)
    Unix.dup2 old_oc_fd new_oc_fd;
    self#copy_fd new_vt_fd new_oc_fd old_vt_fd;
    put_return (Int64.of_int new_vt_fd)

  method sys_eventfd2 initval flags =
    ignore(initval);
    let oc_flags = Unix.O_RDWR ::
      (if (flags land 0o4000) <> 0 then [Unix.O_NONBLOCK] else [])
    in
    let oc_fd = Unix.openfile "/dev/null" oc_flags 0o666 in
    let vt_fd = self#fresh_fd () in
      self#new_fd vt_fd oc_fd;
      fd_info.(vt_fd).fname <- "/dev/fake/eventfd";
      put_return (Int64.of_int vt_fd)

  method sys_exit status =
    raise (SimulatedExit(status))

  method sys_exit_group status =
    raise (SimulatedExit(status))

  method sys_fadvise64_64 fd offset len advice =
    ignore(fd); ignore(offset); ignore(len); ignore(advice);
    put_return 0L (* success *)

  method private fcntl_common fd cmd arg =
    match cmd with
      | 1 (* F_GETFD *) ->
	  ignore(fd);
	  ignore(arg);
	  put_return 1L (* FD_CLOEXEC set *)
      | 2 (* F_SETFD *) ->
	  let real_fd = self#get_fd fd in
	    if (Int64.logand arg 0o2000000L) <> 0L then
	      Unix.set_close_on_exec real_fd
	    else
	      Unix.clear_close_on_exec real_fd;
	  put_return 0L (* success *)
      | 3 (* F_GETFL *) ->
	  ignore(fd);
	  ignore(arg);
	  put_return 2L (* O_RDWR *)
      | 4 (* F_SETFL*) ->
	  let real_fd = self#get_fd fd in
	    if (Int64.logand arg 0o4000L) <> 0L then
	      Unix.set_nonblock real_fd
	    else
	      Unix.clear_nonblock real_fd;
	  put_return 0L (* success *)
      | 6 (* F_SETLK *)
      | 7 (* F_SETLKW *)
      | 13 (* F_SETLK64 *)
      | 14 (* F_SETLKW64 *) ->
	  (* Ignore locks for the moment. OCaml has only lockf, so
	     emulation would be a bit complex. *)
	  ignore(fd);
	  ignore(arg);
	  put_return 0L (* success *)
      | _ -> failwith "Unhandled cmd in fcntl{,64}"

  method sys_fcntl fd cmd arg =
    self#fcntl_common fd cmd arg

  method sys_fcntl64 fd cmd arg =
    self#fcntl_common fd cmd arg

  method sys_futex uaddr op value timebuf uaddr2 val3 =
    let ret = 
      match (op, value) with
	| (129 (* FUTEX_WAKE_PRIVATE *), _) ->
	    0L (* never anyone to wake *)
	| (393 (* FUTEX_WAIT_BITSET_PRIVATE|FUTEX_CLOCK_REALTIME *), _) ->
	    (Int64.of_int ~-(self#errno Unix.EAGAIN))
	      (* simulate lack of setup? *)
    | _ -> raise( UnhandledSysCall("Unhandled futex operation"));
    in
      put_return ret

  method sys_getcwd buf size =
    let s = Unix.getcwd () in
    let len = String.length s in
      if len + 1 > size then
	self#put_errno Unix.ERANGE
      else
	(fm#store_cstr buf 0L s;
	 put_return (Int64.of_int len))

  method sys_getdents fd dirp buf_sz =
    try
      let ulong_len = match !opt_arch with X86|ARM -> 4 | X64 -> 8 in
      let store_ulong = match !opt_arch with
	| X86|ARM -> store_word
	| X64 -> store_long
      in
      let dirname = chroot fd_info.(fd).fname in
      let dirh = Unix.opendir dirname in
      let written = ref 0 in
	for i = 0 to fd_info.(fd).dirp_offset - 1 do
	  ignore(Unix.readdir dirh)
	done;
	try
	  while true do
	    let fname = Unix.readdir dirh in
	    let datalen = 2*ulong_len + 2 + (String.length fname) + 1 in
	    let padding = (~- datalen) land (ulong_len - 1) in
	    let reclen = datalen + padding in
	    let next_pos = !written + reclen in
	      if next_pos >= buf_sz then
		raise End_of_file
	      else
		let oc_st = Unix.stat (dirname ^ "/" ^ fname) in
		let d_ino = oc_st.Unix.st_ino in
		  store_ulong dirp !written (Int64.of_int d_ino);
		  written := !written + ulong_len;
		  (* The meaning of the d_off field varies by file
		     system, and programs are recommended to ignore
		     it. This meaninig of it ("next_pos") was inspired by
		     what the man page says, but upon further
		     investigation does not match what any real Linux
		     filesystems do. *)
		  store_ulong dirp !written (Int64.of_int next_pos);
		  written := !written + ulong_len;
		  fm#store_short_conc (lea dirp 0 0 !written) reclen;
		  written := !written + 2;
		  fm#store_cstr dirp (Int64.of_int !written) fname;
		  written := !written + (String.length fname) + 1;
		  written := !written + padding;
		  assert(!written = next_pos);
		  fd_info.(fd).dirp_offset <- fd_info.(fd).dirp_offset + 1;
	  done;
	with End_of_file -> ();
	  Unix.closedir dirh;
	  put_return (Int64.of_int !written)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_getdents64 fd dirp buf_sz =
    try
      let dirname = chroot fd_info.(fd).fname in
      let dirh = Unix.opendir dirname in
      let written = ref 0 in
	if fd_info.(fd).readdir_eof = 0 then (
          for i = 0 to fd_info.(fd).dirp_offset - 1 do
            ignore(Unix.readdir dirh)
          done;);
	try
	  while true do
            if fd_info.(fd).readdir_eof = 1 then (
              raise End_of_file);
            fd_info.(fd).readdir_eof <- 1;
            let fname = Unix.readdir dirh in
              fd_info.(fd).readdir_eof <- 0;
              let reclen = 19 + (String.length fname) + 1 in
              let next_pos = !written + reclen in
		if next_pos >= buf_sz then
		  raise End_of_file
		else
		  let oc_st = Unix.stat (dirname ^ "/" ^ fname) in
		  let d_ino = oc_st.Unix.st_ino in
		    store_word dirp !written (Int64.of_int d_ino);
		    written := !written + 4;
		    store_word dirp !written 0L; (* high bits of d_ino *)
          written := !written + 4;
          store_word dirp !written (Int64.of_int next_pos);
          written := !written + 4;
          store_word dirp !written 0L; (* high bits of d_off *)
          written := !written + 4;
          fm#store_short_conc (lea dirp 0 0 !written) reclen;
          written := !written + 2;
          fm#store_byte_conc (lea dirp 0 0 !written) 0; (* d_type *)
          written := !written + 1;
          fm#store_cstr dirp (Int64.of_int !written) fname;
          written := !written + (String.length fname) + 1;
          fd_info.(fd).dirp_offset <- fd_info.(fd).dirp_offset + 1;
      done;
    with End_of_file -> ();
      Unix.closedir dirh;
      put_return (Int64.of_int !written)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_getrlimit rsrc buf =
    (match !opt_arch with
      | (X86|ARM) ->
	  store_word buf 0 0xffffffffL; (* infinity *)
	  store_word buf 4 0xffffffffL; (* infinity *)
      | X64 ->
	  store_long buf 0 (-1L); (* infinity *)
	  store_long buf 8 (-1L)  (* infinity *) );
    put_return 0L (* success *)

  method sys_setrlimit resource rlim =
    ignore(resource);
    ignore(rlim);
    put_return 0L (* success *)

  method private sys_prlimit64 pid rsrc new_limit_buf old_limit_buf =
    ignore(pid);
    ignore(rsrc);
    ignore(new_limit_buf);
    if old_limit_buf <> 0L then
      (store_long old_limit_buf 0 (-1L); (* infinity *)
       store_long old_limit_buf 8 (-1L)); (* infinity *)
    put_return 0L (* success *)

  method sys_getgid () = 
    put_return (Int64.of_int (Unix.getgid ()))

  method sys_getgid32 () = 
    put_return (Int64.of_int (Unix.getgid ()))

  method sys_getegid () = 
    put_return (Int64.of_int (Unix.getegid ()))

  method sys_getegid32 () = 
    put_return (Int64.of_int (Unix.getegid ()))

  method sys_getresgid32 rgid_ptr egid_ptr sgid_ptr =
    let rgid = Int64.of_int (Unix.getgid ()) and
	egid = Int64.of_int (Unix.getegid ()) in
    let sgid = egid in
      store_word rgid_ptr 0 rgid;
      store_word egid_ptr 0 egid;
      store_word sgid_ptr 0 sgid;
      put_return 0L (* success *)

  method sys_getgroups32 size list =
    let oc_groups = Unix.getgroups () in
    let len = Array.length oc_groups in
      if size <> 0 && size < len then
	self#put_errno Unix.ERANGE	
      else
	(for i = 0 to len - 1 do
	   store_word (lea list i 4 0) 0 (Int64.of_int (oc_groups.(i)))
	 done;
	 put_return (Int64.of_int len))

  method sys_setgroups32 size list =
    let oc_groups = Array.init size 
      (fun i -> Int64.to_int (load_word (lea list i 4 0))) in
    try
      Unix.setgroups oc_groups;
      put_return 0L;
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_getuid () = 
    put_return (Int64.of_int (Unix.getuid ()))

  method sys_getuid32 () = 
    put_return (Int64.of_int (Unix.getuid ()))

  method sys_geteuid () = 
    put_return (Int64.of_int (Unix.geteuid ()))

  method sys_geteuid32 () = 
    put_return (Int64.of_int (Unix.geteuid ()))

  method sys_getresuid32 ruid_ptr euid_ptr suid_ptr =
    let ruid = Int64.of_int (Unix.getuid ()) and
	euid = Int64.of_int (Unix.geteuid ()) in
    let suid = euid in
      store_word ruid_ptr 0 ruid;
      store_word euid_ptr 0 euid;
      store_word suid_ptr 0 suid;
      put_return 0L (* success *)

  method sys_getpid () =
    let pid = self#get_pid in
      put_return (Int64.of_int pid)

  method sys_getpgid target =
    assert(target = 0 || target = self#get_pid); (* Only works on ourselves *)
    let pgid = self#get_pgrp in
      put_return (Int64.of_int pgid)

  method sys_getpgrp () =
    let pgid = self#get_pgrp in
      put_return (Int64.of_int pgid)

  method sys_getppid () =
    let ppid = self#get_ppid in
      put_return (Int64.of_int ppid)

  method sys_getsid () =
    let sid = self#get_sid in
      put_return (Int64.of_int sid)

  method sys_gettid =
    (* On Linux, thread id is the process id *)
    let tid = Int64.of_int (self#get_pid) in
      put_return tid

  method private sys_getrandom buf buflen flags =
    ignore(flags); (* no flags implemented *)
    for i = 0 to buflen - 1 do
      fm#store_byte_idx buf i fm#random_byte
    done;
    put_return (Int64.of_int buflen) (* success *)

  method private sys_getrusage32 who buf =
    ignore(who);
    store_word buf  0 0L; (* utime secs *)
    store_word buf  4 0L; (* utime usecs *)
    store_word buf  8 0L; (* stime secs *)
    store_word buf 12 0L; (* stime usecs *)
    store_word buf 16 0L; (* maxrss *)
    store_word buf 20 0L; (* ixrss *)
    store_word buf 24 0L; (* idrss *)
    store_word buf 28 0L; (* isrss *)
    store_word buf 32 0L; (* minflt *)
    store_word buf 36 0L; (* majflt *)
    store_word buf 40 0L; (* nswap *)
    store_word buf 44 0L; (* inblock *)
    store_word buf 48 0L; (* outblock *)
    store_word buf 52 0L; (* msgsnd *)
    store_word buf 56 0L; (* msgrcv *)
    store_word buf 60 0L; (* nsignals *)
    store_word buf 64 0L; (* nvcsw *)
    store_word buf 68 0L; (* nivcsw *)
    put_return 0L (* success *)

  method private sys_getrusage64 who buf =
    ignore(who);
    store_word buf  0  0L; (* utime secs *)
    store_word buf  8  0L; (* utime usecs *)
    store_word buf 16  0L; (* stime secs *)
    store_word buf 24  0L; (* stime usecs *)
    store_word buf 32  0L; (* maxrss *)
    store_word buf 40  0L; (* ixrss *)
    store_word buf 48  0L; (* idrss *)
    store_word buf 56  0L; (* isrss *)
    store_word buf 64  0L; (* minflt *)
    store_word buf 72  0L; (* majflt *)
    store_word buf 80  0L; (* nswap *)
    store_word buf 88  0L; (* inblock *)
    store_word buf 96  0L; (* outblock *)
    store_word buf 104 0L; (* msgsnd *)
    store_word buf 112 0L; (* msgrcv *)
    store_word buf 120 0L; (* nsignals *)
    store_word buf 128 0L; (* nvcsw *)
    store_word buf 136 0L; (* nivcsw *)
    put_return 0L (* success *)

  method sys_getpeername sockfd addr addrlen_ptr =
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.ENOSYS "getpeername(2) on netlink socket fd not implemented";
      let socka_oc = Unix.getpeername (self#get_fd sockfd) in
      self#write_sockaddr socka_oc addr addrlen_ptr;
      let len = ref (Int64.to_int (load_word addrlen_ptr)) in
      if !len < 16 then (* Hack needed to get past getnameinfo(3) *)
        store_word addrlen_ptr 0 16L;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_socketpair dom_i typ_i prot_i addr =
    try
      let domain = match dom_i with
      | 1 -> Unix.PF_UNIX
      | _ ->
        raise (Unix.Unix_error(
                 Unix.EOPNOTSUPP, "Unsupported domain in socketpair(2)",""))
      and
      typ = match typ_i land 0o777 with
      | 1 -> Unix.SOCK_STREAM
      | 2 -> Unix.SOCK_DGRAM
      | 3 -> Unix.SOCK_RAW
      | 5 -> Unix.SOCK_SEQPACKET
      | _ -> raise (Unix.Unix_error(Unix.EINVAL, "Bad socket type", ""))
      in
      let (oc_fd1, oc_fd2) = Unix.socketpair domain typ prot_i in
      let vt_fd1 = self#fresh_fd () in
      self#new_fd vt_fd1 oc_fd1;
      let vt_fd2 = self#fresh_fd () in
      self#new_fd vt_fd2 oc_fd2;
      store_word addr 0 (Int64.of_int vt_fd1);
      store_word addr 4 (Int64.of_int vt_fd2);
      put_return 0L
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_getsockname sockfd addr addrlen_ptr =
    try
      if sockfd <> !netlink_sim_sockfd then (
        let socka_oc = Unix.getsockname (self#get_fd sockfd) in
        self#write_sockaddr socka_oc addr addrlen_ptr;
        let len = ref (Int64.to_int (load_word addrlen_ptr)) in
        if !len < 16 then (* Hack needed to get past getnameinfo(3) *)
          store_word addrlen_ptr 0 16L;)
      else (
        (* Storing nl_pid to be the PID of the userspace process *)
	    store_word addr 4 (Int64.of_int (self#get_pid));
      );
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_gettimeofday timep zonep =
    if timep <> 0L then
      self#write_ftime_as_words (Unix.gettimeofday ()) timep 1e6;
    if zonep <> 0L then
      (* Simulate a modern system where the kernel knows nothing about
	 the timezone: *)
      (store_word zonep 0 0L; (* UTC *)
       store_word zonep 4 0L); (* no DST *)
    put_return 0L

  method sys_symlink target linkpath =
    try
      Unix.symlink target (chroot linkpath);
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_getxattr path name value_ptr size =
    ignore(path); ignore(name); ignore(value_ptr); ignore(size);
    put_return (Int64.of_int (-61)) (* ENODATA *)
 
  method sys_lgetxattr path name value_ptr size =
    ignore(path); ignore(name); ignore(value_ptr); ignore(size);
    put_return (Int64.of_int (-61)) (* ENODATA *)

  method sys_ioctl fd req argp =
    compare_fds !netlink_sim_sockfd fd 
      Unix.EOPNOTSUPP "Unsupported ioctl(2) on netlink socket fd";
    match req with
      | 0x5401L -> (* TCGETS *)
	  let baud_to_flags = function
	    | 0       -> 0o000000
	    | 50      -> 0o000001
	    | 75      -> 0o000002
	    | 110     -> 0o000003
	    | 134     -> 0o000004
	    | 150     -> 0o000005
	    | 200     -> 0o000006
	    | 300     -> 0o000007
	    | 600     -> 0o000010
	    | 1200    -> 0o000011
	    | 1800    -> 0o000012
	    | 2400    -> 0o000013
	    | 4800    -> 0o000014
	    | 9600    -> 0o000015
	    | 19200   -> 0o000016
	    | 38400   -> 0o000017
	    | 57600   -> 0o010001
	    | 115200  -> 0o010002
	    | 230400  -> 0o010003
	    | 460800  -> 0o010004
	    | 500000  -> 0o010005
	    | 576000  -> 0o010006
	    | 921600  -> 0o010007
	    | 1000000 -> 0o010010
	    | 1152000 -> 0o010011
	    | 1500000 -> 0o010012
	    | 2000000 -> 0o010013
	    | 2500000 -> 0o010014
	    | 3000000 -> 0o010015
	    | 3500000 -> 0o010016
	    | 4000000 -> 0o010017
	    | _ -> failwith "Unhandled baud rate in TCGETS"
	  in
	    (try
	       let t = Unix.tcgetattr (self#get_fd fd) in
	       let my_icrnl = true in
	       let my_imaxbel = true in
	       let my_onlcr = true in
	       let my_iexten = true in
	       let my_echoctl = true in
	       let my_echoke = true in
	       let iflag =
	             (if t.Unix.c_ignbrk then 0o000001 else 0)
		 lor (if t.Unix.c_brkint then 0o000002 else 0)
		 lor (if t.Unix.c_ignpar then 0o000004 else 0)
		 lor (if t.Unix.c_parmrk then 0o000010 else 0)
		 lor (if t.Unix.c_inpck  then 0o000020 else 0)
		 lor (if t.Unix.c_istrip then 0o000040 else 0)
		 lor (if t.Unix.c_inlcr  then 0o000100 else 0)
		 lor (if t.Unix.c_igncr  then 0o000200 else 0)
		 lor (if       my_icrnl  then 0o000400 else 0)
		 lor (if t.Unix.c_ixon   then 0o002000 else 0)
		 lor (if t.Unix.c_ixoff  then 0o010000 else 0)
		 lor (if      my_imaxbel then 0o020000 else 0) and
		   oflag =
	             (if t.Unix.c_opost then 0o000001 else 0)
		 lor (if       my_onlcr then 0o000004 else 0) and
		   cflag =
		     (baud_to_flags t.Unix.c_obaud)
		 lor (match t.Unix.c_csize with 
			| 5 -> 0 | 6 -> 0o20 | 7 -> 0o40 | 8 -> 0o60
			| _ -> failwith "Unexpected character size in TCGETS")
		 lor (match t.Unix.c_cstopb with 1 -> 0 | 2 -> 0o100
			| _ -> failwith "Unexpected # of stop bits in TCGETS")
		 lor (if t.Unix.c_cread  then 0o000200 else 0)
		 lor (if t.Unix.c_parenb then 0o000400 else 0)
		 lor (if t.Unix.c_parodd then 0o001000 else 0)
		 lor (if t.Unix.c_hupcl  then 0o002000 else 0)
		 lor (if t.Unix.c_hupcl  then 0o004000 else 0)
		 lor ((baud_to_flags t.Unix.c_obaud) lsl 16) and
		  lflag =
	             (if t.Unix.c_isig   then 0o000001 else 0)
		 lor (if t.Unix.c_icanon then 0o000002 else 0)
		 lor (if t.Unix.c_echo   then 0o000010 else 0)
		 lor (if t.Unix.c_echoe  then 0o000020 else 0)
		 lor (if t.Unix.c_echok  then 0o000040 else 0)
		 lor (if t.Unix.c_echonl then 0o000100 else 0)
		 lor (if t.Unix.c_noflsh then 0o000200 else 0)
		 lor (if      my_echoctl then 0o001000 else 0)
		 lor (if       my_echoke then 0o004000 else 0)
		 lor (if       my_iexten then 0o100000 else 0)
	       in
	       let cc = Array.make 19 '\000' in
		 cc.( 0) <- t.Unix.c_vintr;
		 cc.( 1) <- t.Unix.c_vquit;
		 cc.( 2) <- t.Unix.c_verase;
		 cc.( 3) <- t.Unix.c_vkill;
		 cc.( 4) <- t.Unix.c_veof;
		 cc.( 5) <- Char.chr (t.Unix.c_vtime);
		 cc.( 6) <- Char.chr (t.Unix.c_vmin);
		 cc.( 7) <- '\000'; (* VSWTC: default disabled *)
		 cc.( 8) <- t.Unix.c_vstart;
		 cc.( 9) <- t.Unix.c_vstop;
		 cc.(10) <- '\x1a'; (* VSUSP: default ^Z *)
		 cc.(11) <- t.Unix.c_veol;
		 cc.(12) <- '\x12'; (* VREPRINT: default ^R *)
		 cc.(13) <- '\x0f'; (* VDISCARD ("flush"): default ^O *)
		 cc.(14) <- '\x17'; (* VWERASE: default ^W *)
		 cc.(15) <- '\x16'; (* VLNEXT: default ^V *)
		 cc.(16) <- '\000'; (* VEOL2: default disabled *)
		 cc.(17) <- '\000';
		 cc.(18) <- '\000';
		 store_word argp  0 (Int64.of_int iflag);
		 store_word argp  4 (Int64.of_int oflag);
		 store_word argp  8 (Int64.of_int cflag);
		 store_word argp 12 (Int64.of_int lflag);
		 fm#store_byte_idx argp 16 0; (* c_line *)
		 for i = 0 to 18 do
		   fm#store_byte_idx argp (17 + i) (Char.code cc.(i))
		 done;
		 put_return 0L (* success *)
	     with
	       | Unix.Unix_error(err, _, _) -> self#put_errno err)
      | 0x5402L -> (* TCSETS *)
	  (* Todo: implement in terms of Unix.tcsetattr *)
	  put_return 0L (* success *)
      | 0x540fL -> (* TIOCGPGRP *)
	  store_word argp 0 (Int64.of_int (self#get_pid));
	  put_return 0L (* success *)
      | 0x5413L -> (* TCGWINSZ *)
	  fm#store_short_conc (lea argp 0 0 0) 24; (* ws_row *)
	  fm#store_short_conc (lea argp 0 0 2) 80; (* ws_col *)
	  fm#store_short_conc (lea argp 0 0 4) 0; (* ws_xpixel *)
	  fm#store_short_conc (lea argp 0 0 6) 0; (* ws_ypixel *)
	  put_return 0L (* success *)
      | 0x541bL -> (* FIONREAD *)
	  (* Not sure how to support this in OCaml *)
	  self#put_errno Unix.EINVAL
      | _ -> 	  raise (UnhandledSysCall ("Unhandled ioctl sub-call"))

  method sys_link oldpath newpath =
    try
      Unix.link (chroot oldpath) (chroot newpath);
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_listen sockfd backlog =
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.EOPNOTSUPP "Unsupported listen(2) on netlink socket fd";
      Unix.listen (self#get_fd sockfd) backlog;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_lseek (fd: int) (offset: Int64.t) (whence: int) =
    try
      let seek_cmd = match whence with
	| 0 -> Unix.SEEK_SET
	| 1 -> Unix.SEEK_CUR
	| 2 -> Unix.SEEK_END
	| _ -> raise
	    (Unix.Unix_error(Unix.EINVAL, "Bad whence argument to llseek", ""))
      in
      let loc = Unix.lseek (self#get_fd fd) (Int64.to_int offset) seek_cmd in
	if seek_cmd = Unix.SEEK_SET && offset = 0L then
	  fd_info.(fd).dirp_offset <- 0; (* rewinddir *)
	put_return (Int64.of_int loc);
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys__llseek fd offset resultp whence =
    try
      let seek_cmd = match whence with
	| 0 -> Unix.SEEK_SET
	| 1 -> Unix.SEEK_CUR
	| 2 -> Unix.SEEK_END
	| _ -> raise
	    (Unix.Unix_error(Unix.EINVAL, "Bad whence argument to llseek", ""))
      in
      let loc = Unix.lseek (self#get_fd fd) (Int64.to_int offset) seek_cmd in
	fm#store_long_conc resultp (Int64.of_int loc);
	put_return 0L
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private sys_madvise addr length advice =
    match advice with
      | 14 (* MADV_HUGEPAGE *) ->
	  self#put_errno Unix.EINVAL
      | _ ->
	  put_return 0L (* pretend successful *)

  method sys_mincore addr length vec =
    for i = 0 to (length / 4096) - 1 do
      (* Say everything is present *)
      fm#store_byte_idx addr i 1
    done;
    put_return 0L

  method sys_mkdir path mode =
    try
      Unix.mkdir (chroot path) mode;
      put_return 0L
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err
	  
  method private mmap_common addr length prot flags fd offset =
    compare_fds !netlink_sim_sockfd fd
      Unix.ENOSYS "mmap(2) for netlink socket fd not implemented";
    let offset_i = Int64.to_int offset in
    let do_read addr = 
      let len = Int64.to_int length in
      let fd_oc = self#get_fd fd in
      let old_loc = Unix.lseek fd_oc 0 Unix.SEEK_CUR in
	ignore(Unix.lseek fd_oc offset_i Unix.SEEK_SET);
	ignore(self#do_unix_read fd_oc addr len);
	ignore(Unix.lseek fd_oc old_loc Unix.SEEK_SET);
	(* assert(nr = len); *)
	addr
    in
    let ret =
      match (addr, length, prot, flags, fd) with
	| (_, length, _, _, _) when
	    length < 0L || length > 1073741824L ->
	    raise (Unix.Unix_error(Unix.ENOMEM, "Too large in mmap", ""))
	| (0L, _, 0x3 (* PROT_READ|PROT_WRITE *),
	   (0x22|0x20022) (* MAP_PRIVATE|MAP_ANONYMOUS, opt.MAP_STACK *), -1) ->
	    let fresh = self#fresh_addr length in
	      zero_region fresh (Int64.to_int length);
	      fresh
	| (0L, _, 0x0 (* PROT_NONE *),
	   0x4022 (* MAP_NORESERVE|MAP_PRIVATE|MAP_ANONYMOUS *),
	   -1) ->
	    let fresh = self#fresh_addr length in
	      zero_region fresh (Int64.to_int length);
	      fresh	    
	| (_, _, _,
	   (0x832|0x32) (* MAP_DENYWRITE|PRIVATE|FIXED|ANONYMOUS *), -1) ->
	    zero_region addr (Int64.to_int length);
	    addr
	| (_, _, (0x0) (* PROT_NONE *),
	   0x4032 (* MAP_NORESERVE|PRIVATE|FIXED|ANONYMOUS *), -1) ->
	    addr
	| (0L, _, 
	   (0x1|0x5) (* PROT_READ|PROT_EXEC *),
	   (0x802|0x2|0x1) (* MAP_PRIVATE|MAP_DENYWRITE|MAP_SHARED *), _) ->
	    let dest_addr = self#fresh_addr length in
	      do_read dest_addr
	| (_, _,
	   (0x1|0x5) (* PROT_READ|PROT_EXEC *),
	   (0x802|0x2|0x1) (* MAP_PRIVATE|MAP_DENYWRITE|MAP_SHARED *), _) ->
	    do_read addr
	| (_, _, (0x1|0x3|0x5) (* R, RW, RX *),
	   0x12 (* MAP_PRIVATE|FIXED *), _) ->
	    do_read addr
	| (_, _, (0x1|0x3|0x5|0x7) (* R RW RX RWX *),
	   0x812 (* MAP_DENYWRITE|PRIVATE|FIXED *), _) ->
	    do_read addr
	| _ -> failwith "Unhandled mmap operation"
    in
      put_return ret

  method sys_mmap addr length prot flags fd offset =
    try
      self#mmap_common addr length prot flags fd offset
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_mmap2 addr length prot flags fd pgoffset =
    try
      self#mmap_common addr length prot flags fd (Int64.mul 4096L pgoffset)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_mprotect addr len prot =
    (* treat as no-op *)
    put_return 0L;

  method private sys_mremap old_addr old_size new_size flags new_addr =
    (* Unsupported *)
    ignore(old_addr); ignore(old_size); ignore(new_size);
    ignore(flags); ignore(new_size);
    self#put_errno Unix.ENOSYS

  method sys_munmap addr len =
    (* treat as no-op *)
    put_return 0L

  method private open_throw path flags mode =
    let oc_flags = self#flags_to_oc_flags flags in
    let oc_fd = Unix.openfile (chroot path) oc_flags mode and
	  vt_fd = self#fresh_fd () in
        self#new_fd vt_fd oc_fd;
	fd_info.(vt_fd).fname <- path;
	fd_info.(vt_fd).dirp_offset <- 0;
	fd_info.(vt_fd).readdir_eof <- 0;
	(* XXX: canonicalize filename here? *)
	if Hashtbl.mem symbolic_fnames path then
          (fd_info.(vt_fd).is_symbolic <- true;
           fd_info.(vt_fd).is_concolic <- (Hashtbl.find symbolic_fnames path));
	put_return (Int64.of_int vt_fd)

  method sys_open path flags mode =
    try
      self#open_throw path flags mode;
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_openat dirfd path flags mode =
    try
      let dirpath = (if dirfd <> -100 then 
                       ref fd_info.(dirfd).fname 
                     else ref "" ) in
      let dirpathlen = (String.length !dirpath) in
      if dirpathlen > 0 then
        if !dirpath.[dirpathlen - 1]  <> '/' then
          dirpath := !dirpath ^ "/"; 
      let fullpath = !dirpath ^ path in
      self#open_throw fullpath flags mode;
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_pipe buf =
    try
      let (oc_fd1, oc_fd2) = Unix.pipe () in
      let vt_fd1 = self#fresh_fd () in
	self#new_fd vt_fd1 oc_fd1;
	let vt_fd2 = self#fresh_fd () in
	  self#new_fd vt_fd2 oc_fd2;
	  store_word buf 0 (Int64.of_int vt_fd1);
	  store_word buf 4 (Int64.of_int vt_fd2);
	  put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_pipe2 buf flags =
    try
      let (oc_fd1, oc_fd2) = Unix.pipe () in
      let vt_fd1 = self#fresh_fd () in
	self#new_fd vt_fd1 oc_fd1;
	let vt_fd2 = self#fresh_fd () in
	  self#new_fd vt_fd2 oc_fd2;
	  store_word buf 0 (Int64.of_int vt_fd1);
	  store_word buf 4 (Int64.of_int vt_fd2);
	  if (flags land 0o4000 <> 0) then (* O_NONBLOCK *)
	    (Unix.set_nonblock oc_fd1;
	     Unix.set_nonblock oc_fd2);
	  if (flags land 0o2000000 <> 0) then (* O_CLOEXEC *)
	    (Unix.set_close_on_exec oc_fd1;
	     Unix.set_close_on_exec oc_fd2);
	  put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_poll fds_buf nfds timeout_ms =
    let get_pollfd buf idx =
      let fd = load_word (lea buf idx 8 0) and
          events = load_short (lea buf idx 8 4) in
      ((Int64.to_int fd), events)
    in
    let put_pollfd buf idx flags =
      fm#store_short_conc (lea buf idx 8 6) flags
    in
    let filter_event flag pollfds =
      List.map (fun (fd,_) -> (self#get_fd fd))
    (List.filter (fun (_,e) -> e land flag <> 0) pollfds)
    in
    let timeout_f = (Int64.to_float timeout_ms) /. 1000.0 in
    let pollfds_a = Array.init nfds (get_pollfd fds_buf) in
    Array.iter (
      fun (fd,_) ->
        compare_fds !netlink_sim_sockfd fd
          Unix.ENOSYS "poll(2) for netlink socket fd not implemented";
    ) pollfds_a;
    let pollfds = Array.to_list pollfds_a in
    let readfds = filter_event 0x1 (* POLLIN *) pollfds and
        writefds = filter_event 0x4 (* POLLOUT *) pollfds and
        execepfds = filter_event 0x3682 (* XXX others *) pollfds in
    let (r_fds, w_fds, e_fds) =
      Unix.select readfds writefds execepfds timeout_f in
    let count = ref 0 in
    for i = 0 to nfds - 1 do
      let (fd,_) = pollfds_a.(i) in
      let oc_fd = self#get_fd fd in
      let r_flag = if List.mem oc_fd r_fds then 0x1 else 0 and
          w_flag = if List.mem oc_fd w_fds then 0x4 else 0 in
      let flags = (r_flag lor w_flag) (* XXX other flags? *) in
      if flags <> 0 then
        count := !count + 1;
      put_pollfd fds_buf i flags
    done;
    put_return (Int64.of_int !count)

  (* For read-like system calls, handle filling in the memory buffer
     with the actual data, or symbolic bytes.
   *)
  method private fill_read_buf fd buf num_read str =
    if num_read > 0 && fd_info.(fd).is_symbolic then
      let is_concolic = fd_info.(fd).is_concolic in
      fm#maybe_start_symbolic
        (fun () ->
	  (let name = if fd = 0 then "stdin" else "file" and
               pos = fd_info.(fd).num_read_symbolic
           in
           if is_concolic then
	     fm#store_concolic_name_str buf (String.sub str 0 num_read)
               name pos
	   else
	     fm#make_symbolic_region buf num_read name pos;
           fd_info.(fd).num_read_symbolic <- pos + num_read;
	   max_input_string_length :=
	     max (!max_input_string_length) fd_info.(fd).num_read_symbolic))
    else
      fm#store_str buf 0L (String.sub str 0 num_read);

  method private read_throw fd buf count =
    let str = self#string_create count in
    let oc_fd = self#get_fd fd in
    let num_read = Unix.read oc_fd str 0 count in
    self#fill_read_buf fd buf num_read str;
    put_return (Int64.of_int num_read)

  method sys_read fd buf count =
    try
      self#read_throw fd buf count
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_readv fd iov cnt =
    try
      let num_bytes = self#iovec_size iov cnt in
      let str = self#string_create num_bytes in
      let oc_fd = self#get_fd fd in
      let num_read = Unix.read oc_fd str 0 num_bytes in
	assert(not (fd_info.(fd).is_symbolic)); (* unimplemented *)
	self#scatter_iovec iov cnt str;
	put_return (Int64.of_int num_read)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_pread64 fd buf count off =
    let oc_fd = self#get_fd fd in
      try
	let old_loc = Unix.lseek oc_fd 0 Unix.SEEK_CUR in
	  try
	    ignore(Unix.lseek oc_fd (Int64.to_int off) Unix.SEEK_SET);
	    let err_or = ref None in
	      (try
		 self#read_throw fd buf count
	       with
		 | Unix.Unix_error(err, _, _) -> err_or := Some err);
	      (try
		 ignore(Unix.lseek oc_fd old_loc Unix.SEEK_SET);
	       with
		 | Unix.Unix_error(err, _, _) -> ()
		     (* ignore in favor of read error *) );
	      match !err_or with
		| Some err -> self#put_errno err
		| None -> ()
	  with
	    | Unix.Unix_error(err, _, _) -> self#put_errno err
      with
	| Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_readlink path out_buf buflen =
    try
      let real = Unix.readlink (chroot path) in
      let written = min buflen (String.length real) in
	fm#store_str out_buf 0L (String.sub real 0 written);
	put_return (Int64.of_int written);
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_recv sockfd buf len flags =
    try
      let str = self#string_create len in
      let flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
		  (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else [])
      in
      let num_read =
	Unix.recv (self#get_fd sockfd) str 0 len flags
      in
        self#fill_read_buf sockfd buf num_read str;
	put_return (Int64.of_int num_read) (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_recvfrom sockfd buf len flags addrbuf addrlen_ptr =
    try
      let str = self#string_create len in
      let flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
		  (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else [])
      in
      let (num_read, sockaddr) =
        if addrbuf <> 0L then
	  Unix.recvfrom (self#get_fd sockfd) str 0 len flags
        else
          (* The OCaml recvfrom doesn't deal correctly with not
             getting a source address: when the kernel doesn't write
             address information, it doesn't know what type of address
             to allocate. We can fall back on plain "recv" for this
             case if we know the caller doesn't care. *)
          (Unix.recv (self#get_fd sockfd) str 0 len flags,
           Unix.ADDR_UNIX("unused"))
      in
        self#fill_read_buf sockfd buf num_read str;
        if addrbuf <> 0L then
	  self#write_sockaddr sockaddr addrbuf addrlen_ptr;
	put_return (Int64.of_int num_read) (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_shutdown sockfd how =
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.EOPNOTSUPP "Unsupported shutdown(2) for netlink socket fd";
      let shutdown_cmd = match how with
        | 0 -> Unix.SHUTDOWN_RECEIVE
        | 1 -> Unix.SHUTDOWN_SEND
        | 2 -> Unix.SHUTDOWN_ALL 
        | _ -> raise (Unix.Unix_error(Unix.EINVAL, "Bad value for how", ""))
      in
      Unix.shutdown (self#get_fd sockfd) shutdown_cmd;
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private sys_recvmsg32 sockfd msg flags =
    let store_nlmsg_newaddr buf =
      (* nlmsghdr *)
      store_word buf 0 32L;(* nlmsg_len *)
      store_short buf 4 20;(* nlmsg_type *)
      store_short buf 6 2;(* nlmsg_flags *)
      store_word buf 8 !netlink_sim_seq;(* nlmsg_seq *)
      store_word buf 12 (Int64.of_int (self#get_pid)); (* nlmsg_pid *)
      (* ifaddrmsg *)
      fm#store_byte_idx buf 16 2;(* ifa_family *)
      fm#store_byte_idx buf 17 8;(* ifa_prefixlen *)
      fm#store_byte_idx buf 18 128;(* ifa_flags *)
      fm#store_byte_idx buf 19 254;(* ifa_scopre *)
      store_word buf 20 1L;(* ifa_index *)
      (* rta_attr *)
      store_short buf 24 8;(* rta_len *)
      store_short buf 26 1;(* rta_type *)
      (* address *)
      store_word buf 28 16777343L; (* storing 127.0.0.1 *)
      put_return 32L;
    in
    let store_nlmsg_done buf =
      store_word buf 0 20L;(* nlmsg_len = 20 *)
      store_short buf 4 3;(* nlmsg_type = 3 *)
      store_short buf 6 2;(* nlmsg_flags = 2 *)
      store_word buf 8 !netlink_sim_seq;(* nlmsg_seq *)
      store_word buf 12 (Int64.of_int (self#get_pid)); (* nlmsg_pid *)
      put_return 20L;
    in
    try
      if !netlink_sim_sockfd = sockfd then (
        (* Store PID into nladdr.pid *)
        let nladdr = lea msg 0 0 0 in
	    store_word (load_word nladdr) 4 0L;
	    (* store_word (load_word nladdr) 4 (Int64.of_int (self#get_pid)); *)
        let iov = load_word (lea msg 0 0 8) in
        let buf = load_word iov in
        (match !netlink_cnt with
        | 0 -> store_nlmsg_newaddr buf; incr netlink_cnt;
        | 1 -> store_nlmsg_done buf; netlink_cnt := 0;
        | _ -> 
          failwith 
            "No support for recvmsg(2) more than twice on netlink socket")
      )
      else (
        let iov = load_word (lea msg 0 0 8) and
            cnt = Int64.to_int (load_word (lea msg 0 0 12)) in
        let len = self#iovec_size iov cnt in
        let str = self#string_create len in
        let flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
                    (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else [])
        and addrbuf = load_word (lea msg 0 0 0)
        in
        let (num_read, sockaddr) =
        if addrbuf <> 0L then
	  Unix.recvfrom (self#get_fd sockfd) str 0 len flags
        else
          (Unix.recv (self#get_fd sockfd) str 0 len flags,
           Unix.ADDR_UNIX("unused"))
        in
	assert(not (fd_info.(sockfd).is_symbolic)); (* unimplemented *)
        (if addrbuf <> 0L then
           let addrlen_ptr = load_word (lea msg 0 0 4) in
           self#write_sockaddr sockaddr addrbuf addrlen_ptr);
        self#scatter_iovec iov cnt str;
        put_return (Int64.of_int num_read) (* success *)	)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private sys_recvmsg64 sockfd msg flags =
    try
      let iov = load_long (lea msg 0 0 16) and
          cnt = Int64.to_int (load_long (lea msg 0 0 24)) in
      let len = self#iovec_size iov cnt in
      let str = self#string_create len in
      let flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
        (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else [])
      and addrbuf = load_long (lea msg 0 0 0)
      in
      let (num_read, sockaddr) =
        if addrbuf <> 0L then
	  Unix.recvfrom (self#get_fd sockfd) str 0 len flags
        else
          (Unix.recv (self#get_fd sockfd) str 0 len flags,
           Unix.ADDR_UNIX("unused"))
      in
	assert(not (fd_info.(sockfd).is_symbolic)); (* unimplemented *)
        (if addrbuf <> 0L then
           let addrlen_ptr = load_long (lea msg 0 0 8) in
             self#write_sockaddr sockaddr addrbuf addrlen_ptr);
        self#scatter_iovec iov cnt str;
        put_return (Int64.of_int num_read) (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_rename oldpath newpath =
    try
      Unix.rename (chroot oldpath) (chroot newpath);
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_sched_getparam pid buf =
    ignore(pid); (* Pretend all processes are SCHED_OTHER *)
    store_word buf 0 0L; (* sched_priority = 0 *)
    put_return 0L (* success *)

  method sys_sched_get_priority_max policy =
    assert(policy = 0); (* SCHED_OTHER *)
    put_return 0L

  method sys_sched_get_priority_min policy =
    assert(policy = 0); (* SCHED_OTHER *)
    put_return 0L

  method sys_sched_getscheduler pid =
    ignore(pid);
    put_return 0L (* SCHED_OTHER *)

  method private read_timeval_as_secs addr =
    let secs_f   = (if addr <> 0x0L then Int64.to_float (load_word addr)
                    else -1.0) and
        susecs_f = (if addr <> 0x0L then 
                      Int64.to_float (load_word (lea addr 0 0 4)) 
                    else 0.0) in
    let ret = secs_f +. (susecs_f /. 1000000.0) in
    ret;

  method sys_select nfds readfds writefds exceptfds timeout =
    let read_bitmap addr =
      if addr = 0L then
        []
      else
        let l = ref [] in
        for i = 0 to nfds - 1 do
          let w = load_word (lea addr (i / 32) 4 0) and
              b = i mod 32 in
          if (Int64.logand (Int64.shift_right w b) 1L) = 1L then
          l := i :: !l
        done;
        !l
    in
    let put_sel_fd fd_bm idx fd_w_or =
      fm#store_word_conc (lea fd_bm idx 4 0) fd_w_or;
    in
    let write_bitmap fd_bm fd_l nfds =
        zero_region fd_bm nfds;
        for i = 0 to nfds - 1 do ( 
          if List.mem (self#get_fd i) fd_l then (
            let fd_lsh = Int64.shift_left 1L i in
            let w = load_word (lea fd_bm (i / 32) 4 0) in
            let fd_w_or = Int64.logor fd_lsh w in
            put_sel_fd fd_bm (i / 32) fd_w_or;
            )
          )
        done;
    in
    let rec format_fds l =
      match l with
      | [] -> ""
      | [x] -> string_of_int x
      | x :: rest -> (string_of_int x) ^ ", " ^ (format_fds rest)
    in
    let rl = read_bitmap readfds and
        wl = read_bitmap writefds and
        el = read_bitmap exceptfds in
    if !opt_trace_syscalls then
      Printf.printf "\nselect(%d, [%s], [%s], [%s], 0x%08Lx)"
        nfds (format_fds rl) (format_fds wl) (format_fds el) timeout;
    try
      let map_fd fds =
        List.map (fun (fd) ->  (self#get_fd fd)) fds
      in
      let rl_file_descr = map_fd rl and 
          wl_file_descr = map_fd wl and 
          el_file_descr = map_fd el and
          timeout_f = self#read_timeval_as_secs timeout in
      let (r_fds, w_fds, e_fds) = 
        Unix.select rl_file_descr wl_file_descr el_file_descr timeout_f in
      let r_fds_len = (List.length r_fds) and
          w_fds_len = (List.length w_fds) and 
          e_fds_len = (List.length e_fds) in
      if readfds <> 0L then 
        write_bitmap readfds r_fds nfds;
      if writefds <> 0L then 
        write_bitmap writefds w_fds nfds;
      if exceptfds <> 0L then 
        write_bitmap exceptfds e_fds nfds;
      put_return (Int64.of_int (r_fds_len + w_fds_len + e_fds_len))
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_send sockfd buf len flags =
    try
      let str = string_of_char_array (read_buf buf len) in
      let flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
	          (if (flags land 4) <> 0 then [Unix.MSG_DONTROUTE] else []) @
		  (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else [])
      in
      let num_sent =
	Unix.send (self#get_fd sockfd) str 0 len flags
      in
	put_return (Int64.of_int num_sent) (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_sendto sockfd buf len flags addrbuf addrlen =
    try
      if !netlink_sim_sockfd = sockfd then (
        let netlink_req_type = load_short (lea buf 0 0 4) in
        if netlink_req_type <> 22 then (* RTM_GETADDR *)
          raise 
            (Unix.Unix_error(
              Unix.ENOSYS, "Missing implementation for rtnetlink family", ""));
        netlink_sim_seq := load_word (lea buf 0 0 8);
        put_return (Int64.of_int len))
      else (
        let str = string_of_char_array (read_buf buf len) and
            flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
                    (if (flags land 4) <> 0 then [Unix.MSG_DONTROUTE] else []) @
                    (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else []) and
            sockaddr = self#read_sockaddr addrbuf addrlen
        in
        let num_sent =
          Unix.sendto (self#get_fd sockfd) str 0 len flags sockaddr
        in
        put_return (Int64.of_int num_sent) (* success *))
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private sendmsg_throw sockfd msg flags =
    let str = string_of_char_array
      (self#gather_iovec (load_word (lea msg 0 0 8))
      (Int64.to_int (load_word (lea msg 0 0 12))))
    and flags = (if (flags land 1) <> 0 then [Unix.MSG_OOB] else []) @
                (if (flags land 4) <> 0 then [Unix.MSG_DONTROUTE] else []) @
                (if (flags land 2) <> 0 then [Unix.MSG_PEEK] else [])
    and addrbuf = load_word (lea msg 0 0 0) in
    let num_sent = 
      if addrbuf = 0L then
        Unix.send (self#get_fd sockfd) str 0 (String.length str) flags
      else
        let sockaddr = self#read_sockaddr addrbuf
          (Int64.to_int (load_word (lea msg 0 0 4)))
        in
        Unix.sendto (self#get_fd sockfd) str 0 (String.length str)
          flags sockaddr
    in
    num_sent ;

  method sys_sendmsg sockfd msg flags =
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.ENOSYS "sendmsg(2) for netlink socket fd not implemented";
      let ret = self#sendmsg_throw sockfd msg flags in
      put_return (Int64.of_int ret);
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_sendmmsg sockfd mmsg vlen flags =
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.ENOSYS "sendmmsg(2) for netlink socket fd not implemented";
      let rec sendmmsg sockfd mmsg vlen flags =
        let char_sent = self#sendmsg_throw sockfd mmsg flags in 
        let send_res = (if char_sent < 0 then 0 else 1) in
        let next_send = 
          (if vlen > 1 && send_res = 1 then 
             sendmmsg sockfd (lea mmsg 1 32 0) (vlen-1) flags
           else 0) in 

        fm#store_word_conc (lea mmsg 7 4 0) (Int64.of_int char_sent);
        send_res + next_send;
      in
      put_return (Int64.of_int(sendmmsg sockfd mmsg vlen flags));
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_setgid32 gid =
    Unix.setgid gid;
    put_return 0L (* success *)

  method sys_setreuid new_ruid new_euid =
    try
      (* Most of these cases probably can't be implemented with
	 just OCaml's Unix.setuid. *)
      match (Unix.getuid (), Unix.geteuid(), new_ruid, new_euid) with
	| (u1, u2, 65535, 0) when u1 <> 0 && u1 = u2 ->
	    raise (Unix.Unix_error(Unix.EPERM, "setreuid", ""))
	| (u1, u2, 65535, u4) when u1 <> 0 && u2 = u4 ->
	    put_return 0L (* success *)
	| _ -> failwith "Unhandled case in setreuid"
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_setresuid32 new_ruid new_euid new_suid =
    try
      (* Similar to sys_setreuid *)
      match (Unix.getuid (), Unix.geteuid(), new_ruid, new_euid, new_suid) with
      | (u1, u2, 65535, 0, 65535) when u1 <> 0 && u1 = u2 ->
        raise (Unix.Unix_error(Unix.EPERM, "setresuid", ""))
      | (u1, u2, -1, u4, -1) when u1 <> 0 && u2 = u4 ->
        put_return 0L (* faked for OpenSSH ssh client *)
      | _ -> failwith "Unhandled case in setresuid32"
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_setresgid32 new_ruid new_euid new_suid =
    try
      (* Similar to sys_setreuid *)
      match (Unix.getuid (), Unix.geteuid(), new_ruid, new_euid, new_suid) with
      | (u1, u2, 65535, 0, 65535) when u1 <> 0 && u1 = u2 ->
        raise (Unix.Unix_error(Unix.EPERM, "setresgid32", ""))
      | (u1, u2, -1, u4, -1) when u1 <> 0 && u2 = u4 ->
        put_return 0L (* faked for OpenSSH sshd *)
      | _ -> failwith "Unhandled case in setresgid32"
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_setuid32 uid =
    Unix.setuid uid;
    put_return 0L (* success *)

  method sys_setsockopt sockfd level name valp len =
    let as_bool () =
      let w = load_word valp in
	w <> 0L
    in
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.ENOSYS "setsockopt(2) for netlink socket fd not implemented";
      let bool_option = ref false and
          bool_option_val = ref Unix.SO_DEBUG and
          int_option = ref false and
          int_option_val = ref Unix.SO_SNDBUF and
          float_option = ref false and
          float_option_val = ref Unix.SO_RCVTIMEO and
          option_handled = ref false and
          fd = self#get_fd sockfd in
      (match (level, name) with
	  (* 0 = SOL_IP *)
	  | (0, 6) (* IP_RECVOPTS *) -> () (* No OCaml support *)
      (* 1 = SOL_SOCKET *)
      | (1, 1) ->
        bool_option := true;
        bool_option_val := Unix.SO_DEBUG;
      | (1, 2) ->
        bool_option := true;
        bool_option_val := Unix.SO_REUSEADDR;
      | (1, 3) ->
        int_option := true;
        int_option_val := Unix.SO_TYPE;
      | (1, 5) ->
        bool_option := true;
        bool_option_val := Unix.SO_DONTROUTE;
      | (1, 6) ->
        bool_option := true;
        bool_option_val := Unix.SO_BROADCAST;
      | (1, 7) ->
        int_option := true;
        int_option_val := Unix.SO_SNDBUF;
      | (1, 8) ->
        int_option := true;
        int_option_val := Unix.SO_RCVBUF;
      | (1, 9) ->
        bool_option := true;
        bool_option_val := Unix.SO_KEEPALIVE;
      | (1, 10) ->
        bool_option := true;
        bool_option_val := Unix.SO_OOBINLINE;
      | (1, 11) -> (* SO_NO_CHECK *) () (* No OCaml support *)
      | (1, 12) -> (* SO_PRIORITY *) () (* No OCaml support *)
      | (1, 13) ->
        let lonoff = Int64.to_int (load_word valp) and
            linger = Int64.to_int (load_word (lea valp 1 4 0)) in
        let linger_oc = (if lonoff <> 0 && linger <> 0 then Some linger
                         else None) in
        Unix.setsockopt_optint fd Unix.SO_LINGER linger_oc;
        option_handled := true;
      | (1, 14) -> (* SO_BSDCOMPAT *) () (* No OCaml support *)
      | (1, 15) -> (* SO_REUSEPORT *) () (* No OCaml support *)
      | (1, 16) -> (* SO_PASSCRED *) () (* No OCaml support *)
      | (1, 17) -> (* SO_PEERCRED *) () (* No OCaml support *)
      | (1, 18) ->
        int_option := true;
        int_option_val := Unix.SO_RCVLOWAT;
      | (1, 19) ->
        int_option := true;
        int_option_val := Unix.SO_SNDLOWAT;
      | (1, 20) ->
        float_option := true;
        float_option_val := Unix.SO_RCVTIMEO;
      | (1, 21) ->
        float_option := true;
        float_option_val := Unix.SO_SNDTIMEO;
      | (1, 22) -> (* SO_SECURITY_AUTHENTICATION *) () (* No OCaml support *)
      | (1, 23) -> (* SO_SECURITY_ENCRYPTION_TRANSPORT *) 
          () (* No OCaml support *)
      | (1, 24) -> (* SO_SECURITY_ENCRYPTION_NETWORK *) 
          () (* No OCaml support *)
      | (1, 25) -> (* SO_BINDTODEVICE *) () (* No OCaml support *)
      | (1, 26) -> (* SO_ATTACH_FILTER *) () (* No OCaml support *)
      | (1, 27) -> (* SO_DETACH_FILTER *) () (* No OCaml support *)
      | (1, 28) -> (* SO_PEERNAME *) () (* No OCaml support *)
      | (1, 29) -> (* SO_TIMESTAMP *) () (* No OCaml support *)
      | (1, 30) ->
        bool_option := true;
        bool_option_val := Unix.SO_ACCEPTCONN;
      (* 6 = SOL_TCP *)
	  | (6, 1) -> 
        Unix.setsockopt fd Unix.TCP_NODELAY (as_bool ());
        option_handled := true;
	  | _ -> ()); (* ignore unrecognized options *)
      if !option_handled = false then (
        if !bool_option = true then (
          Unix.setsockopt fd !bool_option_val (as_bool ());
        )
        else if !int_option = true then (
          Unix.setsockopt_int fd !int_option_val 
                              (Int64.to_int (load_word valp));
        )
        else if !float_option = true then (
          let timeval_f = (self#read_timeval_as_secs valp) in
          Unix.setsockopt_float fd !float_option_val timeval_f;
        )
      );
      put_return 0L;
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_getsockopt sockfd level name valp lenp =
    let write_timeval_from_secs addr secs_f =
      let secs   = Int64.of_float secs_f in
      let susecs = Int64.of_float 
                     ((secs_f -. (Int64.to_float secs))*.1000000.0) in
      fm#store_word_conc addr secs;
      fm#store_word_conc (lea addr 1 4 0) susecs;
    in
    try
      compare_fds !netlink_sim_sockfd sockfd
        Unix.ENOSYS "getsockopt(2) for netlink socket fd not implemented";
      let fd = self#get_fd sockfd and
          bool_option = ref false and
          bool_option_val = ref Unix.SO_DEBUG and
          int_option = ref false and
          int_option_val = ref Unix.SO_SNDBUF and
          float_option = ref false and
          float_option_val = ref Unix.SO_RCVTIMEO and
          option_handled = ref false in
      (match (level, name) with
      (* 0 = SOL_IP *)
      | (0, 4) (* IP_OPTIONS *) ->
        (fm#store_word_conc lenp 0L;) (* No OCaml support *)
      | (0, 6) (* IP_RECVOPTS *) -> () (* No OCaml support *)
      (* 1 = SOL_SOCKET *)
      | (1, 1) ->
        bool_option := true;
        bool_option_val := Unix.SO_DEBUG;
      | (1, 2) ->
        bool_option := true;
        bool_option_val := Unix.SO_REUSEADDR;
      | (1, 3) ->
        int_option := true;
        int_option_val := Unix.SO_TYPE;
      | (1, 4) ->
        let error = Unix.getsockopt_error fd in
        (match error with
        | Some err ->
          fm#store_word_conc valp (Int64.of_int ~-(self#errno err));
        | None ->
          fm#store_word_conc valp 0L;
        );
        fm#store_word_conc lenp 4L;
        option_handled := true; 
      | (1, 5) ->
        bool_option := true;
        bool_option_val := Unix.SO_DONTROUTE;
      | (1, 6) ->
        bool_option := true;
        bool_option_val := Unix.SO_BROADCAST;
      | (1, 7) ->
        int_option := true;
        int_option_val := Unix.SO_SNDBUF;
      | (1, 8) ->
        int_option := true;
        int_option_val := Unix.SO_RCVBUF;
      | (1, 9) ->
        bool_option := true;
        bool_option_val := Unix.SO_KEEPALIVE;
      | (1, 10) ->
        bool_option := true;
        bool_option_val := Unix.SO_OOBINLINE;
      | (1, 11) -> (* SO_NO_CHECK *) () (* No OCaml support *)
      | (1, 12) -> (* SO_PRIORITY *) () (* No OCaml support *)
      | (1, 13) ->
        let linger_opt = Unix.getsockopt_optint fd Unix.SO_LINGER in
        (match linger_opt with
        | Some l_onoff ->
          fm#store_word_conc valp 1L;
          fm#store_word_conc (lea valp 1 4 0) (Int64.of_int l_onoff);
          fm#store_word_conc lenp 8L;
        | None -> (* Linger disabled *)
          fm#store_word_conc valp 0L;
          fm#store_word_conc lenp 4L;
        );
        option_handled := true;
      | (1, 14) -> (* SO_BSDCOMPAT *) () (* No OCaml support *)
      | (1, 15) -> (* SO_REUSEPORT *) () (* No OCaml support *)
      | (1, 16) -> (* SO_PASSCRED *) () (* No OCaml support *)
      | (1, 17) -> (* SO_PEERCRED *) () (* No OCaml support *)
      | (1, 18) ->
        int_option := true;
        int_option_val := Unix.SO_RCVLOWAT;
      | (1, 19) ->
        int_option := true;
        int_option_val := Unix.SO_SNDLOWAT;
      | (1, 20) ->
        float_option := true;
        float_option_val := Unix.SO_RCVTIMEO;
      | (1, 21) ->
        float_option := true;
        float_option_val := Unix.SO_SNDTIMEO;
      | (1, 22) -> (* SO_SECURITY_AUTHENTICATION *) () (* No OCaml support *)
      | (1, 23) -> (* SO_SECURITY_ENCRYPTION_TRANSPORT *) 
          () (* No OCaml support *)
      | (1, 24) -> (* SO_SECURITY_ENCRYPTION_NETWORK *) 
          () (* No OCaml support *)
      | (1, 25) -> (* SO_BINDTODEVICE *) () (* No OCaml support *)
      | (1, 26) -> (* SO_ATTACH_FILTER *) () (* No OCaml support *)
      | (1, 27) -> (* SO_DETACH_FILTER *) () (* No OCaml support *)
      | (1, 28) -> (* SO_PEERNAME *) () (* No OCaml support *)
      | (1, 29) -> (* SO_TIMESTAMP *) () (* No OCaml support *)
      | (1, 30) ->
        bool_option := true;
        bool_option_val := Unix.SO_ACCEPTCONN;
      (* 6 = SOL_TCP *)
      | (6, 1) ->
        bool_option := true;
        bool_option_val := Unix.TCP_NODELAY;
      | _ -> ()); (* ignore unrecognized options *)
      if !option_handled = false then
        if !bool_option = true then (
          let value = (if (Unix.getsockopt fd !bool_option_val) = true then 1L
                       else 0L) in
          fm#store_word_conc valp value;
          fm#store_word_conc lenp 4L;
        )
        else if !int_option = true then (
          let value = Unix.getsockopt_int fd !int_option_val in
          fm#store_word_conc valp (Int64.of_int value);
          fm#store_word_conc lenp 4L;
        )
        else if !float_option = true then (
          let secs_f = Unix.getsockopt_float fd !float_option_val in
          write_timeval_from_secs valp secs_f;
          fm#store_word_conc lenp 8L;
        );
      put_return 0L;
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_set_robust_list addr len =
    put_return 0L (* success *)

  method sys_set_thread_area uinfo =
    let old_ent = load_word (lea uinfo 0 0 0)
    and
	base  = load_word (lea uinfo 0 0 4) and
	limit = load_word (lea uinfo 0 0 8) in
      if !opt_trace_syscalls then
	Printf.printf " set_thread_area({entry: %Ld, base: %Lx, limit: %Ld})\n"
	  old_ent base limit;
      (match (old_ent, base, limit) with
	 | (0xffffffffL, _, _) ->
	     let new_ent = 12 in
	       linux_setup_tcb_seg fm new_ent 0x60000000L base limit;
	       store_word uinfo 0 (Int64.of_int new_ent);
	       put_return 0L (* success *)
	 | _ -> failwith "Unhandled args to set_thread_area")

  method sys_arch_prctl code addr =
    (match code with
       | 0x1001 (* ARCH_SET_GS *) ->
	   fm#set_long_var R_GS_BASE addr;
	   put_return 0L
       | 0x1002 (* ARCH_SET_FS *) ->
	   fm#set_long_var R_FS_BASE addr;
	   put_return 0L
       | 0x1003 (* ARCH_GET_FS *) ->
	   store_long addr 0 (fm#get_long_var R_FS_BASE);
	   put_return 0L
       | 0x1004 (* ARCH_GET_GS *) ->
	   store_long addr 0 (fm#get_long_var R_GS_BASE);
	   put_return 0L
       | (0x1011|0x1012) -> (* ARCH_GET/SET_CPUID *)
	   self#put_errno Unix.EINVAL
       | (0x2001|0x2002|0x2003) -> (* ARCH_MAP_VDSO_X32/32/64 *)
	   self#put_errno Unix.EINVAL
       | 0x3001 -> (* ARCH_CET_STATUS, widely unsupported *)
	   self#put_errno Unix.EINVAL
       | _ -> failwith "Unexpected arch_prctl subfunction")

  method sys_set_tid_address addr =
    let pid = self#get_pid in
      put_return (Int64.of_int pid)

  method sys_set_tls tp_value =
    store_word 0xffff0ff0L 0 tp_value;
    put_reg R_TPIDRURO tp_value;
    put_return 0L (* success *)

  method private sys_shmget key size shmflag =
    self#put_errno Unix.ENOSYS

  method sys_rt_sigaction signum newbuf oldbuf setlen =
    try
      (if oldbuf = 0L then () else
	 let (action, mask_low, mask_high, flags) =
	   match signum with
	     | 0 -> raise (Unix.Unix_error(Unix.EINVAL, "rt_sigaction", ""))
	     | 1  (* HUP *)
	     | 2  (* INT *)
	     | 3  (* QUIT *)
	     | 4  (* ILL *)
	     | 5  (* TRAP *)
	     | 6  (* ABRT *)
	     | 7  (* BUS *)
	     | 8  (* FPE *) 
	     | 9  (* KILL *)
	     | 10 (* USR1 *)
             | 11 (* SEGV *)
	     | 12 (* USR2 *)
	     | 13 (* PIPE *)
	     | 14 (* ALRM *)
	     | 15 (* TERM *)
	     | 16 (* STKFLT *)
	     | 17 (* CHLD *)
	     | 18 (* CONT *)
	     | 19 (* STOP *)
	     | 20 (* TSTP *)
	     | 21 (* TTIN *)
	     | 22 (* TTOU *)
	     | 23 (* URG *)
	     | 24 (* XCPU *)
	     | 25 (* XFSZ *)
	     | 26 (* VTALRM *)
	     | 27 (* PROF *)
	     | 28 (* WINCH *)
	     | 29 (* IO *)
	     | 30 (* PWR *)
	     | 31 (* SYS *)
	       -> (0L, 0L, 0L, 0L)
	     | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39
	     | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47
	     | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55
	     | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64
             (* Real-time signals *)
		 -> (0L, 0L, 0L, 0L) 
	     | _ -> raise (UnhandledSysCall("Unhandled old signal in rt_sigaction"));
	 in
	   store_word oldbuf 0 action;
	   store_word oldbuf 4 flags;
	   store_word oldbuf 8 0L; (* restorer *)
	   store_word oldbuf 12 mask_low;
	   store_word oldbuf 12 mask_high);
      put_return 0L; (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_sigaltstack new_stack_t old_stack_t =
    if old_stack_t <> 0L then
      (store_word old_stack_t 0 0L; (* base address *)
       store_word old_stack_t 4 2L; (* flags: SS_DISABLE *)
       store_word old_stack_t 8 0L); (* size *)
    put_return 0L; (* success *)

  method sys_rt_sigprocmask how newset oldset setlen =
    (if oldset = 0L then () else
       ((* report an empty old mask *)
	 match setlen with
	   | 4 -> 
	       store_word oldset 0 0L;
	   | 8 ->
	       store_word oldset 0 0L;
	       store_word oldset 4 0L
	   | _ -> failwith "Unexpected set size in (rt_)sigprocmask"));
    put_return 0L (* success *)

  method sys_nanosleep req_addr rem_addr =
    let req_time = self#read_words_as_ftime req_addr 1e9 in
      (* Unix.sleepf req_time; (* not added until 4.03 *) *)
      ignore(Unix.select [] [] [] req_time);
      self#write_ftime_as_words 0.0 rem_addr 1e9;
      put_return 0L (* success *)

  method sys_socket dom_i typ_i prot_i =
    try
      let netlink_flag = ref false in
      let domain = match dom_i with
      | 1 -> Unix.PF_UNIX
      | 2 -> Unix.PF_INET
      | 10 -> Unix.PF_INET6
      | 16 ->
      (match prot_i with
      (* NETLINK_ROUTE *)
      | 0 -> netlink_flag := true;
        Unix.PF_INET
      | _ -> raise 
               (Unix.Unix_error(
                  Unix.ENOSYS, "Missing implementation for netlink family", "")))
      | _ -> raise (Unix.Unix_error(Unix.EINVAL, "Bad protocol family", ""))
      in
      let typ = match typ_i land 0o777 with
      | 1 -> Unix.SOCK_STREAM
      | 2 -> Unix.SOCK_DGRAM
      | 3 -> Unix.SOCK_RAW
      | 5 -> Unix.SOCK_SEQPACKET
      | _ -> raise (Unix.Unix_error(Unix.EINVAL, "Bad socket type", ""))
      in
      if !netlink_flag = false then (
        let oc_fd = Unix.socket domain typ prot_i and
        vt_fd = self#fresh_fd () in
        self#new_fd vt_fd oc_fd;
        put_return (Int64.of_int vt_fd))
      else (
        let vt_fd = self#fresh_fd () in
        netlink_sim_sockfd := vt_fd;
        (* Plugging in a placeholder Unix file descriptor *)
        self#new_fd vt_fd Unix.stdin;
        put_return (Int64.of_int vt_fd))
    with 
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_stat path buf_addr =
    try
      let oc_buf = Unix.stat (chroot path) in
      (match !opt_arch with
      | X86 -> self#write_oc_statbuf_as_stat buf_addr oc_buf;
      | X64 -> self#write_oc_statbuf_as_x64_stat buf_addr oc_buf
      | ARM -> failwith "Unimplemented: ARM 32-bit stat");
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_lstat path buf_addr =
    try
      let oc_buf = Unix.lstat (chroot path) in
	(match !opt_arch with
	   | X86 -> self#write_oc_statbuf_as_stat buf_addr oc_buf;
	   | X64 -> self#write_oc_statbuf_as_x64_stat buf_addr oc_buf
	   | ARM -> failwith "Unimplemented: ARM 32-bit lstat");
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fstat fd buf_addr =
    try
      let oc_buf =
	(if (fd <> 1 || true) then
	   Unix.fstat (self#get_fd fd)
	 else
	   Unix.stat "/etc/group") (* pretend stdout is always redirected *) in
	(match !opt_arch with
	   | X86 -> self#write_oc_statbuf_as_stat buf_addr oc_buf
	   | X64 -> self#write_oc_statbuf_as_x64_stat buf_addr oc_buf
	   | ARM -> failwith "Unimplemented: ARM 32-bit fstat");
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_stat64 path buf_addr =
    try
      let oc_buf = Unix.stat (chroot path) in
	self#write_oc_statbuf_as_stat64 buf_addr oc_buf;
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_lstat64 path buf_addr =
    try
      let oc_buf = Unix.lstat (chroot path) in
	self#write_oc_statbuf_as_stat64 buf_addr oc_buf;
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fstat64 fd buf_addr =
    try
      let oc_buf =
	(if (fd <> 1 || true) then
	   Unix.fstat (self#get_fd fd)
	 else
	   Unix.stat "/etc/group") (* pretend stdout is always redirected *) in
	self#write_oc_statbuf_as_stat64 buf_addr oc_buf;
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err
	  
  method sys_statfs path buf =
    ignore(path);
    try
      ignore(Unix.stat path); (* only for the errors it generates *)
      (match !opt_arch with
	 | X86| ARM ->
	     self#write_fake_statfs_buf buf
	 | X64 ->
	     self#write_fake_statfs_x64_buf buf);
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fstatfs fd buf =
    ignore(fd);
    (match !opt_arch with
       | X86| ARM ->
	   self#write_fake_statfs_buf buf
       | X64 ->
	   self#write_fake_statfs_x64_buf buf);
    put_return 0L (* success *)

  method sys_statfs64 path buf_len struct_buf =
    assert(buf_len = 84 || buf_len = 88); (* Same layout, different padding *)
    self#write_fake_statfs64buf struct_buf;
    put_return 0L (* success *)

  method private sys_fstatfs64 fd buf_len struct_buf =
    assert(buf_len = 84 || buf_len = 88); (* Same layout, different padding *)
    self#write_fake_statfs64buf struct_buf;
    put_return 0L (* success *)

  method sys_fsync fd =
    ignore(fd);
    put_return 0L (* success *)

  method private sys_sysinfo info_buf =
    ignore(info_buf);
    self#put_errno Unix.ENOSYS

  method sys_tgkill tgid tid signal : unit =
    let my_pid = self#get_pid in
      if tgid = my_pid && tid = my_pid && signal = 6 then
	raise SimulatedAbort;
      failwith "Unhandled args to tgkill (not abort())"

  method sys_time addr =
    let time = Int64.of_float (Unix.time ()) in
      if addr <> 0L then
	store_word addr 0 time else ();
      put_return time

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
      put_return (Int64.add ut st)

  method sys_umask new_mask =
    let old_mask = Unix.umask new_mask in
      put_return (Int64.of_int old_mask)

  method private one_line_from_cmd cmd =
    let ic = Unix.open_process_in cmd in
    let s = try
      input_line ic
    with
      | End_of_file -> ""
    in
      ignore(Unix.close_process_in ic);
      s

  method private external_uname =
    [(self#one_line_from_cmd "uname -s");
     (self#one_line_from_cmd "uname -n");
     (self#one_line_from_cmd "uname -r");
     (self#one_line_from_cmd "uname -v");
     (self#one_line_from_cmd "uname -m");
     (self#one_line_from_cmd "domainname")]

  method sys_uname buf =
    let nodename = (Unix.gethostname ()) in
      List.iter2
	(fun i str ->
	   fm#store_cstr buf (Int64.mul 65L i) str)
	[0L; 1L; 2L; 3L; 4L; 5L]
	(match (!opt_external_uname, !opt_arch) with
	   | (true, _) -> self#external_uname
	   | (false, X86) ->
	       ["Linux"; (* sysname *)
		nodename; (* nodename *)
		"3.2.0-1-amd64"; (* release *)
		"#1 SMP Fri Mar 27 04:02:59 UTC 2011"; (* version *)
		"i686"; (* machine *)
		"example.com" (* domain *)
	       ]
	   | (false, X64) ->
	       ["Linux"; (* sysname *)
		nodename; (* nodename *)
		"3.2.0-1-amd64"; (* release *)
		"#1 SMP Fri Mar 27 04:02:59 UTC 2011"; (* version *)
		"x86_64"; (* machine *)
		"example.com" (* domain *)
	       ]
	   | (false, ARM) ->
	       ["Linux"; (* sysname *)
		nodename; (* nodename *)
		"3.2.0-1-ARCH"; (* release *)
		"#1 Wed Jun 15 07:34:48 UTC 2011"; (* version *)
		"armv5tejl"; (* machine *)
		"example.com" (* domain *)
	       ]
	);
      put_return 0L (* success *)

  method sys_alarm sec =
    if sec <> 0 then
      raise (Unix.Unix_error
               (Unix.EOPNOTSUPP, 
               "Nonzero argument to alarm(2) not supported", ""))
    else
      put_return 0L

  method sys_unlink path =
    try
      Unix.unlink path;
      put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_utime path times_buf =
    let actime = Int64.to_float (load_word (lea times_buf 0 0 0)) and
	modtime = Int64.to_float (load_word (lea times_buf 0 0 4))
    in
      Unix.utimes path actime modtime;
      put_return 0L (* success *)

  method sys_utimensat dirfd path_buf timespec_buf flags =
    let path = match (dirfd, path_buf, flags) with
      | (_, 0L, 0) when dirfd <> -100 ->
	  fd_info.(dirfd).fname
      | _ -> failwith "Unsupported args to utimensat"
    in
    let field off = load_word (lea timespec_buf 0 0 off) in
    let float off = Int64.to_float (field off) in
    let actime  = (float 0) +. (float 4)  /. 1000000000.0 and
	modtime = (float 8) +. (float 12) /. 1000000000.0
    in
      assert((field  4) <> 0x3fffffffL); (* UTIME_NOW *)
      assert((field  4) <> 0x3ffffffeL); (* UTIME_OMIT *)
      assert((field 12) <> 0x3fffffffL); (* UTIME_NOW *)
      assert((field 12) <> 0x3ffffffeL); (* UTIME_OMIT *)
      Unix.utimes path actime modtime;
      put_return 0L (* success *)      

  method sys_write fd bytes count =
    self#do_write fd bytes count

  method private iovec_load_base iov i =
    match !opt_arch with
      | X86|ARM -> (load_word (lea iov i 8  0))
      | X64     -> (load_long (lea iov i 16 0))

  method private iovec_load_len iov i =
    match !opt_arch with
      | X86|ARM -> (load_word (lea iov i 8  4))
      | X64     -> (load_long (lea iov i 16 8))

  method private iovec_size iov cnt =
    let sum = ref 0 in
      for i = 0 to cnt - 1 do
	let len = Int64.to_int (self#iovec_load_len iov i) in
	  sum := !sum + len
      done;
      !sum

  method private scatter_iovec iov cnt buf =
    let off = ref 0 in
      for i = 0 to cnt - 1 do
	let base = self#iovec_load_base iov i and
	    len = Int64.to_int (self#iovec_load_len iov i)
	in
	  fm#store_str base 0L (String.sub buf !off len);
	  off := !off + len
      done

  method private gather_iovec iov cnt =
    let read_vec i =
      read_buf
	(self#iovec_load_base iov i) (* iov_base *)
	(Int64.to_int (self#iovec_load_len iov i)) (* iov_len *)
    in
      Array.concat (Vine_util.mapn read_vec (cnt - 1))

  method sys_writev fd iov cnt =
    let bytes = self#gather_iovec iov cnt in
      self#do_write fd bytes (Array.length bytes)

  (* If you don't like long methods, you may want to stop reading this
     file now. This method is the dispatcher that reads the system call
     number and arguments out of registers, and then calls the
     appropriate sys_* method. The method is big because Linux has more
     than 300 system calls, and the numbering and some interface details
     differ by architecture. The list is roughly in order by x86-32
     system call number (generally corresponding to when syscalls were
     added to the kernel), with other-architecture versions of a syscall
     grouped with the x86-32. *)
  method private handle_linux_syscall () =
    let get_reg r = 
      match (!opt_arch, !opt_symbolic_syscall_error) with
	| (X64, None) ->
	    fm#get_long_var_concretize r
	      !opt_measure_influence_syscall_args "syscall arg"
	| (X64, _) ->
	    fm#get_long_var r (* fail if not concrete *)
	| ((X86|ARM), None) ->
	    fm#get_word_var_concretize r
	      !opt_measure_influence_syscall_args "syscall arg"
	| ((X86|ARM), _) ->
	    fm#get_word_var r (* fail if not concrete *)
    in
    let uh s = raise (UnhandledSysCall(s))
    in
    let (callnum_reg, arg_regs, ret_reg) = match !opt_arch with
      |	X86 -> (R_EAX, [| R_EBX; R_ECX; R_EDX; R_ESI; R_EDI; R_EBP |], R_EAX)
      | ARM -> (R7, [| R0; R1; R2; R3; R4; R5; R6 |], R0)
      | X64 -> (R_RAX, [| R_RDI; R_RSI; R_RDX; R_R10; R_R8; R_R9 |], R_RAX)

    in
    (let syscall_num = Int64.to_int (get_reg callnum_reg) and
	 read_1_reg () = get_reg arg_regs.(0) in
     let read_2_regs () =
       let ebx = read_1_reg () and
	   ecx = get_reg arg_regs.(1) in
	 (ebx, ecx) in
     let read_3_regs () = 
       let (ebx, ecx) = read_2_regs () and
	   edx = get_reg arg_regs.(2) in
	 (ebx, ecx, edx) in
     let read_4_regs () =
       let (ebx, ecx, edx) = read_3_regs () and
	   esi = get_reg arg_regs.(3) in
	 (ebx, ecx, edx, esi) in
     let read_5_regs () =
       let (ebx, ecx, edx, esi) = read_4_regs () and
	   edi = get_reg arg_regs.(4) in
	 (ebx, ecx, edx, esi, edi) in
     let read_6_regs () =
       let (ebx, ecx, edx, esi, edi) = read_5_regs () and
	   ebp = get_reg arg_regs.(5) in
	 (ebx, ecx, edx, esi, edi, ebp) in
     let read_7_regs () =
       assert(!opt_arch <> X86); (* x86 only has 6 available registers *)
       let (r0, r1, r2, r3, r4, r5) = read_6_regs () and
	   r6 = get_reg arg_regs.(6) in
	 (r0, r1, r2, r3, r4, r5, r6)
     in
       ignore(0, read_7_regs);
       match (!opt_arch, syscall_num) with 
	 | ((X86|ARM), 0) -> (* restart_syscall *)
	     uh "Unhandled Linux system call restart_syscall (0)"
	 | (X64, 219) -> (* restart_syscall *)
	     uh "Unhandled Linux/x64 system call restart_syscall (219)"
	 | ((X86|ARM), 1) (* exit *)
	 | (X64, 60)   -> (* exit *)
	     let arg1 = read_1_reg () in
	     let status = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "exit(%Ld) (no return)\n" status;
	       self#sys_exit status
	 | ((X86|ARM), 2) -> (* fork *)
	     uh "Unhandled Linux system call fork (2)"
	 | (X64, 57) -> (* fork *)
	     uh "Unhandled Linux/x64 system call fork (57)"
	 | ((X86|ARM), 3) (* read *)
	 | (X64, 0) (* read *) ->
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "read(%d, 0x%08Lx, %d)" fd buf count;
	       self#sys_read fd buf count;
	 | ((X86|ARM), 4) (* write *)
	 | (X64, 1) (* write *) ->
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "write(%d, 0x%08Lx, %d)\n" fd buf count;
	       let bytes = read_buf buf count in
		 self#sys_write fd bytes count
	 | ((X86|ARM), 5) (* open *)
	 | (X64, 2) (* open *) ->
	     let (arg1, arg2) = read_2_regs () in
	     let arg3 = (if (Int64.logand arg2 0o100L) <> 0L then
			   get_reg arg_regs.(2)
			 else
			   0L) in
	     let path_buf = arg1 and
		 flags    = Int64.to_int arg2 and
		 mode     = Int64.to_int arg3 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "open(\"%s\", 0x%x, 0o%o)" path flags mode;
	       self#sys_open path flags mode
	 | ((X86|ARM), 6) (* close *)
	 | (X64, 3) (* close *) ->
	     let arg1 = read_1_reg () in
	     let fd = Int64.to_int arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "close(%d)" fd;
	       self#sys_close fd
	 | (ARM, 7) -> uh "No waitpid (7) syscall in Linux/ARM (E)ABI"
	 | (X86, 7) -> (* waitpid *)
	     uh "Unhandled Linux system call waitpid (7)"
	 | ((X86|ARM), 8) -> (* creat *)
	     uh "Unhandled Linux system call creat (8)"
	 | (X64, 85) -> (* creat *)
	     uh "Unhandled Linux/x64 system call creat (85)"
	 | ((X86|ARM), 9) (* link *)
	 | (X64, 86)   -> (* link *)
	     let (arg1, arg2) = read_2_regs () in
	     let oldpath = fm#read_cstr arg1 and
		 newpath = fm#read_cstr arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "link(\"%s\", \"%s\")" oldpath newpath;
	       self#sys_link oldpath newpath
	 | (ARM, 10) -> uh "Check whether ARM unlink syscall matches x86"
	 | (X86, 10)    (* unlink *)
	 | (X64, 87) -> (* unlink *)
	     let ebx = read_1_reg () in
	     let path = fm#read_cstr ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "unlink(\"%s\")" path;
	       self#sys_unlink path
	 | ((X86|ARM), 11) -> (* execve *)
	     uh "Unhandled Linux system call execve (11)"
	 | (X64, 59) -> (* execve *)
	     uh "Unhandled Linux/x64 system call execve (59)"
	 | (ARM, 12) -> uh "Check whether ARM chdir syscall matches x86"
	 | (X86, 12)    (* chdir *)
	 | (X64, 80) -> (* chdir *)
	     let ebx = read_1_reg () in
	     let path = fm#read_cstr ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "chdir(\"%s\")" path;
	       self#sys_chdir path
	 | (ARM, 13) -> uh "Check whether ARM time syscall matches x86"
	 | (X86, 13) (* time *)
	 | (X64, 201) -> (* time *)
	     let arg1 = read_1_reg () in
	     let addr = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "time(0x%08Lx)" addr;
	       self#sys_time addr
	 | ((X86|ARM), 14) -> (* mknod *)
	     uh "Unhandled Linux system call mknod (14)"
	 | (X64, 133) -> (* mknod *)
	     uh "Unhandled Linux/x64 system call mknod (133)"
	 | ((X86|ARM), 15) (* chmod *)
	 | (X64, 90)    -> (* chmod *)
	     let (arg1, arg2) = read_2_regs () in
	     let path = fm#read_cstr arg1 and
		 mode = Int64.to_int arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "chmod(\"%s\", 0o%o)" path mode;
	       self#sys_chmod path mode
	 | ((X86|ARM), 16) -> (* lchown *)
	     uh "Unhandled Linux system call lchown (16)"
	 | (X64, 94) -> (* lchown *)
	     uh "Unhandled Linux/x64 system call lchown (94)"
	 | (ARM, 17) -> uh "No break (17) syscall in Linux/ARM (E)ABI"
	 | (X86, 17) -> (* break *)
	     uh "Unhandled Linux system call break (17)"
	 | (ARM, 18) -> uh "No oldstat (18) syscall in Linux/ARM (E)ABI"
	 | (X86, 18) -> (* oldstat *)
	     uh "Unhandled Linux system call oldstat (18)"
	 | (ARM, 19) -> uh "Check whether ARM lseek syscall matches x86"
	 | (X86, 19) (* lseek *)
	 | (X64,  8) -> (* lseek *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let (fd: int) = Int64.to_int ebx and
		 offset = ecx and
		 whence = (Int64.to_int edx) in
	       if !opt_trace_syscalls then
		 Printf.printf "lseek(%d, %Ld, %d)" fd offset whence;
	       self#sys_lseek fd offset whence
	 | ((X86|ARM), 20) (* getpid *)
	 | (X64, 39)    -> (* getpid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getpid()";
	     self#sys_getpid ()
	 | ((X86|ARM), 21) -> (* mount *)
	     uh "Unhandled Linux system call mount (21)"
	 | (X64, 165) -> (* mount *)
	     uh "Unhandled Linux/x64 system call mount (165)"
	 | ((X86|ARM), 22) -> (* umount *)
	     uh "Unhandled Linux system call umount (22)"
	 | ((X86|ARM), 23) -> (* setuid *)
	     uh "Unhandled Linux system call setuid (23)"
	 | (X64, 105) -> (* setuid *)
	     uh "Unhandled Linux/x64 system call setuid (105)"
	 | (ARM, 24) -> uh "Check whether ARM getuid syscall matches x86"
	 | (X86, 24) (* getuid *)
	 | (X64, 102) -> (* getuid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getuid()";
	     self#sys_getuid ()
	 | ((X86|ARM), 25) -> (* stime *)
	     uh "Unhandled Linux system call stime (25)"
	 | ((X86|ARM), 26) -> (* ptrace *)
	     uh "Unhandled Linux system call ptrace (26)"
	 | (X64, 101) -> (* ptrace *)
	     uh "Unhandled Linux/x64 system call ptrace (101)"
	 | ((X86|ARM), 27) (* alarm *)
	 | (X64, 37)    -> (* alarm *)
             let arg = read_1_reg () in
             let sec = Int64.to_int arg in
               if !opt_trace_syscalls then
		 Printf.printf "alarm(%d)" sec;
               self#sys_alarm sec
	 | (ARM, 28) -> uh "No oldfstat (28) syscall in Linux/ARM (E)ABI"
	 | (X86, 28) -> (* oldfstat *)
	     uh "Unhandled Linux system call oldfstat (28)"
	 | ((X86|ARM), 29) -> (* pause *)
	     uh "Unhandled Linux system call pause (29)"
	 | (X64, 34) -> (* pause *)
	     uh "Unhandled Linux/x64 system call pause (34)"
	 | (ARM, 30) -> uh "Check whether ARM utime syscall matches x86"
	 | (X64, 132) -> uh "Check whether x64 utime syscall matches x86"
	 | (X86, 30) -> (* utime *)
	     let (ebx, ecx) = read_2_regs () in
	     let path = fm#read_cstr ebx and
		 times_buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "utime(\"%s\", 0x%08Lx)" path times_buf;
	       self#sys_utime path times_buf
	 | (ARM, 31) -> uh "No stty (31) syscall in Linux/ARM (E)ABI"
	 | (X86, 31) -> (* stty *)
	     uh "Unhandled Linux system call stty (31)"
	 | (ARM, 32) -> uh "No gtty (32) syscall in Linux/ARM (E)ABI"
	 | (X86, 32) -> (* gtty *)
	     uh "Unhandled Linux system call gtty (32)"
	 | ((X86|ARM), 33) (* access *)
	 | (X64, 21) ->    (* access *)
	     let (arg1, arg2) = read_2_regs () in
	     let path_buf = arg1 and
		 mode     = Int64.to_int arg2 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "access(\"%s\", 0x%x)" path mode;
	       self#sys_access path mode
	 | ((X86|ARM), 34) -> (* nice *)
	     uh "Unhandled Linux system call nice (34)"
	 | (ARM, 35) -> uh "No ftime (35) syscall in Linux/ARM (E)ABI"
	 | (X86, 35) -> (* ftime *)
	     uh "Unhandled Linux system call ftime (35)"
	 | ((X86|ARM), 36) -> (* sync *)
	     uh "Unhandled Linux system call sync (36)"
	 | (X64, 162) -> (* sync *)
	     uh "Unhandled Linux/x64 system call sync (162)"
	 | ((X86|ARM), 37) -> (* kill *)
	     uh "Unhandled Linux system call kill (37)"
	 | (X64, 62) -> (* kill *)
	     uh "Unhandled Linux/x64 system call kill (62)"
	 | ((X86|ARM), 38) (* rename *)
	 | (X64, 82)    -> (* rename *)
	     let (arg1, arg2) = read_2_regs () in
	     let oldpath = fm#read_cstr arg1 and
		 newpath = fm#read_cstr arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "rename(\"%s\", \"%s\")" oldpath newpath;
	       self#sys_rename oldpath newpath
	 | (ARM, 39) -> uh "Check whether ARM mkdir syscall matches x86"
	 | (X86, 39)    (* mkdir *)
	 | (X64, 83) -> (* mkdir *)
	     let (ebx, ecx) = read_2_regs () in
	     let path_buf = ebx and
		 mode     = Int64.to_int ecx in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "mkdir(\"%s\", 0x%x)" path mode;
	       self#sys_mkdir path mode
	 | ((X86|ARM), 40) -> (* rmdir *)
	     uh "Unhandled Linux system call rmdir (40)"
	 | (X64, 84) -> (* rmdir *)
	     uh "Unhandled Linux/x64 system call rmdir (84)"
	 | ((X86|ARM), 41) (* dup *)
	 | (X64, 32)    -> (* dup *)
	     let arg1 = read_1_reg () in
	     let fd = Int64.to_int arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "dup(%d)" fd;
	       self#sys_dup fd
	 | (X64, 22) -> uh "Check whether x64 pipe syscall matches x86"
	 | (ARM, 42) -> uh "Check whether ARM pipe syscall matches x86"
	 | (X86, 42) -> (* pipe *)
	     let ebx = read_1_reg () in
	     let buf = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "pipe(0x%08Lx)" buf;
	       self#sys_pipe buf
	 | (ARM, 43) -> uh "Check whether ARM times syscall matches x86"
	 | (X64, 100) -> uh "Check whether x64 times syscall matches x86"
	 | (X86, 43) -> (* times *)
	     let ebx = read_1_reg () in
	     let addr = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "times(0x%08Lx)" addr;
	       self#sys_times addr
	 | (ARM, 44) -> uh "No prof (44) syscall in Linux/ARM (E)ABI"
	 | (X86, 44) -> (* prof *)
	     uh "Unhandled Linux system call prof (44)"
	 | ((X86|ARM), 45) (* brk *)
	 | (X64, 12) -> (* brk *)
	     let arg1 = read_1_reg () in
	     let addr = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "brk(0x%08Lx)" addr;
	       self#sys_brk addr
	 | ((X86|ARM), 46) -> (* setgid *)
	     uh "Unhandled Linux system call setgid (46)"
	 | (X64, 106) -> (* setgid *)
	     uh "Unhandled Linux/x64 system call setgid (106)"
	 | (ARM, 47) -> uh "Check whether ARM getgid syscall matches x86"
	 | (X86, 47)     (* getgid *)
	 | (X64, 104) -> (* getgid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getgid()";
	     self#sys_getgid ()
	 | (ARM, 48) -> uh "No signal (48) syscall in Linux/ARM (E)ABI"
	 | (X86, 48) -> (* signal *)
	     uh "Unhandled Linux system call signal (48)"
	 | (ARM, 49) -> uh "Check whether ARM geteuid syscall matches x86"
	 | (X86, 49) (* geteuid *)
	 | (X64, 107) -> (* geteuid *)
	     if !opt_trace_syscalls then
	       Printf.printf "geteuid()";
	     self#sys_geteuid ()
	 | (ARM, 50) -> uh "Check whether ARM getegid syscall matches x86"
	 | (X86, 50) (* getegid *)
	 | (X64, 108) -> (* geteuid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getegid()";
	     self#sys_getegid ()
	 | ((X86|ARM), 51) -> (* acct *)
	     uh "Unhandled Linux system call acct (51)"
	 | (X64, 163) -> (* acct *)
	     uh "Unhandled Linux/x64 system call acct (163)"
	 | ((X86|ARM), 52) -> (* umount2 *)
	     uh "Unhandled Linux system call umount2 (52)"
	 | (X64, 166) -> (* umount2 *)
	     uh "Unhandled Linux/x64 system call umount2 (166)"
	 | (ARM, 53) -> uh "No lock (53) syscall in Linux/ARM (E)ABI"
	 | (X86, 53) -> (* lock *)
	     uh "Unhandled Linux system call lock (53)"
	 | ((X86|ARM), 54) (* ioctl *)
	 | (X64, 16)    -> (* ioctl *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd   = Int64.to_int arg1 and
		 req  = arg2 and
		 argp = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "ioctl(%d, 0x%Lx, 0x%08Lx)" fd req argp;
	       self#sys_ioctl fd req argp;
	 | (ARM, 55) -> uh "Check whether ARM fcntl syscall matches x86"
	 | (X86, 55) (* fcntl *)
	 | (X64, 72) (* fnctl *) ->
	     let (ebx, ecx, edx) = read_3_regs () in
	     let fd = Int64.to_int ebx and
		 cmd = Int64.to_int ecx and
		 arg = edx in
	       if !opt_trace_syscalls then
		 Printf.printf "fcntl(%d, %d, 0x%08Lx)" fd cmd arg;
	       self#sys_fcntl fd cmd arg
	 | (ARM, 56) -> uh "No mpx (56) syscall in Linux/ARM (E)ABI"
	 | (X86, 56) -> (* mpx *)
	     uh "Unhandled Linux system call mpx (56)"
	 | ((X86|ARM), 57) -> (* setpgid *)
	     uh "Unhandled Linux system call setpgid (57)"
	 | (X64, 109) -> (* setpgid *)
	     uh "Unhandled Linux/x64 system call setpgid (109)"
	 | (ARM, 58) -> uh "No ulimit (58) syscall in Linux/ARM (E)ABI"
	 | (X86, 58) -> (* ulimit *)
	     uh "Unhandled Linux system call ulimit (58)"
	 | (ARM, 59) -> uh "No ulimit (59) syscall in Linux/ARM (E)ABI"
	 | (X86, 59) -> (* oldolduname *)
	     uh "Unhandled Linux system call oldolduname (59)"
	 | (ARM, 60) -> uh "Check whether ARM umask syscall matches x86"
	 | (X86, 60)    (* umask *)
	 | (X64, 95) -> (* umask *)
	     let ebx = read_1_reg () in
	     let mask = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "umask(0o%03o)" mask;
	       self#sys_umask mask;
	 | ((X86|ARM), 61) -> (* chroot *)
	     uh "Unhandled Linux system call chroot (61)"
	 | (X64, 161) -> (* chroot *)
	     uh "Unhandled Linux/x64 system call chroot (161)"
	 | ((X86|ARM), 62) -> (* ustat *)
	     uh "Unhandled Linux system call ustat (62)"
	 | (X64, 136) -> (* ustat *)
	     uh "Unhandled Linux/x64 system call ustat (136)"
	 | ((X86|ARM), 63) (* dup2 *)
	 | (X64, 33)    -> (* dup2 *)
             let (arg1,arg2) = read_2_regs () in
             let fd1 = Int64.to_int arg1 and
		 fd2 = Int64.to_int arg2 in
               if !opt_trace_syscalls then
		 Printf.printf "dup2(%d,%d)" fd1 fd2;
               self#sys_dup2 fd1 fd2
	 | (ARM, 64) -> uh "Check whether ARM getppid syscall matches x86"
	 | (X86, 64)     (* getppid *)
	 | (X64, 110) -> (* getppid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getppid()";
	     self#sys_getppid ()
	 | (ARM, 65) -> uh "Check whether ARM getpgrp syscall matches x86"
	 | (X86,  65)    (* getpgrp *)
	 | (X64, 111) -> (* getpgrp *)
	     if !opt_trace_syscalls then
	       Printf.printf "getpgrp()";
	     self#sys_getpgrp ()
	 | ((X86|ARM), 66) -> (* setsid *)
	     uh "Unhandled Linux system call setsid (66)"
	 | ((X86|ARM), 67) -> (* sigaction *)
	     uh "Unhandled Linux system call sigaction (67)"
	 | (ARM, 68) -> uh "No sgetmask (68) syscall in Linux/ARM (E)ABI"
	 | (X86, 68) -> (* sgetmask *)
	     uh "Unhandled Linux system call sgetmask (68)"
	 | (ARM, 69) -> uh "No ssetmask (69) syscall in Linux/ARM (E)ABI"
	 | (X86, 69) -> (* ssetmask *)
	     uh "Unhandled Linux system call ssetmask (69)"
	 | (ARM, 70) -> uh "Check whether ARM setreuid syscall matches x86"
	 | (X86, 70) -> (* setreuid *)
	     let (ebx, ecx) = read_2_regs () in
	     let ruid = Int64.to_int ebx and
		 euid = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "setreuid(%d, %d)" ruid euid;
	       self#sys_setreuid ruid euid
	 | ((X86|ARM), 71) -> (* setregid *)
	     uh "Unhandled Linux system call setregid (71)"
	 | ((X86|ARM), 72) -> (* sigsuspend *)
	     uh "Unhandled Linux system call sigsuspend (72)"
	 | ((X86|ARM), 73) -> (* sigpending *)
	     uh "Unhandled Linux system call sigpending (73)"
	 | ((X86|ARM), 74) -> (* sethostname *)
	     uh "Unhandled Linux system call sethostname (74)"
	 | (X64, 170) -> (* sethostname *)
	     uh "Unhandled Linux/x64 system call sethostname (170)"
	 | (ARM, 75) -> uh "Check whether ARM setrlimit syscall matches x86"
	 | (X64, 160) -> uh "Check whether x64 setrlimit syscall matches x86"
	 | (X86, 75) -> (* setrlimit *)
	     let (ebx, ecx) = read_2_regs () in
	     let resource = Int64.to_int ebx and
		 rlim = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "setrlimit(%d, 0x%08Lx)" resource rlim;
	       self#sys_setrlimit resource rlim
	 | ((X86|ARM), 76) -> (* getrlimit *)
	     uh "Unhandled Linux system call (old) getrlimit (76)"
	 | (X64, 97) -> (* getrlimit *)
	     let (arg1, arg2) = read_2_regs () in
	     let rsrc = Int64.to_int arg1 and
		 buf  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "getrlimit(%d, 0x%08Lx)" rsrc buf;
	       self#sys_getrlimit rsrc buf
	 | ((X86|ARM), 77) -> (* getrusage, 32-bit structure *)
	     let (ebx, ecx) = read_2_regs () in
	     let who = Int64.to_int ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "getrusage(%d, 0x%08Lx)" who buf;
	       self#sys_getrusage32 who buf
	 | (X64, 98) -> (* getrusage, 64-bit structure *)
	     let (arg1, arg2) = read_2_regs () in
	     let who = Int64.to_int arg1 and
		 buf = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "getrusage(%d, 0x%08Lx)" who buf;
	       self#sys_getrusage64 who buf
	 | (X64, 96) (* gettimeofday *)
	 | ((X86|ARM), 78) -> (* gettimeofday *)
	     let (arg1, arg2) = read_2_regs () in
	     let timep = arg1 and
		 zonep = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "gettimeofday(0x%08Lx, 0x%08Lx)" timep zonep;
	       self#sys_gettimeofday timep zonep
	 | ((X86|ARM), 79) -> (* settimeofday *)
	     uh "Unhandled Linux system call settimeofday (79)"
	 | (X64, 164) -> (* settimeofday *)
	     uh "Unhandled Linux/x64 system call settimeofday (164)"
	 | ((X86|ARM), 80) -> (* getgroups *)
	     uh "Unhandled Linux system call getgroups (80)"
	 | ((X86|ARM), 81) -> (* setgroups *)
	     uh "Unhandled Linux system call setgroups (81)"
	 | ((X86|ARM), 82) -> (* select *)
	     uh "Unhandled Linux system call select (82)"
	 | (X64, 23) -> (* select *)
	     let (ebx, ecx, edx, esi, edi) = read_5_regs () in
	     let nfds = Int64.to_int ebx and
		 readfds = ecx and
		 writefds = edx and
		 exceptfds = esi and
		 timeout = edi in
	       if !opt_trace_syscalls then
		 Printf.printf "select(%d, 0x%08Lx, 0x%08Lx, 0x%08Lx, 0x%08Lx)"
		   nfds readfds writefds exceptfds timeout;
	       self#sys_select nfds readfds writefds exceptfds timeout
	 | ((X86|ARM), 83) (* symlink *)
	 | (X64, 88)    -> (* symlink *)
             let (arg1, arg2) = read_2_regs () in
             let target = (fm#read_cstr arg1) and
		 linkpath = (fm#read_cstr arg2) in
               if !opt_trace_syscalls then
		 Printf.printf "symlink(%s, %s)" target linkpath;
               self#sys_symlink target linkpath
	 | (ARM, 84) -> uh "No oldlstat (84) syscall in Linux/ARM (E)ABI"
	 | (X86, 84) -> (* oldlstat *)
	     uh "Unhandled Linux system call oldlstat (84)"
	 | ((X86|ARM), 85) (* readlink *)
	 | (X64, 89)    -> (* readlink *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let path_buf = arg1 and
		 out_buf  = arg2 and
		 buflen   = Int64.to_int arg3 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "readlink(\"%s\", 0x%08Lx, %d)"
		   path out_buf buflen;
	       self#sys_readlink path out_buf buflen
	 | ((X86|ARM), 86) -> (* uselib *)
	     uh "Unhandled Linux system call uselib (86)"
	 | (X64, 134) -> (* uselib *)
	     uh "Unhandled Linux/x64 system call uselib (134)"
	 | ((X86|ARM), 87) -> (* swapon *)
	     uh "Unhandled Linux system call swapon (87)"
	 | (X64, 167) -> (* swapon *)
	     uh "Unhandled Linux/x64 system call swapon (167)"
	 | ((X86|ARM), 88) -> (* reboot *)
	     uh "Unhandled Linux system call reboot (88)"
	 | (X64, 169) -> (* reboot *)
	     uh "Unhandled Linux/x64 system call reboot (169)"
	 | ((X86|ARM), 89) -> (* readdir *)
	     uh "Unhandled Linux system call readdir (89)"
	 | (ARM, 90) -> uh "Check whether ARM mmap (90) syscall matches x86"
	 | (X86, 90) -> (* mmap *)
	     let ebx = read_1_reg () in
	     let addr   = load_word ebx and
		 length = load_word (lea ebx 0 0 4) and
		 prot   = Int64.to_int (load_word (lea ebx 0 0 8)) and
		 flags  = Int64.to_int (load_word (lea ebx 0 0 12)) and
		 fd     = Int64.to_int (load_word (lea ebx 0 0 16)) and
		 offset = load_word (lea ebx 0 0 20) in
	       if !opt_trace_syscalls then
		 Printf.printf "mmap(0x%08Lx, %Ld, 0x%x, 0x%0x, %d, %Ld)"
		   addr length prot flags fd offset;
	       self#sys_mmap addr length prot flags fd offset
	 | (X64, 9) -> (* mmap *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let addr     = arg1 and
		 length   = arg2 and
		 prot     = Int64.to_int arg3 and
		 flags    = Int64.to_int arg4 and
		 fd       = Int64.to_int (fix_s32 arg5) and
		 offset   = arg6 in
	       if !opt_trace_syscalls then
		 Printf.printf "mmap(0x%08Lx, %Ld, 0x%x, 0x%x, %d, %Ld)"
		   addr length prot flags fd offset;
	       self#sys_mmap addr length prot flags fd offset
	 | ((X86|ARM), 91) (* munmap *)
	 | (X64, 11) -> (* munmap *)
	     let (arg1, arg2) = read_2_regs () in
	     let addr = arg1 and
		 len  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "munmap(0x%08Lx, %Ld)" addr len;
	       self#sys_munmap addr len
	 | ((X86|ARM), 92) -> (* truncate *)
	     uh "Unhandled Linux system call truncate (92)"
	 | (X64, 76) -> (* truncate *)
	     uh "Unhandled Linux/x64 system call truncate (76)"
	 | ((X86|ARM), 93) -> (* ftruncate *)
	     uh "Unhandled Linux system call ftruncate (93)"
	 | (X64, 77) -> (* ftruncate *)
	     uh "Unhandled Linux/x64 system call ftruncate (77)"
	 | (ARM, 94) -> uh "Check whether ARM fchmod syscall matches x86"
	 | (X86, 94)    (* fchmod *)
	 | (X64, 91) -> (* fchmod *)
	     let (ebx, ecx) = read_2_regs () in
	     let fd = Int64.to_int ebx and
		 mode = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "fchmod(%d, 0o%03o)" fd mode;
	       self#sys_fchmod fd mode
	 | ((X86|ARM), 95) -> (* fchown *)
	     uh "Unhandled Linux system call fchown (95)"
	 | (X64, 93) -> (* fchown *)
	     uh "Unhandled Linux/x64 system call fchown (93)"
	 | ((X86|ARM), 96) -> (* getpriority *)
	     uh "Unhandled Linux system call getpriority (96)"
	 | (X64, 140) -> (* getpriority *)
	     uh "Unhandled Linux/x64 system call getpriority (140)"
	 | ((X86|ARM), 97) -> (* setpriority *)
	     uh "Unhandled Linux system call setpriority (97)"
	 | (X64, 141) -> (* setpriority *)
	     uh "Unhandled Linux/x64 system call setpriority (141)"
	 | (ARM, 98) -> uh "No profil (98) syscall in Linux/ARM (E)ABI"
	 | (X86, 98) -> (* profil *)
	     uh "Unhandled Linux system call profil (98)"
	 | (ARM, 99) -> uh "Check whether ARM statfs syscall matches x86"
	 | (X86,  99)    (* statfs *)
	 | (X64, 137) -> (* statfs *)
	     let (ebx, ecx) = read_2_regs () in
	     let path = fm#read_cstr ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "statfs(\"%s\", 0x%08Lx)" path buf;
	       self#sys_statfs path buf
	 | (ARM, 100) -> uh "Check whether ARM fstatfs syscall matches x86"
	 | (X86, 100)    (* fstatfs *)
	 | (X64, 138) -> (* fstatfs *)
	     let (ebx, ecx) = read_2_regs () in
	     let fd = Int64.to_int ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "fstatfs(%d, 0x%08Lx)" fd buf;
	       self#sys_fstatfs fd buf
	 | (ARM, 101) -> uh "No ioperm (101) syscall in Linux/ARM (E)ABI"
	 | (X86, 101) -> (* ioperm *)
	     uh "Unhandled Linux system call ioperm (101)"
	 | (X64, 173) -> (* ioperm *)
	     uh "Unhandled Linux/x64 system call ioperm (173)"
	 | (ARM, 102) -> uh "Check whether ARM socketcall syscall matches x86"
	 | (X86, 102) -> (* socketcall *)
	     let (ebx, ecx) = read_2_regs () in
	     let call = Int64.to_int ebx and
		 args = ecx in
	       (* Each sub-call has at least 2 args, so we can always
		  read them. The unit functions ensure we don't read the
		  later args unless needed, since memory reads in
		  FuzzBALL can have various side effects. *)
	     let arg1 = load_word args and
		 arg2 = load_word (lea args 0 0 4) and
		 get_arg3 () = load_word (lea args 0 0 8) and
		 get_arg4 () = load_word (lea args 0 0 12) and
		 get_arg5 () = load_word (lea args 0 0 16) and
		 get_arg6 () = load_word (lea args 0 0 20) in
	       (match call with
		  | 1 -> 
		      let dom_i = Int64.to_int arg1 and
			  typ_i = Int64.to_int arg2 and
			  prot_i = Int64.to_int (get_arg3 ()) in
			if !opt_trace_syscalls then
			  Printf.printf "socket(%d, %d, %d)"
			    dom_i typ_i prot_i;
			self#sys_socket dom_i typ_i prot_i
		  | 2 ->
		      let sockfd = Int64.to_int arg1 and
			  addr = arg2 and
			  addrlen = Int64.to_int (get_arg3 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf "bind(%d, 0x%08Lx, %d)"
			    sockfd addr addrlen;
			self#sys_bind sockfd addr addrlen
		  | 3 -> 
		      let sockfd = Int64.to_int arg1 and
			  addr = arg2 and
			  addrlen = Int64.to_int (get_arg3 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf "connect(%d, 0x%08Lx, %d)"
			    sockfd addr addrlen;
			self#sys_connect sockfd addr addrlen
		  | 4 -> 
		      let sockfd = Int64.to_int arg1 and
			  backlog = Int64.to_int arg2
		      in
			if !opt_trace_syscalls then
			  Printf.printf "listen(%d, %d)" sockfd backlog;
			self#sys_listen sockfd backlog
		  | 5 ->
		      let sockfd = Int64.to_int arg1 and
			  addr = arg2 and
			  addrlen_ptr = get_arg3 ()
		      in
			if !opt_trace_syscalls then
			  Printf.printf "accept(%d, 0x%08Lx, 0x%08Lx)"
			    sockfd addr addrlen_ptr;
			self#sys_accept sockfd addr addrlen_ptr
		  | 6 ->
		      let sockfd = Int64.to_int arg1 and
			  addr = arg2 and
			  addrlen_ptr = get_arg3 ()
		      in
			if !opt_trace_syscalls then
			  Printf.printf "getsockname(%d, 0x%08Lx, 0x%08Lx)"
			    sockfd addr addrlen_ptr;
			self#sys_getsockname sockfd addr addrlen_ptr
		  | 7 -> 
		      let sockfd = Int64. to_int arg1 and
			  addr = arg2 and
			  addrlen_ptr = get_arg3 ()
		      in
			if !opt_trace_syscalls then
			  Printf.printf "getpeername(%d, 0x%08Lx, 0x%08Lx)"
			    sockfd addr addrlen_ptr;
			self#sys_getpeername sockfd addr addrlen_ptr
		  | 8 -> 
		      let dom_i = Int64.to_int arg1 and
			  typ_i = Int64.to_int arg2 and
			  prot_i = Int64.to_int (get_arg3 ()) and
			  addr = get_arg4 () in
			if !opt_trace_syscalls then
			  Printf.printf "socketpair(%d, %d, %d, 0x%08Lx)"
			    dom_i typ_i prot_i addr;
			self#sys_socketpair dom_i typ_i prot_i addr
		  | 9 ->
		      let sockfd = Int64.to_int arg1 and
			  buf = arg2 and
			  len = Int64.to_int (get_arg3 ()) and
			  flags = Int64.to_int (get_arg4 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf "send(%d, 0x%08Lx, %d, %d)"
			    sockfd buf len flags;
			self#sys_send sockfd buf len flags
		  | 10 ->
		      let sockfd = Int64.to_int arg1 and
			  buf = arg2 and
			  len = Int64.to_int (get_arg3 ()) and
			  flags = Int64.to_int (get_arg4 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf "recv(%d, 0x%08Lx, %d, %d)"
			    sockfd buf len flags;
			self#sys_recv sockfd buf len flags
		  | 11 ->
		      let sockfd = Int64.to_int arg1 and
			  buf = arg2 and
			  len = Int64.to_int (get_arg3 ()) and
			  flags = Int64.to_int (get_arg4 ()) and
			  addr = get_arg5 () and
			  addrlen = Int64.to_int (get_arg6 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "sendto(%d, 0x%08Lx, %d, %d, 0x%08Lx, %d)"
			    sockfd buf len flags addr addrlen;
			self#sys_sendto sockfd buf len flags addr addrlen
		  | 12 ->
		      let sockfd = Int64.to_int arg1 and
			  buf = arg2 and
			  len = Int64.to_int (get_arg3 ()) and
			  flags = Int64.to_int (get_arg4 ()) and
			  addr = get_arg5 () and
			  addrlen_ptr = get_arg6 ()
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "recvfrom(%d, 0x%08Lx, %d, %d, 0x%08Lx, 0x%08Lx)"
			    sockfd buf len flags addr addrlen_ptr;
			self#sys_recvfrom sockfd buf len flags addr addrlen_ptr
		  | 13 ->
		      let sockfd = Int64.to_int arg1 and
			  how = Int64.to_int arg2 in
			if !opt_trace_syscalls then
			  Printf.printf "shutdown(%d, %d)" sockfd how;
			self#sys_shutdown sockfd how
		  | 14 ->
		      let sockfd = Int64.to_int arg1 and
			  level = Int64.to_int arg2 and
			  name = Int64.to_int (get_arg3 ()) and
			  valp = get_arg4 () and
			  len = Int64.to_int (get_arg5 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "setsockopt(%d, %d, %d, 0x%08Lx, %d)"
			    sockfd level name valp len;
			self#sys_setsockopt sockfd level name valp len
		  | 15 ->
		      let sockfd = Int64.to_int arg1 and
			  level = Int64.to_int arg2 and
			  name = Int64.to_int (get_arg3 ()) and
			  valp = get_arg4 () and
			  lenp = get_arg5 ()
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "getsockopt(%d, %d, %d, 0x%08Lx, 0x%08Lx)"
			    sockfd level name valp lenp;
			self#sys_getsockopt sockfd level name valp lenp

		  | 16 ->
		      let sockfd = Int64.to_int arg1 and
			  msg = arg2 and
			  flags = Int64.to_int (get_arg3 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf "sendmsg(%d, 0x%08Lx, %d)"
			    sockfd msg flags;
			self#sys_sendmsg sockfd msg flags
		  | 17 ->
		      let sockfd = Int64.to_int arg1 and
			  msg = arg2 and
			  flags = Int64.to_int (get_arg3 ())
		      in
			if !opt_trace_syscalls then
			  Printf.printf "recvmsg(%d, 0x%08Lx, %d)"
			    sockfd msg flags;
			self#sys_recvmsg32 sockfd msg flags
		  | 18 -> uh"Unhandled Linux system call accept4 (102:18)"
		  | _ -> self#put_errno Unix.EINVAL)
	 | ((X86|ARM|X64), 103) -> (* syslog *)
	     uh "Unhandled Linux system call syslog (103)"
	 | ((X86|ARM), 104) -> (* setitimer *)
	     uh "Unhandled Linux system call setitimer (104)"
	 | (X64, 38) -> (* setitimer *)
	     uh "Unhandled Linux/x64 system call setitimer (38)"
	 | ((X86|ARM), 105) -> (* getitimer *)
	     uh "Unhandled Linux system call getitimer (105)"
	 | (X64, 36) -> (* getitimer *)
	     uh "Unhandled Linux/x64 system call getitimer (36)"
	 | (ARM, 106) -> uh "Check whether ARM stat (106) syscall matches x86"
	 | (X86, 106) 
	 | (X64, 4) -> (* stat *)
	     let (ebx, ecx) = read_2_regs () in
	     let path_buf = ebx and
		 buf_addr = ecx in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "stat(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_stat path buf_addr
	 | (ARM, 107) -> uh "Check whether ARM lstat (107) syscall matches x86"
	 | (X86, 107)    (* lstat *)
	 | (X64,   6) -> (* lstat *)
	     let (ebx, ecx) = read_2_regs () in
	     let path_buf = ebx and
		 buf_addr = ecx in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "lstat(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_lstat path buf_addr
	 | (ARM, 108) -> uh "Check whether ARM fstat (108) syscall matches x86"
	 | (X86, 108) (* fstat *)
	 | (X64, 5) -> (* fstat *)
	     let (ebx, ecx) = read_2_regs () in
	     let fd = Int64.to_int ebx and
		 buf_addr = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "fstat(%d, 0x%08Lx)" fd buf_addr;
	       self#sys_fstat fd buf_addr
	 | (ARM, 109) -> uh "No olduname (109) syscall in Linux/ARM (E)ABI"
	 | (X86, 109) -> (* olduname *)
	     uh "Unhandled Linux system call olduname (109)"
	 | (ARM, 110) -> uh "No iopl (110) syscall in Linux/ARM (E)ABI"
	 | (x86, 110) -> (* iopl *)
	     uh "Unhandled Linux system call iopl (110)"
	 | (X64, 172) -> (* iopl *)
	     uh "Unhandled Linux/x64 system call iopl (172)"
	 | ((X86|ARM), 111) -> (* vhangup *)
	     uh "Unhandled Linux system call vhangup (111)"
	 | (X64, 153) -> (* vhangup *)
	     uh "Unhandled Linux/x64 system call vhangup (153)"
	 | (ARM, 112) -> uh "No idle (112) syscall in Linux/ARM (E)ABI"
	 | (X86, 112) -> (* idle *)
	     uh "Unhandled Linux system call idle (112)"
	 | (X86, 113) -> (* vm86old *)
	     uh "Unhandled Linux/x86 system call vm86old (113)"
	 | (ARM, 113) -> (* syscall *)
	     uh "Unhandled Linux/x86 system call syscall (113)"
	 | ((X86|ARM), 114) -> (* wait4 *)
	     uh "Unhandled Linux system call wait4 (114)"
	 | (X64, 61) -> (* wait4 *)
	     uh "Unhandled Linux/x64 system call wait4 (61)"
	 | ((X86|ARM), 115) -> (* swapoff *)
	     uh "Unhandled Linux system call swapoff (115)"
	 | (X64, 168) -> (* swapoff *)
	     uh "Unhandled Linux/x64 system call swapoff (168)"
	 | ((X86|ARM), 116) (* sysinfo *)
	 | (X64, 99)     -> (* sysinfo *)
	     let arg1 = read_1_reg () in
	     let info_buf = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "sysinfo(0x%Lx)" info_buf;
	       self#sys_sysinfo info_buf
	 | ((X86|ARM), 117) -> (* ipc *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let call = Int64.to_int arg1 and
		 first = Int64.to_int arg2 and
		 second = Int64.to_int arg3 and
		 third = Int64.to_int arg4 and
		 ptr = arg5 and
		 fifth = arg6
	     in
	       if !opt_trace_syscalls then
		 Printf.printf "ipc(%d, %d, %d, %d, 0x%08Lx, 0x%Lx)"
		   call first second third ptr fifth;
	       self#put_errno Unix.ENOSYS
	 | ((X86|ARM), 118) (* fsync *)
	 | (X64, 74)     -> (* fsync *)
	     let arg1 = read_1_reg () in
	     let fd = Int64.to_int arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "fsync(%d)" fd;
	       self#sys_fsync fd
	 | ((X86|ARM), 119) -> (* sigreturn *)
	     uh "Unhandled Linux system call sigreturn (119)"
	 | ((X86|ARM), 120) (* clone *)
         | (X64, 56)     -> (* clone *)
	     uh ("Unhandled Linux system call clone "
		 ^"(is your program multi-threaded?)")
	 | ((X86|ARM), 121) -> (* setdomainname *)
	     uh "Unhandled Linux system call setdomainname (121)"
	 | (X64, 171) -> (* setdomainname *)
	     uh "Unhandled Linux/x64 system call setdomainname (171)"
	 | ((X86|ARM), 122) (* uname *)
	 | (X64, 63)     -> (* uname *)
	     let arg1 = read_1_reg () in
	     let buf = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "uname(0x%08Lx)" buf;
	       self#sys_uname buf
	 | (ARM, 123) -> uh "No modify_ldt (112) syscall in Linux/ARM (E)ABI"
	 | (X86, 123) -> (* modify_ldt *)
	     uh "Unhandled Linux system call modify_ldt (123)"
	 | (X64, 154) -> (* modify_ldt *)
	     uh "Unhandled Linux/x64 system call modify_ldt (154)"
	 | ((X86|ARM), 124) -> (* adjtimex *)
	     uh "Unhandled Linux system call adjtimex (124)"
	 | (X64, 159) -> (* adjtimex *)
	     uh "Unhandled Linux/x64 system call adjtimex (159)"
	 | ((X86|ARM), 125) (* mprotect *)
	 | (X64, 10) (* mprotect *) ->
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let addr = arg1 and
		 len  = arg2 and
		 prot = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "mprotect(0x%08Lx, %Ld, %Ld)" addr len prot;
	       self#sys_mprotect addr len prot
	 | ((X86|ARM), 126) -> (* sigprocmask *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let how    = Int64.to_int arg1 and
		 newset = arg2 and
		 oldset = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "sigprocmask(%d, 0x%08Lx, 0x%08Lx)"
		   how newset oldset;
	       self#sys_rt_sigprocmask how newset oldset 4
	 | (ARM, 127) -> uh "No create_module syscall in Linux/ARM (E)ABI"
	 | (X86, 127) -> (* create_module *)
	     uh "Unhandled Linux system call create_module (127)"
	 | (X64, 174) -> (* create_module *)
	     uh "Unhandled Linux/x64 system call create_module (174)"
	 | ((X86|ARM), 128) -> (* init_module *)
	     uh "Unhandled Linux system call init_module (128)"
	 | (X64, 175) -> (* init_module *)
	     uh "Unhandled Linux/x64 system call init_module (175)"
	 | ((X86|ARM), 129) -> (* delete_module *)
	     uh "Unhandled Linux system call delete_module (129)"
	 | (X64, 176) -> (* delete_module *)
	     uh "Unhandled Linux/x64 system call delete_module (176)"
	 | (ARM, 130) -> uh "No get_kernel_syms syscall in Linux/ARM (E)ABI"
	 | (X86, 130) -> (* get_kernel_syms *)
	     uh "Unhandled Linux system call get_kernel_syms (130)"
	 | (X64, 177) -> (* get_kernel_syms *)
	     uh "Unhandled Linux/x64 system call get_kernel_syms (177)"
	 | ((X86|ARM), 131) -> (* quotactl *)
	     uh "Unhandled Linux system call quotactl (131)"
	 | (X64, 179) -> (* quotactl *)
	     uh "Unhandled Linux/x64 system call quotactl (179)"
	 | (ARM, 132) -> uh "Check whether ARM getpgid syscall matches x86"
	 | (X86, 132)    (* getpgid *)
	 | (X64, 121) -> (* getpgid *)
	     let ebx = read_1_reg () in
	     let pid = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "getpgid()";
	       self#sys_getpgid pid
	 | (ARM, 133) -> uh "Check whether ARM fchdir syscall matches x86"
	 | (X86, 133)   (* fchdir *)
	 | (X64, 81) -> (* fchdir *)
	     let ebx = read_1_reg () in
	     let fd = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "fchdir(%d)" fd;
	       self#sys_fchdir fd
	 | ((X86|ARM), 134) -> (* bdflush *)
	     uh "Unhandled Linux system call bdflush (134)"
	 | ((X86|ARM), 135) -> (* sysfs *)
	     uh "Unhandled Linux system call sysfs (135)"
	 | (X64, 139) -> (* sysfs *)
	     uh "Unhandled Linux/x64 system call sysfs (139)"
	 | ((X86|ARM), 136) -> (* personality *)
	     uh "Unhandled Linux system call personality (136)"
	 | (X64, 135) -> (* personality *)
	     uh "Unhandled Linux/x64 system call personality (135)"
	 | (ARM, 137) -> uh "No afs_syscall syscall in Linux/ARM (E)ABI"
	 | (X86, 137) -> (* afs_syscall *)
	     uh "Unhandled Linux system call afs_syscall (137)"
	 | (X64, 183) -> (* afs_syscall *)
	     uh "Unhandled Linux/x64 system call afs_syscall (183)"
	 | ((X86|ARM), 138) -> (* setfsuid *)
	     uh "Unhandled Linux system call setfsuid (138)"
	 | (X64, 122) -> (* setfsuid *)
	     uh "Unhandled Linux/x64 system call setfsuid (122)"
	 | ((X86|ARM), 139) -> (* setfsgid *)
	     uh "Unhandled Linux system call setfsgid (139)"
	 | (X64, 123) -> (* setfsgid *)
	     uh "Unhandled Linux/x64 system call setfsgid (123)"
	 | ((X86|ARM), 140) -> (* _llseek *)
	     let (arg1, arg2, arg3, arg4, arg5) = read_5_regs () in
	     let fd = Int64.to_int arg1 and
		 off_high = arg2 and
		 off_low = arg3 and
		 resultp = arg4 and
		 whence = Int64.to_int arg5 in
	     let offset = assemble64 off_high off_low in
	       if !opt_trace_syscalls then
		 Printf.printf "_llseek(%d, %Ld, 0x%08Lx, %d)"
		   fd offset resultp whence;
	       self#sys__llseek fd offset resultp whence
	 | (ARM, 141) -> uh "Check whether ARM getdents syscall matches x86"
	 | (X86, 141)    (* getdents *)
	 | (X64,  78) -> (* getdents *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let fd = Int64.to_int ebx and
		 dirp = ecx and
		 count = Int64.to_int edx in
	       if !opt_trace_syscalls then
		 Printf.printf "getdents(%d, 0x%08Lx, %d)" fd dirp count;
	       self#sys_getdents fd dirp count
	 | (ARM, 142) -> uh "Check whether ARM _newselect (142) matches x86"
	 | (X86, 142) -> (* _newselect *)
	     let (ebx, ecx, edx, esi, edi) = read_5_regs () in
	     let nfds = Int64.to_int ebx and
		 readfds = ecx and
		 writefds = edx and
		 exceptfds = esi and
		 timeout = edi in
	       if !opt_trace_syscalls then
		 Printf.printf "select(%d, 0x%08Lx, 0x%08Lx, 0x%08Lx, 0x%08Lx)"
		   nfds readfds writefds exceptfds timeout;
	       self#sys_select nfds readfds writefds exceptfds timeout
	 | ((X86|ARM), 143) -> (* flock *)
	     uh "Unhandled Linux system call flock (143)"
	 | (X64, 73) -> (* flock *)
	     uh "Unhandled Linux/x64 system call flock (73)"
	 | ((X86|ARM), 144) -> (* msync *)
	     uh "Unhandled Linux system call msync (144)"
	 | (X64, 26) -> (* msync *)
	     uh "Unhandled Linux/x64 system call msync (26)"
	 | ((X86|ARM), 145) (* readv *)
	 | (X64, 19)     -> (* readv *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd  = Int64.to_int arg1 and
		 iov = arg2 and
		 cnt = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "readv(%d, 0x%08Lx, %d)" fd iov cnt;
	       self#sys_readv fd iov cnt
	 | ((X86|ARM), 146) (* writev *)
	 | (X64, 20) ->     (* writev *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd  = Int64.to_int arg1 and
		 iov = arg2 and
		 cnt = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "writev(%d, 0x%08Lx, %d)" fd iov cnt;
	       self#sys_writev fd iov cnt
	 | (ARM, 147) -> uh "Check whether ARM getsid syscall matches x86"
	 | (X86, 147)    (* getsid *)
	 | (X64, 124) -> (* getsid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getsid()";
	     self#sys_getsid ()
	 | ((X86|ARM), 148) -> (* fdatasync *)
	     uh "Unhandled Linux system call fdatasync (148)"
	 | (X64, 75) -> (* fdatasync *)
	     uh "Unhandled Linux/x64 system call fdatasync (75)"
	 | ((X86|ARM), 149) -> (* _sysctl *)
	     uh "Unhandled Linux system call _sysctl (149)"
	 | (X64, 156) -> (* _sysctl *)
	     uh "Unhandled Linux/x64 system call _sysctl (156)"
	 | ((X86|ARM), 150) -> (* mlock *)
	     uh "Unhandled Linux system call mlock (150)"
	 | (X64, 149) -> (* mlock *)
	     uh "Unhandled Linux/x64 system call mlock (149)"
	 | ((X86|ARM), 151) -> (* munlock *)
	     uh "Unhandled Linux system call munlock (151)"
	 | (X64, 150) -> (* munlock *)
	     uh "Unhandled Linux/x64 system call munlock (150)"
	 | ((X86|ARM), 152) -> (* mlockall *)
	     uh "Unhandled Linux system call mlockall (152)"
	 | (X64, 151) -> (* mlockall *)
	     uh "Unhandled Linux/x64 system call mlockall (151)"
	 | ((X86|ARM), 153) -> (* munlockall *)
	     uh "Unhandled Linux system call munlockall (153)"
	 | (X64, 152) -> (* munlockall *)
	     uh "Unhandled Linux/x64 system call munlockall (152)"
	 | ((X86|ARM), 154) -> (* sched_setparam *)
	     uh "Unhandled Linux system call sched_setparam (154)"
	 | (X64, 142) -> (* sched_setparam *)
	     uh "Unhandled Linux/x64 system call sched_setparam (142)"
	 | (ARM, 155) -> uh "Check whether ARM sched_getparam matches x86"
	 | (X64, 143) -> uh "Check whether x64 sched_getparam matches x86"
	 | (X86, 155) -> (* sched_getparam *)
	     let (ebx, ecx) = read_2_regs () in
	     let pid = Int64.to_int ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_getparam(%d, 0x%08Lx)" pid buf;
	       self#sys_sched_getparam pid buf
	 | ((X86|ARM), 156) -> (* sched_setscheduler *)
	     uh "Unhandled Linux system call sched_setscheduler (156)"
	 | (X64, 144) -> (* sched_setscheduler *)
	     uh "Unhandled Linux/x64 system call sched_setscheduler (144)"
	 | (ARM, 157) -> uh "Check whether ARM sched_getscheduler matches x86"
	 | (X64, 145) -> uh "Check whether x64 sched_getscheduler matches x86"
	 | (X86, 157) -> (* sched_getscheduler *)
	     let ebx = read_1_reg () in
	     let pid = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_getscheduler(%d)" pid;
	       self#sys_sched_getscheduler pid
	 | ((X86|ARM), 158) -> (* sched_yield *)
	     uh "Unhandled Linux system call sched_yield (158)"
	 | (X64, 24) -> (* sched_yield *)
	     uh "Unhandled/x64 Linux system call sched_yield (24)"
	 | (ARM, 159) -> uh "Check whether ARM sched_get_priority_max matches x86"
	 | (X64, 146) -> uh "Check whether x64 sched_get_priority_max matches x86"
	 | (X86, 159) -> (* sched_get_priority_max *)
	     let ebx = read_1_reg () in
	     let policy = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_get_priority_max(%d)" policy;
	       self#sys_sched_get_priority_max policy
	 | (ARM, 160) -> uh "Check whether ARM sched_get_priority_min matches x86"
	 | (X64, 147) -> uh "Check whether x64 sched_get_priority_min syscall matches x86"
	 | (X86, 160) -> (* sched_get_priority_min *)
	     let ebx = read_1_reg () in
	     let policy = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_get_priority_min(%d)" policy;
	       self#sys_sched_get_priority_min policy
	 | ((X86|ARM), 161) -> (* sched_rr_get_interval *)
	     uh "Unhandled Linux system call sched_rr_get_interval (161)"
	 | (X64, 148) -> (* sched_rr_get_interval *)
	     uh "Unhandled Linux/x64 system call sched_rr_get_interval (148)"
	 | ((X86|ARM), 162)
	 | (X64, 35) -> (* nanosleep *)
	     let (req_addr, rem_addr) = read_2_regs () in
	       if !opt_trace_syscalls then
		 Printf.printf "nanosleep(0x%08Lx, 0x%08Lx)" req_addr rem_addr;
	       self#sys_nanosleep req_addr rem_addr
	 | ((X86|ARM), 163) (* mremap *)
	 | (X64, 25) -> (* mremap *)
	     let (arg1, arg2, arg3, arg4, arg5) = read_5_regs () in
	     let old_addr = arg1 and
		 old_size = arg2 and
		 new_size = arg3 and
		 flags = Int64.to_int arg4 and
		 new_addr = arg5 in
	       if !opt_trace_syscalls then
		 Printf.printf "mremap(0x%08Lx, %Ld, %Ld, %d, 0x%08Lx)"
		   old_addr old_size new_size flags new_addr;
	       self#sys_mremap old_addr old_size new_size flags new_addr;
	 | ((X86|ARM), 164) -> (* setresuid *)
	     uh "Unhandled Linux system call setresuid (164)"
	 | ((X86|ARM), 165) -> (* getresuid *)
	     uh "Unhandled Linux system call getresuid (165)"
	 | (ARM, 166) -> uh "No vm86 syscall in Linux/ARM (E)ABI"
	 | (X86, 166) -> (* vm86 *)
	     uh "Unhandled Linux system call vm86 (166)"
	 | (ARM, 167) -> uh "No query_module syscall in Linux/ARM (E)ABI"
	 | (X86, 167) -> (* query_module *)
	     uh "Unhandled Linux system call query_module (167)"
	 | (X64, 178) -> (* query_module *)
	     uh "Unhandled Linux/x64 system call query_module (178)"
	 | ((X86|ARM), 168)  (* poll *)
	 | (X64, 7) ->       (* poll *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fds_buf = arg1 and
		 nfds = Int64.to_int arg2 and
		 timeout = arg3 in
	       if !opt_trace_syscalls then	
		 Printf.printf "poll(0x%08Lx, %d, %Ld)%!" fds_buf nfds timeout;
	       self#sys_poll fds_buf nfds timeout
	 | ((X86|ARM), 169) -> (* nfsservctl *)
	     uh "Unhandled Linux system call nfsservctl (169)"
	 | (X64, 180) -> (* nfsservctl *)
	     uh "Unhandled Linux/x64 system call nfsservctl (180)"
	 | ((X86|ARM), 170) -> (* setresgid *)
	     uh "Unhandled Linux system call setresgid (170)"
	 | ((X86|ARM), 171) -> (* getresgid *)
	     uh "Unhandled Linux system call getresgid (171)"
	 | ((X86|ARM), 172) -> (* prctl *)
	     uh "Unhandled Linux system call prctl (172)"
	 | (X64, 157) -> (* prctl *)
	     uh "Unhandled Linux/x64 system call prctl (157)"
	 | (X86, 384)    (* arch_prctl *)
	 | (X64, 158) -> (* arch_prctl *)
	     let (arg1, arg2) = read_2_regs () in
	     let code = Int64.to_int arg1 and
		 addr = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "arch_prctl(0x%x, 0x%08Lx)" code addr;
	       self#sys_arch_prctl code addr
	 | ((X86|ARM), 173) -> (* rt_sigreturn *)
	     uh "Unhandled Linux system call rt_sigreturn (173)"
	 | (X64, 15) -> (* rt_sigreturn *)
	     uh "Unhandled Linux/x64 system call rt_sigreturn (15)"
	 | ((X86|ARM), 174) (* rt_sigaction *)
	 | (X64, 13) -> (* rt_sigaction *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let signum = Int64.to_int arg1 and
		 newbuf = arg2 and
		 oldbuf = arg3 and
		 setlen = Int64.to_int arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "rt_sigaction(%d, 0x%08Lx, 0x%08Lx, %d)"
		   signum newbuf oldbuf setlen;
	       self#sys_rt_sigaction signum newbuf oldbuf setlen
	 | ((X86|ARM), 175) (* rt_sigprocmask *)
	 | (X64, 14) -> (* rt_sigprocmask *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let how    = Int64.to_int arg1 and
		 newset = arg2 and
		 oldset = arg3 and
		 setlen = Int64.to_int arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "rt_sigprocmask(%d, 0x%08Lx, 0x%08Lx, %d)"
		   how newset oldset setlen;
	       self#sys_rt_sigprocmask how newset oldset setlen
	 | ((X86|ARM), 176) -> (* rt_sigpending *)
	     uh "Unhandled Linux system call rt_sigpending (176)"
	 | (X64, 127) -> (* rt_sigpending *)
	     uh "Unhandled Linux/x64 system call rt_sigpending (127)"
	 | ((X86|ARM), 177) -> (* rt_sigtimedwait *)
	     uh "Unhandled Linux system call rt_sigtimedwait (177)"
	 | (X64, 128) -> (* rt_sigtimedwait *)
	     uh "Unhandled Linux/x64 system call rt_sigtimedwait (128)"
	 | ((X86|ARM), 178) -> (* rt_sigqueueinfo *)
	     uh "Unhandled Linux system call rt_sigqueueinfo (178)"
	 | (X64, 129) -> (* rt_sigqueueinfo *)
	     uh "Unhandled Linux/x64 system call rt_sigqueueinfo (129)"
	 | ((X86|ARM), 179) -> (* rt_sigsuspend *)
	     uh "Unhandled Linux system call rt_sigsuspend (179)"
	 | (X64, 130) -> (* rt_sigsuspend *)
	     uh "Unhandled Linux/x64 system call rt_sigsuspend (130)"
	 | ((X86|ARM), 180) -> (* pread64, 32 bit *)
	     let (arg1, arg2, arg3, arg4, arg5) = read_5_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 and
		 off   = Int64.logor (Int64.shift_left arg5 32) arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "pread64(%d, 0x%08Lx, %d, %Ld)"
		   fd buf count off;
	       self#sys_pread64 fd buf count off;
	 | (X64, 17) -> (* pread64, 64-bit *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 and
		 off   = arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "pread64(%d, 0x%08Lx, %d, %Ld)"
		   fd buf count off;
	       self#sys_pread64 fd buf count off;
	 | ((X86|ARM), 181) -> (* pwrite64 *)
	     uh "Unhandled Linux system call pwrite64 (181)"
	 | (X64, 18) ->  (* pwrite64 *)
	     uh "Unhandled Linux/x64 ststem call pwrite64 (18)"
	 | ((X86|ARM), 182) -> (* chown *)
	     uh "Unhandled Linux system call chown (182)"
	 | (X64, 92) -> (* chown *)
	     uh "Unhandled Linux/x64 system call chown (92)"
	 | (ARM, 183) -> uh "Check whether ARM getcwd syscall matches x86"
	 | (X64,  79) -> uh "Check whether x64 getcwd syscall matches x86"
	 | (X86, 183) -> (* getcwd *)
	     let (ebx, ecx) = read_2_regs () in
	     let buf = ebx and
		 size = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "getcwd(0x%08Lx, %d)" buf size;
	       self#sys_getcwd buf size
	 | (ARM, 184) -> uh "Check whether ARM capget syscall matches x86"
	 | (X86, 184)    (* capget *)
	 | (X64, 125) -> (* capget *)
	     let (ebx, ecx) = read_2_regs () in
	     let hdrp = ebx and
		 datap = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "capget(0x%08Lx, 0x%08Lx)" hdrp datap;
	       self#sys_capget hdrp datap
	 | ((X86|ARM), 185) -> (* capset *)
	     uh "Unhandled Linux system call capset (185)"
	 | (X64, 126) -> (* capset *)
	     uh "Unhandled Linux/x64 system call capset (126)"
	 | (X64, 131) -> uh "Check whether x64 sigaltstack syscall matches x86"
	 | ((X86|ARM), 186) -> (* sigaltstack *)
	     let (arg1, arg2) = read_2_regs () in
	     let new_stack_t = arg1 and
		 old_stack_t = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "sigaltstack(0x%08Lx, 0x%08Lx)"
		   new_stack_t old_stack_t;
	       self#sys_sigaltstack new_stack_t old_stack_t
	 | ((X86|ARM), 187) -> (* sendfile *)
	     uh "Unhandled Linux system call sendfile (187)"
	 | (X64, 40) -> (* sendfile *)
	     uh "Unhandled Linux/x64 system call sendfile (40)"
	 | (ARM, 188) -> uh "No getpmsg (188) syscall in Linux/ARM (E)ABI"
	 | (X86, 188) -> (* getpmsg *)
	     uh "Unhandled Linux system call getpmsg (188)"
	 | (X64, 181) -> (* getpmsg *)
	     uh "Unhandled Linux/x64 system call getpmsg (181)"
	 | (ARM, 189) -> uh "No putpmsg (189) syscall in Linux/ARM (E)ABI"
	 | (X86, 189) -> (* putpmsg *)
	     uh "Unhandled Linux system call putpmsg (189)"
	 | (X64, 182) -> (* putpmsg *)
	     uh "Unhandled Linux/x64 system call putpmsg (182)"
	 | ((X86|ARM), 190) -> (* vfork *)
	     uh "Unhandled Linux system call vfork (190)"
	 | (X64, 58) -> (* vfork *)
	     uh "Unhandled Linux/x64 system call vfork (58)"
	 | ((X86|ARM), 191) -> (* ugetrlimit *)
	     let (arg1, arg2) = read_2_regs () in
	     let rsrc = Int64.to_int arg1 and
		 buf  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "getrlimit(%d, 0x%08Lx)" rsrc buf;
	       self#sys_getrlimit rsrc buf
	 | ((X86|ARM), 192) -> (* mmap2 *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let addr     = arg1 and
		 length   = arg2 and
		 prot     = Int64.to_int arg3 and
		 flags    = Int64.to_int arg4 and
		 fd       = Int64.to_int (fix_s32 arg5) and
		 pgoffset = arg6 in
	       if !opt_trace_syscalls then
		 Printf.printf "mmap2(0x%08Lx, %Ld, 0x%x, 0x%0x, %d, %Ld)"
		   addr length prot flags fd pgoffset;
	       self#sys_mmap2 addr length prot flags fd pgoffset
	 | ((X86|ARM), 193) -> (* truncate64 *)
	     uh "Unhandled Linux system call truncate64 (193)"
	 | ((X86|ARM), 194) -> (* ftruncate64 *)
	     uh "Unhandled Linux system call ftruncate64 (194)"
	 | ((X86|ARM), 195) -> (* stat64 *)
	     let (arg1, arg2) = read_2_regs () in
	     let path_buf = arg1 and
		 buf_addr = arg2 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "stat64(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_stat64 path buf_addr
	 | ((X86|ARM), 196) -> (* lstat64 *)
	     let (arg1, arg2) = read_2_regs () in
	     let path_buf = arg1 and
		 buf_addr = arg2 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "lstat64(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_lstat64 path buf_addr
	 | ((X86|ARM), 197) -> (* fstat64 *)
	     let (arg1, arg2) = read_2_regs () in
	     let fd = Int64.to_int arg1 and
		 buf_addr = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "fstat64(%d, 0x%08Lx)" fd buf_addr;
	       self#sys_fstat64 fd buf_addr
	 | ((X86|ARM), 198) -> (* lchown32 *)
	     uh "Unhandled Linux system call lchown32 (198)"
	 | ((X86|ARM), 199) -> (* getuid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "getuid32()";
	     self#sys_getuid32 ()
	 | ((X86|ARM), 200) -> (* getgid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "getgid32()";
	     self#sys_getgid32 ()
	 | ((X86|ARM), 201) -> (* geteuid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "geteuid32()";
	     self#sys_geteuid32 ()
	 | ((X86|ARM), 202) -> (* getegid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "getegid32()";
	     self#sys_getegid32 ()
	 | ((X86|ARM), 203) -> (* setreuid32 *)
	     uh "Unhandled Linux system call setreuid32 (203)"
	 | ((X86|ARM), 204) -> (* setregid32 *)
	     uh "Unhandled Linux system call setregid32 (204)"
	 | (ARM, 205) -> uh "Check whether ARM getgroups32 syscall matches x86"
	 | (X86, 205) -> (* getgroups32 *)
	     let (ebx, ecx) = read_2_regs () in
	     let size = Int64.to_int ebx and
		 list = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "getgroups32(%d, 0x%08Lx)" size list;
	       self#sys_getgroups32 size list
	 | ((X86|ARM), 206) -> (* setgroups32 *)
         let (ebx, ecx) = read_2_regs () in
         let size = Int64.to_int ebx and
         list = ecx in
         if !opt_trace_syscalls then
           Printf.printf "setgroups32(%d, 0x%08Lx)" size list;
         self#sys_setgroups32 size list
	 | (ARM, 207) -> uh "Check whether ARM fchown32 syscall matches x86"
	 | (X86, 207) -> (* fchown32 *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let fd = Int64.to_int ebx and
		 user = Int64.to_int ecx and
		 group = Int64.to_int edx in
	       if !opt_trace_syscalls then
		 Printf.printf "fchown32(%d, %d, %d)" fd user group;
	       self#sys_fchown32 fd user group
	 | ((X86|ARM), 208) -> (* setresuid32 *)
         let (ebx, ecx, edx) = read_3_regs () in
         let ruid = Int64.to_int ebx and
         euid = Int64.to_int ecx and
         suid = Int64.to_int edx in
         if !opt_trace_syscalls then
           Printf.printf "setresuid32(%d, %d, %d)" ruid euid suid;
         self#sys_setresuid32 ruid euid suid
	 | (ARM, 209) -> uh "Check whether ARM getresuid32 syscall matches x86"
	 | (X86, 209) -> (* getresuid32 *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let ruid_ptr = ebx and
		 euid_ptr = ecx and
		 suid_ptr = edx in
	     if !opt_trace_syscalls then
	       Printf.printf "getresuid32(0x%08Lx, 0x%08Lx, 0x%08Lx)"
		 ruid_ptr euid_ptr suid_ptr;
	     self#sys_getresuid32 ruid_ptr euid_ptr suid_ptr;
	 | ((X86|ARM), 210) -> (* setresgid32 *)
         let (ebx, ecx, edx) = read_3_regs () in
         let ruid = Int64.to_int ebx and
             euid = Int64.to_int ecx and
             suid = Int64.to_int edx in
         if !opt_trace_syscalls then
           Printf.printf "setresgid32(0x%d, 0x%d, 0x%d)"
             ruid euid suid;
         self#sys_setresgid32 ruid euid suid;
	 | (ARM, 211) -> uh "Check whether ARM getresgid32 syscall matches x86"
	 | (X86, 211) -> (* getresgid32 *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let rgid_ptr = ebx and
		 egid_ptr = ecx and
		 sgid_ptr = edx in
	     if !opt_trace_syscalls then
	       Printf.printf "getresgid32(0x%08Lx, 0x%08Lx, 0x%08Lx)"
		 rgid_ptr egid_ptr sgid_ptr;
	     self#sys_getresgid32 rgid_ptr egid_ptr sgid_ptr;
	 | ((X86|ARM), 212) -> (* chown32 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let path = fm#read_cstr arg1 and
		 uid = Int64.to_int arg2 and
		 gid = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "chown32(\"%s\", %d, %d)" path uid gid;
	       self#sys_chown path uid gid
	 | (ARM, 213) -> uh "Check whether ARM setuid32 syscall matches x86"
	 | (X86, 213) -> (* setuid32 *)
	     let ebx = read_1_reg () in
	     let uid = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "setuid32(%d)" uid;
	       self#sys_setuid32 uid
	 | (ARM, 214) -> uh "Check whether ARM setgid32 syscall matches x86"
	 | (X86, 214) -> (* setgid32 *)
	     let ebx = read_1_reg () in
	     let gid = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "setgid32(%d)" gid;
	       self#sys_setgid32 gid
	 | ((X86|ARM), 215) -> (* setfsuid32 *)
	     uh "Unhandled Linux system call setfsuid32 (215)"
	 | ((X86|ARM), 216) -> (* setfsgid32 *)
	     uh "Unhandled Linux system call setfsgid32 (216)"
	 | (ARM, 218)
	 | (X86, 217) -> (* pivot_root *)
	     uh "Unhandled Linux system call pivot_root"
	 | (X64, 155) -> (* pivot_root *)
	     uh "Unhandled Linux/x64 system call pivot_root (155)"
	 | (ARM, 219) -> uh "Check whether ARM mincore syscall matches x86"
	 | (X64, 27) -> uh "Check whether x64 mincore syscall matches x86"
	 | (X86, 218) -> (* mincore *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let addr = ebx and
		 length = Int64.to_int ecx and
		 vec = edx in
	       if !opt_trace_syscalls then
		 Printf.printf "mincore(0x%08Lx, %d, 0x%08Lx)" addr length vec;
	       self#sys_mincore addr length vec
	 | (ARM, 220)
	 | (X86, 219)
	 | (X64, 28) -> (* madvise *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let addr = arg1 and
		 length = arg2 and
		 advice = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "madvise(0x%08Lx, %Ld, %d)" addr length advice;
	       self#sys_madvise addr length advice
	 | (X64, 217) -> uh "Check whether x64 getdents64 syscall matches x86"
	 | (ARM, 217)
	 | (X86, 220) -> (* getdents64 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd = Int64.to_int arg1 and
		 dirp = arg2 and
		 count = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "getdents64(%d, 0x%08Lx, %d)" fd dirp count;
	       self#sys_getdents64 fd dirp count
	 | ((X86|ARM), 221) -> (* fcntl64 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd = Int64.to_int arg1 and
		 cmd = Int64.to_int arg2 and
		 arg = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "fcntl64(%d, %d, 0x%08Lx)" fd cmd arg;
	       self#sys_fcntl64 fd cmd arg
	 | ((X86|ARM), 222) -> uh "No such Linux syscall 222 (was for tux)"
	 | (X64, 184) -> (* tuxcall *)
	     uh "Unhandled Linux/x64 system call tuxcall (184)"
	 | ((X86|ARM), 223) -> uh "No such Linux syscall 223 (unused)"
	 | (X64, 185) -> (* security *)
	     uh "Unhandled Linux/x64 system call security (185)"
	 | (ARM, 224) -> uh "Check whether ARM gettid syscall matches x86"
	 | (X86, 224)    (* gettid *)
	 | (X64, 186) -> (* gettid *)
	     if !opt_trace_syscalls then
	       Printf.printf "gettid()";
	     self#sys_gettid 
	 | ((X86|ARM), 225) -> (* readahead *)
	     uh "Unhandled Linux system call readahead (225)"
	 | (X64, 187) -> (* readahead *)
	     uh "Unhandled Linux/x64 system call readahead (187)"
	 | ((X86|ARM), 226) -> (* setxattr *)
	     uh "Unhandled Linux system call setxattr (226)"
	 | (X64, 188) -> (* setxattr *)
	     uh "Unhandled Linux/x64 system call setxattr (188)"
	 | ((X86|ARM), 227) -> (* lsetxattr *)
	     uh "Unhandled Linux system call lsetxattr (227)"
	 | (X64, 189) -> (* lsetxattr *)
	     uh "Unhandled Linux/x64 system call lsetxattr (189)"
	 | ((X86|ARM), 228) -> (* fsetxattr *)
	     uh "Unhandled Linux system call fsetxattr (228)"
	 | (X64, 190) -> (* fsetxattr *)
	     uh "Unhandled Linux/x64 system call fsetxattr (190)"
	 | (ARM, 229) -> uh "Check whether ARM getxattr syscall matches x86"
	 | (X64, 191) -> uh "Check whether x64 getxattr syscall matches x86"
	 | (X86, 229) -> (* getxattr *)
	     let (ebx, ecx, edx, esi) = read_4_regs () in
	     let path_ptr = ebx and
		 name_ptr = ecx and
		 value_ptr = edx and
		 size = Int64.to_int esi in
	     let path = fm#read_cstr path_ptr and
		 name = fm#read_cstr name_ptr in
	       if !opt_trace_syscalls then
		 Printf.printf "getxattr(\"%s\", \"%s\", 0x%08Lx, %d)"
		   path name value_ptr size;
	       self#sys_getxattr path name value_ptr size
	 | (X64, 192) -> uh "Check whether x64 lgetxattr syscall matches x86"
	 | ((X86|ARM), 230) -> (* lgetxattr *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let path_ptr = arg1 and
		 name_ptr = arg2 and
		 value_ptr = arg3 and
		 size = Int64.to_int arg4 in
	     let path = fm#read_cstr path_ptr and
		 name = fm#read_cstr name_ptr in
	       if !opt_trace_syscalls then
		 Printf.printf "lgetxattr(\"%s\", \"%s\", 0x%08Lx, %d)"
		   path name value_ptr size;
	       self#sys_lgetxattr path name value_ptr size
	 | ((X86|ARM), 231) -> (* fgetxattr *)
	     uh "Unhandled Linux system call fgetxattr (231)"
	 | (X64, 193) -> (* fgetxattr *)
	     uh "Unhandled Linux/x64 system call fgetxattr (193)"
	 | ((X86|ARM), 232) -> (* listxattr *)
	     uh "Unhandled Linux system call listxattr (232)"
	 | (X64, 194) -> (* listxattr *)
	     uh "Unhandled Linux/x64 system call listxattr (194)"
	 | ((X86|ARM), 233) -> (* llistxattr *)
	     uh "Unhandled Linux system call llistxattr (233)"
	 | (X64, 195) -> (* llistxattr *)
	     uh "Unhandled Linux/x64 system call llistxattr (195)"
	 | ((X86|ARM), 234) -> (* flistxattr *)
	     uh "Unhandled Linux system call flistxattr (234)"
	 | (X64, 196) -> (* flistxattr *)
	     uh "Unhandled Linux/x64 system call flistxattr (196)"
	 | ((X86|ARM), 235) -> (* removexattr *)
	     uh "Unhandled Linux system call removexattr (235)"
	 | (X64, 197) -> (* removexattr *)
	     uh "Unhandled Linux/x64 system call removexattr (197)"
	 | ((X86|ARM), 236) -> (* lremovexattr *)
	     uh "Unhandled Linux system call lremovexattr (236)"
	 | (X64, 198) -> (* lremovexattr *)
	     uh "Unhandled Linux/x64 system call lremovexattr (198)"
	 | ((X86|ARM), 237) -> (* fremovexattr *)
	     uh "Unhandled Linux system call fremovexattr (237)"
	 | (X64, 199) -> (* fremovexattr *)
	     uh "Unhandled Linux/x64 system call fremovexattr (199)"
	 | ((X86|ARM), 238) -> (* tkill *)
	     uh "Unhandled Linux system call tkill (238)"
	 | (X64, 200) -> (* tkill *)
	     uh "Unhandled Linux/x64 system call tkill (200)"
	 | ((X86|ARM), 239) -> (* sendfile64 *)
	     uh "Unhandled Linux system call sendfile64 (239)"
	 | ((X86|ARM), 240) (* futex *)
	 | (X64, 202) -> (* futex *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let uaddr    = arg1 and
		 op       = Int64.to_int arg2 and
		 value    = arg3 and
		 timebuf  = arg4 and
		 uaddr2   = arg5 and
		 val3     = arg6 in
	       if !opt_trace_syscalls then
		 Printf.printf "futex(0x%08Lx, %d, %Ld, 0x%08Lx, 0x%08Lx, %Ld)"
		   uaddr op value timebuf uaddr2 val3;
	       self#sys_futex uaddr op value timebuf uaddr2 val3
	 | ((X86|ARM), 241) -> (* sched_setaffinity *)
	     uh "Unhandled Linux system call sched_setaffinity (241)"
	 | (X64, 203) -> (* sched_setaffinity *)
	     uh "Unhandled Linux/x64 system call sched_setaffinity (203)"
	 | ((X86|ARM), 242) -> (* sched_getaffinity *)
	     uh "Unhandled Linux system call sched_getaffinity (242)"
	 | (X64, 204) -> (* sched_getaffinity *)
	     uh "Unhandled Linux/x64 system call sched_getaffinity (204)"
	 (* Here's where the x86 and ARM syscall numbers diverge,
	    because ARM lacks {get,set}_thread_area *)
	 | (X86, 243) -> (* set_thread_area *)
	     let ebx = read_1_reg () in
	     let uinfo = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "set_thread_area(0x%08Lx)" uinfo;
	       self#sys_set_thread_area uinfo
	 | (X64, 205) -> (* set_thread_area *)
	     uh "Unhandled Linux/x64 system call set_thread_area (205)"
	 | (X86, 244) -> (* get_thread_area *)
	     uh "Unhandled Linux/x86 system call get_thread_area (244)"
	 | (X64, 211) -> (* get_thread_area *)
	     uh "Unhandled Linux/x64 system call get_thread_area (211)"
	 | (ARM, 243)    (* io_setup *)
	 | (X64, 206)    (* io_setup *)
	 | (X86, 245) -> (* io_setup *)
	     uh "Unhandled Linux system call io_setup"
	 | (ARM, 244)    (* io_destroy *)
	 | (X64, 207)    (* io_destroy *)
	 | (X86, 246) -> (* io_destroy *)
	     uh "Unhandled Linux system call io_destroy"
	 | (ARM, 245)    (* io_getevents *)
	 | (X64, 208)    (* io_getevents *)
	 | (X86, 247) -> (* io_getevents *)
	     uh "Unhandled Linux system call io_getevents"
	 | (ARM, 246)    (* io_submit *)
	 | (X64, 209)    (* io_submit *)
	 | (X86, 248) -> (* io_submit *)
	     uh "Unhandled Linux system call io_submit"
	 | (ARM, 247)    (* io_cancel *)
	 | (X64, 210)    (* io_cancel *)
	 | (X86, 249) -> (* io_cancel *)
	     uh "Unhandled Linux system call io_cancel"
	 | (X86, 250) -> (* fadvise64 *)
	     uh "Unhandled Linux system call fadvise64 (250)"
	 | (X64, 221) -> (* fadvise64 *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let fd = Int64.to_int arg1 and
		 offset = arg2 and
		 len = arg3 and
		 advice = Int64.to_int arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "fadvise64(%d, %Ld, %Ld, %d)"
		   fd offset len advice;
	       self#sys_fadvise64_64 fd offset len advice
	 | (ARM, 248)    (* exit_group *)
	 | (X86, 252)    (* exit_group *)
	 | (X64, 231) -> (* exit_group *)
	     let arg1 = read_1_reg () in
	     let status = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "exit_group(%Ld) (no return)\n" status;
	       self#sys_exit_group status
	 | (ARM, 249)    (* lookup_dcookie *)
	 | (X64, 212)    (* lookup_dcookie *)
	 | (X86, 253) -> (* lookup_dcookie *)
	     uh "Unhandled Linux system call lookup_dcookie"
	 | (ARM, 250)    (* epoll_create *)
	 | (X64, 213)    (* epoll_create *)
	 | (X86, 254) -> (* epoll_create *)
	     uh "Unhandled Linux system call epoll_create"
	 | (X64, 214) -> (* epoll_ctl_old *)
	     uh "Unhandled Linux/x64 system call epoll_ctl_old (214)"
	 | (ARM, 251)    (* epoll_ctl *)
	 | (X64, 233)    (* epoll_ctl *)
	 | (X86, 255) -> (* epoll_ctl *)
	     uh "Unhandled Linux system call epoll_ctl"
	 | (X64, 215) -> (* epoll_wait_old *)
	     uh "Unhandled Linux/x64 system call epoll_wait_old (215)"
	 | (ARM, 252)    (* epoll_wait *)
	 | (X64, 232)    (* epoll_wait *)
	 | (X86, 256) -> (* epoll_wait *)
	     uh "Unhandled Linux system call epoll_wait"
	 | (ARM, 253)    (* remap_file_pages *)
	 | (X64, 216)    (* remap_file_pages *)
	 | (X86, 257) -> (* remap_file_pages *)
	     uh "Unhandled Linux system call remap_file_pages"
	 | (ARM, 254) -> uh "No set_thread_area (254) syscall in Linux/ARM (E)ABI"
	 | (ARM, 255) -> uh "No get_thread_area (255) syscall in Linux/ARM (E)ABI"
	 | (ARM, 256)
	 | (X86, 258) (* set_tid_address *)
	 | (X64, 218) -> (* set_tid_address *)
	     let arg1 = read_1_reg () in
	     let addr = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "set_tid_address(0x%08Lx)" addr;
	       self#sys_set_tid_address addr
	 | (ARM, 257)    (* timer_create *)
	 | (X64, 222)    (* timer_create *)
	 | (X86, 259) -> (* timer_create *)
	     uh "Unhandled Linux system call timer_create"
	 | (ARM, 258)    (* timer_settime *)
	 | (X64, 223)    (* timer_settime *)
	 | (X86, 260) -> (* timer_settime *)
	     uh "Unhandled Linux system call timer_settime"
	 | (ARM, 259)    (* timer_gettime *)
	 | (X64, 224)    (* timer_gettime *)
	 | (X86, 261) -> (* timer_gettime *)
	     uh "Unhandled Linux system call timer_gettime"
	 | (ARM, 260)    (* timer_getoverrun *)
	 | (X64, 225)    (* timer_getoverrun *)
	 | (X86, 262) -> (* timer_getoverrun *)
	     uh "Unhandled Linux system call timer_getoverrun"
	 | (ARM, 261)    (* timer_delete *)
	 | (X64, 226)    (* timer_delete *)
	 | (X86, 263) -> (* timer_delete *)
	     uh "Unhandled Linux system call timer_delete"
	 | (ARM, 262)    (* clock_settime *)
	 | (X64, 227)    (* clock_settime *)
	 | (X86, 264) -> (* clock_settime *)
	     uh "Unhandled Linux system call clock_settime"
	 | (ARM, 263)    (* clock_gettime *)
	 | (X64, 228)    (* clock_gettime *)
	 | (X86, 265) -> (* clock_gettime *)
	     let (arg1, arg2) = read_2_regs () in
	     let clkid = Int64.to_int arg1 and
		 timep = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "clock_gettime(%d, 0x%08Lx)" clkid timep;
	       self#sys_clock_gettime clkid timep
	 | (ARM, 264) -> uh "Check whether ARM clock_getres matches x86"
	 | (X64, 229) -> uh "Check whether x64 clock_getres matches x86"
	 | (X86, 266) -> (* clock_getres *)
	     let (ebx, ecx) = read_2_regs () in
	     let clkid = Int64.to_int ebx and
		 timep = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "clock_getres(%d, 0x%08Lx)" clkid timep;
	       self#sys_clock_getres clkid timep
	 | (ARM, 265)    (* clock_nanosleep *)
	 | (X64, 230)    (* clock_nanosleep *)
	 | (X86, 267) -> (* clock_nanosleep *)
	     uh "Unhandled Linux system call clock_nanosleep"
	 | (ARM, 266)
	 | (X86, 268) -> (* statfs64 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let path_buf = arg1 and
		 buf_len = Int64.to_int arg2 and
		 struct_buf = arg3 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "statfs64(\"%s\", %d, 0x%08Lx)"
		   path buf_len struct_buf;
	       self#sys_statfs64 path buf_len struct_buf
	 | (ARM, 267)    (* fstatfs64 *)
	 | (X86, 269) -> (* fstatfs64 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd = Int64.to_int arg1 and
		 buf_len = Int64.to_int arg2 and
		 struct_buf = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "fstatfs64(%d, %d, 0x%08Lx)"
		   fd buf_len struct_buf;
	       self#sys_fstatfs64 fd buf_len struct_buf
	 | (ARM, 268)    (* tgkill *)
	 | (X64, 234)    (* tgkill *)
	 | (X86, 270) -> (* tgkill *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let tgid = Int64.to_int arg1 and
		 tid = Int64.to_int arg2 and
		 signal = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "tgkill(%d, %d, %d)"
		   tgid tid signal;
	       self#sys_tgkill tgid tid signal
	 | (ARM, 269)    (* utimes *)
	 | (X64, 235)    (* utimes *)
	 | (X86, 271) -> (* utimes *)
	     uh "Unhandled Linux system call utimes"
	 | (ARM, 270) -> uh "Check whether ARM fadvise64_64 matches x86"
	 | (X86, 272) -> (* fadvise64_64 *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let fd = Int64.to_int arg1 and
		 offset = assemble64 arg2 arg3 and
		 len = assemble64 arg4 arg5 and
		 advice = Int64.to_int arg6 in
	       if !opt_trace_syscalls then
		 Printf.printf "fadvise64_64(%d, %Ld, %Ld, %d)"
		   fd offset len advice;
	       self#sys_fadvise64_64 fd offset len advice
	 | (ARM, 271) -> (* pciconfig_iobase *)
	     uh "Unhandled Linux/ARM system call pciconfig_iobase (271)"
	 | (ARM, 272) -> (* pciconfig_read *)
	     uh "Unhandled Linux/ARM system call pciconfig_read (272)"
	 | (ARM, 273) -> (* pciconfig_write *)
	     uh "Unhandled Linux/ARM system call pciconfig_write (273)"
	 | (ARM, 313)    (* vserver *)
	 | (X64, 236)    (* vserver *)
	 | (X86, 273) -> (* vserver *)
	     uh "Unhandled Linux system call vserver"
	 | (ARM, 319)    (* mbind *)
	 | (X64, 237)    (* mbind *)
	 | (X86, 274) -> (* mbind *)
	     uh "Unhandled Linux system call mbind"
	 | (ARM, 320)    (* get_mempolicy *)
	 | (X64, 239)    (* get_mempolicy *)
	 | (X86, 275) -> (* get_mempolicy *)
	     uh "Unhandled Linux system call get_mempolicy"
	 | (ARM, 321)    (* set_mempolicy *)
	 | (X64, 238)    (* set_mempolicy *)
	 | (X86, 276) -> (* set_mempolicy *)
	     uh "Unhandled Linux system call set_mempolicy"
	 | (ARM, 274)    (* mq_open *)
	 | (X64, 240)    (* mq_open *)
	 | (X86, 277) -> (* mq_open *)
	     uh "Unhandled Linux system call mq_open"
	 | (ARM, 275)    (* mq_unlink *)
	 | (X64, 241)    (* mq_unlink *)
	 | (X86, 278) -> (* mq_unlink *)
	     uh "Unhandled Linux system call mq_unlink"
	 | (ARM, 276)    (* mq_timedsend *)
	 | (X64, 242)    (* mq_timedsend *)
	 | (X86, 279) -> (* mq_timedsend *)
	     uh "Unhandled Linux system call mq_timedsend"
	 | (ARM, 277)    (* mq_timedreceive *)
	 | (X64, 243)    (* mq_timedreceive *)
	 | (X86, 280) -> (* mq_timedreceive *)
	     uh "Unhandled Linux system call mq_timedreceive"
	 | (ARM, 278)    (* mq_notify *)
	 | (X64, 244)    (* mq_notify *)
	 | (X86, 281) -> (* mq_notify *)
	     uh "Unhandled Linux system call mq_notify"
	 | (ARM, 279)    (* mq_getsetattr *)
	 | (X64, 245)    (* mq_getsetattr *)
	 | (X86, 282) -> (* mq_getsetattr *)
	     uh "Unhandled Linux system call mq_getsetattr"
	 | (ARM, 347)    (* kexec_load *)
	 | (X64, 246)    (* kexec_load *)
	 | (X86, 283) -> (* kexec_load *)
	     uh "Unhandled Linux system call kexec_load"
	 | (ARM, 280)    (* waitid *)
	 | (X64, 247)    (* waitid *)
	 | (X86, 284) -> (* waitid *)
	     uh "Unhandled Linux system call waitid"
	 | (ARM, 281)    (* socket *)
	 | (X64,  41) -> (* socket *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let dom_i = Int64.to_int arg1 and
		 typ_i = Int64.to_int arg2 and
		 prot_i = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "socket(%d, %d, %d)"
		   dom_i typ_i prot_i;
	       self#sys_socket dom_i typ_i prot_i
	 | (ARM, 282) -> (* bind *)
	     uh "Unhandled Linux/ARM system call bind (282)"
	 | (X64, 49) -> (* bind *)
	     uh "Unhandled Linux/x64 system call bind (49)"
	 | (ARM, 283)    (* connect *)
	 | (X64,  42) -> (* connect *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let sockfd = Int64.to_int arg1 and
		 addr = arg2 and
		 addrlen = Int64.to_int arg3
	     in
	       if !opt_trace_syscalls then
		 Printf.printf "connect(%d, 0x%08Lx, %d)"
		   sockfd addr addrlen;
	       self#sys_connect sockfd addr addrlen
	 | (ARM, 284) -> (* listen *)
	     uh "Unhandled Linux/ARM system call listen (284)"
	 | (X64, 50) -> (* listen *)
	     uh "Unhandled Linux/x64 system call listen (50)"
	 | (ARM, 285) -> (* accept *)
	     uh "Unhandled Linux/ARM system call accept (285)"
	 | (X64, 43) -> (* accept *)
	     uh "Unhandled Linux/x64 system call accept (43)"
	 | (ARM, 286)    (* getsockname *)
	 | (X64,  51) -> (* getsockname *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let sockfd = Int64.to_int arg1 and
		 addr = load_word arg2 and
		 addrlen_ptr = arg3
	     in
	       if !opt_trace_syscalls then
		 Printf.printf "getsockname(%d, 0x%08Lx, 0x%08Lx)"
		   sockfd addr addrlen_ptr;
	       self#sys_getsockname sockfd addr addrlen_ptr
	 | (ARM, 287)    (* getpeername *)
	 | (X64,  52) -> (* getpeername *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let sockfd = Int64.to_int arg1 and
		 addr = arg2 and
		 addrlen_ptr = arg3
	     in
	       if !opt_trace_syscalls then
		 Printf.printf "getpeername(%d, 0x%08Lx, 0x%08Lx)"
		   sockfd addr addrlen_ptr;
	       self#sys_getpeername sockfd addr addrlen_ptr
	 | (ARM, 288) -> (* socketpair *)
	     uh "Unhandled Linux/ARM system call socketpair (288)"
	 | (X64, 53) -> (* socketpair *)
	     uh "Unhandled Linux/x64 system call socketpair (53)"
	 | (ARM, 289) -> (* send *)
	     uh "Unhandled Linux/ARM system call send (289)"
	 | (ARM, 290) -> (* sendto *)
	     uh "Unhandled Linux/ARM system call sendto (290)"
	 | (X64, 44) -> (* sendto *)
	     uh "Unhandled Linux/x64 system call sendto (44)"
	 | (ARM, 291) -> (* recv *)
	     uh "Unhandled Linux/ARM system call recv (291)"
	 | (ARM, 292) -> (* recvfrom *)
	     uh "Unhandled Linux/ARM system call recvfrom (292)"
	 | (X64,  45) -> (* recvfrom *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let sockfd = Int64.to_int arg1 and
		 buf = arg2 and
		 len = Int64.to_int arg3 and
		 flags = Int64.to_int arg4 and
		 addr = arg5 and
		 addrlen_ptr = arg6
	     in
	       if !opt_trace_syscalls then
		 Printf.printf
		   "recvfrom(%d, 0x%08Lx, %d, %d, 0x%08Lx, 0x%08Lx)"
		   sockfd buf len flags addr addrlen_ptr;
	       self#sys_recvfrom sockfd buf len flags addr addrlen_ptr
	 | (ARM, 293) -> (* shutdown *)
	     uh "Unhandled Linux/ARM system call shutdown (293)"
	 | (X64, 48) -> (* shutdown *)
	     let (arg1, arg2) = read_2_regs () in
	     let sockfd = Int64.to_int arg1 and
		 how = Int64.to_int arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "shutdown(%d, %d)" sockfd how;
	       self#sys_shutdown sockfd how
	 | (ARM, 294) -> (* setsockopt *)
	     uh "Unhandled Linux/ARM system call setsockopt (294)"
	 | (X64, 54) -> (* setsockopt *)
	     let (arg1, arg2, arg3, arg4, arg5) = read_5_regs () in
             let sockfd = Int64.to_int arg1 and
		 level = Int64.to_int arg2 and
		 name = Int64.to_int arg3 and
		 valp = arg4 and
		 len = Int64.to_int arg5 in
	     if !opt_trace_syscalls then
	       Printf.printf "setsockopt(%d, %d, %d, 0x%08Lx, %d)"
		 sockfd level name valp len;
	     self#sys_setsockopt sockfd level name valp len
	 | (ARM, 295) -> (* getsockopt *)
	     uh "Unhandled Linux/ARM system call getsockopt (295)"
	 | (X64, 55) -> (* getsockopt *)
	     uh "Unhandled Linux/x64 system call getsockopt (55)"
	 | (ARM, 296) -> (* sendmsg *)
	     uh "Unhandled Linux/ARM system call sendmsg (296)"
	 | (X64, 46) -> (* sendmsg *)
	     uh "Unhandled Linux/x64 system call sendmsg (46)"
	 | (ARM, 297) -> (* recvmsg *)
	     uh "Unhandled Linux/ARM system call recvmsg (297)"
	 | (X64,  47) -> (* recvmsg *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let sockfd = Int64.to_int arg1 and
		 msg = arg2 and
		 flags = Int64.to_int arg3
	     in
	       if !opt_trace_syscalls then
		 Printf.printf "recvmsg(%d, 0x%08Lx, %d)"
		   sockfd msg flags;
	       self#sys_recvmsg64 sockfd msg flags
	 | (ARM, 298) -> (* semop *)
	     uh "Unhandled Linux/ARM system call semop (298)"
	 | (X64, 65) -> (* semop *)
	     uh "Unhandled Linux/x64 system call semop (65)"
	 | (ARM, 299) -> (* semget *)
	     uh "Unhandled Linux/ARM system call semget (299)"
	 | (X64, 64) -> (* semget *)
	     uh "Unhandled Linux/x64 system call semget (64)"
	 | (ARM, 300) -> (* semctl *)
	     uh "Unhandled Linux/ARM system call semctl (300)"
	 | (X64, 66) -> (* semctl *)
	     uh "Unhandled Linux/x64 system call semctl (66)"
	 | (ARM, 301) -> (* msgsnd *)
	     uh "Unhandled Linux/ARM system call msgsnd (301)"
	 | (X64, 69) -> (* msgsnd *)
	     uh "Unhandled Linux/x64 system call msgsnd (69)"
	 | (ARM, 302) -> (* msgrcv *)
	     uh "Unhandled Linux/ARM system call msgrcv (302)"
	 | (X64, 70) -> (* msgrcv *)
	     uh "Unhandled Linux/x64 system call msgrcv (70)"
	 | (ARM, 303) -> (* msgget *)
	     uh "Unhandled Linux/ARM system call msgget (303)"
	 | (X64, 68) -> (* msgget *)
	     uh "Unhandled Linux/x64 system call msgget (68)"
	 | (ARM, 304) -> (* msgctl *)
	     uh "Unhandled Linux/ARM system call msgctl (304)"
	 | (X64, 71) -> (* msgctl *)
	     uh "Unhandled Linux/x64 system call msgctl (71)"
	 | (ARM, 305) -> (* shmat *)
	     uh "Unhandled Linux/ARM system call shmat (305)"
	 | (X64, 30) -> (* shmat *)
	     uh "Unhandled Linux/x64 system call shmat (30)"
	 | (ARM, 306) -> (* shmdt *)
	     uh "Unhandled Linux/ARM system call shmdt (306)"
	 | (X64, 67) -> (* shmdt *)
	     uh "Unhandled Linux/x64 system call shmdt (67)"
	 | (ARM, 307) -> (* shmget *)
	     uh "Unhandled Linux/ARM system call shmget (307)"
	 | (X64,  29) -> (* shmget *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let key = arg1 and
		 size = Int64.to_int arg2 and
		 shmflag = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "shmget(%Ld, %d, %d)" key size shmflag;
	       self#sys_shmget key size shmflag
	 | (ARM, 308) -> (* shmctl *)
	     uh "Unhandled Linux/ARM system call shmctl (308)"
	 | (X64, 31) -> (* shmctl *)
	     uh "Unhandled Linux/x64 system call shmctl (31)"
	 | (ARM, 309)    (* add_key *)
	 | (X64, 248)    (* add_key *)
	 | (X86, 286) -> (* add_key *)
	     uh "Unhandled Linux system call add_key"
	 | (ARM, 310)    (* request_key *)
	 | (X64, 249)    (* request_key *)
	 | (X86, 287) -> (* request_key *)
	     uh "Unhandled Linux system call request_key"
	 | (ARM, 311)    (* keyctl *)
	 | (X64, 250)    (* keyctl *)
	 | (X86, 288) -> (* keyctl *)
	     uh "Unhandled Linux system call keyctl"
	 | (ARM, 312) -> (* semtimedop *)
	     uh "Unhandled Linux/ARM system call semtimedop"
	 | (X64, 220) -> (* semtimedop *)
	     uh "Unhandled Linux/x64 system call semtimedop (220)"
	 | (ARM, 314)    (* ioprio_set *)
	 | (X64, 251)    (* ioprio_set *)
	 | (X86, 289) -> (* ioprio_set *)
	     uh "Unhandled Linux system call ioprio_set"
	 | (ARM, 315)    (* ioprio_get *)
	 | (X64, 252)    (* ioprio_get *)
	 | (X86, 290) -> (* ioprio_get *)
	     uh "Unhandled Linux system call ioprio_get"
	 | (ARM, 316)    (* inotify_init *)
	 | (X64, 253)    (* inotify_init *)
	 | (X86, 291) -> (* inotify_init *)
	     uh "Unhandled Linux system call inotify_init"
	 | (ARM, 317)    (* inotify_add_watch *)
	 | (X64, 254)    (* inotify_add_watch *)
	 | (X86, 292) -> (* inotify_add_watch *)
	     uh "Unhandled Linux system call inotify_add_watch"
	 | (ARM, 318)    (* inotify_rm_watch *)
	 | (X64, 255)    (* inotify_rm_watch *)
	 | (X86, 293) -> (* inotify_rm_watch *)
	     uh "Unhandled Linux system call inotify_rm_watch"
	 | (X86, 294) -> (* migrate_pages *)
	     uh "Unhandled Linux/x86 system call migrate_pages (294)"
	 | (X64, 256) -> (* migrate_pages *)
	     uh "Unhandled Linux/x64 system call migrate_pages (256)"
	 | (ARM, 322)    (* openat *)
	 | (X86, 295)    (* openat *)
	 | (X64, 257) -> (* openat *)
             let (arg1, arg2, arg3) = read_3_regs () in
             let arg4 = (if (Int64.logand arg3 0o100L) <> 0L then
			   get_reg arg_regs.(3)
			 else
			   0L) in
             let dirfd    = Int64.to_int (fix_s32 arg1) and
		 path_buf = arg2 and
		 flags    = Int64.to_int arg3 and
		 mode     = Int64.to_int arg4 in
             let path = fm#read_cstr path_buf in
               if !opt_trace_syscalls then
		 Printf.printf "openat(%d, \"%s\", 0x%x, 0o%o)"
		   dirfd path flags mode;
               self#sys_openat dirfd path flags mode
	 | (ARM, 323)    (* mkdirat *)
	 | (X64, 258)    (* mkdirat *)
	 | (X86, 296) -> (* mkdirat *)
	     uh "Unhandled Linux system call mkdirat"
	 | (ARM, 324)    (* mknodat *)
	 | (X64, 259)    (* mknodat *)
	 | (X86, 297) -> (* mknodat *)
	     uh "Unhandled Linux system call mknodat"
	 | (ARM, 325)    (* fchownat *)
	 | (X64, 260)    (* fchownat *)
	 | (X86, 298) -> (* fchownat *)
	     uh "Unhandled Linux system call fchownat"
	 | (ARM, 326)    (* futimesat *)
	 | (X64, 261)    (* futimesat *)
	 | (X86, 299) -> (* futimesat *)
	     uh "Unhandled Linux system call futimesat"
	 | (ARM, 327)    (* fstatat64 *)
	 | (X86, 300) -> (* fstatat64 *)
	     uh "Unhandled Linux system call fstatat64"
	 | (X64, 262) -> (* newfstatat *)
	     uh "Unhandled Linux/x64 system call newfstatat (262)"
	 | (ARM, 328)    (* unlinkat *)
	 | (X64, 263)    (* unlinkat *)
	 | (X86, 301) -> (* unlinkat *)
	     uh "Unhandled Linux system call unlinkat"
	 | (ARM, 329)    (* renameat *)
	 | (X64, 264)    (* renameat *)
	 | (X86, 302) -> (* renameat *)
	     uh "Unhandled Linux system call renameat"
	 | (ARM, 330)    (* linkat *)
	 | (X64, 265)    (* linkat *)
	 | (X86, 303) -> (* linkat *)
	     uh "Unhandled Linux system call linkat"
	 | (ARM, 331)    (* symlinkat *)
	 | (X64, 266)    (* symlinkat *)
	 | (X86, 304) -> (* symlinkat *)
	     uh "Unhandled Linux system call symlinkat"
	 | (ARM, 332)    (* readlinkat *)
	 | (X64, 267)    (* readlinkat *)
	 | (X86, 305) -> (* readlinkat *)
	     uh "Unhandled Linux system call readlinkat"
	 | (ARM, 333)    (* fchmodat *)
	 | (X64, 268)    (* fchmodat *)
	 | (X86, 306) -> (* fchmodat *)
	     uh "Unhandled Linux system call fchmodat"
	 | (ARM, 334)    (* faccessat *)
	 | (X64, 269)    (* faccessat *)
	 | (X86, 307) -> (* faccessat *)
	     uh "Unhandled Linux system call faccessat"
	 | (ARM, 335)    (* pselect6 *)
	 | (X64, 270)    (* pselect6 *)
	 | (X86, 308) -> (* pselect6 *)
	     uh "Unhandled Linux system call pselect6"
	 | (ARM, 336)    (* ppoll *)
	 | (X64, 271)    (* ppoll *)
	 | (X86, 309) -> (* ppoll *)
	     uh "Unhandled Linux system call ppoll"
	 | (ARM, 337)    (* unshare *)
	 | (X64, 272)    (* unshare *)
	 | (X86, 310) -> (* unshare *)
	     uh "Unhandled Linux system call unshare"
	 | (ARM, 338)    (* set_robust_list *)
	 | (X86, 311)    (* set_robust_list *)
	 | (X64, 273) -> (* set_robust_list *)
	     let (arg1, arg2) = read_2_regs () in
	     let addr = arg1 and
		 len  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "set_robust_list(0x%08Lx, %Ld)" addr len;
	       self#sys_set_robust_list addr len
	 | (ARM, 339)    (* get_robust_list *)
	 | (X64, 274)    (* get_robust_list *)
	 | (X86, 312) -> (* get_robust_list *)
	     uh "Unhandled Linux system call get_robust_list"
	 | (ARM, 340)    (* splice *)
	 | (X64, 275)    (* splice *)
	 | (X86, 313) -> (* splice *)
	     uh "Unhandled Linux system call splice"
	 | (ARM, 341)    (* sync_file_range *)
	 | (X64, 277)    (* sync_file_range *)
	 | (X86, 314) -> (* sync_file_range *)
	     uh "Unhandled Linux system call sync_file_range"
	 | (ARM, 342)    (* tee *)
	 | (X64, 276)    (* tee *)
	 | (X86, 315) -> (* tee *)
	     uh "Unhandled Linux system call tee"
	 | (ARM, 343)    (* vmsplice *)
	 | (X64, 278)    (* vmsplice *)
	 | (X86, 316) -> (* vmsplice *)
	     uh "Unhandled Linux system call vmsplice"
	 | (ARM, 344)    (* move_pages *)
	 | (X64, 279)    (* move_pages *)
	 | (X86, 317) -> (* move_pages *)
	     uh "Unhandled Linux system call move_pages"
	 | (ARM, 345)    (* getcpu *)
	 | (X64, 309)    (* getcpu *)
	 | (X86, 318) -> (* getcpu *)
	     uh "Unhandled Linux system call getcpu"
	 | (ARM, 346)    (* epoll_pwait *)
	 | (X64, 281)    (* epoll_pwait *)
	 | (X86, 319) -> (* epoll_pwait *)
	     uh "Unhandled Linux system call epoll_pwait"
	 | (X64, 280) -> uh "Check whether x64 utimensat syscall matches x86"
	 | (ARM, 348)    (* utimensat *)
	 | (X86, 320) -> (* utimensat *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let dirfd = Int64.to_int arg1 and
		 path_buf = arg2 and
		 times = arg3 and
		 flags = Int64.to_int arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "utimensat(%d, 0x%08Lx, 0x%08Lx, %d)"
		   dirfd path_buf times flags;
	       self#sys_utimensat dirfd path_buf times flags
	 | (ARM, 349)    (* signalfd *)
	 | (X64, 282)    (* signalfd *)
	 | (X86, 321) -> (* signalfd *)
	     uh "Unhandled Linux system call signalfd"
	 | (ARM, 350)    (* timerfd_create *)
	 | (X64, 283)    (* timerfd_create *)
	 | (X86, 322) -> (* timerfd_create *)
	     uh "Unhandled Linux system call timerfd_create"
	 | (ARM, 351)    (* eventfd *)
	 | (X64, 284)    (* eventfd *)
	 | (X86, 323) -> (* eventfd *)
	     uh "Unhandled Linux system call eventfd"
	 | (ARM, 352)    (* fallocate *)
	 | (X64, 285)    (* fallocate *)
	 | (X86, 324) -> (* fallocate *)
	     uh "Unhandled Linux system call fallocate"
	 | (ARM, 353)    (* timerfd_settime *)
	 | (X64, 286)    (* timerfd_settime *)
	 | (X86, 325) -> (* timerfd_settime *)
	     uh "Unhandled Linux system call timerfd_settime"
	 | (ARM, 354)    (* timerfd_gettime *)
	 | (X64, 287)    (* timerfd_gettime *)
	 | (X86, 326) -> (* timerfd_gettime *)
	     uh "Unhandled Linux system call timerfd_gettime"
	 | (ARM, 355)    (* signalfd4 *)
	 | (X64, 289)    (* signalfd4 *)
	 | (X86, 327) -> (* signalfd4 *)
	     uh "Unhandled Linux system call signalfd4"
	 | (ARM, 356)    (* eventfd2 *)
	 | (X64, 290)    (* eventfd2 *)
	 | (X86, 328) -> (* eventfd2 *)
	     let (arg1, arg2) = read_2_regs () in
	     let initval = arg1 and
		 flags = Int64.to_int arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "eventfd2(%Ld, %d)" initval flags;
	       self#sys_eventfd2 initval flags
	 | (ARM, 357)    (* epoll_create1 *)
	 | (X64, 291)    (* epoll_create1 *)
	 | (X86, 329) -> (* epoll_create1 *)
	     uh "Unhandled Linux system call epoll_create1"
	 | (ARM, 358)    (* dup3 *)
	 | (X64, 292)    (* dup3 *)
	 | (X86, 330) -> (* dup3 *)
	     uh "Unhandled Linux system call dup3"
	 | (ARM, 359) -> uh "Check whether ARM pipe2 syscall matches x86"
	 | (X64, 293) -> uh "Check whether x64 pipe2 syscall matches x86"
	 | (X86, 331) -> (* pipe2 *)
	     let (ebx, ecx) = read_2_regs () in
	     let buf = ebx and
		 flags = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "pipe2(0x%08Lx, %d)" buf flags;
	       self#sys_pipe2 buf flags
	 | (ARM, 360)    (* inotify_init1 *)
	 | (X64, 294)    (* inotify_init1 *)
	 | (X86, 332) -> (* inotify_init1 *)
	     uh "Unhandled Linux system call inotify_init1"
	 | (ARM, 361)    (* preadv *)
	 | (X64, 295)    (* preadv *)
	 | (X86, 333) -> (* preadv *)
	     uh "Unhandled Linux system call preadv"
	 | (ARM, 362)    (* pwritev *)
	 | (X64, 296)    (* pwritev *)
	 | (X86, 334) -> (* pwritev *)
	     uh "Unhandled Linux system call pwritev"
	 | (ARM, 363)    (* rt_tgsigqueueinfo *)
	 | (X64, 297)    (* rt_tgsigqueueinfo *)
	 | (X86, 335) -> (* rt_tgsigqueueinfo *)
	     uh "Unhandled Linux system call rt_tgsigqueueinfo"
	 | (ARM, 364)    (* perf_event_open *)
	 | (X64, 298)    (* perf_event_open *)
	 | (X86, 336) -> (* perf_event_open *)
	     uh "Unhandled Linux system call perf_event_open"
	 | (ARM, 365) ->
	     uh "No 365 syscall in Linux/ARM (E)ABI"
	 | (ARM, 366) -> (* accept4 *)
	     uh "Unhandled Linux/ARM system call accept4"
	 | (X64, 288) -> (* accept4 *)
	     uh "Unhandled Linux/x64 system call accept4 (288)"
	 | (X64, 299)    (* recvmmsg *)
	 | (X86, 337) -> (* recvmmsg *)
	     uh "Unhandled Linux system call recvmmsg"
	 | (X64, 300)    (* fanotify_init *)
	 | (X86, 338) -> (* fanotify_init *)
	     uh "Unhandled Linux system call fanotify_init"
	 | (X64, 301)    (* fanotify_mark *)
	 | (X86, 339) -> (* fanotify_mark *)
	     uh "Unhandled Linux system call fanotify_mark"
	 | (X64, 302)    (* prlimit64 *)
	 | (X86, 340) -> (* prlimit64 *)
             let (arg1, arg2, arg3, arg4) = read_4_regs () in
             let pid = Int64.to_int arg1 and
                 rsrc = Int64.to_int arg2 and
                 new_limit_buf = arg3 and
                 old_limit_buf = arg4
             in
             if !opt_trace_syscalls then
               Printf.printf "prlimit64(%d, %d, 0x%08Lx, 0x%08Lx)"
                 pid rsrc new_limit_buf old_limit_buf;
             self#sys_prlimit64 pid rsrc new_limit_buf old_limit_buf
	 | (X64, 303)    (* name_to_handle_at *)
	 | (X86, 341) -> (* name_to_handle_at *)
	     uh "Unhandled Linux system call name_to_handle_at"
	 | (X64, 304)    (* open_by_handle_at *)
	 | (X86, 342) -> (* open_by_handle_at *)
	     uh "Unhandled Linux system call open_by_handle_at"
	 | (X64, 305)    (* clock_adjtime *)
	 | (X86, 343) -> (* clock_adjtime *)
	     uh "Unhandled Linux system call clock_adjtime"
	 | (X64, 306)    (* syncfs *)
	 | (X86, 344) -> (* syncfs *)
	     uh "Unhandled Linux system call syncfs"
	 | (X64, 307) -> uh "Check whether x64 sendmmsg syscall matches x86"
	 | (X86, 345) -> (* sendmmsg *)
             let (ebx, ecx, edx, esi) = read_4_regs () in
             let sockfd = Int64.to_int ebx and
		 msg = ecx and
		 vlen = Int64.to_int edx and
		 flags = Int64.to_int esi
             in
               if !opt_trace_syscalls then
		 Printf.printf "sendmmsg(%d, 0x%08Lx, %d, %d)"
		   sockfd msg vlen flags;
               self#sys_sendmmsg sockfd msg vlen flags
	 | (X86, 346)    (* setns *)
	 | (X64, 308) -> (* setns *)
	     uh "Unhandled Linux system call setns"
	 | (X86, 347)    (* process_vm_readv *)
	 | (X64, 310) -> (* process_vm_readv *)
	     uh "Unhandled Linux system call process_vm_readv"
	 | (X86, 348)    (* process_vm_writev *)
	 | (X64, 311) -> (* process_vm_writev *)
	     uh "Unhandled Linux system call process_vm_writev"
	 | (X86, 349)    (* kcmp *)
	 | (X64, 312) -> (* kcmp *)
	     uh "Unhandled Linux system call kcmp"
	 | (X86, 350)    (* finit_module *)
	 | (X64, 313) -> (* finit_module *)
	     uh "Unhandled Linux system call finit_module"
	 | (X86, 351)    (* sched_setattr *)
	 | (X64, 314) -> (* sched_setattr *)
	     uh "Unhandled Linux system call sched_setattr"
	 | (X86, 352)    (* sched_getattr *)
	 | (X64, 315) -> (* sched_getattr *)
	     uh "Unhandled Linux system call sched_getattr"
	 | (X86, 353)    (* renameat2 *)
	 | (X64, 316) -> (* renameat2 *)
	     uh "Unhandled Linux system call renameat2"
	 | (X86, 354)    (* seccomp *)
	 | (X64, 317) -> (* seccomp *)
	     uh "Unhandled Linux system call seccomp"
	 | (X86, 355)    (* getrandom *)
	 | (X64, 318) -> (* getrandom *)
             let (arg1, arg2, arg3) = read_3_regs () in
	     let buf = arg1 and
		 buflen = Int64.to_int arg2 and
		 flags = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "getrandom(0x%08Lx, %d, %d)" buf buflen flags;
	       self#sys_getrandom buf buflen flags
	 | (X86, 356)    (* memfd_create *)
	 | (X64, 319) -> (* memfd_create *)
	     uh "Unhandled Linux system call memfd_create"
	 | (X64, 320) -> (* kexec_file_load *)
	     uh "Unhandled Linux system call kexec_file_load"
	 | (X86, 357)    (* bpf *)
	 | (X64, 321) -> (* bpf *)
	     uh "Unhandled Linux system call bpf"
	 | (X86, 358)    (* execveat *)
	 | (X64, 322) -> (* execveat *)
	     uh "Unhandled Linux system call execveat"
	 | (X86, 374)    (* userfaultfd *)
	 | (X64, 323) -> (* userfaultfd *)
	     uh "Unhandled Linux system call userfaultfd"
	 | (X86, 375)    (* membarrier *)
	 | (X64, 324) -> (* membarrier *)
	     uh "Unhandled Linux system call membarrier"
	 | (X86, 376)    (* mlock2 *)
	 | (X64, 325) -> (* mlock2 *)
	     uh "Unhandled Linux system call mlock2"

	 | (ARM, 0xf0001) -> (* breakpoint *)
	     uh "Unhandled Linux/ARM pseudo-syscall breakpoint (0xf0001)"
	 | (ARM, 0xf0002) -> (* cacheflush *)
	     uh "Unhandled Linux/ARM pseudo-syscall cacheflush (0xf0002)"
	 | (ARM, 0xf0003) -> (* usr26 *)
	     uh "Unhandled Linux/ARM pseudo-syscall usr26 (0xf0003)"
	 | (ARM, 0xf0004) -> (* usr32 *)
	     uh "Unhandled Linux/ARM pseudo-syscall usr32 (0xf0004)"
	 | (ARM, 0xf0005) -> (* set_tls *)
	     let r0 = read_1_reg () in
	     let tp_value = r0 in
	       if !opt_trace_syscalls then
		 Printf.printf "set_tls(0x%08Lx)" tp_value;
	     self#sys_set_tls tp_value

	 | (X86, _) ->
	     Printf.printf "Unknown Linux/x86 system call %d\n" syscall_num;
	     uh "Unhandled Linux system call"
	 | (ARM, _) ->
	     Printf.printf "Unknown Linux/ARM system call %d\n" syscall_num;
	     uh "Unhandled Linux system call"
	 | (X64, _) ->
	     Printf.printf "Unknown Linux/x86-64 system call %d\n" syscall_num;
	     uh "Unhandled Linux system call");
    if !opt_trace_syscalls then
      let ret_val = match !opt_arch with
	| (X86|ARM) -> fm#get_word_var ret_reg
	| X64 -> fm#get_long_var ret_reg
      in
	Printf.printf " = %Ld (0x%08Lx)\n" (fix_s32 ret_val) ret_val;
	flush stdout

  (* The address to which a sysenter-based syscall will return is
     controlled by MSRs which are set by the kernel and not visible from
     user space, so this has to be somewhat of a guess. It also seems
     to have changed between kernel versions. So match some features of
     layouts we've seen, otherwise die.  *)
  method private guess_sysexit_addr enter =
    assert(Int64.logand enter 0xfL = 5L);
    let next = Int64.add enter 2L in
    match load_byte next with
      | 0x90 ->
	  (* Older layout:
	     sysenter (0f 34)
             nop x 7 (90 90 90 90 90 90 90)
             int 0x80 (cd 80)
             <return_point> *)
	  assert(load_byte (Int64.add next 1L) = 0x90);
	  assert(load_byte (Int64.add next 2L) = 0x90);
	  assert(load_byte (Int64.add next 3L) = 0x90);
	  assert(load_byte (Int64.add next 4L) = 0x90);
	  assert(load_byte (Int64.add next 5L) = 0x90);
	  assert(load_byte (Int64.add next 6L) = 0x90);
	  assert(load_byte (Int64.add next 7L) = 0xcd);
	  assert(load_byte (Int64.add next 8L) = 0x80);
	  Int64.add next 9L (* in this layout, 16-byte aligned *)
      | 0xcd ->
	  (* Newer layout: similar to above, but with the nops *)
	  assert(load_byte (Int64.add next 1L) = 0x80);
	  Int64.add next 2L
      | _ -> failwith "Unhandled sysenter call layout"

  method handle_special str =
    let handle_catch () =
      try
	self#handle_linux_syscall ()
      with
	  NotConcrete(_) ->
	    match !opt_symbolic_syscall_error with
	      | Some errno -> put_return errno
	      | None -> raise SymbolicSyscall
    in

      match str with
	| "int 0x80" ->
	    handle_catch();
	    Some []
	| "syscall" -> (* e.g., ARM *)
	    handle_catch();
	    Some []	    
	| "sysenter" ->
	    let sysenter_eip = fm#get_word_var R_EIP in
	    let sysexit_eip = self#guess_sysexit_addr sysenter_eip in
	    let label = "pc_0x" ^ (Printf.sprintf "%08Lx" sysexit_eip) in
	      handle_catch ();
	      Some [V.Jmp(V.Name(label))]
	| _ -> None
end

(*
  Copyright (C) BitBlaze, 2009-2013. All rights reserved.
*)

module V = Vine

open Exec_utils
open Exec_exceptions
open Exec_options
open Fragment_machine

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
  mutable dirp_offset : int;
  mutable readdir_eof: int;
  mutable fname : string;
  mutable snap_pos : int option;
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
       | X64 -> failwith "64-bit syscalls not supported"
       | ARM -> put_reg R0)
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
  let zero_region base len =
    assert(len >= 0 && len <= 0x20000000); (* sanity check *)
    for i = 0 to len - 1 do
      fm#store_byte_idx base i 0
    done
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
    if vt_fd < 0 || vt_fd >= (Array.length unix_fds) then
      raise (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))
    else
      match unix_fds.(vt_fd) with
	| Some fd -> fd
	| None -> raise
	    (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))

  val fd_info = Array.init 1024
    (fun _ -> { dirp_offset = 0; readdir_eof = 0; fname = ""; snap_pos = None })

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
    with Invalid_argument("String.create")
	-> raise (Unix.Unix_error(Unix.EFAULT, "String.create", ""))

  (* Right now we always redirect the program's FDs 1 and 2 (stdout
     and stderr) to FuzzBALL's stdout. We might want to consider doing
     this more selectively (e.g., only if they're still pointing the same
     place as they did when the program started), or controlled by a
     command-line flag. *)
  method do_write fd bytes count =
    (try
       (match !opt_prefix_out, fd with
	  | (Some prefix, (1|2)) ->
	      Printf.printf "[%s fd %d]: " prefix fd
	  | _ -> ());
       (match fd with
	  | (1|2) -> Array.iter print_char bytes;
	      put_return (Int64.of_int count)
	  | _ ->
	      let str = string_of_char_array bytes and
		  ufd = self#get_fd fd
	      in
		match Unix.write ufd str 0 count
		with
		  | i when i = count -> put_return (Int64.of_int count)
		  | _ -> raise (Unix.Unix_error(Unix.EINTR, "", "")))
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
      (if (flags land 0o4000) != 0  then [Unix.O_NONBLOCK] else []) @
      (if (flags land 0o2000) != 0  then [Unix.O_APPEND]   else []) @
      (if (flags land 0o100) != 0   then [Unix.O_CREAT]    else []) @
      (if (flags land 0o1000) != 0  then [Unix.O_TRUNC]    else []) @
      (if (flags land 0o200) != 0   then [Unix.O_EXCL]     else []) @
      (if (flags land 0o10000) != 0 then [Unix.O_SYNC]     else [])

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
      store_word addr 76 0L;      (* mtime naonsecs *)
      store_word addr 80 ctime;
      store_word addr 84 0L;      (* ctime nanosecs *)
      store_word addr 88 ino;     (* low bits of 64-bit inode *)
      store_word addr 92 0L;      (* high bits of 64-bit inode *)

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

  method write_fake_statfs_buf addr =
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

  method write_fake_statfs64buf addr =
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
  method write_ftime_as_words ftime addr resolution =
    let fsecs = floor ftime in
    let secs = Int64.of_float fsecs and
	fraction = Int64.of_float (resolution *. (ftime -. fsecs)) in
      store_word addr 0 secs;
      store_word addr 4 fraction

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
  val symbolic_fds = Hashtbl.create 11

  method add_symbolic_file s is_concolic =
    Hashtbl.replace symbolic_fnames s is_concolic

  method private save_sym_fd_positions = 
    Hashtbl.iter
      (fun fd _ -> fd_info.(fd).snap_pos <- 
	 Some (Unix.lseek (self#get_fd fd) 0 Unix.SEEK_CUR))
      symbolic_fds

  method private reset_sym_fd_positions = 
    Hashtbl.iter
      (fun fd _ ->
	 match fd_info.(fd).snap_pos with
	   | Some pos -> ignore(Unix.lseek (self#get_fd fd) pos Unix.SEEK_SET)
	   | None -> ())
      symbolic_fds

  method make_snap = 
    self#save_sym_fd_positions;
    self#save_memory_state

  method reset = 
    self#reset_sym_fd_positions;
    self#reset_memory_state

  method sys_access path mode =
    let oc_mode =
      (if   (mode land 0x7)= 0 then [Unix.F_OK] else []) @
	(if (mode land 0x1)!=0 then [Unix.X_OK] else []) @
	(if (mode land 0x2)!=0 then [Unix.W_OK] else []) @
	(if (mode land 0x4)!=0 then [Unix.R_OK] else []) 
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
      Array.set unix_fds vt_fd (Some sockfd_oc);
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
	     timep 1000000000.0);
	  put_return 0L
      | 0 -> (* CLOCK_REALTIME *)
	  self#write_ftime_as_words (Unix.gettimeofday ()) timep 1000000000.0;
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
      Array.set unix_fds fd None;
      Hashtbl.remove symbolic_fds fd;
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
      Array.set unix_fds new_vt_fd (Some new_oc_fd);
      fd_info.(new_vt_fd).fname <- fd_info.(old_vt_fd).fname;
      fd_info.(new_vt_fd).dirp_offset <- fd_info.(old_vt_fd).dirp_offset;
      fd_info.(new_vt_fd).readdir_eof <- fd_info.(old_vt_fd).readdir_eof;
      put_return (Int64.of_int new_vt_fd)

  method sys_dup2 old_vt_fd new_vt_fd =
    let old_oc_fd = self#get_fd old_vt_fd and
        new_oc_fd = self#get_fd new_vt_fd in
    Unix.dup2 old_oc_fd new_oc_fd;
    fd_info.(new_vt_fd).fname <- fd_info.(old_vt_fd).fname;
    fd_info.(new_vt_fd).dirp_offset <- fd_info.(old_vt_fd).dirp_offset;
    fd_info.(new_vt_fd).readdir_eof <- fd_info.(old_vt_fd).readdir_eof;
    put_return (Int64.of_int new_vt_fd)

  method sys_eventfd2 initval flags =
    ignore(initval);
    let oc_flags = Unix.O_RDWR ::
      (if (flags land 0o4000) <> 0 then [Unix.O_NONBLOCK] else [])
    in
    let oc_fd = Unix.openfile "/dev/null" oc_flags 0o666 in
    let vt_fd = self#fresh_fd () in
      Array.set unix_fds vt_fd (Some oc_fd);
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
      | 7 (* F_SETLKW *) ->
	  (* Ignore locks for the moment. OCaml has only lockf, so
	     emulation would be a bit complex. *)
	  ignore(fd);
	  ignore(arg);
	  put_return 0L (* success *)
      | _ -> failwith "Unhandled cmd in fcntl64"

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
      let dirname = chroot fd_info.(fd).fname in
      let dirh = Unix.opendir dirname in
      let written = ref 0 in
	for i = 0 to fd_info.(fd).dirp_offset - 1 do
	  ignore(Unix.readdir dirh)
	done;
	try
	  while true do
	    let fname = Unix.readdir dirh in
	    let reclen = 10 + (String.length fname) + 1 in
	    let next_pos = !written + reclen in
	      if next_pos >= buf_sz then
		raise End_of_file
	      else
		let oc_st = Unix.stat (dirname ^ "/" ^ fname) in
		let d_ino = oc_st.Unix.st_ino in
		  store_word dirp !written (Int64.of_int d_ino);
		  written := !written + 4;
		  store_word dirp !written (Int64.of_int next_pos);
		  written := !written + 4;
		  fm#store_short_conc (lea dirp 0 0 !written) reclen;
		  written := !written + 2;
		  fm#store_cstr dirp (Int64.of_int !written) fname;
		  written := !written + (String.length fname) + 1;
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

  method sys_ugetrlimit rsrc buf =
    store_word buf 0 0xffffffffL; (* infinity *)
    store_word buf 4 0xffffffffL; (* infinity *)
    put_return 0L (* success *)

  method sys_setrlimit resource rlim =
    ignore(resource);
    ignore(rlim);
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

  method sys_getrusage who buf =
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
      Array.set unix_fds vt_fd1 (Some oc_fd1);
      let vt_fd2 = self#fresh_fd () in
      Array.set unix_fds vt_fd2 (Some oc_fd2);
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
      self#write_ftime_as_words (Unix.gettimeofday ()) timep 1000000.0;
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
    let fdi = Int64.to_int fd in
    compare_fds !netlink_sim_sockfd fdi
      Unix.ENOSYS "mmap(2) for netlink socket fd not implemented";
    let do_read addr = 
      let len = Int64.to_int length in
      let old_loc = Unix.lseek (self#get_fd fdi) 0 Unix.SEEK_CUR in
      let _ = Unix.lseek (self#get_fd fdi) offset Unix.SEEK_SET in
      let _ = self#do_unix_read (self#get_fd fdi) addr len in
      let _ = Unix.lseek (self#get_fd fdi) old_loc Unix.SEEK_SET in
	(* assert(nr = len); *)
	addr
    in
    let ret =
      match (addr, length, prot, flags, fd) with
	| (_, length, _, _, _) when
	    length < 0L || length > 1073741824L ->
	    raise (Unix.Unix_error(Unix.ENOMEM, "Too large in mmap", ""))
	| (0L, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x22L (* MAP_PRIVATE|MAP_ANONYMOUS *), 0xffffffffL) ->
	    let fresh = self#fresh_addr length in
	      zero_region fresh (Int64.to_int length);
	      fresh
	| (0L, _, 0x0L (* PROT_NONE *),
	   0x4022L (* MAP_NORESERVE|MAP_PRIVATE|MAP_ANONYMOUS *),
	   0xffffffffL) ->
	    let fresh = self#fresh_addr length in
	      zero_region fresh (Int64.to_int length);
	      fresh	    
	| (_, _, (0x3L|0x7L) (* PROT_READ|PROT_WRITE|PROT_EXEC) *),
	   0x32L (* MAP_PRIVATE|FIXED|ANONYMOUS *), 0xffffffffL) ->
	    zero_region addr (Int64.to_int length);
	    addr
	| (0L, _, 
	   (0x1L|0x5L) (* PROT_READ|PROT_EXEC *),
	   (0x802L|0x2L|0x1L) (* MAP_PRIVATE|MAP_DENYWRITE|MAP_SHARED *), _) ->
	    let dest_addr = self#fresh_addr length in
	      do_read dest_addr
	| (_, _,
	   (0x1L|0x5L) (* PROT_READ|PROT_EXEC *),
	   (0x802L|0x2L|0x1L) (* MAP_PRIVATE|MAP_DENYWRITE|MAP_SHARED *), _) ->
	    do_read addr
	| (_, _, (0x3L|0x7L) (* PROT_READ|PROT_WRITE|PROT_EXEC *),
	   0x812L (* MAP_DENYWRITE|PRIVATE|FIXED *), _) ->
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
      self#mmap_common addr length prot flags fd (4096*pgoffset)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_mprotect addr len prot =
    (* treat as no-op *)
    put_return 0L;

  method sys_munmap addr len =
    (* treat as no-op *)
    put_return 0L

  method private open_throw path flags mode =
    let oc_flags = self#flags_to_oc_flags flags in
    let oc_fd = Unix.openfile (chroot path) oc_flags mode and
	  vt_fd = self#fresh_fd () in
	Array.set unix_fds vt_fd (Some oc_fd);
	fd_info.(vt_fd).fname <- path;
	fd_info.(vt_fd).dirp_offset <- 0;
    fd_info.(vt_fd).readdir_eof <- 0;
	(* XXX: canonicalize filename here? *)
	if Hashtbl.mem symbolic_fnames path then
	  Hashtbl.replace symbolic_fds vt_fd
	    (Hashtbl.find symbolic_fnames path);
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
    let (oc_fd1, oc_fd2) = Unix.pipe () in
    let vt_fd1 = self#fresh_fd () in
    Array.set unix_fds vt_fd1 (Some oc_fd1);
    let vt_fd2 = self#fresh_fd () in
    Array.set unix_fds vt_fd2 (Some oc_fd2);
    store_word buf 0 (Int64.of_int vt_fd1);
    store_word buf 4 (Int64.of_int vt_fd2);
    put_return 0L (* success *)

  method sys_pipe2 buf flags =
    let (oc_fd1, oc_fd2) = Unix.pipe () in
    let vt_fd1 = self#fresh_fd () in
      Array.set unix_fds vt_fd1 (Some oc_fd1);
    let vt_fd2 = self#fresh_fd () in
      Array.set unix_fds vt_fd2 (Some oc_fd2);
    store_word buf 0 (Int64.of_int vt_fd1);
    store_word buf 4 (Int64.of_int vt_fd2);
    if (flags land 0o4000 <> 0) then (* O_NONBLOCK *)
      (Unix.set_nonblock oc_fd1;
      Unix.set_nonblock oc_fd2);
    if (flags land 0o2000000 <> 0) then (* O_CLOEXEC *)
      (Unix.set_close_on_exec oc_fd1;
      Unix.set_close_on_exec oc_fd2);
    put_return 0L (* success *)

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

  method private read_throw fd buf count =
    let str = self#string_create count in
    let oc_fd = self#get_fd fd in
    let num_read = Unix.read oc_fd str 0 count in
      if num_read > 0 && Hashtbl.mem symbolic_fds fd then
	let is_concolic = Hashtbl.find symbolic_fds fd in
	  fm#maybe_start_symbolic
	    (fun () ->
	       (if is_concolic then
		  fm#store_concolic_cstr buf (String.sub str 0 num_read) false
		else
		  fm#make_symbolic_region buf num_read;
		max_input_string_length :=
		  max (!max_input_string_length) num_read))
      else
	fm#store_str buf 0L (String.sub str 0 num_read);
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
	assert(not (Hashtbl.mem symbolic_fds fd)); (* unimplemented *)
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
	if num_read > 0 && Hashtbl.mem symbolic_fds sockfd then    
	  fm#maybe_start_symbolic
	    (fun () -> (fm#make_symbolic_region buf num_read;
			max_input_string_length :=
			  max (!max_input_string_length) num_read))
	else
	  fm#store_str buf 0L (String.sub str 0 num_read);
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
	Unix.recvfrom (self#get_fd sockfd) str 0 len flags
      in
	if num_read > 0 && Hashtbl.mem symbolic_fds sockfd then
	  fm#maybe_start_symbolic
	    (fun () -> (fm#make_symbolic_region buf num_read;
			max_input_string_length :=
			  max (!max_input_string_length) num_read))
	else
	  fm#store_str buf 0L (String.sub str 0 num_read);
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

  method sys_recvmsg sockfd msg flags =
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
          Unix.recvfrom (self#get_fd sockfd) str 0 len flags
        in
        assert(not (Hashtbl.mem symbolic_fds sockfd)); (* unimplemented *)
        (if addrbuf <> 0L then
           let addrlen_ptr = load_word (lea msg 0 0 4) in
           self#write_sockaddr sockaddr addrbuf addrlen_ptr);
        self#scatter_iovec iov cnt str;
        put_return (Int64.of_int num_read) (* success *)	)
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

  method sys_set_tid_address addr =
    let pid = self#get_pid in
      put_return (Int64.of_int pid)

  method sys_set_tls tp_value =
    store_word 0xffff0ff0L 0 tp_value;
    put_reg R_TPIDRURO tp_value;
    put_return 0L (* success *)

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
        Array.set unix_fds vt_fd (Some oc_fd);
        put_return (Int64.of_int vt_fd))
      else (
        let vt_fd = self#fresh_fd () in
        netlink_sim_sockfd := vt_fd;
        (* Plugging in a placeholder into unix_fds *)
        Array.set unix_fds vt_fd (Some Unix.stdin);
        put_return (Int64.of_int vt_fd))
    with 
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_stat path buf_addr =
    try
      let oc_buf = Unix.stat (chroot path) in
	self#write_oc_statbuf_as_stat buf_addr oc_buf;
	put_return 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_lstat path buf_addr =
    try
      let oc_buf = Unix.lstat (chroot path) in
	self#write_oc_statbuf_as_stat buf_addr oc_buf;
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
	self#write_oc_statbuf_as_stat buf_addr oc_buf;
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
    self#write_fake_statfs_buf buf;
    put_return 0L (* success *)

  method sys_fstatfs fd buf =
    ignore(fd);
    self#write_fake_statfs_buf buf;
    put_return 0L (* success *)

  method sys_statfs64 path buf_len struct_buf =
    assert(buf_len = 84 || buf_len = 88); (* Same layout, different padding *)
    self#write_fake_statfs64buf struct_buf;
    put_return 0L (* success *)

  method sys_fsync fd =
    ignore(fd);
    put_return 0L (* success *)

  method sys_tgkill tgid tid signal : unit =
    let my_pid = self#get_pid in
      if tgid = my_pid && tid = my_pid && signal = 6 then
	raise SimulatedAbort;
      failwith "Unhandled args to tgkill (not abort())"

  method sys_time addr =
    let time = Int64.of_float (Unix.time ()) in
      if addr != 0L then
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
		"2.6.32-5-amd64"; (* release *)
		"#1 SMP Fri Mar 27 04:02:59 UTC 2011"; (* version *)
		"i686"; (* machine *)
		"example.com" (* domain *)
	       ]
	   | (false, X64) ->
	       ["Linux"; (* sysname *)
		nodename; (* nodename *)
		"2.6.32-5-amd64"; (* release *)
		"#1 SMP Fri Mar 27 04:02:59 UTC 2011"; (* version *)
		"x86_64"; (* machine *)
		"example.com" (* domain *)
	       ]
	   | (false, ARM) ->
	       ["Linux"; (* sysname *)
		nodename; (* nodename *)
		"2.6.32-5-versatile"; (* release *)
		"#1 Wed Jun 15 07:34:48 UTC 2011"; (* version *)
		"armv5tejl"; (* machine *)
		"example.com" (* domain *)
	       ]
	);
      put_return 0L (* success *)

  method sys_alarm sec =
    try
      let ret = Unix.alarm sec in
      put_return (Int64.of_int ret)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

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

  method private iovec_size iov cnt =
    let sum = ref 0 in
      for i = 0 to cnt - 1 do
	let len = Int64.to_int (load_word (lea iov i 8 4)) in
	  sum := !sum + len
      done;
      !sum

  method private scatter_iovec iov cnt buf =
    let off = ref 0 in
      for i = 0 to cnt - 1 do
	let base = load_word (lea iov i 8 0) and
	    len = Int64.to_int (load_word (lea iov i 8 4))
	in
	  fm#store_str base 0L (String.sub buf !off len);
	  off := !off + len
      done

  method private gather_iovec iov cnt =
    Array.concat
      (Vine_util.mapn
	 (fun i -> read_buf
	    (load_word (lea iov i 8 0)) (* iov_base *)
	    (Int64.to_int (load_word (lea iov i 8 4)))) (* iov_len *)
	 (cnt - 1))

  method sys_writev fd iov cnt =
    let bytes = self#gather_iovec iov cnt in
      self#do_write fd bytes (Array.length bytes)

  method private handle_linux_syscall () =
    let get_reg r = 
      if !opt_symbolic_syscall_error <> None then
	fm#get_word_var r (* fail if not concrete *)
      else
	fm#get_word_var_concretize r
	  !opt_measure_influence_syscall_args "syscall arg"
    in
    let uh s = raise (UnhandledSysCall(s))
    in
    let (callnum_reg, arg_regs, ret_reg) = match !opt_arch with
      |	X86 -> (R_EAX, [| R_EBX; R_ECX; R_EDX; R_ESI; R_EDI; R_EBP |], R_EAX)
      | ARM -> (R7, [| R0; R1; R2; R3; R4; R5; R6 |], R0)
      | X64 -> failwith "64-bit syscalls not supported"

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
	 | (_, 0) -> (* restart_syscall *)
	     uh "Unhandled Linux system call restart_syscall (0)"
	 | (_, 1) -> (* exit *)
	     let arg1 = read_1_reg () in
	     let status = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "exit(%Ld) (no return)\n" status;
	       self#sys_exit status
	 | (_, 2) -> (* fork *)
	     uh "Unhandled Linux system call fork (2)"
	 | (_, 3) -> (* read *)		    
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "read(%d, 0x%08Lx, %d)" fd buf count;
	       self#sys_read fd buf count;
	 | (_, 4) -> (* write *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "write(%d, 0x%08Lx, %d)\n" fd buf count;
	       let bytes = read_buf buf count in
		 self#sys_write fd bytes count
	 | (_, 5) -> (* open *)
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
	 | (_, 6) -> (* close *)
	     let arg1 = read_1_reg () in
	     let fd = Int64.to_int arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "close(%d)" fd;
	       self#sys_close fd
	 | (ARM, 7) -> uh "No waitpid (7) syscall in Linux/ARM (E)ABI"
	 | (X86, 7) -> (* waitpid *)
	     uh "Unhandled Linux system call waitpid (7)"
	 | (_, 8) -> (* creat *)
	     uh "Unhandled Linux system call creat (8)"
	 | (_, 9) -> (* link *)
	     let (arg1, arg2) = read_2_regs () in
	     let oldpath = fm#read_cstr arg1 and
		 newpath = fm#read_cstr arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "link(\"%s\", \"%s\")" oldpath newpath;
	       self#sys_link oldpath newpath
	 | (ARM, 10) -> uh "Check whether ARM unlink syscall matches x86"
	 | (X86, 10) -> (* unlink *)
	     let ebx = read_1_reg () in
	     let path = fm#read_cstr ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "unlink(\"%s\")" path;
	       self#sys_unlink path
	 | (_, 11) -> (* execve *)
	     uh "Unhandled Linux system call execve (11)"
	 | (ARM, 12) -> uh "Check whether ARM chdir syscall matches x86"
	 | (X86, 12) -> (* chdir *)
	     let ebx = read_1_reg () in
	     let path = fm#read_cstr ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "chdir(\"%s\")" path;
	       self#sys_chdir path
	 | (ARM, 13) -> uh "Check whether ARM time syscall matches x86"
	 | (X86, 13) -> (* time *)
	     let ebx = read_1_reg () in
	     let addr = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "time(0x%08Lx)" addr;
	       self#sys_time addr
	 | (_, 14) -> (* mknod *)
	     uh "Unhandled Linux system call mknod (14)"
	 | (_, 15) -> (* chmod *)
	     let (arg1, arg2) = read_2_regs () in
	     let path = fm#read_cstr arg1 and
		 mode = Int64.to_int arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "chmod(\"%s\", 0o%o)" path mode;
	       self#sys_chmod path mode
	 | (_, 16) -> (* lchown *)
	     uh "Unhandled Linux system call lchown (16)"
	 | (ARM, 17) -> uh "No break (17) syscall in Linux/ARM (E)ABI"
	 | (X86, 17) -> (* break *)
	     uh "Unhandled Linux system call break (17)"
	 | (ARM, 18) -> uh "No oldstat (18) syscall in Linux/ARM (E)ABI"
	 | (X86, 18) -> (* oldstat *)
	     uh "Unhandled Linux system call oldstat (18)"
	 | (ARM, 19) -> uh "Check whether ARM lseek syscall matches x86"
	 | (X86, 19) -> (* lseek *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let (fd: int) = Int64.to_int ebx and
		 offset = ecx and
		 whence = (Int64.to_int edx) in
	       if !opt_trace_syscalls then
		 Printf.printf "lseek(%d, %Ld, %d)" fd offset whence;
	       self#sys_lseek fd offset whence
	 | (_, 20) -> (* getpid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getpid()";
	     self#sys_getpid ()
	 | (_, 21) -> (* mount *)
	     uh "Unhandled Linux system call mount (21)"
	 | (_, 22) -> (* umount *)
	     uh "Unhandled Linux system call umount (22)"
	 | (_, 23) -> (* setuid *)
	     uh "Unhandled Linux system call setuid (23)"
	 | (ARM, 24) -> uh "Check whether ARM getuid syscall matches x86"
	 | (X86, 24) -> (* getuid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getuid()";
	     self#sys_getuid ()
	 | (_, 25) -> (* stime *)
	     uh "Unhandled Linux system call stime (25)"
	 | (_, 26) -> (* ptrace *)
	     uh "Unhandled Linux system call ptrace (26)"
	 | (_, 27) -> (* alarm *)
         let arg = read_1_reg () in
         let sec = Int64.to_int arg in
         if !opt_trace_syscalls then
           Printf.printf "alarm(%d)" sec;
         self#sys_alarm sec
	 | (ARM, 28) -> uh "No oldfstat (28) syscall in Linux/ARM (E)ABI"
	 | (X86, 28) -> (* oldfstat *)
	     uh "Unhandled Linux system call oldfstat (28)"
	 | (_, 29) -> (* pause *)
	     uh "Unhandled Linux system call pause (29)"
	 | (ARM, 30) -> uh "Check whether ARM utime syscall matches x86"
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
	 | (_, 33) -> (* access *)
	     let (arg1, arg2) = read_2_regs () in
	     let path_buf = arg1 and
		 mode     = Int64.to_int arg2 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "access(\"%s\", 0x%x)" path mode;
	       self#sys_access path mode
	 | (_, 34) -> (* nice *)
	     uh "Unhandled Linux system call nice (34)"
	 | (ARM, 35) -> uh "No ftime (35) syscall in Linux/ARM (E)ABI"
	 | (X86, 35) -> (* ftime *)
	     uh "Unhandled Linux system call ftime (35)"
	 | (_, 36) -> (* sync *)
	     uh "Unhandled Linux system call sync (36)"
	 | (_, 37) -> (* kill *)
	     uh "Unhandled Linux system call kill (37)"
	 | (_, 38 )-> (* rename *)
	     let (arg1, arg2) = read_2_regs () in
	     let oldpath = fm#read_cstr arg1 and
		 newpath = fm#read_cstr arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "rename(\"%s\", \"%s\")" oldpath newpath;
	       self#sys_rename oldpath newpath
	 | (ARM, 39) -> uh "Check whether ARM mkdir syscall matches x86"
	 | (X86, 39) -> (* mkdir *)
	     let (ebx, ecx) = read_2_regs () in
	     let path_buf = ebx and
		 mode     = Int64.to_int ecx in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "mkdir(\"%s\", 0x%x)" path mode;
	       self#sys_mkdir path mode
	 | (_, 40) -> (* rmdir *)
	     uh "Unhandled Linux system call rmdir (40)"
	 | (_, 41) -> (* dup *)
	     let arg1 = read_1_reg () in
	     let fd = Int64.to_int arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "dup(%d)" fd;
	       self#sys_dup fd
	 | (ARM, 42) -> uh "Check whether ARM pipe syscall matches x86"
	 | (X86, 42) -> (* pipe *)
	     let ebx = read_1_reg () in
	     let buf = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "pipe(0x%08Lx)" buf;
	       self#sys_pipe buf
	 | (ARM, 43) -> uh "Check whether ARM times syscall matches x86"
	 | (X86, 43) -> (* times *)
	     let ebx = read_1_reg () in
	     let addr = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "times(0x%08Lx)" addr;
	       self#sys_times addr
	 | (ARM, 44) -> uh "No prof (44) syscall in Linux/ARM (E)ABI"
	 | (X86, 44) -> (* prof *)
	     uh "Unhandled Linux system call prof (44)"
	 | (_, 45) -> (* brk *)
	     let arg1 = read_1_reg () in
	     let addr = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "brk(0x%08Lx)" addr;
	       self#sys_brk addr
	 | (_, 46) -> (* setgid *)
	     uh "Unhandled Linux system call setgid (46)"
	 | (ARM, 47) -> uh "Check whether ARM getgid syscall matches x86"
	 | (X86, 47) -> (* getgid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getgid()";
	     self#sys_getgid ()
	 | (ARM, 48) -> uh "No signal (48) syscall in Linux/ARM (E)ABI"
	 | (X86, 48) -> (* signal *)
	     uh "Unhandled Linux system call signal (48)"
	 | (ARM, 49) -> uh "Check whether ARM geteuid syscall matches x86"
	 | (X86, 49) -> (* geteuid *)
	     if !opt_trace_syscalls then
	       Printf.printf "geteuid()";
	     self#sys_geteuid ()
	 | (ARM, 50) -> uh "Check whether ARM getegid syscall matches x86"
	 | (X86, 50) -> (* getegid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getegid()";
	     self#sys_getegid ()
	 | (_, 51) -> (* acct *)
	     uh "Unhandled Linux system call acct (51)"
	 | (_, 52) -> (* umount2 *)
	     uh "Unhandled Linux system call umount2 (52)"
	 | (ARM, 53) -> uh "No lock (53) syscall in Linux/ARM (E)ABI"
	 | (X86, 53) -> (* lock *)
	     uh "Unhandled Linux system call lock (53)"
	 | (_, 54) -> (* ioctl *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd   = Int64.to_int arg1 and
		 req  = arg2 and
		 argp = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "ioctl(%d, 0x%Lx, 0x%08Lx)" fd req argp;
	       self#sys_ioctl fd req argp;
	 | (ARM, 55) -> uh "Check whether ARM fcntl syscall matches x86"
	 | (X86, 55) -> (* fcntl *)
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
	 | (_, 57) -> (* setpgid *)
	     uh "Unhandled Linux system call setpgid (57)"
	 | (ARM, 58) -> uh "No ulimit (58) syscall in Linux/ARM (E)ABI"
	 | (X86, 58) -> (* ulimit *)
	     uh "Unhandled Linux system call ulimit (58)"
	 | (ARM, 59) -> uh "No ulimit (59) syscall in Linux/ARM (E)ABI"
	 | (X86, 59) -> (* oldolduname *)
	     uh "Unhandled Linux system call oldolduname (59)"
	 | (ARM, 60) -> uh "Check whether ARM umask syscall matches x86"
	 | (X86, 60) -> (* umask *)
	     let ebx = read_1_reg () in
	     let mask = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "umask(0o%03o)" mask;
	       self#sys_umask mask;
	 | (_, 61) -> (* chroot *)
	     uh "Unhandled Linux system call chroot (61)"
	 | (_, 62) -> (* ustat *)
	     uh "Unhandled Linux system call ustat (62)"
	 | (_, 63) -> (* dup2 *)
         let (arg1,arg2) = read_2_regs () in
         let fd1 = Int64.to_int arg1 and
             fd2 = Int64.to_int arg2 in
         if !opt_trace_syscalls then
           Printf.printf "dup2(%d,%d)" fd1 fd2;
         self#sys_dup2 fd1 fd2
	 | (ARM, 64) -> uh "Check whether ARM getppid syscall matches x86"
	 | (X86, 64) -> (* getppid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getppid()";
	     self#sys_getppid ()
	 | (ARM, 65) -> uh "Check whether ARM getpgrp syscall matches x86"
	 | (X86, 65) -> (* getpgrp *)
	     if !opt_trace_syscalls then
	       Printf.printf "getpgrp()";
	     self#sys_getpgrp ()
	 | (_, 66) -> (* setsid *)
	     uh "Unhandled Linux system call setsid (66)"
	 | (_, 67) -> (* sigaction *)
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
	 | (_, 71) -> (* setregid *)
	     uh "Unhandled Linux system call setregid (71)"
	 | (_, 72) -> (* sigsuspend *)
	     uh "Unhandled Linux system call sigsuspend (72)"
	 | (_, 73) -> (* sigpending *)
	     uh "Unhandled Linux system call sigpending (73)"
	 | (_, 74) -> (* sethostname *)
	     uh "Unhandled Linux system call sethostname (74)"
	 | (ARM, 75) -> uh "Check whether ARM setrlimit syscall matches x86"
	 | (X86, 75) -> (* setrlimit *)
	     let (ebx, ecx) = read_2_regs () in
	     let resource = Int64.to_int ebx and
		 rlim = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "setrlimit(%d, 0x%08Lx)" resource rlim;
	       self#sys_setrlimit resource rlim
	 | (_, 76) -> (* getrlimit *)
	     uh "Unhandled Linux system call getrlimit (76)"
	 | (_, 77) -> (* getrusage *)
	     let (ebx, ecx) = read_2_regs () in
	     let who = Int64.to_int ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "getrusage(%d, 0x%08Lx)" who buf;
	       self#sys_getrusage who buf
	 | (_, 78) -> (* gettimeofday *)
	     let (arg1, arg2) = read_2_regs () in
	     let timep = arg1 and
		 zonep = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "gettimeofday(0x%08Lx, 0x%08Lx)" timep zonep;
	       self#sys_gettimeofday timep zonep
	 | (_, 79) -> (* settimeofday *)
	     uh "Unhandled Linux system call settimeofday (79)"
	 | (_, 80) -> (* getgroups *)
	     uh "Unhandled Linux system call getgroups (80)"
	 | (_, 81) -> (* setgroups *)
	     uh "Unhandled Linux system call setgroups (81)"
	 | (_, 82) -> (* select *)
	     uh "Unhandled Linux system call select (82)"
	 | (_, 83) -> (* symlink *)
         let (arg1, arg2) = read_2_regs () in
         let target = (fm#read_cstr arg1) and
             linkpath = (fm#read_cstr arg2) in
         if !opt_trace_syscalls then
           Printf.printf "symlink(%s, %s)" target linkpath;
         self#sys_symlink target linkpath
	 | (ARM, 84) -> uh "No oldlstat (84) syscall in Linux/ARM (E)ABI"
	 | (X86, 84) -> (* oldlstat *)
	     uh "Unhandled Linux system call oldlstat (84)"
	 | (_, 85) -> (* readlink *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let path_buf = arg1 and
		 out_buf  = arg2 and
		 buflen   = Int64.to_int arg3 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "readlink(\"%s\", 0x%08Lx, %d)"
		   path out_buf buflen;
	       self#sys_readlink path out_buf buflen
	 | (_, 86) -> (* uselib *)
	     uh "Unhandled Linux system call uselib (86)"
	 | (_, 87) -> (* swapon *)
	     uh "Unhandled Linux system call swapon (87)"
	 | (_, 88) -> (* reboot *)
	     uh "Unhandled Linux system call reboot (88)"
	 | (_, 89) -> (* readdir *)
	     uh "Unhandled Linux system call readdir (89)"
	 | (ARM, 90) -> uh "Check whether ARM mmap (90) syscall matches x86"
	 | (X86, 90) -> (* mmap *)
	     let ebx = read_1_reg () in
	     let addr   = load_word ebx and
		 length = load_word (lea ebx 0 0 4) and
		 prot   = load_word (lea ebx 0 0 8) and
		 flags  = load_word (lea ebx 0 0 12) and
		 fd     = load_word (lea ebx 0 0 16) and
		 offset = Int64.to_int (load_word (lea ebx 0 0 20)) in
	       if !opt_trace_syscalls then
		 Printf.printf "mmap(0x%08Lx, %Ld, 0x%Lx, 0x%0Lx, %Ld, %d)"
		   addr length prot flags fd offset;
	       self#sys_mmap addr length prot flags fd offset
	 | (_, 91) -> (* munmap *)
	     let (arg1, arg2) = read_2_regs () in
	     let addr = arg1 and
		 len  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "munmap(0x%08Lx, %Ld)" addr len;
	       self#sys_munmap addr len
	 | (_, 92) -> (* truncate *)
	     uh "Unhandled Linux system call truncate (92)"
	 | (_, 93) -> (* ftruncate *)
	     uh "Unhandled Linux system call ftruncate (93)"
	 | (ARM, 94) -> uh "Check whether ARM fchmod syscall matches x86"
	 | (X86, 94) -> (* fchmod *)
	     let (ebx, ecx) = read_2_regs () in
	     let fd = Int64.to_int ebx and
		 mode = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "fchmod(%d, 0o%03o)" fd mode;
	       self#sys_fchmod fd mode
	 | (_, 95) -> (* fchown *)
	     uh "Unhandled Linux system call fchown (95)"
	 | (_, 96) -> (* getpriority *)
	     uh "Unhandled Linux system call getpriority (96)"
	 | (_, 97) -> (* setpriority *)
	     uh "Unhandled Linux system call setpriority (97)"
	 | (ARM, 98) -> uh "No profil (98) syscall in Linux/ARM (E)ABI"
	 | (X86, 98) -> (* profil *)
	     uh "Unhandled Linux system call profil (98)"
	 | (ARM, 99) -> uh "Check whether ARM statfs syscall matches x86"
	 | (X86, 99) -> (* statfs *)
	     let (ebx, ecx) = read_2_regs () in
	     let path = fm#read_cstr ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "statfs(\"%s\", 0x%08Lx)" path buf;
	       self#sys_statfs path buf
	 | (ARM, 100) -> uh "Check whether ARM fstatfs syscall matches x86"
	 | (X86, 100) -> (* fstatfs *)
	     let (ebx, ecx) = read_2_regs () in
	     let fd = Int64.to_int ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "fstatfs(%d, 0x%08Lx)" fd buf;
	       self#sys_fstatfs fd buf
	 | (ARM, 101) -> uh "No ioperm (101) syscall in Linux/ARM (E)ABI"
	 | (X86, 101) -> (* ioperm *)
	     uh "Unhandled Linux system call ioperm (101)"
	 | (ARM, 102) -> uh "Check whether ARM socketcall syscall matches x86"
	 | (X86, 102) -> (* socketcall *)
	     let (ebx, ecx) = read_2_regs () in
	     let call = Int64.to_int ebx and
		 args = ecx in
	       (match call with
		  | 1 -> 
		      let dom_i = Int64.to_int (load_word args) and
			  typ_i = Int64.to_int (load_word (lea args 0 0 4)) and
			  prot_i = Int64.to_int (load_word (lea args 0 0 8)) in
			if !opt_trace_syscalls then
			  Printf.printf "socket(%d, %d, %d)"
			    dom_i typ_i prot_i;
			self#sys_socket dom_i typ_i prot_i
		  | 2 ->
		      let sockfd = Int64.to_int (load_word args) and
			  addr = load_word (lea args 0 0 4) and
			  addrlen = Int64.to_int (load_word (lea args 0 0 8))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "bind(%d, 0x%08Lx, %d)"
			    sockfd addr addrlen;
			self#sys_bind sockfd addr addrlen
		  | 3 -> 
		      let sockfd = Int64.to_int (load_word args) and
			  addr = load_word (lea args 0 0 4) and
			  addrlen = Int64.to_int (load_word (lea args 0 0 8))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "connect(%d, 0x%08Lx, %d)"
			    sockfd addr addrlen;
			self#sys_connect sockfd addr addrlen
		  | 4 -> 
		      let sockfd = Int64.to_int (load_word args) and
			  backlog = Int64.to_int (load_word (lea args 0 0 4))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "listen(%d, %d)" sockfd backlog;
			self#sys_listen sockfd backlog
          | 5 ->
              let sockfd = Int64.to_int (load_word args) and
              addr = load_word (lea args 0 0 4) and
              addrlen_ptr = load_word (lea args 0 0 8)
		      in
			  if !opt_trace_syscalls then
			    Printf.printf "accept(%d, 0x%08Lx, 0x%08Lx)"
			      sockfd addr addrlen_ptr;
			  self#sys_accept sockfd addr addrlen_ptr
		  | 6 ->
		      let sockfd = Int64.to_int (load_word args) and
			  addr = load_word (lea args 0 0 4) and
			  addrlen_ptr = load_word (lea args 0 0 8)
		      in
			if !opt_trace_syscalls then
			  Printf.printf "getsockname(%d, 0x%08Lx, 0x%08Lx)"
			    sockfd addr addrlen_ptr;
			self#sys_getsockname sockfd addr addrlen_ptr
		  | 7 -> 
		      let sockfd = Int64.to_int (load_word args) and
			  addr = load_word (lea args 0 0 4) and
			  addrlen_ptr = load_word (lea args 0 0 8)
		      in
			if !opt_trace_syscalls then
			  Printf.printf "getpeername(%d, 0x%08Lx, 0x%08Lx)"
			    sockfd addr addrlen_ptr;
			self#sys_getpeername sockfd addr addrlen_ptr
		  | 8 -> 
              let dom_i = Int64.to_int (load_word args) and
                  typ_i = Int64.to_int (load_word (lea args 0 0 4)) and
                  prot_i = Int64.to_int (load_word (lea args 0 0 8)) and
                  addr = load_word (lea args 0 0 12) in
              if !opt_trace_syscalls then
                Printf.printf "socketpair(%d, %d, %d, 0x%08Lx)"
                  dom_i typ_i prot_i addr;
              self#sys_socketpair dom_i typ_i prot_i addr
		  | 9 ->
		      let sockfd = Int64.to_int (load_word args) and
			  buf = load_word (lea args 0 0 4) and
			  len = Int64.to_int (load_word (lea args 0 0 8)) and
			  flags = Int64.to_int (load_word (lea args 0 0 12))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "send(%d, 0x%08Lx, %d, %d)"
			    sockfd buf len flags;
			self#sys_send sockfd buf len flags
		  | 10 ->
		      let sockfd = Int64.to_int (load_word args) and
			  buf = load_word (lea args 0 0 4) and
			  len = Int64.to_int (load_word (lea args 0 0 8)) and
			  flags = Int64.to_int (load_word (lea args 0 0 12))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "recv(%d, 0x%08Lx, %d, %d)"
			    sockfd buf len flags;
			self#sys_recv sockfd buf len flags
		  | 11 ->
		      let sockfd = Int64.to_int (load_word args) and
			  buf = load_word (lea args 0 0 4) and
			  len = Int64.to_int (load_word (lea args 0 0 8)) and
			  flags = Int64.to_int (load_word (lea args 0 0 12))
									  and
			  addr = load_word (lea args 0 0 16) and
			  addrlen = Int64.to_int (load_word (lea args 0 0 20))
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "sendto(%d, 0x%08Lx, %d, %d, 0x%08Lx, %d)"
			    sockfd buf len flags addr addrlen;
			self#sys_sendto sockfd buf len flags addr addrlen
		  | 12 ->
		      let sockfd = Int64.to_int (load_word args) and
			  buf = load_word (lea args 0 0 4) and
			  len = Int64.to_int (load_word (lea args 0 0 8)) and
			  flags = Int64.to_int (load_word (lea args 0 0 12))
									  and
			  addr = load_word (lea args 0 0 16) and
			  addrlen_ptr = load_word (lea args 0 0 20)
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "recvfrom(%d, 0x%08Lx, %d, %d, 0x%08Lx, 0x%08Lx)"
			    sockfd buf len flags addr addrlen_ptr;
			self#sys_recvfrom sockfd buf len flags addr addrlen_ptr
		  | 13 ->
              let sockfd = Int64.to_int (load_word args) and
                  how = Int64.to_int (load_word(lea args 0 0 4)) in
              if !opt_trace_syscalls then
                Printf.printf "shutdown(%d, %d)" sockfd how;
              self#sys_shutdown sockfd how 
		  | 14 ->
		      let sockfd = Int64.to_int (load_word args) and
			  level = Int64.to_int (load_word (lea args 0 0 4)) and
			  name = Int64.to_int (load_word (lea args 0 0 8)) and
			  valp = load_word (lea args 0 0 12) and
			  len = Int64.to_int (load_word (lea args 0 0 16))
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "setsockopt(%d, %d, %d, 0x%08Lx, %d)"
			    sockfd level name valp len;
			self#sys_setsockopt sockfd level name valp len
		  | 15 ->
              let sockfd = Int64.to_int (load_word args) and
			  level = Int64.to_int (load_word (lea args 0 0 4)) and
			  name = Int64.to_int (load_word (lea args 0 0 8)) and
			  valp = load_word (lea args 0 0 12) and
			  lenp = load_word (lea args 0 0 16)
		      in
			if !opt_trace_syscalls then
			  Printf.printf
			    "getsockopt(%d, %d, %d, 0x%08Lx, 0x%08Lx)"
			    sockfd level name valp lenp;
			self#sys_getsockopt sockfd level name valp lenp

		  | 16 ->
		      let sockfd = Int64.to_int (load_word args) and
			  msg = load_word (lea args 0 0 4) and
			  flags = Int64.to_int (load_word (lea args 0 0 8))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "sendmsg(%d, 0x%08Lx, %d)"
			    sockfd msg flags;
			self#sys_sendmsg sockfd msg flags
		  | 17 ->
		      let sockfd = Int64.to_int (load_word args) and
			  msg = load_word (lea args 0 0 4) and
			  flags = Int64.to_int (load_word (lea args 0 0 8))
		      in
			if !opt_trace_syscalls then
			  Printf.printf "recvmsg(%d, 0x%08Lx, %d)"
			    sockfd msg flags;
			self#sys_recvmsg sockfd msg flags
		  | 18 -> uh"Unhandled Linux system call accept4 (102:18)"
		  | _ -> self#put_errno Unix.EINVAL)
	 | (_, 103) -> (* syslog *)
	     uh "Unhandled Linux system call syslog (103)"
	 | (_, 104) -> (* setitimer *)
	     uh "Unhandled Linux system call setitimer (104)"
	 | (_, 105) -> (* getitimer *)
	     uh "Unhandled Linux system call getitimer (105)"
	 | (ARM, 106) -> uh "Check whether ARM stat (106) syscall matches x86"
	 | (X86, 106) -> (* stat *)
	     let (ebx, ecx) = read_2_regs () in
	     let path_buf = ebx and
		 buf_addr = ecx in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "stat(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_stat path buf_addr
	 | (ARM, 107) -> uh "Check whether ARM lstat (107) syscall matches x86"
	 | (X86, 107) -> (* lstat *)
	     let (ebx, ecx) = read_2_regs () in
	     let path_buf = ebx and
		 buf_addr = ecx in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "lstat(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_lstat path buf_addr
	 | (ARM, 108) -> uh "Check whether ARM fstat (108) syscall matches x86"
	 | (X86, 108) -> (* fstat *)
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
	 | (_, 111) -> (* vhangup *)
	     uh "Unhandled Linux system call vhangup (111)"
	 | (ARM, 112) -> uh "No idle (112) syscall in Linux/ARM (E)ABI"
	 | (X86, 112) -> (* idle *)
	     uh "Unhandled Linux system call idle (112)"
	 | (X86, 113) -> (* vm86old *)
	     uh "Unhandled Linux/x86 system call vm86old (113)"
	 | (ARM, 113) -> (* syscall *)
	     uh "Unhandled Linux/x86 system call syscall (113)"
	 | (_, 114) -> (* wait4 *)
	     uh "Unhandled Linux system call wait4 (114)"
	 | (_, 115) -> (* swapoff *)
	     uh "Unhandled Linux system call swapoff (115)"
	 | (_, 116) -> (* sysinfo *)
	     uh "Unhandled Linux system call sysinfo (116)"
	 | (_, 117) -> (* ipc *)
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
	 | (_, 118) -> (* fsync *)
	     let arg1 = read_1_reg () in
	     let fd = Int64.to_int arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "fsync(%d)" fd;
	       self#sys_fsync fd
	 | (_, 119) -> (* sigreturn *)
	     uh "Unhandled Linux system call sigreturn (119)"
	 | (_, 120) -> (* clone *)
	     uh "Unhandled Linux system call clone (120)"
	 | (_, 121) -> (* setdomainname *)
	     uh "Unhandled Linux system call setdomainname (121)"
	 | (_, 122) -> (* uname *)
	     let arg1 = read_1_reg () in
	     let buf = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "uname(0x%08Lx)" buf;
	       self#sys_uname buf
	 | (ARM, 123) -> uh "No modify_ldt (112) syscall in Linux/ARM (E)ABI"
	 | (X86, 123) -> (* modify_ldt *)
	     uh "Unhandled Linux system call modify_ldt (123)"
	 | (_, 124) -> (* adjtimex *)
	     uh "Unhandled Linux system call adjtimex (124)"
	 | (_, 125) -> (* mprotect *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let addr = arg1 and
		 len  = arg2 and
		 prot = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "mprotect(0x%08Lx, %Ld, %Ld)" addr len prot;
	       self#sys_mprotect addr len prot
	 | (_, 126) -> (* sigprocmask *)
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
	 | (_, 128) -> (* init_module *)
	     uh "Unhandled Linux system call init_module (128)"
	 | (_, 129) -> (* delete_module *)
	     uh "Unhandled Linux system call delete_module (129)"
	 | (ARM, 130) -> uh "No get_kernel_syms syscall in Linux/ARM (E)ABI"
	 | (X86, 130) -> (* get_kernel_syms *)
	     uh "Unhandled Linux system call get_kernel_syms (130)"
	 | (_, 131) -> (* quotactl *)
	     uh "Unhandled Linux system call quotactl (131)"
	 | (ARM, 132) -> uh "Check whether ARM getpgid syscall matches x86"
	 | (X86, 132) -> (* getpgid *)
	     let ebx = read_1_reg () in
	     let pid = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "getpgid()";
	       self#sys_getpgid pid
	 | (ARM, 133) -> uh "Check whether ARM fchdir syscall matches x86"
	 | (X86, 133) -> (* fchdir *)
	     let ebx = read_1_reg () in
	     let fd = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "fchdir(%d)" fd;
	       self#sys_fchdir fd
	 | (_, 134) -> (* bdflush *)
	     uh "Unhandled Linux system call bdflush (134)"
	 | (_, 135) -> (* sysfs *)
	     uh "Unhandled Linux system call sysfs (135)"
	 | (_, 136) -> (* personality *)
	     uh "Unhandled Linux system call personality (136)"
	 | (ARM, 137) -> uh "No afs_syscall syscall in Linux/ARM (E)ABI"
	 | (X86, 137) -> (* afs_syscall *)
	     uh "Unhandled Linux system call afs_syscall (137)"
	 | (_, 138) -> (* setfsuid *)
	     uh "Unhandled Linux system call setfsuid (138)"
	 | (_, 139) -> (* setfsgid *)
	     uh "Unhandled Linux system call setfsgid (139)"
	 | (_, 140) -> (* _llseek *)
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
	 | (X86, 141) -> (* getdents *)
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
	 | (_, 143) -> (* flock *)
	     uh "Unhandled Linux system call flock (143)"
	 | (_, 144) -> (* msync *)
	     uh "Unhandled Linux system call msync (144)"
	 | (_, 145) -> (* readv *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd  = Int64.to_int arg1 and
		 iov = arg2 and
		 cnt = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "readv(%d, 0x%08Lx, %d)" fd iov cnt;
	       self#sys_readv fd iov cnt
	 | (_, 146) -> (* writev *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd  = Int64.to_int arg1 and
		 iov = arg2 and
		 cnt = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "writev(%d, 0x%08Lx, %d)" fd iov cnt;
	       self#sys_writev fd iov cnt
	 | (ARM, 147) -> uh "Check whether ARM getsid syscall matches x86"
	 | (X86, 147) -> (* getsid *)
	     if !opt_trace_syscalls then
	       Printf.printf "getsid()";
	     self#sys_getsid ()
	 | (_, 148) -> (* fdatasync *)
	     uh "Unhandled Linux system call fdatasync (148)"
	 | (_, 149) -> (* _sysctl *)
	     uh "Unhandled Linux system call _sysctl (149)"
	 | (_, 150) -> (* mlock *)
	     uh "Unhandled Linux system call mlock (150)"
	 | (_, 151) -> (* munlock *)
	     uh "Unhandled Linux system call munlock (151)"
	 | (_, 152) -> (* mlockall *)
	     uh "Unhandled Linux system call mlockall (152)"
	 | (_, 153) -> (* munlockall *)
	     uh "Unhandled Linux system call munlockall (153)"
	 | (_, 154) -> (* sched_setparam *)
	     uh "Unhandled Linux system call sched_setparam (154)"
	 | (ARM, 155) -> uh "Check whether ARM sched_getparam matches x86"
	 | (X86, 155) -> (* sched_getparam *)
	     let (ebx, ecx) = read_2_regs () in
	     let pid = Int64.to_int ebx and
		 buf = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_getparam(%d, 0x%08Lx)" pid buf;
	       self#sys_sched_getparam pid buf
	 | (_, 156) -> (* sched_setscheduler *)
	     uh "Unhandled Linux system call sched_setscheduler (156)"
	 | (ARM, 157) -> uh "Check whether ARM sched_getscheduler matches x86"
	 | (X86, 157) -> (* sched_getscheduler *)
	     let ebx = read_1_reg () in
	     let pid = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_getscheduler(%d)" pid;
	       self#sys_sched_getscheduler pid
	 | (_, 158) -> (* sched_yield *)
	     uh "Unhandled Linux system call sched_yield (158)"
	 | (ARM, 159) -> uh "Check whether ARM sched_get_priority_max matches x86"
	 | (X86, 159) -> (* sched_get_priority_max *)
	     let ebx = read_1_reg () in
	     let policy = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_get_priority_max(%d)" policy;
	       self#sys_sched_get_priority_max policy
	 | (ARM, 160) -> uh "Check whether ARM sched_get_priority_min matches x86"
	 | (X86, 160) -> (* sched_get_priority_min *)
	     let ebx = read_1_reg () in
	     let policy = Int64.to_int ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "sched_get_priority_min(%d)" policy;
	       self#sys_sched_get_priority_min policy
	 | (_, 161) -> (* sched_rr_get_interval *)
	     uh "Unhandled Linux system call sched_rr_get_interval (161)"
	 | (_, 162) -> (* nanosleep *)
	     uh "Unhandled Linux system call nanosleep (162)"
	 | (_, 163) -> (* mremap *)
	     uh "Unhandled Linux system call mremap (163)"
	 | (_, 164) -> (* setresuid *)
	     uh "Unhandled Linux system call setresuid (164)"
	 | (_, 165) -> (* getresuid *)
	     uh "Unhandled Linux system call getresuid (165)"
	 | (ARM, 166) -> uh "No vm86 syscall in Linux/ARM (E)ABI"
	 | (X86, 166) -> (* vm86 *)
	     uh "Unhandled Linux system call vm86 (166)"
	 | (ARM, 167) -> uh "No query_module syscall in Linux/ARM (E)ABI"
	 | (X86, 167) -> (* query_module *)
	     uh "Unhandled Linux system call query_module (167)"
	 | (_, 168) -> (* poll *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fds_buf = arg1 and
		 nfds = Int64.to_int arg2 and
		 timeout = arg3 in
	       if !opt_trace_syscalls then	
		 Printf.printf "poll(0x%08Lx, %d, %Ld)" fds_buf nfds timeout;
	       self#sys_poll fds_buf nfds timeout
	 | (_, 169) -> (* nfsservctl *)
	     uh "Unhandled Linux system call nfsservctl (169)"
	 | (_, 170) -> (* setresgid *)
	     uh "Unhandled Linux system call setresgid (170)"
	 | (_, 171) -> (* getresgid *)
	     uh "Unhandled Linux system call getresgid (171)"
	 | (_, 172) -> (* prctl *)
	     uh "Unhandled Linux system call prctl (172)"
	 | (_, 173) -> (* rt_sigreturn *)
	     uh "Unhandled Linux system call rt_sigreturn (173)"
	 | (_, 174) -> (* rt_sigaction *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let signum = Int64.to_int arg1 and
		 newbuf = arg2 and
		 oldbuf = arg3 and
		 setlen = Int64.to_int arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "rt_sigaction(%d, 0x%08Lx, 0x%08Lx, %d)"
		   signum newbuf oldbuf setlen;
	       self#sys_rt_sigaction signum newbuf oldbuf setlen
	 | (_, 175) -> (* rt_sigprocmask *)
	     let (arg1, arg2, arg3, arg4) = read_4_regs () in
	     let how    = Int64.to_int arg1 and
		 newset = arg2 and
		 oldset = arg3 and
		 setlen = Int64.to_int arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "rt_sigprocmask(%d, 0x%08Lx, 0x%08Lx, %d)"
		   how newset oldset setlen;
	       self#sys_rt_sigprocmask how newset oldset setlen
	 | (_, 176) -> (* rt_sigpending *)
	     uh "Unhandled Linux system call rt_sigpending (176)"
	 | (_, 177) -> (* rt_sigtimedwait *)
	     uh "Unhandled Linux system call rt_sigtimedwait (177)"
	 | (_, 178) -> (* rt_sigqueueinfo *)
	     uh "Unhandled Linux system call rt_sigqueueinfo (178)"
	 | (_, 179) -> (* rt_sigsuspend *)
	     uh "Unhandled Linux system call rt_sigsuspend (179)"
	 | (_, 180) -> (* pread64 *)
	     let (arg1, arg2, arg3, arg4, arg5) = read_5_regs () in
	     let fd    = Int64.to_int arg1 and
		 buf   = arg2 and
		 count = Int64.to_int arg3 and
		 off   = Int64.logor (Int64.shift_left arg5 32) arg4 in
	       if !opt_trace_syscalls then
		 Printf.printf "pread64(%d, 0x%08Lx, %d, %Ld)"
		   fd buf count off;
	       self#sys_pread64 fd buf count off;
	 | (_, 181) -> (* pwrite64 *)
	     uh "Unhandled Linux system call pwrite64 (181)"
	 | (_, 182) -> (* chown *)
	     uh "Unhandled Linux system call chown (182)"
	 | (ARM, 183) -> uh "Check whether ARM getcwd syscall matches x86"
	 | (X86, 183) -> (* getcwd *)
	     let (ebx, ecx) = read_2_regs () in
	     let buf = ebx and
		 size = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "getcwd(0x%08Lx, %d)" buf size;
	       self#sys_getcwd buf size
	 | (ARM, 184) -> uh "Check whether ARM capget syscall matches x86"
	 | (X86, 184) -> (* capget *)
	     let (ebx, ecx) = read_2_regs () in
	     let hdrp = ebx and
		 datap = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "capget(0x%08Lx, 0x%08Lx)" hdrp datap;
	       self#sys_capget hdrp datap
	 | (_, 185) -> (* capset *)
	     uh "Unhandled Linux system call capset (185)"
	 | (_, 186) -> (* sigaltstack *)
	     let (arg1, arg2) = read_2_regs () in
	     let new_stack_t = arg1 and
		 old_stack_t = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "sigaltstack(0x%08Lx, 0x%08Lx)"
		   new_stack_t old_stack_t;
	       self#sys_sigaltstack new_stack_t old_stack_t
	 | (_, 187) -> (* sendfile *)
	     uh "Unhandled Linux system call sendfile (187)"
	 | (ARM, 188) -> uh "No getpmsg (188) syscall in Linux/ARM (E)ABI"
	 | (X86, 188) -> (* getpmsg *)
	     uh "Unhandled Linux system call getpmsg (188)"
	 | (ARM, 189) -> uh "No putpmsg (189) syscall in Linux/ARM (E)ABI"
	 | (X86, 189) -> (* putpmsg *)
	     uh "Unhandled Linux system call putpmsg (189)"
	 | (_, 190) -> (* vfork *)
	     uh "Unhandled Linux system call vfork (190)"
	 | (_, 191) -> (* ugetrlimit *)
	     let (arg1, arg2) = read_2_regs () in
	     let rsrc = Int64.to_int arg1 and
		 buf  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "ugetrlimit(%d, 0x%08Lx)" rsrc buf;
	       self#sys_ugetrlimit rsrc buf
	 | (_, 192) -> (* mmap2 *)
	     let (arg1, arg2, arg3, arg4, arg5, arg6) = read_6_regs () in
	     let addr     = arg1 and
		 length   = arg2 and
		 prot     = arg3 and
		 flags    = arg4 and
		 fd       = arg5 and
		 pgoffset = Int64.to_int arg6 in
	       if !opt_trace_syscalls then
		 Printf.printf "mmap2(0x%08Lx, %Ld, 0x%Lx, 0x%0Lx, %Ld, %d)"
		   addr length prot flags fd pgoffset;
	       self#sys_mmap2 addr length prot flags fd pgoffset
	 | (_, 193) -> (* truncate64 *)
	     uh "Unhandled Linux system call truncate64 (193)"
	 | (_, 194) -> (* ftruncate64 *)
	     uh "Unhandled Linux system call ftruncate64 (194)"
	 | (_, 195) -> (* stat64 *)
	     let (arg1, arg2) = read_2_regs () in
	     let path_buf = arg1 and
		 buf_addr = arg2 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "stat64(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_stat64 path buf_addr
	 | (_, 196) -> (* lstat64 *)
	     let (arg1, arg2) = read_2_regs () in
	     let path_buf = arg1 and
		 buf_addr = arg2 in
	     let path = fm#read_cstr path_buf in
	       if !opt_trace_syscalls then
		 Printf.printf "lstat64(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_lstat64 path buf_addr
	 | (_, 197) -> (* fstat64 *)
	     let (arg1, arg2) = read_2_regs () in
	     let fd = Int64.to_int arg1 and
		 buf_addr = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "fstat64(%d, 0x%08Lx)" fd buf_addr;
	       self#sys_fstat64 fd buf_addr
	 | (_, 198) -> (* lchown32 *)
	     uh "Unhandled Linux system call lchown32 (198)"
	 | (_, 199) -> (* getuid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "getuid32()";
	     self#sys_getuid32 ()
	 | (_, 200) -> (* getgid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "getgid32()";
	     self#sys_getgid32 ()
	 | (_, 201) -> (* geteuid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "geteuid32()";
	     self#sys_geteuid32 ()
	 | (_, 202) -> (* getegid32 *)
	     if !opt_trace_syscalls then
	       Printf.printf "getegid32()";
	     self#sys_getegid32 ()
	 | (_, 203) -> (* setreuid32 *)
	     uh "Unhandled Linux system call setreuid32 (203)"
	 | (_, 204) -> (* setregid32 *)
	     uh "Unhandled Linux system call setregid32 (204)"
	 | (ARM, 205) -> uh "Check whether ARM getgroups32 syscall matches x86"
	 | (X86, 205) -> (* getgroups32 *)
	     let (ebx, ecx) = read_2_regs () in
	     let size = Int64.to_int ebx and
		 list = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "getgroups32(%d, 0x%08Lx)" size list;
	       self#sys_getgroups32 size list
	 | (_, 206) -> (* setgroups32 *)
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
	 | (_, 208) -> (* setresuid32 *)
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
	 | (_, 210) -> (* setresgid32 *)
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
	 | (_, 212) -> (* chown32 *)
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
	 | (_, 215) -> (* setfsuid32 *)
	     uh "Unhandled Linux system call setfsuid32 (215)"
	 | (_, 216) -> (* setfsgid32 *)
	     uh "Unhandled Linux system call setfsgid32 (216)"
	 | (ARM, 218)
	 | (X86, 217) -> (* pivot_root *)
	     uh "Unhandled Linux system call pivot_root"
	 | (ARM, 219) -> uh "Check whether ARM mincore syscall matches x86"
	 | (X86, 218) -> (* mincore *)
	     let (ebx, ecx, edx) = read_3_regs () in
	     let addr = ebx and
		 length = Int64.to_int ecx and
		 vec = edx in
	       if !opt_trace_syscalls then
		 Printf.printf "mincore(0x%08Lx, %d, 0x%08Lx)" addr length vec;
	       self#sys_mincore addr length vec
	 | (ARM, 220)
	 | (X86, 219) -> (* madvise *)
	     uh "Unhandled Linux system call madvise"
	 | (ARM, 217)
	 | (X86, 220) -> (* getdents64 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd = Int64.to_int arg1 and
		 dirp = arg2 and
		 count = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "getdents64(%d, 0x%08Lx, %d)" fd dirp count;
	       self#sys_getdents64 fd dirp count
	 | (_, 221) -> (* fcntl64 *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let fd = Int64.to_int arg1 and
		 cmd = Int64.to_int arg2 and
		 arg = arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "fcntl64(%d, %d, 0x%08Lx)" fd cmd arg;
	       self#sys_fcntl64 fd cmd arg
	 | (_, 222) -> uh "No such Linux syscall 222 (was for tux)"
	 | (_, 223) -> uh "No such Linux syscall 223 (unused)"
	 | (ARM, 224) -> uh "Check whether ARM gettid syscall matches x86"
	 | (X86, 224) -> (* gettid *)
	     if !opt_trace_syscalls then
	       Printf.printf "gettid()";
	     self#sys_gettid 
	 | (_, 225) -> (* readahead *)
	     uh "Unhandled Linux system call readahead (225)"
	 | (_, 226) -> (* setxattr *)
	     uh "Unhandled Linux system call setxattr (226)"
	 | (_, 227) -> (* lsetxattr *)
	     uh "Unhandled Linux system call lsetxattr (227)"
	 | (_, 228) -> (* fsetxattr *)
	     uh "Unhandled Linux system call fsetxattr (228)"
	 | (ARM, 229) -> uh "Check whether ARM getxattr syscall matches x86"
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
	 | (_, 230) -> (* lgetxattr *)
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
	 | (_, 231) -> (* fgetxattr *)
	     uh "Unhandled Linux system call fgetxattr (231)"
	 | (_, 232) -> (* listxattr *)
	     uh "Unhandled Linux system call listxattr (232)"
	 | (_, 233) -> (* llistxattr *)
	     uh "Unhandled Linux system call llistxattr (233)"
	 | (_, 234) -> (* flistxattr *)
	     uh "Unhandled Linux system call flistxattr (234)"
	 | (_, 235) -> (* removexattr *)
	     uh "Unhandled Linux system call removexattr (235)"
	 | (_, 236) -> (* lremovexattr *)
	     uh "Unhandled Linux system call lremovexattr (236)"
	 | (_, 237) -> (* fremovexattr *)
	     uh "Unhandled Linux system call fremovexattr (237)"
	 | (_, 238) -> (* tkill *)
	     uh "Unhandled Linux system call tkill (238)"
	 | (_, 239) -> (* sendfile64 *)
	     uh "Unhandled Linux system call sendfile64 (239)"
	 | (_, 240) -> (* futex *)
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
	 | (_, 241) -> (* sched_setaffinity *)
	     uh "Unhandled Linux system call sched_setaffinity (241)"
	 | (_, 242) -> (* sched_getaffinity *)
	     uh "Unhandled Linux system call sched_getaffinity (242)"
	 (* Here's where the x86 and ARM syscall numbers diverge,
	    because ARM lacks {get,set}_thread_area *)
	 | (X86, 243) -> (* set_thread_area *)
	     let ebx = read_1_reg () in
	     let uinfo = ebx in
	       if !opt_trace_syscalls then
		 Printf.printf "set_thread_area(0x%08Lx)" uinfo;
	       self#sys_set_thread_area uinfo
	 | (X86, 244) -> (* get_thread_area *)
	     uh "Unhandled Linux/x86 system call get_thread_area (244)"
	 | (ARM, 243)    (* io_setup *)
	 | (X86, 245) -> (* io_setup *)
	     uh "Unhandled Linux system call io_setup"
	 | (ARM, 244)    (* io_destroy *)
	 | (X86, 246) -> (* io_destroy *)
	     uh "Unhandled Linux system call io_destroy"
	 | (ARM, 245)    (* io_getevents *)
	 | (X86, 247) -> (* io_getevents *)
	     uh "Unhandled Linux system call io_getevents"
	 | (ARM, 246)    (* io_submit *)
	 | (X86, 248) -> (* io_submit *)
	     uh "Unhandled Linux system call io_submit"
	 | (ARM, 247)    (* io_cancel *)
	 | (X86, 249) -> (* io_cancel *)
	     uh "Unhandled Linux system call io_cancel"
	 | (X86, 250) -> (* fadvise64 *)
	     uh "Unhandled Linux system call fadvise64 (250)"
	 | (ARM, 248)    (* exit_group *)
	 | (X86, 252) -> (* exit_group *)
	     let arg1 = read_1_reg () in
	     let status = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "exit_group(%Ld) (no return)\n" status;
	       self#sys_exit_group status
	 | (ARM, 249)    (* lookup_dcookie *)
	 | (X86, 253) -> (* lookup_dcookie *)
	     uh "Unhandled Linux system call lookup_dcookie"
	 | (ARM, 250)    (* epoll_create *)
	 | (X86, 254) -> (* epoll_create *)
	     uh "Unhandled Linux system call epoll_create"
	 | (ARM, 251)    (* epoll_ctl *)
	 | (X86, 255) -> (* epoll_ctl *)
	     uh "Unhandled Linux system call epoll_ctl"
	 | (ARM, 252)    (* epoll_wait *)
	 | (X86, 256) -> (* epoll_wait *)
	     uh "Unhandled Linux system call epoll_wait"
	 | (ARM, 253)    (* remap_file_pages *)
	 | (X86, 257) -> (* remap_file_pages *)
	     uh "Unhandled Linux system call remap_file_pages"
	 | (ARM, 254) -> uh "No set_thread_area (254) syscall in Linux/ARM (E)ABI"
	 | (ARM, 255) -> uh "No get_thread_area (255) syscall in Linux/ARM (E)ABI"
	 | (ARM, 256)
	 | (X86, 258) -> (* set_tid_address *)
	     let arg1 = read_1_reg () in
	     let addr = arg1 in
	       if !opt_trace_syscalls then
		 Printf.printf "set_tid_address(0x%08Lx)" addr;
	       self#sys_set_tid_address addr
	 | (ARM, 257)    (* timer_create *)
	 | (X86, 259) -> (* timer_create *)
	     uh "Unhandled Linux system call timer_create"
	 | (ARM, 258)    (* timer_settime *)
	 | (X86, 260) -> (* timer_settime *)
	     uh "Unhandled Linux system call timer_settime"
	 | (ARM, 259)    (* timer_gettime *)
	 | (X86, 261) -> (* timer_gettime *)
	     uh "Unhandled Linux system call timer_gettime"
	 | (ARM, 260)    (* timer_getoverrun *)
	 | (X86, 262) -> (* timer_getoverrun *)
	     uh "Unhandled Linux system call timer_getoverrun"
	 | (ARM, 261)    (* timer_delete *)
	 | (X86, 263) -> (* timer_delete *)
	     uh "Unhandled Linux system call timer_delete"
	 | (ARM, 262)    (* clock_settime *)
	 | (X86, 264) -> (* clock_settime *)
	     uh "Unhandled Linux system call clock_settime"
	 | (ARM, 263)
	 | (X86, 265) -> (* clock_gettime *)
	     let (arg1, arg2) = read_2_regs () in
	     let clkid = Int64.to_int arg1 and
		 timep = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "clock_gettime(%d, 0x%08Lx)" clkid timep;
	       self#sys_clock_gettime clkid timep
	 | (ARM, 264) -> uh "Check whether ARM clock_getres matches x86"
	 | (X86, 266) -> (* clock_getres *)
	     let (ebx, ecx) = read_2_regs () in
	     let clkid = Int64.to_int ebx and
		 timep = ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "clock_getres(%d, 0x%08Lx)" clkid timep;
	       self#sys_clock_getres clkid timep
	 | (ARM, 265)    (* clock_nanosleep *)
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
	     uh "Unhandled Linux system call fstatfs64"
	 | (ARM, 268)    (* tgkill *)
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
	 | (X86, 273) -> (* vserver *)
	     uh "Unhandled Linux system call vserver"
	 | (ARM, 319)    (* mbind *)
	 | (X86, 274) -> (* mbind *)
	     uh "Unhandled Linux system call mbind"
	 | (ARM, 320)    (* get_mempolicy *)
	 | (X86, 275) -> (* get_mempolicy *)
	     uh "Unhandled Linux system call get_mempolicy"
	 | (ARM, 321)    (* set_mempolicy *)
	 | (X86, 276) -> (* set_mempolicy *)
	     uh "Unhandled Linux system call set_mempolicy"
	 | (ARM, 274)    (* mq_open *)
	 | (X86, 277) -> (* mq_open *)
	     uh "Unhandled Linux system call mq_open"
	 | (ARM, 275)    (* mq_unlink *)
	 | (X86, 278) -> (* mq_unlink *)
	     uh "Unhandled Linux system call mq_unlink"
	 | (ARM, 276)    (* mq_timedsend *)
	 | (X86, 279) -> (* mq_timedsend *)
	     uh "Unhandled Linux system call mq_timedsend"
	 | (ARM, 277)    (* mq_timedreceive *)
	 | (X86, 280) -> (* mq_timedreceive *)
	     uh "Unhandled Linux system call mq_timedreceive"
	 | (ARM, 278)    (* mq_notify *)
	 | (X86, 281) -> (* mq_notify *)
	     uh "Unhandled Linux system call mq_notify"
	 | (ARM, 279)    (* mq_getsetattr *)
	 | (X86, 282) -> (* mq_getsetattr *)
	     uh "Unhandled Linux system call mq_getsetattr"
	 | (ARM, 347)    (* kexec_load *)
	 | (X86, 283) -> (* kexec_load *)
	     uh "Unhandled Linux system call kexec_load"
	 | (ARM, 280)    (* waitid *)
	 | (X86, 284) -> (* waitid *)
	     uh "Unhandled Linux system call waitid"
	 | (ARM, 281) -> (* socket *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let dom_i = Int64.to_int arg1 and
		 typ_i = Int64.to_int arg2 and
		 prot_i = Int64.to_int arg3 in
	       if !opt_trace_syscalls then
		 Printf.printf "socket(%d, %d, %d)"
		   dom_i typ_i prot_i;
	       self#sys_socket dom_i typ_i prot_i
	 | (ARM, 282) -> (* bind *)
	     uh "Unhandled Linux/ARM system call bind (282, split)"
	 | (ARM, 283) -> (* connect *)
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
	     uh "Unhandled Linux/ARM system call listen (284, split)"
	 | (ARM, 285) -> (* accept *)
	     uh "Unhandled Linux/ARM system call accept (285, split)"
	 | (ARM, 286) -> (* getsockname *)
	     let (arg1, arg2, arg3) = read_3_regs () in
	     let sockfd = Int64.to_int arg1 and
		 addr = load_word arg2 and
		 addrlen_ptr = arg3
	     in
	       if !opt_trace_syscalls then
		 Printf.printf "getsockname(%d, 0x%08Lx, 0x%08Lx)"
		   sockfd addr addrlen_ptr;
	       self#sys_getsockname sockfd addr addrlen_ptr
	 | (ARM, 287) -> (* getpeername *)
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
	     uh "Unhandled Linux/ARM system call socketpair (288, split)"
	 | (ARM, 289) -> (* send *)
	     uh "Unhandled Linux/ARM system call send (289, split)"
	 | (ARM, 290) -> (* sendto *)
	     uh "Unhandled Linux/ARM system call sendto (290, split)"
	 | (ARM, 291) -> (* recv *)
	     uh "Unhandled Linux/ARM system call recv (291, split)"
	 | (ARM, 292) -> (* recvfrom *)
	     uh "Unhandled Linux/ARM system call recvfrom (292, split)"
	 | (ARM, 293) -> (* shutdown *)
	     uh "Unhandled Linux/ARM system call shutdown (293, split)"
	 | (ARM, 294) -> (* setsockopt *)
	     uh "Unhandled Linux/ARM system call setsockopt (294, split)"
	 | (ARM, 295) -> (* getsockopt *)
	     uh "Unhandled Linux/ARM system call getsockopt (295, split)"
	 | (ARM, 296) -> (* sendmsg *)
	     uh "Unhandled Linux/ARM system call sendmsg (296, split)"
	 | (ARM, 297) -> (* recvmsg *)
	     uh "Unhandled Linux/ARM system call recvmsg (297, split)"
	 | (ARM, 298) -> (* semop *)
	     uh "Unhandled Linux/ARM system call semop (298, split)"
	 | (ARM, 299) -> (* semget *)
	     uh "Unhandled Linux/ARM system call semget (299, split)"
	 | (ARM, 300) -> (* semctl *)
	     uh "Unhandled Linux/ARM system call semctl (300, split)"
	 | (ARM, 301) -> (* msgsnd *)
	     uh "Unhandled Linux/ARM system call msgsnd (301, split)"
	 | (ARM, 302) -> (* msgrcv *)
	     uh "Unhandled Linux/ARM system call msgrcv (302, split)"
	 | (ARM, 303) -> (* msgget *)
	     uh "Unhandled Linux/ARM system call msgget (303, split)"
	 | (ARM, 304) -> (* msgctl *)
	     uh "Unhandled Linux/ARM system call msgctl (304, split)"
	 | (ARM, 305) -> (* shmat *)
	     uh "Unhandled Linux/ARM system call shmat (305, split)"
	 | (ARM, 306) -> (* shmdt *)
	     uh "Unhandled Linux/ARM system call shmdt (306, split)"
	 | (ARM, 307) -> (* shmget *)
	     uh "Unhandled Linux/ARM system call shmget (307, split)"
	 | (ARM, 308) -> (* shmctl *)
	     uh "Unhandled Linux/ARM system call shmctl (308, split)"
	 | (ARM, 309)    (* add_key *)
	 | (X86, 286) -> (* add_key *)
	     uh "Unhandled Linux system call add_key"
	 | (ARM, 310)    (* request_key *)
	 | (X86, 287) -> (* request_key *)
	     uh "Unhandled Linux system call request_key"
	 | (ARM, 311)    (* keyctl *)
	 | (X86, 288) -> (* keyctl *)
	     uh "Unhandled Linux system call keyctl"
	 | (ARM, 312) -> (* semtimedop *)
	     uh "Unhandled Linux/ARM system call semtimedop"
	 | (ARM, 314)    (* ioprio_set *)
	 | (X86, 289) -> (* ioprio_set *)
	     uh "Unhandled Linux system call ioprio_set"
	 | (ARM, 315)    (* ioprio_get *)
	 | (X86, 290) -> (* ioprio_get *)
	     uh "Unhandled Linux system call ioprio_get"
	 | (ARM, 316)    (* inotify_init *)
	 | (X86, 291) -> (* inotify_init *)
	     uh "Unhandled Linux system call inotify_init"
	 | (ARM, 317)    (* inotify_add_watch *)
	 | (X86, 292) -> (* inotify_add_watch *)
	     uh "Unhandled Linux system call inotify_add_watch"
	 | (ARM, 318)    (* inotify_rm_watch *)
	 | (X86, 293) -> (* inotify_rm_watch *)
	     uh "Unhandled Linux system call inotify_rm_watch"
	 | (X86, 294) -> (* migrate_pages *)
	     uh "Unhandled Linux/x86 system call migrate_pages (294)"
	 | (ARM, 322)    (* openat *)
	 | (X86, 295) -> (* openat *)
         let (arg1, arg2, arg3) = read_3_regs () in
         let arg4 = (if (Int64.logand arg3 0o100L) <> 0L then
                       get_reg arg_regs.(3)
                     else
                       0L) in
         let dirfd    = Int64.to_int arg1 and
             path_buf = arg2 and
             flags    = Int64.to_int arg3 and
             mode     = Int64.to_int arg4 in
         let path = fm#read_cstr path_buf in
         if !opt_trace_syscalls then
           Printf.printf "openat(%d, \"%s\", 0x%x, 0o%o)" dirfd path flags mode;
         self#sys_openat dirfd path flags mode

	 | (ARM, 323)    (* mkdirat *)
	 | (X86, 296) -> (* mkdirat *)
	     uh "Unhandled Linux system call mkdirat"
	 | (ARM, 324)    (* mknodat *)
	 | (X86, 297) -> (* mknodat *)
	     uh "Unhandled Linux system call mknodat"
	 | (ARM, 325)    (* fchownat *)
	 | (X86, 298) -> (* fchownat *)
	     uh "Unhandled Linux system call fchownat"
	 | (ARM, 326)    (* futimesat *)
	 | (X86, 299) -> (* futimesat *)
	     uh "Unhandled Linux system call futimesat"
	 | (ARM, 327)    (* fstatat64 *)
	 | (X86, 300) -> (* fstatat64 *)
	     uh "Unhandled Linux system call fstatat64"
	 | (ARM, 328)    (* unlinkat *)
	 | (X86, 301) -> (* unlinkat *)
	     uh "Unhandled Linux system call unlinkat"
	 | (ARM, 329)    (* renameat *)
	 | (X86, 302) -> (* renameat *)
	     uh "Unhandled Linux system call renameat"
	 | (ARM, 330)    (* linkat *)
	 | (X86, 303) -> (* linkat *)
	     uh "Unhandled Linux system call linkat"
	 | (ARM, 331)    (* symlinkat *)
	 | (X86, 304) -> (* symlinkat *)
	     uh "Unhandled Linux system call symlinkat"
	 | (ARM, 332)    (* readlinkat *)
	 | (X86, 305) -> (* readlinkat *)
	     uh "Unhandled Linux system call readlinkat"
	 | (ARM, 333)    (* fchmodat *)
	 | (X86, 306) -> (* fchmodat *)
	     uh "Unhandled Linux system call fchmodat"
	 | (ARM, 334)    (* faccessat *)
	 | (X86, 307) -> (* faccessat *)
	     uh "Unhandled Linux system call faccessat"
	 | (ARM, 335)    (* pselect6 *)
	 | (X86, 308) -> (* pselect6 *)
	     uh "Unhandled Linux system call pselect6"
	 | (ARM, 336)    (* ppoll *)
	 | (X86, 309) -> (* ppoll *)
	     uh "Unhandled Linux system call ppoll"
	 | (ARM, 337)    (* unshare *)
	 | (X86, 310) -> (* unshare *)
	     uh "Unhandled Linux system call unshare"
	 | (ARM, 338)
	 | (X86, 311) -> (* set_robust_list *)
	     let (arg1, arg2) = read_2_regs () in
	     let addr = arg1 and
		 len  = arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "set_robust_list(0x%08Lx, %Ld)" addr len;
	       self#sys_set_robust_list addr len
	 | (ARM, 339)    (* get_robust_list *)
	 | (X86, 312) -> (* get_robust_list *)
	     uh "Unhandled Linux system call get_robust_list"
	 | (ARM, 340)    (* splice *)
	 | (X86, 313) -> (* splice *)
	     uh "Unhandled Linux system call splice"
	 | (ARM, 341)    (* sync_file_range *)
	 | (X86, 314) -> (* sync_file_range *)
	     uh "Unhandled Linux system call sync_file_range"
	 | (ARM, 342)    (* tee *)
	 | (X86, 315) -> (* tee *)
	     uh "Unhandled Linux system call tee"
	 | (ARM, 343)    (* vmsplice *)
	 | (X86, 316) -> (* vmsplice *)
	     uh "Unhandled Linux system call vmsplice"
	 | (ARM, 344)    (* move_pages *)
	 | (X86, 317) -> (* move_pages *)
	     uh "Unhandled Linux system call move_pages"
	 | (ARM, 345)    (* getcpu *)
	 | (X86, 318) -> (* getcpu *)
	     uh "Unhandled Linux system call getcpu"
	 | (ARM, 346)    (* epoll_pwait *)
	 | (X86, 319) -> (* epoll_pwait *)
	     uh "Unhandled Linux system call epoll_pwait"
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
	 | (X86, 321) -> (* signalfd *)
	     uh "Unhandled Linux system call signalfd"
	 | (ARM, 350)    (* timerfd_create *)
	 | (X86, 322) -> (* timerfd_create *)
	     uh "Unhandled Linux system call timerfd_create"
	 | (ARM, 351)    (* eventfd *)
	 | (X86, 323) -> (* eventfd *)
	     uh "Unhandled Linux system call eventfd"
	 | (ARM, 352)    (* fallocate *)
	 | (X86, 324) -> (* fallocate *)
	     uh "Unhandled Linux system call fallocate"
	 | (ARM, 353)    (* timerfd_settime *)
	 | (X86, 325) -> (* timerfd_settime *)
	     uh "Unhandled Linux system call timerfd_settime"
	 | (ARM, 354)    (* timerfd_gettime *)
	 | (X86, 326) -> (* timerfd_gettime *)
	     uh "Unhandled Linux system call timerfd_gettime"
	 | (ARM, 355)    (* signalfd4 *)
	 | (X86, 327) -> (* signalfd4 *)
	     uh "Unhandled Linux system call signalfd4"
	 | (ARM, 356)    (* eventfd2 *)
	 | (X86, 328) -> (* eventfd2 *)
	     let (arg1, arg2) = read_2_regs () in
	     let initval = arg1 and
		 flags = Int64.to_int arg2 in
	       if !opt_trace_syscalls then
		 Printf.printf "eventfd2(%Ld, %d)" initval flags;
	       self#sys_eventfd2 initval flags
	 | (ARM, 357)    (* epoll_create1 *)
	 | (X86, 329) -> (* epoll_create1 *)
	     uh "Unhandled Linux system call epoll_create1"
	 | (ARM, 358)    (* dup3 *)
	 | (X86, 330) -> (* dup3 *)
	     uh "Unhandled Linux system call dup3"
	 | (ARM, 359) -> uh "Check whether ARM pipe2 syscall matches x86"
	 | (X86, 331) -> (* pipe2 *)
	     let (ebx, ecx) = read_2_regs () in
	     let buf = ebx and
		 flags = Int64.to_int ecx in
	       if !opt_trace_syscalls then
		 Printf.printf "pipe2(0x%08Lx, %d)" buf flags;
	       self#sys_pipe2 buf flags
	 | (ARM, 360)    (* inotify_init1 *)
	 | (X86, 332) -> (* inotify_init1 *)
	     uh "Unhandled Linux system call inotify_init1"
	 | (ARM, 361)    (* preadv *)
	 | (X86, 333) -> (* preadv *)
	     uh "Unhandled Linux system call preadv"
	 | (ARM, 362)    (* pwritev *)
	 | (X86, 334) -> (* pwritev *)
	     uh "Unhandled Linux system call pwritev"
	 | (ARM, 363)    (* rt_tgsigqueueinfo *)
	 | (X86, 335) -> (* rt_tgsigqueueinfo *)
	     uh "Unhandled Linux system call rt_tgsigqueueinfo"
	 | (ARM, 364)    (* perf_event_open *)
	 | (X86, 336) -> (* perf_event_open *)
	     uh "Unhandled Linux system call perf_event_open"
	 | (ARM, 365) ->
	     uh "No 365 syscall in Linux/ARM (E)ABI"
	 | (ARM, 366) -> (* accept4 *)
	     uh "Unhandled Linux/ARM system call accept4"
	 | (X86, 337) -> (* recvmmsg *)
	     uh "Unhandled Linux system call recvmmsg (337)"
	 | (X86, 338) -> (* fanotify_init *)
	     uh "Unhandled Linux system call fanotify_init (338)"
	 | (X86, 339) -> (* fanotify_mark *)
	     uh "Unhandled Linux system call fanotify_mark (339)"
	 | (X86, 340) -> (* prlimit64 *)
	     uh "Unhandled Linux system call prlimit64 (340)"
	 | (X86, 341) -> (* name_to_handle_at *)
	     uh "Unhandled Linux system call name_to_handle_at (341)"
	 | (X86, 342) -> (* open_by_handle_at *)
	     uh "Unhandled Linux system call open_by_handle_at (342)"
	 | (X86, 343) -> (* clock_adjtime *)
	     uh "Unhandled Linux system call clock_adjtime (343)"
	 | (X86, 344) -> (* syncfs *)
	     uh "Unhandled Linux system call syncfs (344)"
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
	     failwith "64-bit syscalls not supported");
    if !opt_trace_syscalls then
      let ret_val = fm#get_word_var ret_reg in
	Printf.printf " = %Ld (0x%08Lx)\n" (fix_s32 ret_val) ret_val;
	flush stdout

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
	    let sysexit_eip = (Int64.logor 0x430L
				 (Int64.logand 0xfffff000L sysenter_eip)) in
	    let label = "pc_0x" ^ (Printf.sprintf "%08Lx" sysexit_eip) in
	      handle_catch ();
	      Some [V.Jmp(V.Name(label))]
	| _ -> None
end

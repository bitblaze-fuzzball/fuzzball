
module V = Vine

open Exec_utils
open Exec_exceptions
open Exec_options
open Fragment_machine

class cgcos_special_handler (fm : fragment_machine) =
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
  let load_word_or_zero addr =
    try
      fm#load_word_conc addr
    with
      | NotConcrete(_) -> 0L
  in
  let load_byte addr =
    fm#load_byte_concretize addr !opt_measure_influence_syscall_args
      "syscall arg"
  in
  let read_buf addr len =
    if !opt_stop_on_symbolic_syscall_args then
      try
        fm#read_buf addr len (* Works for concrete values only *)
      with
        NotConcrete(_) -> raise SymbolicSyscall
    else
      (assert ((len >= 0) && (len < Sys.max_array_length));
       Array.init len
         (fun i -> Char.chr (load_byte (Int64.add addr (Int64.of_int i)))))
  in
  let store_word base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_word_conc addr v
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
  let lea base i step off =
    Int64.add base (Int64.add (Int64.mul (Int64.of_int i) (Int64.of_int step))
		      (Int64.of_int off)) in
object(self)
  method private get_fd vt_fd =
    match vt_fd with
      | 0 -> Unix.stdin
      | 1 -> Unix.stdout
      | 2 -> Unix.stderr
      | _ ->
	  raise (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))

  method private errno err =
    match err with
      | Unix.EBADF -> 1
      | Unix.EFAULT -> 2
      | Unix.EINVAL -> 3
      | Unix.ENOMEM -> 4
      | Unix.ENOSYS -> 5
      | Unix.EPIPE -> 6
      | Unix.EUNKNOWNERR(_) -> 3
      | _ -> 3

  val mutable transmit_pos = 0

  (* Right now we always redirect the program's FDs 1 and 2 (stdout
     and stderr) to FuzzBALL's stdout. We might want to consider doing
     this more selectively, or controlled by a command-line flag. *)
  method private do_write (fd : int) (bytes : char array) (count : int) (tx_bytes : int64) =
    (* file descriptor, array, to send, transmitted byets? *)
    let success num_bytes =
      if tx_bytes <> 0L then
	store_word tx_bytes 0 num_bytes;
      transmit_pos <- transmit_pos + (Int64.to_int num_bytes);
      (match !opt_max_transmit_bytes with
      | Some max ->
	if transmit_pos > max then
	  raise DeepPath
      | _ -> ());
      put_return 0L
    in
      (try
	 (match !opt_prefix_out, fd with
         | (Some prefix, (1|2)) ->
	   Printf.printf "[%s fd %d]: " prefix fd
         | _ -> ());
	 (match fd with
         | (1|2) -> Array.iter print_char bytes;
	   success (Int64.of_int count)
         | _ ->
	   let str = string_of_char_array bytes and
               ufd = self#get_fd fd
	   in
           match Unix.write ufd str 0 count
           with
           | i when i = count -> success (Int64.of_int count)
           | _ -> raise (Unix.Unix_error(Unix.EINTR, "", "")))
       with
       | Unix.Unix_error(err, _, _) -> self#put_errno err);
    ()

  val mutable next_fresh_addr = 0x50000000L

  method private fresh_addr size = 
    let ret = next_fresh_addr in
      next_fresh_addr <- Int64.add next_fresh_addr size;
      next_fresh_addr <- Int64.logand 0xffff_ffff_ffff_f000L
        (Int64.add next_fresh_addr 0x0fffL); (* page align *)
      ret

  val mutable pm = new Pointer_management.pointer_management
  
  method enablePointerManagementMemoryChecking =
    fm#set_pointer_management pm

  val mutable saved_next_fresh_addr = 0L
  val mutable saved_pointer_managment = None

  method private save_memory_state =
    saved_next_fresh_addr <- next_fresh_addr;
    saved_pointer_managment <- Some pm#construct_deep_copy;

  method private reset_memory_state =
    next_fresh_addr <- saved_next_fresh_addr;
    match saved_pointer_managment with
    | Some ptrmng -> pm <- ptrmng
    | _ -> failwith "no was pointer management snapshot created!"

  val mutable num_receives = 0
  val mutable saved_num_receives = 0

  val mutable num_transmits = 0
  val mutable saved_num_transmits = 0

 (* val mutable transmit_pos is defined above for use in do_write *)
  val mutable saved_transmit_pos = 0

  val mutable receive_pos = 0
  val mutable saved_receive_pos = 0

  val mutable random_pos = 0
  val mutable saved_random_pos = 0

  val mutable randomness = Random.State.make [|!opt_random_seed; 1|]
  val mutable saved_randomness = Random.State.make [|0|]

  method private save_depth_state =
    saved_num_receives <- num_receives;
    saved_num_transmits <- num_transmits;
    saved_transmit_pos <- transmit_pos;
    saved_receive_pos <- receive_pos;
    saved_random_pos <- random_pos;
    saved_randomness <- randomness

  method private reset_depth_state =
    num_receives <- saved_num_receives;
    num_transmits <- saved_num_transmits;
    transmit_pos <- saved_transmit_pos;
    receive_pos <- saved_receive_pos;
    random_pos <- saved_random_pos;
    randomness <- saved_randomness

  method make_snap = 
    self#save_memory_state;
    self#save_depth_state

  method reset = 
    self#reset_memory_state;
    self#reset_depth_state

  method state_json : Yojson.Safe.json option =
    Some (`Assoc
	    ["num_receives", `Int num_receives;
	     "num_transmits", `Int num_transmits;
	     "receive_pos", `Int receive_pos;
	     "transmit_pos", `Int transmit_pos;
	     "random_pos", `Int random_pos])

  method private string_create len =
    try String.create len
    with Invalid_argument("String.create")
	-> raise (Unix.Unix_error(Unix.EFAULT, "String.create", ""))

  method private cgcos_allocate length is_exec addr_p =
    ignore(is_exec); (* We have no page permissions yet *)
    let fresh = self#fresh_addr length in
      if !opt_memory_watching then
        pm#add_alloc fresh length;
      zero_region fresh (Int64.to_int length);
      store_word addr_p 0 fresh;
      put_return 0L

  method private cgcos_deallocate addr len =
    ignore(addr);
    ignore(len);
    if !opt_memory_watching then
      pm#add_dealloc addr len;
    put_return 0L

  method private cgcos_fdwait nfds readfds writefds timeout ready_cnt_p =
    let read_timeval_as_secs addr =
      let secs_f = Int64.to_float (load_word addr) and
          susecs_f = Int64.to_float (load_word (lea addr 0 0 4)) in
      let ret = secs_f +. (susecs_f /. 1000000.0) in
      ret
    in
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
        wl = read_bitmap writefds in
    if !opt_trace_syscalls then
      Printf.printf "\nfdwait(%d, [%s], [%s], 0x%08Lx, 0x%08Lx)"
        nfds (format_fds rl) (format_fds wl) timeout ready_cnt_p;
    try
      let map_fd fds =
        List.map (fun (fd) ->  (self#get_fd fd)) fds
      in
      let rl_file_descr = map_fd rl and 
          wl_file_descr = map_fd wl and 
          timeout_f = (if !opt_skip_timeouts then
			 0.0
		       else
			 read_timeval_as_secs timeout)
      in
      let (r_fds, w_fds, e_fds) = 
        Unix.select rl_file_descr wl_file_descr [] timeout_f in
      let r_fds_len = (List.length r_fds) and
          w_fds_len = (List.length w_fds) in
      if readfds <> 0L then 
        write_bitmap readfds r_fds nfds;
      if writefds <> 0L then 
        write_bitmap writefds w_fds nfds;
      store_word ready_cnt_p 0 (Int64.of_int (r_fds_len + w_fds_len));
      put_return 0L
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private cgcos_random buf count count_out_p =
    if !opt_symbolic_random then
      fm#maybe_start_symbolic
	(fun () -> 
	   (ignore (fm#populate_symbolic_region "random" random_pos buf count);
	    random_pos <- random_pos + count))
    else
      for i = 0 to count - 1 do
	let byte = 
	  if !opt_one_random then
	    (* Deterministic PRNG, with a standard state at the beginning
	       of execution *)
	    (Random.State.bits randomness) land 255
	  else
	    (* Randomness from the decision tree is different on each
	       path *)
	    Int64.to_int (Int64.logand (fm#random_word) 0xffL)
	in
	  fm#store_byte_idx buf i byte
      done;
    store_word count_out_p 0 (Int64.of_int count);
    put_return 0L

  method private read_throw (fd : int) (buf : int64) (count :int) num_bytes_p =
    let str = self#string_create count in
    let oc_fd = self#get_fd fd in
    let num_read =
      if !opt_symbolic_receive then
	(* At the moment this (1) does not do an actual read and (2)
	   always returns the maximum length of data. *)
	let num_read = count in
	  fm#maybe_start_symbolic
	    (fun () ->
	      (* you aren't catching symbolic reads. That's the issue. JTT 11/17/14 *)
	       (let constraints = fm#populate_symbolic_region "input0" receive_pos buf num_read; in
		Pov_xml.add_symbolic_transmit
		  (Int64.of_int receive_pos)
		  (Int64.of_int (receive_pos + num_read))
		  constraints));
	num_read
      else
	let num_read = Unix.read oc_fd str 0 count in
	let read_str = String.sub str 0 num_read in
	(* at this point, we know the actual data that was read into the buffer. *)
	Pov_xml.add_transmit_str read_str count;
        if !opt_concolic_receive then
	  fm#populate_concolic_string "input0" receive_pos buf read_str
	else
	  fm#store_str buf 0L read_str; (* base pointer, offset string *)
	num_read
    in
      store_word num_bytes_p 0 (Int64.of_int num_read);
      receive_pos <- receive_pos + num_read;
      max_input_string_length := max (!max_input_string_length) receive_pos;
      (match !opt_max_receive_bytes with
	 | Some max ->
	     if receive_pos > max then
	       raise DeepPath
	 | _ -> ());
      put_return 0L

  method private cgcos_receive (fd : int) (buf : int64) (count : int) num_bytes_p =
    (match !opt_max_receives with
       | Some m ->
	   if num_receives >= m then
	     raise DeepPath
       | None -> ());
    num_receives <- num_receives + 1;
    try
      self#read_throw fd buf count num_bytes_p
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private cgcos_terminate status =
    raise (SimulatedExit(status))

  (* (fd : int) (bytes : char array) (count : int) (tx_bytes : int64) *)
  method private cgcos_transmit fd bytes count tx_bytes =
    (match !opt_max_transmits with
       | Some m ->
	   if num_transmits >= m then
	     raise DeepPath
       | None -> ());
    num_transmits <- num_transmits + 1;
    Pov_xml.add_read_car bytes tx_bytes;
    self#do_write fd bytes count tx_bytes

  method private put_errno err =
    put_return (Int64.of_int (self#errno err))

  method private errno_to_string errno =
    let err = Int64.to_int errno in
      match err with
	| 0 -> "0 (Success)"
	| 1 -> "1 (EBADF: bad file descriptor)"
	| 2 -> "2 (EFAULT: bad address)"
	| 3 -> "3 (EINVAL: invalid argument)"
	| 4 -> "4 (ENOMEM: out of memory)"
	| 5 -> "5 (ENOSYS: function not implemented)"
	| 6 -> "6 (EPIPE: broken pipe)"
	| _ -> failwith ("Unexpected errno value " ^ (string_of_int err))

  method private handle_cgcos_syscall () =
    let get_reg r = 
      if !opt_symbolic_syscall_error <> None then
        fm#get_word_var r (* fail if not concrete *)
      else
        fm#get_word_var_concretize r
          !opt_measure_influence_syscall_args "syscall arg"
    in
    let (callnum_reg, arg_regs, ret_reg) = match !opt_arch with
      | X86 -> (R_EAX, [| R_EBX; R_ECX; R_EDX; R_ESI; R_EDI; R_EBP |], R_EAX)
      | ARM -> (R7, [| R0; R1; R2; R3; R4; R5; R6 |], R0)
      | X64 -> failwith "64-bit syscalls not supported"

    in
    let syscall_num = Int64.to_int (get_reg callnum_reg) and
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
        (ebx, ecx, edx, esi, edi, ebp)
    in
      ignore(0, read_6_regs);
      assert(!opt_arch = X86);
      let result_p =
	match syscall_num with
	  | 0 -> (* nosys *)
	      self#put_errno Unix.ENOSYS;
	      None
	  | 1 -> (* terminate = Linux sys_exit *)
              let arg1 = read_1_reg () in
              let status = arg1 in
		if !opt_trace_syscalls then
                  Printf.printf "terminate(%Ld) (no return)\n" status;
		self#cgcos_terminate status;
		None
	  | 2 -> (* transmit, similar to Linux sys_write *)
              let (arg1, arg2, arg3, arg4) = read_4_regs () in
              let fd       = Int64.to_int arg1 and
                  buf      = arg2 and
                  count    = Int64.to_int arg3 and
		  tx_bytes = arg4 in
	      (* povxml -- corresponds to the transmit in povs *)
		if !opt_trace_syscalls then
                  Printf.printf "transmit(%d, 0x%08Lx, %d, 0x%08Lx)"
		    fd buf count tx_bytes;
		let bytes = read_buf buf count in
                  self#cgcos_transmit fd bytes count tx_bytes;
		  Some tx_bytes
	  | 3 -> (* receive, similar to Linux sys_read  *)
              let (arg1, arg2, arg3, arg4) = read_4_regs () in
              let fd       = Int64.to_int arg1 and
                  buf      = arg2 and
                  count    = Int64.to_int arg3 and
		  num_bytes_p = arg4 in
	      (* povxml -- insert a read here *)
		if !opt_trace_syscalls then
                  Printf.printf "receive(%d, 0x%08Lx, %d, 0x%08Lx)"
		    fd buf count num_bytes_p;
		self#cgcos_receive fd buf count num_bytes_p;
		Some num_bytes_p
	  | 4 -> (* fdwait, similar to Linux sys_select *)
              let (arg1, arg2, arg3, arg4, arg5) = read_5_regs () in
	      let nfds        = Int64.to_int arg1 and
		  readfds     = arg2 and
		  writefds    = arg3 and
		  timeout     = arg4 and
		  ready_cnt_p = arg5 in
	      (* povxml -- insert a delay object *)
		self#cgcos_fdwait nfds readfds writefds timeout ready_cnt_p;
		Some ready_cnt_p
	  | 5 -> (* allocate, subset of Linux sys_mmap *)
	      let (arg1, arg2, arg3) = read_3_regs () in
	      let len     = arg1 and
		  is_exec = Int64.to_int arg2 and
		  addr_p  = arg3 in
		if !opt_trace_syscalls then
		  Printf.printf "allocate(%Ld, %d, 0x%08Lx)"
		    len is_exec addr_p;
		self#cgcos_allocate len is_exec addr_p;
		Some addr_p
	  | 6 -> (* deallocate = Linux sys_munmap *)
	      let (arg1, arg2) = read_2_regs () in
	      let addr = arg1 and
		  len  = arg2
	      in
		if !opt_trace_syscalls then
		  Printf.printf "deallocate(0x%08Lx, %Ld)" addr len;
		self#cgcos_deallocate addr len;
		None
	  | 7 -> (* random, similar to Linux read from /dev/urandom  *)
	      let (arg1, arg2, arg3) = read_3_regs () in
	      let buf         = arg1 and
		  count       = Int64.to_int arg2 and
		  count_out_p = arg3
	      in
		if !opt_trace_syscalls then
		  Printf.printf "random(0x%08Lx, %d, 0x%08Lx)"
		    buf count count_out_p;
		self#cgcos_random buf count count_out_p;
		Some count_out_p
	  | _ -> 
	      self#put_errno Unix.ENOSYS;
	      None
      in
	if !opt_trace_syscalls then
	  let ret_val = fm#get_word_var ret_reg in
            Printf.printf " = %s" (self#errno_to_string ret_val);
	    (match result_p with
	       | None -> ()
	       | Some ptr ->
		   let v = load_word_or_zero ptr in
		     Printf.printf ", %Ld (0x%08Lx)" (fix_s32 v) v);
	    Printf.printf "\n";
	    flush stdout
	      
  method handle_special str : V.stmt list option =
    let handle_catch () =
      try
        self#handle_cgcos_syscall ()
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
        | _ -> None
end

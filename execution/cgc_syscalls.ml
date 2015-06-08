
module V = Vine

open Exec_utils
open Exec_exceptions
open Exec_options
open Fragment_machine
open Exec_assert_minder

let strstr big little =
  let big_len = String.length big and
      little_len = String.length little and
      ret = ref None
  in
    for i = big_len - little_len downto 0 do
      if (String.sub big i little_len) = little then
	ret := Some (String.sub big i (big_len - i))
    done;
    !ret

class cgcos_special_handler (fm : fragment_machine) =
  let printed_random = ref false in
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
      (g_assert ((len >= 0) && (len < Sys.max_array_length)) 100 "cgc_syscalls.read_buf";
       let lb = 
	 if !opt_skip_output_concretize then load_byte_or_q else load_byte
       in
       Array.init len
         (fun i -> Char.chr (lb (Int64.add addr (Int64.of_int i)))))
  in
  let store_word ?(prov = Interval_tree.Internal) base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_word_conc ~prov addr v
  in
  let zero_region base len =
    g_assert(len >= 0 && len <= 0x20000000) 100 "cgc_syscalls.zero_region"; (* sanity check *)
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
  val unix_fds = 
    let a = Array.make 1024 None in
      Array.set a 0 (Some Unix.stdin);
      Array.set a 1 (Some Unix.stdout);
      a

  method private get_fd vt_fd =
    if vt_fd < 0 || vt_fd >= (Array.length unix_fds) then
      raise (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))
    else
      match unix_fds.(vt_fd) with
	| Some fd -> fd
	| None -> 
	  let descriptor = "/dev/fd/" ^ (string_of_int vt_fd) in
	  if (vt_fd > !opt_num_fd) then 
	    raise (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", "")) 
	  else 
	    let n_fd = Unix.openfile (if !opt_symbolic_receive then "/dev/null" else descriptor) [ Unix.O_RDWR ] 0o666 in
	    Array.set unix_fds vt_fd (Some n_fd); 
	    n_fd

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

  val mutable check_buf = ""

  method private check_msgs str =
    let pats = !opt_stop_on_error_msgs in
    let check_sz =
      List.fold_left
	(fun max s -> let l = String.length s in
	   if l > max then l else max) 0 pats
    in
    let full_str = check_buf ^ str in
      List.iter
	(fun pat ->
	   match strstr full_str pat with
	     | Some _ -> raise (SentErrorMessage pat)
	     | None -> ())
	pats;
      let full_len = String.length full_str in
      let trim_sz = if full_len < check_sz then full_len else check_sz in
	check_buf <- String.sub full_str (full_len - trim_sz) trim_sz

  val mutable transmit_pos = 0
  val mutable last_string_written = ""
  val possible_error_str_table = Hashtbl.create 30

  method private count_repeats () =
    let count = try (Hashtbl.find possible_error_str_table last_string_written) with Not_found -> 0 in
      (match !opt_error_msg_threshold with
      | Some m ->
           (Hashtbl.replace possible_error_str_table last_string_written (count + 1);
            if count = m then
              (opt_stop_on_error_msgs := !opt_stop_on_error_msgs @ [last_string_written];))
      | None -> ());

  (* Right now we always redirect the program's FDs 1 and 2 (stdout
     and stderr) to FuzzBALL's stdout. We might want to consider doing
     this more selectively, or controlled by a command-line flag. *)
  method private do_write (fd : int) (bytes : char array) (count : int) (tx_bytes : int64) =
    (* file descriptor, array, to send, transmitted byets? *)
    let str = string_of_char_array bytes in
    let success num_bytes =
      if tx_bytes <> 0L then
	(* internal, because the system is saying how many bytes were written. *)
	store_word ~prov:Interval_tree.Internal tx_bytes 0 num_bytes;
      transmit_pos <- transmit_pos + (Int64.to_int num_bytes);
      if !opt_stop_on_error_msgs <> [] then
	last_string_written <- str;
        (* self#count_repeats (); *)
	self#check_msgs str;
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
	     flush stdout;
	     success (Int64.of_int count)
         | _ ->
	   let ufd = self#get_fd fd in
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

  val pm = new Pointer_management.pointer_management
  
  method enablePointerManagementMemoryChecking =
    fm#set_pointer_management pm

  val mutable saved_next_fresh_addr = 0L

  method private save_memory_state =
    saved_next_fresh_addr <- next_fresh_addr;
    pm#make_snap

  method private reset_memory_state =
    next_fresh_addr <- saved_next_fresh_addr;
    pm#reset

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

  method private cgcos_allocate unpadded_length is_exec addr_p =
    ignore(is_exec); (* We have no page permissions yet *)
    (* Round up the next multiple of 4096, since Linux/CGC memory
       management is always page-granularity *)
    let length = Int64.add unpadded_length
      (Int64.logand (Int64.neg unpadded_length) 4095L)
    in
    let fresh = self#fresh_addr length in
      if !opt_memory_watching then
        pm#add_alloc fresh length;
      zero_region fresh (Int64.to_int length);
      store_word ~prov:Interval_tree.Internal addr_p 0 fresh;
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
      (* we're storing which fd we're waiting on somewhere, I think this is internal. *)
      fm#store_word_conc ~prov:Interval_tree.Internal (lea fd_bm idx 4 0) fd_w_or;
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
      (* how many fds are ready to be read? Looks like it's not transmit or reading from disk at least. *)
      store_word ~prov:Interval_tree.Internal ready_cnt_p 0 (Int64.of_int (r_fds_len + w_fds_len));
      put_return 0L
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private cgcos_random buf count count_out_p =
    if !opt_symbolic_random then
      fm#maybe_start_symbolic
	(fun () -> 
	   (ignore (fm#populate_symbolic_region ~prov:Interval_tree.Random "random" random_pos buf count);
	    random_pos <- random_pos + count))
    else if !opt_concolic_random then
      let r_chars = Array.init count (fun _ -> 
				      Char.chr ((Random.State.bits randomness) land 255)) in  
      fm#maybe_start_symbolic
	(fun () -> 
	  fm#populate_concolic_string ~prov:Interval_tree.Random "random" random_pos buf (string_of_char_array r_chars);
	    random_pos <- random_pos + count)
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
    store_word ~prov:Interval_tree.Random count_out_p 0 (Int64.of_int count);
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
	       (let constraints = fm#populate_symbolic_region ~prov:Interval_tree.External "input0" receive_pos buf num_read; in
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
	    fm#populate_concolic_string ~prov:Interval_tree.External "input0" receive_pos buf read_str
	  else
	  (* JTT -- We probably want the provenance info here. *)
	  fm#store_str buf 0L read_str; (* base pointer, offset string *)
	num_read
    in
    
      store_word ~prov:Interval_tree.External num_bytes_p 0 (Int64.of_int num_read);
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
      (* see comment in transmit() on fd swapping *)
      let fd' = if fd = 1 then 0 else fd in
	self#read_throw fd' buf count num_bytes_p
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method private cgcos_terminate status = 
    self#count_repeats ();
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
    (* JTT -- We already need to know the provenance by the time we get here. *)
    (* If the program tries to transmit on FD 0, assume they meant FD
       1. This seems conceptually strange, but the real system supports
       it because the two descriptors are really just open on the same
       file (e.g., socket). It would however fail in our test environment
       if FuzzBALL's stdin and stdout are redirected differently. *)
    let fd' = if fd = 0 then 1 else fd in
      self#do_write fd' bytes count tx_bytes

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
    let module Log = (val !Loggers.cgc_restart_json : Yojson_list_logger.JSONListLog) in
    let log_random start_addr len = 
      Log.always
	(Yojson_list_logger.LazyJson
	   (lazy
	      (`Assoc
		  ["function", `String "cgc_os_call";
		   "type", `String ":called_random";
		   "starts_at", `String (Printf.sprintf "0x%08LX" start_addr);
		   "length", `Int len;]))) in
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
      g_assert(!opt_arch = X86) 100 "cgc_syscalls.cgc_os_syscall";
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
	    begin
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
	      let prov = 
		match fm#get_pointer_management () with
	        | None -> Interval_tree.Internal
	        | Some pm -> pm#find_read_prov buf arg3 in  (* int64 version of count *) 
	      begin
		match prov with
		| Interval_tree.External ->
		  let start = Printf.sprintf "0x%08LX" buf
		  and endl = Printf.sprintf "0x%08LX" (Int64.add buf arg3) in
		  fm#add_event_detail "end-addr" (`String endl);
		  fm#add_event_detail "start-addr" (`String start);
		  fm#add_event_detail "subtag" (`String ":echoed-external");	
		  fm#add_event_detail "tag" (`String ":protocol-hint");
		| Interval_tree.Random ->
		  let start = Printf.sprintf "0x%08LX" buf
		  and endl = Printf.sprintf "0x%08LX" (Int64.add buf arg3) in
		    fm#add_event_detail "end-addr" (`String endl);	
		    fm#add_event_detail "start-addr" (`String start);	
		    fm#add_event_detail "subtag" (`String ":sent-random");
		    fm#add_event_detail "tag" (`String ":protocol-hint");	    
		| _ -> ()
	      end;
	      self#cgcos_transmit fd bytes count tx_bytes;
		  Some tx_bytes
	    end
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
	      (match !opt_log_random with
	      | Never -> ()
	      | Once ->
		(if not !printed_random
		 then (printed_random := true;
		       log_random buf count))
	      | Always -> log_random buf count);
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

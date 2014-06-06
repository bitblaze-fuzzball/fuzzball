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
(*   let load_word addr = *)
(*     fm#load_word_concretize addr !opt_measure_influence_syscall_args *)
(*       "syscall arg" *)
(*   in *)
(*   let load_short addr = *)
(*     fm#load_short_concretize addr !opt_measure_influence_syscall_args *)
(*       "syscall arg" *)
(*   in *)
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
      Array.init len
        (fun i -> Char.chr (load_byte (Int64.add addr (Int64.of_int i))))
  in
  let store_word base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_word_conc addr v
  in
  let string_of_char_array ca =
    let s = String.create (Array.length ca) in
      for i = 0 to (Array.length ca) - 1 do
        s.[i] <- ca.(i)
      done;
      s
  in
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

  (* Right now we always redirect the program's FDs 1 and 2 (stdout
     and stderr) to FuzzBALL's stdout. We might want to consider doing
     this more selectively, or controlled by a command-line flag. *)
  method private do_write fd bytes count tx_bytes =
    let success num_bytes =
      if tx_bytes <> 0L then
	store_word tx_bytes 0 num_bytes;
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

  val mutable saved_next_fresh_addr = 0L

  method private save_memory_state =
    saved_next_fresh_addr <- next_fresh_addr;

  method private reset_memory_state =
    next_fresh_addr <- saved_next_fresh_addr;

  method make_snap = 
    self#save_memory_state

  method reset = 
    self#reset_memory_state

  method private cgcos_terminate status =
    raise (SimulatedExit(status))

  method private cgcos_transmit fd bytes count tx_bytes =
    self#do_write fd bytes count tx_bytes

  method private put_errno err =
    put_return (Int64.of_int (self#errno err))

  method private handle_cgcos_syscall () =
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
      | X86 -> (R_EAX, [| R_EBX; R_ECX; R_EDX; R_ESI; R_EDI; R_EBP |], R_EAX)
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
         (ebx, ecx, edx, esi, edi, ebp)
     in
       ignore(0, read_6_regs);
       assert(!opt_arch = X86);
       match syscall_num with
	 | 0 -> (* nosys *)
	     self#put_errno Unix.ENOSYS
	 | 1 -> (* terminate = Linux sys_exit *)
             let arg1 = read_1_reg () in
             let status = arg1 in
               if !opt_trace_syscalls then
                 Printf.printf "terminate(%Ld) (no return)\n" status;
               self#cgcos_terminate status
	 | 2 -> (* transmit, similar to Linux sys_write *)
             let (arg1, arg2, arg3, arg4) = read_4_regs () in
             let fd       = Int64.to_int arg1 and
                 buf      = arg2 and
                 count    = Int64.to_int arg3 and
		 tx_bytes = arg4 in
               if !opt_trace_syscalls then
                 Printf.printf "transmit(%d, 0x%08Lx, %d, 0x%07Lx)\n"
		   fd buf count tx_bytes;
               let bytes = read_buf buf count in
                 self#cgcos_transmit fd bytes count tx_bytes
	 | 3 -> (* receive *)
	     uh "Unhandled CGCOS system call receive (3)"
	 | 4 -> (* fdwait *)
	     uh "Unhandled CGCOS system call fdwait (4)"
	 | 5 -> (* allocate *)
	     uh "Unhandled CGCOS system call allocate (5)"
	 | 6 -> (* deallocate *)
	     uh "Unhandled CGCOS system call deallocate (6)"
	 | 7 -> (* random *)
	     uh "Unhandled CGCOS system call random (7)"
	 | _ -> 
	     self#put_errno Unix.ENOSYS);
    if !opt_trace_syscalls then
      let ret_val = fm#get_word_var ret_reg in
        Printf.printf " = %Ld (0x%08Lx)\n" (fix_s32 ret_val) ret_val;
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

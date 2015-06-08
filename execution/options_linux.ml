(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;

let opt_load_base = ref None
let opt_linux_syscalls = ref false
let opt_setup_initial_proc_state = ref None
let opt_load_data = ref true
let opt_tls_base = ref None
let opt_load_extra_regions = ref []
let opt_core_file_name = ref None
let opt_use_ids_from_core = ref false
let opt_symbolic_files = ref []
let opt_concolic_files = ref []

let opt_extra_programs = ref []

let set_linux_defaults_for_concrete () =
  opt_linux_syscalls := true

let linux_cmdline_opts =
  [
    ("-core", Arg.String (fun s -> opt_core_file_name := Some s),
     "corefile Load memory state from an ELF core dump");
    ("-pid", Arg.String
       (fun s -> opt_pid := (Int32.to_int (Int32.of_string s))),
     "pid Use regs from specified LWP when loading from core");
    ("-use-ids-from-core", Arg.Set(opt_use_ids_from_core),
     " Simulate getpid(), etc., using values from core file");
    ("-external-uname", Arg.Set(opt_external_uname),
     " Use real uname and domainname to simulate uname(2)");
    ("-setup-initial-proc-state",
     Arg.Bool(fun b -> opt_setup_initial_proc_state := Some b),
     "bool Setup initial process state (argv, etc.)?"); 
    ("-load-region", Arg.String
       (add_delimited_pair opt_load_extra_regions '+'),
     "base+size Load an additional region from program image");
    ("-load-base", Arg.String
       (fun s -> opt_load_base := Some (Int64.of_string s)),
     "addr Base address for program image");
    ("-load-data", Arg.Bool(fun b -> opt_load_data := b),
     "bool Load data segments from a binary?"); 
    ("-env", Arg.String
       (fun s ->
	  let (k, v) = split_string '=' s in
	    Hashtbl.replace opt_extra_env k v),
     "name=val Set environment variable for program");
    ("-tls-base", Arg.String
       (fun s -> opt_tls_base := Some (Int64.of_string s)),
     "addr Use a Linux TLS (%gs) segment at the given address");
    ("-linux-syscalls", Arg.Set(opt_linux_syscalls),
     " Simulate Linux system calls on the real system");
    ("-trace-syscalls", Arg.Set(opt_trace_syscalls),
     " Print systems calls (like strace)");
    ("-prefix-out", Arg.String
       (fun s -> opt_prefix_out := Some s),
     "prefix Add a distinguishing prefix before the program's writes");
    ("-symbolic-file", Arg.String
       (fun s -> opt_symbolic_files := s :: !opt_symbolic_files),
     "fname Make data read from the named file symbolic");
    ("-concolic-file", Arg.String
       (fun s -> opt_concolic_files := s :: !opt_concolic_files),
     "fname Make data read from the named file concolic");
    ("-symbolic-syscall-error", Arg.String
       (fun s -> opt_symbolic_syscall_error := Some (Int64.of_string s)),
     "errno Force syscalls with symbolic args to return given value");
    ("-stop-on-symbolic-syscall-args",
     Arg.Set(opt_stop_on_symbolic_syscall_args),
     " Cut of path on symbolic value in system call argument");
    ("-skip-output-concretize", Arg.Set(opt_skip_output_concretize),
     " Output symbolic bytes as ? instead of solving");
    ("-chroot", Arg.String
       (fun s -> opt_chroot_path := Some s),
     "path Prepend PATH to absolute filenames");
    ("--", Arg.Rest(fun s -> opt_argv := !opt_argv @ [s]),
     " Pass any remaining arguments to the program");

    (* Decree/CGCEF/CGCOS options. Not yet enough that it makes sense
       to make a new file for them. *)
    ("-decree", Arg.Set(opt_decree),
     " Use CGC binary format and syscalls (similar to Linux/x86/ELF)");
    ("-symbolic-receive", Arg.Set(opt_symbolic_receive),
     " Make all data read by receive(2cgc) symbolic");
    ("-concolic-receive", Arg.Set(opt_concolic_receive),
     " Make all data read by receive(2cgc) concolic");
    ("-max-receives", Arg.Int
       (fun i -> opt_max_receives := Some i),
     "NUM Stop path after too many receive(2cgc) calls");
    ("-max-transmits", Arg.Int
       (fun i -> opt_max_transmits := Some i),
     "NUM Stop path after too many transmit(2cgc) calls");
    ("-max-receive-bytes", Arg.Int
       (fun i -> opt_max_receive_bytes := Some i),
     "NUM Stop path after too much data receive(2cgc)ed");
    ("-max-transmit-bytes", Arg.Int
       (fun i -> opt_max_transmit_bytes := Some i),
     "NUM Stop path after too much data transmit(2cgc)ed");
    ("-symbolic-random", Arg.Set(opt_symbolic_random),
     " Make all data read by random(2cgc) symbolic");
    ("-concolic-random", Arg.Set(opt_concolic_random),
     " Make all data read by random(2cgc) concolic");
    ("-one-random", Arg.Set(opt_one_random),
     " Make random(2cgc) deterministic");
    ("-skip-timeouts", Arg.Set(opt_skip_timeouts),
     " Don't wait when fdwait(2cgc) says to");
    (* This actually works with both Linux and Decree for testing purposes,
       but the interface wouldn't be right for Linux because there's no
       place to put the arguments to the other programs. *)
    ("-extra-program", Arg.String
       (fun s -> opt_extra_programs := s :: !opt_extra_programs),
     "BINARY Run another CB at the same time, may be repeated");
    ("-stop-on-error-msgs", Arg.String
       (fun s -> opt_stop_on_error_msgs := split_string_list '|' s),
     "STR1|STR2 Stop if one of the given messages is printed");
    ("-error-msg-threshold", Arg.Int
       (fun i -> opt_error_msg_threshold := Some i),
     "NUM Stop if transmit(2cgc)->terminate(2cgc) repeats on unspecified messages");
  ]

let apply_linux_cmdline_opts (fm : Fragment_machine.fragment_machine) =
  let load_base = match !opt_load_base with
    | Some a -> a
    | None ->
	(match !opt_arch with
	   | X86 -> 0x08048000L
	   | X64 -> 0x00400000L
	   | ARM -> 0x8000L
	)
  in
  let do_setup = match !opt_setup_initial_proc_state with
    | Some b -> b
    | None ->
	if !opt_start_addr <> None then
	  false
	else if !opt_argv <> [] then
	  true
	else if !opt_decree then
	  true (* CBs don't take args anyway, so don't require
		  users to give -- *)
	else
	  failwith ("Can't decide whether to "^
		      "-setup-initial-proc-state")
  in
  let add_special_handlers fm linux_break =
    if !opt_decree then
      let csh = new Cgc_syscalls.cgcos_special_handler fm in
	if !opt_memory_watching then
	  csh#enablePointerManagementMemoryChecking;
	fm#add_special_handler (csh :> Fragment_machine.special_handler)      
    else if !opt_linux_syscalls then
      let lsh = new Linux_syscalls.linux_special_handler fm in
	if !opt_use_ids_from_core then
	  lsh#set_proc_identities !Linux_loader.proc_identities;
	List.iter (fun f -> lsh#add_symbolic_file f false) !opt_symbolic_files;
	List.iter (fun f -> lsh#add_symbolic_file f  true) !opt_concolic_files;
	Linux_syscalls.linux_set_up_arm_kuser_page fm;
	lsh#set_the_break linux_break;
	fm#add_special_handler (lsh :> Fragment_machine.special_handler)
    else
      fm#add_special_handler
	((new Special_handlers.linux_special_nonhandler fm)
	 :> Fragment_machine.special_handler);
    (match !opt_x87_emulator with
       | Some emulator_path -> 
	   opt_x87_entry_point :=
	     Some (Linux_loader.load_x87_emulator fm emulator_path);
	   let fpu_sh = new Special_handlers.x87_emulator_special_handler fm in
	     fm#add_special_handler
	       (fpu_sh :> Fragment_machine.special_handler)
       | None -> ());
    (match !opt_sse_emulator with
       | Some s ->
	   (match s with 
	      | "punt" -> let sse_punter = new Special_handlers.sse_floating_point_punter fm in
		  fm#add_special_handler (sse_punter :> Fragment_machine.special_handler)
	      | _ -> failwith (Printf.sprintf "Unrecognized sse handler %s" s ))
       | None -> ())
  in
  let linux_break = ref 0L in
  (match !opt_program_name with
     | Some name ->
	   if !opt_decree then
	     state_start_addr := Some
	       (Decree_loader.load_cb fm name
		  load_base !opt_load_data do_setup
		  !opt_load_extra_regions)
	   else
	     let (s_addr, init_break) = 
	       (Linux_loader.load_dynamic_program fm name
		  load_base !opt_load_data do_setup
		  !opt_load_extra_regions !opt_argv)
	     in
	       state_start_addr := Some s_addr;
	       linux_break := init_break
     | _ -> ());
  List.iter
    (fun p ->
       let pid = fm#alloc_proc
	 (fun () ->
	    fm#make_regs_zero;
	    fm#on_missing_zero;
	    let (start_addr, linux_break) =
	      if !opt_decree then
		((Decree_loader.load_cb fm p load_base !opt_load_data true []),
		 0L)
	      else
		Linux_loader.load_dynamic_program fm p load_base
		  !opt_load_data do_setup [] []
	    in
	      fm#set_eip start_addr;
	      add_special_handlers fm linux_break)
       in
	 ignore(pid))
    !opt_extra_programs;
  (match !opt_core_file_name with
     | Some name -> 
	 state_start_addr :=
	   Some (Linux_loader.load_core fm name)
     | None -> ());
  add_special_handlers fm !linux_break;
  (match !opt_tls_base with
     | Some base -> Linux_loader.setup_tls_segment fm 0x60000000L base
     | None -> ())


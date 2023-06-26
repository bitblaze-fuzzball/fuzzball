(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;

let opt_load_base = ref None
let opt_linux_syscalls = ref false
let opt_noop_syscalls = ref false
let opt_setup_initial_proc_state = ref None
let opt_load_data = ref true
let opt_tls_base = ref None
let opt_load_extra_regions = ref []
let opt_core_file_name = ref None
let opt_use_ids_from_core = ref false
let opt_symbolic_files = ref []
let opt_concolic_files = ref []
let opt_symbolic_stdin_concrete_size = ref false
let opt_concolic_stdin = ref false

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
    ("-hwcap", Arg.String
       (fun s-> Linux_loader.opt_hwcap := Some (Int64.of_string s)),
     "bits Specify CPU features to use in auxv (default: minimal)");
    ("-linux-syscalls", Arg.Set(opt_linux_syscalls),
     " Simulate Linux system calls on the real system");
    ("-noop-syscalls", Arg.Set(opt_noop_syscalls),
     " Simulate Linux system calls as having no effect");
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
    ("-symbolic-stdin-concrete-size",
     Arg.Set(opt_symbolic_stdin_concrete_size),
     " Make data read from standard input symbolic");
    ("-concolic-stdin", Arg.Set(opt_concolic_stdin),
     " Make data read from standard input concolic");
    ("-symbolic-syscall-error", Arg.String
       (fun s -> opt_symbolic_syscall_error := Some (Int64.of_string s)),
     "errno Force syscalls with symbolic args to return given value");
    ("-stop-on-symbolic-syscall-args",
     Arg.Set(opt_stop_on_symbolic_syscall_args),
     " Cut off path on symbolic value in system call argument");
    ("-skip-output-concretize", Arg.Set(opt_skip_output_concretize),
     " Output symbolic bytes as ? instead of solving");
    ("-chroot", Arg.String
       (fun s -> opt_chroot_path := Some s),
     "path Prepend PATH to absolute filenames");
    ("-disqualify-on-message", Arg.String
       (fun s -> opt_disqualify_on_message := Some s),
     "STR Stop execution if a given string is printed");
    ("--", Arg.Rest(fun s -> opt_argv := !opt_argv @ [s]),
     " Pass any remaining arguments to the program");
  ]

let apply_linux_cmdline_opts (fm : Fragment_machine.fragment_machine) =
  (match !opt_program_name with
     | Some name ->
	 let do_setup = match !opt_setup_initial_proc_state with
	   | Some b -> b
	   | None ->
	       if !opt_start_addr <> None then
		 false
	       else if !opt_argv <> [] then
		 true
	       else
		 failwith ("Can't decide whether to "^
			     "-setup-initial-proc-state")
	 in
	 let load_base = match !opt_load_base with
	   | Some a -> a
	   | None ->
	       (match !opt_arch with
		  | X86 -> 0x08048000L
		  | X64 -> 0x00400000L
		  | ARM -> 0x00010000L (* previously common: 0x8000 *)
	       )
	 in
	   state_start_addr := Some
	     (Linux_loader.load_dynamic_program fm name
		load_base !opt_load_data do_setup
		!opt_load_extra_regions !opt_argv)
     | _ -> ());
  (match !opt_core_file_name with
     | Some name -> 
	 state_start_addr :=
	   Some (Linux_loader.load_core fm name)
     | None -> ());
  if !opt_linux_syscalls then
    let lsh = new Linux_syscalls.linux_special_handler fm in
      if !opt_use_ids_from_core then
	lsh#set_proc_identities !Linux_loader.proc_identities;
      List.iter (fun f -> lsh#add_symbolic_file f false) !opt_symbolic_files;
      List.iter (fun f -> lsh#add_symbolic_file f  true) !opt_concolic_files;
      if !opt_symbolic_stdin_concrete_size then
	lsh#add_symbolic_fd 0 false;
      if !opt_concolic_stdin then
	lsh#add_symbolic_fd 0 true;
      if (!opt_symbolic_files <> []) || (!opt_concolic_files <> []) || !opt_concolic_stdin
         || !opt_symbolic_stdin_concrete_size then
        (fm#declare_symbolic_region "file";
         fm#declare_symbolic_region "stdin");
      Linux_syscalls.linux_set_up_arm_kuser_page fm;
      fm#add_special_handler (lsh :> Fragment_machine.special_handler)
  else if !opt_noop_syscalls then
    let nsh = new Noop_syscalls.noop_linux_special_handler fm in
      fm#add_special_handler (nsh :> Fragment_machine.special_handler)
  else
    fm#add_special_handler
      ((new Special_handlers.linux_special_nonhandler fm)
       :> Fragment_machine.special_handler);
  (match !opt_x87_emulator with
     | Some emulator_path -> 
	 opt_x87_entry_point :=
	   Some (Linux_loader.load_x87_emulator fm emulator_path);
	 let fpu_sh = new Special_handlers.x87_emulator_special_handler fm in
	   fm#add_special_handler (fpu_sh :> Fragment_machine.special_handler)
     | None -> ());
  (match !opt_tls_base with
     | Some base -> Linux_loader.setup_tls_segment fm 0x60000000L base
     | None -> ())


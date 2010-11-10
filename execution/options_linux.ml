(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

open Exec_options;;

let opt_load_base = ref 0x08048000L (* Linux user space default *)
let opt_linux_syscalls = ref false
let opt_setup_initial_proc_state = ref None
let opt_load_data = ref true
let opt_tls_base = ref None
let opt_load_extra_regions = ref []
let opt_core_file_name = ref None
let opt_use_ids_from_core = ref false
let opt_symbolic_files = ref []

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
    ("-setup-initial-proc-state",
     Arg.Bool(fun b -> opt_setup_initial_proc_state := Some b),
     "bool Setup initial process state (argv, etc.)?"); 
    ("-load-region", Arg.String
       (add_delimited_pair opt_load_extra_regions '+'),
     "base+size Load an additional region from program image");
    ("-load-base", Arg.String
       (fun s -> opt_load_base := Int64.of_string s),
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
    ("-prefix-out", Arg.Set(opt_prefix_out),
     " Add a distinguishing prefix before the program's writes");
    ("-symbolic-file", Arg.String
       (fun s -> opt_symbolic_files := s :: !opt_symbolic_files),
     "fname Make data read from the named file symbolic");
    ("-symbolic-syscall-error", Arg.String
       (fun s -> opt_symbolic_syscall_error := Some (Int64.of_string s)),
     "errno Force syscalls with symbolic args to return given value");
    ("-chroot", Arg.String
       (fun s -> opt_chroot_path := Some s),
     "path Prepend PATH to absolute filenames");
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
	   state_start_addr := Some
	     (Linux_loader.load_dynamic_program fm name
		!opt_load_base !opt_load_data do_setup
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
      List.iter lsh#add_symbolic_file !opt_symbolic_files;
      fm#add_special_handler (lsh :> Fragment_machine.special_handler)
  else
    fm#add_special_handler
      ((new Special_handlers.linux_special_nonhandler fm)
       :> Fragment_machine.special_handler);
  (match !opt_tls_base with
     | Some base -> Linux_loader.setup_tls_segment fm 0x60000000L base
     | None -> ())


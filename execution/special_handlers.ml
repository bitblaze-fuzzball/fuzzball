(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_options;;
open Exec_exceptions;;
open Fragment_machine;;

class linux_special_nonhandler (fm : fragment_machine) =
object(self)
  method private unhandle_syscall str =
    if !opt_trace_stopping then
      (Printf.printf "Not handling system call special %s\n" str;
       fm#print_regs);
    raise (UnhandledSysCall("System calls disabled"))

  method handle_special str : V.stmt list option =
    match str with
      | "int 0x80" -> self#unhandle_syscall str
      | "sysenter" -> self#unhandle_syscall str
      | _ -> None

  method make_snap : unit = ()
  method reset : unit = ()
end

class trap_special_nonhandler (fm : fragment_machine) =
object(self)
  method handle_special str : V.stmt list option =
    match str with
      | "trap" -> raise UnhandledTrap
      | _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
end

class cpuid_special_handler (fm : fragment_machine)
=
object(self)
  method handle_special str : V.stmt list option =
    match str with
      | "cpuid" -> ( 
	  (* Modelled after VEX/priv/guest-x86/ghelpers.c *)
	  let eaxval = fm#get_word_var R_EAX in
	    (match eaxval with 
	       | 0L -> 
		   fm#set_word_var R_EAX 1L;
		   fm#set_word_var R_EBX 0x756e6547L;
		   fm#set_word_var R_ECX 0x6c65746eL;
		   fm#set_word_var R_EDX 0x49656e69L;
	       | _ ->
		   fm#set_word_var R_EAX 0x543L;
		   fm#set_word_var R_EBX 0x0L;
		   fm#set_word_var R_ECX 0x0L;
		   fm#set_word_var R_EDX 0x8001bfL;
	    );
	    Some ([])
	)
      | _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
end
  
class x87_emulator_special_handler (fm : fragment_machine) =
object(self)
  method handle_special str : V.stmt list option =
    match str with
      | "x87 emulator trap" ->
	  (* This is like a call, but with the return to the current
	     address, which is the first FPU instruction *)
	  assert(!opt_arch = X86);
	  let this_addr = fm#get_eip and
	      emu_addr = match !opt_x87_entry_point with
		| Some addr -> addr
		| None -> failwith "Missing x87_entry_point in special handler"
	  in
	    if !opt_trace_fpu then
	      Printf.printf "Triggering x87 emulator at 0x%08Lx: %s\n"
		this_addr (fm#disasm_insn_at this_addr);
	    Some (fm#fake_call_to_from emu_addr this_addr)
      | _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
end

(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Exec_options;;
open Exec_exceptions;;
open Fragment_machine;;

class linux_special_nonhandler fm =
object(self)
  method private unhandle_syscall str =
    if !opt_trace_stopping then
      (Printf.printf "Not handling system call special %s\n" str;
       fm#print_x86_regs);
    raise (UnhandledSysCall("System calls disabled"))

  method handle_special str : V.stmt list option =
    match str with
      | "int 0x80" -> self#unhandle_syscall str
      | "sysenter" -> self#unhandle_syscall str
      | _ -> None
end

class trap_special_nonhandler fm =
object(self)
  method handle_special str : V.stmt list option =
    match str with
      | "trap" -> raise UnhandledTrap
      | _ -> None
end

class cpuid_special_handler
  (fm :
   < get_word_var : Fragment_machine.register_name -> int64;
     set_word_var : Fragment_machine.register_name -> int64 -> unit; .. >)
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
end
  

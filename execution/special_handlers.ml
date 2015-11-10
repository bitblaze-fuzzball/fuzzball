(*
  Copyright (C) BitBlaze, 2009-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_options;;
open Exec_exceptions;;
open Fragment_machine;;
open Exec_assert_minder;;

class linux_special_nonhandler (fm : fragment_machine) =
object(self)
  method private unhandle_syscall str =
    if !opt_trace_stopping then
      (Printf.eprintf "Not handling system call special %s\n" str;
       fm#print_regs);
    raise (UnhandledSysCall("System calls disabled"))

  method handle_special str : V.stmt list option =
    match str with
      | "int 0x80" -> self#unhandle_syscall str
      | "sysenter" -> self#unhandle_syscall str
      | _ -> None

  method make_snap : unit = ()
  method reset : unit = ()
  method state_json : Yojson.Safe.json option = None
end

class trap_special_nonhandler (fm : fragment_machine) =
object(self)
  method handle_special str : V.stmt list option =
    match str with
      | "trap" -> raise UnhandledTrap
      | _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
  method state_json : Yojson.Safe.json option = None
end

class cpuid_special_handler (fm : fragment_machine)
=
object(self)
  method handle_special str : V.stmt list option =
    match (str, !opt_arch) with
      | ("cpuid", X86) ->
	  (* Modeled after VEX/priv/guest-x86/ghelpers.c *)
	  (match fm#get_word_var R_EAX with
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
      | ("cpuid", X64) ->
	  (match Int64.logand 0xffffffffL (fm#get_long_var R_RAX) with
	     | 0L ->
		 fm#set_long_var R_RAX 1L;
		 fm#set_long_var R_RBX 0x68747541L;
		 fm#set_long_var R_RCX 0x444d4163L;
		 fm#set_long_var R_RDX 0x69746e65L;
	     | 1L ->
		 fm#set_long_var R_RAX 0x00000f5aL;
		 fm#set_long_var R_RBX 0x01000800L;
		 fm#set_long_var R_RCX 0x0L;
		 fm#set_long_var R_RDX 0x078bfbffL;
	     | _ ->
		 fm#set_long_var R_RAX 0x0L;
		 fm#set_long_var R_RBX 0x0L;
		 fm#set_long_var R_RCX 0x0L;
		 fm#set_long_var R_RDX 0x0L;
	  );
	  Some ([])
      | (_, _) -> None
  method make_snap : unit = ()
  method reset : unit = ()
  method state_json : Yojson.Safe.json option = None
end
  
class x87_emulator_special_handler (fm : fragment_machine) =
object(self)
  method handle_special str : V.stmt list option =
    match str with
      | "x87 emulator trap" ->
	  (* This is like a call, but with the return to the current
	     address, which is the first FPU instruction *)
	  g_assert(!opt_arch = X86) 100 "Special_handlers.x87_emulator_special_handler";
	  let this_addr = fm#get_eip and
	      emu_addr = match !opt_x87_entry_point with
		| Some addr -> addr
		| None -> failwith "Missing x87_entry_point in special handler"
	  in
	    if !opt_trace_fpu then
	      Printf.eprintf "Triggering x87 emulator at 0x%08Lx: %s\n"
		this_addr (fm#disasm_insn_at this_addr);
	    Some (fm#fake_call_to_from emu_addr this_addr)
      | _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
  method state_json : Yojson.Safe.json option = None
end


class sse_floating_point_punter (fm : fragment_machine) =
  (** CLang encodes floating point math in a different way than GCC by default.
      This code just punts on SSE floating point instructions *)
object(self)
  method handle_special (str : string) : V.stmt list option =
    try
      let relevant = String.sub str 10 3 in (* magic relies on the way error messages come from libvex *)
      if (String.compare "SSE" relevant ) = 0
      then Some [] (* some no-op *)
      else None
    with _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
  method state_json : Yojson.Safe.json option = None
end

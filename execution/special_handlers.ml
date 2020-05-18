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

let translate_creq tool_str req_num =
  match (tool_str, req_num) with
    | ("FC",  0) -> Some "VALGRIND_MAKE_MEM_NOACCESS"
    | ("FC",  1) -> Some "VALGRIND_MAKE_MEM_UNDEFINED"
    | ("FC",  2) -> Some "VALGRIND_MAKE_MEM_DEFINED"
    | ("FC",  3) -> Some "VALGRIND_DISCARD"
    | ("FC",  4) -> Some "VALGRIND_CHECK_MEM_IS_ADDRESSABLE"
    | ("FC",  5) -> Some "VALGRIND_CHECK_MEM_IS_DEFINED"
    | ("FC",  6) -> Some "VALGRIND_DO_LEAK_CHECK"
    | ("FC",  7) -> Some "VALGRIND_COUNT_LEAKS"
    | ("FC",  8) -> Some "VALGRIND_GET_VBITS"
    | ("FC",  9) -> Some "VALGRIND_SET_VBITS"
    | ("FC", 10) -> Some "VALGRIND_CREATE_BLOCK"
    | ("FC", 11) -> Some "VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE"
    | ("FC", 12) -> Some "FC_PUSH_ENCLOSE"
    | ("FC", 13) -> Some "FC_POP_ENCLOSE"
    | ("FC", 14) -> Some "FC_LEAK_WORD"
    | ("FC", 15) -> Some "FC_MAYBE_LEAK_WORD"
    | ("FC", 16) -> Some "FC_TAINT_WORD"
    | ("FC", 17) -> Some "FC_UNTAINT_WORD"
    | ("FC", 18) -> Some "FC_MD5SUM_BLOCK"
    | ("FC", 19) -> Some "FC_PREPARE_ROLLBACK"
    | ("FC", 20) -> Some "FC_PREPARE_ESCAPEE"
    | ("FC", 21) -> Some "FC_DO_ROLLBACK"
    | ("FC", 22) -> Some "FC_NOTE_ITERATION"
    | _ -> None

class vg_client_req_special_handler (fm : fragment_machine) =
object(self)
  method private handle_creq =
    let eip = fm#get_eip in
    let args_ptr = match !opt_arch with
      | X86 -> fm#get_word_var R_EAX
      | X64 -> fm#get_long_var R_RAX
      | ARM -> fm#get_word_var R4
    in
    let req_id = match !opt_arch with
      | (X86|ARM) -> fm#load_word_conc args_ptr
      | X64 -> fm#load_long_conc args_ptr
    in
    let byte_str i64 = String.make 1 (Char.chr (Int64.to_int i64)) in
    let tool_str = (byte_str (Int64.shift_right req_id 24)) ^
      (byte_str (Int64.logand 0xffL (Int64.shift_right req_id 16)))
    in
    let req_num = Int64.to_int (Int64.logand req_id 0xffffL) in
    let req_name = match translate_creq tool_str req_num with
      | Some name -> name
      | None -> Printf.sprintf "%s-%d" tool_str req_num
    in
    let arg_size = match !opt_arch with
      | (X86|ARM) -> 4
      | X64 -> 8
    in
    let at_off offset =
      fm#load_word_conc (Int64.add args_ptr (Int64.of_int (arg_size * offset)))
    in
    let arg1 = at_off 1 and
	arg2 = at_off 2 and
	arg3 = at_off 3 and
	arg4 = at_off 4 and
	arg5 = at_off 5
    in
      if !opt_trace_client_reqs then
	Printf.printf "0x%08Lx: %s(0x%Lx, 0x%Lx, 0x%Lx, 0x%Lx, 0x%Lx)\n"
	  eip req_name arg1 arg2 arg3 arg4 arg5

  method handle_special str : V.stmt list option =
    match str with
      | "Valgrind client request" -> self#handle_creq; Some []
      | _ -> None
  method make_snap : unit = ()
  method reset : unit = ()
end


class cpuid_special_handler (fm : fragment_machine)
=
object(self)
  method handle_special str : V.stmt list option =
    match (str, !opt_arch) with
      | ("cpuid", X86) ->
	  (* Modeled after VEX/priv/guest-x86/ghelpers.c's
	     x86g_dirtyhelper_CPUID_sse1 (Pentium III) *)
	  (match fm#get_word_var R_EAX with
	     | 0L ->
		 fm#set_word_var R_EAX 2L;
		 fm#set_word_var R_EBX 0x756e6547L;
		 fm#set_word_var R_ECX 0x6c65746eL;
		 fm#set_word_var R_EDX 0x49656e69L;
	     | 1L ->
		 fm#set_word_var R_EAX 0x6b1L;
		 fm#set_word_var R_EBX 4L;
		 fm#set_word_var R_ECX 0L;
		 fm#set_word_var R_EDX 0x0383fbffL;
	     | _ ->
		 fm#set_word_var R_EAX 0x03020101L;
		 fm#set_word_var R_EBX 0x0L;
		 fm#set_word_var R_ECX 0x0L;
		 fm#set_word_var R_EDX 0x0c040883L;
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

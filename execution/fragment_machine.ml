(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

open Exec_domain;;
open Exec_exceptions;;
open Concrete_domain;;
open Exec_options;;
open Formula_manager;;
open Query_engine;;
open Stpvc_engine;;
open Exec_influence;;
open Stp_external_engine;;
open Concrete_memory;;
open Granular_memory;;

let bool64 f = fun a b -> if (f a b) then 1L else 0L

let move_hash src dest =
  V.VarHash.clear dest;
  V.VarHash.iter (fun a b -> V.VarHash.add dest a b) src

class virtual special_handler = object(self)
  method virtual handle_special : string -> V.stmt list option
end

type register_name = 
  | R_EBP | R_ESP | R_ESI | R_EDI | R_EIP | R_EAX | R_EBX | R_ECX | R_EDX
  | EFLAGSREST | R_CF | R_PF | R_AF | R_ZF | R_SF | R_OF
  | R_CC_OP | R_CC_DEP1 | R_CC_DEP2 | R_CC_NDEP
  | R_DFLAG | R_IDFLAG | R_ACFLAG | R_EMWARN
  | R_LDT | R_GDT | R_CS | R_DS| R_ES | R_FS | R_GS | R_SS
  | R_FTOP | R_FPROUND | R_FC3210 | R_SSEROUND | R_IP_AT_SYSCALL

let reg_to_regstr reg = match reg with
  | R_EBP -> "R_EBP" | R_ESP -> "R_ESP" | R_ESI -> "R_ESI"
  | R_EDI -> "R_EDI" | R_EIP -> "R_EIP" | R_EAX -> "R_EAX" | R_EBX -> "R_EBX"
  | R_ECX -> "R_ECX" | R_EDX -> "R_EDX"
  | EFLAGSREST -> "EFLAGSREST" | R_CF -> "R_CF" | R_PF -> "R_PF"
  | R_AF -> "R_AF"| R_ZF -> "R_ZF" | R_SF -> "R_SF" | R_OF -> "R_OF"
  | R_CC_OP -> "R_CC_OP" | R_CC_DEP1 -> "R_CC_DEP2"
  | R_CC_DEP2 -> "R_CC_DEP2" | R_CC_NDEP -> "R_CC_NDEP"
  | R_DFLAG -> "R_DFLAG" | R_IDFLAG -> "R_IDFLAG" | R_ACFLAG -> "R_ACFLAG"
  | R_EMWARN -> "R_EMWARN"
  | R_LDT -> "R_LDT" | R_GDT -> "R_GDT" | R_CS -> "R_CS" | R_DS -> "R_DS"
  | R_ES -> "R_ES" | R_FS -> "R_FS" | R_GS -> "R_GS"| R_SS -> "R_SS"
  | R_FTOP -> "R_FTOP" | R_FPROUND -> "R_FPROUND" | R_FC3210  -> "R_FC3210"
  | R_SSEROUND -> "R_SSEROUND" | R_IP_AT_SYSCALL -> "R_IP_AT_SYSCALL"

let regstr_to_reg s = match s with
  | "R_EBP" -> R_EBP | "R_ESP" -> R_ESP | "R_ESI" -> R_ESI
  | "R_EDI" -> R_EDI | "R_EIP" -> R_EIP | "R_EAX" -> R_EAX | "R_EBX" -> R_EBX
  | "R_ECX" -> R_ECX | "R_EDX" -> R_EDX
  | "EFLAGSREST" -> EFLAGSREST | "R_CF" -> R_CF | "R_PF" -> R_PF
  | "R_AF" -> R_AF| "R_ZF" -> R_ZF | "R_SF" -> R_SF | "R_OF" -> R_OF
  | "R_CC_OP" -> R_CC_OP | "R_CC_DEP1" -> R_CC_DEP2
  | "R_CC_DEP2" -> R_CC_DEP2 | "R_CC_NDEP" -> R_CC_NDEP
  | "R_DFLAG" -> R_DFLAG | "R_IDFLAG" -> R_IDFLAG | "R_ACFLAG" -> R_ACFLAG
  | "R_EMWARN" -> R_EMWARN
  | "R_LDT" -> R_LDT | "R_GDT" -> R_GDT | "R_CS" -> R_CS | "R_DS" -> R_DS
  | "R_ES" -> R_ES | "R_FS" -> R_FS | "R_GS" -> R_GS| "R_SS" -> R_SS
  | "R_FTOP" -> R_FTOP | "R_FPROUND" -> R_FPROUND | "R_FC3210"  -> R_FC3210
  | "R_SSEROUND" -> R_SSEROUND | "R_IP_AT_SYSCALL" -> R_IP_AT_SYSCALL
  | _ -> failwith ("Unrecognized register name " ^ s)

module FragmentMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module GM = GranularMemoryFunctor(D)
  module FormMan = FormulaManagerFunctor(D)

  class frag_machine = object(self)
    val mem = (new GM.granular_second_snapshot_memory
		 (new GM.granular_snapshot_memory
		    (new GM.concrete_maybe_adaptor_memory
		       (new string_maybe_memory))
		    (new GM.granular_hash_memory))
		 (new GM.granular_hash_memory))

    val form_man = new FormMan.formula_manager

    val reg_store = V.VarHash.create 100
    val reg_to_var = Hashtbl.create 100
    val temps = V.VarHash.create 100
    val mutable frag = ([], [])
    val mutable insns = []
    val mutable loop_cnt = 0L

    val mutable snap = (V.VarHash.create 1, V.VarHash.create 1)

    method init_prog (dl, sl) =
      List.iter
	(fun ((n,s,t) as v) ->
	   if s <> "mem" then
	     (V.VarHash.add reg_store v (D.uninit);
	      Hashtbl.add reg_to_var (regstr_to_reg s) v)) dl;
      self#set_frag (dl, sl);
      let result = self#run () in
	match result with
	  | "fallthrough" -> ()
	  | _ -> failwith "Initial program should fall through"

    method set_frag (dl, sl) =
      frag <- (dl, sl);
      V.VarHash.clear temps;
      loop_cnt <- 0L;
      self#concretize_misc;
      insns <- sl

    method concretize_misc = ()

    method eip_hook eip = ignore(eip)

    method set_eip eip =
      self#set_word_var R_EIP eip

    method run_eip_hooks =
      self#eip_hook (self#get_word_var R_EIP)

    method private on_missing_zero_m (m:GM.granular_memory) =
      m#on_missing
	(fun size _ -> match size with
	   | 8  -> D.from_concrete_8  0
	   | 16 -> D.from_concrete_16 0
	   | 32 -> D.from_concrete_32 0L
	   | 64 -> D.from_concrete_64 0L
	   | _ -> failwith "Bad size in on_missing_zero")

    method on_missing_zero =
      self#on_missing_zero_m (mem :> GM.granular_memory)

    method private on_missing_symbol_m (m:GM.granular_memory) name =
      m#on_missing
	(fun size addr -> 
	   match size with
	     | 8  -> form_man#fresh_symbolic_mem_8  name addr
	     | 16 -> form_man#fresh_symbolic_mem_16 name addr
	     | 32 -> form_man#fresh_symbolic_mem_32 name addr
	     | 64 -> form_man#fresh_symbolic_mem_64 name addr
	     | _ -> failwith "Bad size in on_missing_symbol")

    method on_missing_symbol =
      self#on_missing_symbol_m (mem :> GM.granular_memory) "mem"

    method make_x86_regs_zero =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R_FTOP (D.from_concrete_32 0L);	
	reg EFLAGSREST (D.from_concrete_32 0L);
	reg R_LDT (D.from_concrete_32 0x00000000L);
	reg R_DFLAG (D.from_concrete_32 1L);
	reg R_IDFLAG (D.from_concrete_32 0L);
	reg R_ACFLAG (D.from_concrete_32 0L);
	reg R_EBP (D.from_concrete_32 0x00000000L);
	reg R_ESP (D.from_concrete_32 0x00000000L);
	reg R_ESI (D.from_concrete_32 0x00000000L);
	reg R_EDI (D.from_concrete_32 0x00000000L);
	reg R_EAX (D.from_concrete_32 0x00000000L);
	reg R_EBX (D.from_concrete_32 0x00000000L);
	reg R_ECX (D.from_concrete_32 0x00000000L);
	reg R_EDX (D.from_concrete_32 0x00000000L);
	reg R_CS (D.from_concrete_16 0);
	reg R_DS (D.from_concrete_16 0);
	reg R_ES (D.from_concrete_16 0);
	reg R_FS (D.from_concrete_16 0);
	reg R_GS (D.from_concrete_16 0);
	reg R_GDT (D.from_concrete_32 0x00000000L);
	reg R_LDT (D.from_concrete_32 0x00000000L);

    method make_x86_regs_symbolic =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R_EBP (form_man#fresh_symbolic_32 "initial_ebp");
	reg R_ESP (form_man#fresh_symbolic_32 "initial_esp");
	reg R_ESI (form_man#fresh_symbolic_32 "initial_esi");
	reg R_EDI (form_man#fresh_symbolic_32 "initial_edi");
	reg R_EAX (form_man#fresh_symbolic_32 "initial_eax");
	reg R_EBX (form_man#fresh_symbolic_32 "initial_ebx");
	reg R_ECX (form_man#fresh_symbolic_32 "initial_ecx");
	reg R_EDX (form_man#fresh_symbolic_32 "initial_edx");
	reg R_CS (D.from_concrete_16 0x23);
	reg R_DS (D.from_concrete_16 0x2b);
	reg R_ES (D.from_concrete_16 0x2b);
	reg R_FS (D.from_concrete_16 0x0);
	reg R_GS (D.from_concrete_16 0x63);
	reg R_GDT (D.from_concrete_32 0x60000000L);
	reg R_LDT (D.from_concrete_32 0x61000000L);
	reg R_DFLAG (D.from_concrete_32 1L);
	reg R_ACFLAG (D.from_concrete_32 0L);
	reg R_IDFLAG (D.from_concrete_32 0L);
	reg EFLAGSREST (D.from_concrete_32 0L);
	reg R_PF (D.from_concrete_1 0);
	reg R_CF (D.from_concrete_1 0);
	reg R_AF (D.from_concrete_1 0);
	reg R_SF (D.from_concrete_1 0);
	reg R_OF (D.from_concrete_1 0);
	reg R_ZF (D.from_concrete_1 0);
	(* reg EFLAGSREST (form_man#fresh_symbolic_32 "initial_eflagsrest");*)
	reg R_FTOP (D.from_concrete_32 0L);
	(* Linux user space CS segment: *)
	self#store_byte_conc 0x60000020L 0xff;
	self#store_byte_conc 0x60000021L 0xff;
	self#store_byte_conc 0x60000022L 0x00;
	self#store_byte_conc 0x60000023L 0x00;
	self#store_byte_conc 0x60000024L 0x00;
	self#store_byte_conc 0x60000025L 0xfb;
	self#store_byte_conc 0x60000026L 0xcf;
	self#store_byte_conc 0x60000027L 0x00;
	(* Linux user space DS/ES segment: *)
	self#store_byte_conc 0x60000028L 0xff;
	self#store_byte_conc 0x60000029L 0xff;
	self#store_byte_conc 0x6000002aL 0x00;
	self#store_byte_conc 0x6000002bL 0x00;
	self#store_byte_conc 0x6000002cL 0x00;
	self#store_byte_conc 0x6000002dL 0xf3;
	self#store_byte_conc 0x6000002eL 0xcf;
	self#store_byte_conc 0x6000002fL 0x00;
	(* Linux user space GS segment: *)
	self#store_byte_conc 0x60000060L 0xff;
	self#store_byte_conc 0x60000061L 0xff;
	self#store_byte_conc 0x60000062L 0x00;
	self#store_byte_conc 0x60000063L 0x00;
	self#store_byte_conc 0x60000064L 0x00;
	self#store_byte_conc 0x60000065L 0xf3;
	self#store_byte_conc 0x60000066L 0xcf;
	self#store_byte_conc 0x60000067L 0x62;
	(* Linux kernel space CS segment: *)
	self#store_byte_conc 0x60000070L 0xff;
	self#store_byte_conc 0x60000071L 0xff;
	self#store_byte_conc 0x60000072L 0x00;
	self#store_byte_conc 0x60000073L 0x00;
	self#store_byte_conc 0x60000074L 0x00;
	self#store_byte_conc 0x60000075L 0xfb;
	self#store_byte_conc 0x60000076L 0xcf;
	self#store_byte_conc 0x60000077L 0x00;
	(* Linux kernel space DS/ES segment: *)
	self#store_byte_conc 0x60000078L 0xff;
	self#store_byte_conc 0x60000079L 0xff;
	self#store_byte_conc 0x6000007aL 0x00;
	self#store_byte_conc 0x6000007bL 0x00;
	self#store_byte_conc 0x6000007cL 0x00;
	self#store_byte_conc 0x6000007dL 0xf3;
	self#store_byte_conc 0x6000007eL 0xcf;
	self#store_byte_conc 0x6000007fL 0x00;
	(* ReactOS kernel space FS segment: *)
(* 	self#store_byte_conc 0x60000030L 0x02; (* limit low *) *)
(* 	self#store_byte_conc 0x60000031L 0x00; (* limit mid *) *)
(* 	self#store_byte_conc 0x60000032L 0x00; (* base low *) *)
(* 	self#store_byte_conc 0x60000033L 0xf0; (* base mid-low *) *)
(* 	self#store_byte_conc 0x60000034L 0xdf; (* base mid-high *) *)
(* 	self#store_byte_conc 0x60000035L 0xf3; (* flags *) *)
(* 	self#store_byte_conc 0x60000036L 0xc0; (* flags, limit high *) *)
(* 	self#store_byte_conc 0x60000037L 0xff; (* base high *) *)
	(* Windows 7 kernel space FS segment: *)
	self#store_byte_conc 0x60000030L 0x04; (* limit low *)
	self#store_byte_conc 0x60000031L 0x00; (* limit mid *)
	self#store_byte_conc 0x60000032L 0x00; (* base low *)
	self#store_byte_conc 0x60000033L 0xec; (* base mid-low *)
	self#store_byte_conc 0x60000034L 0x92; (* base mid-high *)
	self#store_byte_conc 0x60000035L 0xf3; (* flags *)
	self#store_byte_conc 0x60000036L 0xc0; (* flags, limit high *)
	self#store_byte_conc 0x60000037L 0x82; (* base high *)
	(* Windows 7 user space FS segment: *)
	self#store_byte_conc 0x60000038L 0x01; (* limit low *)
	self#store_byte_conc 0x60000039L 0x00; (* limit mid *)
	self#store_byte_conc 0x6000003aL 0x00; (* base low *)
	self#store_byte_conc 0x6000003bL 0xe0; (* base mid-low *)
	self#store_byte_conc 0x6000003cL 0x92; (* base mid-high *)
	self#store_byte_conc 0x6000003dL 0xf3; (* flags *)
	self#store_byte_conc 0x6000003eL 0xfd; (* flags, limit high *)
	self#store_byte_conc 0x6000003fL 0x7f; (* base high *)

    method load_x86_user_regs regs =
      self#set_word_var R_EAX (Int64.of_int32 regs.Temu_state.eax);
      self#set_word_var R_EBX (Int64.of_int32 regs.Temu_state.ebx);
      self#set_word_var R_ECX (Int64.of_int32 regs.Temu_state.ecx);
      self#set_word_var R_EDX (Int64.of_int32 regs.Temu_state.edx);
      self#set_word_var R_ESI (Int64.of_int32 regs.Temu_state.esi);
      self#set_word_var R_EDI (Int64.of_int32 regs.Temu_state.edi);
      self#set_word_var R_ESP (Int64.of_int32 regs.Temu_state.esp);
      self#set_word_var R_EBP (Int64.of_int32 regs.Temu_state.ebp);
      self#set_word_var EFLAGSREST
	(Int64.logand (Int64.of_int32 regs.Temu_state.eflags) 0xfffff72aL);
      (let eflags_i = Int32.to_int regs.Temu_state.eflags in
	 self#set_bit_var R_CF (eflags_i land 1);
	 self#set_bit_var R_PF ((eflags_i lsr 2) land 1);
	 self#set_bit_var R_AF ((eflags_i lsr 4) land 1);
	 self#set_bit_var R_ZF ((eflags_i lsr 6) land 1);
	 self#set_bit_var R_SF ((eflags_i lsr 7) land 1);
	 self#set_bit_var R_OF ((eflags_i lsr 11) land 1));
      self#set_short_var R_CS (Int32.to_int regs.Temu_state.xcs);
      self#set_short_var R_DS (Int32.to_int regs.Temu_state.xds);
      self#set_short_var R_ES (Int32.to_int regs.Temu_state.xes);
      self#set_short_var R_FS (Int32.to_int regs.Temu_state.xfs);
      self#set_short_var R_GS (Int32.to_int regs.Temu_state.xgs);
      self#set_short_var R_SS (Int32.to_int regs.Temu_state.xss)

    method print_x86_regs =
      let reg str r =
	Printf.printf "%s: " str;
	Printf.printf "%s\n"
	  (D.to_string_32 (self#get_int_var (Hashtbl.find reg_to_var r)))
      in
	reg "%eax" R_EAX;
	reg "%ebx" R_EBX;
	reg "%ecx" R_ECX;
	reg "%edx" R_EDX;
	reg "%esi" R_ESI;
	reg "%edi" R_EDI;
	reg "%esp" R_ESP;
	reg "%ebp" R_EBP

    method private store_byte  addr b = mem#store_byte  addr b
    method private store_short addr s = mem#store_short addr s
    method private store_word  addr w = mem#store_word  addr w
    method private store_long  addr l = mem#store_long  addr l

    method store_byte_conc  addr b = mem#store_byte addr (D.from_concrete_8 b)
    method store_short_conc addr s = mem#store_short addr(D.from_concrete_16 s)
    method store_word_conc  addr w = mem#store_word addr (D.from_concrete_32 w)
    method store_long_conc  addr l = mem#store_long addr (D.from_concrete_64 l)

    method store_page_conc  addr p = mem#store_page addr p

    method private load_byte  addr = mem#load_byte  addr
    method private load_short addr = mem#load_short addr
    method private load_word  addr = mem#load_word  addr
    method private load_long  addr = mem#load_long  addr

    method load_byte_conc  addr = D.to_concrete_8  (mem#load_byte  addr)
    method load_short_conc addr = D.to_concrete_16 (mem#load_short addr)
    method load_word_conc  addr = D.to_concrete_32 (mem#load_word  addr)
    method load_long_conc  addr = D.to_concrete_64 (mem#load_long  addr)

    method start_symbolic = mem#inner_make_snap ()

    method make_snap () =
      mem#make_snap ();
      snap <- (V.VarHash.copy reg_store, V.VarHash.copy temps)

    method reset () =
      mem#reset ();
      match snap with (r, t) ->
	move_hash r reg_store;
	move_hash t temps;

    val mutable special_handler_list = ([] : #special_handler list)

    method add_special_handler (h:special_handler) =
      special_handler_list <- h :: special_handler_list

    method handle_special str =
      try
	let sl_r = ref [] in
	  ignore(List.find
		   (fun h ->
		      match h#handle_special str with
			| None -> false
			| Some sl -> sl_r := sl; true)
		   special_handler_list);
	  Some !sl_r
      with
	  Not_found -> None

    method private get_int_var ((_,vname,ty) as var) =
      try
	let v = V.VarHash.find reg_store var in
	  (* if v = D.uninit then
	    Printf.printf "Warning: read uninitialized register %s\n"
	     vname; *)
	  v
      with
	| Not_found ->
	    (try 
	       V.VarHash.find temps var
	     with
	       | Not_found -> V.pp_var print_string var; 
		   failwith "Unknown variable")

    method get_bit_var reg =
      D.to_concrete_1 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_byte_var reg =
      D.to_concrete_8 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_short_var reg =
      D.to_concrete_16 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_word_var reg =
      D.to_concrete_32 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_long_var reg =
      D.to_concrete_64 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method private set_int_var ((_,_,ty) as var) value =
      try
	ignore(V.VarHash.find reg_store var);
	V.VarHash.replace reg_store var value
      with
	  Not_found ->
	    V.VarHash.replace temps var value

    method set_bit_var reg v =
      self#set_int_var (Hashtbl.find reg_to_var reg) (D.from_concrete_1 v)

    method set_byte_var reg v =
      self#set_int_var (Hashtbl.find reg_to_var reg) (D.from_concrete_8 v)

    method set_short_var reg v =
      self#set_int_var (Hashtbl.find reg_to_var reg) (D.from_concrete_16 v)

    method set_word_var reg v =
      self#set_int_var (Hashtbl.find reg_to_var reg) (D.from_concrete_32 v)

    method set_long_var reg v =
      self#set_int_var (Hashtbl.find reg_to_var reg) (D.from_concrete_64 v)

    method set_word_var_low_short reg v =
      let var = Hashtbl.find reg_to_var reg in
      let high = D.extract_16_from_32 (self#get_int_var var) 2 in
      let newv = D.assemble32 (D.from_concrete_16 v) high in
	self#set_int_var var newv

    method set_word_var_low_byte reg v =
      let var = Hashtbl.find reg_to_var reg in
      let high_s = D.extract_16_from_32 (self#get_int_var var) 2 in
      let second_b = D.extract_8_from_32 (self#get_int_var var) 1 in
      let newv = D.assemble32
	(D.assemble16 (D.from_concrete_8 v) second_b) high_s
      in
	self#set_int_var var newv

    method set_word_var_second_byte reg v =
      let var = Hashtbl.find reg_to_var reg in
      let high_s = D.extract_16_from_32 (self#get_int_var var) 2 in
      let low_b = D.extract_8_from_32 (self#get_int_var var) 0 in
      let newv = D.assemble32
	(D.assemble16 low_b (D.from_concrete_8 v)) high_s
      in
	self#set_int_var var newv

    val mutable symbol_uniq = 0
      
    method set_word_reg_symbolic reg s =
      self#set_int_var (Hashtbl.find reg_to_var reg)
	(form_man#fresh_symbolic_32 (s ^ "_" ^ (string_of_int symbol_uniq)));
      symbol_uniq <- symbol_uniq + 1

    method private handle_load addr_e ty =
      let addr = self#eval_addr_exp addr_e in
      let v =
	(match ty with
	   | V.REG_8 -> self#load_byte addr
	   | V.REG_16 -> self#load_short addr
	   | V.REG_32 -> self#load_word addr
	   | V.REG_64 -> self#load_long addr
	   | _ -> failwith "Unsupported memory type") in
	(v, ty)

    method private handle_store addr_e ty rhs_e =
      let addr = self#eval_addr_exp addr_e and
	  value = self#eval_int_exp_simplify rhs_e in
	match ty with
	  | V.REG_8 -> self#store_byte addr value
	  | V.REG_16 -> self#store_short addr value
	  | V.REG_32 -> self#store_word addr value
	  | V.REG_64 -> self#store_long addr value
	  | _ -> failwith "Unsupported type in memory move"

    method private maybe_concretize_binop op v1 v2 ty1 ty2 =
      (v1, v2)

    method private eval_int_exp_ty exp =
      match exp with
	| V.BinOp(op, e1, e2) ->
	    let (v1, ty1) = self#eval_int_exp_ty e1 and
		(v2, ty2) = self#eval_int_exp_ty e2 in
	    let ty = 
	      (match op with
		 | V.PLUS | V.MINUS | V.TIMES
		 | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
		 | V.BITAND | V.BITOR | V.XOR
		     -> assert(ty1 = ty2); ty1
		 | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		     -> ty1
		 | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		     -> assert(ty1 = ty2); V.REG_1) in
	    let func =
	      (match (op, ty1) with
		 | (V.PLUS, V.REG_1)  -> D.plus1 
		 | (V.PLUS, V.REG_8)  -> D.plus8 
		 | (V.PLUS, V.REG_16) -> D.plus16
		 | (V.PLUS, V.REG_32) -> D.plus32
		 | (V.PLUS, V.REG_64) -> D.plus64
		 | (V.MINUS, V.REG_1)  -> D.minus1 
		 | (V.MINUS, V.REG_8)  -> D.minus8 
		 | (V.MINUS, V.REG_16) -> D.minus16
		 | (V.MINUS, V.REG_32) -> D.minus32
		 | (V.MINUS, V.REG_64) -> D.minus64
		 | (V.TIMES, V.REG_1)  -> D.times1 
		 | (V.TIMES, V.REG_8)  -> D.times8 
		 | (V.TIMES, V.REG_16) -> D.times16
		 | (V.TIMES, V.REG_32) -> D.times32
		 | (V.TIMES, V.REG_64) -> D.times64
		 | (V.DIVIDE, V.REG_1)  -> D.divide1 
		 | (V.DIVIDE, V.REG_8)  -> D.divide8 
		 | (V.DIVIDE, V.REG_16) -> D.divide16
		 | (V.DIVIDE, V.REG_32) -> D.divide32
		 | (V.DIVIDE, V.REG_64) -> D.divide64
		 | (V.SDIVIDE, V.REG_1)  -> D.sdivide1 
		 | (V.SDIVIDE, V.REG_8)  -> D.sdivide8 
		 | (V.SDIVIDE, V.REG_16) -> D.sdivide16
		 | (V.SDIVIDE, V.REG_32) -> D.sdivide32
		 | (V.SDIVIDE, V.REG_64) -> D.sdivide64
		 | (V.MOD, V.REG_1)  -> D.mod1 
		 | (V.MOD, V.REG_8)  -> D.mod8 
		 | (V.MOD, V.REG_16) -> D.mod16
		 | (V.MOD, V.REG_32) -> D.mod32
		 | (V.MOD, V.REG_64) -> D.mod64
		 | (V.SMOD, V.REG_1)  -> D.smod1 
		 | (V.SMOD, V.REG_8)  -> D.smod8 
		 | (V.SMOD, V.REG_16) -> D.smod16
		 | (V.SMOD, V.REG_32) -> D.smod32
		 | (V.SMOD, V.REG_64) -> D.smod64
		 | (V.LSHIFT, V.REG_1)  -> D.lshift1 
		 | (V.LSHIFT, V.REG_8)  -> D.lshift8 
		 | (V.LSHIFT, V.REG_16) -> D.lshift16
		 | (V.LSHIFT, V.REG_32) -> D.lshift32
		 | (V.LSHIFT, V.REG_64) -> D.lshift64
		 | (V.RSHIFT, V.REG_1)  -> D.rshift1 
		 | (V.RSHIFT, V.REG_8)  -> D.rshift8 
		 | (V.RSHIFT, V.REG_16) -> D.rshift16
		 | (V.RSHIFT, V.REG_32) -> D.rshift32
		 | (V.RSHIFT, V.REG_64) -> D.rshift64
		 | (V.ARSHIFT, V.REG_1)  -> D.arshift1 
		 | (V.ARSHIFT, V.REG_8)  -> D.arshift8 
		 | (V.ARSHIFT, V.REG_16) -> D.arshift16
		 | (V.ARSHIFT, V.REG_32) -> D.arshift32
		 | (V.ARSHIFT, V.REG_64) -> D.arshift64
		 | (V.BITAND, V.REG_1)  -> D.bitand1 
		 | (V.BITAND, V.REG_8)  -> D.bitand8 
		 | (V.BITAND, V.REG_16) -> D.bitand16
		 | (V.BITAND, V.REG_32) -> D.bitand32
		 | (V.BITAND, V.REG_64) -> D.bitand64
		 | (V.BITOR, V.REG_1)  -> D.bitor1 
		 | (V.BITOR, V.REG_8)  -> D.bitor8 
		 | (V.BITOR, V.REG_16) -> D.bitor16
		 | (V.BITOR, V.REG_32) -> D.bitor32
		 | (V.BITOR, V.REG_64) -> D.bitor64
		 | (V.XOR, V.REG_1)  -> D.xor1 
		 | (V.XOR, V.REG_8)  -> D.xor8 
		 | (V.XOR, V.REG_16) -> D.xor16
		 | (V.XOR, V.REG_32) -> D.xor32
		 | (V.XOR, V.REG_64) -> D.xor64
		 | (V.EQ, V.REG_1)  -> D.eq1 
		 | (V.EQ, V.REG_8)  -> D.eq8 
		 | (V.EQ, V.REG_16) -> D.eq16
		 | (V.EQ, V.REG_32) -> D.eq32
		 | (V.EQ, V.REG_64) -> D.eq64
		 | (V.NEQ, V.REG_1)  -> D.neq1 
		 | (V.NEQ, V.REG_8)  -> D.neq8 
		 | (V.NEQ, V.REG_16) -> D.neq16
		 | (V.NEQ, V.REG_32) -> D.neq32
		 | (V.NEQ, V.REG_64) -> D.neq64
		 | (V.LT, V.REG_1)  -> D.lt1 
		 | (V.LT, V.REG_8)  -> D.lt8 
		 | (V.LT, V.REG_16) -> D.lt16
		 | (V.LT, V.REG_32) -> D.lt32
		 | (V.LT, V.REG_64) -> D.lt64
		 | (V.LE, V.REG_1)  -> D.le1 
		 | (V.LE, V.REG_8)  -> D.le8 
		 | (V.LE, V.REG_16) -> D.le16
		 | (V.LE, V.REG_32) -> D.le32
		 | (V.LE, V.REG_64) -> D.le64
		 | (V.SLT, V.REG_1)  -> D.slt1 
		 | (V.SLT, V.REG_8)  -> D.slt8 
		 | (V.SLT, V.REG_16) -> D.slt16
		 | (V.SLT, V.REG_32) -> D.slt32
		 | (V.SLT, V.REG_64) -> D.slt64
		 | (V.SLE, V.REG_1)  -> D.sle1 
		 | (V.SLE, V.REG_8)  -> D.sle8 
		 | (V.SLE, V.REG_16) -> D.sle16
		 | (V.SLE, V.REG_32) -> D.sle32
		 | (V.SLE, V.REG_64) -> D.sle64
		 | _ -> failwith "unexpected binop/type in eval_int_exp_ty")
	    in
	    let (v1', v2') = self#maybe_concretize_binop op v1 v2 ty1 ty2 in
	      (func v1' v2'), ty
	| V.UnOp(op, e1) ->
	    let (v1, ty1) = self#eval_int_exp_ty e1 in
	    let result = 
	      (match (op, ty1) with
		 | (V.NEG, V.REG_1)  -> D.neg1 v1
		 | (V.NEG, V.REG_8)  -> D.neg8 v1
		 | (V.NEG, V.REG_16) -> D.neg16 v1
		 | (V.NEG, V.REG_32) -> D.neg32 v1
		 | (V.NEG, V.REG_64) -> D.neg64 v1
		 | (V.NOT, V.REG_1)  -> D.not1 v1
		 | (V.NOT, V.REG_8)  -> D.not8 v1
		 | (V.NOT, V.REG_16) -> D.not16 v1
		 | (V.NOT, V.REG_32) -> D.not32 v1
		 | (V.NOT, V.REG_64) -> D.not64 v1
		 | _ -> failwith "unexpected unop/type in eval_int_exp_ty")
	  in
	    result, ty1
	| V.Constant(V.Int(V.REG_1, i)) ->
	    (D.from_concrete_1 (Int64.to_int i)), V.REG_1
	| V.Constant(V.Int(V.REG_8, i)) ->
	    (D.from_concrete_8 (Int64.to_int i)), V.REG_8
	| V.Constant(V.Int(V.REG_16,i)) -> 
	    (D.from_concrete_16 (Int64.to_int i)),V.REG_16
	| V.Constant(V.Int(V.REG_32,i)) -> (D.from_concrete_32 i),V.REG_32
	| V.Constant(V.Int(V.REG_64,i)) -> (D.from_concrete_64 i),V.REG_64
	| V.Constant(V.Int(_,_)) -> failwith "unexpected integer constant type"
	| V.Lval(V.Temp((_,_,ty) as var)) -> (self#get_int_var var), ty
	| V.Lval(V.Mem(memv, idx, ty)) ->
	    self#handle_load idx ty
	| V.Cast(kind, ty, e) ->
	    let (v1, ty1) = self#eval_int_exp_ty e in
	    let func =
	      match (kind, ty1, ty) with
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_8)  -> D.cast1u8
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_16) -> D.cast1u16
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_32) -> D.cast1u32
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_64) -> D.cast1u64
		| (V.CAST_UNSIGNED, V.REG_8,  V.REG_16) -> D.cast8u16
		| (V.CAST_UNSIGNED, V.REG_8,  V.REG_32) -> D.cast8u32
		| (V.CAST_UNSIGNED, V.REG_8,  V.REG_64) -> D.cast8u64
		| (V.CAST_UNSIGNED, V.REG_16, V.REG_32) -> D.cast16u32
		| (V.CAST_UNSIGNED, V.REG_16, V.REG_64) -> D.cast16u64
		| (V.CAST_UNSIGNED, V.REG_32, V.REG_64) -> D.cast32u64
		| (V.CAST_SIGNED, V.REG_1,  V.REG_8)  -> D.cast1s8
		| (V.CAST_SIGNED, V.REG_1,  V.REG_16) -> D.cast1s16
		| (V.CAST_SIGNED, V.REG_1,  V.REG_32) -> D.cast1s32
		| (V.CAST_SIGNED, V.REG_1,  V.REG_64) -> D.cast1s64
		| (V.CAST_SIGNED, V.REG_8,  V.REG_16) -> D.cast8s16
		| (V.CAST_SIGNED, V.REG_8,  V.REG_32) -> D.cast8s32
		| (V.CAST_SIGNED, V.REG_8,  V.REG_64) -> D.cast8s64
		| (V.CAST_SIGNED, V.REG_16, V.REG_32) -> D.cast16s32
		| (V.CAST_SIGNED, V.REG_16, V.REG_64) -> D.cast16s64
		| (V.CAST_SIGNED, V.REG_32, V.REG_64) -> D.cast32s64
		| (V.CAST_LOW, V.REG_64, V.REG_1)  -> D.cast64l1
		| (V.CAST_LOW, V.REG_64, V.REG_8)  -> D.cast64l8
		| (V.CAST_LOW, V.REG_64, V.REG_16) -> D.cast64l16
		| (V.CAST_LOW, V.REG_64, V.REG_32) -> D.cast64l32
		| (V.CAST_LOW, V.REG_32, V.REG_1)  -> D.cast32l1
		| (V.CAST_LOW, V.REG_32, V.REG_8)  -> D.cast32l8
		| (V.CAST_LOW, V.REG_32, V.REG_16) -> D.cast32l16
		| (V.CAST_LOW, V.REG_16, V.REG_8)  -> D.cast16l8
		| (V.CAST_LOW, V.REG_16, V.REG_1)  -> D.cast16l1
		| (V.CAST_LOW, V.REG_8,  V.REG_1)  -> D.cast8l1
		| (V.CAST_HIGH, V.REG_64, V.REG_1)  -> D.cast64h1
		| (V.CAST_HIGH, V.REG_64, V.REG_8)  -> D.cast64h8
		| (V.CAST_HIGH, V.REG_64, V.REG_16) -> D.cast64h16
		| (V.CAST_HIGH, V.REG_64, V.REG_32) -> D.cast64h32
		| (V.CAST_HIGH, V.REG_32, V.REG_1)  -> D.cast32h1
		| (V.CAST_HIGH, V.REG_32, V.REG_8)  -> D.cast32h8
		| (V.CAST_HIGH, V.REG_32, V.REG_16) -> D.cast32h16
		| (V.CAST_HIGH, V.REG_16, V.REG_8)  -> D.cast16h8
		| (V.CAST_HIGH, V.REG_16, V.REG_1)  -> D.cast16h1
		| (V.CAST_HIGH, V.REG_8,  V.REG_1)  -> D.cast8h1
		| _ -> failwith "bad cast kind in eval_int_exp_ty"
	    in
	      ((func v1), ty)
	(* XXX move this to something like a special handler: *)
	| V.Unknown("rdtsc") -> ((D.from_concrete_64 1L), V.REG_64) 
	| _ -> failwith "Unsupported (or non-int) expr type in eval_int_exp_ty"
	  
    method private eval_int_exp exp =
      let (v, _) = self#eval_int_exp_ty exp in
	v

    method private eval_int_exp_simplify exp =
      match self#eval_int_exp_ty exp with
	| (v, V.REG_1) -> form_man#simplify1 v
	| (v, V.REG_8) -> form_man#simplify8 v
	| (v, V.REG_16) -> form_man#simplify16 v
	| (v, V.REG_32) -> form_man#simplify32 v
	| (v, V.REG_64) -> form_man#simplify64 v
	| _ -> failwith "Unexpected type in eval_int_exp_simplify"

    method eval_bool_exp exp =
      let v = self#eval_int_exp exp in
	if (D.to_concrete_1 v) = 1 then true else false

    method eval_addr_exp exp =
      let v = self#eval_int_exp exp in
	(D.to_concrete_32 v)

    method eval_label_exp e =
      match e with
	| V.Name(lab) -> lab
	| _ ->
	    let addr = self#eval_addr_exp e in
	      Printf.sprintf "pc_0x%Lx" addr

    method jump do_jump lab =
      let rec find_label lab sl =
	match sl with
	  | [] -> None
	  | V.Label(l) :: rest when l = lab -> Some sl
	  | st :: rest -> find_label lab rest 
      in
	loop_cnt <- Int64.succ loop_cnt;
	if loop_cnt > !opt_iteration_limit then raise TooManyIterations;
	let (_, sl) = frag in
	  match find_label lab sl with
	    | None -> lab
	    | Some sl ->
		self#run_sl do_jump sl
	      
    method run_sl do_jump sl =
      let jump lab =
	if do_jump lab then
	  self#jump do_jump lab
	else
	  lab
      in
      let rec loop = 
	function
	  | [] -> "fallthrough"
	  | st :: rest ->
	      (match st with
		 | V.Jmp(l) -> jump (self#eval_label_exp l)
		 | V.CJmp(cond, l1, l2) ->
		     let cond_v = self#eval_bool_exp cond in
		       if cond_v then
			 jump (self#eval_label_exp l1)
		       else
			 jump (self#eval_label_exp l2)
		 | V.Move(V.Temp(v), e) ->
		     self#set_int_var v (self#eval_int_exp_simplify e);
		     loop rest
		 | V.Move(V.Mem(memv, idx_e, ty), rhs_e) ->
		     self#handle_store idx_e ty rhs_e;
		     loop rest
		 | V.Special("VEX decode error") ->
		     raise IllegalInstruction
		 | V.Special(str) ->
		     (match self#handle_special str with
			| Some sl -> 
			    loop (sl @ rest)
			| None ->
			    Printf.printf "Unhandled special %s\n" str;
			    failwith "Unhandled special")
		 | V.Label(_) -> loop rest
		 | V.ExpStmt(e) ->
		     let v = self#eval_int_exp e in
		       ignore(v);
		       loop rest
		 | V.Comment(s) -> 
		     if (!opt_print_callrets) then (
		       if (Str.string_match
			     (Str.regexp ".*\\(call\\|ret\\).*") s 0) then (
			 let eip = self#get_word_var R_EIP in
			   Printf.printf "%s @ 0x%Lx\n" s eip
		       );
		     );
		     loop rest
		 | V.Block(_,_) -> failwith "Block unsupported"
		 | V.Function(_,_,_,_,_) -> failwith "Function unsupported"
		 | V.Return(_) -> failwith "Return unsupported"
		 | V.Call(_,_,_) -> failwith "Call unsupported"
		 | V.Attr(st, _) -> loop (st :: rest)
		 | V.Assert(e) ->
		     let v = self#eval_bool_exp e in
		       assert(v);
		       loop rest
		 | V.Halt(e) ->
		     let v = D.to_concrete_32 (self#eval_int_exp e) in
		       Printf.sprintf "halt_%Ld" v)
      in
	loop sl

    method run () = self#run_sl (fun lab -> true) insns

    method run_to_jump () =
      self#run_sl (fun lab -> (String.sub lab 0 3) <> "pc_") insns

    method measure_size =
      let measure_add k v n = n + (D.measure_size v) in
	mem#measure_size
	+ (V.VarHash.fold measure_add reg_store 0)
	+ (V.VarHash.fold measure_add temps 0)

    method store_byte_idx base idx b =
      self#store_byte (Int64.add base (Int64.of_int idx)) 
	(D.from_concrete_8 b)

    method store_str base idx str =
      for i = 0 to (String.length str - 1) do
	self#store_byte_idx (Int64.add base idx) i (Char.code str.[i])
      done

    val mutable symbolic_string_id = 0

    method make_symbolic_region base len =
      let varname = "input" ^ (string_of_int symbolic_string_id) in
	symbolic_string_id <- symbolic_string_id + 1;
	for i = 0 to len - 1 do
	  self#store_byte (Int64.add base (Int64.of_int i))
	    (form_man#fresh_symbolic_mem_8 varname (Int64.of_int i))
	done

    method store_symbolic_cstr base len =
      let varname = "input" ^ (string_of_int symbolic_string_id) ^ "_" in
	symbolic_string_id <- symbolic_string_id + 1;
	for i = 0 to len - 1 do
	  self#store_byte (Int64.add base (Int64.of_int i))
	    (form_man#fresh_symbolic_8 (varname ^ (string_of_int i)))
	done;
	self#store_byte_idx base len 0

    method store_symbolic_wcstr base len =
      let varname = "winput" ^ (string_of_int symbolic_string_id) ^ "_" in
	symbolic_string_id <- symbolic_string_id + 1;
	for i = 0 to len - 1 do
	  self#store_short (Int64.add base (Int64.of_int (2*i)))
	    (form_man#fresh_symbolic_16 (varname ^ (string_of_int i)))
	done;
	self#store_byte_idx base (2*len) 0;
	self#store_byte_idx base (2*len + 1) 0

    method store_symbolic_byte addr varname =
      self#store_byte addr (form_man#fresh_symbolic_8 varname)

    method store_symbolic_short addr varname =
      self#store_short addr (form_man#fresh_symbolic_16 varname)

    method store_symbolic_word addr varname =
      self#store_word addr (form_man#fresh_symbolic_32 varname)

    method store_symbolic_long addr varname =
      self#store_long addr (form_man#fresh_symbolic_64 varname)

    method store_mixed_bytes addr byte_array =
      let v_array = Array.map
	(function 
	   | (None, v) -> D.from_concrete_8 v
	   | (Some (s, i), v) -> form_man#make_concolic_mem_8 s i v)
	byte_array in
	match Array.length v_array with
	  | 1 -> self#store_byte addr v_array.(0)
	  | 2 -> self#store_short addr
	      (D.reassemble16 v_array.(0) v_array.(1))
	  | 4 -> self#store_word addr
	      (D.reassemble32 (D.reassemble16 v_array.(0) v_array.(1))
		 (D.reassemble16 v_array.(2) v_array.(3)))
	  | 8 -> self#store_long addr
	      (D.reassemble64
		 (D.reassemble32 (D.reassemble16 v_array.(0) v_array.(1))
		    (D.reassemble16 v_array.(2) v_array.(3)))
		 (D.reassemble32 (D.reassemble16 v_array.(4) v_array.(5))
		    (D.reassemble16 v_array.(6) v_array.(7))))
	  | _ -> failwith "Unsupported length in store_mixed_bytes"

    method parse_symbolic_expr str =
      Vine_parser.parse_exp_from_string (form_man#input_dl) str

    method store_cstr base idx str =
      self#store_str base idx str;
      self#store_byte_idx (Int64.add base idx) (String.length str) 0

    method read_buf addr len =
      Array.init len
	(fun i -> Char.chr
	   (D.to_concrete_8 (mem#load_byte (Int64.add addr (Int64.of_int i)))))

    method read_cstr addr =
      let rec bytes_loop i =
	let b = D.to_concrete_8 (mem#load_byte
				   (Int64.add addr (Int64.of_int i)))
	in
	  if b = 0 then [] else b :: bytes_loop (i + 1)
      in
	String.concat ""
	  (List.map (fun b -> String.make 1 (Char.chr b))
	     (bytes_loop 0))

    method zero_fill vaddr n =
      for i = 0 to n - 1 do
	self#store_byte_conc (Int64.add vaddr (Int64.of_int i)) 0
      done

    method print_backtrace =
      let read_addr addr =
	try
	  let v = self#load_word_conc addr in
	  (v, Printf.sprintf "0x%08Lx" v)
	with NotConcrete(s) -> (0L, "<symbolic " ^ (V.exp_to_string s) ^ ">")
      in
      let rec loop ebp =
	let (prev_ebp, prev_ebp_s) = read_addr ebp and
	    (_, ret_addr_s) = read_addr (Int64.add ebp 4L) in
	  Printf.printf "0x%08Lx %s %s\n" ebp prev_ebp_s ret_addr_s;
	  if (prev_ebp <> 0L) then
	    loop prev_ebp
      in
	loop (self#get_word_var R_EBP)

    method private eval_expr_to_string e =
      match self#eval_int_exp_ty e with
	| (v, V.REG_1) -> D.to_string_1 v
	| (v, V.REG_8) -> D.to_string_8 v
	| (v, V.REG_16) -> D.to_string_16 v
	| (v, V.REG_32) -> D.to_string_32 v
	| (v, V.REG_64) -> D.to_string_64 v
	| _ -> failwith "Unexpected type in eval_expr_to_string"

    method watchpoint =
      match !opt_watch_expr with
	| Some e -> Printf.printf "Watched expression %s = %s\n"
	    (match !opt_watch_expr_str with Some s -> s | None -> "???")
	      (self#eval_expr_to_string e)
	| None -> ()

    method mem_val_as_string addr ty =
      match ty with
	| V.REG_8  -> D.to_string_8  (self#load_byte addr)
	| V.REG_16 -> D.to_string_16 (self#load_byte addr)
	| V.REG_32 -> D.to_string_32 (self#load_byte addr)
	| V.REG_64 -> D.to_string_64 (self#load_byte addr)
	| _ -> failwith "Unexpected type in mem_val_as_string"
  end
end

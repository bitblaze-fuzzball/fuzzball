(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

open Exec_domain;;
open Exec_exceptions;;
open Exec_utils;;
open Exec_options;;
open Formula_manager;;
open Query_engine;;
open Stpvc_engine;;
open Stp_external_engine;;
open Concrete_memory;;
open Granular_memory;;

let bool64 f = fun a b -> if (f a b) then 1L else 0L

let move_hash src dest =
  V.VarHash.clear dest;
  V.VarHash.iter (fun a b -> V.VarHash.add dest a b) src

let skip_strings =
  (let h = Hashtbl.create 2 in
     Hashtbl.replace h "NoOp" ();
     Hashtbl.replace h "x86g_use_seg_selector" ();
     h)

(* The interface for Vine to give us the disassembly of an instruction
   is to put it in a comment, but it uses comments for other things as
   well. So this code tries to filter out all the things that are not
   instruction disassemblies. It would be cleaner to have a special
   syntax. *)
let comment_is_insn s =
  (not (Hashtbl.mem skip_strings s))
  && ((String.length s < 13) || (String.sub s 0 13) <> "eflags thunk:")

class virtual special_handler = object(self)
  method virtual handle_special : string -> V.stmt list option
  method virtual make_snap : unit
  method virtual reset : unit
end

type register_name = 
  (* VEX generic *)
  | R_CC_OP | R_CC_DEP1 | R_CC_DEP2 | R_CC_NDEP
  | R_IP_AT_SYSCALL | R_EMWARN | R_EMNOTE
  (* Common to x86, x64, and ARM: *)
  | R_CF | R_ZF
  (* Common to x86 and x64: *)
  | R_PF | R_AF | R_SF | R_OF
  | R_DFLAG | R_IDFLAG | R_ACFLAG
  | R_CS | R_DS| R_ES | R_FS | R_GS | R_SS
  | R_FTOP | R_FPROUND | R_FC3210 | R_SSEROUND 
  (* x86 *)
  | R_EBP | R_ESP | R_ESI | R_EDI | R_EIP | R_EAX | R_EBX | R_ECX | R_EDX
  | EFLAGSREST | R_LDT | R_GDT 
  (* x64 *)
  | R_RBP | R_RSP | R_RSI | R_RDI | R_RIP | R_RAX | R_RBX | R_RCX | R_RDX
  | R_R8 | R_R9 | R_R10 | R_R11 | R_R12 | R_R13 | R_R14 | R_R15
  | R_RFLAGSREST
  (* ARM *)
  | R0 | R1 |  R2 |  R3 |  R4 |  R5 |  R6 |  R7
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | R15T
  |  R_D0 |  R_D1 |  R_D2 |  R_D3 |  R_D4 |  R_D5 |  R_D6 |  R_D7
  |  R_D8 |  R_D9 | R_D10 | R_D11 | R_D12 | R_D13 | R_D14 | R_D15
  | R_D16 | R_D17 | R_D18 | R_D19 | R_D20 | R_D21 | R_D22 | R_D23
  | R_D24 | R_D25 | R_D26 | R_D27 | R_D28 | R_D29 | R_D30 | R_D31
  | R_CC | R_NF | R_VF
  | R_QFLAG32 | R_GEFLAG0 | R_GEFLAG1 | R_GEFLAG2 | R_GEFLAG3
  | R_TISTART | R_TILEN | R_NRADDR
  | R_FPSCR | R_TPIDRURO | R_ITSTATE

let reg_to_regstr reg = match reg with
  | R_EBP -> "R_EBP" | R_ESP -> "R_ESP" | R_ESI -> "R_ESI"
  | R_EDI -> "R_EDI" | R_EIP -> "R_EIP" | R_EAX -> "R_EAX" | R_EBX -> "R_EBX"
  | R_ECX -> "R_ECX" | R_EDX -> "R_EDX"
  | R_RBP -> "R_RBP" | R_RSP -> "R_RSP" | R_RSI -> "R_RSI"
  | R_RDI -> "R_RDI" | R_RIP -> "R_RIP" | R_RAX -> "R_RAX" | R_RBX -> "R_EBX"
  | R_RCX -> "R_RCX" | R_RDX -> "R_RDX"
  | R_R8 -> "R_R8" | R_R9 -> "R_R9" | R_R10 -> "R_R10" | R_R11 -> "R_R11"
  | R_R12 -> "R_R12" | R_R13 -> "R_R13" | R_R14 -> "R_R14" | R_R15 -> "R_R15"
  | R_RFLAGSREST -> "R_RFLAGSREST"
  | EFLAGSREST -> "EFLAGSREST" | R_CF -> "R_CF" | R_PF -> "R_PF"
  | R_AF -> "R_AF"| R_ZF -> "R_ZF" | R_SF -> "R_SF" | R_OF -> "R_OF"
  | R_CC_OP -> "R_CC_OP" | R_CC_DEP1 -> "R_CC_DEP1"
  | R_CC_DEP2 -> "R_CC_DEP2" | R_CC_NDEP -> "R_CC_NDEP"
  | R_DFLAG -> "R_DFLAG" | R_IDFLAG -> "R_IDFLAG" | R_ACFLAG -> "R_ACFLAG"
  | R_EMWARN -> "R_EMWARN" | R_EMNOTE -> "R_EMNOTE"
  | R_LDT -> "R_LDT" | R_GDT -> "R_GDT" | R_CS -> "R_CS" | R_DS -> "R_DS"
  | R_ES -> "R_ES" | R_FS -> "R_FS" | R_GS -> "R_GS"| R_SS -> "R_SS"
  | R_FTOP -> "R_FTOP" | R_FPROUND -> "R_FPROUND" | R_FC3210  -> "R_FC3210"
  | R_SSEROUND -> "R_SSEROUND" | R_IP_AT_SYSCALL -> "R_IP_AT_SYSCALL"
  | R0  ->  "R0" | R1  ->  "R1" |  R2 ->  "R2" | R3  -> "R3"
  | R4  ->  "R4" | R5  ->  "R5" |  R6 ->  "R6" | R7  -> "R7"
  | R8  ->  "R8" | R9  ->  "R9" | R10 -> "R10" | R11 -> "R11"
  | R12 -> "R12" | R13 -> "R13" | R14 -> "R14" | R15 -> "R15"
  | R15T -> "R15T"
  | R_D0  -> "R_D0"  | R_D1  -> "R_D1"  | R_D2  -> "R_D2"  | R_D3  ->  "R_D3"
  | R_D4  -> "R_D4"  | R_D5  -> "R_D5"  | R_D6  -> "R_D6"  | R_D7  ->  "R_D7"
  | R_D8  -> "R_D8"  | R_D9  -> "R_D9"  | R_D10 -> "R_D10" | R_D11 -> "R_D11"
  | R_D12 -> "R_D12" | R_D13 -> "R_D13" | R_D14 -> "R_D14" | R_D15 -> "R_D15"
  | R_D16 -> "R_D16" | R_D17 -> "R_D17" | R_D18 -> "R_D18" | R_D19 -> "R_D19"
  | R_D20 -> "R_D20" | R_D21 -> "R_D21" | R_D22 -> "R_D22" | R_D23 -> "R_D23"
  | R_D24 -> "R_D24" | R_D25 -> "R_D25" | R_D26 -> "R_D26" | R_D27 -> "R_D27"
  | R_D28 -> "R_D28" | R_D29 -> "R_D29" | R_D30 -> "R_D30" | R_D31 -> "R_D31"
  | R_CC -> "R_CC" | R_NF -> "R_NF" | R_VF -> "R_VF" | R_QFLAG32 -> "R_QFLAG32"
  | R_GEFLAG0 -> "R_GEFLAG0" | R_GEFLAG1 -> "R_GEFLAG1"
  | R_GEFLAG2 -> "R_GEFLAG2" | R_GEFLAG3 -> "R_GEFLAG3"
  | R_TISTART -> "R_TISTART" | R_TILEN -> "R_TILEN"
  | R_NRADDR -> "R_NRADDR"
  | R_FPSCR -> "R_FPSCR" | R_TPIDRURO -> "R_TPIDRURO"
  | R_ITSTATE -> "R_ITSTATE"

let regstr_to_reg s = match s with
  | "R_EBP" -> R_EBP | "R_ESP" -> R_ESP | "R_ESI" -> R_ESI
  | "R_EDI" -> R_EDI | "R_EIP" -> R_EIP | "R_EAX" -> R_EAX | "R_EBX" -> R_EBX
  | "R_ECX" -> R_ECX | "R_EDX" -> R_EDX
  | "R_RBP" -> R_RBP | "R_RSP" -> R_RSP | "R_RSI" -> R_RSI
  | "R_RDI" -> R_RDI | "R_RIP" -> R_RIP | "R_RAX" -> R_RAX | "R_RBX" -> R_RBX
  | "R_RCX" -> R_RCX | "R_RDX" -> R_RDX
  | "R_R8" -> R_R8 | "R_R9" -> R_R9 | "R_R10" -> R_R10 | "R_R11" -> R_R11
  | "R_R12" -> R_R12 | "R_R13" -> R_R13 | "R_R14" -> R_R14 | "R_R15" -> R_R15
  | "R_RFLAGSREST" -> R_RFLAGSREST
  | "EFLAGSREST" -> EFLAGSREST | "R_CF" -> R_CF | "R_PF" -> R_PF
  | "R_AF" -> R_AF| "R_ZF" -> R_ZF | "R_SF" -> R_SF | "R_OF" -> R_OF
  | "R_CC_OP" -> R_CC_OP | "R_CC_DEP1" -> R_CC_DEP1
  | "R_CC_DEP2" -> R_CC_DEP2 | "R_CC_NDEP" -> R_CC_NDEP
  | "R_DFLAG" -> R_DFLAG | "R_IDFLAG" -> R_IDFLAG | "R_ACFLAG" -> R_ACFLAG
  | "R_EMWARN" -> R_EMWARN | "R_EMNOTE" -> R_EMNOTE
  | "R_LDT" -> R_LDT | "R_GDT" -> R_GDT | "R_CS" -> R_CS | "R_DS" -> R_DS
  | "R_ES" -> R_ES | "R_FS" -> R_FS | "R_GS" -> R_GS| "R_SS" -> R_SS
  | "R_FTOP" -> R_FTOP | "R_FPROUND" -> R_FPROUND | "R_FC3210"  -> R_FC3210
  | "R_SSEROUND" -> R_SSEROUND | "R_IP_AT_SYSCALL" -> R_IP_AT_SYSCALL
  | "R0"  ->  R0 | "R1"  ->  R1 |  "R2" ->  R2 | "R3"  -> R3
  | "R4"  ->  R4 | "R5"  ->  R5 |  "R6" ->  R6 | "R7"  -> R7
  | "R8"  ->  R8 | "R9"  ->  R9 | "R10" -> R10 | "R11" -> R11
  | "R12" -> R12 | "R13" -> R13 | "R14" -> R14 | "R15" -> R15
  | "R15T" -> R15T
  | "R_D0"  -> R_D0  | "R_D1"  -> R_D1  | "R_D2"  -> R_D2  | "R_D3"  -> R_D3
  | "R_D4"  -> R_D4  | "R_D5"  -> R_D5  | "R_D6"  -> R_D6  | "R_D7"  -> R_D7
  | "R_D8"  -> R_D8  | "R_D9"  -> R_D9  | "R_D10" -> R_D10 | "R_D11" -> R_D11
  | "R_D12" -> R_D12 | "R_D13" -> R_D13 | "R_D14" -> R_D14 | "R_D15" -> R_D15
  | "R_D16" -> R_D16 | "R_D17" -> R_D17 | "R_D18" -> R_D18 | "R_D19" -> R_D19
  | "R_D20" -> R_D20 | "R_D21" -> R_D21 | "R_D22" -> R_D22 | "R_D23" -> R_D23
  | "R_D24" -> R_D24 | "R_D25" -> R_D25 | "R_D26" -> R_D26 | "R_D27" -> R_D27
  | "R_D28" -> R_D28 | "R_D29" -> R_D29 | "R_D30" -> R_D30 | "R_D31" -> R_D31
  | "R_CC" -> R_CC | "R_NF" -> R_NF | "R_VF" -> R_VF | "R_QFLAG32" -> R_QFLAG32
  | "R_GEFLAG0" -> R_GEFLAG0 | "R_GEFLAG1" -> R_GEFLAG1
  | "R_GEFLAG2" -> R_GEFLAG2 | "R_GEFLAG3" -> R_GEFLAG3
  | "R_TISTART" -> R_TISTART | "R_TILEN" -> R_TILEN
  | "R_NRADDR" -> R_NRADDR
  | "R_FPSCR" -> R_FPSCR | "R_TPIDRURO" -> R_TPIDRURO
  | "R_ITSTATE" -> R_ITSTATE
  | _ -> failwith ("Unrecognized register name " ^ s)

class virtual fragment_machine = object
  method virtual init_prog : Vine.program -> unit
  method virtual set_frag : Vine.program -> unit
  method virtual concretize_misc : unit
  method virtual add_extra_eip_hook :
    (fragment_machine -> int64 -> unit) -> unit
  method virtual eip_hook : int64 -> unit
  method virtual get_eip : int64
  method virtual set_eip : int64 -> unit
  method virtual run_eip_hooks : unit
  method virtual get_esp : int64
  method virtual jump_hook : string -> int64 -> int64 -> unit
  method virtual run_jump_hooks : string -> int64 -> int64 -> unit
  
  method virtual set_cjmp_heuristic :
    (int64 -> int64 -> int64 -> float -> bool option -> bool option) -> unit

  method virtual on_missing_zero : unit
  method virtual on_missing_random : unit
  method virtual on_missing_symbol : unit

  method virtual make_regs_zero : unit
  method virtual make_regs_symbolic : unit
  method virtual load_x86_user_regs : Temu_state.userRegs -> unit
  method virtual print_regs : unit

  method virtual store_byte_conc  : int64 -> int   -> unit
  method virtual store_short_conc : int64 -> int   -> unit
  method virtual store_word_conc  : int64 -> int64 -> unit
  method virtual store_long_conc  : int64 -> int64 -> unit

  method virtual store_page_conc  : int64 -> string -> unit

  method virtual load_byte_conc  : int64 -> int
  method virtual load_short_conc : int64 -> int
  method virtual load_word_conc  : int64 -> int64
  method virtual load_long_conc  : int64 -> int64

  method virtual load_byte_concolic  : int64 -> int
  method virtual load_short_concolic : int64 -> int
  method virtual load_word_concolic  : int64 -> int64
  method virtual load_long_concolic  : int64 -> int64

  method virtual started_symbolic : bool
  method virtual maybe_start_symbolic : (unit -> unit) -> unit
  method virtual start_symbolic : unit

  method virtual finish_fuzz : string -> unit
  method virtual unfinish_fuzz : string -> unit
  method virtual finish_reasons : string list

  method virtual make_snap : unit -> unit
  method virtual reset : unit -> unit

  method virtual add_special_handler : special_handler -> unit

  method virtual get_bit_var   : register_name -> int
  method virtual get_byte_var  : register_name -> int
  method virtual get_short_var : register_name -> int
  method virtual get_word_var  : register_name -> int64
  method virtual get_long_var  : register_name -> int64

  method virtual get_bit_var_concolic   : register_name -> int
  method virtual get_byte_var_concolic  : register_name -> int
  method virtual get_short_var_concolic : register_name -> int
  method virtual get_word_var_concolic  : register_name -> int64
  method virtual get_long_var_concolic  : register_name -> int64

  method virtual set_bit_var   : register_name -> int   -> unit
  method virtual set_byte_var  : register_name -> int   -> unit
  method virtual set_short_var : register_name -> int   -> unit
  method virtual set_word_var  : register_name -> int64 -> unit
  method virtual set_long_var  : register_name -> int64 -> unit

  method virtual set_word_var_low_short   : register_name -> int -> unit
  method virtual set_word_var_low_byte    : register_name -> int -> unit
  method virtual set_word_var_second_byte : register_name -> int -> unit

  method virtual set_word_reg_symbolic : register_name -> string -> unit
  method virtual set_word_reg_concolic :
    register_name -> string -> int64 -> unit
  method virtual set_word_reg_fresh_symbolic : register_name -> string -> unit
  method virtual set_word_reg_fresh_region : register_name -> string -> unit

  method virtual run_sl : (string -> bool) -> Vine.stmt list -> string
		  
  method virtual run : unit -> string
  method virtual run_to_jump : unit -> string
  method virtual fake_call_to_from : int64 -> int64 -> Vine.stmt list
  method virtual disasm_insn_at : int64 -> string

  method virtual measure_mem_size : int * int * int
  method virtual measure_form_man_size : int * int
  method virtual measure_dt_size : int
  method virtual measure_size : int * int

  method virtual store_byte_idx : int64 -> int -> int -> unit

  method virtual store_str : int64 -> int64 -> string -> unit

  method virtual make_symbolic_region : int64 -> int -> unit

  method virtual store_symbolic_cstr : int64 -> int -> bool -> bool -> unit
  method virtual store_concolic_cstr : int64 -> string -> bool -> unit

  method virtual store_symbolic_wcstr : int64 -> int -> unit

  method virtual store_symbolic_byte  : int64 -> string -> unit
  method virtual store_symbolic_short : int64 -> string -> unit
  method virtual store_symbolic_word  : int64 -> string -> unit
  method virtual store_symbolic_long  : int64 -> string -> unit

  method virtual store_concolic_mem_byte :
    int64 -> string -> int64 -> int -> unit

  method virtual store_concolic_byte  : int64 -> string -> int   -> unit
  method virtual store_concolic_short : int64 -> string -> int   -> unit
  method virtual store_concolic_word  : int64 -> string -> int64 -> unit
  method virtual store_concolic_long  : int64 -> string -> int64 -> unit

  method virtual set_reg_conc_bytes : register_name 
    -> (int option array) -> unit
  method virtual set_reg_concolic_mem_bytes : register_name 
    -> ((string * int64 * int) option array) -> unit

  method virtual store_concolic_exp : int64 -> V.exp ->
    (string * int) list -> (string * int) list ->
    (string * int64) list -> (string * int64) list -> unit
  method virtual set_word_reg_concolic_exp : register_name -> V.exp ->
    (string * int) list -> (string * int) list ->
    (string * int64) list -> (string * int64) list -> unit

  method virtual mem_byte_has_loop_var  : int64 -> bool
  method virtual mem_short_has_loop_var : int64 -> bool
  method virtual mem_word_has_loop_var  : int64 -> bool
  method virtual mem_long_has_loop_var  : int64 -> bool
  method virtual word_reg_has_loop_var : register_name -> bool

  method virtual parse_symbolic_expr : string -> Vine.exp

  method virtual store_cstr : int64 -> int64 -> string -> unit

  method virtual read_buf : int64 -> int -> char array

  method virtual read_cstr : int64 -> string

  method virtual zero_fill : int64 -> int -> unit

  method virtual print_backtrace : unit

  method virtual eval_expr_to_int64 : Vine.exp -> int64
      
  method virtual eval_expr_to_symbolic_expr : Vine.exp -> Vine.exp

  method virtual watchpoint : unit

  method virtual mem_val_as_string : int64 -> Vine.typ -> string

  method virtual get_path_cond : Vine.exp list

  method virtual set_query_engine : Query_engine.query_engine -> unit

  method virtual query_with_path_cond : Vine.exp -> bool
    -> (bool * (string * int64) list)

  method virtual match_input_var : string -> int option

  method virtual print_tree : out_channel -> unit

  method virtual set_iter_seed : int -> unit

  method virtual finish_path : bool

  method virtual after_exploration : unit

  method virtual make_x86_segtables_symbolic : unit
  method virtual store_word_special_region :
    register_name -> int64 -> int64 -> unit

  method virtual get_word_var_concretize :
    register_name -> bool -> string -> int64

  method virtual load_byte_concretize  : int64 -> bool -> string -> int
  method virtual load_short_concretize : int64 -> bool -> string -> int
  method virtual load_word_concretize  : int64 -> bool -> string -> int64

  method virtual make_sink_region : string -> int64 -> unit
end

module FragmentMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module GM = GranularMemoryFunctor(D)
  module FormMan = FormulaManagerFunctor(D)

  let change_some_short_bytes form_man d bytes construct =
    assert(Array.length bytes = 2);
    let select old = function
      | None -> old
      | Some x -> construct x
    in
    let o0 = D.extract_8_from_16 d 0 and
	o1 = D.extract_8_from_16 d 1in
    let b0 = select o0 bytes.(0) and
	b1 = select o1 bytes.(1) in
      form_man#simplify16 (D.reassemble16 b0 b1)

  let change_some_word_bytes form_man d bytes construct =
    assert(Array.length bytes = 4);
    let select old = function
      | None -> old
      | Some x -> construct x
    in
    let o0 = D.extract_8_from_32 d 0 and
	o1 = D.extract_8_from_32 d 1 and
	o2 = D.extract_8_from_32 d 2 and
	o3 = D.extract_8_from_32 d 3 in
    let b0 = select o0 bytes.(0) and
	b1 = select o1 bytes.(1) and
	b2 = select o2 bytes.(2) and
	b3 = select o3 bytes.(3) in
      form_man#simplify32
	(D.reassemble32 (D.reassemble16 b0 b1) (D.reassemble16 b2 b3))

  let change_some_long_bytes form_man d bytes construct =
    assert(Array.length bytes = 8);
    let select old = function
      | None -> old
      | Some x -> construct x
    in
    let o0 = D.extract_8_from_32 d 0 and
	o1 = D.extract_8_from_32 d 1 and
	o2 = D.extract_8_from_32 d 2 and
	o3 = D.extract_8_from_32 d 3 and
	o4 = D.extract_8_from_32 d 4 and
	o5 = D.extract_8_from_32 d 5 and
	o6 = D.extract_8_from_32 d 6 and
	o7 = D.extract_8_from_32 d 7 in
    let b0 = select o0 bytes.(0) and
	b1 = select o1 bytes.(1) and
	b2 = select o2 bytes.(2) and
	b3 = select o3 bytes.(3) and
	b4 = select o4 bytes.(4) and
	b5 = select o5 bytes.(5) and
	b6 = select o6 bytes.(6) and
	b7 = select o7 bytes.(7) in
      form_man#simplify64
	(D.reassemble64
	   (D.reassemble32 (D.reassemble16 b0 b1) (D.reassemble16 b2 b3))
	   (D.reassemble32 (D.reassemble16 b4 b5) (D.reassemble16 b6 b7)))

  class frag_machine = object(self)
    val mem = (new GM.granular_second_snapshot_memory
		 (new GM.granular_snapshot_memory
		    (new GM.concrete_maybe_adaptor_memory
		       (new string_maybe_memory))
		    (new GM.granular_hash_memory))
		 (new GM.granular_hash_memory))

    val form_man = new FormMan.formula_manager
    method get_form_man = form_man

    val reg_store = V.VarHash.create 100
    val reg_to_var = Hashtbl.create 100
    val temps = V.VarHash.create 100
    val mutable mem_var = V.newvar "mem" (V.TMem(V.REG_32, V.Little))
    val mutable frag = ([], [])
    val mutable insns = []

    val mutable snap = (V.VarHash.create 1, V.VarHash.create 1)

    method init_prog (dl, sl) =
      List.iter
	(fun ((n,s,t) as v) ->
	   if s = "mem" then
	     mem_var <- v
	   else
	     (V.VarHash.add reg_store v (D.uninit);
	      Hashtbl.add reg_to_var (regstr_to_reg s) v)) dl;
      self#set_frag (dl, sl);
      let result = self#run () in
	match result with
	  | "fallthrough" -> ()
	  | _ -> failwith "Initial program should fall through"

    val mutable loop_cnt = 0L
    method get_loop_cnt = loop_cnt

    method set_frag (dl, sl) =
      frag <- (dl, sl);
      V.VarHash.clear temps;
      loop_cnt <- 0L;
      self#concretize_misc;
      insns <- sl

    method concretize_misc = ()

    val mutable extra_eip_hooks = []

    method add_extra_eip_hook f =
      extra_eip_hooks <- f :: extra_eip_hooks

    val unique_eips = Hashtbl.create 1001

    val mutable deferred_start_symbolic = None

    val mutable insn_count = 0L

    method eip_hook eip =
      (* Shouldn't be needed; we instead simplify the registers when
	 writing to them: *)
      (* self#simplify_regs; *)
      (match deferred_start_symbolic with
	 | Some setup ->
	     deferred_start_symbolic <- None;
	     raise (StartSymbolic(eip, setup))
	 | None -> ());
      if !opt_trace_registers then
	self#print_regs;
      if !opt_trace_eip then
	Printf.printf "EIP is 0x%08Lx\n" eip;
      (if !opt_trace_unique_eips then
	 (if Hashtbl.mem unique_eips eip then
	    ()
	  else
	    (Printf.printf "Saw new EIP 0x%08Lx\n" eip;
	     Hashtbl.add unique_eips eip ())));
      (* Libasmir.print_disasm_rawbytes Libasmir.Bfd_arch_i386 eip insn_bytes;
	 print_string "\n"; *)
      List.iter (fun fn -> (fn (self :> fragment_machine) eip))
	extra_eip_hooks;
      self#watchpoint

    method get_eip =
      match !opt_arch with
	| X86 -> self#get_word_var R_EIP
	| X64 -> self#get_word_var R_RIP
	| ARM -> self#get_word_var R15T

    method set_eip eip =
      match !opt_arch with
	| X86 -> self#set_word_var R_EIP eip
	| X64 -> self#set_word_var R_RIP eip
	| ARM -> self#set_word_var R15T eip

    method run_eip_hooks =
      self#eip_hook (self#get_eip)

    method get_esp =
      match !opt_arch with
	| X86 -> self#get_word_var R_ESP
	| X64 -> self#get_word_var R_RSP
	| ARM -> self#get_word_var R13

    val mutable call_stack = []

    method private trace_callstack last_insn last_eip eip =
      let pop_callstack esp =
	while match call_stack with
	  | (old_esp, _, _, _) :: _ when old_esp < esp -> true
	  | _ -> false do
	      call_stack <- List.tl call_stack
	done
      in
      let get_retaddr esp =
	match !opt_arch with
	  | X86 -> self#load_word_conc esp
	  | X64 -> self#load_long_conc esp
	  | ARM -> self#get_word_var R14
      in
      let size = match !opt_arch with
	| X86 -> 4L
	| X64 -> 8L
	| ARM -> 4L
      in
      let kind =
	match !opt_arch with
	  | X86 | X64 ->
	      let s = last_insn ^ "    " in
		if (String.sub s 0 4) = "call" then
		  "call"
		else if (String.sub s 0 3) = "ret" then
		  "return"
		else if (String.sub s 0 3) = "jmp" then
		  "unconditional jump"
		else if (String.sub s 0 1) = "j" then
		  "conditional jump"
		else
		  "not a jump"
	  | ARM ->
	      (* TODO: add similar parsing for ARM mnemonics *)
	      "not a jump"
      in
	match kind with
	  | "call" ->
	      let esp = self#get_esp in
	      let depth = List.length call_stack and
		  ret_addr = get_retaddr esp
	      in
		for i = 0 to depth - 1 do Printf.printf " " done;
		Printf.printf
		  "Call from 0x%08Lx to 0x%08Lx (return to 0x%08Lx)\n"
		  last_eip eip ret_addr;
		call_stack <- (esp, last_eip, eip, ret_addr) :: call_stack;
	  | "return" ->
	      let esp = self#get_esp in
		pop_callstack (Int64.sub esp size);
		let depth = List.length call_stack in
		  for i = 0 to depth - 2 do Printf.printf " " done;
		  Printf.printf "Return from 0x%08Lx to 0x%08Lx\n"
		    last_eip eip;
		  pop_callstack esp;
	  | _ -> ()

    method jump_hook last_insn last_eip eip =
      if !opt_trace_callstack then
	self#trace_callstack last_insn last_eip eip

    method run_jump_hooks last_insn last_eip eip =
      self#jump_hook last_insn last_eip eip

    method set_cjmp_heuristic
      (func:(int64 -> int64 -> int64 -> float -> bool option -> bool option))
      = ()

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

    method private make_x86_regs_zero =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R_EAX (D.from_concrete_32 0x00000000L);
	reg R_EBX (D.from_concrete_32 0x00000000L);
	reg R_ECX (D.from_concrete_32 0x00000000L);
	reg R_EDX (D.from_concrete_32 0x00000000L);
	reg R_EBP (D.from_concrete_32 0x00000000L);
	reg R_ESP (D.from_concrete_32 0x00000000L);
	reg R_ESI (D.from_concrete_32 0x00000000L);
	reg R_EDI (D.from_concrete_32 0x00000000L);
	reg R_CS (D.from_concrete_16 0);
	reg R_DS (D.from_concrete_16 0);
	reg R_ES (D.from_concrete_16 0);
	reg R_FS (D.from_concrete_16 0);
	reg R_GS (D.from_concrete_16 0);
	reg R_PF (D.from_concrete_1 0);
	reg R_CF (D.from_concrete_1 0);
	reg R_AF (D.from_concrete_1 0);
	reg R_SF (D.from_concrete_1 0);
	reg R_OF (D.from_concrete_1 0);
	reg R_ZF (D.from_concrete_1 0);
	reg R_FTOP (D.from_concrete_32 0L);	
	reg EFLAGSREST (D.from_concrete_32 0L);
	reg R_LDT (D.from_concrete_32 0x00000000L);
	reg R_GDT (D.from_concrete_32 0x00000000L);
	reg R_DFLAG (D.from_concrete_32 1L);
	reg R_IDFLAG (D.from_concrete_32 0L);
	reg R_ACFLAG (D.from_concrete_32 0L);
	reg R_CC_OP   (D.from_concrete_32 0L);
	reg R_CC_DEP1 (D.from_concrete_32 0L);
	reg R_CC_DEP2 (D.from_concrete_32 0L);
	reg R_CC_NDEP (D.from_concrete_32 0L);
	reg R_SSEROUND (D.from_concrete_32 0L);
	()

    method private make_x64_regs_zero =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R_RAX (D.from_concrete_64 0x0000000000000000L);
	reg R_RBX (D.from_concrete_64 0x0000000000000000L);
	reg R_RCX (D.from_concrete_64 0x0000000000000000L);
	reg R_RDX (D.from_concrete_64 0x0000000000000000L);
	reg R_RBP (D.from_concrete_64 0x0000000000000000L);
	reg R_RSP (D.from_concrete_64 0x0000000000000000L);
	reg R_RSI (D.from_concrete_64 0x0000000000000000L);
	reg R_RDI (D.from_concrete_64 0x0000000000000000L);
	reg R_R8  (D.from_concrete_64 0x0000000000000000L);
	reg R_R9  (D.from_concrete_64 0x0000000000000000L);
	reg R_R10 (D.from_concrete_64 0x0000000000000000L);
	reg R_R11 (D.from_concrete_64 0x0000000000000000L);
	reg R_R12 (D.from_concrete_64 0x0000000000000000L);
	reg R_R13 (D.from_concrete_64 0x0000000000000000L);
	reg R_R14 (D.from_concrete_64 0x0000000000000000L);
	reg R_R15 (D.from_concrete_64 0x0000000000000000L);
	reg R_PF (D.from_concrete_1 0);
	reg R_CF (D.from_concrete_1 0);
	reg R_AF (D.from_concrete_1 0);
	reg R_SF (D.from_concrete_1 0);
	reg R_OF (D.from_concrete_1 0);
	reg R_ZF (D.from_concrete_1 0);
	reg R_FTOP (D.from_concrete_32 0L);	
	reg R_RFLAGSREST (D.from_concrete_64 0L);
	reg R_DFLAG (D.from_concrete_64 1L);
	reg R_IDFLAG (D.from_concrete_64 0L);
	reg R_ACFLAG (D.from_concrete_64 0L);
	reg R_CC_OP   (D.from_concrete_64 0L);
	reg R_CC_DEP1 (D.from_concrete_64 0L);
	reg R_CC_DEP2 (D.from_concrete_64 0L);
	reg R_CC_NDEP (D.from_concrete_64 0L);
	reg R_SSEROUND (D.from_concrete_64 0L);
	()

    method private make_arm_regs_zero =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R0   (D.from_concrete_32 0x00000000L);
	reg R1   (D.from_concrete_32 0x00000000L);
	reg R2   (D.from_concrete_32 0x00000000L);
	reg R3   (D.from_concrete_32 0x00000000L);
	reg R4   (D.from_concrete_32 0x00000000L);
	reg R5   (D.from_concrete_32 0x00000000L);
	reg R6   (D.from_concrete_32 0x00000000L);
	reg R7   (D.from_concrete_32 0x00000000L);
	reg R8   (D.from_concrete_32 0x00000000L);
	reg R9   (D.from_concrete_32 0x00000000L);
	reg R10  (D.from_concrete_32 0x00000000L);
	reg R11  (D.from_concrete_32 0x00000000L);
	reg R12  (D.from_concrete_32 0x00000000L);
	reg R13  (D.from_concrete_32 0x00000000L);
	reg R14  (D.from_concrete_32 0x00000000L);
	reg R15T (D.from_concrete_32 0x00000000L);
	reg R_NF (D.from_concrete_1 0);
	reg R_ZF (D.from_concrete_1 0);
	reg R_CF (D.from_concrete_1 0);
	reg R_VF (D.from_concrete_1 0);
	reg R_ITSTATE (D.from_concrete_32 0x00000000L);
	reg R_TPIDRURO (D.from_concrete_32 0x00000000L);
	reg R_FPSCR (D.from_concrete_32 0x00000000L);
	()

    method make_regs_zero =
      match !opt_arch with
	| X86 -> self#make_x86_regs_zero
	| X64 -> self#make_x64_regs_zero
	| ARM -> self#make_arm_regs_zero

    method private make_x86_regs_symbolic =
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

    method private make_x64_regs_symbolic =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R_RBP (form_man#fresh_symbolic_64 "initial_rbp");
	reg R_RSP (form_man#fresh_symbolic_64 "initial_rsp");
	reg R_RSI (form_man#fresh_symbolic_64 "initial_rsi");
	reg R_RDI (form_man#fresh_symbolic_64 "initial_rdi");
	reg R_RAX (form_man#fresh_symbolic_64 "initial_rax");
	reg R_RBX (form_man#fresh_symbolic_64 "initial_rbx");
	reg R_RCX (form_man#fresh_symbolic_64 "initial_rcx");
	reg R_RDX (form_man#fresh_symbolic_64 "initial_rdx");
	reg R_R8  (form_man#fresh_symbolic_64 "initial_r8");
	reg R_R9  (form_man#fresh_symbolic_64 "initial_r9");
	reg R_R10 (form_man#fresh_symbolic_64 "initial_r10");
	reg R_R11 (form_man#fresh_symbolic_64 "initial_r11");
	reg R_R12 (form_man#fresh_symbolic_64 "initial_r12");
	reg R_R13 (form_man#fresh_symbolic_64 "initial_r13");
	reg R_R14 (form_man#fresh_symbolic_64 "initial_r14");
	reg R_R15 (form_man#fresh_symbolic_64 "initial_r15");
	reg R_DFLAG (D.from_concrete_64 1L);
	reg R_ACFLAG (D.from_concrete_64 0L);
	reg R_IDFLAG (D.from_concrete_64 0L);
	reg R_RFLAGSREST (D.from_concrete_64 0L);
	reg R_PF (D.from_concrete_1 0);
	reg R_CF (D.from_concrete_1 0);
	reg R_AF (D.from_concrete_1 0);
	reg R_SF (D.from_concrete_1 0);
	reg R_OF (D.from_concrete_1 0);
	reg R_ZF (D.from_concrete_1 0);
	reg R_FTOP (D.from_concrete_32 0L);

    method private make_arm_regs_symbolic =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R0   (form_man#fresh_symbolic_32 "initial_r0");
	reg R1   (form_man#fresh_symbolic_32 "initial_r1");
	reg R2   (form_man#fresh_symbolic_32 "initial_r2");
	reg R3   (form_man#fresh_symbolic_32 "initial_r3");
	reg R4   (form_man#fresh_symbolic_32 "initial_r4");
	reg R5   (form_man#fresh_symbolic_32 "initial_r5");
	reg R6   (form_man#fresh_symbolic_32 "initial_r6");
	reg R7   (form_man#fresh_symbolic_32 "initial_r7");
	reg R8   (form_man#fresh_symbolic_32 "initial_r8");
	reg R9   (form_man#fresh_symbolic_32 "initial_r9");
	reg R10  (form_man#fresh_symbolic_32 "initial_r10");
	reg R11  (form_man#fresh_symbolic_32 "initial_r11");
	reg R12  (form_man#fresh_symbolic_32 "initial_r12");
	reg R13  (form_man#fresh_symbolic_32 "initial_r13");
	reg R14  (form_man#fresh_symbolic_32 "initial_r14");
	reg R15T (form_man#fresh_symbolic_32 "initial_r15");
	reg R_NF (form_man#fresh_symbolic_1  "initial_nf");
	reg R_ZF (form_man#fresh_symbolic_1  "initial_zf");
	reg R_CF (form_man#fresh_symbolic_1  "initial_cf");
	reg R_VF (form_man#fresh_symbolic_1  "initial_vf");
	reg R_ITSTATE (form_man#fresh_symbolic_32  "initial_itstate");
	()

    method make_regs_symbolic =
      match !opt_arch with	
	| X86 -> self#make_x86_regs_symbolic
	| X64 -> self#make_x64_regs_symbolic
	| ARM -> self#make_arm_regs_symbolic

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

    method private print_reg32 str r = 
	Printf.printf "%s: " str;
	Printf.printf "%s\n"
	  (D.to_string_32 
	     (self#get_int_var (Hashtbl.find reg_to_var r)))
     
    method private print_reg1 str r = 
	Printf.printf "%s: " str;
	Printf.printf "%s\n"
	  (D.to_string_1 
	     (self#get_int_var (Hashtbl.find reg_to_var r)))

    method private print_x86_regs =
      self#print_reg32 "%eax" R_EAX;
      self#print_reg32 "%ebx" R_EBX;
      self#print_reg32 "%ecx" R_ECX;
      self#print_reg32 "%edx" R_EDX;
      self#print_reg32 "%esi" R_ESI;
      self#print_reg32 "%edi" R_EDI;
      self#print_reg32 "%esp" R_ESP;
      self#print_reg32 "%ebp" R_EBP;
      self#print_reg1 "CF" R_CF;
      self#print_reg1 "PF" R_PF;
      self#print_reg1 "AF" R_AF;
      self#print_reg1 "ZF" R_ZF;
      self#print_reg1 "SF" R_SF;
      self#print_reg1 "OF" R_OF

    method private print_reg64 str r = 
	Printf.printf "%s: " str;
	Printf.printf "%s\n"
	  (D.to_string_64
	     (self#get_int_var (Hashtbl.find reg_to_var r)))

    method private print_x64_regs =
      self#print_reg64 "%rax" R_RAX;
      self#print_reg64 "%rbx" R_RBX;
      self#print_reg64 "%rcx" R_RCX;
      self#print_reg64 "%rdx" R_RDX;
      self#print_reg64 "%rsi" R_RSI;
      self#print_reg64 "%rdi" R_RDI;
      self#print_reg64 "%rsp" R_RSP;
      self#print_reg64 "%rbp" R_RBP;
      self#print_reg64 "%r8"  R_R8;
      self#print_reg64 "%r9"  R_R9;
      self#print_reg64 "%r10" R_R10;
      self#print_reg64 "%r11" R_R11;
      self#print_reg64 "%r12" R_R12;
      self#print_reg64 "%r13" R_R13;
      self#print_reg64 "%r14" R_R14;
      self#print_reg64 "%r15" R_R15;
      self#print_reg1 "CF" R_CF;
      self#print_reg1 "PF" R_PF;
      self#print_reg1 "AF" R_AF;
      self#print_reg1 "ZF" R_ZF;
      self#print_reg1 "SF" R_SF;
      self#print_reg1 "OF" R_OF

    method private print_arm_regs =
      self#print_reg32 " r0" R0;
      self#print_reg32 " r1" R1;
      self#print_reg32 " r2" R2;
      self#print_reg32 " r3" R3;
      self#print_reg32 " r4" R4;
      self#print_reg32 " r5" R5;
      self#print_reg32 " r6" R6;
      self#print_reg32 " r7" R7;
      self#print_reg32 " r8" R8;
      self#print_reg32 " r9" R9;
      self#print_reg32 "r10" R10;
      self#print_reg32 "r11" R11;
      self#print_reg32 "r12" R12;
      self#print_reg32 " sp" R13;
      self#print_reg32 " lr" R14;
      self#print_reg32 " pc" R15T;
      self#print_reg1 "NF" R_NF;
      self#print_reg1 "ZF" R_ZF;
      self#print_reg1 "CF" R_CF;
      self#print_reg1 "VF" R_VF;
      self#print_reg32 " IT" R_ITSTATE;
      ()

    method print_regs =
      match !opt_arch with	
	| X86 -> self#print_x86_regs
	| X64 -> self#print_x64_regs
	| ARM -> self#print_arm_regs

    method private simplify_reg32 r =
      let var = Hashtbl.find reg_to_var r in
	self#set_int_var var (form_man#simplify32 (self#get_int_var var))

    method private simplify_reg1 r =
      let var = Hashtbl.find reg_to_var r in
	self#set_int_var var (form_man#simplify1 (self#get_int_var var))

    method private simplify_x86_regs =
      self#simplify_reg32 R_EAX;
      self#simplify_reg32 R_EBX;
      self#simplify_reg32 R_ECX;
      self#simplify_reg32 R_EDX;
      self#simplify_reg32 R_ESI;
      self#simplify_reg32 R_EDI;
      self#simplify_reg32 R_ESP;
      self#simplify_reg32 R_EBP;
      self#simplify_reg1 R_CF;
      self#simplify_reg1 R_PF;
      self#simplify_reg1 R_AF;
      self#simplify_reg1 R_ZF;
      self#simplify_reg1 R_SF;
      self#simplify_reg1 R_OF;
      ()

    method private simplify_reg64 r =
      let var = Hashtbl.find reg_to_var r in
	self#set_int_var var (form_man#simplify64 (self#get_int_var var))

    method private simplify_x64_regs =
      self#simplify_reg64 R_RAX;
      self#simplify_reg64 R_RBX;
      self#simplify_reg64 R_RCX;
      self#simplify_reg64 R_RDX;
      self#simplify_reg64 R_RSI;
      self#simplify_reg64 R_RDI;
      self#simplify_reg64 R_RSP;
      self#simplify_reg64 R_RBP;
      self#simplify_reg64 R_R8;
      self#simplify_reg64 R_R9;
      self#simplify_reg64 R_R10;
      self#simplify_reg64 R_R11;
      self#simplify_reg64 R_R12;
      self#simplify_reg64 R_R13;
      self#simplify_reg64 R_R14;
      self#simplify_reg64 R_R15;
      self#simplify_reg1 R_CF;
      self#simplify_reg1 R_PF;
      self#simplify_reg1 R_AF;
      self#simplify_reg1 R_ZF;
      self#simplify_reg1 R_SF;
      self#simplify_reg1 R_OF;
      ()

    method private simplify_arm_regs =
      self#simplify_reg32 R0;
      self#simplify_reg32 R1;
      self#simplify_reg32 R2;
      self#simplify_reg32 R3;
      self#simplify_reg32 R4;
      self#simplify_reg32 R5;
      self#simplify_reg32 R6;
      self#simplify_reg32 R7;
      self#simplify_reg32 R8;
      self#simplify_reg32 R9;
      self#simplify_reg32 R10;
      self#simplify_reg32 R11;
      self#simplify_reg32 R12;
      self#simplify_reg32 R13;
      self#simplify_reg32 R14;
      self#simplify_reg32 R15;
      self#simplify_reg1 R_NF;
      self#simplify_reg1 R_ZF;
      self#simplify_reg1 R_CF;
      self#simplify_reg1 R_VF;
      self#simplify_reg32 R_ITSTATE;
      ()

    method private simplify_regs =
      match !opt_arch with	
	| X86 -> self#simplify_x86_regs
	| X64 -> self#simplify_x64_regs
	| ARM -> self#simplify_arm_regs

    method store_byte  addr b = mem#store_byte  addr b
    method store_short addr s = mem#store_short addr s
    method store_word  addr w = mem#store_word  addr w
    method store_long  addr l = mem#store_long  addr l

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

    method load_byte_concolic  addr =
      form_man#concolic_eval_8  (mem#load_byte  addr)
    method load_short_concolic addr =
      form_man#concolic_eval_16 (mem#load_short addr)
    method load_word_concolic  addr =
      form_man#concolic_eval_32 (mem#load_word  addr)
    method load_long_concolic  addr =
      form_man#concolic_eval_64 (mem#load_long  addr)

    val mutable started_symbolic = false

    method started_symbolic = started_symbolic

    method maybe_start_symbolic setup =
      if not started_symbolic then
	deferred_start_symbolic <- Some setup (* takes effect at end of insn *)
      else
	setup ()

    method start_symbolic =
      mem#inner_make_snap ();
      started_symbolic <- true

    val mutable special_handler_list = ([] : #special_handler list)

    method make_snap () =
      mem#make_snap ();
      snap <- (V.VarHash.copy reg_store, V.VarHash.copy temps);
      List.iter (fun h -> h#make_snap) special_handler_list

    val mutable fuzz_finish_reasons = []
    val mutable disqualified = false

    method finish_fuzz s =
      fuzz_finish_reasons <- s :: fuzz_finish_reasons;
      if !opt_trace_stopping then
	Printf.printf "Final iteration, %s\n" s

    method unfinish_fuzz s =
      fuzz_finish_reasons <- [];
      disqualified <- true;
      if !opt_trace_stopping then
	Printf.printf "Non-finish condition %s\n" s

    method finish_reasons =
      if disqualified then
	[]
      else
	fuzz_finish_reasons

    method reset () =
      mem#reset ();
      (match snap with (r, t) ->
	 move_hash r reg_store;
	 move_hash t temps);
      fuzz_finish_reasons <- [];
      disqualified <- false;
      List.iter (fun h -> h#reset) special_handler_list

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

    method get_bit_var_d   reg = self#get_int_var (Hashtbl.find reg_to_var reg)
    method get_byte_var_d  reg = self#get_int_var (Hashtbl.find reg_to_var reg)
    method get_short_var_d reg = self#get_int_var (Hashtbl.find reg_to_var reg)
    method get_word_var_d  reg = self#get_int_var (Hashtbl.find reg_to_var reg)
    method get_long_var_d  reg = self#get_int_var (Hashtbl.find reg_to_var reg)

    method get_bit_var   reg = D.to_concrete_1  (self#get_bit_var_d   reg)
    method get_byte_var  reg = D.to_concrete_8  (self#get_byte_var_d  reg)
    method get_short_var reg = D.to_concrete_16 (self#get_short_var_d reg)
    method get_word_var  reg = D.to_concrete_32 (self#get_word_var_d  reg)
    method get_long_var  reg = D.to_concrete_64 (self#get_long_var_d  reg)

    method get_bit_var_concolic reg =
      form_man#concolic_eval_1 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_byte_var_concolic reg =
      form_man#concolic_eval_8 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_short_var_concolic reg =
      form_man#concolic_eval_16
	(self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_word_var_concolic reg =
      form_man#concolic_eval_32
	(self#get_int_var (Hashtbl.find reg_to_var reg))

    method get_long_var_concolic reg =
      form_man#concolic_eval_64
	(self#get_int_var (Hashtbl.find reg_to_var reg))

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
      let newv = form_man#simplify32
	(D.assemble32 (D.from_concrete_16 v) high)
      in
	self#set_int_var var newv

    method set_word_var_low_byte reg v =
      let var = Hashtbl.find reg_to_var reg in
      let high_s = D.extract_16_from_32 (self#get_int_var var) 2 in
      let second_b = D.extract_8_from_32 (self#get_int_var var) 1 in
      let newv = form_man#simplify32
	(D.assemble32
	   (D.assemble16 (D.from_concrete_8 v) second_b) high_s)
      in
	self#set_int_var var newv

    method set_word_var_second_byte reg v =
      let var = Hashtbl.find reg_to_var reg in
      let high_s = D.extract_16_from_32 (self#get_int_var var) 2 in
      let low_b = D.extract_8_from_32 (self#get_int_var var) 0 in
      let newv = form_man#simplify32
	(D.assemble32
	   (D.assemble16 low_b (D.from_concrete_8 v)) high_s)
      in
	self#set_int_var var newv

    method set_word_reg_symbolic reg s =
      self#set_int_var (Hashtbl.find reg_to_var reg)
	(form_man#fresh_symbolic_32 s);

    method set_word_reg_concolic reg s i64 =
      self#set_int_var (Hashtbl.find reg_to_var reg)
	(form_man#make_concolic_32 s i64)

    val mutable symbol_uniq = 0
      
    method set_word_reg_fresh_symbolic reg s =
      self#set_word_reg_symbolic reg (s ^ "_" ^ (string_of_int symbol_uniq)); 
      symbol_uniq <- symbol_uniq + 1

    method set_word_reg_fresh_region reg s =
      let name = s ^ "_" ^ (string_of_int symbol_uniq) in
      (* Make up a fake value for things like null and alignment checks,
	 in case this run is concolic. *)
      let addr = Int64.add 0x70000000L
	(Int64.mul 0x10000L (Int64.of_int symbol_uniq))
      in
	self#set_int_var (Hashtbl.find reg_to_var reg)
	  (form_man#fresh_region_base_concolic name addr);
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

    method private eval_binop op v1 ty1 v2 ty2 =
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

    method private eval_unop op v1 ty1 =
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

    method private eval_cast kind ty v1 ty1 =
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

    method private eval_ite v_c v_t v_f ty_t =
      let func =
	match ty_t with
	  | V.REG_1  -> D.ite1
	  | V.REG_8  -> D.ite8
	  | V.REG_16 -> D.ite16
	  | V.REG_32 -> D.ite32
	  | V.REG_64 -> D.ite64
	  | _ -> failwith "unexpeceted type in eval_ite"
      in
	((func v_c v_t v_f), ty_t)

    method eval_int_exp_ty exp =
      match exp with
	| V.BinOp(op, e1, e2) ->
	    let (v1, ty1) = self#eval_int_exp_ty e1 and
		(v2, ty2) = self#eval_int_exp_ty e2 in
	      self#eval_binop op v1 ty1 v2 ty2
	| V.UnOp(op, e1) ->
	    let (v1, ty1) = self#eval_int_exp_ty e1 in
	      self#eval_unop op v1 ty1
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
	      self#eval_cast kind ty v1 ty1
	| V.Ite(cond, true_e, false_e) ->
	    let (v_c, ty_c) = self#eval_int_exp_ty cond and
		(v_t, ty_t) = self#eval_int_exp_ty true_e and
		(v_f, ty_f) = self#eval_int_exp_ty false_e in
	      assert(ty_c = V.REG_1);
	      assert(ty_t = ty_f);
	      self#eval_ite v_c v_t v_f ty_t
	(* XXX move this to something like a special handler: *)
	| V.Unknown("rdtsc") -> ((D.from_concrete_64 1L), V.REG_64) 
	| V.Unknown(_) ->
	    failwith "Unsupported unknown in eval_int_exp_ty"
	| V.Let(_,_,_)
	| V.Name(_)
	| V.Constant(V.Str(_))
	  -> failwith "Unsupported (or non-int) expr type in eval_int_exp_ty"
	    
    method private eval_int_exp exp =
      let (v, _) = self#eval_int_exp_ty exp in
	v

    method eval_int_exp_simplify exp =
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

    method eval_cjmp exp targ1 targ2 =
      self#eval_bool_exp exp

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

    val mutable last_eip = -1L
    val mutable last_insn = "none"
    val mutable saw_jump = false

    method run_sl do_jump sl =
      let jump lab =
	saw_jump <- true;
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
		 | V.CJmp(cond, V.Name(l1), V.Name(l2))
		     when
		       ((String.length l1 > 5) &&
			  ((String.sub l1 0 5) = "pc_0x")) ||
			 ((String.length l2 > 5) &&
			    ((String.sub l2 0 5) = "pc_0x")) ->
		     let a1 = try Vine.label_to_addr l1
		     with V.VineError(_) -> self#get_eip and
			 a2 = try Vine.label_to_addr l2
		     with V.VineError(_) -> self#get_eip in
		       if (self#eval_cjmp cond a1 a2) then
			 jump l1
		       else
			 jump l2
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
		 | V.Label(l) ->
		     if ((String.length l > 5) && 
			   (String.sub l 0 5) = "pc_0x") then
		       (let eip = Vine.label_to_addr l in
			  self#set_eip eip;
			  (* saw_jump will be set for the fallthrough
			     from one instruction to the next unless it's
			     optimized away (which it won't be when we
			     translate one instruction at a time), so it's
			     an overapproximation. *)
			  if saw_jump then
			    self#run_jump_hooks last_insn last_eip eip;
			  self#run_eip_hooks;
			  last_eip <- eip;
			  saw_jump <- false);
		     loop rest
		 | V.ExpStmt(e) ->
		     let v = self#eval_int_exp e in
		       ignore(v);
		       loop rest
		 | V.Comment(s) ->
		     if comment_is_insn s then
		       last_insn <- s;
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

    method fake_call_to_from func_addr ret_addr =
      match !opt_arch with
	| X86 ->
	    let esp = Hashtbl.find reg_to_var R_ESP in
	      [V.Move(V.Temp(esp),
		      V.BinOp(V.MINUS, V.Lval(V.Temp(esp)),
			      V.Constant(V.Int(V.REG_32, 4L))));
	       V.Move(V.Mem(mem_var, V.Lval(V.Temp(esp)), V.REG_32),
		      V.Constant(V.Int(V.REG_32, ret_addr)));
	       V.Jmp(V.Constant(V.Int(V.REG_32, func_addr)))]
	| _ -> failwith "Unsupported arch in fake_call_to_from"

    method disasm_insn_at eip = 
      let bytes = Array.init 16
	(fun i -> Char.chr (self#load_byte_conc
			      (Int64.add eip (Int64.of_int i))))
      in
	Libasmir.sprintf_disasm_rawbytes
	  (libasmir_arch_of_execution_arch !opt_arch)
	  false eip bytes

    method measure_mem_size = mem#measure_size
    method measure_form_man_size = form_man#measure_size
    method measure_dt_size = 0

    method measure_size =
      let measure_add k v n = n + (D.measure_size v) in
	((V.VarHash.fold measure_add reg_store 0),
	 (V.VarHash.fold measure_add temps 0))

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

    method store_symbolic_cstr base len fulllen terminate =
      let varname = "input" ^ (string_of_int symbolic_string_id) ^ "_" in
	symbolic_string_id <- symbolic_string_id + 1;
	for i = 0 to len - 1 do
	  let d = form_man#fresh_symbolic_8 (varname ^ (string_of_int i)) in
	    self#store_byte (Int64.add base (Int64.of_int i)) d;
	    if fulllen then
	      opt_extra_conditions :=
		V.BinOp(V.NEQ, V.Constant(V.Int(V.REG_8, 0L)),
			(D.to_symbolic_8 d))
	      :: !opt_extra_conditions
	done;
	if terminate then
	  self#store_byte_idx base len 0

    method store_concolic_cstr base str terminate =
      let len = String.length str in
      let varname = "input" ^ (string_of_int symbolic_string_id) ^ "_" in
	symbolic_string_id <- symbolic_string_id + 1;
	for i = 0 to len - 1 do
	  self#store_byte (Int64.add base (Int64.of_int i))
	    (form_man#make_concolic_8 (varname ^ (string_of_int i))
	       (Char.code str.[i]))
	done;
	if terminate then
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

    method store_concolic_mem_byte addr varname idx b =
      self#store_byte addr (form_man#make_concolic_mem_8 varname idx b)

    method store_concolic_byte addr varname i =
      self#store_byte addr (form_man#make_concolic_8 varname i)

    method store_concolic_short addr varname i =
      self#store_short addr (form_man#make_concolic_16 varname i)

    method store_concolic_word addr varname i64 =
      self#store_word addr (form_man#make_concolic_32 varname i64)

    method store_concolic_long addr varname i64 =
      self#store_long addr (form_man#make_concolic_64 varname i64)

    method set_reg_conc_bytes reg byte_array =
      let change_func = match Array.length byte_array with
	| 2 -> change_some_short_bytes form_man
	| 4 -> change_some_word_bytes form_man
	| 8 -> change_some_long_bytes form_man
	| _ -> failwith "Unsupported length in set_reg_conc_bytes"
      in
      let var = Hashtbl.find reg_to_var reg in
      let old_d = self#get_int_var var in
      let new_d =
	change_func old_d byte_array (fun b -> D.from_concrete_8 b)
      in
	self#set_int_var var new_d

    method set_reg_concolic_mem_bytes reg byte_array =
      let change_func = match Array.length byte_array with
	| 2 -> change_some_short_bytes form_man
	| 4 -> change_some_word_bytes form_man
	| 8 -> change_some_long_bytes form_man
	| _ -> failwith "Unsupported length in set_reg_concolic_mem_bytes"
      in
      let var = Hashtbl.find reg_to_var reg in
      let old_d = self#get_int_var var in
      let new_d =
	change_func old_d byte_array
	  (fun (s,i,v) -> form_man#make_concolic_mem_8 s i v)
      in
	self#set_int_var var new_d

    method private assemble_concolic_exp exp 
      byte_vars short_vars word_vars long_vars =
      let byte_ds =
	List.map (fun (s, v) -> (s, form_man#make_concolic_8 s v))
	  byte_vars in
      let short_ds =
	List.map (fun (s, v) -> (s, form_man#make_concolic_16 s v))
	  short_vars in
      let word_ds =
	List.map (fun (s, v) -> (s, form_man#make_concolic_32 s v))
	  word_vars in
      let long_ds =
	List.map (fun (s, v) -> (s, form_man#make_concolic_64 s v))
	  long_vars in
      let rec rw_loop e =
	match e with
	  | V.Unknown(s) ->
	      (try
		 (List.assoc s byte_ds, V.REG_8)
 	       with Not_found -> try
		 (List.assoc s short_ds, V.REG_16)
 	       with Not_found -> try
		 (List.assoc s word_ds, V.REG_32)
 	       with Not_found ->
		 (List.assoc s long_ds, V.REG_16))
	  | V.Constant(V.Int(V.REG_1, i)) ->
	      (D.from_concrete_1 (Int64.to_int i)), V.REG_1
	  | V.Constant(V.Int(V.REG_8, i)) ->
	      (D.from_concrete_8 (Int64.to_int i)), V.REG_8
	  | V.Constant(V.Int(V.REG_16,i)) -> 
	      (D.from_concrete_16 (Int64.to_int i)),V.REG_16
	  | V.Constant(V.Int(V.REG_32,i)) -> (D.from_concrete_32 i),V.REG_32
	  | V.Constant(V.Int(V.REG_64,i)) -> (D.from_concrete_64 i),V.REG_64
	  | V.Constant(V.Int(_, _)) ->
	      failwith "Unhandled weird-typed integer constant in concolic_exp"
	  | V.BinOp(op, e1, e2) ->
	      let (v1, ty1) = rw_loop e1 and
		  (v2, ty2) = rw_loop e2 in
		self#eval_binop op v1 ty1 v2 ty2
	  | V.UnOp(op, e1) ->
	      let (v1, ty1) = rw_loop e1 in
		self#eval_unop op v1 ty1
	  | V.Cast(kind, ty, e1) ->
	      let (v1, ty1) = rw_loop e1 in
		self#eval_cast kind ty v1 ty1
	  | V.Ite(ce, te, fe) ->
	    let (v_c, ty_c) = rw_loop ce and
		(v_t, ty_t) = rw_loop te and
		(v_f, ty_f) = rw_loop fe in
	      self#eval_ite v_c v_t v_f ty_t
	  | V.Let(_, _, _)
	  | V.Name(_)
	  | V.Lval(_)
	  | V.Constant(V.Str(_))
	    -> failwith "Unhandled expression type in concolic_exp"
      in
	rw_loop exp

    method store_concolic_exp addr exp bv sv wv lv =
      let (d, ty) = self#assemble_concolic_exp exp bv sv wv lv in
	match ty with
	  | V.REG_8  -> self#store_byte  addr d
	  | V.REG_16 -> self#store_short addr d
	  | V.REG_32 -> self#store_word  addr d
	  | V.REG_64 -> self#store_long  addr d
	  | _ -> failwith "Unsupported type in store_conolic_exp"

    method set_word_reg_concolic_exp reg exp bv sv wv lv =
      let (d, _) = self#assemble_concolic_exp exp bv sv wv lv in
	self#set_int_var (Hashtbl.find reg_to_var reg) d

    method mem_byte_has_loop_var addr =
      form_man#has_loop_var (self#load_byte addr)

    method mem_short_has_loop_var addr =
      form_man#has_loop_var (self#load_short addr)

    method mem_word_has_loop_var addr =
      form_man#has_loop_var (self#load_word addr)

    method mem_long_has_loop_var addr =
      form_man#has_loop_var (self#load_long addr)

    method word_reg_has_loop_var reg =
      form_man#has_loop_var
	(self#get_int_var (Hashtbl.find reg_to_var reg))      

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

    method eval_expr_to_int64 e =
      match self#eval_int_exp_ty e with
	| (v, V.REG_1) -> Int64.of_int (D.to_concrete_1 v)
	| (v, V.REG_8) -> Int64.of_int (D.to_concrete_8 v)
	| (v, V.REG_16) -> Int64.of_int (D.to_concrete_16 v)
	| (v, V.REG_32) -> D.to_concrete_32 v
	| (v, V.REG_64) -> D.to_concrete_64 v
	| _ -> failwith "Unexpected type in eval_expr_to_int64"

    method eval_expr_to_symbolic_expr e =
      match self#eval_int_exp_ty e with
	| (v, V.REG_1) -> D.to_symbolic_1 v
	| (v, V.REG_8) -> D.to_symbolic_8 v
	| (v, V.REG_16) -> D.to_symbolic_16 v
	| (v, V.REG_32) -> D.to_symbolic_32 v
	| (v, V.REG_64) -> D.to_symbolic_64 v
	| _ -> failwith "Unexpected type in eval_expr_to_symbolic_expr"

    method watchpoint =
      match !opt_watch_expr with
	| Some e -> Printf.printf "Watched expression %s = %s\n"
	    (match !opt_watch_expr_str with Some s -> s | None -> "???")
	      (self#eval_expr_to_string e)
	| None -> ()

    method mem_val_as_string addr ty =
      match ty with
	| V.REG_8  -> D.to_string_8  (self#load_byte  addr)
	| V.REG_16 -> D.to_string_16 (self#load_short addr)
	| V.REG_32 -> D.to_string_32 (self#load_word  addr)
	| V.REG_64 -> D.to_string_64 (self#load_long  addr)
	| _ -> failwith "Unexpected type in mem_val_as_string"

    method query_with_path_cond (e:Vine.exp) (v:bool)
      : (bool * (string * int64) list) =
      (false, [])
    method match_input_var (s:string) : int option = None
    method get_path_cond : Vine.exp list = []
    method on_missing_random : unit =
      failwith "FM.on_missing_random: unimplemented"
    method set_query_engine (qe:Query_engine.query_engine) = ()
    method print_tree (oc:out_channel) = ()
    method set_iter_seed (i:int) = ()
    method finish_path = false
    method after_exploration = ()
    method make_x86_segtables_symbolic = ()
    method store_word_special_region (r:register_name) (i1:int64) (i2:int64)
      : unit =
      failwith "store_word_special_region needs a symbolic region machine"
    method get_word_var_concretize r (b:bool) (s:string) = self#get_word_var r
    method load_byte_concretize  addr (b:bool) (s:string)
      = self#load_byte_conc addr
    method load_short_concretize addr (b:bool) (s:string)
      = self#load_short_conc addr
    method load_word_concretize  addr (b:bool) (s:string)
      = self#load_word_conc addr
    method make_sink_region (s:string) (i:int64) = ()
  end
end

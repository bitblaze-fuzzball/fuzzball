(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

class virtual special_handler : object
  method virtual handle_special : string -> Vine.stmt list option
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

val reg_to_regstr : register_name -> string
val regstr_to_reg : string -> register_name

val comment_is_insn : string -> bool

(* This virtual class is the outside interface to a fragment machine,
   hiding internal methods. It's also convenient that it hides the domain
   functors. *)
class virtual fragment_machine : object
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

  method virtual store_concolic_exp : int64 -> Vine.exp ->
    (string * int) list -> (string * int) list ->
    (string * int64) list -> (string * int64) list -> unit
  method virtual set_word_reg_concolic_exp : register_name -> Vine.exp ->
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

module FragmentMachineFunctor :
  functor (D : Exec_domain.DOMAIN) ->
sig
  class frag_machine : object
    method init_prog : Vine.program -> unit
    method set_frag : Vine.program -> unit
    method concretize_misc : unit
    method add_extra_eip_hook : (fragment_machine -> int64 -> unit) -> unit
    method eip_hook : int64 -> unit
    method get_eip : int64
    method set_eip : int64 -> unit
    method run_eip_hooks : unit
    method get_esp : int64
    method jump_hook : string -> int64 -> int64 -> unit
    method run_jump_hooks : string -> int64 -> int64 -> unit

    method set_cjmp_heuristic :
      (int64 -> int64 -> int64 -> float -> bool option -> bool option) -> unit

    method private on_missing_zero_m :
      Granular_memory.GranularMemoryFunctor(D).granular_memory -> unit
    method private on_missing_symbol_m :
      Granular_memory.GranularMemoryFunctor(D).granular_memory
      -> string -> unit
    method on_missing_zero : unit
    method on_missing_random : unit
    method on_missing_symbol : unit

    method make_regs_zero : unit
    method make_regs_symbolic : unit
    method load_x86_user_regs : Temu_state.userRegs -> unit
    method print_regs : unit

    method store_byte  : int64 -> D.t -> unit
    method store_short : int64 -> D.t -> unit
    method store_word  : int64 -> D.t -> unit
    method store_long  : int64 -> D.t -> unit

    method store_byte_conc  : int64 -> int   -> unit
    method store_short_conc : int64 -> int   -> unit
    method store_word_conc  : int64 -> int64 -> unit
    method store_long_conc  : int64 -> int64 -> unit

    method store_page_conc  : int64 -> string -> unit

    method private load_byte  : int64 -> D.t
    method private load_short : int64 -> D.t
    method private load_word  : int64 -> D.t
    method private load_long  : int64 -> D.t

    method load_byte_conc  : int64 -> int
    method load_short_conc : int64 -> int
    method load_word_conc  : int64 -> int64
    method load_long_conc  : int64 -> int64

    method load_byte_concolic  : int64 -> int
    method load_short_concolic : int64 -> int
    method load_word_concolic  : int64 -> int64
    method load_long_concolic  : int64 -> int64

    method started_symbolic : bool
    method maybe_start_symbolic : (unit -> unit) -> unit
    method start_symbolic : unit

    method finish_fuzz : string -> unit
    method unfinish_fuzz : string -> unit
    method finish_reasons : string list

    method make_snap : unit -> unit
    method reset : unit -> unit

    method add_special_handler : special_handler -> unit

    method handle_special : string -> Vine.stmt list option

    method private get_int_var : Vine.var -> D.t

    method get_bit_var_d   : register_name -> D.t
    method get_byte_var_d  : register_name -> D.t
    method get_short_var_d : register_name -> D.t
    method get_word_var_d  : register_name -> D.t
    method get_long_var_d  : register_name -> D.t

    method get_bit_var   : register_name -> int
    method get_byte_var  : register_name -> int
    method get_short_var : register_name -> int
    method get_word_var  : register_name -> int64
    method get_long_var  : register_name -> int64

    method get_bit_var_concolic   : register_name -> int
    method get_byte_var_concolic  : register_name -> int
    method get_short_var_concolic : register_name -> int
    method get_word_var_concolic  : register_name -> int64
    method get_long_var_concolic  : register_name -> int64

    method private set_int_var : Vine.var -> D.t -> unit

    method set_bit_var   : register_name -> int   -> unit
    method set_byte_var  : register_name -> int   -> unit
    method set_short_var : register_name -> int   -> unit
    method set_word_var  : register_name -> int64 -> unit
    method set_long_var  : register_name -> int64 -> unit

    method set_word_var_low_short   : register_name -> int -> unit
    method set_word_var_low_byte    : register_name -> int -> unit
    method set_word_var_second_byte : register_name -> int -> unit

    method set_word_reg_symbolic : register_name -> string -> unit
    method set_word_reg_concolic : register_name -> string -> int64 -> unit
    method set_word_reg_fresh_symbolic : register_name -> string -> unit
    method set_word_reg_fresh_region : register_name -> string -> unit

    method private handle_load : Vine.exp -> Vine.typ -> (D.t * Vine.typ)
    method private handle_store : Vine.exp -> Vine.typ -> Vine.exp -> unit

    method private maybe_concretize_binop :
      Vine.binop_type -> D.t -> D.t -> Vine.typ -> Vine.typ ->
      (D.t * D.t)

    method eval_int_exp_ty : Vine.exp -> (D.t * Vine.typ)
	    
    method private eval_int_exp : Vine.exp -> D.t

    method eval_int_exp_simplify : Vine.exp -> D.t

    method eval_cjmp : Vine.exp -> int64 -> int64 -> bool
    method eval_bool_exp : Vine.exp -> bool
    method eval_addr_exp : Vine.exp -> int64
    method eval_label_exp : Vine.exp -> string

    method jump : (string -> bool) -> string -> string

    method run_sl : (string -> bool) -> Vine.stmt list -> string
		  
    method run : unit -> string
    method run_to_jump : unit -> string
    method fake_call_to_from : int64 -> int64 -> Vine.stmt list
    method disasm_insn_at : int64 -> string

    method measure_mem_size : int * int * int
    method measure_form_man_size : int * int
    method measure_dt_size : int
    method measure_size : int * int

    method store_byte_idx : int64 -> int -> int -> unit

    method store_str : int64 -> int64 -> string -> unit

    method make_symbolic_region : int64 -> int -> unit

    method store_symbolic_cstr : int64 -> int -> bool -> bool -> unit
    method store_concolic_cstr : int64 -> string -> bool -> unit

    method store_symbolic_wcstr : int64 -> int -> unit

    method store_symbolic_byte  : int64 -> string -> unit
    method store_symbolic_short : int64 -> string -> unit
    method store_symbolic_word  : int64 -> string -> unit
    method store_symbolic_long  : int64 -> string -> unit

    method store_concolic_mem_byte : int64 -> string -> int64 -> int -> unit

    method store_concolic_byte  : int64 -> string -> int   -> unit
    method store_concolic_short : int64 -> string -> int   -> unit
    method store_concolic_word  : int64 -> string -> int64 -> unit
    method store_concolic_long  : int64 -> string -> int64 -> unit

    method set_reg_conc_bytes : register_name 
      -> (int option array) -> unit
    method set_reg_concolic_mem_bytes : register_name 
      -> ((string * int64 * int) option array) -> unit

    method store_concolic_exp : int64 -> Vine.exp ->
      (string * int) list -> (string * int) list ->
      (string * int64) list -> (string * int64) list -> unit
    method set_word_reg_concolic_exp : register_name -> Vine.exp ->
      (string * int) list -> (string * int) list ->
      (string * int64) list -> (string * int64) list -> unit

    method mem_byte_has_loop_var  : int64 -> bool
    method mem_short_has_loop_var : int64 -> bool
    method mem_word_has_loop_var  : int64 -> bool
    method mem_long_has_loop_var  : int64 -> bool
    method word_reg_has_loop_var : register_name -> bool

    method parse_symbolic_expr : string -> Vine.exp

    method store_cstr : int64 -> int64 -> string -> unit

    method read_buf : int64 -> int -> char array

    method read_cstr : int64 -> string

    method zero_fill : int64 -> int -> unit

    method print_backtrace : unit

    method private eval_expr_to_string : Vine.exp -> string

    method eval_expr_to_int64 : Vine.exp -> int64

    method eval_expr_to_symbolic_expr : Vine.exp -> Vine.exp

    method watchpoint : unit

    method mem_val_as_string : int64 -> Vine.typ -> string

    method get_loop_cnt : int64

    val form_man : Formula_manager.FormulaManagerFunctor(D).formula_manager
    method get_form_man :
      Formula_manager.FormulaManagerFunctor(D).formula_manager
    val reg_to_var : (register_name, Vine.var) Hashtbl.t
    val mem :
      Granular_memory.GranularMemoryFunctor(D).granular_second_snapshot_memory

    method get_path_cond : Vine.exp list
    method set_query_engine : Query_engine.query_engine -> unit
    method query_with_path_cond : Vine.exp -> bool
      -> (bool * (string * int64) list)
    method match_input_var : string -> int option
    method print_tree : out_channel -> unit
    method set_iter_seed : int -> unit
    method finish_path : bool
    method after_exploration : unit
    method make_x86_segtables_symbolic : unit
    method store_word_special_region :
      register_name -> int64 -> int64 -> unit
    method get_word_var_concretize :
      register_name -> bool -> string -> int64
    method load_byte_concretize  : int64 -> bool -> string -> int
    method load_short_concretize : int64 -> bool -> string -> int
    method load_word_concretize  : int64 -> bool -> string -> int64
    method make_sink_region : string -> int64 -> unit
  end
end

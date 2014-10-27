(*
  Copyright (C) BitBlaze, 2009-2013, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module SymRegionFragMachineFunctor :
  functor (D : Exec_domain.DOMAIN) ->
sig
  val is_high_mask : Vine.typ -> int64 -> bool

  val floor_log2 : int64 -> int

  val narrow_bitwidth :
    Formula_manager.FormulaManagerFunctor(D).formula_manager ->
    Vine.exp -> int

  val split_terms : Vine.exp ->
    Formula_manager.FormulaManagerFunctor(D).formula_manager ->
    Vine.exp list

  val select_one : 'a list -> (unit -> bool) -> ('a * 'a list)

  val sum_list : Vine.exp list -> Vine.exp 

  class sym_region_frag_machine : Decision_tree.decision_tree -> object
    method get_eip : int64
    method set_eip : int64 -> unit

    method eval_addr_exp_region : Vine.exp -> (int option * int64)
		  
    method eval_addr_exp : Vine.exp -> int64

    method get_word_var_concretize : Fragment_machine.register_name ->
      bool -> string -> int64

    method load_word_concretize  : int64 -> bool -> string -> int64
    method load_byte_concretize  : int64 -> bool -> string -> int
    method load_short_concretize : int64 -> bool -> string -> int

    method make_sink_region : string -> int64 -> unit

    method private maybe_concretize_binop : 
      Vine.binop_type -> D.t -> D.t -> Vine.typ -> Vine.typ ->
      (D.t * D.t)

    method private handle_load : Vine.exp -> Vine.typ -> (D.t * Vine.typ)

    method private handle_store : Vine.exp -> Vine.typ -> Vine.exp -> unit

    method concretize_misc : unit

    method make_sink_region : string -> int64 -> unit

    method make_x86_segtables_symbolic : unit

    method store_word_special_region :
      Fragment_machine.register_name -> int64 -> int64 -> unit

    method reset : unit -> unit

    method after_exploration : unit

    method get_depth : int
    method get_hist_str : string
    method set_influence_manager : Exec_no_influence.influence_manager -> unit
    method get_path_cond : Vine.exp list
    method add_to_path_cond : Vine.exp -> unit
    method restore_path_cond : (unit -> unit) -> unit
    method set_query_engine : Query_engine.query_engine -> unit
    method match_input_var : string -> int option
    method print_ce : (string * int64) list -> unit
    method input_depth : int
    method query_with_path_cond : Vine.exp -> bool
      -> (bool * (string * int64) list)
    method query_unique_value : Vine.exp -> Vine.typ -> int64 option
    method follow_or_random : bool 
    method query_with_pc_choice : Vine.exp -> bool -> (unit -> bool)
      -> (bool * Vine.exp)
    method extend_pc_random : Vine.exp -> bool -> bool
    method extend_pc_known : Vine.exp -> bool -> bool -> bool 
    method extend_pc_pref : Vine.exp -> bool -> bool -> bool
    method random_case_split : bool -> bool
    method set_cjmp_heuristic :
      (int64 -> int64 -> int64 -> float -> bool option -> bool option) -> unit
    method eval_cjmp : Vine.exp -> int64 -> int64 -> bool
    method eval_bool_exp : Vine.exp -> bool
    method on_missing_random : unit
    method on_missing_zero : unit
    method add_extra_eip_hook :
      (Fragment_machine.fragment_machine -> int64 -> unit) -> unit
    method eip_hook : int64 -> unit	  
    method finish_path : bool
    method print_tree : out_channel -> unit
    method set_iter_seed : int -> unit
    method init_prog : Vine.program -> unit
    method set_frag : Vine.program -> unit
    method get_esp : int64
    method run_eip_hooks : unit
    method jump_hook : string -> int64 -> int64 -> unit
    method run_jump_hooks : string -> int64 -> int64 -> unit
    method on_missing_symbol : unit
    method private on_missing_zero_m :
      Granular_memory.GranularMemoryFunctor(D).granular_memory -> unit
    method private on_missing_symbol_m :
      Granular_memory.GranularMemoryFunctor(D).granular_memory
      -> string -> unit
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
    method add_special_handler : Fragment_machine.special_handler -> unit
    method handle_special : string -> Vine.stmt list option
    method private get_int_var : Vine.var -> D.t
    method get_bit_var_d   : Fragment_machine.register_name -> D.t
    method get_byte_var_d  : Fragment_machine.register_name -> D.t
    method get_short_var_d : Fragment_machine.register_name -> D.t
    method get_word_var_d  : Fragment_machine.register_name -> D.t
    method get_long_var_d  : Fragment_machine.register_name -> D.t
    method get_bit_var   : Fragment_machine.register_name -> int
    method get_byte_var  : Fragment_machine.register_name -> int
    method get_short_var : Fragment_machine.register_name -> int
    method get_word_var  : Fragment_machine.register_name -> int64
    method get_long_var  : Fragment_machine.register_name -> int64
    method get_bit_var_concolic   : Fragment_machine.register_name -> int
    method get_byte_var_concolic  : Fragment_machine.register_name -> int
    method get_short_var_concolic : Fragment_machine.register_name -> int
    method get_word_var_concolic  : Fragment_machine.register_name -> int64
    method get_long_var_concolic  : Fragment_machine.register_name -> int64
    method private set_int_var : Vine.var -> D.t -> unit
    method set_bit_var   : Fragment_machine.register_name -> int   -> unit
    method set_byte_var  : Fragment_machine.register_name -> int   -> unit
    method set_short_var : Fragment_machine.register_name -> int   -> unit
    method set_word_var  : Fragment_machine.register_name -> int64 -> unit
    method set_long_var  : Fragment_machine.register_name -> int64 -> unit
    method set_word_var_low_short   : 
      Fragment_machine.register_name -> int -> unit
    method set_word_var_low_byte    :
      Fragment_machine.register_name -> int -> unit
    method set_word_var_second_byte :
      Fragment_machine.register_name -> int -> unit
    method set_word_reg_symbolic :
      Fragment_machine.register_name -> string -> unit
    method set_word_reg_concolic :
      Fragment_machine.register_name -> string -> int64 -> unit
    method set_word_reg_fresh_symbolic :
      Fragment_machine.register_name -> string -> unit
    method set_word_reg_fresh_region : 
      Fragment_machine.register_name -> string -> unit
    method eval_int_exp_ty : Vine.exp -> (D.t * Vine.typ)	    
    method private eval_int_exp : Vine.exp -> D.t
    method eval_int_exp_simplify : Vine.exp -> D.t
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
    method set_reg_conc_bytes : Fragment_machine.register_name 
      -> (int option array) -> unit
    method set_reg_concolic_mem_bytes : Fragment_machine.register_name 
      -> ((string * int64 * int) option array) -> unit
    method store_concolic_exp : int64 -> Vine.exp ->
      (string * int) list -> (string * int) list ->
      (string * int64) list -> (string * int64) list -> unit
    method set_word_reg_concolic_exp : Fragment_machine.register_name ->
      Vine.exp -> (string * int) list -> (string * int) list ->
      (string * int64) list -> (string * int64) list -> unit
    method mem_byte_has_loop_var  : int64 -> bool
    method mem_short_has_loop_var : int64 -> bool
    method mem_word_has_loop_var  : int64 -> bool
    method mem_long_has_loop_var  : int64 -> bool
    method word_reg_has_loop_var : Fragment_machine.register_name -> bool
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
    val reg_to_var :(Fragment_machine.register_name, Vine.var) Hashtbl.t
    val mem :
      Granular_memory.GranularMemoryFunctor(D).granular_second_snapshot_memory
  end
end

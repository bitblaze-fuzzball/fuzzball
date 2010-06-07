(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val solver_sats : int64 ref
val solver_unsats : int64 ref
val solver_fails : int64 ref

module SymPathFragMachineFunctor :
  functor (D : Exec_domain.DOMAIN) ->
sig
  class sym_path_frag_machine : object
    val dt : Decision_tree.decision_tree

    method add_to_path_cond : Vine.exp -> unit
      
    method restore_path_cond : (unit -> unit) -> unit

    method take_measure_eip : Vine.exp -> unit

    method take_measure_expr : Vine.exp -> Vine.exp -> unit

    method set_query_engine : Query_engine.query_engine -> unit

    method match_input_var : string -> int option

    method print_ce : (string * int64) list -> unit

    method measure_influence_common : Vine.decl list
      -> (Vine.var * Vine.exp) list -> Vine.exp -> Vine.exp -> float

    method measure_influence : Vine.exp -> float

    method compute_multipath_influence : string -> unit

    method compute_all_multipath_influence : unit

    method store_symbolic_byte_influence  : int64 -> string -> unit
    method store_symbolic_short_influence : int64 -> string -> unit
    method store_symbolic_word_influence  : int64 -> string -> unit
    method store_symbolic_long_influence  : int64 -> string -> unit

    method maybe_periodic_influence : unit

    method path_end_influence : unit

    method query_with_path_cond : Vine.exp -> bool -> bool

    method follow_or_random : bool 

    method query_with_pc : Vine.exp -> bool -> (unit -> bool)
      -> (bool * Vine.exp)

    method extend_pc_random : Vine.exp -> bool -> bool

    method extend_pc_known : Vine.exp -> bool -> bool -> bool 

    method random_case_split : bool -> bool

    method eval_bool_exp : Vine.exp -> bool

    method measure_point_influence : string -> Vine.exp -> unit

    method maybe_measure_influence_deref : Vine.exp -> unit

    method measure_influence_rep : unit

    method measure_influence_expr : Vine.exp -> unit

    method eval_addr_exp : Vine.exp -> int64

    method on_missing_random : unit

    method on_missing_zero : unit

    method disqualify_path : unit

    method eip_hook : int64 -> unit
	  
    method finish_path : bool

    method print_tree : out_channel -> unit

    method set_iter_seed : int -> unit
      
    method reset : unit -> unit


    method init_prog : Vine.program -> unit
    method set_frag : Vine.program -> unit
    method concretize_misc : unit
    method set_eip : int64 -> unit
    method run_eip_hooks : unit
    method on_missing_symbol : unit
    method private on_missing_zero_m :
      Granular_memory.GranularMemoryFunctor(D).granular_memory -> unit
    method private on_missing_symbol_m :
      Granular_memory.GranularMemoryFunctor(D).granular_memory
      -> string -> unit
    method make_x86_regs_zero : unit
    method make_x86_regs_symbolic : unit
    method load_x86_user_regs : Temu_state.userRegs -> unit
    method print_x86_regs : unit
    method private store_byte  : int64 -> D.t -> unit
    method private store_short : int64 -> D.t -> unit
    method private store_word  : int64 -> D.t -> unit
    method private store_long  : int64 -> D.t -> unit
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
    method start_symbolic : unit
    method make_snap : unit -> unit
    method add_special_handler : Fragment_machine.special_handler -> unit
    method handle_special : string -> Vine.stmt list option
    method private get_int_var : Vine.var -> D.t
    method get_bit_var   : Fragment_machine.register_name -> int
    method get_byte_var  : Fragment_machine.register_name -> int
    method get_short_var : Fragment_machine.register_name -> int
    method get_word_var  : Fragment_machine.register_name -> int64
    method get_long_var  : Fragment_machine.register_name -> int64
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
    method private handle_load : Vine.exp -> Vine.typ -> (D.t * Vine.typ)
    method private handle_store : Vine.exp -> Vine.typ -> Vine.exp -> unit
    method private maybe_concretize_binop :
      Vine.binop_type -> D.t -> D.t -> Vine.typ -> Vine.typ ->
      (D.t * D.t)
    method private eval_int_exp_ty : Vine.exp -> (D.t * Vine.typ)	    
    method private eval_int_exp : Vine.exp -> D.t
    method private eval_int_exp_simplify : Vine.exp -> D.t
    method eval_label_exp : Vine.exp -> string
    method jump : (string -> bool) -> string -> string
    method run_sl : (string -> bool) -> Vine.stmt list -> string
    method run : unit -> string
    method run_to_jump : unit -> string
    method measure_size : int
    method store_byte_idx : int64 -> int -> int -> unit
    method store_str : int64 -> int64 -> string -> unit
    method make_symbolic_region : int64 -> int -> unit
    method store_symbolic_cstr : int64 -> int -> unit
    method store_symbolic_wcstr : int64 -> int -> unit
    method store_symbolic_byte  : int64 -> string -> unit
    method store_symbolic_short : int64 -> string -> unit
    method store_symbolic_word  : int64 -> string -> unit
    method store_symbolic_long  : int64 -> string -> unit
    method store_mixed_bytes : int64 ->
      ((string * int64) option * int) array -> unit
    method parse_symbolic_expr : string -> Vine.exp
    method store_cstr : int64 -> int64 -> string -> unit
    method read_buf : int64 -> int -> char array
    method read_cstr : int64 -> string
    method zero_fill : int64 -> int -> unit
    method print_backtrace : unit
    method private eval_expr_to_string : Vine.exp -> string
    method watchpoint : unit
    method mem_val_as_string : int64 -> Vine.typ -> string
    val form_man : Formula_manager.FormulaManagerFunctor(D).formula_manager
    val mutable loop_cnt : int64
    val reg_to_var :(Fragment_machine.register_name, Vine.var) Hashtbl.t
    val mem :
      Granular_memory.GranularMemoryFunctor(D).granular_second_snapshot_memory
    method make_x86_segtables_symbolic : unit
    method store_word_special_region :
      Fragment_machine.register_name -> int64 -> int64 -> unit
    method get_word_var_concretize :
      Fragment_machine.register_name -> bool -> string -> int64
    method load_byte_concretize  : int64 -> bool -> string -> int
    method load_short_concretize : int64 -> bool -> string -> int
    method load_word_concretize  : int64 -> bool -> string -> int64
    method make_sink_region : string -> int64 -> unit
    method store_symbolic_byte_influence  : int64 -> string -> unit
    method store_symbolic_short_influence : int64 -> string -> unit
    method store_symbolic_word_influence  : int64 -> string -> unit
    method store_symbolic_long_influence  : int64 -> string -> unit
  end
end

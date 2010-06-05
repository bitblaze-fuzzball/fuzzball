(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val cmdline_opts              : (string * Arg.spec * string) list
val trace_replay_cmdline_opts : (string * Arg.spec * string) list

val set_program_name : string -> unit

val apply_cmdline_opts :
  < add_special_handler : Fragment_machine.special_handler -> 'a;
    get_short_var : Fragment_machine.register_name -> int;
    get_word_var : Fragment_machine.register_name -> int64;
    get_word_var_concretize : (Fragment_machine.register_name
			       -> bool -> string -> int64);
    load_byte_concretize : int64 -> bool -> string -> int;
    load_short_concretize : int64 -> bool -> string -> int;
    load_word_conc : int64 -> int64;
    load_word_concretize : int64 -> bool -> string -> int64;
    load_x86_user_regs : Temu_state.userRegs -> unit;
    make_x86_regs_symbolic : unit;
    make_x86_regs_zero : unit;
    on_missing_random : unit;
    on_missing_symbol : unit;
    on_missing_zero : unit;
    print_x86_regs : unit;
    read_buf : int64 -> int -> char array;
    read_cstr : int64 -> string;
    set_query_engine : Query_engine.query_engine -> unit;
    set_short_var : Fragment_machine.register_name -> int -> unit;
    set_word_var : Fragment_machine.register_name -> int64 -> unit;
    store_byte_conc : int64 -> int -> unit;
    store_byte_idx : int64 -> int -> int -> unit;
    store_cstr : int64 -> int64 -> string -> unit;
    store_long_conc : int64 -> int64 -> unit;
    store_page_conc : int64 -> string -> unit;
    store_short_conc : int64 -> int -> unit;
    store_str : int64 -> int64 -> string -> unit;
    store_word_conc : int64 -> int64 -> unit;
    watchpoint : unit;
    zero_fill : int64 -> int -> unit; .. >
  -> Vine.decl list -> unit

val make_symbolic_init :
  < make_sink_region : string -> int64 -> unit;
    make_symbolic_region : int64 -> int -> unit;
    parse_symbolic_expr : string -> Vine.exp;
    store_symbolic_byte : int64 -> string -> unit;
    store_symbolic_byte_influence : int64 -> string -> unit;
    store_symbolic_cstr : int64 -> int -> unit;
    store_symbolic_long : int64 -> string -> unit;
    store_symbolic_long_influence : int64 -> string -> unit;
    store_symbolic_short : int64 -> string -> unit;
    store_symbolic_short_influence : int64 -> string -> unit;
    store_symbolic_wcstr : int64 -> int -> unit;
    store_symbolic_word : int64 -> string -> unit;
    store_symbolic_word_influence : int64 -> string -> unit; .. >
  -> (unit -> unit)

val decide_start_addrs : unit -> (int64 * int64)

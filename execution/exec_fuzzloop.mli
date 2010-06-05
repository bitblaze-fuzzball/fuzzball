(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val fuzz : int64 -> int64 -> int64 list ->
  <
    compute_all_multipath_influence : unit;
    compute_multipath_influence : string -> unit;
    finish_path : bool;
    load_byte_conc : int64 -> int;
    make_snap : unit -> unit;
    measure_size : int;
    print_tree : out_channel -> unit;
    print_x86_regs : unit;
    reset : unit -> unit;
    run : unit -> string;
    run_eip_hooks : unit;
    set_eip : int64 -> unit;
    set_frag : Vine.decl list * Vine.stmt list -> unit;
    set_iter_seed : int -> unit;
    set_word_reg_symbolic : Fragment_machine.register_name -> string -> unit;
    set_word_var : Fragment_machine.register_name -> int64 -> unit;
    start_symbolic : unit;
    watchpoint : unit;
    .. >
    -> Asmir.varctx -> (unit -> unit) -> unit

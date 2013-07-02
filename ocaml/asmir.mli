(** High level interface to libasmir.
    
    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

(* This interface is a work-in-progress. I just created it now to avoid exposing
   some variables I added. --aij
*)


type asmprogram = Libasmir.asm_program_t

type arch
val arch_i386 : arch
val arch_x64 : arch
val arch_arm : arch


val exp_cache_enabled : bool ref


type varctx = (string, Vine.var) Hashtbl.t
val gamma_create : Vine.decl -> Vine.decl list -> varctx
val gamma_lookup : varctx -> string -> Vine.var
val gamma_extend : varctx -> string -> Vine.decl -> unit
val gamma_unextend : varctx -> string -> unit



val tr_exp : varctx -> Libasmir.exp -> Vine.exp
val tr_binop :
  varctx ->
  Libasmir.binop_type_t -> Libasmir.exp -> Libasmir.exp -> Vine.exp
val tr_lval : varctx -> Libasmir.exp -> Vine.lvalue
val tr_vardecl : varctx -> Libasmir.stmt -> Vine.var * (unit -> unit)
val tr_vardecls :
  varctx -> Libasmir.stmt list -> Vine.var list * (unit -> unit)
val tr_stmt : varctx -> Libasmir.stmt -> Vine.stmt
val tr_vine_block_t :
  varctx -> asmprogram -> Libasmir.vine_block_t -> Vine.stmt list
val tr_vine_blocks_t :
  varctx ->
  asmprogram -> Libasmir.vine_blocks_t -> Vine.stmt list
val instmap_translate_address_range :
  varctx ->
  Libasmir.instmap_t ->
  Libasmir.address_t ->
  Libasmir.address_t -> (Libasmir.address_t * Vine.stmt list) list


(* these shold be deprecated, but vine_tovine and some other stuff still uses
   them *)
val x86_regs : Vine.decl list
val x86_mem : Vine.decl
val x86_eflags_helpers : unit -> Vine.stmt list

(* newer interface to replace x86_* *)
val decls_for_arch : arch -> Vine.decl list
val gamma_for_arch : arch -> varctx
val helpers_for_arch : arch -> Vine.stmt list

val asmprogram_arch : asmprogram -> arch


val instmap_has_address : Libasmir.instmap_t -> Libasmir.address_t -> bool
val disassemble_program : string -> asmprogram
val asmprogram_to_vine : ?init_mem:bool -> asmprogram -> Vine.program
val asm_addr_to_vine :
  varctx -> asmprogram -> Libasmir.address_t -> Vine.stmt list
val tr_dyn_function_t :
  Libasmir.dyn_function_t -> Libasmir.address_t * string
val tr_vine_symbol_t :
  Libasmir._vine_symbol_t option -> Libasmir.address_t * string * bool * bool
val tr_dyn_functions_t :
  Libasmir.dyn_functions_t -> (Libasmir.address_t * string) list
val tr_vine_symbols_t :
  Libasmir.vine_symbols_t -> (Libasmir.address_t * string * bool * bool) list
val get_synthetic_functions : string -> (Libasmir.address_t * string) list
val vine_symbols_of_file :
  string -> (Libasmir.address_t * string * bool * bool) list
val tr_memory_cell_data_t :
  Libasmir.memory_cell_data_t -> Libasmir.address_t * int
val tr_memory_data_t :
  Libasmir.memory_data_t -> (Libasmir.address_t * int) list
val get_rodata_from_file : string -> (Libasmir.address_t * int) list
val get_bss_from_file : string -> (Libasmir.address_t * int) list

(* takes a char array for historical reasons, but a string would be better *)
val asm_bytes_to_vine : varctx -> arch -> Libasmir.address_t -> (*string*) char array -> Vine.stmt list

val print_i386_rawbytes : Libasmir.address_t -> char array -> unit
val print_disasm_rawbytes :
  arch -> Libasmir.address_t -> char array -> unit
val sprintf_i386_rawbytes : bool -> Libasmir.address_t -> char array -> string

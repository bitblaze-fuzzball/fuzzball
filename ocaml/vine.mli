(** Main Vine module

*)
exception ParseError of string
exception TypeError of string
exception Unimplemented of string
exception VineError of string

type label = string
type attr = string
type attributes = attr list

type endian = Little | Big

type typ =  REG_1
	   | REG_8
	   | REG_16
	   | REG_32
	   | REG_64
	   | TString 
	   | TMem of typ * endian
	   | TFun of typ option * typ list * bool
	   | Array  of typ * int64
	   | TAttr of typ * attributes


type var = int * string * typ

module Var :
sig
  type t = var
  val hash : var -> int
  val equal : var -> var -> bool
  val compare : var -> var -> int
end


(** Hash from variables to anything.
    This should work faster than using a generic hashtable, since it uses
    the trivial hash function and faster comparisons.
*)
module VarHash : Hashtbl.S with type key = var

(** Map from variables to anything.*)
module VarMap : Map.S with type key = var

(** set over variables *)
module VarSet : Set.S with type elt = var

type cast_type = CAST_UNSIGNED | CAST_SIGNED | CAST_HIGH | CAST_LOW
type fcast_type =
  | CAST_SFLOAT | CAST_UFLOAT | CAST_SFIX | CAST_UFIX
  | CAST_FWIDEN | CAST_FNARROW
type binop_type =
    PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | SDIVIDE
  | MOD
  | SMOD
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | BITAND
  | BITOR
  | XOR
  | EQ
  | NEQ
  | LT
  | LE
  | SLT
  | SLE
type fbinop_type = FPLUS | FMINUS | FTIMES | FDIVIDE | FEQ | FNEQ | FLT | FLE
type unop_type = NEG | NOT
type funop_type = FNEG
type value = Int of typ *  int64
	     | Str of string

type decl = var

type pos = string * int

type attribute = Pos of pos | ACall | AReturn
type lvalue = Temp of var
	      | Mem of var * exp * typ
and exp =
    BinOp of binop_type * exp * exp
  | FBinOp of fbinop_type * Vine_util.round_mode * exp * exp
  | UnOp of unop_type * exp
  | FUnOp of funop_type * Vine_util.round_mode * exp
  | Constant of value
  | Lval of lvalue
  | Name of label
  | Cast of cast_type * typ * exp
  | FCast of fcast_type * Vine_util.round_mode * typ * exp
  | Unknown of string
  | Let of lvalue * exp * exp
  | Ite of exp * exp * exp
type stmt =
    Jmp of exp
  | CJmp of exp * exp * exp
  | Move of lvalue * exp
  | Special of string
  | Label of label
  | ExpStmt of exp
  | Comment of string
  | Block of decl list * stmt list
  | Function of label * typ option * decl list * bool * stmt option
  | Return of exp option
  | Call of lvalue option * exp * exp list
  | Attr of stmt *  attribute
  | Assert of exp
  | Halt of exp 

type program = decl list * stmt list

val bool_t : typ
val char_t : typ
val addr_t : typ

val exp_true : exp
val exp_false : exp
val exp_bool : bool -> exp
val bool_of_const : exp -> bool

val exp_eq : exp -> exp -> exp
val exp_and : exp -> exp -> exp
val exp_or : exp -> exp -> exp
val exp_not : exp -> exp
val exp_implies : exp -> exp -> exp
val exp_plus : exp -> exp -> exp
val exp_ite : exp -> typ -> exp -> exp -> exp

val const_of_int64 : typ -> int64 -> exp
val const_of_int : typ -> int -> exp

val newvar : string -> typ -> var
val renewvar : var -> var
val newlab : label -> label

val bits_of_width : typ -> int
val binop_of_string : string -> binop_type
val unop_of_string : string -> unop_type
val casttype_of_string : string -> cast_type
val const_of_stringt : typ -> string -> value
val type_of_string : string -> typ

val attr_to_string : attribute -> string
val round_mode_to_string : Vine_util.round_mode -> string
val binop_to_string : binop_type -> string
val fbinop_to_string : fbinop_type -> string
val unop_to_string : unop_type -> string
val funop_to_string : funop_type -> string
val casttype_to_string : cast_type -> string
val fcasttype_to_string : fcast_type -> string
val type_to_string : typ -> string
val val_to_string : value -> string
val pp_var : (string -> unit) -> var -> unit
val pp_exp : (string -> unit) -> exp -> unit
val pp_lval : (string -> unit) -> lvalue -> unit
val pp_value : (string -> unit) -> value -> unit
val pp_typ : (string -> unit) -> typ -> unit
val exp_to_string : exp -> string
val lval_to_string : lvalue -> string
val decl_to_string : decl -> string
val var_to_string : var -> string
val stmt_to_string : stmt -> string
val stmt_to_channel : out_channel -> stmt -> unit
val pp_stmt : (string -> unit) -> stmt -> unit
val pp_program : (string -> unit) -> program -> unit
val format_value : Format.formatter -> value -> unit
val format_var : ?print_type:bool -> Format.formatter -> var -> unit
val format_typ : Format.formatter -> typ -> unit
val format_exp :  Format.formatter -> exp -> unit
val format_lval :  Format.formatter -> lvalue -> unit
val format_stmt :  Format.formatter -> stmt -> unit
val format_program : Format.formatter -> program -> unit
val format2pp : (Format.formatter -> 'a -> unit) -> (string -> unit) -> 'a -> unit
val format2string : (Format.formatter -> 'a -> unit) -> 'a -> string


type 'a visit_action =
    SkipChildren
  | DoChildren
  | ChangeTo of 'a
  | ChangeDoChildrenPost of 'a * ('a -> 'a)
class type vine_visitor =
  object
    method visit_alvalue : lvalue -> lvalue visit_action
    method visit_binding : lvalue * exp -> (lvalue * exp) visit_action
    method visit_decl : decl -> decl visit_action
    method visit_exp : exp -> exp visit_action
    method visit_rlvalue : lvalue -> lvalue visit_action
    method visit_stmt : stmt -> stmt visit_action
  end
class nop_vine_visitor : vine_visitor
val exp_accept : #vine_visitor -> exp -> exp
val _lvalue_accept : #vine_visitor ->
  (lvalue -> lvalue visit_action) -> lvalue -> lvalue
val alvalue_accept : #vine_visitor -> lvalue -> lvalue
val rlvalue_accept : #vine_visitor -> lvalue -> lvalue
val binding_accept : #vine_visitor -> lvalue * exp -> lvalue * exp
val decl_accept : #vine_visitor -> decl -> decl
val stmt_accept : #vine_visitor -> stmt -> stmt
val stmts_accept : #vine_visitor -> stmt list -> stmt list
val prog_accept : #vine_visitor -> program -> program
val freerefs_exp : exp -> lvalue list
val freerefs_stmt : stmt -> lvalue list
val onlymems : lvalue list -> lvalue list
val freemems_exp : exp -> lvalue list
val freetemps_exp : exp -> lvalue list
val get_req_ctx : exp -> decl list
val strip_attrs : stmt -> stmt * (stmt -> stmt)
val remove_stmt_attrs : stmt -> stmt
val remove_unknowns : stmt -> (stmt * stmt list)
val addr_to_label : int64 -> label
val label_to_addr : label -> int64
val replace_special_jumps :
   (stmt -> stmt) -> (stmt -> stmt) -> stmt list -> stmt list

val get_last_static_stmt : stmt list -> stmt option


(** [is_integer_type t] returns true when [t] is an integral type *)
val is_integer_type : typ -> bool

(** [is_float_type t] returns true when [t] is a type that could
    contain a floating point value *)
val is_float_type : typ -> bool

(** [unwind_type t] unwinds any type attributes and returns the
    real type *)
val unwind_type : typ -> typ

(** [remove_untypeable sl] will remove Vine.Special statements, and
    any statement containing the "Unknown" expression *)
val remove_untypeable : stmt list -> stmt list

(** [array_idx_type_to_size t] will give you back the range for type
    t. It was written to support legacy code to convert an index type
    to the number of elements that index type representes, e.g.
    in [var a:reg32_t\[reg32_t\]], this is really an array that takes in
    numbers from 0 to 2{^32}-1, and returns things of type [REG_32]
*)
val array_idx_type_to_size : typ -> int64


(** [idx_size_to_type i] will return the vine type needed for indexes
    of size i, i.e., if 0<i<255, then REG_8 is needed *)
val idx_size_to_type : int64 -> typ

(** return the type of a variable *)
val typeof_var : var -> typ

(** return true iff var is not type memory or array *)
val is_not_memory : var -> bool

(** return true iff binop is an arithmetic (not relational) operator *)
val is_arithmetic_op : binop_type -> bool

(** count the number of subexpressions in an expression *)
val exp_size : exp -> int

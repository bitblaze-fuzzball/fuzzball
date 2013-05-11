(*
*  vine_absyn.mli
*
*  Made by (David Brumley)
*  Login   <dbrumley@rtfm.ece.cmu.edu>
*
*  Started on  Mon Jun 11 15:34:09 2007 David Brumley
*  Last update Sun Aug  5 14:49:11 2007 David Brumley
*)

(** this file contains one abstract syntax for the vine language. This
    abstract syntax is then converted into the VinE IR.
  @author David Brumley
*)

(** The following mirror vine.ml's definitions for the most part. The
    only big changes are that a) call's can be expressions so that we
    can parse them, and b) we have declarations *)

(* type endian = Vine.endian *)

val strip_nums : bool ref

(* type program = absstmt list *)
(* and var = int * string * abstyp *)
type abspos = int * int


(* and absvalue =  *)
(*     Int of abstyp * int64 *)
(*   | Str of string *)

(* and abstyp = *)
(*     REG_1 *)
(*   | REG_8 *)
(*   | REG_16 *)
(*   | REG_32 *)
(*   | REG_64 *)
(*   | TString *)
(*   | TMem of abstyp * endian *)
(*   | Array of abstyp * int64 *)
(*   | TAttr of abstyp * Vine.attributes *)
(*   | TVoid *)
      
(* and abslval = *)
(*     ATemp of var * abspos *)
(*   | AMem of var * absexp * abstyp *  abspos *)

(* and absexp = *)
(*     ABinOp of Vine.binop_type * absexp * absexp * abspos *)
(*   | AUnOp of Vine.unop_type * absexp * abspos *)
(*   | AConstant of absvalue *  abspos *)
(*   | ALval of abslval * abspos *)
(*   | AName of Vine.label * abspos *)
(*   | ACast of Vine.cast_type * abstyp * absexp * abspos *)
(*   | AUnknown of string * abspos *)
(*   | ALet of abslval * absexp * absexp * abspos *)
(*   | ACall of absexp * absexp list * abspos *)

(* and absstmt = *)
(*     AJmp of absexp * abspos *)
(*   | ACJmp of absexp * absexp * absexp * abspos *)
(*   | AMove of abslval * absexp * abspos *)
(*   | ASpecial of string * abspos *)
(*   | ALabel of Vine.label * abspos *)
(*   | AExpStmt of absexp * abspos *)
(*   | AComment of string * abspos *)
(*   | ABlock of absstmt list * abspos *)
(*   | AReturn of absexp option * abspos *)
(*   | AVarDecl of var * abspos *)
(*   | AFunDecl of Vine.label * abstyp * var list * bool * abspos *)
(*   | AFunction of Vine.label * abstyp * var list * absstmt * abspos *)
(*   | ANop of abspos *)

val lineNum : int ref
val linePos : int list ref
val unknown_pos : int * int
val track_line_numbers : bool ref
val set_line_tracking : bool -> unit
type linePosT = Pos of int | PosNewFile of int * int * string
type info = {
  mutable linenum : int;
  mutable linepos : linePosT list;
  mutable fileName : string;
  mutable errors : bool;
  mutable is_reversed : bool;
}
val current : info
val extractFileName : string -> string
val startFile : string -> unit
val startNewline : int -> unit
val startNewFile : int -> string -> int -> unit
val getLocation : info -> int -> int -> string
val pos2info : int * int -> string * int
val error : int * int -> string -> unit
val fmt_error : int * int -> string -> string

(* val flag_coerce : bool ref *)

(* val typeof_var : var -> abstyp  *)

(* val strip_casts : abslval -> abslval *)
(* val to_vine_value : absvalue -> Vine.value *)
(* val to_vine_typ : abstyp -> Vine.typ *)
(* val to_vine_lval :  abslval -> Vine.lvalue *)
(* val to_vine_exp :  absexp -> Vine.exp *)
(* val to_vine_stmt :  absstmt -> Vine.stmt option *)
(* val to_vine : bool -> bool -> absstmt list -> Vine.program *)

class scoping :
object
    method add : string -> Vine.typ -> Vine.var
    method dec : unit -> unit
    method inc : unit -> unit
    method decls : unit -> Vine.var list
    method scopenum : unit -> int 
    method lookup : string -> Vine.var
    method clear : unit -> unit
    (** same as add, except we don't create a new var, we use the
	given var.  *)
    method insert : Vine.var -> unit
  end

val defscope : scoping option ref
val getscope : unit -> scoping


val err : abspos -> string -> 'a
val typeof_var : Vine.var -> Vine.typ

val check_lval : scoping -> string -> Vine.exp option -> Vine.typ
  option -> abspos -> Vine.lvalue

(* val array_idx_type_to_size : Vine.typ -> int64 *)

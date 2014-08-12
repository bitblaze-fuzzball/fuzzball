(** 

    This file contains the type declaration for the VinE IR and some
    basic functions to manipulate it. (Shorter constructors, pretty
    printer, visitor.)
    
    Basically all vine projects will depend on this.
    
  @author Ivan Jager
 *)

open Vine_util
open ExtList

module D = Debug.Make(struct let name="Vine" and default=`Debug end)
open D

(** raised by the parser for parse errors *)
exception ParseError of string
(** raised when a typing error is detected *)
exception TypeError of string
(** raised when something is unimplemented *)
exception Unimplemented of string
(** generic exception for an error in Vine. *)
exception VineError of string


type label = string
(** the type for a string that represents a label name*)


type attr = string
(** an attribute on a type, e.g., const *)

type attributes = attr list

type endian = Little | Big

(** The IR type of a Vine expression *)
type typ =  REG_1    (** a boolean *)
	   | REG_8   (** an 8-bit byte *)
	   | REG_16  (** a 16-bit int *)
	   | REG_32  (** a 32-bit int *)
	   | REG_64  (** a 64-bit int *)
	   | TString 
	   | TMem of typ * endian (** Memory of given index type and endianness*)
	   | TFun of typ option * typ list * bool
	   | Array  of typ * int64 (** Array of element type, size. *)
	   | TAttr of typ * attributes

type var = int * string * typ
(** The type for a variable identifier.
    The int should uniquely identify the variable. The string is simply to make
    it easier for humans to read.
    The type is included here so that it doesn't need to be in the lval.

    Please try to recycle the same var rather than creating multiple copies
    of it, as that is a waste of memory. In other words, if two vars refer to
    the same thing, they should be [==].
 *)

module Var =
struct
  type t = var
  let hash (i,_,_) = i

    (* more conservative version for now *)
  let equal x y =
    x == y ||
      match (x,y) with
	| ((x,_,_), (y,_,_)) when x == y ->
	    pwarn "Var.equal: different variables with same int";
	    true
	| _ -> false

  let equal =
    if true (* debug? *)
    then equal
    else (==) (* faster comparisons which should work in the future *)

  (* is it faster to use the generic compare, or < and = ? *)
  let compare (x,_,_) (y,_,_) = compare x y

end

module VarHash = Hashtbl.Make(Var)
module VarMap = Map.Make(Var)
module VarSet = Set.Make(Var)

(** Different forms of casting *)
type cast_type = CAST_UNSIGNED (** 0-padding widening cast. *)
		 | CAST_SIGNED (** Sign-extending widening cast. *)
		 | CAST_HIGH (** Narrowning cast. Keeps the high bits. *)
		 | CAST_LOW (** Narrowing cast. Keeps the low bits. *)

(** Binary operations implemented in the IR *)
type binop_type = PLUS (** Integer addition. (commutative, associative) *)
		  | MINUS (** Subtract second integer from first. *)
		  | TIMES (** Integer multiplication. (commutative, associative)*)
		  | DIVIDE (** Unsigned integer division. *)
		  | SDIVIDE (** Signed integer division. *)
		  | MOD (** Unsigned modulus. *)
		  | SMOD (** Signed modulus. *)
		  | LSHIFT (** Left shift. *)
		  | RSHIFT (** Right shift, fill with 0. *)
		  | ARSHIFT (** Right shift, sign extend. *)
		  | BITAND (** Bitwise and. (commutative, associative) *)
		  | BITOR (** Bitwise or. (commutative, associative) *)
		  | XOR (** Bitwise xor. (commutative, associative) *)
		  | EQ (** Equals (commutative) (associative on booleans) *)
		  | NEQ (** Not equals (commutative) (associative on booleans) *)
		  | LT (** Unsigned less than *)
		  | LE (** Unsigned less than or equal to *)
		  | SLT (** Signed less than *)
		  | SLE (** Signed less than or equal to *)
                  

(** Unary operations implemented in the IR *)
type unop_type = NEG (** Negate (2's complement) *)
		 | NOT (** Bitwise not *)


(** A value that can be used as a constant *)
type value =  Int of typ * int64
	      | Str of string


(** A declaration declaring the type of a variable *)
type decl =  var

(** The position of a statement in a source file *)
type pos = (string * int)

(** Extra attributes we can add to things.
    There may be a nicer way to implement this. Basically any extra information
    about a statement can be saved by wrapping the statement inside an Attr
    statement.
 *)
type attribute = 
    Pos of pos  (** The position of a statement in the source file *)
    | ACall
    | AReturn

type lvalue = 
    Temp of var (** A global or local variable *)
    | Mem of var * exp * typ (** A memory reference *)

(** An expression in the IR *)
and exp = BinOp of binop_type * exp * exp
	   | UnOp of unop_type * exp
	   | Constant of  value
	   | Lval of lvalue
	   | Name of label (** The address of a label *)
	   | Cast of cast_type * typ * exp (** Cast to a new type. *)
	   | Unknown of string (* FIXME: * register_type *)
	   | Let of lvalue * exp * exp (** Let(lv,e1,e2) binds lv to e1 in
					   the scope of e2 *)
	       (* Note: We allow binding memory in an expression, so we don't
		  need to replace all memory references in the subexpression with
		  ITEs, when calculating the WP. *)
	   | Ite of exp * exp * exp (** Functional if-then-else *)

(** The IR statement type. *)
type stmt = Jmp of exp (** Jump to a label/address *)
	    | CJmp of exp * exp * exp (** Conditional jump.
					  If e1 is true, jumps to e2,
					  otherwise jumps to e3 *)
	    | Move of lvalue * exp (** Copy the value on the right to the
				       lvalue on the left *)
	    | Special of string (** A "special" statement. (does magic) *)
	    | Label of label (** A label we can jump to *)
	    | ExpStmt of exp (** An expression which is to be ignored *)
	    | Comment of string (** A comment to be ignored *)
	    | Block of decl list * stmt list (** A local scope. Any variables
						 declared in the block fall
						 out of scope upon leaving the
						 block. *)
	    | Function of label * typ option * decl list * bool * stmt option
		(** A function: name, return type, formal arguments, true if
		    external, some stmt if this is the definition or None if
		    it is just a declaration *)
	    | Return of exp option (** Return a value from a function *)
	    | Call of lvalue option * exp * exp list 
		(** A call to a function. *)
	    | Attr of stmt *  attribute (** A statment with attributes *)
	    | Assert of exp (** an assertion *)
	    | Halt of exp (** halt execution normally *)

type program = decl list * stmt list


(** Create a new "unused" variable with the given name as a base. *)
let newvar =
  let varcounter = ref 0 in
  let nv s t = 
    let n = !varcounter in
      if n = -1 then failwith "newvar: counter wrapped around";
      (varcounter := n+1;
       (n,s,t))
  in
    nv

(** Create a new unused variable with the same name and type as the given one *)
let renewvar (_,name,t) = newvar name t

(** Create a new, unused label *)
let newlab =
  let labcounter = ref 0 in
  let nv s = 
    let n = !labcounter in
      (labcounter := n+1;
       s^"___"^string_of_int n)
  in
    nv
(** Note that because labels are denoted by strings, the name may
    actually be taken already. *)



(* convenient constructor functions *)
let bool_t = REG_1
let char_t = REG_8
let addr_t = REG_32 (* XXX 64-bit platforms? *)
let exp_true = Constant(Int(bool_t, 1L))
let exp_false = Constant(Int(bool_t, 0L))
let exp_bool c =  Constant(Int(bool_t,if c then 1L else 0L))
let bool_of_const =
  function Constant(Int(bool_t, v)) -> v <> 0L
    | _ -> raise(TypeError "bool_of_const on non boolean constant")

let exp_eq e1 e2 = BinOp(EQ, e1, e2)
let exp_and e1 e2 = BinOp(BITAND, e1, e2)
let exp_or e1 e2 = BinOp(BITOR, e1, e2)
let exp_not e = UnOp(NOT, e)
let exp_implies e1 e2 = exp_or (exp_not e1) e2
let exp_plus e1 e2 = BinOp(PLUS, e1, e2)
let exp_ite cond typ e1 e2 =
  ignore(typ);
  Ite(cond, e1, e2)

let const_of_int64 t i = Constant(Int(t, i))
let const_of_int t i = Constant(Int(t, Int64.of_int i))


let attr_to_string at =
  match at with
      Pos(s, i) -> "#"^(string_of_int i)^" \""^s^"\"\n"
    | ACall -> "// call\n"
    | AReturn -> "// ret \n"
	
(** @return the number of bits a given type can store, unless it is an array *)
let rec bits_of_width t = 
  match t with
    | REG_1 -> 1
    | REG_8 -> 8
    | REG_16 -> 16
    | REG_32 -> 32
    | REG_64 -> 64
    | TAttr(t', _) -> bits_of_width t'
    | _ -> raise (Invalid_argument "unknown width")
(*    | Array(at, vt) -> bits_of_width vt (* FIXME *)
    | TString -> failwith "bits_of_width of string undefined" *)

(** Parse a binop from a string *)
let binop_of_string s =
  (match s with
      "PLUS" ->     PLUS
    | "MINUS" ->    MINUS
    | "TIMES" ->    TIMES
    | "DIVIDE" ->   DIVIDE
    | "MOD" ->      MOD
    | "SMOD" ->     SMOD
    | "LSHIFT" ->   LSHIFT
    | "RSHIFT" ->   RSHIFT
    | "ARSHIFT" ->  ARSHIFT
    | "BITAND" ->   BITAND
    | "BITOR" ->    BITOR
    | "XOR" ->      XOR
    | "EQ" ->       EQ 
    | "NEQ" ->      NEQ
    | "LT" ->       LT
    | "LE" ->       LE
    | "SLT" ->      SLT
    | "SLE" ->      SLE
    | s -> raise(ParseError("\""^s^"\" is  not a known binop"))
  )

(** Parse a unop from a string *)
let unop_of_string s =
  (match s with
      "NEG" -> NEG
    | "NOT" -> NOT
    | "BNOT" -> NOT
    | s -> raise(ParseError("\""^s^"\" is  not a known unnop"))
  )

(** Parse a cast_type from a string *)
let casttype_of_string s =
  (match s with
     "U"
    | "Unsigned"  -> CAST_UNSIGNED
    | "S"
    | "Signed" -> CAST_SIGNED  
    | "H"
    | "High" -> 	CAST_HIGH    
    | "L"
    | "Low" -> 		CAST_LOW     
    | s -> raise(ParseError("\""^s^"\" is  not a known type of cast"))
  )

(** Parse a constant of the given type out of the string.*)
let const_of_stringt t = match t with
    REG_1 | REG_8 | REG_16 | REG_32 | REG_64
      -> (fun s ->
	try Int(t, Int64.of_string s)
	with Failure e -> failwith("parsing '"^s^"' failed: "^e)
	 )
  | _ -> failwith "reading non-scalar constants unimplemented"

(** Parse an IR type from a string *)
let type_of_string = function
    "reg1_t"  -> REG_1 
  | "reg8_t"  -> REG_8
  | "reg16_t" -> REG_16
  | "reg32_t" -> REG_32
  | "reg64_t" -> REG_64
  | "meml32l_t" -> TMem(REG_32, Little)
  | "mem64l_t" -> TMem(REG_64, Little)
  | "REG_1"  -> REG_1 
  | "REG_8"  -> REG_8
  | "REG_16" -> REG_16
  | "REG_32" -> REG_32
  | "REG_64" -> REG_64
  | _ -> raise(VineError("unknown string type"))

(** @return the string representation of a binop *)
let binop_to_string = function
    PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | SDIVIDE -> "/$" 
  | MOD -> "%"
  | SMOD -> "%$"
  | LSHIFT -> "<<"
  | RSHIFT -> ">>"
  | ARSHIFT -> "@>>"
  | BITAND -> "&"
  | BITOR -> "|"
  | XOR -> "^"
  | EQ -> "=="
  | NEQ -> "<>"
  | LT -> "<"
  | LE -> "<="
  | SLT -> "<$"
  | SLE -> "<=$"

(** @return the string representation of a unop *)
let unop_to_string = function
    NEG -> "-"
  | NOT -> "!"

(** @return the string representation of a cast_type *)
let casttype_to_string = function
    CAST_UNSIGNED ->  "U"
  | CAST_SIGNED ->    "S"
  | CAST_HIGH ->      "H"
  | CAST_LOW ->       "L"

let rec tattr_to_string at = 
  match at with
      [] -> ""
    | a::[] -> a
    | a::tail -> a^","^(tattr_to_string tail)



(** Pretty print a variable using the given formatter *)
let rec format_name ft (vid,name,typ) =
  let pp = Format.pp_print_string ft in
    pp name;
    pp "_";
    pp (string_of_int vid)

and format_var ?(print_type=true) ft ((vid,name,typ) as var) =
  let pp = Format.pp_print_string ft in
    format_name ft var;
    if print_type then (
      pp ":";
      format_typ ft typ
    ) else () 

and format_decl ft v = 
  format_var ~print_type:true ft v

    
and format_typ ft t = 
  let pp = Format.pp_print_string ft in 
  let space = Format.pp_print_space ft in
  let open_box = Format.pp_open_box ft in
  let close_box = Format.pp_close_box ft in
    open_box  0;
    (
            match t with
	  REG_1 -> pp "reg1_t" 
	| REG_8  -> pp "reg8_t"
	| REG_16 -> pp "reg16_t"
	| REG_32 -> pp "reg32_t"
	| REG_64 -> pp "reg64_t"
	| TString -> pp "string_t"
	| TMem(REG_32, Little) -> pp "mem32l_t"
	| TMem(REG_64, Little) -> pp "mem64l_t"
	| TMem _ -> failwith "Unsupported memory type"
	| Array(t1,i ) -> 
	    format_typ ft t1;
	    pp "[";
	    pp (Int64.to_string i);
	    pp "]"
	| TFun(rettype, argtypes, is_extern) -> (
	    (* we can't read this back in, but its something for now *)
	    pp "(";
	    (match argtypes with
	       | [] -> ()
	       | x::xs ->
		   format_typ ft x;
		   List.fold_left
		     (fun _ x -> pp ","; space(); format_typ ft x)
		     () xs
	    );
	    pp ")";
	    space();
	    pp "-> ";
	    (match rettype with
		None ->  pp "void"
		| Some(rt') -> 	format_typ ft rt'
	    );
	    if (is_extern) then
	      pp ":extern"
	    else
	      pp ""
	  )
	| TAttr(t', a) ->
	    format_typ ft t';
	    pp "_attr_(";
	    pp (print_separated_list id ", " a);
	    pp ")"
    );
    close_box ()


let format_value ft v = 
  let pp = Format.pp_print_string ft in 
    match v with
      | Int(REG_1, 1L) -> pp "true"
      | Int(REG_1, 0L) -> pp "false"
      | Int(t,x) -> 
	  (* No moding here, since the value should have been modded earlier,
	  and if it wasn't, it's better not to hide it. *)
	  pp (if 0L <= x && x < 10L
	      then Int64.to_string x
	      else Printf.sprintf "0x%Lx" x);
	  pp ":";
	  format_typ ft t
      | Str(x) -> pp "\""; pp x; pp "\""	  



(** Pretty print an expression using the given formater *)
let rec format_exp ft e =
  let pp = Format.pp_print_string ft in
  let space = Format.pp_print_space ft in
  let open_box = Format.pp_open_box ft in
  let close_box = Format.pp_close_box ft in
    (* FIXME: Needs  some way to know when to parenthesize expressions.
       Eg: let x = y in (let x = 2:reg32_t in x) + x
       Eg: let x = y in x + let x = 2:reg32_t in x
       so just specifying a minimal binding strength isn't optimal, although
       it may be good enough.
    *)
  let rec fe prec e =
    (* prec tells us how much parenthization we need. 0 means it doesn't need
       to be parenthesized. Larger numbers means it has higher precedence.
       Maximum prec before paretheses are added are as follows:
       100 LET
       150 ? :
       200 OR XOR AND
       300 EQUAL NEQ
       400 LT SLT SLE LE
       500 LSHIFT RSHIFT ARSHIFT
       600 PLUS MINUS
       700 TIMES DIVIDE SDIVIDE MOD
       800 UMINUS NOT

       (* We intentiorally print parentheses around things with precedence
       200 because those should have different precedences to match what
       C does. *)
 *)
    let lparen bind = if bind < prec then pp "(" in
    let rparen bind = if bind < prec then pp ")" in
    Format.pp_open_box ft 0;
    (match e with
	 Let(l,e1,e2) -> 
	   lparen 100;
	   pp "let ";
	   open_box 0;
	   format_lval ft l;
	   pp " ="; space();
	   fe 0 e1;
	   close_box();
	   space();
	   pp "in";
	   space();
	   fe 0 e2;
	   rparen 100;
       | Ite(cond_e, true_e, false_e) ->
	   lparen 150;
	   open_box 1;
	   fe 151 cond_e;
	   space ();
	   pp "?";
	   space ();
	   fe 151 true_e;
	   space ();
	   pp ":";
	   space ();
	   fe 151 false_e;
	   close_box ();
	   rparen 150;
       | BinOp(b,e1,e2) ->
	   let op_prec = match b with
               BITOR | XOR | BITAND	 -> 200
	     | EQ | NEQ			 -> 300
	     | LT | SLT | SLE | LE	 -> 400
	     | LSHIFT | RSHIFT | ARSHIFT -> 500
	     | PLUS | MINUS		 -> 600
	     | TIMES|DIVIDE|SDIVIDE|MOD|SMOD  -> 700
	   in
	   Format.pp_print_cut ft ();
	   lparen op_prec;
	   open_box 2;
	   (* all binops are left associative *)
	   (* but we want parentheses for things of the same precedence,
	      because some of our precedences are counterintuitive *)
	   fe (if op_prec = 200 then 201 else op_prec) e1;
	   space();
	   pp(binop_to_string b);
	   pp " ";
	   fe (op_prec+1) e2;
	   close_box();
	   rparen op_prec;
	   Format.pp_print_cut ft ()
       | UnOp(u,e) ->
	   lparen 800;
	   pp(unop_to_string u);
	   fe 800 e;
	   rparen 800
       | Constant(v) ->
	   format_value ft v
       | Lval l ->
	   format_lval ft l
       | Name l ->
	   pp "name("; pp l; pp ")"
       | Cast(ct,t,e) ->
	   pp "cast(";
	   fe 0 e;
	   pp (")"^casttype_to_string ct^":");
	   format_typ ft t
       | Unknown u ->
	   pp "unknown \""; pp u; pp "\""
    );
    Format.pp_close_box ft ()
  in
    fe 0 e

(** Pretty print an lvalue using the given formater *)
and format_lval ft l =
  let pp = Format.pp_print_string ft in
    Format.pp_open_box ft 0;
    (match l with
	 Temp var ->
	   format_var ft var
       | Mem(var, idx, wt) ->
	   (* FIXME: How much do we care to preserve this old syntax?
	   pp var;
	   pp "[";
	   format_exp ft idx;
	   pp "]:";
	   pp (type_to_string t)
	   *)
	   format_name ft var;
	   pp "[";
	   format_exp ft idx;
	   pp "]:";
	   format_typ ft wt
    );
    Format.pp_close_box ft ()
;;



(** Pretty print a statement using the given formater *)
let format_stmt ft s =
  let pp = Format.pp_print_string ft in
  let space = Format.pp_print_space ft in
  let open_box = Format.pp_open_box ft in
  let close_box = Format.pp_close_box ft in
(*  let comma () = pp ","; Format.pp_print_cut ft () in *)
  let fe = format_exp ft in
  let rec fs s =
    open_box 0;
    (match s with
	 Jmp e ->
	   pp "jmp(";
	   fe e;
	   pp ");"
       | CJmp(e1,e2,e3) ->
	   pp "cjmp(";
	   fe e1;
	   pp ",";
	   space();
	   fe e2;
	   pp ",";
	   space();
	   fe e3;
	   pp ");"
       | Move(l,e) ->
	   format_lval ft l;
	   pp " =";
	   space();
	   format_exp ft e;
	   pp ";"
       | Special s ->
	   pp "special(\"";
	   pp s;
	   pp "\");"
       | Label l ->
	   pp "label ";
	   pp l;
	   pp ":"
       | ExpStmt e ->
	   format_exp ft e;
	   pp ";"
       | Comment s ->
	     pp "/*";
	     pp s;
	     pp "*/" ;
	     Format.pp_force_newline ft ();
       | Block(dl,sl') ->
	   Format.pp_open_hvbox ft 0;
	   pp "{";
	   Format.pp_open_hvbox ft 4;
	   space();
	   List.iter (fun d -> pp "var "; format_decl ft d; pp ";"; space()) dl;
	   List.iter fs sl';
	   close_box();
	   (*space(); *)
	   Format.pp_print_cut ft ();
	   pp "}";
	   close_box()
       | Attr(s,a) ->
	   pp (attr_to_string a);
	   fs s
       | Function(v,ropt,al,is_ext,bopt) -> (
	   if is_ext then (pp "extern"; space());
	   (
	     match ropt with
		 None -> pp "void"
	       | Some(rt') -> format_typ ft rt'
	   );
	   space();
	   pp v;
	   (match al with
		[] -> pp "()"
	      | x::xs ->
		  pp "(";
		  format_var ft x;
		  List.iter (fun y -> pp ","; space(); format_decl ft y) xs;
		  pp ")"
	   );
	   match bopt with
	       None -> pp ";"
	     | Some(blk) -> 
		 space();
		 fs blk
	 )
       | Call (lv_o, e, al) -> 
	   (match lv_o with
	      | None -> ()
	      | Some lv ->
		  format_lval ft lv; pp " ="; space()
	   );
	   pp "call"; space();
	   format_exp ft e;
	   pp "(";
	   ignore(List.fold_left
		    (fun c a ->
		       format_exp ft a; (if c then pp ",";space());true)
		    false al);
	   pp ");"
      | Return None ->
	  pp "return;"
      | Return(Some e) -> 
	  pp "return";
	  space();
	  format_exp ft e;
	  pp ";"
      | Assert e ->
	  pp "assert(";
	  format_exp ft e;
	  pp ");"
      | Halt e ->
	  pp "halt(";
	  format_exp ft e;
	  pp ");"
    );
    close_box();
    space()
in
    fs s
;;

(** Pretty print a program using the given formater *)
let format_program ft (dl,sl) =
  let pp = Format.pp_print_string ft in
  let space = Format.pp_print_space ft in
    Format.pp_open_hvbox ft 0;
    List.iter (fun d -> pp "var "; format_decl ft d; pp ";"; space()) dl;
    Format.pp_close_box ft ();
    Format.pp_force_newline ft ();
    Format.pp_open_hvbox ft 0;
    List.iter (format_stmt ft) sl;
    Format.pp_close_box ft ();
    Format.pp_print_newline ft ()



(** makes a pp_foo function out of a format_foo one *)
let format2pp format_fun pr x =
  let ft = Format.make_formatter (fun s p n -> pr(String.sub s p n)) ignore in
  let () = format_fun ft x in
    Format.pp_print_flush ft ()


(** makes a foo_to_string function out of a format_foo one *)
let format2string format_fun x =
  let buf = Buffer.create 1000 in
  let ft = Format.formatter_of_buffer buf in
  let (out,flush,newline,spaces) =
    Format.pp_get_all_formatter_output_functions ft ()
  in
  let () = Format.pp_set_all_formatter_output_functions ft
    ~out:out ~flush:flush ~newline:newline ~spaces:(fun _ -> spaces 1)
  in
  let () = format_fun ft x in
  let () = Format.pp_print_flush ft () in
    Buffer.contents buf

(** "Pretty print" a type
    @param pr the printer to use (e.g. print_string)
    @param t the type
*)
let pp_typ = format2pp format_typ

(** "Pretty print" a value
    @param pr the printer to use (e.g. print_string)
    @param v the value
*)
let pp_value = format2pp format_value

(** "Pretty print" a variable. 
    @param pr the printer to use (eg. print_string)
    @param v the variable
*)
let pp_var = format2pp format_var

(** "Pretty print" an expression. 
    @param pr the printer to use (eg. print_string)
    @param e the expression to print
*)
let pp_exp = format2pp format_exp

(** "Pretty print" an lvalue. 
    @param pr the printer to use (eg. print_string)
    @param v the lvalue
*)
let pp_lval = format2pp format_lval

(** "Pretty print" a stmt. 
    @param pr the printer to use (eg. print_string)
    @param s the stmt
*)
let pp_stmt = format2pp format_stmt

(** "Pretty print" a program. 
    @param pr the printer to use (eg. print_string)
    @param p the program
*)
let pp_program = format2pp format_program


(** convert an expression to a string *)
let exp_to_string = format2string format_exp

let lval_to_string = format2string format_lval

let stmt_to_string = format2string format_stmt

let var_to_string = format2string format_var

let decl_to_string = var_to_string

let type_to_string = format2string format_typ

let val_to_string = format2string format_value


(** write a statement directly to a channel.*)
let rec stmt_to_channel oc =
  pp_stmt (output_string oc)


(* stolen from Cil *)
(** Different visiting actions. 'a will be instantiated with [exp], [stmt],
    etc. *)
type 'a visit_action = 

    SkipChildren                        (** Do not visit the children. Return 
                                            the node as it is. *)
  | DoChildren                          (** Continue with the children of this 
                                            node. Rebuild the node on return 
                                            if any of the children changes 
                                            (use == test) *)
  | ChangeTo of 'a                      (** Replace the expression with the 
                                            given one *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (** First consider that the entire 
                                           exp is replaced by the first 
                                           parameter. Then continue with 
                                           the children. On return rebuild 
                                           the node if any of the children 
                                           has changed and then apply the 
                                           function on the node *)


class type vine_visitor = object
  (** Called when visiting an expression *)
  method visit_exp: exp -> exp visit_action

  (** Called when visiting a declaration *)
  method visit_decl: decl -> decl visit_action

  (** Called when visiting assigned lvalue *)
  method visit_alvalue: lvalue -> lvalue visit_action

  (** Called when visiting a referenced lvalue *)
  method visit_rlvalue: lvalue -> lvalue visit_action

  (* Called on the binding when  recursinig into a Let. visit_exp will not
  only visit the subexpression this binding applies to. *)    
  method visit_binding: lvalue * exp -> (lvalue * exp) visit_action 

  (** Called when visiting a statement.
      FIXME: It may be better if this was [stmt -> stmt list visit_action]
      so that it could add or remove statements too. --aij
 *)
  method visit_stmt: stmt -> stmt visit_action

end

(** A visitor that visits everything, but changes and does nothing. *)
class nop_vine_visitor : vine_visitor = object
  method visit_exp _ = DoChildren
  method visit_alvalue _ = DoChildren
  method visit_rlvalue _ = DoChildren
  method visit_binding _ = DoChildren
  method visit_decl _ = DoChildren
  method visit_stmt _ = DoChildren
end

(* Whether the visitor will return an existing value when it can.
   The idea here is that it might be worth doing a few comparisons to
   put less strain on the GC. It seemed to make Vine_opt.simplify_faster
   marginally slower initially, but after making constant_fold_more return
   the original expression when it didn't change anything, it is almost 4
   times faster.

   This must be defined as a constant in this module so that the 
   conditionals can be compiled out. *)
let vis_avoid_alloc = true

(** Apply a visitor to an expression *)
let rec exp_accept visitor expression =
  let rec vis exp =
    match visitor#visit_exp exp with
	SkipChildren -> exp
      | ChangeTo e -> e
      | DoChildren -> vischil exp
      | ChangeDoChildrenPost(e,f) -> f (vischil e)
  and vischil exp =
    (* visit children *)
    let exp' = 
      match exp with
	BinOp(bot, e1, e2) ->
	  let e1' = vis e1 in
	  let e2' = vis e2 in
	    if vis_avoid_alloc && e1' == e1 && e2' == e2  then exp
	    else BinOp(bot, e1', e2')
      | UnOp(op, e) ->
	  let e' = vis e in
	    if vis_avoid_alloc && e' == e then exp
	    else UnOp(op, e')
      | Cast(c,w, e) ->
	  let e' = vis e in
	    if vis_avoid_alloc && e' == e then exp
	    else Cast(c, w, e')
      | Let(v, e1, e2) ->
	  let (v', e1') = binding_accept visitor (v,e1) in
	  let e2' = vis e2 in
	    if vis_avoid_alloc && v' == v && e1' == e1 && e2' == e2 then exp
	    else Let(v', e1', e2')
      | Ite(ce, te, fe) ->
	  let ce' = vis ce in
	  let te' = vis te in
	  let fe' = vis fe in
	    if vis_avoid_alloc && ce' == ce && te' == te && fe' == fe then exp
	    else Ite(ce', te', fe')
      | Lval l ->
	  let l' = rlvalue_accept visitor l in
	    if vis_avoid_alloc && l' == l then exp
	    else Lval l'
      | Constant _
      | Name _
      | Unknown _
	->
	  exp
    in
    (* return original exp object if same as new one *)
    (* disabled: structural comparison here makes visiting an
       expression quadratic in the number of subexpressions *)
    (*     if (exp = exp') then *)
    (*       exp *)
    (*     else *)
    exp'
  in
  vis expression

and _lvalue_accept visitor visitorf lval =
  let vischil l =
    match l with
	Mem(n,e,t) ->
	  let e' = exp_accept visitor e in
	    if vis_avoid_alloc && e' == e then l
	    else Mem(n,e',t)
      | Temp _ -> l
  in
    match visitorf lval with
	SkipChildren -> lval
      | ChangeTo e -> e
      | DoChildren -> vischil lval
      | ChangeDoChildrenPost(e,f) -> f (vischil e)

(** Apply a visitor to an assigned lvalue *)
and alvalue_accept visitor =
  _lvalue_accept visitor (fun x -> visitor#visit_alvalue x)

(** Apply a visitor to a referenced lvalue *)
and rlvalue_accept visitor =
  _lvalue_accept visitor (fun x -> visitor#visit_rlvalue x)

(** Apply a visitor to a let binding *)
and binding_accept visitor ((l,r) as b) =
  match visitor#visit_binding b with
      SkipChildren -> b
    | ChangeTo b' -> b'
    | DoChildren ->
	let l' = alvalue_accept visitor l in
	let r' = exp_accept visitor r in
	  (l', r')
    | ChangeDoChildrenPost((l,r), f) ->
	let l' = alvalue_accept visitor l in
	let r' = exp_accept visitor r in
	  f (l', r')

(** Apply a visitor to a declaration *)
let decl_accept visitor d =
  match visitor#visit_decl d with
      SkipChildren -> d
    | ChangeTo d' -> d'
    | DoChildren -> d
    | ChangeDoChildrenPost(d',f)  -> f d'

(** Apply a visitor to a statement *)
let rec stmt_accept visitor s =
  let rec vis s = 
    match visitor#visit_stmt s with
	SkipChildren -> s
      | ChangeTo s' -> s'
      | DoChildren -> vischil s
      | ChangeDoChildrenPost(s', f) -> f (vischil s') 
  and vischil s =
    match s with
      Jmp(l) -> Jmp(exp_accept visitor l)
    | CJmp(e,l1,l2) ->
	let e' = exp_accept visitor e in
	let l1' = exp_accept visitor l1 in
	let l2' = exp_accept visitor l2 in
	  CJmp(e', l1', l2')

    | Move(lv,e) ->
	let lv' = alvalue_accept visitor lv in
	let e' = exp_accept visitor e in
	  Move(lv', e')
    | ExpStmt(e) -> ExpStmt(exp_accept visitor e)
    | Block(dl,sl) -> 
	let dl' = List.map(fun x -> decl_accept visitor x) dl in
	let sl' = List.map vis sl in
	Block( dl', sl')
    | Attr(s',a) -> Attr(vis s', a)
    | Function(n,r,d,e,Some(s')) ->
	let d' = List.map (fun x -> decl_accept visitor x) d in
	Function(n,r,d',e,Some(vis s'))
    | Function(n,r,d,e,None) ->
	let d' = List.map (fun x -> decl_accept visitor x) d in
	  Function(n,r,d',e,None)
    | Label _ 
    | Comment _
    | Special _ -> s
    | Call(Some(lv), e, el) -> 
	let e' = exp_accept visitor e in 
	let el' = List.map (fun x -> exp_accept visitor x) el in 
	Call(Some(alvalue_accept visitor lv),e', el')
    | Call(None, e, el) ->
	let e' = exp_accept visitor e in 
	let el' = List.map (fun x -> exp_accept visitor x) el in 
	  Call(None, e', el')
    | Return(Some(e)) -> Return(Some(exp_accept visitor e))
    | Return(None) -> s
    | Assert(e) -> Assert(exp_accept visitor e)
    | Halt(e) -> Halt(exp_accept visitor e)
  in
    vis s

(** Apply a visitor to a list of statements *)
let stmts_accept vis stmts =
  List.map (stmt_accept vis) stmts


(** Apply a visitor to a program *)
let prog_accept visitor (dl,sl) =
  let dl' = List.map (decl_accept visitor) dl in
  let sl' = stmts_accept visitor sl in 
    (dl',sl')


class freerefs_visitor =
object
  inherit nop_vine_visitor
  val mutable ctx = ([]:lvalue list) (* FIXME: change to VH.t *)
  val mutable found = ([]:lvalue list)
    
  method get_found = found
    
  method visit_exp e =
    match e with
	Let(v, e1, e2) -> 
	  ChangeDoChildrenPost(e, (fun x -> (ctx <- List.tl ctx; x)))
      | _ -> DoChildren
	  
  method visit_binding ((l,r) as b) =
    ChangeDoChildrenPost(b, fun x -> ctx <- l::ctx; x)
      
  method visit_rlvalue r =
    let () = if List.for_all ((<>) r) ctx then (found <- r::found ) in
      DoChildren
end

(** @return a list of external temps and memory addresses referenced by the
expression. *)
let freerefs_exp e =
  let freevis = new freerefs_visitor in
  let _ = exp_accept freevis e in
    freevis#get_found

(** @return a list of external temps and memory addresses referenced by the
statement. *)
let freerefs_stmt s =
  let freevis = new freerefs_visitor in
  let _ = stmt_accept freevis s in
    freevis#get_found

let onlymems freevars =
  List.filter (function Mem _ -> true | _ -> false) freevars

(** @return the memory references made by an expression *)
let freemems_exp e =
  onlymems (freerefs_exp e)

(** @return a list of Temps referenced by the expression *)
let freetemps_exp e =
  List.filter (function Temp _ -> true | _ -> false) (freerefs_exp e)

(** Note: Do not use this to "fix" errors. 
    Do use it when you want to typecheck a subexpression, but be aware of the
    implications.
   @return a list of declarations that are required in
   the context in which this expression can be used. *)
let get_req_ctx e =
  let freevis =
    object(self)
      inherit nop_vine_visitor
      val mutable ctx = ([]:lvalue list)
      val mutable found = ([]:var list)

      method get_found = found
      method add_dec (var as dec) = 
	if List.for_all ((<>) (Temp var)) ctx && List.for_all ((<>) dec) found
	then (found <- dec::found )
	else ()

      method visit_exp e =
	match e with
	    Let(v, e1, e2) -> 
	      (match v with
		   Mem(n,_,t) -> self#add_dec n
		 | Temp _ -> ());
	      ChangeDoChildrenPost(e, (fun x -> (ctx <- List.tl ctx; x)))
	  | _ -> DoChildren
	      
      method visit_binding ((l,r) as b) =
	ChangeDoChildrenPost(b, fun x -> ctx <- l::ctx; x)
	
      method visit_rlvalue r =
	let () = self#add_dec (match r with Temp n|Mem(n,_,_) -> n)
	in
	  DoChildren
    end
  in
  let _ = exp_accept freevis e in
    freevis#get_found



(** Removes Attrs from around the statement.
    @return the stmt with no toplevel attrs, and a function to reaply the attrs
*)
let strip_attrs s =
  let rec strip s f =
    match s with
	Attr(s',a) ->
	  strip s' (fun x -> f(Attr(x, a)))
      | _ -> (s,f)
  in
    strip s (fun x->x)

      (** remove all Attr from a statement *)
let rec remove_stmt_attrs s = 
    match s with
      | Attr(s', a) -> remove_stmt_attrs s'
      | Block(dl, sl) -> 
	  let sl' = List.fold_left (fun acc s' ->
	    (remove_stmt_attrs s') :: acc) [] sl in 
	    Block(dl, (List.rev sl'))
      | Function(v, topt, dl, b, Some(s')) ->
	  let s'' = remove_stmt_attrs s' in 
	    Function(v,topt, dl, b, Some(s''))
      | _ -> s

(** remove minimal sub-stmt that contains unknown exp from a given stmt
    return a new stmt and a list of sub-stmts removed *)
let remove_unknowns s =
  let removed = ref [] in
  let found = ref false in
  let unknown_vis =
    object
      inherit nop_vine_visitor
      method visit_exp e =
	match e with
	  Unknown _ -> found := true; SkipChildren
	| _ -> DoChildren
      method visit_stmt s =
	ChangeDoChildrenPost 
	  (s, fun s' -> 
	    if (!found) then 
	      (found := false;
	       removed := s :: !removed;
	       pp_stmt prerr_string s;
	       Comment ("stmt containing Unknown is removed"))
	    else s')
    end
  in
  let res = (stmt_accept unknown_vis s, !removed) in
    if !removed <> []
    then dprintf "%d unknowns found and removed.\n" (List.length !removed);
    res



	  

(** takes in a 64-bit number and returns what the C++ vine code would
    have generated for a label (irtoir.cpp)
    @param addr a 64-bit number
    @return a label string
 *)
let addr_to_label addr = 
  Printf.sprintf "pc_0x%Lx" addr

(**
   @param l is a label that is formatted like a pc label (irtoir.cpp)
   @return a 64-bit number (an address) 
*)
let label_to_addr (l:label) = 
  try
    Scanf.sscanf l "pc_0x%Lx" (fun x -> x)
  with
      _ -> 
	raise (VineError "label_to_addr given non address-like label")


(** Replace the call jump and the return jump with an attribute wrapper
    Assuming that the input stmt list contains no  Attr 
    @param sl statement list
    @param callf a function which replaces a call with something else
    For example, using  fun s -> Attr(s1, Call) is one possibility
    @param retf similar to callf, but for returns.
    @return statement list where call's and return specials are
    replaced by attributres.
*)
let replace_special_jumps  callf retf sl =
  let rec replace_jumps res stmts =
    match stmts with
	Block(dl, sl')::ys -> 
	  let b'= Block(dl,replace_jumps [] sl') in 
	   replace_jumps (b'::res) ys 
      | s1 :: s2 :: tail ->
	(match (s1,s2) with
	  (Jmp _, Special "call") ->
	    replace_jumps ( (callf s1) :: res) tail
	| (Jmp _, Special "ret") ->
	    replace_jumps ( (retf s1) :: res) tail
	| _ ->
	    replace_jumps (s1::res) (s2::tail))
    | [s] ->
	replace_jumps (s::res) []
    | [] -> List.rev res
  in
  replace_jumps [] sl


(** 
    return the last static statement given a list of statements 
*)
let get_last_static_stmt = function 
    x::ys as lst ->  (
      let rec glss stmts current = 
	match stmts with
	    Attr(s,_)::ss -> glss (s::ss) current
	  | Block(_,sl)::ss -> glss (List.append sl ss)  current
	  | s::ss -> glss ss (Some(s))
	  | [] -> current
      in
	glss lst None
    ) 
  |  [] -> None
	   
	

let is_integer_type = function
    REG_1 | REG_8 | REG_16 | REG_32 | REG_64 -> true
  | _ -> false
	

let rec unwind_type t = 
  match t with
      TAttr(t',_) ->  unwind_type t'
    | _ -> t


let remove_untypeable sl = 
  let vis = object(self)
    inherit nop_vine_visitor
      
    val mutable has_unknown = false

    method get_unknown = has_unknown

    method reset_unknown = has_unknown <- false
    method visit_exp  e = 
      match e with 
	  Unknown _ -> 
	    has_unknown <- true; DoChildren
	| _ -> DoChildren
  end
  in
    List.fold_left (fun acc s ->
		      match s with
			  Special _ -> 
			    dprintf "removing untypeable statement %s"
			      (stmt_to_string s); 
			    acc
			| _ -> 
			    ignore( stmt_accept vis s);
			    if vis#get_unknown then (
			      dprintf "removing untypeable statement %s"
				(stmt_to_string s); 
			      acc 
			    ) else 
			      s::acc
		   ) [] sl

	 
let has_warned = ref false;;

let array_idx_type_to_size t =
  match (unwind_type t) with
      REG_1 -> 1L
    | REG_8 -> Int64.shift_left 1L 8 
    | REG_16 -> Int64.shift_left 1L 16
    | REG_32 -> Int64.shift_left 1L 32
    | REG_64 -> 
	if not !has_warned then (
	  pwarn "64-bit array index size is 2^{63}-1, not 2^{64}";
	  has_warned := true
	);
	Int64.max_int
    | _ -> raise (VineError "Nonsensical type for array index")


let idx_size_to_type (i:int64) : typ = 
  let r8 = Int64.shift_left 1L 8 in 
  let r16 = Int64.shift_left 1L 16 in 
  let r32 = Int64.shift_left 1L 32 in 
  match i with
      t when t  = 1L -> REG_1
    | t when t < r8 -> REG_8
    | t when t < r16 -> REG_16
    | t when t < r32 -> REG_32
    | _ -> REG_64


let typeof_var (_,_,t) = t

let is_not_memory (_,_,t) =
  match unwind_type t  with
      TMem _ -> false
    | Array _ -> false
    | _ -> true

let is_arithmetic_op op = 
  match op with
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
    | XOR -> true
    | _ -> false


let exp_size e =
  let s = ref 0 in
  let vis = object
    inherit nop_vine_visitor
    method visit_exp _ =
      inc s;
      DoChildren
  end in
    ignore(exp_accept vis e : exp);
    !s

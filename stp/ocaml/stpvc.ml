(** Higher level interface to Libstp.
   
   Note that for garbage collection to work properly, all expressions must
   contain a reference to the vc they were created in, because when the
   vc is destroyed, the expressions are no longer valid.
   This will be fixed in the next version. (Until recently, there wasn't any
   way to free resources)
  
   The easiest way to do that of course would be to have exp include a
   reference to the vc, which also has the advantage that functions that
   take an exp wouldn't need to get the vc explicitly.
  
   The shift by expression problem could be solved by also keeping track
   of the type in the expression. (or having stp provide a way to get it)
  
   Another idea would be to have separate ocaml types for boolean,
   bitvector, and array expressions. This would let the ocaml typechecker
   catch errors at compile time, rather than defering those for STP to catch
   at runtime. Maybe something like:
   type bool_exp
   type bv_exp
   type ('a,'b) array_exp
   It would be nice if we could get the width of the bitvectors in there
   somehow.

   Freeing resources associated with a VC using vc_Destroy currently 
   throws segmentation faults. This is a known STP issue. 
   Thus, the proper handling is to use a single VC for all queries rather 
   than creating and then destroying one for each query.
   When using a single VC, call [Libstp.vc_push : vc -> unit] before 
   the query and [Libstp.vc_pop : vc -> unit] after getting the counterxample. 
   This frees most resources associated with the query, 
   without destroying the VC.
  
   @author Ivan Jager
 *)


(* These commmands are reasonably useful for debugging

let vc = Stpvc.create_validity_checker()
let f = Stpvc.e_false vc
let t = Stpvc.e_true vc
let bool = Stpvc.bool_t vc
let bv32 = Stpvc.bitvector_t vc 32
let b = Stpvc.e_var vc "b" bool
let x = Stpvc.e_var vc "x" bv32
let y = Stpvc.e_var vc "y" bv32
let bv = Stpvc.e_bv_of_int64 vc 32 0L
let e = (Stpvc.e_not vc (Stpvc.e_bvbitextract vc bv 0))
;;
Stpvc.query vc e;;
*)

open Libstp


type vc = Libstp.vc
type exp = (*vc *) Libstp.expr 
type typ = Libstp.typ 
type kind = Libstp.exprkind_t
type type_t = Libstp.type_t
type whole_ce = Libstp.wholeCounterExample

(* some helper functions *)
let finalize f v = Gc.finalise f v; v

let expwarn = ref true
let free_exp = Libstp.vc_DeleteExpr

let wrap1 f arg1 = finalize free_exp (f arg1)
let wrap2 f arg1 arg2 = finalize free_exp (f arg1 arg2)
let wrap3 f arg1 arg2 arg3 = finalize free_exp (f arg1 arg2 arg3)
let wrap4 f arg1 arg2 arg3 arg4 = finalize free_exp (f arg1 arg2 arg3 arg4)
let wrap3bv f arg1 arg2 arg3 =
  if arg2 > 0
  then finalize free_exp (f arg1 arg2 arg3)
  else failwith "bitvector width must be greater than 0"
let wrap4bv f arg1 arg2 arg3 arg4 =
  if arg2 > 0
  then finalize free_exp (f arg1 arg2 arg3 arg4)
  else failwith "bitvector width must be greater than 0"

(* end helper functions *)

let vc_creation_count = ref 0

let create_validity_checker () =
(* Freeing resources associated with a VC using vc_Destroy currently 
  throws segmentation faults. This is a known STP issue. 
  Thus, the proper handling is to use the same VC for many queries rather 
  than creating one for each query and then destroying it.
  When using the same VC, call vc_push before the query and vc_pop after 
  getting the counterxample. 
  This frees most resources associated with the query, 
  without destroying the VC.
*)
  (* finalize Libstp.vc_Destroy (vc_createValidityChecker()) *)
  if !vc_creation_count > 0 then
    failwith "Creating multiple STP VCs is not currently supported";
  incr vc_creation_count;
  let vc = vc_createValidityChecker() in
    (* This flag (and the whole setInterfaceFlags interface) was added
       in STP r940. With the new default value 1 of the flag, the STP
       c_interface code will maintin a vector of all of the types and
       integer constant expressions created, in order to know to delete
       them when vc_Destroy is called. But since we never call vc_Destroy
       (as described above), the vector would grow without bound. So we
       want the old behavior of not keeping the vector, which is now
       obtained by setting the flag to 0. If your version of STP doesn't
       have the vc_setInterfaceFlags function, you probably don't need
       it, so you can comment this out. *)
    vc_setInterfaceFlags vc EXPRDELETE 0;
    vc

(* Functions for constructing types. Because the current STP interface
   doesn't give us a proper way to free these, we intern them
   instead. This will at least avoid a leak if you build the same types
   over and over again. Though I've heard that vc_DeleteExpr actually
   works on types. *)

let type_intern_hash = Hashtbl.create 101

(* This function is one of the places where having multiple VCs at
   once does not work. When I wrote it, I thought we could use the vc
   value to disambiguate, but in fact OCaml can't do "=" comparison on vc
   objects, so if you ever have more than once the Hashtbl lookup will
   fail with the somewhat obscure message Invalid_argument("equal:
   abstract value"). But I think it's not worth fixing here because there
   are other things on the STP side that are also obstacles to having
   multiple VCs. *)
let intern_type vc str thunk = 
  try Hashtbl.find type_intern_hash (vc, str) with
      Not_found ->
	let t = (thunk ()) in
	  Hashtbl.add type_intern_hash (vc, str) t;
	  t

let bool_t vc =
  intern_type vc "bool" (fun () -> vc_boolType vc)
let array_t vc index_t data_t =
  intern_type vc ((typeString data_t) ^ "[" ^ (typeString index_t) ^ "]")
    (fun () -> vc_arrayType vc index_t data_t)
let bitvector_t vc width =
  intern_type vc ("BV(" ^ (string_of_int width) ^ ")")
    (fun () -> vc_bvType vc width)

let e_simplify = wrap2 vc_simplify

(* functions for constructing expressions *)
let e_var = wrap3 vc_varExpr
let e_eq = wrap3 vc_eqExpr
let e_true = wrap1 vc_trueExpr
let e_false = wrap1 vc_falseExpr
let e_not = wrap2 vc_notExpr
let e_and = wrap3 vc_andExpr
let e_or = wrap3 vc_orExpr
let e_implies = wrap3 vc_impliesExpr
let e_iff = wrap3 vc_iffExpr
let e_ite = wrap4 vc_iteExpr

let e_boolbv = wrap2 vc_boolToBVExpr
let e_read = wrap3 vc_readExpr
let e_write = wrap4 vc_writeExpr

let e_bv_of_string = wrap2 vc_bvConstExprFromStr
let e_bv_of_int = wrap3bv vc_bvConstExprFromInt
let e_bv_of_int32 = wrap2 vc_bv32ConstExprFromInt
let e_bv_of_int64 = wrap3bv vc_bvConstExprFromLL

let e_bvconcat = wrap3 vc_bvConcatExpr
let e_bvplus = wrap4 vc_bvPlusExpr
let e_bvminus = wrap4 vc_bvMinusExpr
let e_bvmult = wrap4 vc_bvMultExpr
let e_bvdiv = wrap4 vc_bvDivExpr
let e_bvmod = wrap4 vc_bvModExpr
let e_sbvdiv = wrap4 vc_sbvDivExpr
let e_sbvmod = wrap4 vc_sbvModExpr
let e_bvand = wrap3 vc_bvAndExpr
let e_bvor = wrap3 vc_bvOrExpr
let e_bvxor = wrap3 vc_bvXorExpr
let e_bvneg = wrap2 vc_bvUMinusExpr
let e_bvnot = wrap2 vc_bvNotExpr

let e_bvextract = wrap4 vc_bvExtract

(* vc_bvBoolExtract seems to be implemented backwards and unlikely to get fixed
let e_bvbitextract = wrap3 vc_bvBoolExtract *)
let e_bvbitextract vc child bit =
  e_eq vc (e_bvextract vc child bit bit) (e_bv_of_int vc 1 1)

let e_bvsextend vc w e = wrap3 vc_bvSignExtend vc e w
(* some shift functions that are a bit more consistent with the other 
 * STP functions *)
let e_bvconstshiftleft vc w e sh_amt =
  if sh_amt = 0
  then e_bvextract vc e (w-1) 0
  else e_bvextract vc (e_bvconcat vc e (e_bv_of_int vc sh_amt 0)) (w-1) 0
let e_bvconstshiftright vc w e sh_amt =
  let shifted = (e_bvextract vc e (w-1) sh_amt) in
  if sh_amt = 0
  then shifted 
  else e_bvconcat vc (e_bv_of_int vc sh_amt 0) shifted
let e_bvconstshiftright_arith vc w e sh_amt = 
  e_bvsextend vc w (e_bvextract vc e (w-1) sh_amt)

(* make a variable shift out of ITE and constant shifts *)
(* w2 is the width of the expression to shift by. Hopefull we can get rid of it
 * soonish. *)
let var_sh csh vc w e w2 sh =
  let make_one n other =
    e_ite vc (e_eq vc sh (e_bv_of_int vc w2 n)) (csh vc w e n) other
  in 
  let rec make_them n last =
    if n = -1 then last
    else make_them (n-1) (make_one n last)
  in
    e_simplify vc (make_them (w-1) (e_bv_of_int vc w 0))

let e_bvshiftleft = var_sh e_bvconstshiftleft
let e_bvshiftright = var_sh e_bvconstshiftright
let e_bvshiftright_arith = var_sh e_bvconstshiftright_arith

let e_bvlt = wrap3 vc_bvLtExpr
let e_bvle = wrap3 vc_bvLeExpr
let e_bvgt = wrap3 vc_bvGtExpr
let e_bvge = wrap3 vc_bvGeExpr
let e_bvslt = wrap3 vc_sbvLtExpr
let e_bvsle = wrap3 vc_sbvLeExpr
let e_bvsgt = wrap3 vc_sbvGtExpr
let e_bvsge = wrap3 vc_sbvGeExpr


let do_assert = vc_assertFormula

let query vc exp =
  match vc_query vc exp with
      0 -> false (* invalid *)
    | 1 -> true (* valid *)
    | 2 -> failwith "vc_query failed"
    | _ -> failwith "vc_query returned unexpected result"

let get_counterexample = wrap2 vc_getCounterExample
let int_of_e = getBVInt
let int64_of_e = getBVUnsignedLongLong

let to_string = exprString
let type_to_string = typeString


(* register the error handler, so errors raise exceptions, rather than
 * exiting the program from C code. *)
let () = libstp_regerrorhandler()



(* OOP interface *)
(* The only advantage of this is that if you don't open Stpvc, you can
 * write "vc#func" rather than "Stpvc.func vc". A nicer approach might be to
 * write a functor, that will return a structure with functions already bound
 * to a vc, so you could open it and just type "func" 
class validity_checker =
object
  val vc = create_validity_checker ()
  method bool_t = bool_t vc
  method array_t = array_t vc
  method bitvector_t = bitvector_t vc

  method exp_var = exp_var vc
  method exp_eq = exp_eq vc
end
*)

let get_kind = getExprKind

let get_degree = getDegree

let get_child = getChild

let get_bvlength = getBVLength

let get_type = getType

let get_vwidth = getVWidth

let get_iwidth = getIWidth

let get_name = exprName

let get_id = getExprID

  
(**
   [get_true_counterexample vc] returns a list of ( symbol, expression )
   pairs as appeared in the command-line version of STP.
*)
let get_true_counterexample vc =
  let key = vc_getTrueCounterExampleFst vc in
  let ce = vc_getTrueCounterExampleSnd vc in
  assert(Array.length key = Array.length ce);
  let (pairs,n) =
    Array.fold_right
      (fun k (p,i) ->
	 ((finalize free_exp k, finalize free_exp ce.(i)) :: p,  i-1)
      )
      key ([],Array.length key - 1)
  in
    assert(n = -1);
    pairs


let free_whole_ce = Libstp.vc_deleteWholeCounterExample

let get_whole_counterexample vc =
  finalize free_whole_ce (vc_getWholeCounterExample vc)

let get_term_from_counterexample vc e ce =
  wrap3 vc_getTermFromCounterExample vc e ce

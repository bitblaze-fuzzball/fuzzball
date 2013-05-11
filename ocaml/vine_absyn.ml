(**
  abstract syntax for vine parser. This syntax is translated into the
  IR. See .mli file for comments.
   @author david brumley
*)

module H = Hashtbl;;
open Vine_util
open Vine


module List=ExtList.List;;

module D = Debug.Make(struct let name = "Vine_absyn" and default=`Debug end)
open D


(** whether or not to warn when shadowing a local. Enabling this
   slows things down. *)
let shadow_warn = ref false;;

(** Whether or not to strip the trailing _number from variable names *)
let strip_nums = ref false

let stripnum1 = Str.replace_first (Str.regexp "_[0-9]+$") ""
let stripnum x =
  if !strip_nums then
    stripnum1 x
  else
    x

type abspos = int * int


(* and abspos = int * int *)

(* type endian = Vine.endian  *)

(* type program = absstmt list *)

(* and var = int * string * abstyp *)

(* and absvalue =  *)
(*     Int of abstyp * int64 *)
(*   | Str of string *)

(* and abstyp =  *)
(*     REG_1 *)
(*   | REG_8 *)
(*   | REG_16 *)
(*   | REG_32 *)
(*   | REG_64 *)
(*   | TString *)
(*   | TMem of abstyp * endian  *)
(*   | Array of abstyp * int64 *)
(*   | TAttr of abstyp * Vine.attributes *)
(*   | TVoid *)

(* and abspos = int * int *)

(* and abslval = ATemp of var *  abspos *)
(* 	      | AMem of var * absexp * abstyp * abspos *)


(* and absexp = ABinOp of V.binop_type * absexp * absexp * abspos *)
(* 	     | AUnOp of V.unop_type * absexp * abspos *)
(* 	     | AConstant of absvalue * abspos *)
(* 	     | ALval of abslval * abspos *)
(* 	     | AName of V.label  * abspos *)
(* 	     | ACast of V.cast_type * abstyp * absexp  *abspos *)
(* 	     | AUnknown of string * abspos *)
(* 	     | ALet of abslval * absexp * absexp *abspos *)
(* 	     | ACall of absexp * absexp list  * abspos *)
		 
(* and absstmt = AJmp of absexp  * abspos *)
(* 	      | ACJmp of absexp * absexp * absexp  * abspos *)
(* 	      | AMove of abslval * absexp  * abspos *)
(* 	      | ASpecial of string  * abspos *)
(* 	      | ALabel of V.label  * abspos *)
(* 	      | AExpStmt of absexp  * abspos *)
(* 	      | AComment of string  * abspos *)
(* 	      | ABlock of absstmt list * abspos *)
(* 	      | AReturn of absexp option * abspos *)
(* 	      | AVarDecl of var  * abspos *)
(* 	      | AFunDecl of V.label * abstyp  * var list  *)
(* 		  * bool * abspos *)
(* 	      | AFunction of V.label * abstyp  * var list  *)
(* 		  * absstmt * abspos *)
(* 	      | ANop of abspos *)


(* the following code is derived from CMU course 15-411 *)
(* Original authors: Benjamin Vernot, Peter Lee, Roland Flury *) 
(* This code manages converting from an vine_absyn.pos = int * int *)
(* to line numbers and file names in the preprocessed (cpp) file *)

let lineNum = ref 1
let linePos = ref [1]

(* an unknown abspos position *)
let unknown_pos = (-1,-1);;

let track_line_numbers = ref false;;

let set_line_tracking b = 
  track_line_numbers := b


type linePosT =
  | Pos of int
  | PosNewFile of int * int * string

type info =
    { mutable  linenum: int      ;
      mutable  linepos: linePosT list ;
      mutable  fileName: string  ;
      mutable  errors: bool      ; 
      mutable is_reversed : bool;
}
      
let current : info = 
  { linenum  = 1   ;
    linepos  = [Pos(1)] ;
    fileName = ""  ;
    errors = false ; 
    is_reversed = true;
}

let extractFileName s =
  let su = String.sub s 1 (String.length s -1) in
  let pos = String.index su '"' in
  String.sub su 0 pos

    
(** Initialize current-struct mainly with file-name *)
let startFile fname =
  current.linenum  <- 1     ;
  current.linepos  <- [Pos(0)]   ;
  current.fileName <- fname ;
  current.errors   <- false;
  current.is_reversed <- true
      
(** Called for each newline *)
let startNewline n =
  current.linenum <- current.linenum + 1;
  current.linepos <- Pos(n)::current.linepos
			   
(** Called when a new file is scanned *)
let startNewFile n fname startLine = 
  current.linenum <- current.linenum + 1;
  current.linepos <- PosNewFile(n,startLine,fname)::current.linepos

(** Creates a string <filename>:<line:offset-line:offset> *)
let getLocation (i : info) pos pos2 = 
  let rec look line prev = function
    | [Pos(a)] -> (line+1, pos - a -1, pos2 -a -1)
    | Pos(a) :: _ when (pos<a) -> (line, pos -prev -1, pos2 -prev -1)
    | PosNewFile(a, l, n)::tail-> i.fileName <- n; look l a tail
    | Pos(a)::tail -> look (line+1) a tail
    | _ -> (0, 0, 0)
  in
  let () = if i.is_reversed then (
    i.linepos <- List.rev i.linepos;
    i.is_reversed <- false
  ) else () 
  in
  let (lin,col,col2) = look 0 0 i.linepos in
  let preloc =
    if pos2 <= 0 then
      "-" 
    else
      if col=col2 then 
	Printf.sprintf "%d.%d" lin (col)
      else 
	Printf.sprintf "%d.%d-%d.%d" lin (col) lin (col2)
  in
  Printf.sprintf "%s:%s" i.fileName preloc

(** Retrieve file-name and line-number of a position *)
let pos2info (pos, pos2) = 
  let i = current in
  let rec look line prev = function
    | [Pos(a)] -> (line+1, pos - a -1, pos2 -a -1)
    | Pos(a) :: _ when (pos<a) -> (line, pos -prev -1, pos2 -prev -1)
    | PosNewFile(a, l, n)::tail-> i.fileName <- n; look l a tail
    | Pos(a)::tail -> look (line+1) a tail
    | _ -> (0, 0, 0)
  in
  let () = if i.is_reversed then (
    i.linepos <- List.rev (i.linepos);
    i.is_reversed <- false
  ) else () in
  let (lin,_,_) = look 0 0 i.linepos in  
  (i.fileName, lin)

(** Error msg to user *)
let error (startpos, endpos) msg =
  current.errors <- true;
  Printf.eprintf "%s:error: %s\n" (getLocation current startpos endpos) msg

let fmt_error (startpos, endpos) msg = 
  current.errors <- true;
  Printf.sprintf "%s:error: %s\n" (getLocation current startpos
				      endpos) msg
  
(*******************)
(* End 15-411 code *)
(*******************)

(* let flag_coerce = ref false;; *)


(* let rec strip_casts lval = *)
(*   match lval with *)
(*       ATemp _ -> lval *)
(*     | AMem _ -> lval *)
(*     (\* | ALVAnnot(l,_,_) -> l *\) *)

(* let rec unwind_type t =  *)
(*   match t with *)
(*       TAttr(t', a) -> unwind_type t' *)
(*     | _ -> t *)

(* let typeof_var (_,_,t) = *)
(*   unwind_type t *)




(* let rec to_vine_typ t  =  *)
(*   match t with *)
(*       REG_1  -> Vine.REG_1 *)
(*     | REG_8  -> Vine.REG_8 *)
(*     | REG_16 -> Vine.REG_16 *)
(*     | REG_32 -> Vine.REG_32 *)
(*     | REG_64 -> Vine.REG_64 *)
(*     | TString -> Vine.TString *)
(*     | TVoid  -> (\* if you use newvar, you will never get here *\) *)
(* 	raise (V.VineError ("Variables cannot be void")) *)
(*     | Array(t1, i) -> *)
(* 	Vine.Array(to_vine_typ t1, i) *)
(*     | TAttr(t', a) -> Vine.TAttr(to_vine_typ t', a) *)
(*     | TMem(t', endian) -> Vine.TMem(to_vine_typ t, endian) *)


(* let to_vine_var (i,n,t) : Vine.var =  *)
(*   (i,n, to_vine_typ t) *)

(* let rec to_vine_lval lv =  *)
(*   match lv with *)
(*       ATemp(v, p) -> V.Temp(to_vine_var v) *)
(*     | AMem(v,e,t,p) -> V.Mem(to_vine_var v, *)
(* 			     to_vine_exp e, *)
(* 			     to_vine_typ t ) *)

(* and  to_vine_value v =  *)
(*   match v with *)
(*       Int(t,i) -> Vine.Int(to_vine_typ t, i) *)
(*     | Str(s) -> Vine.Str(s) *)

(* and  to_vine_exp  e =  *)
(*   match e with *)
(*     | ABinOp(bop, e1, e2, _) ->  *)
(* 	V.BinOp(bop, (to_vine_exp e1), (to_vine_exp e2)) *)
(*     | AUnOp(uop, e, _) -> *)
(* 	V.UnOp(uop, (to_vine_exp e)) *)
(*     | AConstant(v, p) -> *)
(* 	V.Constant(to_vine_value v) *)
(*     | ALval(lv, _) -> *)
(* 	V.Lval(to_vine_lval  lv) *)
(*     | AName(l, _) -> V.Name(l) *)
(*     | ACast(ct,t,e,p) -> V.Cast(ct,  *)
(* 				to_vine_typ t,  *)
(* 				to_vine_exp e) *)
(*     | AUnknown(s,_) -> V.Unknown(s) *)
(*     | ALet(lv, e, e2,_) ->  *)
(* 	let lv = to_vine_lval lv in *)
(* 	let e = to_vine_exp e in *)
(* 	let e2 = to_vine_exp e2 in  *)
(* 	  V.Let(lv,e,e2) *)
(*     | ACall(_,_,p) ->  *)
(* 	let msg = "Calls cannot be in nested exp for now (ACall error)" *)
(* 	in raise (V.VineError(fmt_error p msg)) *)
(* ;; *)


(* let declvar_to_var = function *)
(*     AVarDecl(v,p) -> to_vine_var v *)
(*   | _ -> raise (Invalid_argument "declvar only works on var decls") *)

(* (\** converts an abstract syntax statement to a vine statement *)
(*     option. We return an option since variable declarations are not *)
(*     translated as vine IR statements *\) *)
(* let rec to_vine_stmt  s : (V.stmt option) =  *)
(*   let lnstmt (vinestmt:V.stmt) p = *)
(*     if !track_line_numbers then ( *)
(*       let pinfo = pos2info p in  *)
(* 	V.Attr(vinestmt, (Vine.Pos(pinfo))) *)
(*     ) else vinestmt *)
(*   in  *)
(*     match (s:absstmt) with *)
(* 	AJmp(e, p) -> Some(lnstmt (V.Jmp(to_vine_exp  e)) p) *)
(*       | ACJmp(e, e1, e2, p) -> *)
(* 	  Some(lnstmt (Vine.CJmp(to_vine_exp e, *)
(* 				to_vine_exp e1, *)
(* 				to_vine_exp e2)) p) *)
(*       | AMove(lv, ACall(e, al,_), p) -> *)
(* 	  let lv = to_vine_lval lv in *)
(* 	  let e = to_vine_exp e in  *)
(* 	  let al = List.map (to_vine_exp) al in  *)
(* 	    Some(lnstmt(Vine.Call(Some(lv), e, al)) p) *)
(*       | AMove(lv, e, p) -> *)
(* 	  Some(lnstmt (Vine.Move(to_vine_lval lv,  *)
(* 				to_vine_exp e)) p) *)
(*       | ASpecial(str, p) -> Some(lnstmt (Vine.Special(str)) p) *)
(*       | ALabel(lbl, p) -> Some(lnstmt (Vine.Label(lbl)) p) *)
(*       | AExpStmt(ACall(e,al,_),p) -> *)
(* 	  let e= to_vine_exp e in  *)
(* 	  let al = List.map (to_vine_exp) al in  *)
(* 	    Some(lnstmt (Vine.Call(None, e, al)) p) *)
(*       | AExpStmt(e, p) -> Some(lnstmt (Vine.ExpStmt(to_vine_exp e)) p) *)
(*       | AComment(str, p) -> Some(lnstmt (Vine.Comment(str)) p) *)
(*       | ABlock(sl, p) -> ( *)
(* 	  let blkdecls = ref [] in  *)
(* 	  let newstmts = ref [] in  *)
(* 	  let addstmt newstmt  = *)
(* 	    match newstmt with *)
(* 		Some(s') -> newstmts := s'::!newstmts *)
(* 	      | _ -> ()  *)
(* 	  in  *)
(* 	  let () = List.iter *)
(* 	    (fun s' -> *)
(* 	       match s' with *)
(* 		    AVarDecl(v,p) ->  *)
(* 		      blkdecls := (declvar_to_var s') :: !blkdecls *)
(* 		  | AFunDecl(v,_,_,_,p) -> *)
(* 		      let msg = v^": Functions must be declared"^ *)
(* 			"in the global scope" in *)
(* 			raise (V.VineError(fmt_error p msg)) *)
(* 		  | AFunction(v,_,_,_,p) -> *)
(* 		      let msg = v^": Functions must be defined"^ *)
(* 			"in the global scope" in *)
(* 			raise (V.VineError(fmt_error p msg)) *)
(* 		  | _ -> addstmt (to_vine_stmt s') *)
(* 		) sl; *)
(* 	  in *)
(* 	    Some(lnstmt (V.Block(List.rev !blkdecls, List.rev !newstmts)) p) *)
(* 	) *)
(*     | AReturn(Some(e), p) -> *)
(* 	Some(lnstmt (Vine.Return(Some(to_vine_exp e))) p ) *)
(*     | AReturn(None, p) -> *)
(* 	Some(lnstmt (Vine.Return(None)) p ) *)
(*     | AVarDecl(v,p) -> None *)
(*     | AFunDecl(v,t,frmls, isextern, p) -> ( *)
(* 	let frmls = List.map (to_vine_var) frmls in  *)
(* 	let t = (match t with *)
(* 		     TVoid -> None *)
(* 		   | x -> Some(to_vine_typ t)) in  *)
(* 	  Some(lnstmt (V.Function(v, t, frmls, isextern, None)) p) *)
(*       ) *)
(*     | AFunction(v,t,frmls, s', p) ->  ( *)
(* 	let frmls = List.map (to_vine_var) frmls in  *)
(* 	let t = (match t with *)
(* 		     TVoid -> None *)
(* 		   | x -> Some(to_vine_typ t)) in  *)
(* 	let s' = to_vine_stmt s' in  *)
(* 	let s'' = (\* we drop attributes on the function definition *)
(* 		     code block because some of the old vine code *)
(* 		     expects a function definition to begin *)
(* 		     with a Block, and Attr(Block..) may mess *)
(* 		     them up *\) *)
(* 	  match s' with *)
(* 	      Some(V.Attr(s'', _)) -> Some(s'') *)
(* 	    | Some _ -> s' *)
(* 	    | None -> None *)
(* 	in *)
(* 	  Some(lnstmt (V.Function(v,t,frmls, false, s'')) p) *)
(*       ) *)
(*     | ANop(p) -> None *)
(* ;; *)


(* let to_vine coerce track_lines sl  : Vine.program = *)
(* (\** convert an abstract tree (statement list) into a vine IR statement *)
(*   block *\) *)
(*   flag_coerce := coerce; *)
(*   track_line_numbers := track_lines; *)
(*   let decls,stmts = List.partition  *)
(*     (function AVarDecl _ -> true | _ -> false) sl in  *)
(*   let dl = List.map (declvar_to_var) decls in  *)
(*   let sl = list_map_some (to_vine_stmt) stmts in  *)
(*     (dl,sl) *)
(* ;; *)


class scoping = 
object(self)

  val ctx = Hashtbl.create 97883
  val scopes = let s = Stack.create() in Stack.push [] s; s

  method inc () =
    Stack.push [] scopes

  method scopenum () =
    Stack.length scopes - 1

  method dec () =
    try
      let dl = Stack.pop scopes in
      List.iter (Hashtbl.remove ctx) dl 
    with Stack.Empty -> failwith "No scope to decrement!"


  method decls () =
    try
      let dl = Stack.top scopes in
	List.rev_map (Hashtbl.find ctx) dl
    with Stack.Empty -> pwarn "Called scoping#decls without scope"; []

  method lookup = 
    Hashtbl.find ctx

  method clear () = 
    Hashtbl.clear ctx;
    Stack.clear scopes;
    Stack.push [] scopes

  method private shadow_warn n = 
    (
      try 
	let _  = self#lookup n in
	  pwarn (n^" shadowing previous declaration")
      with
	  Not_found -> ()
    )

  method add n (t:Vine.typ) = 
    if !shadow_warn then 
      self#shadow_warn n;
    let v = newvar (stripnum n) t in 
    ( try 
       (let dl = Stack.pop scopes in
         Stack.push (n::dl) scopes
        )
      with 
        Stack.Empty -> Stack.push [n] scopes
    );
      Hashtbl.add ctx n v;
      v

  (** FIXME: Undocumented. When should this be used? *)
  method insert ((_,n,_) as var) =
    let dl = Stack.pop scopes in 
      Stack.push (n::dl) scopes;
      Hashtbl.add ctx n var

end

let defscope = ref None

let getscope () = 
  match !defscope with
      None -> 
	let newscope = new scoping in 
	defscope := Some(newscope);
	newscope
    | Some(x) ->
	x

let err lctxt s = 
  raise (Vine.ParseError (fmt_error lctxt s))
    
    
let typeof_var (_,_,t) = t


let check_lval scope v optind opttyp lctx = 
    let var = (
      try 
	scope#lookup v 
      with
	  Not_found -> err lctx (v^" not declared")
     )  in
      match optind,opttyp with
	| None,Some t' ->
	    let t = typeof_var var in
	      if  t' = t then
		(* temp with type annotation which is same as decled type *)
		Temp(var)
	      else
		(* temp with some type annotation not the same as decled
		   type *)
		err lctx ("Temp '"^v^"' annotation and declaration type mismatch: "
			  ^Vine.type_to_string t' ^" != "^Vine.type_to_string t)
	| None,None ->
	    (* no annoation. use type from context *)
	    Temp(var)
	| Some(e), None  -> (
	    (* index with no type annotation. Only supported for array *)
	    match  typeof_var var with
		Array(t1,i) -> Mem(var,e ,t1)
	      | TMem _ -> 
		  err lctx ("memory reads/writes must be annotated with type")
	      | _ -> 
		  err lctx ("can only index memory and array type variables")
	  )
	| Some(e), Some(t)  -> (
	    match typeof_var var with
		Array(t1,i) when t1 = t ->
		  (* array read with type annotation same as what you
		     would read *)
		  Mem(var,e,t1)
	      | Array(t1,_) -> (* array annotation with different than
			      decled type *)
		  err lctx ("Mem '"^v^"' annotation and declaration type mismatch")
	      | TMem _ ->
		  Mem(var, e, t)
	      | _ -> 
		  err lctx (v^ " cannot be indexed (not array or memory)")
	  )


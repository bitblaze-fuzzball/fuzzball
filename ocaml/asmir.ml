(** High level interface to libasmir.
    
    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

open Libasmir
open Vine
open Vine_util

type asmprogram = Libasmir.asm_program_t

type arch = Libasmir.asmir_arch

let arch_i386 = Libasmir.Asmir_arch_x86
let arch_x64  = Libasmir.Asmir_arch_x64
let arch_arm  = Libasmir.Asmir_arch_arm
(*more to come later when we support them*)

module D = Debug.Make(struct let name = "ASMIR" and default=`Debug end)
open D

let assert_mem (_,n,t) = 
  match unwind_type t with
      TMem _ -> ()
    | _ -> raise 
	(Vine.VineError (n^" should be type memory") )


(** Translate a unop *)
let tr_unop = function
    Libasmir.NEG -> NEG
  | Libasmir.NOT -> NOT

let tr_funop = function
  | Libasmir.FNEG -> FNEG

(** Translate a type *)
let tr_regtype = function
    Libasmir.REG_1   -> REG_1 
  | Libasmir.REG_8   -> REG_8 
  | Libasmir.REG_16  -> REG_16
  | Libasmir.REG_32  -> REG_32
  | Libasmir.REG_64  -> REG_64

(* primitive exp translation cache.
   set exp_cache_enabled to use.
*)
(* let exp_cache_max_size = 100000;; *)
(* let exp_cache_current_size = ref 0;; *)
(* let exp_cache_hits = ref 0;; *)
(* let exp_cache_misses = ref 0;; *)
(* let exp_cache = Hashtbl.create exp_cache_max_size;; *)
let exp_cache_enabled = ref false;;
module ExpHash =
struct
  type t = Vine.exp
  let equal = (=)
  let hash = Hashtbl.hash
end
module ExpCache = Weak.Make(ExpHash);;
(* It would be nice to pre-allocate this hash table to a large size
   only when it's enabled, but to do that we'd need to change the
   interface so that there's a function that enables it rather than
   just a flag. It looks like this isn't commonly used at the moment,
   so let's make it small. -SMcC *)
let exp_cache = ExpCache.create 1;;

(*let name_cache = Hashtbl.create 113;; *)


(* maps a string variable to the var we are using for it *)
type varctx = (string,var) Hashtbl.t

(** [gamma_create mem decls] creates a new varctx for use during translation. 
    [mem] is the var that should be used for memory references, and [decls]
    should be a list of variables already in scope.
*)
let gamma_create mem decls : varctx =
  let h = Hashtbl.create 57 in
  (* let mem = newvar "mem"  (Array(addr_t, REG_8)) in *)
  let () = List.iter (fun ((_,nm,_) as var) -> Hashtbl.add h nm var) decls in
  let () = Hashtbl.add h "$mem" mem in
    h

let gamma_lookup (g:varctx) s =
  try Hashtbl.find g s
  with Not_found ->
    failwith("Disassembled code had undeclared variable '"^s^"'. Something is broken.")

let gamma_extend = Hashtbl.add


let gamma_unextend = Hashtbl.remove

let warn_mem =
  let rec r = 
    ref (fun () ->
	   pwarn  "Using different variable for mem, due to typing issues. This may break stuff.";
	   r := (fun () -> ())
	)
  in
    (fun () -> !r())

(** Translate an expression *)
let rec tr_exp g e =
  let tr = 
    match Libasmir.exp_type e with
    | BINOP ->
	tr_binop g (Libasmir.binop_type e) (Libasmir.binop_lhs e) (Libasmir.binop_rhs e)
    | FBINOP ->
	tr_fbinop g (Libasmir.fbinop_type e) (Libasmir.fbinop_lhs e) (Libasmir.fbinop_rhs e)
    | UNOP ->
        UnOp(tr_unop(Libasmir.unop_type e),
	     tr_exp g (Libasmir.unop_subexp e) )
    | FUNOP ->
        FUnOp(tr_funop(Libasmir.funop_type e), ROUND_NEAREST,
	      tr_exp g (Libasmir.funop_subexp e) )
    | CONSTANT ->
        Constant(Int (tr_regtype (constant_regtype e), 
		      Libasmir.constant_val e))
    | MEM -> (
	let mem = gamma_lookup g "$mem" in
	  (* checkme *)
	let wtyp = tr_regtype (mem_regtype e) in 
	  assert_mem mem;
	  Lval(Mem(mem, tr_exp g (mem_addr e), wtyp))
      )
    | TEMP ->
	let nm = temp_name e  in
	let (_,_,t) as var = gamma_lookup g nm in
	let t' = tr_regtype(temp_regtype e) in
	  if t <> t'
	  then failwith("Disassembly produced incorrect type "^type_to_string t'^" for"^var_to_string var)
	  else Lval(Temp var)
    | CAST ->
        let sube = tr_exp g (cast_subexp e) in
        let newt = tr_regtype(cast_width e) in
	(match cast_casttype e with
	   Libasmir.CAST_UNSIGNED -> Cast(CAST_UNSIGNED, newt, sube)
	 | Libasmir.CAST_SIGNED   -> Cast(CAST_SIGNED, newt, sube)
	 | Libasmir.CAST_HIGH	   -> Cast(CAST_HIGH, newt, sube)
	 | Libasmir.CAST_LOW	   -> Cast(CAST_LOW, newt, sube)
	)
    | FCAST ->
        let sube = tr_exp g (fcast_subexp e) in
        let newt = tr_regtype(fcast_width e) in
	let vine_ct =
	  (match fcast_casttype e with
	     | Libasmir.CAST_SFLOAT -> CAST_SFLOAT
	     | Libasmir.CAST_UFLOAT -> CAST_UFLOAT
	     | Libasmir.CAST_SFIX -> CAST_SFIX
	     | Libasmir.CAST_UFIX -> CAST_UFIX
	     | Libasmir.CAST_FWIDEN -> CAST_FWIDEN
	     | Libasmir.CAST_FNARROW -> CAST_FNARROW)
	in
	  FCast(vine_ct, ROUND_NEAREST, newt, sube)
    | NAME ->
        Name(name_string e)
    | UNKNOWN ->
        Unknown(unknown_str e)
    | LET ->
        let letv = tr_lval g ( let_var e) in 
        let lete = tr_exp g (let_exp e) in 
	let unextend =
	  match letv with
	      Temp((_,nm,_) as var) ->
		gamma_extend g nm var;
		(fun () -> gamma_unextend g nm)
	    | _ -> fun () -> ()
	in
        let letin = tr_exp g (let_in e) in 
	  unextend();
	  Let(letv, lete, letin)
    | ITE ->
	let cond_e = tr_exp g (ite_cond e) and
	    true_e = tr_exp g (ite_true_e e) and
	    false_e = tr_exp g (ite_false_e e) in
	  Ite(cond_e, true_e, false_e)
    | EXTENSION ->
        failwith "Unknown extension type."
    | VECTOR -> failwith "LibASMIR Vector should not escape to OCaml"
    | _ -> failwith "Unsupported stmt type"
  in
  if !exp_cache_enabled then
    ExpCache.merge exp_cache tr
  else
    tr

(*   if ExpCache.mem exp_cache tr then ( *)
 (*     Printf.printf "Cache hit\n%!"; *)
(*     ExpCache.find exp_cache tr *)
(*   ) else ( *)
(*     ExpCache.merge exp_cache tr *)
(*   ) *)

(*   if !exp_cache_enabled then ( *)
(*     if Hashtbl.mem exp_cache tr then ( *)
(*       inc exp_cache_hits; *)
(*       Hashtbl.find exp_cache tr *)
(*     ) else ( *)
(*       inc exp_cache_misses; *)
(*       if (!exp_cache_current_size >= exp_cache_max_size) then ( *)
(*         Hashtbl.clear exp_cache; *)
(*         exp_cache_current_size := 0 *)
(*       ); *)
(*       Hashtbl.add exp_cache tr tr; *)
(*       inc exp_cache_current_size; *)
(*       tr *)
(*     ) *)
(*   ) else  *)
(*     tr *)

(** Translate a binop *)
and tr_binop g b lhs rhs =
  let (lhs,rhs) = (tr_exp g lhs, tr_exp g rhs) in
  match b with
    | Libasmir.PLUS     -> BinOp(PLUS    , lhs, rhs)
    | Libasmir.MINUS	-> BinOp(MINUS   , lhs, rhs)
    | Libasmir.TIMES	-> BinOp(TIMES   , lhs, rhs)
    | Libasmir.DIVIDE	-> BinOp(DIVIDE  , lhs, rhs)
    | Libasmir.SDIVIDE	-> BinOp(SDIVIDE , lhs, rhs)
    | Libasmir.MOD	-> BinOp(MOD     , lhs, rhs)
    | Libasmir.SMOD	-> BinOp(SMOD    , lhs, rhs)
    | Libasmir.LSHIFT	-> BinOp(LSHIFT  , lhs, rhs)
    | Libasmir.RSHIFT	-> BinOp(RSHIFT  , lhs, rhs)
    | Libasmir.ARSHIFT	-> BinOp(ARSHIFT , lhs, rhs)
    | Libasmir.LROTATE
    | Libasmir.RROTATE	-> failwith "rotate is deprecated"
    | Libasmir.LOGICAND -> BinOp(BITAND  , lhs, rhs)
    | Libasmir.LOGICOR	-> BinOp(BITOR   , lhs, rhs)
    | Libasmir.BITAND	-> BinOp(BITAND  , lhs, rhs)
    | Libasmir.BITOR	-> BinOp(BITOR   , lhs, rhs)
    | Libasmir.XOR	-> BinOp(XOR     , lhs, rhs)
    | Libasmir.EQ	-> BinOp(EQ      , lhs, rhs)
    | Libasmir.NEQ	-> BinOp(NEQ     , lhs, rhs)
    | Libasmir.LT	-> BinOp(LT, lhs, rhs)
    | Libasmir.LE       -> BinOp(LE, lhs, rhs)
    | Libasmir.SLT	-> BinOp(SLT, lhs, rhs)
    | Libasmir.SLE      -> BinOp(SLE, lhs, rhs)
	(* At the moment, we only use < and <= (which is generally
	   VEX's strategy as well). But > and >= can be easily reduced
	   if needed: *)
    | Libasmir.GT	-> BinOp(LT, rhs, lhs) (* (x > y) <-> (y < x) *)
    | Libasmir.GE	-> BinOp(LE, rhs, lhs) (* (x >= y) <-> (y <= x) *)

and tr_fbinop g b lhs rhs =
  let (lhs,rhs) = (tr_exp g lhs, tr_exp g rhs) in
  match b with
    | Libasmir.FPLUS   -> FBinOp(FPLUS,   ROUND_NEAREST, lhs, rhs)
    | Libasmir.FMINUS  -> FBinOp(FMINUS,  ROUND_NEAREST, lhs, rhs)
    | Libasmir.FTIMES  -> FBinOp(FTIMES,  ROUND_NEAREST, lhs, rhs)
    | Libasmir.FDIVIDE -> FBinOp(FDIVIDE, ROUND_NEAREST, lhs, rhs)
    | Libasmir.FEQ     -> FBinOp(FEQ,     ROUND_NEAREST, lhs, rhs)
    | Libasmir.FNEQ    -> FBinOp(FNEQ,    ROUND_NEAREST, lhs, rhs)
    | Libasmir.FLT     -> FBinOp(FLT,     ROUND_NEAREST, lhs, rhs)
    | Libasmir.FLE     -> FBinOp(FLE,     ROUND_NEAREST, lhs, rhs)

(** Translate an lvalue *)
and tr_lval g e =
  match tr_exp g e with
      Lval l -> l
    | _ -> failwith "Other expression where Lval required"

(** Translate a vardecl, and adds the variable to the context
    
    @return vardecl and a function to restore the context
*)
let tr_vardecl (g:varctx) s =
  assert(Libasmir.stmt_type s = VARDECL);
  let nm = Libasmir.vardecl_name s in 
  let var = newvar nm (tr_regtype(Libasmir.vardecl_type s)) in
    gamma_extend g nm var;
    (var, fun () -> gamma_unextend g nm)
    
(** Translate a list of vardecls, adding them to the context.
    @return vardecls and a function to restore the context *)
let tr_vardecls g ss =
  let decls,unextends = List.split(List.map (tr_vardecl g) ss) in
    (decls, fun x -> List.iter (fun f -> f x) unextends)

(** Translate a statement *)
let rec tr_stmt g s =
  match Libasmir.stmt_type s with
      JMP ->
	Jmp(tr_exp g (Libasmir.jmp_target s))
    | CJMP ->
	CJmp(tr_exp g (Libasmir.cjmp_cond s),
	    tr_exp g (Libasmir.cjmp_ttarget s),
	    tr_exp g (Libasmir.cjmp_ftarget s) )
    | SPECIAL ->
	Special(Libasmir.special_string s)
    | MOVE ->
	Move(tr_lval g (move_lhs s), tr_exp g (move_rhs s))
    | COMMENT ->
	Comment(Libasmir.comment_string s)
    | LABEL ->
	Label(Libasmir.label_string s)
    | EXPSTMT ->
	ExpStmt(tr_exp g (Libasmir.expstmt_exp s))
    | VARDECL -> 
	(* FIXME: extend context *)
	Comment "vardecl"
    | CALL ->
        let lval_opt = 
          match Libasmir.call_has_lval s with
          | false -> None
          | true -> Some(tr_lval g (Libasmir.call_lval_opt s))
        in
        let params = 
          let params_a = Libasmir.call_params s in
          let params_l = Array.to_list params_a in
          List.map (tr_exp g) params_l
        in
	  (* tr_exp g (Libasmir.call_fnname s) *)
        Call(lval_opt, tr_exp g (Libasmir.call_fnname s), params)
    | RETURN ->
        let rv = 
          match  Libasmir.ret_has_exp s with
          | false -> None
          | true -> Some(tr_exp g (Libasmir.ret_exp s))
        in
        Return(rv)
    | FUNCTION ->
        let is_vardecl x =  Libasmir.stmt_type x = VARDECL in
          
        let fnname = Libasmir.func_name s in
        let rt_o = 
          match Libasmir.func_has_rv s with
          | false -> None
          | true -> Some(tr_regtype (Libasmir.func_rt s))
        in
        let (params,unextend) = 
	  tr_vardecls g (Array.to_list( Libasmir.func_params s))
        in
        let ext = Libasmir.func_is_external s in
        let body =
	  let body_l = Array.to_list(Libasmir.func_body s) in
          let body_decls, body_stmts =
            List.partition is_vardecl body_l
          in
          let body_decls,unextend' = 
            List.split(List.map (tr_vardecl g) body_decls)
	  in
          let body_stmts = List.map (tr_stmt g) body_stmts in
	    unextend();
            match (body_decls, body_stmts) with
              | [], [] -> None
              | [], [s] -> Some(s)
              | _ -> Some(Block(body_decls, body_stmts))
        in
	  unextend ();
          Function(fnname, rt_o, params, ext, body)
    | ASSERT ->
	Assert(tr_exp g (Libasmir.assert_cond s))

(** Translate a whole vine_block_t (as returned by
    Libasmir.asmir_vine_blocks_get) into a list of statements *)
let tr_vine_block_t g asmp b = 
  let size = Libasmir.asmir_vine_block_size b - 1 in
  let asm =Libasmir.string_blockinsn asmp b in
  let (decs,stmts) =
    foldn (fun (ds,ss) n -> let s = asmir_vine_block_get b n in
	     match Libasmir.stmt_type s with
		 VARDECL -> (s::ds,ss)
	       | _ -> (ds,s::ss) )
      ([],[]) size
  in
  let decls, unextend = tr_vardecls g decs in
  let stmts = List.map (tr_stmt g) stmts in
  let stmts = match stmts with
    | Label _ as l::ys -> (* TODO: move label outside of block *)
	[Block(decls,l::Comment(asm)::ys)]
    | _ -> [Block(decls, Comment(asm)::stmts)]
  in 
    unextend();
    stmts

(** Translate a vine_blocks_t (as returned by
    Libasmir.asmir_asmprogram_to_vine) into a list of statements *)
let tr_vine_blocks_t g asmp bs = 
  let size = Libasmir.asmir_vine_blocks_size bs -1 in
    foldn (fun i n -> tr_vine_block_t g asmp (asmir_vine_blocks_get bs n)@i) [] size

let instmap_translate_address_range g m startaddr endaddr = 
  let bs = Libasmir.instmap_translate_address_range m startaddr
    endaddr  in
  let size = Libasmir.asmir_vine_blocks_size bs -1 in
  let prog = Libasmir.instmap_to_asm_program m in 
  let res = 
    foldn (fun i n -> 
	     let blk = asmir_vine_blocks_get bs n in 
	     let sl = tr_vine_block_t g prog blk in
	     let addr = Libasmir.asmir_vine_block_address blk in 
	       (addr,sl)::i)
      [] size
  in
  let () = destroy_vine_blocks bs in 
    res





let x86_regs : decl list =
  List.map (fun (n,t) -> newvar n t)
    [
  (* 32 bit regs *)
  ("R_EBP", REG_32);
  ("R_ESP", REG_32);
  ("R_ESI", REG_32);
  ("R_EDI", REG_32);
  ("R_EIP", REG_32);
  ("R_EAX", REG_32);
  ("R_EBX", REG_32);
  ("R_ECX", REG_32);
  ("R_EDX", REG_32);
  (* To get the real EFLAGS, you need to combine with R_CF
     through R_OF below *)
  ("EFLAGSREST", REG_32);

(* We shouldn't ever see these subregisters 
  (* 16 bit regs *)
  ("R_AX", REG_16);
  ("R_BX", REG_16);
  ("R_CX", REG_16);
  ("R_DX", REG_16);
  ("R_BP", REG_16);
  ("R_SP", REG_16);
  ("R_SI", REG_16);
  ("R_DI", REG_16);

  (* low 8-bit regs *)
  ("R_AL", REG_8);
  ("R_BL", REG_8);
  ("R_CL", REG_8);
  ("R_DL", REG_8);

  (* high 8-bit regs *)
  ("R_AH", REG_8);
  ("R_BH", REG_8);
  ("R_CH", REG_8);
  ("R_DH", REG_8);
*)

  (* condition flag bits *)
  ("R_CF", REG_1);
  ("R_PF", REG_1);
  ("R_AF", REG_1);
  ("R_ZF", REG_1);
  ("R_SF", REG_1);
  ("R_OF", REG_1);

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", REG_32);
  ("R_CC_DEP1", REG_32);
  ("R_CC_DEP2", REG_32);
  ("R_CC_NDEP", REG_32);

  (* more status flags *)
  ("R_DFLAG", REG_32);
  ("R_IDFLAG", REG_32);
  ("R_ACFLAG", REG_32);
  ("R_EMWARN", REG_32);
  ("R_LDT", REG_32); 
  ("R_GDT", REG_32); 

  (* segment regs *)
  ("R_CS", REG_16); 
  ("R_DS", REG_16); 
  ("R_ES", REG_16); 
  ("R_FS", REG_16); 
  ("R_GS", REG_16); 
  ("R_SS", REG_16); 

  (* floating point *)
  ("R_FTOP", REG_32);
  ("R_FPROUND", REG_32);
  ("R_FC3210", REG_32);
  (* main x87 FP registers, pretending they're 64-bit as VEX does *)
  ("R_FPREG0", REG_64);
  ("R_FPREG1", REG_64);
  ("R_FPREG2", REG_64);
  ("R_FPREG3", REG_64);
  ("R_FPREG4", REG_64);
  ("R_FPREG5", REG_64);
  ("R_FPREG6", REG_64);
  ("R_FPREG7", REG_64);
  (* In-use tags for the above *)
  ("R_FPTAG0", REG_8);
  ("R_FPTAG1", REG_8);
  ("R_FPTAG2", REG_8);
  ("R_FPTAG3", REG_8);
  ("R_FPTAG4", REG_8);
  ("R_FPTAG5", REG_8);
  ("R_FPTAG6", REG_8);
  ("R_FPTAG7", REG_8);

  (* SIMD instructions *)
  ("R_SSEROUND", REG_32);

  (* more recent VEX quirks *)
  ("R_EMWARN", REG_32);
  ("R_IP_AT_SYSCALL", REG_32);

  (* SSE registers, each divided in half since REG_64 is our biggest *)
  ("R_XMM0L", REG_64);
  ("R_XMM0H", REG_64);
  ("R_XMM1L", REG_64);
  ("R_XMM1H", REG_64);
  ("R_XMM2L", REG_64);
  ("R_XMM2H", REG_64);
  ("R_XMM3L", REG_64);
  ("R_XMM3H", REG_64);
  ("R_XMM4L", REG_64);
  ("R_XMM4H", REG_64);
  ("R_XMM5L", REG_64);
  ("R_XMM5H", REG_64);
  ("R_XMM6L", REG_64);
  ("R_XMM6H", REG_64);
  ("R_XMM7L", REG_64);
  ("R_XMM7H", REG_64);
]


(* exectrace needs fixing if this is REG_64 *)
let x86_mem :decl = newvar "mem" (TMem(Vine.addr_t, Little))
let arm_mem = x86_mem

let x64_regs : decl list =
  List.map (fun (n,t) -> newvar n t)
    [
  (* 64 bit regs *)
  ("R_RBP", REG_64);
  ("R_RSP", REG_64);
  ("R_RSI", REG_64);
  ("R_RDI", REG_64);
  ("R_RIP", REG_64);
  ("R_RAX", REG_64);
  ("R_RBX", REG_64);
  ("R_RCX", REG_64);
  ("R_RDX", REG_64);
  ("R_R8",  REG_64);
  ("R_R9",  REG_64);
  ("R_R10", REG_64);
  ("R_R11", REG_64);
  ("R_R12", REG_64);
  ("R_R13", REG_64);
  ("R_R14", REG_64);
  ("R_R15", REG_64);
  (* To get the real RFLAGS, you need to combine with R_CF
     through R_OF below *)
  ("R_RFLAGSREST", REG_64);

  (* Limited 64-bit segments support: just a base address for %fs and
     %gs *)
  ("R_FS_BASE", REG_64);
  ("R_GS_BASE", REG_64);

  (* condition flag bits *)
  ("R_CF", REG_1);
  ("R_PF", REG_1);
  ("R_AF", REG_1);
  ("R_ZF", REG_1);
  ("R_SF", REG_1);
  ("R_OF", REG_1);

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", REG_64);
  ("R_CC_DEP1", REG_64);
  ("R_CC_DEP2", REG_64);
  ("R_CC_NDEP", REG_64);

  (* more status flags *)
  ("R_DFLAG", REG_64);
  ("R_IDFLAG", REG_64);
  ("R_ACFLAG", REG_64);
  ("R_EMNOTE", REG_32);

  (* floating point *)
  ("R_FTOP", REG_32);
  ("R_FPROUND", REG_64);
  ("R_FC3210", REG_64);
  (* main x87 FP registers, pretending they're 64-bit as VEX does *)
  ("R_FPREG0", REG_64);
  ("R_FPREG1", REG_64);
  ("R_FPREG2", REG_64);
  ("R_FPREG3", REG_64);
  ("R_FPREG4", REG_64);
  ("R_FPREG5", REG_64);
  ("R_FPREG6", REG_64);
  ("R_FPREG7", REG_64);
  (* In-use tags for the above *)
  ("R_FPTAG0", REG_8);
  ("R_FPTAG1", REG_8);
  ("R_FPTAG2", REG_8);
  ("R_FPTAG3", REG_8);
  ("R_FPTAG4", REG_8);
  ("R_FPTAG5", REG_8);
  ("R_FPTAG6", REG_8);
  ("R_FPTAG7", REG_8);

  (* SIMD instructions *)
  ("R_SSEROUND", REG_64);

  (* SSE registers, each divided in 4 since REG_64 is our biggest *)
  ( "R_YMM0_0", REG_64); ( "R_YMM0_1", REG_64);
  ( "R_YMM0_2", REG_64); ( "R_YMM0_3", REG_64);
  ( "R_YMM1_0", REG_64); ( "R_YMM1_1", REG_64);
  ( "R_YMM1_2", REG_64); ( "R_YMM1_3", REG_64);
  ( "R_YMM2_0", REG_64); ( "R_YMM2_1", REG_64);
  ( "R_YMM2_2", REG_64); ( "R_YMM2_3", REG_64);
  ( "R_YMM3_0", REG_64); ( "R_YMM3_1", REG_64);
  ( "R_YMM3_2", REG_64); ( "R_YMM3_3", REG_64);
  ( "R_YMM4_0", REG_64); ( "R_YMM4_1", REG_64);
  ( "R_YMM4_2", REG_64); ( "R_YMM4_3", REG_64);
  ( "R_YMM5_0", REG_64); ( "R_YMM5_1", REG_64);
  ( "R_YMM5_2", REG_64); ( "R_YMM5_3", REG_64);
  ( "R_YMM6_0", REG_64); ( "R_YMM6_1", REG_64);
  ( "R_YMM6_2", REG_64); ( "R_YMM6_3", REG_64);
  ( "R_YMM7_0", REG_64); ( "R_YMM7_1", REG_64);
  ( "R_YMM7_2", REG_64); ( "R_YMM7_3", REG_64);
  ( "R_YMM8_0", REG_64); ( "R_YMM8_1", REG_64);
  ( "R_YMM8_2", REG_64); ( "R_YMM8_3", REG_64);
  ( "R_YMM9_0", REG_64); ( "R_YMM9_1", REG_64);
  ( "R_YMM9_2", REG_64); ( "R_YMM9_3", REG_64);
  ("R_YMM10_0", REG_64); ("R_YMM10_1", REG_64);
  ("R_YMM10_2", REG_64); ("R_YMM10_3", REG_64);
  ("R_YMM11_0", REG_64); ("R_YMM11_1", REG_64);
  ("R_YMM11_2", REG_64); ("R_YMM11_3", REG_64);
  ("R_YMM12_0", REG_64); ("R_YMM12_1", REG_64);
  ("R_YMM12_2", REG_64); ("R_YMM12_3", REG_64);
  ("R_YMM13_0", REG_64); ("R_YMM13_1", REG_64);
  ("R_YMM13_2", REG_64); ("R_YMM13_3", REG_64);
  ("R_YMM14_0", REG_64); ("R_YMM14_1", REG_64);
  ("R_YMM14_2", REG_64); ("R_YMM14_3", REG_64);
  ("R_YMM15_0", REG_64); ("R_YMM15_1", REG_64);
  ("R_YMM15_2", REG_64); ("R_YMM15_3", REG_64);

  (* more recent VEX quirks *)
  ("R_EMNOTE", REG_32);
  ("R_IP_AT_SYSCALL", REG_64);
]


let x64_mem : decl = newvar "mem" (TMem(Vine.REG_64, Little))

let x86_eflags_helpers () =
  (* FIXME: destroy C++ stmts *)
  let stmts = Libasmir.gen_eflags_helpers_c () in
  let stmts = Array.to_list stmts in
  let g = gamma_create x86_mem x86_regs in
  let stmts = List.map (tr_stmt g) stmts in
  stmts


let arm_regs : decl list =
  List.map (fun n -> newvar n REG_32)
    [ "R0";
      "R1";
      "R2";
      "R3";
      "R4";
      "R5";
      "R6";
      "R7";
      "R8";
      "R9";
      "R10";
      "R11";
      "R12";
      "R13";
      "R14";
      "R15";
      "R15T";
      "R_CC";
      "R_CC_OP";	 
      "R_CC_DEP1";
      "R_CC_DEP2";
      "R_CC_NDEP";
      "R_QFLAG32";
      "R_GEFLAG0";
      "R_GEFLAG1";
      "R_GEFLAG2";
      "R_GEFLAG3";
      "R_EMWARN";
      "R_TISTART";
      "R_TILEN";
      "R_NRADDR";
      "R_IP_AT_SYSCALL";
      "R_FPSCR";
      "R_TPIDRURO";
      "R_ITSTATE";
    ]
  @
  List.map (fun (n,t) -> newvar n t)
    [
      ("R_D0", REG_64);
      ("R_D1", REG_64);
      ("R_D2", REG_64);
      ("R_D3", REG_64);
      ("R_D4", REG_64);
      ("R_D5", REG_64);
      ("R_D6", REG_64);
      ("R_D7", REG_64);
      ("R_D8", REG_64);
      ("R_D9", REG_64);
      ("R_D10", REG_64);
      ("R_D11", REG_64);
      ("R_D12", REG_64);
      ("R_D13", REG_64);
      ("R_D14", REG_64);
      ("R_D15", REG_64);
      ("R_D16", REG_64);
      ("R_D17", REG_64);
      ("R_D18", REG_64);
      ("R_D19", REG_64);
      ("R_D20", REG_64);
      ("R_D21", REG_64);
      ("R_D22", REG_64);
      ("R_D23", REG_64);
      ("R_D24", REG_64);
      ("R_D25", REG_64);
      ("R_D26", REG_64);
      ("R_D27", REG_64);
      ("R_D28", REG_64);
      ("R_D29", REG_64);
      ("R_D30", REG_64);
      ("R_D31", REG_64);

      ("R_NF", REG_1);
      ("R_ZF", REG_1);
      ("R_CF", REG_1);
      ("R_VF", REG_1);
    ]

let decls_for_arch = function
  | Asmir_arch_x86 -> x86_mem::x86_regs
  | Asmir_arch_x64 -> x64_mem::x64_regs
  | Asmir_arch_arm -> arm_mem::arm_regs

let gamma_for_arch = function
  | Asmir_arch_x86 -> gamma_create x86_mem x86_regs
  | Asmir_arch_x64 -> gamma_create x64_mem x64_regs
  | Asmir_arch_arm -> gamma_create arm_mem arm_regs

let helpers_for_arch = function
  | Asmir_arch_x86 -> x86_eflags_helpers()
  | Asmir_arch_x64 -> []
  | Asmir_arch_arm -> []

let asmprogram_arch = Libasmir.asmprogram_arch

let instmap_has_address m addr = 
  0 <> Libasmir.instmap_has_address m addr


let fold_memory_data f md acc =
    let size = Libasmir.memory_data_size md - 1 in
      Vine_util.foldn
	(fun a n ->
	   let mcd = Libasmir.memory_data_get md n in
	     f 
	       (Libasmir.memory_cell_data_address mcd)
	       (Libasmir.memory_cell_data_value mcd)
	       a
	)
	acc
	size

let fold_rodata f prog =
  let rodata = Libasmir.get_rodata prog in
    fold_memory_data f rodata

let fold_bss f prog =
  let bss = Libasmir.get_bssdata prog in
    fold_memory_data f bss

(** obtain RODATA (a list of (address, value) pairs) from given file *)
let get_rodata_from_file filename =
  let asmp = disassemble_program filename in
    fold_rodata (fun a d acc -> (a,d)::acc) asmp []

(** obtain BSS section (a list of (address, value) pairs) from given file *)
let get_bss_from_file filename =
  let asmp = disassemble_program filename in
    fold_bss (fun a d acc -> (a,d)::acc) asmp []

let get_rodata_assignments ?(prepend_to=[]) mem prog =
  fold_rodata
    (fun a v acc -> Move(Mem(mem, Constant(Int(addr_t, a)), REG_8),
			 Constant(Int(REG_8, Int64.of_int v)))      :: acc)
    prog prepend_to

let zero_reg8 = Constant(Int(REG_8, 0L)) (* recycle the same one *)
let get_bss_assignments ?(prepend_to=[]) mem prog =
  fold_bss
    (fun a v acc ->
       assert (v = 0);
       Move(Mem(mem, Constant(Int(addr_t, a)), REG_8), zero_reg8) :: acc)
    prog prepend_to


(** Disassemble a program. @return a Libasmir.asm_program_t which will be
    freed automagically by the GC. *)
let disassemble_program filename =
  let prog = Libasmir.disassemble_program filename in
    (* tell the GC how to free resources associated with prog *)
  let () = Gc.finalise Libasmir.free_asm_program prog in
    prog

(** Translate an entire Libasmir.asm_program_t into a Vine program *)
let asmprogram_to_vine ?(init_mem=false) asmp : Vine.program= 
  let vine_blocks = Libasmir.asmir_asmprogram_to_vine asmp in
  let arch = asmprogram_arch asmp in
  let g = gamma_for_arch arch in
  let ir = tr_vine_blocks_t g asmp vine_blocks in
  let () = destroy_vine_blocks vine_blocks in
  let ir =
    if init_mem then (
      let m = gamma_lookup g "$mem" in
      let ir = get_bss_assignments ~prepend_to:ir m asmp in
	get_rodata_assignments ~prepend_to:ir m asmp
    )
    else ir
  in
    (decls_for_arch arch, helpers_for_arch arch @ ir)



(** Translate only one address of a  Libasmir.asm_program_t to Vine *)
let asm_addr_to_vine g prog addr =
  let block= Libasmir.asm_addr_to_ir prog addr in
  let ir = tr_vine_block_t g prog block in
  let () = destroy_vine_block block in
    ir

(** Translate dyn_function_t to (address * function name) *)
let tr_dyn_function_t d =
  let name = Libasmir.dyn_functions_name d in
  let addr = Libasmir.dyn_functions_addr d in
  (addr, name)

(** translate a single symbol info *)
let tr_vine_symbol_t symopt = 
  match symopt with
      None -> failwith "C++ vine returned NULL symbol"
    | Some(sym) -> 
	(sym.addr, sym.name, (sym.is_function = 1), (sym.is_dynamic = 1))


(** Translate dyn_functions_t to (string * int64) list *)
let tr_dyn_functions_t ds =
  let size = Libasmir.dyn_functions_size ds - 1 in
  foldn (fun i n -> tr_dyn_function_t (Libasmir.dyn_functions_get ds n)::i) [] size

(** translate a symbol info list *)
let tr_vine_symbols_t syms = 
  let size = Libasmir.symbols_size syms -1 in 
    foldn (fun i n -> tr_vine_symbol_t (Libasmir.symbols_get syms
					   n)::i) [] size
;;


(** get dynamic library information*)
let get_synthetic_functions filename =
  let dfs = Libasmir.get_synthetic_symbols filename in
  let ret = tr_dyn_functions_t dfs in
  let () = destroy_dyn_functions dfs in
  ret

(** return the symbol information in a file. 
    @param filename is the executable filename
    @return (symbol addr, symbol name, is_function, is_dynamic) list
*)
let vine_symbols_of_file filename = 
  let syms = Libasmir.get_symbols_of_file filename in
  let ret = tr_vine_symbols_t syms in
  let () = destroy_symbols syms in 
    ret

(** translate memory cell data to a pair of address and value
    @deprecated Don't use this
 *)
let tr_memory_cell_data_t mcd =
  (Libasmir.memory_cell_data_address mcd, Libasmir.memory_cell_data_value mcd)

(** translate a memory data to a list of (address, value) pairs
    @deprecated Don't use this
*)
let tr_memory_data_t md =
  let size = Libasmir.memory_data_size md - 1 in
  Vine_util.mapn (fun i -> tr_memory_cell_data_t (
    Libasmir.memory_data_get md i)) size

(** Translate an instruction to Vine
    [asm_bytes_to_vine g arch addr bytes] uses context [g] for variable lookups,
    [arch] as the architecture the instruction is for, [addr] as the address
    the instruction is at, and [bytes] as the bytes of the instruction itself.
    Right now, bytes is a char array for historical reasons, but it should be
    a string in the future.
 *)
let asm_bytes_to_vine g arch addr bytes =
  let asmp = Libasmir.byte_insn_to_asmp arch addr bytes in
  let ir = asm_addr_to_vine g asmp addr in
  let () = Libasmir.free_asm_program asmp in
  ir


  
(** Print the disassembly of an i386 instruction.
    This will be deprecated soon, as soon as I figure out how we want to specify
    the architecture in ocaml.
*)
let print_i386_rawbytes = Libasmir.print_disasm_rawbytes Libasmir.Asmir_arch_x86

let print_disasm_rawbytes = Libasmir.print_disasm_rawbytes

(** Get the disassembly of an i386 instruction as a string.
    This will be deprecated soon, as soon as I figure out how we want to specify
    the architecture in ocaml.
*)
let sprintf_i386_rawbytes = 
      Libasmir.sprintf_disasm_rawbytes Libasmir.Asmir_arch_x86

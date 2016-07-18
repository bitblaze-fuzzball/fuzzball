(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module V = Vine

open Exec_utils
open Exec_exceptions
open Frag_simplify
open Formula_manager

module SymbolicDomain : Exec_domain.DOMAIN = struct
  type t = V.exp

  let from_concrete_1 v  = assert(v = 0 || v = 1 || v = -1);    
    V.Constant(V.Int(V.REG_1,  (Int64.of_int (v land 1))))
  let from_concrete_8 v  = assert(v >= -128 && v <= 0xff);
    V.Constant(V.Int(V.REG_8,  (Int64.of_int (v land 0xff))))
  let from_concrete_16 v = assert(v >= -65536 && v <= 0xffff);
    V.Constant(V.Int(V.REG_16, (Int64.of_int (v land 0xffff))))
  let from_concrete_32 v = assert(v >= -4294967296L && v <= 0xffffffffL);
    V.Constant(V.Int(V.REG_32, (Int64.logand v 0xffffffffL)))
  let from_concrete_64 v = V.Constant(V.Int(V.REG_64, v))

  let to_concrete_1 e = match constant_fold_rec e with
    | V.Constant(V.Int(V.REG_1,  v)) -> (Int64.to_int v)
    | V.Constant(V.Int(_,  v)) -> failwith "bad type in to_concrete_1"
    | _ -> raise (NotConcrete e)

  let to_concrete_8 e = match constant_fold_rec e with
    | V.Constant(V.Int(V.REG_8,  v)) -> (Int64.to_int v)
    | V.Constant(V.Int(_,  v)) -> failwith "bad type in to_concrete_8"
    | _ -> raise (NotConcrete e)

  let to_concrete_16 e = match constant_fold_rec e with
    | V.Constant(V.Int(V.REG_16, v)) -> (Int64.to_int v)
    | V.Constant(V.Int(_,  v)) -> failwith "bad type in to_concrete_16"
    | _ -> raise (NotConcrete e)

  let to_concrete_32 e = match constant_fold_rec e with
    | V.Constant(V.Int(V.REG_32, v)) -> v
    | V.Constant(V.Int(_,  v)) -> failwith "bad type in to_concrete_32"
    | _ -> raise (NotConcrete e)

  let to_concrete_64 e = match constant_fold_rec e with
    | V.Constant(V.Int(V.REG_64, v)) -> v
    | V.Constant(V.Int(_,  v)) -> failwith "bad type in to_concrete_64"
    | _ -> raise (NotConcrete e)

  let to_symbolic_1  e = e
  let to_symbolic_8  e = e
  let to_symbolic_16 e = e
  let to_symbolic_32 e = e
  let to_symbolic_64 e = e

  let from_symbolic e = e

  let inside_symbolic fn e = (fn e)

  let measure_size e = expr_size e

  let make_extract t which e =
    V.Cast(V.CAST_LOW, t, 
	   V.BinOp(V.RSHIFT, e,
		   V.Constant(V.Int(V.REG_8,
				    (Int64.mul 8L (Int64.of_int which))))))
      
  let extract_8_from_64 e which =
    match e with
      | V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr)), V.REG_64)) ->
	  let addr' = Int64.add addr (Int64.of_int which) in
	    V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr')), V.REG_8))
      | _ ->
	  match which with
	    | 0 -> V.Cast(V.CAST_LOW, V.REG_8, e)
	    | 7 -> V.Cast(V.CAST_HIGH, V.REG_8, e)
	    | _ -> make_extract V.REG_8 which e

  let extract_8_from_32 e which =
    match e with
      | V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr)), V.REG_32)) ->
	  let addr' = Int64.add addr (Int64.of_int which) in
	    V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr')), V.REG_8))
      | _ ->
	  match which with
	    | 0 -> V.Cast(V.CAST_LOW, V.REG_8, e)
	    | 3 -> V.Cast(V.CAST_HIGH, V.REG_8, e)
	    | _ -> make_extract V.REG_8 which e

  let extract_8_from_16 e which =
    match e with
      | V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr)), V.REG_16)) ->
	  let addr' = Int64.add addr (Int64.of_int which) in
	    V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr')), V.REG_8))
      | _ ->
	  match which with
	    | 0 -> V.Cast(V.CAST_LOW, V.REG_8, e)
	    | 1 -> V.Cast(V.CAST_HIGH, V.REG_8, e)
	    | _ -> failwith "bad which in extract_8_from_16"
		
  let extract_16_from_64 e which =
    match e with
      | V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr)), V.REG_64)) ->
	  let addr' = Int64.add addr (Int64.of_int which) in
	    V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr')), V.REG_16))
      | _ ->
	  match which with
	    | 0 -> V.Cast(V.CAST_LOW, V.REG_16, e)
	    | 6 -> V.Cast(V.CAST_HIGH, V.REG_16, e)
	    | _ -> make_extract V.REG_16 which e

  let extract_16_from_32 e which =
    match e with
      | V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr)), V.REG_32)) ->
	  let addr' = Int64.add addr (Int64.of_int which) in
	    V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr')), V.REG_16))
      | _ ->
	  match which with
	    | 0 -> V.Cast(V.CAST_LOW, V.REG_16, e)
	    | 2 -> V.Cast(V.CAST_HIGH, V.REG_16, e)
	    | _ -> failwith "bad which in extract_16_from_32"

  let extract_32_from_64 e which =
    match e with
      | V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr)), V.REG_64)) ->
	  let addr' = Int64.add addr (Int64.of_int which) in
	    V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr')), V.REG_32))
      | _ ->
	  match which with
	    | 0 -> V.Cast(V.CAST_LOW, V.REG_32, e)
	    | 4 -> V.Cast(V.CAST_HIGH, V.REG_32, e)
	    | _ -> failwith "bad which in extract_32_from_64"

  let assemble16 e e2 =
    V.BinOp(V.BITOR,
	    V.Cast(V.CAST_UNSIGNED, V.REG_16, e),
	    V.BinOp(V.LSHIFT,
		    V.Cast(V.CAST_UNSIGNED, V.REG_16, e2),
		    (from_concrete_8 8)))
  let assemble32 e e2 =
    V.BinOp(V.BITOR,
	    V.Cast(V.CAST_UNSIGNED, V.REG_32, e),
	    V.BinOp(V.LSHIFT,
		    V.Cast(V.CAST_UNSIGNED, V.REG_32, e2),
		    (from_concrete_8 16)))

  let assemble64 e e2 =
    V.BinOp(V.BITOR,
	    V.Cast(V.CAST_UNSIGNED, V.REG_64, e),
	    V.BinOp(V.LSHIFT,
		    V.Cast(V.CAST_UNSIGNED, V.REG_64, e2),
		    (from_concrete_8 32)))

  let reassemble16 e e2 =
    match (e, e2) with
      | (V.Constant(V.Int(V.REG_8, v1)), V.Constant(V.Int(V.REG_8, v2)))
	-> constant_fold_rec (assemble16 e e2)
      | (V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr1)), V.REG_8)),
	 V.Lval(V.Mem(v2, V.Constant(V.Int(V.REG_32, addr2)), V.REG_8)))
	  when v1 = v2 && (Int64.sub addr2 addr1) = 1L
	    ->
	  V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr1)), V.REG_16))
      | (V.Cast(V.CAST_LOW, V.REG_8, s1), (V.Cast(V.CAST_HIGH, V.REG_8, s2)))
	  when s1 = s2 && (Vine_typecheck.infer_type_fast s1) = V.REG_16
	    ->
	  s1
      | _ -> assemble16 e e2

  let reassemble32 e e2 =
    match (e, e2) with
      | (V.Constant(V.Int(V.REG_16, v1)), V.Constant(V.Int(V.REG_16, v2)))
	-> constant_fold_rec (assemble32 e e2)
      | (V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr1)), V.REG_16)),
	 V.Lval(V.Mem(v2, V.Constant(V.Int(V.REG_32, addr2)), V.REG_16)))
	  when v1 = v2 && (Int64.sub addr2 addr1) = 2L
	    ->
	  V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr1)), V.REG_32))
      | (V.Cast(V.CAST_LOW, V.REG_16, w1), (V.Cast(V.CAST_HIGH, V.REG_16, w2)))
	  when w1 = w2 && (Vine_typecheck.infer_type_fast w1) = V.REG_32
	    ->
	  w1
      | _ -> assemble32 e e2

  let reassemble64 e e2 =
    match (e, e2) with
      | (V.Constant(V.Int(V.REG_32, v1)), V.Constant(V.Int(V.REG_32, v2)))
	-> constant_fold_rec (assemble64 e e2)
      | (V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr1)), V.REG_32)),
	 V.Lval(V.Mem(v2, V.Constant(V.Int(V.REG_32, addr2)), V.REG_32)))
	  when v1 = v2 && (Int64.sub addr2 addr1) = 4L
	    ->
	  V.Lval(V.Mem(v1, V.Constant(V.Int(V.REG_32, addr1)), V.REG_64))
      | (V.Cast(V.CAST_LOW, V.REG_32, l1), (V.Cast(V.CAST_HIGH, V.REG_32, l2)))
	  when l1 = l2 && (Vine_typecheck.infer_type_fast l1) = V.REG_64
	    ->
	  l1
      | _ -> assemble64 e e2

  let to_string e = V.exp_to_string e
  let to_string_1  = to_string
  let to_string_8  = to_string
  let to_string_16 = to_string
  let to_string_32 = to_string
  let to_string_64 = to_string

  let uninit = V.Unknown("uninit")

  let binop op e e2 = V.BinOp(op, e, e2)

  let plus1  = binop V.PLUS
  let plus8  = binop V.PLUS
  let plus16 = binop V.PLUS
  let plus32 = binop V.PLUS
  let plus64 = binop V.PLUS

  let minus1  = binop V.MINUS
  let minus8  = binop V.MINUS
  let minus16 = binop V.MINUS
  let minus32 = binop V.MINUS
  let minus64 = binop V.MINUS

  let times1  = binop V.TIMES
  let times8  = binop V.TIMES
  let times16 = binop V.TIMES
  let times32 = binop V.TIMES
  let times64 = binop V.TIMES

  let binop_zero_check op e e2 =
    let e2' = constant_fold_rec e2 in
    match e2' with
      | V.Constant(V.Int(ty, zero))
	  when (ty = V.REG_64 && zero = 0L) ||
	    (ty = V.REG_32 && (fix_u32 zero) = 0L) ||
	    (ty = V.REG_16 && (fix_u16 zero) = 0L) ||
	    (ty = V.REG_8  && (fix_u8  zero) = 0L) ||
	    (ty = V.REG_1  && (fix_u1  zero) = 0L)
	    -> raise DivideByZero
      | _ -> binop op e e2'

  let divide1  = binop_zero_check V.DIVIDE
  let divide8  = binop_zero_check V.DIVIDE
  let divide16 = binop_zero_check V.DIVIDE
  let divide32 = binop_zero_check V.DIVIDE
  let divide64 = binop_zero_check V.DIVIDE

  let sdivide1  = binop_zero_check V.SDIVIDE
  let sdivide8  = binop_zero_check V.SDIVIDE
  let sdivide16 = binop_zero_check V.SDIVIDE
  let sdivide32 = binop_zero_check V.SDIVIDE
  let sdivide64 = binop_zero_check V.SDIVIDE

  let mod1  = binop_zero_check V.MOD
  let mod8  = binop_zero_check V.MOD
  let mod16 = binop_zero_check V.MOD
  let mod32 = binop_zero_check V.MOD
  let mod64 = binop_zero_check V.MOD

  let smod1  = binop_zero_check V.SMOD
  let smod8  = binop_zero_check V.SMOD
  let smod16 = binop_zero_check V.SMOD
  let smod32 = binop_zero_check V.SMOD
  let smod64 = binop_zero_check V.SMOD

  let lshift1  = binop V.LSHIFT
  let lshift8  = binop V.LSHIFT
  let lshift16 = binop V.LSHIFT
  let lshift32 = binop V.LSHIFT
  let lshift64 = binop V.LSHIFT

  let rshift1  = binop V.RSHIFT
  let rshift8  = binop V.RSHIFT
  let rshift16 = binop V.RSHIFT
  let rshift32 = binop V.RSHIFT
  let rshift64 = binop V.RSHIFT

  let arshift1  = binop V.ARSHIFT
  let arshift8  = binop V.ARSHIFT
  let arshift16 = binop V.ARSHIFT
  let arshift32 = binop V.ARSHIFT
  let arshift64 = binop V.ARSHIFT

  let bitand1  = binop V.BITAND
  let bitand8  = binop V.BITAND
  let bitand16 = binop V.BITAND
  let bitand32 = binop V.BITAND
  let bitand64 = binop V.BITAND

  let bitor1  = binop V.BITOR
  let bitor8  = binop V.BITOR
  let bitor16 = binop V.BITOR
  let bitor32 = binop V.BITOR
  let bitor64 = binop V.BITOR

  let xor1  = binop V.XOR
  let xor8  = binop V.XOR
  let xor16 = binop V.XOR
  let xor32 = binop V.XOR
  let xor64 = binop V.XOR

  let eq1  = binop V.EQ
  let eq8  = binop V.EQ
  let eq16 = binop V.EQ
  let eq32 = binop V.EQ
  let eq64 = binop V.EQ

  let neq1  = binop V.NEQ
  let neq8  = binop V.NEQ
  let neq16 = binop V.NEQ
  let neq32 = binop V.NEQ
  let neq64 = binop V.NEQ

  let lt1  = binop V.LT
  let lt8  = binop V.LT
  let lt16 = binop V.LT
  let lt32 = binop V.LT
  let lt64 = binop V.LT

  let le1  = binop V.LE
  let le8  = binop V.LE
  let le16 = binop V.LE
  let le32 = binop V.LE
  let le64 = binop V.LE

  let slt1  = binop V.SLT
  let slt8  = binop V.SLT
  let slt16 = binop V.SLT
  let slt32 = binop V.SLT
  let slt64 = binop V.SLT

  let sle1  = binop V.SLE
  let sle8  = binop V.SLE
  let sle16 = binop V.SLE
  let sle32 = binop V.SLE
  let sle64 = binop V.SLE

  let unop op e = V.UnOp(op, e)

  let neg1  = unop V.NEG
  let neg8  = unop V.NEG
  let neg16 = unop V.NEG
  let neg32 = unop V.NEG
  let neg64 = unop V.NEG

  let not1  = unop V.NOT
  let not8  = unop V.NOT
  let not16 = unop V.NOT
  let not32 = unop V.NOT
  let not64 = unop V.NOT

  let cast kind ty e = V.Cast(kind, ty, e)

  let cast1u8   = cast V.CAST_UNSIGNED V.REG_8 
  let cast1u16  = cast V.CAST_UNSIGNED V.REG_16
  let cast1u32  = cast V.CAST_UNSIGNED V.REG_32
  let cast1u64  = cast V.CAST_UNSIGNED V.REG_64
  let cast8u16  = cast V.CAST_UNSIGNED V.REG_16
  let cast8u32  = cast V.CAST_UNSIGNED V.REG_32
  let cast8u64  = cast V.CAST_UNSIGNED V.REG_64
  let cast16u32 = cast V.CAST_UNSIGNED V.REG_32
  let cast16u64 = cast V.CAST_UNSIGNED V.REG_64
  let cast32u64 = cast V.CAST_UNSIGNED V.REG_64

  let cast1s8   = cast V.CAST_SIGNED V.REG_8
  let cast1s16  = cast V.CAST_SIGNED V.REG_16
  let cast1s32  = cast V.CAST_SIGNED V.REG_32
  let cast1s64  = cast V.CAST_SIGNED V.REG_64
  let cast8s16  = cast V.CAST_SIGNED V.REG_16
  let cast8s32  = cast V.CAST_SIGNED V.REG_32
  let cast8s64  = cast V.CAST_SIGNED V.REG_64
  let cast16s32 = cast V.CAST_SIGNED V.REG_32
  let cast16s64 = cast V.CAST_SIGNED V.REG_64
  let cast32s64 = cast V.CAST_SIGNED V.REG_64

  let cast8l1   = cast V.CAST_LOW V.REG_1 
  let cast16l1  = cast V.CAST_LOW V.REG_1 
  let cast32l1  = cast V.CAST_LOW V.REG_1 
  let cast64l1  = cast V.CAST_LOW V.REG_1 
  let cast16l8  = cast V.CAST_LOW V.REG_8 
  let cast32l8  = cast V.CAST_LOW V.REG_8 
  let cast64l8  = cast V.CAST_LOW V.REG_8 
  let cast32l16 = cast V.CAST_LOW V.REG_16
  let cast64l16 = cast V.CAST_LOW V.REG_16
  let cast64l32 = cast V.CAST_LOW V.REG_32

  let cast8h1   = cast V.CAST_HIGH V.REG_1 
  let cast16h1  = cast V.CAST_HIGH V.REG_1 
  let cast32h1  = cast V.CAST_HIGH V.REG_1 
  let cast64h1  = cast V.CAST_HIGH V.REG_1 
  let cast16h8  = cast V.CAST_HIGH V.REG_8 
  let cast32h8  = cast V.CAST_HIGH V.REG_8 
  let cast64h8  = cast V.CAST_HIGH V.REG_8 
  let cast32h16 = cast V.CAST_HIGH V.REG_16
  let cast64h16 = cast V.CAST_HIGH V.REG_16
  let cast64h32 = cast V.CAST_HIGH V.REG_32

  let ite cond t f = V.Ite(cond, t, f)

  let ite1  = ite
  let ite8  = ite
  let ite16 = ite
  let ite32 = ite
  let ite64 = ite

  let fbinop op rm v1 v2 = V.FBinOp(op, rm, v1, v2)

  let fplus32 = fbinop V.FPLUS
  let fplus64 = fbinop V.FPLUS

  let fminus32 = fbinop V.FMINUS
  let fminus64 = fbinop V.FMINUS

  let ftimes32 = fbinop V.FTIMES
  let ftimes64 = fbinop V.FTIMES

  let fdivide32 = fbinop V.FDIVIDE
  let fdivide64 = fbinop V.FDIVIDE

  let feq32 = fbinop V.FEQ
  let feq64 = fbinop V.FEQ

  let fneq32 = fbinop V.FNEQ
  let fneq64 = fbinop V.FNEQ

  let flt32 = fbinop V.FLT
  let flt64 = fbinop V.FLT

  let fle32 = fbinop V.FLE
  let fle64 = fbinop V.FLE

  let funop op rm v1 = V.FUnOp(op, rm, v1)

  let fneg32 = funop V.FNEG
  let fneg64 = funop V.FNEG

  let fcast op ty rm v = V.FCast(op, rm, ty, v)

  let float1s32  = fcast V.CAST_SFLOAT V.REG_32
  let float8s32  = fcast V.CAST_SFLOAT V.REG_32
  let float16s32 = fcast V.CAST_SFLOAT V.REG_32
  let float32s32 = fcast V.CAST_SFLOAT V.REG_32
  let float64s32 = fcast V.CAST_SFLOAT V.REG_32
  let float1s64  = fcast V.CAST_SFLOAT V.REG_64
  let float8s64  = fcast V.CAST_SFLOAT V.REG_64
  let float16s64 = fcast V.CAST_SFLOAT V.REG_64
  let float32s64 = fcast V.CAST_SFLOAT V.REG_64
  let float64s64 = fcast V.CAST_SFLOAT V.REG_64

  let float1u32  = fcast V.CAST_UFLOAT V.REG_32
  let float8u32  = fcast V.CAST_UFLOAT V.REG_32
  let float16u32 = fcast V.CAST_UFLOAT V.REG_32
  let float32u32 = fcast V.CAST_UFLOAT V.REG_32
  let float64u32 = fcast V.CAST_UFLOAT V.REG_32
  let float1u64  = fcast V.CAST_UFLOAT V.REG_64
  let float8u64  = fcast V.CAST_UFLOAT V.REG_64
  let float16u64 = fcast V.CAST_UFLOAT V.REG_64
  let float32u64 = fcast V.CAST_UFLOAT V.REG_64
  let float64u64 = fcast V.CAST_UFLOAT V.REG_64

  let fix32s1  = fcast V.CAST_SFIX V.REG_1
  let fix32s8  = fcast V.CAST_SFIX V.REG_8
  let fix32s16 = fcast V.CAST_SFIX V.REG_16
  let fix32s32 = fcast V.CAST_SFIX V.REG_32
  let fix32s64 = fcast V.CAST_SFIX V.REG_64
  let fix64s1  = fcast V.CAST_SFIX V.REG_1
  let fix64s8  = fcast V.CAST_SFIX V.REG_8
  let fix64s16 = fcast V.CAST_SFIX V.REG_16
  let fix64s32 = fcast V.CAST_SFIX V.REG_32
  let fix64s64 = fcast V.CAST_SFIX V.REG_64

  let fix32u1  = fcast V.CAST_UFIX V.REG_1
  let fix32u8  = fcast V.CAST_UFIX V.REG_8
  let fix32u16 = fcast V.CAST_UFIX V.REG_16
  let fix32u32 = fcast V.CAST_UFIX V.REG_32
  let fix32u64 = fcast V.CAST_UFIX V.REG_64
  let fix64u1  = fcast V.CAST_UFIX V.REG_1
  let fix64u8  = fcast V.CAST_UFIX V.REG_8
  let fix64u16 = fcast V.CAST_UFIX V.REG_16
  let fix64u32 = fcast V.CAST_UFIX V.REG_32
  let fix64u64 = fcast V.CAST_UFIX V.REG_64

  let fwiden32to64  = fcast V.CAST_FWIDEN  V.REG_64
  let fnarrow64to32 = fcast V.CAST_FNARROW V.REG_32

  let get_tag v = 0L
end

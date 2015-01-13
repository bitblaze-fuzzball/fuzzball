(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module V = Vine

open Exec_exceptions
open Exec_utils

module ConcreteDomain : Exec_domain.DOMAIN = struct
  type t = int64

  let from_concrete_1  = Int64.of_int
  let from_concrete_8  = Int64.of_int
  let from_concrete_16 = Int64.of_int
  let from_concrete_32 v' = v' 
  let from_concrete_64 v' = v'

  let to_concrete_1  v = Int64.to_int (Int64.logand v 0x1L)
  let to_concrete_8  v = Int64.to_int (Int64.logand v 0xffL)
  let to_concrete_16 v = Int64.to_int (Int64.logand v 0xffffL)
  let to_concrete_32 v = Int64.logand v 0xffffffffL
  let to_concrete_64 v = v

  let to_symbolic_1  v : V.exp = failwith "to_symbolic in concrete"
  let to_symbolic_8  v : V.exp = failwith "to_symbolic in concrete"
  let to_symbolic_16 v : V.exp = failwith "to_symbolic in concrete"
  let to_symbolic_32 v : V.exp = failwith "to_symbolic in concrete"
  let to_symbolic_64 v : V.exp = failwith "to_symbolic in concrete"

  let from_symbolic e = failwith "from_symbolic in concrete"

  let inside_symbolic fn v = v

  let measure_size v = 1

  let  extract_8_from_64 v which = Int64.shift_right v (8 * which)
  let  extract_8_from_32 v which = Int64.shift_right v (8 * which)
  let  extract_8_from_16 v which = Int64.shift_right v (8 * which)
  let extract_16_from_64 v which = Int64.shift_right v (8 * which)
  let extract_16_from_32 v which = Int64.shift_right v (8 * which)
  let extract_32_from_64 v which = Int64.shift_right v (8 * which)

  let assemble16 v v2 =
    Int64.logor (Int64.logand 0xffL v) (Int64.shift_left v2 8)

  let assemble32 v v2 =
    Int64.logor (Int64.logand 0xffffL v) (Int64.shift_left v2 16)

  let assemble64 v v2 =
    Int64.logor (Int64.logand 0xffffffffL v) (Int64.shift_left v2 32)

  let reassemble16 = assemble16
  let reassemble32 = assemble32
  let reassemble64 = assemble64

  let to_string_1  v = Printf.sprintf "%d"       (to_concrete_1  v)
  let to_string_8  v = Printf.sprintf "0x%02x"   (to_concrete_8  v)
  let to_string_16 v = Printf.sprintf "0x%04x"   (to_concrete_16 v)
  let to_string_32 v = Printf.sprintf "0x%08Lx"  (to_concrete_32 v)
  let to_string_64 v = Printf.sprintf "0x%016Lx" (to_concrete_64 v)

  let uninit = 0L

  let plus1  = Int64.add
  let plus8  = Int64.add
  let plus16 = Int64.add
  let plus32 = Int64.add
  let plus64 = Int64.add

  let minus1  = Int64.sub
  let minus8  = Int64.sub
  let minus16 = Int64.sub
  let minus32 = Int64.sub
  let minus64 = Int64.sub

  let times1  = Int64.mul
  let times8  = Int64.mul
  let times16 = Int64.mul
  let times32 = Int64.mul
  let times64 = Int64.mul

  let divide1  v v2 = 
    let d = fix_u1 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_udiv (fix_u1  v) d
  let divide8  v v2 =
    let d = fix_u8 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_udiv (fix_u8  v) d
  let divide16 v v2 =
    let d = fix_u16 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_udiv (fix_u16 v) d
  let divide32 v v2 =
    let d = fix_u32 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_udiv (fix_u32 v) d
  let divide64 v v2 =
    if v2 = 0L then raise DivideByZero;
    Vine_util.int64_udiv v v2

  let sdivide1  v v2 =
    let d = fix_s1  v2 in
      if d = 0L then raise DivideByZero;
      Int64.div (fix_s1  v) d
  let sdivide8  v v2 =
    let d = fix_s8  v2 in
      if d = 0L then raise DivideByZero;
      Int64.div (fix_s8  v)d
  let sdivide16 v v2 =
    let d = fix_s16 v2 in
      if d = 0L then raise DivideByZero;
      Int64.div (fix_s16 v) d
  let sdivide32 v v2 =
    let d = fix_s32 v2 in
      if d = 0L then raise DivideByZero;
      Int64.div (fix_s32 v) d
  let sdivide64 v v2 =
    if v2 = 0L then raise DivideByZero;
    Int64.div v v2 

  let mod1  v v2 =
    let d = fix_u1 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_urem (fix_u1  v) d
  let mod8  v v2 =
    let d = fix_u8 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_urem (fix_u8  v) d
  let mod16 v v2 =
    let d = fix_u16 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_urem (fix_u16 v) d
  let mod32 v v2 =
    let d = fix_u32 v2 in
      if d = 0L then raise DivideByZero;
      Vine_util.int64_urem (fix_u32 v) d
  let mod64 v v2 =
    if v2 = 0L then raise DivideByZero;
    Vine_util.int64_urem v v2 

  let smod1  v v2 =
    let d = fix_s1  v2 in
      if d = 0L then raise DivideByZero;
      Int64.rem (fix_s1  v) d
  let smod8  v v2 =
    let d = fix_s8  v2 in
      if d = 0L then raise DivideByZero;
    Int64.rem (fix_s8  v) d
  let smod16 v v2 =
    let d = fix_s16 v2 in
      if d = 0L then raise DivideByZero;
    Int64.rem (fix_s16 v) d
  let smod32 v v2 =
    let d = fix_s32 v2 in
      if d = 0L then raise DivideByZero;
    Int64.rem (fix_s32 v) d
  let smod64 v v2 =
    if v2 = 0L then raise DivideByZero;
    Int64.rem v v2 

  let lshift1  v v2 = Int64.shift_left v (Int64.to_int (fix_u8 v2))
  let lshift8  v v2 = Int64.shift_left v (Int64.to_int (fix_u8 v2))
  let lshift16 v v2 = Int64.shift_left v (Int64.to_int (fix_u8 v2))
  let lshift32 v v2 = Int64.shift_left v (Int64.to_int (fix_u8 v2))
  let lshift64 v v2 = Int64.shift_left v (Int64.to_int (fix_u8 v2))

  let rshift1  v v2 = Int64.shift_right_logical (fix_u1  v) 
    (Int64.to_int (fix_u8 v2))
  let rshift8  v v2 = Int64.shift_right_logical (fix_u8  v)
    (Int64.to_int (fix_u8 v2))
  let rshift16 v v2 = Int64.shift_right_logical (fix_u16 v)
    (Int64.to_int (fix_u8 v2))
  let rshift32 v v2 = Int64.shift_right_logical (fix_u32 v)
    (Int64.to_int (fix_u8 v2))
  let rshift64 v v2 = Int64.shift_right_logical          v 
    (Int64.to_int (fix_u8 v2))

  let arshift1  v v2 = Int64.shift_right (fix_s1 v)  (Int64.to_int (fix_u8 v2))
  let arshift8  v v2 = Int64.shift_right (fix_s8 v)  (Int64.to_int (fix_u8 v2))
  let arshift16 v v2 = Int64.shift_right (fix_s16 v) (Int64.to_int (fix_u8 v2))
  let arshift32 v v2 = Int64.shift_right (fix_s32 v) (Int64.to_int (fix_u8 v2))
  let arshift64 v v2 = Int64.shift_right          v  (Int64.to_int (fix_u8 v2))

  let bitand1  = Int64.logand
  let bitand8  = Int64.logand
  let bitand16 = Int64.logand
  let bitand32 = Int64.logand
  let bitand64 = Int64.logand

  let bitor1  = Int64.logor
  let bitor8  = Int64.logor
  let bitor16 = Int64.logor
  let bitor32 = Int64.logor
  let bitor64 = Int64.logor

  let xor1  = Int64.logxor
  let xor8  = Int64.logxor
  let xor16 = Int64.logxor
  let xor32 = Int64.logxor
  let xor64 = Int64.logxor

  let bool b = if b then 1L else 0L

  let eq1  v v2 = bool ((fix_u1  v) = (fix_u1  v2))
  let eq8  v v2 = bool ((fix_u8  v) = (fix_u8  v2))
  let eq16 v v2 = bool ((fix_u16 v) = (fix_u16 v2))
  let eq32 v v2 = bool ((fix_u32 v) = (fix_u32 v2))
  let eq64 v v2 = bool ((        v) = (        v2))

  let neq1  v v2 = bool ((fix_u1  v) <> (fix_u1  v2))
  let neq8  v v2 = bool ((fix_u8  v) <> (fix_u8  v2))
  let neq16 v v2 = bool ((fix_u16 v) <> (fix_u16 v2))
  let neq32 v v2 = bool ((fix_u32 v) <> (fix_u32 v2))
  let neq64 v v2 = bool ((        v) <> (        v2))

  let lt1  v v2 = bool ((Vine_util.int64_ucompare (fix_u1  v) (fix_u1  v2))< 0)
  let lt8  v v2 = bool ((Vine_util.int64_ucompare (fix_u8  v) (fix_u8  v2))< 0)
  let lt16 v v2 = bool ((Vine_util.int64_ucompare (fix_u16 v) (fix_u16 v2))< 0)
  let lt32 v v2 = bool ((Vine_util.int64_ucompare (fix_u32 v) (fix_u32 v2))< 0)
  let lt64 v v2 = bool ((Vine_util.int64_ucompare (        v) (        v2))< 0)

  let le1  v v2 = bool ((Vine_util.int64_ucompare (fix_u1  v) (fix_u1  v2))<=0)
  let le8  v v2 = bool ((Vine_util.int64_ucompare (fix_u8  v) (fix_u8  v2))<=0)
  let le16 v v2 = bool ((Vine_util.int64_ucompare (fix_u16 v) (fix_u16 v2))<=0)
  let le32 v v2 = bool ((Vine_util.int64_ucompare (fix_u32 v) (fix_u32 v2))<=0)
  let le64 v v2 = bool ((Vine_util.int64_ucompare (        v) (        v2))<=0)

  let slt1  v v2 = bool ((fix_s1  v) < (fix_s1  v2))
  let slt8  v v2 = bool ((fix_s8  v) < (fix_s8  v2))
  let slt16 v v2 = bool ((fix_s16 v) < (fix_s16 v2))
  let slt32 v v2 = bool ((fix_s32 v) < (fix_s32 v2))
  let slt64 v v2 = bool ((        v) < (        v2))

  let sle1  v v2 = bool ((fix_s1  v) <= (fix_s1  v2))
  let sle8  v v2 = bool ((fix_s8  v) <= (fix_s8  v2))
  let sle16 v v2 = bool ((fix_s16 v) <= (fix_s16 v2))
  let sle32 v v2 = bool ((fix_s32 v) <= (fix_s32 v2))
  let sle64 v v2 = bool ((        v) <= (        v2))

  let neg1  = Int64.neg
  let neg8  = Int64.neg
  let neg16 = Int64.neg
  let neg32 = Int64.neg
  let neg64 = Int64.neg

  let not1  = Int64.lognot
  let not8  = Int64.lognot
  let not16 = Int64.lognot
  let not32 = Int64.lognot
  let not64 = Int64.lognot

  let cast1u8   = fix_u1
  let cast1u16  = fix_u1
  let cast1u32  = fix_u1
  let cast1u64  = fix_u1
  let cast8u16  = fix_u8
  let cast8u32  = fix_u8
  let cast8u64  = fix_u8
  let cast16u32 = fix_u16
  let cast16u64 = fix_u16
  let cast32u64 = fix_u32

  let cast1s8   = fix_s1
  let cast1s16  = fix_s1
  let cast1s32  = fix_s1
  let cast1s64  = fix_s1
  let cast8s16  = fix_s8
  let cast8s32  = fix_s8
  let cast8s64  = fix_s8
  let cast16s32 = fix_s16
  let cast16s64 = fix_s16
  let cast32s64 = fix_s32

  let cast8l1   v = v
  let cast16l1  v = v
  let cast32l1  v = v
  let cast64l1  v = v
  let cast16l8  v = v
  let cast32l8  v = v
  let cast64l8  v = v
  let cast32l16 v = v
  let cast64l16 v = v
  let cast64l32 v = v

  let cast_high amt v = Int64.shift_right v amt

  let cast8h1   = cast_high  7
  let cast16h1  = cast_high 15
  let cast32h1  = cast_high 31
  let cast64h1  = cast_high 63
  let cast16h8  = cast_high  8
  let cast32h8  = cast_high 24
  let cast64h8  = cast_high 56
  let cast32h16 = cast_high 16
  let cast64h16 = cast_high 48
  let cast64h32 = cast_high 32

  let as_bool v = (fix_u1 v) <> 0L

  let ite1  cond t f = if as_bool cond then t else f
  let ite8  cond t f = if as_bool cond then t else f
  let ite16 cond t f = if as_bool cond then t else f
  let ite32 cond t f = if as_bool cond then t else f
  let ite64 cond t f = if as_bool cond then t else f

  let fbinop32 op =
    fun rm x y ->
      let x32 = Int64.to_int32 x and
	  y32 = Int64.to_int32 y in
	Int64.of_int32 (op rm x32 y32)

  let fplus32 = fbinop32 Vine_util.f32_add
  let fplus64 = Vine_util.f64_add

  let fminus32 = fbinop32 Vine_util.f32_sub
  let fminus64 = Vine_util.f64_sub

  let ftimes32 = fbinop32 Vine_util.f32_mul
  let ftimes64 = Vine_util.f64_mul

  let fdivide32 = fbinop32 Vine_util.f32_div
  let fdivide64 = Vine_util.f64_div

  let fbinpred32 op =
    fun rm x y ->
      let x32 = Int64.to_int32 x and
	  y32 = Int64.to_int32 y in
	bool (op rm x32 y32)

  let fbinpred64 op =
    fun rm x y -> bool (op rm x y)

  let feq32 = fbinpred32 Vine_util.f32_eq
  let feq64 = fbinpred64 Vine_util.f64_eq

  let fneq32 = fbinpred32 Vine_util.f32_ne
  let fneq64 = fbinpred64 Vine_util.f64_ne

  let flt32 = fbinpred32 Vine_util.f32_lt
  let flt64 = fbinpred64 Vine_util.f64_lt

  let fle32 = fbinpred32 Vine_util.f32_le
  let fle64 = fbinpred64 Vine_util.f64_le

  let funop32 op =
    fun rm x ->
      let x32 = Int64.to_int32 x in
	Int64.of_int32 (op rm x32)

  let fneg32 = funop32 Vine_util.f32_neg
  let fneg64 = Vine_util.f64_neg

  let fixed x = x

  let float_s64 = Int64.to_float
  let float_u64 = Vine_util.int64_u_to_float

  let enc_float32 f = Int64.of_int32 (Int32.bits_of_float f)
  let enc_float64 f = Int64.bits_of_float f

  let do_float fixer floater encoder =
    fun rm v ->
      ignore(rm);
      encoder (floater (fixer v))

  let float1s32  = do_float fix_s1  float_s64 enc_float32
  let float8s32  = do_float fix_s8  float_s64 enc_float32
  let float16s32 = do_float fix_s16 float_s64 enc_float32
  let float32s32 = do_float fix_s32 float_s64 enc_float32
  let float64s32 = do_float fixed   float_s64 enc_float32
  let float1s64  = do_float fix_s1  float_s64 enc_float64
  let float8s64  = do_float fix_s8  float_s64 enc_float64
  let float16s64 = do_float fix_s16 float_s64 enc_float64
  let float32s64 = do_float fix_s32 float_s64 enc_float64
  let float64s64 = do_float fixed   float_s64 enc_float64

  let float1u32  = do_float fix_u1  float_s64 enc_float32
  let float8u32  = do_float fix_u8  float_s64 enc_float32
  let float16u32 = do_float fix_u16 float_s64 enc_float32
  let float32u32 = do_float fix_u32 float_s64 enc_float32
  let float64u32 = do_float fixed   float_u64 enc_float32
  let float1u64  = do_float fix_u1  float_s64 enc_float64
  let float8u64  = do_float fix_u8  float_s64 enc_float64
  let float16u64 = do_float fix_u16 float_s64 enc_float64
  let float32u64 = do_float fix_u32 float_s64 enc_float64
  let float64u64 = do_float fixed   float_u64 enc_float64

  let dec_float32 v = Int32.float_of_bits (Int64.to_int32 v)
  let dec_float64 v = Int64.float_of_bits v

  let unfloat_s64 = Int64.of_float
  let unfloat_u64 = Vine_util.int64_u_of_float

  let do_fix decoder unfloater fixer =
    fun rm v ->
      ignore(rm);
      fixer (unfloater (decoder v))

  let fix32s1  = do_fix dec_float32 unfloat_s64 fix_s1
  let fix32s8  = do_fix dec_float32 unfloat_s64 fix_s8
  let fix32s16 = do_fix dec_float32 unfloat_s64 fix_s16
  let fix32s32 = do_fix dec_float32 unfloat_s64 fix_s32
  let fix32s64 = do_fix dec_float32 unfloat_s64 fixed
  let fix64s1  = do_fix dec_float64 unfloat_s64 fix_s1
  let fix64s8  = do_fix dec_float64 unfloat_s64 fix_s8
  let fix64s16 = do_fix dec_float64 unfloat_s64 fix_s16
  let fix64s32 = do_fix dec_float64 unfloat_s64 fix_s32
  let fix64s64 = do_fix dec_float64 unfloat_s64 fixed

  let fix32u1  = do_fix dec_float32 unfloat_s64 fix_u1
  let fix32u8  = do_fix dec_float32 unfloat_s64 fix_u8
  let fix32u16 = do_fix dec_float32 unfloat_s64 fix_u16
  let fix32u32 = do_fix dec_float32 unfloat_s64 fix_u32
  let fix32u64 = do_fix dec_float32 unfloat_u64 fixed
  let fix64u1  = do_fix dec_float64 unfloat_s64 fix_u1
  let fix64u8  = do_fix dec_float64 unfloat_s64 fix_u8
  let fix64u16 = do_fix dec_float64 unfloat_s64 fix_u16
  let fix64u32 = do_fix dec_float64 unfloat_s64 fix_u32
  let fix64u64 = do_fix dec_float64 unfloat_u64 fixed

  let fwiden32to64  rm v = ignore(rm); enc_float64 (dec_float32 v)
  let fnarrow64to32 rm v = ignore(rm); enc_float32 (dec_float64 v)

  let get_tag v = 0L
end

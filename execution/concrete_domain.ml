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

  let get_tag v = 0L
end

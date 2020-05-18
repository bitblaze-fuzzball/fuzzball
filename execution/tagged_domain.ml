(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module V = Vine;;
open Exec_domain;;

module TaggedDomainFunctor =
  functor (D : DOMAIN) ->
struct
  type t = int64 * D.t

  let next_tag = ref 0L

  let fresh_tag () = 
    let r = !next_tag in
      next_tag := Int64.succ !next_tag;
      r

  let unary_tag tg = tg

  let binary_tag tg tg2 =
    let tg' = fresh_tag () in
      (* Printf.printf "%Ld + %Ld -> %Ld\n" tg tg2 tg'; *)
      tg'

  let from_concrete_1  i = (fresh_tag (), D.from_concrete_1  i)
  let from_concrete_8  i = (fresh_tag (), D.from_concrete_8  i)
  let from_concrete_16 i = (fresh_tag (), D.from_concrete_16 i)
  let from_concrete_32 i = (fresh_tag (), D.from_concrete_32 i)
  let from_concrete_64 i = (fresh_tag (), D.from_concrete_64 i)

  let to_concrete_1  (tg,v) = D.to_concrete_1  v
  let to_concrete_8  (tg,v) = D.to_concrete_8  v
  let to_concrete_16 (tg,v) = D.to_concrete_16 v
  let to_concrete_32 (tg,v) = D.to_concrete_32 v
  let to_concrete_64 (tg,v) = D.to_concrete_64 v

  let to_symbolic_1  (tg,v) = D.to_symbolic_1  v
  let to_symbolic_8  (tg,v) = D.to_symbolic_8  v
  let to_symbolic_16 (tg,v) = D.to_symbolic_16 v
  let to_symbolic_32 (tg,v) = D.to_symbolic_32 v
  let to_symbolic_64 (tg,v) = D.to_symbolic_64 v

  let from_symbolic e = (fresh_tag (), D.from_symbolic e)

  let inside_symbolic fn (tg,v) = (tg, D.inside_symbolic fn v)

  let measure_size (tg,v) = 1 + D.measure_size v

  let  extract_8_from_64 (tg,v) w = (unary_tag tg,  D.extract_8_from_64 v w)
  let  extract_8_from_32 (tg,v) w = (unary_tag tg,  D.extract_8_from_32 v w)
  let  extract_8_from_16 (tg,v) w = (unary_tag tg,  D.extract_8_from_16 v w)
  let extract_16_from_64 (tg,v) w = (unary_tag tg, D.extract_16_from_64 v w)
  let extract_16_from_32 (tg,v) w = (unary_tag tg, D.extract_16_from_32 v w)
  let extract_32_from_64 (tg,v) w = (unary_tag tg, D.extract_32_from_64 v w)

  let assemble16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.assemble16 v v2)
  let assemble32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.assemble32 v v2)
  let assemble64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.assemble64 v v2)

  let reassemble16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.reassemble16 v v2)
  let reassemble32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.reassemble32 v v2)
  let reassemble64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.reassemble64 v v2)

  let to_string_1  (tg,v) = D.to_string_1  v
  let to_string_8  (tg,v) = D.to_string_8  v
  let to_string_16 (tg,v) = D.to_string_16 v
  let to_string_32 (tg,v) = D.to_string_32 v
  let to_string_64 (tg,v) = D.to_string_64 v

  let uninit = (fresh_tag (), D.uninit)

  let plus1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.plus1  v v2)
  let plus8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.plus8  v v2)
  let plus16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.plus16 v v2)
  let plus32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.plus32 v v2)
  let plus64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.plus64 v v2)

  let minus1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.minus1  v v2)
  let minus8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.minus8  v v2)
  let minus16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.minus16 v v2)
  let minus32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.minus32 v v2)
  let minus64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.minus64 v v2)

  let times1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.times1  v v2)
  let times8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.times8  v v2)
  let times16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.times16 v v2)
  let times32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.times32 v v2)
  let times64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.times64 v v2)

  let divide1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.divide1  v v2)
  let divide8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.divide8  v v2)
  let divide16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.divide16 v v2)
  let divide32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.divide32 v v2)
  let divide64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.divide64 v v2)

  let sdivide1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sdivide1  v v2)
  let sdivide8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sdivide8  v v2)
  let sdivide16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sdivide16 v v2)
  let sdivide32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sdivide32 v v2)
  let sdivide64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sdivide64 v v2)

  let mod1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.mod1  v v2)
  let mod8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.mod8  v v2)
  let mod16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.mod16 v v2)
  let mod32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.mod32 v v2)
  let mod64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.mod64 v v2)

  let smod1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.smod1  v v2)
  let smod8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.smod8  v v2)
  let smod16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.smod16 v v2)
  let smod32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.smod32 v v2)
  let smod64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.smod64 v v2)

  let lshift1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lshift1  v v2)
  let lshift8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lshift8  v v2)
  let lshift16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lshift16 v v2)
  let lshift32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lshift32 v v2)
  let lshift64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lshift64 v v2)

  let rshift1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.rshift1  v v2)
  let rshift8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.rshift8  v v2)
  let rshift16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.rshift16 v v2)
  let rshift32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.rshift32 v v2)
  let rshift64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.rshift64 v v2)

  let arshift1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.arshift1  v v2)
  let arshift8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.arshift8  v v2)
  let arshift16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.arshift16 v v2)
  let arshift32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.arshift32 v v2)
  let arshift64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.arshift64 v v2)

  let bitand1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitand1  v v2)
  let bitand8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitand8  v v2)
  let bitand16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitand16 v v2)
  let bitand32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitand32 v v2)
  let bitand64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitand64 v v2)

  let bitor1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitor1  v v2)
  let bitor8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitor8  v v2)
  let bitor16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitor16 v v2)
  let bitor32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitor32 v v2)
  let bitor64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.bitor64 v v2)

  let xor1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.xor1  v v2)
  let xor8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.xor8  v v2)
  let xor16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.xor16 v v2)
  let xor32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.xor32 v v2)
  let xor64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.xor64 v v2)

  let eq1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.eq1  v v2)
  let eq8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.eq8  v v2)
  let eq16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.eq16 v v2)
  let eq32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.eq32 v v2)
  let eq64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.eq64 v v2)

  let neq1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.neq1  v v2)
  let neq8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.neq8  v v2)
  let neq16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.neq16 v v2)
  let neq32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.neq32 v v2)
  let neq64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.neq64 v v2)

  let lt1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lt1  v v2)
  let lt8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lt8  v v2)
  let lt16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lt16 v v2)
  let lt32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lt32 v v2)
  let lt64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.lt64 v v2)

  let le1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.le1  v v2)
  let le8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.le8  v v2)
  let le16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.le16 v v2)
  let le32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.le32 v v2)
  let le64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.le64 v v2)

  let slt1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.slt1  v v2)
  let slt8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.slt8  v v2)
  let slt16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.slt16 v v2)
  let slt32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.slt32 v v2)
  let slt64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.slt64 v v2)

  let sle1  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sle1  v v2)
  let sle8  (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sle8  v v2)
  let sle16 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sle16 v v2)
  let sle32 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sle32 v v2)
  let sle64 (tg,v) (tg2,v2) = (binary_tag tg tg2, D.sle64 v v2)

  let neg1  (tg,v) = (unary_tag tg, D.neg1  v)
  let neg8  (tg,v) = (unary_tag tg, D.neg8  v)
  let neg16 (tg,v) = (unary_tag tg, D.neg16 v)
  let neg32 (tg,v) = (unary_tag tg, D.neg32 v)
  let neg64 (tg,v) = (unary_tag tg, D.neg64 v)

  let not1  (tg,v) = (unary_tag tg, D.not1  v)
  let not8  (tg,v) = (unary_tag tg, D.not8  v)
  let not16 (tg,v) = (unary_tag tg, D.not16 v)
  let not32 (tg,v) = (unary_tag tg, D.not32 v)
  let not64 (tg,v) = (unary_tag tg, D.not64 v)

  let   cast1u8 (tg,v) = (unary_tag tg, D.cast1u8   v)
  let  cast1u16 (tg,v) = (unary_tag tg, D.cast1u16  v)
  let  cast1u32 (tg,v) = (unary_tag tg, D.cast1u32  v)
  let  cast1u64 (tg,v) = (unary_tag tg, D.cast1u64  v)
  let  cast8u16 (tg,v) = (unary_tag tg, D.cast8u16  v)
  let  cast8u32 (tg,v) = (unary_tag tg, D.cast8u32  v)
  let  cast8u64 (tg,v) = (unary_tag tg, D.cast8u64  v)
  let cast16u32 (tg,v) = (unary_tag tg, D.cast16u32 v)
  let cast16u64 (tg,v) = (unary_tag tg, D.cast16u64 v)
  let cast32u64 (tg,v) = (unary_tag tg, D.cast32u64 v)

  let   cast1s8 (tg,v) = (unary_tag tg, D.cast1s8   v)
  let  cast1s16 (tg,v) = (unary_tag tg, D.cast1s16  v)
  let  cast1s32 (tg,v) = (unary_tag tg, D.cast1s32  v)
  let  cast1s64 (tg,v) = (unary_tag tg, D.cast1s64  v)
  let  cast8s16 (tg,v) = (unary_tag tg, D.cast8s16  v)
  let  cast8s32 (tg,v) = (unary_tag tg, D.cast8s32  v)
  let  cast8s64 (tg,v) = (unary_tag tg, D.cast8s64  v)
  let cast16s32 (tg,v) = (unary_tag tg, D.cast16s32 v)
  let cast16s64 (tg,v) = (unary_tag tg, D.cast16s64 v)
  let cast32s64 (tg,v) = (unary_tag tg, D.cast32s64 v)

  let   cast8l1 (tg,v) = (unary_tag tg, D.cast8l1   v)
  let  cast16l1 (tg,v) = (unary_tag tg, D.cast16l1  v)
  let  cast32l1 (tg,v) = (unary_tag tg, D.cast32l1  v)
  let  cast64l1 (tg,v) = (unary_tag tg, D.cast64l1  v)
  let  cast16l8 (tg,v) = (unary_tag tg, D.cast16l8  v)
  let  cast32l8 (tg,v) = (unary_tag tg, D.cast32l8  v)
  let  cast64l8 (tg,v) = (unary_tag tg, D.cast64l8  v)
  let cast32l16 (tg,v) = (unary_tag tg, D.cast32l16 v)
  let cast64l16 (tg,v) = (unary_tag tg, D.cast64l16 v)
  let cast64l32 (tg,v) = (unary_tag tg, D.cast64l32 v)

  let   cast8h1 (tg,v) = (unary_tag tg, D.cast8h1   v)
  let  cast16h1 (tg,v) = (unary_tag tg, D.cast16h1  v)
  let  cast32h1 (tg,v) = (unary_tag tg, D.cast32h1  v)
  let  cast64h1 (tg,v) = (unary_tag tg, D.cast64h1  v)
  let  cast16h8 (tg,v) = (unary_tag tg, D.cast16h8  v)
  let  cast32h8 (tg,v) = (unary_tag tg, D.cast32h8  v)
  let  cast64h8 (tg,v) = (unary_tag tg, D.cast64h8  v)
  let cast32h16 (tg,v) = (unary_tag tg, D.cast32h16 v)
  let cast64h16 (tg,v) = (unary_tag tg, D.cast64h16 v)
  let cast64h32 (tg,v) = (unary_tag tg, D.cast64h32 v)

  let ite op (cond_tg, cond_v) (t_tg, t_v) (f_tg, f_v) =
    ((binary_tag t_tg f_tg), (op cond_v t_v f_v))
  let ite1  = ite D.ite1
  let ite8  = ite D.ite8
  let ite16 = ite D.ite16
  let ite32 = ite D.ite32
  let ite64 = ite D.ite64

  let fplus32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fplus32 rm v v2)
  let fplus64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fplus64 rm v v2)

  let fminus32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fminus32 rm v v2)
  let fminus64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fminus64 rm v v2)

  let ftimes32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.ftimes32 rm v v2)
  let ftimes64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.ftimes64 rm v v2)

  let fdivide32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fdivide32 rm v v2)
  let fdivide64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fdivide64 rm v v2)

  let feq32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.feq32 rm v v2)
  let feq64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.feq64 rm v v2)

  let fneq32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fneq32 rm v v2)
  let fneq64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fneq64 rm v v2)

  let flt32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.flt32 rm v v2)
  let flt64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.flt64 rm v v2)

  let fle32 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fle32 rm v v2)
  let fle64 rm (tg,v) (tg2,v2) = (binary_tag tg tg2, D.fle64 rm v v2)

  let fneg32 rm (tg,v) = (unary_tag tg, D.fneg32 rm v)
  let fneg64 rm (tg,v) = (unary_tag tg, D.fneg64 rm v)

  let float1s32  rm (tg,v) = (unary_tag tg, D.float1s32  rm v)
  let float8s32  rm (tg,v) = (unary_tag tg, D.float8s32  rm v)
  let float16s32 rm (tg,v) = (unary_tag tg, D.float16s32 rm v)
  let float32s32 rm (tg,v) = (unary_tag tg, D.float32s32 rm v)
  let float64s32 rm (tg,v) = (unary_tag tg, D.float64s32 rm v)
  let float1s64  rm (tg,v) = (unary_tag tg, D.float1s64  rm v)
  let float8s64  rm (tg,v) = (unary_tag tg, D.float8s64  rm v)
  let float16s64 rm (tg,v) = (unary_tag tg, D.float16s64 rm v)
  let float32s64 rm (tg,v) = (unary_tag tg, D.float32s64 rm v)
  let float64s64 rm (tg,v) = (unary_tag tg, D.float64s64 rm v)

  let float1u32  rm (tg,v) = (unary_tag tg, D.float1u32  rm v)
  let float8u32  rm (tg,v) = (unary_tag tg, D.float8u32  rm v)
  let float16u32 rm (tg,v) = (unary_tag tg, D.float16u32 rm v)
  let float32u32 rm (tg,v) = (unary_tag tg, D.float32u32 rm v)
  let float64u32 rm (tg,v) = (unary_tag tg, D.float64u32 rm v)
  let float1u64  rm (tg,v) = (unary_tag tg, D.float1u64  rm v)
  let float8u64  rm (tg,v) = (unary_tag tg, D.float8u64  rm v)
  let float16u64 rm (tg,v) = (unary_tag tg, D.float16u64 rm v)
  let float32u64 rm (tg,v) = (unary_tag tg, D.float32u64 rm v)
  let float64u64 rm (tg,v) = (unary_tag tg, D.float64u64 rm v)

  let fix32s1  rm (tg,v) = (unary_tag tg, D.fix32s1  rm v)
  let fix32s8  rm (tg,v) = (unary_tag tg, D.fix32s8  rm v)
  let fix32s16 rm (tg,v) = (unary_tag tg, D.fix32s16 rm v)
  let fix32s32 rm (tg,v) = (unary_tag tg, D.fix32s32 rm v)
  let fix32s64 rm (tg,v) = (unary_tag tg, D.fix32s64 rm v)
  let fix64s1  rm (tg,v) = (unary_tag tg, D.fix64s1  rm v)
  let fix64s8  rm (tg,v) = (unary_tag tg, D.fix64s8  rm v)
  let fix64s16 rm (tg,v) = (unary_tag tg, D.fix64s16 rm v)
  let fix64s32 rm (tg,v) = (unary_tag tg, D.fix64s32 rm v)
  let fix64s64 rm (tg,v) = (unary_tag tg, D.fix64s64 rm v)

  let fix32u1  rm (tg,v) = (unary_tag tg, D.fix32u1  rm v)
  let fix32u8  rm (tg,v) = (unary_tag tg, D.fix32u8  rm v)
  let fix32u16 rm (tg,v) = (unary_tag tg, D.fix32u16 rm v)
  let fix32u32 rm (tg,v) = (unary_tag tg, D.fix32u32 rm v)
  let fix32u64 rm (tg,v) = (unary_tag tg, D.fix32u64 rm v)
  let fix64u1  rm (tg,v) = (unary_tag tg, D.fix64u1  rm v)
  let fix64u8  rm (tg,v) = (unary_tag tg, D.fix64u8  rm v)
  let fix64u16 rm (tg,v) = (unary_tag tg, D.fix64u16 rm v)
  let fix64u32 rm (tg,v) = (unary_tag tg, D.fix64u32 rm v)
  let fix64u64 rm (tg,v) = (unary_tag tg, D.fix64u64 rm v)

  let fwiden32to64  rm (tg,v) = (unary_tag tg, D.fwiden32to64  rm v)
  let fnarrow64to32 rm (tg,v) = (unary_tag tg, D.fnarrow64to32 rm v)

  let get_tag (tg,v) = tg
end

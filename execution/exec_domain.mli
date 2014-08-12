(*
  Copyright (C) BitBlaze, 2009-2010. All rights reserved.
*)

module type DOMAIN = sig
  type t

  val from_concrete_1  : int -> t
  val from_concrete_8  : int -> t
  val from_concrete_16 : int -> t
  val from_concrete_32 : int64 -> t
  val from_concrete_64 : int64 -> t

  val to_concrete_1  : t -> int
  val to_concrete_8  : t -> int
  val to_concrete_16 : t -> int
  val to_concrete_32 : t -> int64
  val to_concrete_64 : t -> int64

  val to_symbolic_1  : t -> Vine.exp
  val to_symbolic_8  : t -> Vine.exp
  val to_symbolic_16 : t -> Vine.exp
  val to_symbolic_32 : t -> Vine.exp
  val to_symbolic_64 : t -> Vine.exp

  val from_symbolic : Vine.exp -> t

  val inside_symbolic : (Vine.exp -> Vine.exp) -> t -> t

  val measure_size : t -> int

  val  extract_8_from_64 : t -> int -> t
  val  extract_8_from_32 : t -> int -> t
  val  extract_8_from_16 : t -> int -> t
  val extract_16_from_64 : t -> int -> t
  val extract_16_from_32 : t -> int -> t
  val extract_32_from_64 : t -> int -> t

  val assemble16 : t -> t -> t
  val assemble32 : t -> t -> t
  val assemble64 : t -> t -> t
    
  val reassemble16 : t -> t -> t
  val reassemble32 : t -> t -> t
  val reassemble64 : t -> t -> t

  val to_string_1  : t -> string
  val to_string_8  : t -> string
  val to_string_16 : t -> string
  val to_string_32 : t -> string
  val to_string_64 : t -> string
    
  val uninit : t

  val plus1  : t -> t -> t
  val plus8  : t -> t -> t
  val plus16 : t -> t -> t
  val plus32 : t -> t -> t
  val plus64 : t -> t -> t

  val minus1  : t -> t -> t
  val minus8  : t -> t -> t
  val minus16 : t -> t -> t
  val minus32 : t -> t -> t
  val minus64 : t -> t -> t

  val times1  : t -> t -> t
  val times8  : t -> t -> t
  val times16 : t -> t -> t
  val times32 : t -> t -> t
  val times64 : t -> t -> t

  val divide1  : t -> t -> t
  val divide8  : t -> t -> t
  val divide16 : t -> t -> t
  val divide32 : t -> t -> t
  val divide64 : t -> t -> t

  val sdivide1  : t -> t -> t
  val sdivide8  : t -> t -> t
  val sdivide16 : t -> t -> t
  val sdivide32 : t -> t -> t
  val sdivide64 : t -> t -> t

  val mod1  : t -> t -> t
  val mod8  : t -> t -> t
  val mod16 : t -> t -> t
  val mod32 : t -> t -> t
  val mod64 : t -> t -> t

  val smod1  : t -> t -> t
  val smod8  : t -> t -> t
  val smod16 : t -> t -> t
  val smod32 : t -> t -> t
  val smod64 : t -> t -> t

  val lshift1  : t -> t -> t
  val lshift8  : t -> t -> t
  val lshift16 : t -> t -> t
  val lshift32 : t -> t -> t
  val lshift64 : t -> t -> t

  val rshift1  : t -> t -> t
  val rshift8  : t -> t -> t
  val rshift16 : t -> t -> t
  val rshift32 : t -> t -> t
  val rshift64 : t -> t -> t

  val arshift1  : t -> t -> t
  val arshift8  : t -> t -> t
  val arshift16 : t -> t -> t
  val arshift32 : t -> t -> t
  val arshift64 : t -> t -> t

  val bitand1  : t -> t -> t
  val bitand8  : t -> t -> t
  val bitand16 : t -> t -> t
  val bitand32 : t -> t -> t
  val bitand64 : t -> t -> t

  val bitor1  : t -> t -> t
  val bitor8  : t -> t -> t
  val bitor16 : t -> t -> t
  val bitor32 : t -> t -> t
  val bitor64 : t -> t -> t

  val xor1  : t -> t -> t
  val xor8  : t -> t -> t
  val xor16 : t -> t -> t
  val xor32 : t -> t -> t
  val xor64 : t -> t -> t

  val eq1  : t -> t -> t
  val eq8  : t -> t -> t
  val eq16 : t -> t -> t
  val eq32 : t -> t -> t
  val eq64 : t -> t -> t

  val neq1  : t -> t -> t
  val neq8  : t -> t -> t
  val neq16 : t -> t -> t
  val neq32 : t -> t -> t
  val neq64 : t -> t -> t

  val lt1  : t -> t -> t
  val lt8  : t -> t -> t
  val lt16 : t -> t -> t
  val lt32 : t -> t -> t
  val lt64 : t -> t -> t

  val le1  : t -> t -> t
  val le8  : t -> t -> t
  val le16 : t -> t -> t
  val le32 : t -> t -> t
  val le64 : t -> t -> t

  val slt1  : t -> t -> t
  val slt8  : t -> t -> t
  val slt16 : t -> t -> t
  val slt32 : t -> t -> t
  val slt64 : t -> t -> t

  val sle1  : t -> t -> t
  val sle8  : t -> t -> t
  val sle16 : t -> t -> t
  val sle32 : t -> t -> t
  val sle64 : t -> t -> t

  val neg1  : t -> t
  val neg8  : t -> t
  val neg16 : t -> t
  val neg32 : t -> t
  val neg64 : t -> t

  val not1  : t -> t
  val not8  : t -> t
  val not16 : t -> t
  val not32 : t -> t
  val not64 : t -> t

  val cast1u8   : t -> t
  val cast1u16  : t -> t
  val cast1u32  : t -> t
  val cast1u64  : t -> t
  val cast8u16  : t -> t
  val cast8u32  : t -> t
  val cast8u64  : t -> t
  val cast16u32 : t -> t
  val cast16u64 : t -> t
  val cast32u64 : t -> t

  val cast1s8   : t -> t
  val cast1s16  : t -> t
  val cast1s32  : t -> t
  val cast1s64  : t -> t
  val cast8s16  : t -> t
  val cast8s32  : t -> t
  val cast8s64  : t -> t
  val cast16s32 : t -> t
  val cast16s64 : t -> t
  val cast32s64 : t -> t

  val cast8l1   : t -> t
  val cast16l1  : t -> t
  val cast32l1  : t -> t
  val cast64l1  : t -> t
  val cast16l8  : t -> t
  val cast32l8  : t -> t
  val cast64l8  : t -> t
  val cast32l16 : t -> t
  val cast64l16 : t -> t
  val cast64l32 : t -> t

  val cast8h1   : t -> t
  val cast16h1  : t -> t
  val cast32h1  : t -> t
  val cast64h1  : t -> t
  val cast16h8  : t -> t
  val cast32h8  : t -> t
  val cast64h8  : t -> t
  val cast32h16 : t -> t
  val cast64h16 : t -> t
  val cast64h32 : t -> t

  val ite1  : t -> t -> t -> t
  val ite8  : t -> t -> t -> t
  val ite16 : t -> t -> t -> t
  val ite32 : t -> t -> t -> t
  val ite64 : t -> t -> t -> t

  val get_tag : t -> int64
end

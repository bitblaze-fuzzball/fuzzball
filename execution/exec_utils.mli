(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

val fix_u1  : int64 -> int64
val fix_u8  : int64 -> int64
val fix_u16 : int64 -> int64
val fix_u32 : int64 -> int64

val fix_s1  : int64 -> int64
val fix_s8  : int64 -> int64
val fix_s16 : int64 -> int64
val fix_s32 : int64 -> int64

val escaped : string -> string
val unescaped : string -> string

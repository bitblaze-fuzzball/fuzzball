(*
 Owned and copyright BitBlaze, 2009-2010. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

val fix_u1  : int64 -> int64
val fix_u8  : int64 -> int64
val fix_u16 : int64 -> int64
val fix_u32 : int64 -> int64

val fix_s1  : int64 -> int64
val fix_s8  : int64 -> int64
val fix_s16 : int64 -> int64
val fix_s32 : int64 -> int64

module ConcreteDomain : Exec_domain.DOMAIN


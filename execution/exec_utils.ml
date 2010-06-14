(*
  Copyright (C) BitBlaze, 2009-2010, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

let fix_u1  x = Int64.logand x 0x1L
let fix_u8  x = Int64.logand x 0xffL
let fix_u16 x = Int64.logand x 0xffffL
let fix_u32 x = Int64.logand x 0xffffffffL

let fix_s1  x = Int64.shift_right (Int64.shift_left x 63) 63
let fix_s8  x = Int64.shift_right (Int64.shift_left x 56) 56
let fix_s16 x = Int64.shift_right (Int64.shift_left x 48) 48
let fix_s32 x = Int64.shift_right (Int64.shift_left x 32) 32


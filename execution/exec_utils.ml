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

let escaped str =
  let ascii_is_printable c =
    (Char.code c) >= 0x20 (* space *) && (Char.code c) <= 0x7e (* ~ *)
  in
  let hex_chr i =
    if i < 10 then
      Char.chr (0x30 + i) (* 0x30 = '0' *)
    else
      Char.chr (0x61 + i - 10) (* 0x61 = 'a' *)
  in
  let len = String.length str in
  let s' = String.create (4*len) in (* worst case c -> \xff *)
  let rec loop i j =
    if i >= len then
      j
    else
      let single_escape c =
	s'.[j] <- '\\';
	s'.[j+1] <- c;
	loop (i+1) (j+2)
      in
      match str.[i] with
	| '\n' -> single_escape 'n'
	| '\t' -> single_escape 't'
	| '\r' -> single_escape 'r'
	| '\b' -> single_escape 'b'
	| '"' -> single_escape '"'
	| '\\' -> single_escape '\\'
	| c when ascii_is_printable c ->
	    s'.[j] <- str.[i];
	    loop (i+1) (j+1)
	| c ->
	    s'.[j] <- '\\';
	    s'.[j+1] <- 'x';
	    s'.[j+2] <- hex_chr ((Char.code c) lsr 4);
	    s'.[j+3] <- hex_chr ((Char.code c) land 0xf);
	    loop (i+1) (j+4)
  in
  let len' = loop 0 0 in
    String.sub s' 0 len'

let unescaped str =
  let len = String.length str in
  let s' = String.create len in
  let rec loop i j =
    if i >= len then
      j
    else
      match str.[i] with
	| '\\' when i + 1 < len ->
	    let char inc c =
	      s'.[j] <- c;
	      loop (i + inc + 1) (j + 1)
	    in
	      (match str.[i+1] with
		 | 'n' -> char 1 '\n'
		 | 'r' -> char 1 '\r'
		 | 't' -> char 1 '\t'
		 | 'b' -> char 1 '\b'
		 | '\\' -> char 1 '\\'
		 | '\'' -> char 1 '\''
		 | '"' -> char 1 '"'
		 | ' ' -> char 1 ' '
		 | 'x' when i + 3 < len ->
		     char 3 (Char.chr (int_of_string
					 ("0x" ^ (String.sub str (i+2) 2))))
		 | '0' .. '9' when i + 3 < len ->
		     char 3 (Char.chr (int_of_string
					 (String.sub str (i+1) 3)))
		 | _ -> failwith "Unexpected escape in string unescape"
	      )
	| _ ->
	    s'.[j] <- str.[i];
	    loop (i+1) (j+1)
  in
  let len' = loop 0 0 in
    String.sub s' 0 len'


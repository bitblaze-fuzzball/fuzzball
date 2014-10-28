(*
  Copyright (C) BitBlaze, 2010-2012, and copyright (C) 2010 Ensighta
  Security Inc.  All rights reserved.
*)

module V = Vine;;

module VarByInt =
struct
  type t = V.var
  let hash (i,_,_) = i
  let equal x y =
    x == y ||
      match (x,y) with
	| ((x,_,_), (y,_,_)) when x == y ->
	    true
	| _ -> false
  let compare (x,_,_) (y,_,_) = compare x y
end

module VarWeak = Weak.Make(VarByInt)

let var_num_to_name = Hashtbl.create 1001

let free_var (n,s,t) =
  Hashtbl.remove var_num_to_name n

let canon_vars = VarWeak.create 1001

let encode_exp_flags e printable =
  let chars = ref [] in
  let vars = ref [] in
  let push c = chars := c :: !chars in
  let push_printable_str s = String.iter push s in
  let push_int64 i =
    let push_char c =
      if printable then
	push_printable_str (Printf.sprintf "(0x%x)" (Char.code c))
      else
	push c
    in
    let push_short s =
      if printable then
	push_printable_str (Printf.sprintf "(0x%x)" s)
      else
	let lb = s land 0xff and
	    hb = s asr 8 in
	  push (Char.chr hb);
	  push (Char.chr lb)
    in
    let push_word i =
      if printable then
	push_printable_str (Printf.sprintf "(0x%Lx)" i)
      else
	(push_short (Int64.to_int (Int64.shift_right i 16));
	 push_short (Int64.to_int (Int64.logand i 0xffffL)))
    in
      match i with
	| (0L|1L|2L|3L|4L|5L|6L|7L|8L|9L) ->
	    push (Char.chr((Char.code '0') + (Int64.to_int i)))
	| i when i >= 10L && i < 36L ->
	    push (Char.chr((Char.code 'a') + (Int64.to_int i) - 10))
	| -1L -> push '-'
	| i when i >= 0L && i < 256L ->
	    push 'C';
	    push_char (Char.chr (Int64.to_int i))
	| i when i >= -128L && i < 0L ->
	    push 'D';
	    push_char (Char.chr (-(Int64.to_int i)))
	| i when i >= 0xffffff00L && i <= 0xffffffffL ->
	    push 'E';
	    push_char (Char.chr (Int64.to_int i land 0xff))
	| i when i >= 0L && i < 65536L ->
	    push 'S';
	    push_short (Int64.to_int i)
	| i when i >= -32768L && i < 0L ->
	    push 'T';
	    push_short (- (Int64.to_int i))
	| i when i >= 0L && i < 0x100000000L ->
	    push 'I';
	    push_word i;
	| i when i >= 2147483648L && i < 0L ->
	    push 'J';
	      push_word (Int64.neg i);
	| _ ->
	    push 'L';
	    push_word (Int64.shift_right i 32);
	    push_word (Int64.logand i 0xffffffffL);
  in
  let push_string s =
    assert(String.length s < 250);
    if printable then
	push_printable_str (Printf.sprintf "(%s)" (Exec_utils.escaped s))
    else
      (push (Char.chr (String.length s));
       for i = 0 to (String.length s) - 1 do
	 push s.[i]
       done)
  in
  let push_var ((n,s,t) as var) =
    let c =
      (match t with
	 | V.REG_1  -> 'b'
	 | V.REG_8  -> 'c'
	 | V.REG_16 -> 's'
	 | V.REG_32 -> 'i'
	 | V.REG_64 -> 'l'
	 | V.TMem(V.REG_32, V.Little) -> 'M'
	 | _ ->
	     Printf.printf "Bad type: %s\n" (V.type_to_string t);
	     failwith "Unexpected variable type in encode_exp") in
      push c;
      if printable then
	push_printable_str (Printf.sprintf "(%s)" s)
      else
	(push_int64 (Int64.of_int n);
	 Hashtbl.replace var_num_to_name n s;
	 ignore(VarWeak.merge canon_vars var);
	 vars := var :: !vars)
  in
  let rec loop e = match e with
    | V.BinOp(V.LT, V.BinOp(V.PLUS, e1, V.Constant(V.Int(V.REG_32, 1L))), e2)
	when e1 = e2 ->
	push 'O';
	loop e1
    | V.BinOp
	(V.XOR,
	 e1,
	 V.BinOp
	   (V.XOR,
	    V.BinOp(V.RSHIFT, e2, V.Constant(V.Int(V.REG_32, 7L))),
	    V.BinOp
	      (V.XOR,
	       V.BinOp(V.RSHIFT, e3, V.Constant(V.Int(V.REG_32, 6L))),
	       V.BinOp
		 (V.XOR,
		  V.BinOp(V.RSHIFT, e4, V.Constant(V.Int(V.REG_32, 5L))),
		  V.BinOp
		    (V.XOR,
		     V.BinOp(V.RSHIFT, e5, V.Constant(V.Int(V.REG_32, 4L))),
		     V.BinOp
		       (V.XOR,
			V.BinOp(V.RSHIFT, e6, V.Constant(V.Int(V.REG_32, 3L))),
			V.BinOp
			  (V.XOR,
			   V.BinOp(V.RSHIFT, e7,
				   V.Constant(V.Int(V.REG_32, 2L))),
			   V.BinOp(V.RSHIFT, e8,
				   V.Constant(V.Int(V.REG_32, 1L))))))))))
	when e1 = e2 && e1 = e3 && e1 = e4 && e1 = e5 && e1 = e6 && e1 = e7
	  && e1 = e7 ->
	push 'P';
	    loop e1
    | V.BinOp(op, e1, e2) ->
	let char =
	  (match op with
	     | V.PLUS -> '+'
	     | V.MINUS -> '-'
	     | V.TIMES -> '*'
	     | V.DIVIDE -> '/'
	     | V.SDIVIDE -> '$'
	     | V.MOD -> '%'
	     | V.SMOD -> '#'
	     | V.LSHIFT -> '['
	     | V.RSHIFT -> ']'
	     | V.ARSHIFT -> '@'
	     | V.BITAND -> '&'
	     | V.BITOR -> '|'
	     | V.XOR -> '^'
	     | V.EQ -> '='
	     | V.NEQ -> '\\'
	     | V.LT -> '<'
	     | V.LE -> ','
	     | V.SLT -> '>'
	     | V.SLE -> '.') in
	  push char;
	  loop e1;
	  loop e2
    | V.UnOp(op, e1) ->
	let char = (match op with V.NEG -> '~' | V.NOT -> '!') in
	  push char;
	  loop e1
    | V.Constant(V.Int(V.REG_1, 0L)) -> push 'F'
    | V.Constant(V.Int(V.REG_1, 1L)) -> push 'T'
    | V.Constant(V.Int(V.REG_8, c))  -> push '8'; push_int64 c
    | V.Constant(V.Int(V.REG_16, c)) -> push '1'; push_int64 c
    | V.Constant(V.Int(V.REG_32, c)) -> push '3'; push_int64 c
    | V.Constant(V.Int(V.REG_64, c)) -> push '6'; push_int64 c
    | V.Constant(V.Int(_, _)) ->
	failwith "Unsupported weird-type integer constant in encode_exp"
    | V.Constant(V.Str(s)) -> push 'S'; push_string s
    | V.Lval(V.Temp(var)) -> push 't'; push_var var
    | V.Lval(V.Mem(mvar, e1, V.REG_8))  -> push 'm'; push_var mvar; loop e1
    | V.Lval(V.Mem(mvar, e1, V.REG_16)) -> push 'n'; push_var mvar; loop e1
    | V.Lval(V.Mem(mvar, e1, V.REG_32)) -> push 'M'; push_var mvar; loop e1
    | V.Lval(V.Mem(mvar, e1, V.REG_64)) -> push 'N'; push_var mvar; loop e1
    | V.Lval(V.Mem(_, _, _)) ->
	failwith "Unsupported weird-type memory lval in encode_exp"
    | V.Name(s) -> push 'l'; push_string s
    | V.Cast(ctype, ty, e1) ->
	let ty_char =
	  (match (ctype, ty) with
	     | (V.CAST_UNSIGNED, V.REG_8)  -> 'U'
	     | (V.CAST_UNSIGNED, V.REG_16) -> 'V'
	     | (V.CAST_UNSIGNED, V.REG_32) -> 'W'
	     | (V.CAST_UNSIGNED, V.REG_64) -> 'X'
	     | (V.CAST_SIGNED,   V.REG_8)  -> 'Q'
	     | (V.CAST_SIGNED,   V.REG_16) -> 'R'
	     | (V.CAST_SIGNED,   V.REG_32) -> 'S'
	     | (V.CAST_SIGNED,   V.REG_64) -> 'T'
	     | (V.CAST_LOW,      V.REG_1)  -> 'l'
	     | (V.CAST_LOW,      V.REG_8)  -> 'm'
	     | (V.CAST_LOW,      V.REG_16) -> 'n'
	     | (V.CAST_LOW,      V.REG_32) -> 'o'
	     | (V.CAST_HIGH,     V.REG_1)  -> 'h'
	     | (V.CAST_HIGH,     V.REG_8)  -> 'i'
	     | (V.CAST_HIGH,     V.REG_16) -> 'j'
	     | (V.CAST_HIGH,     V.REG_32) -> 'k'
	     | _ -> failwith "Unexpected cast type in encode_exp") in
	  push 'C';
	  push ty_char;
	  loop e1
    | V.Ite(ce, te, fe) -> push '?'; loop ce; loop te; loop fe
    | V.Unknown(s) -> push 'U'; push_string s
    | V.Let(_, _, _)
      ->
	Printf.printf "Offending exp: %s\n" (V.exp_to_string e);
	failwith "Unexpected expr type in encode_exp"
  in
    loop e;
    let len = List.length !chars and
	l = ref (List.rev !chars) in
    let str = String.create len in
      for i = 0 to len - 1 do
	str.[i] <- (List.hd !l);
	l := List.tl !l
      done;
      (str, !vars)

let encode_exp e = encode_exp_flags e false

let encode_printable_exp e =
  let (s, _) = encode_exp_flags e true in s

let decode_exp s =
  let parse_short i =
    let hb = Char.code s.[i] and
	lb = Char.code s.[i + 1] in
      (i + 2, (hb lsl 8) lor lb)
  in
  let parse_word i =
    let (i2, hs) = parse_short i in
    let (i3, ls) = parse_short i2 in
      (i3, (Int64.logor (Int64.shift_left (Int64.of_int hs) 16)
	      (Int64.of_int ls)))
  in
  let parse_int64 i =
    let i2 = i + 1 in
      match s.[i] with
	| c when c >= '0' && c <= '9' ->
	    (i2, Int64.of_int ((Char.code c) - (Char.code '0')))
	| c when c >= 'a' && c <= 'z' ->
	    (i2, Int64.of_int ((Char.code c) - (Char.code 'a') + 10))
	| '-' -> (i2, -1L)
	| 'C' ->
	    (i2 + 1, Int64.of_int (Char.code s.[i2]))
	| 'D' ->
	    (i2 + 1, Int64.of_int (-(Char.code s.[i2])))
	| 'E' ->
	    (i2 + 1, Int64.logor 0xffffff00L (Int64.of_int (Char.code s.[i2])))
	| 'S' ->
	    let (i3, s) = parse_short i2 in
	      (i3, Int64.of_int s)
	| 'T' ->
	    let (i3, s) = parse_short i2 in
	      (i3, Int64.of_int (-s))
	| 'I' ->
	    parse_word i2
	| 'J' ->
	    let (i3, i) = parse_word i2 in
	      (i3, Int64.neg i)
	| 'L' ->
	    let (i3, hw) = parse_word i2 in
	    let (i4, lw) = parse_word i3 in
	      (i4, Int64.logor (Int64.shift_left hw 32) lw)
	| _ -> failwith "Unexpected character in parse_int64"
  in
  let parse_const ty i =
    let (i2, c) = parse_int64 i in
      (i2, V.Constant(V.Int(ty, c)))
  in
  let parse_string i =
    let len = Char.code s.[i] in
      (i + len + 1, String.sub s (i + 1) len)
  in
  let parse_var i =
    let ty =
      (match s.[i] with
	 | 'b' -> V.REG_1
	 | 'c' -> V.REG_8
	 | 's' -> V.REG_16
	 | 'i' -> V.REG_32
	 | 'l' -> V.REG_64
	 | 'M' -> V.TMem(V.REG_32, V.Little)
	 | _ -> failwith "Bad type in parse_var") in
    let (i2, n64) = parse_int64 (i + 1) in
    let n = Int64.to_int n64 in
    let var = (n, (Hashtbl.find var_num_to_name n), ty) in
    let cvar = try
      VarWeak.find canon_vars var
    with Not_found -> var
    in
      (i2, cvar)
  in
  let rec parse_binop op i =
    let (i2, e1) = parse i in
    let (i3, e2) = parse i2 in
      (i3, V.BinOp(op, e1, e2))
  and parse_unop op i =
    let (i2, e1) = parse i in
      (i2, V.UnOp(op, e1))
  and parse_mem ty i =
    let (i2, var) = parse_var i in
    let (i3, e1) = parse i2 in
      (i3, V.Lval(V.Mem(var, e1, ty)))
  and parse i =
    let i' = i + 1 in
      match s.[i] with
	| '+'  -> parse_binop V.PLUS    i'
	| '-'  -> parse_binop V.MINUS   i'
	| '*'  -> parse_binop V.TIMES   i'
	| '/'  -> parse_binop V.DIVIDE  i'
	| '$'  -> parse_binop V.SDIVIDE i'
	| '%'  -> parse_binop V.MOD     i'
	| '#'  -> parse_binop V.SMOD    i'
	| '['  -> parse_binop V.LSHIFT  i'
	| ']'  -> parse_binop V.RSHIFT  i'
	| '@'  -> parse_binop V.ARSHIFT i'
	| '&'  -> parse_binop V.BITAND  i'
	| '|'  -> parse_binop V.BITOR   i'
	| '^'  -> parse_binop V.XOR     i'
	| '='  -> parse_binop V.EQ      i'
	| '\\' -> parse_binop V.NEQ     i'
	| '<'  -> parse_binop V.LT      i'
	| ','  -> parse_binop V.LE      i'
	| '>'  -> parse_binop V.SLT     i'
	| '.'  -> parse_binop V.SLE     i'
	| '~' -> parse_unop V.NEG i'
	| '!' -> parse_unop V.NOT i'
	| 'F' -> (i', V.Constant(V.Int(V.REG_1, 0L)))
	| 'T' -> (i', V.Constant(V.Int(V.REG_1, 1L)))
	| '8' -> parse_const V.REG_8  i'
	| '1' -> parse_const V.REG_16 i'
	| '3' -> parse_const V.REG_32 i'
	| '6' -> parse_const V.REG_64 i'
	| 'S' ->
	    let (i2, s) = parse_string i' in
	      (i2, V.Constant(V.Str(s)))
	| 't' ->
	    let (i2, var) = parse_var i' in
	      (i2, V.Lval(V.Temp(var)))
	| 'm' -> parse_mem V.REG_8 i'
	| 'n' -> parse_mem V.REG_16 i'
	| 'M' -> parse_mem V.REG_32 i'
	| 'N' -> parse_mem V.REG_64 i'
	| 'l' ->
	    let (i2, s) = parse_string i' in
	      (i2, V.Name(s))
	| 'C' ->
	    let (ctype, ty) =
	      (match s.[i'] with
		 | 'U' -> (V.CAST_UNSIGNED, V.REG_8)
		 | 'V' -> (V.CAST_UNSIGNED, V.REG_16)
		 | 'W' -> (V.CAST_UNSIGNED, V.REG_32)
		 | 'X' -> (V.CAST_UNSIGNED, V.REG_64)
		 | 'Q' -> (V.CAST_SIGNED,   V.REG_8) 
		 | 'R' -> (V.CAST_SIGNED,   V.REG_16)
		 | 'S' -> (V.CAST_SIGNED,   V.REG_32)
		 | 'T' -> (V.CAST_SIGNED,   V.REG_64)
		 | 'l' -> (V.CAST_LOW,      V.REG_1) 
		 | 'm' -> (V.CAST_LOW,      V.REG_8)
		 | 'n' -> (V.CAST_LOW,      V.REG_16)
		 | 'o' -> (V.CAST_LOW,      V.REG_32)
		 | 'h' -> (V.CAST_HIGH,     V.REG_1)
		 | 'i' -> (V.CAST_HIGH,     V.REG_8)
		 | 'j' -> (V.CAST_HIGH,     V.REG_16)
		 | 'k' -> (V.CAST_HIGH,     V.REG_32)
		 | _ -> failwith "Unknown cast type in decode_exp") in
	    let (i2, e1) = parse (i' + 1) in
	      (i2, V.Cast(ctype, ty, e1))
	| '?' ->
	    let (i2, e1) = parse (i + 1) in
	    let (i3, e2) = parse i2 in
	    let (i4, e3) = parse i3 in
	      (i4, V.Ite(e1, e2, e3))
	| 'U' ->
	    let (i2, s) = parse_string i' in
	      (i2, V.Unknown(s))
	| 'O' ->
	    let (i2, e1) = parse i' in
	      (i2, V.BinOp(V.LT, V.BinOp(V.PLUS, e1,
					 V.Constant(V.Int(V.REG_32, 1L))), e1))
	| 'P' ->
	    let (i2, e1) = parse i' in
	      (i2,
	       V.BinOp
		 (V.XOR,
		  e1,
		  V.BinOp
		    (V.XOR,
		     V.BinOp(V.RSHIFT, e1, V.Constant(V.Int(V.REG_32, 7L))),
		     V.BinOp
		       (V.XOR,
			V.BinOp(V.RSHIFT, e1, V.Constant(V.Int(V.REG_32, 6L))),
			V.BinOp
			  (V.XOR,
			   V.BinOp(V.RSHIFT, e1,
				   V.Constant(V.Int(V.REG_32, 5L))),
			   V.BinOp
			     (V.XOR,
			      V.BinOp(V.RSHIFT, e1,
				      V.Constant(V.Int(V.REG_32, 4L))),
			      V.BinOp
				(V.XOR,
				 V.BinOp(V.RSHIFT, e1,
					 V.Constant(V.Int(V.REG_32, 3L))),
				 V.BinOp
				   (V.XOR,
				    V.BinOp(V.RSHIFT, e1,
					    V.Constant(V.Int(V.REG_32, 2L))),
				    V.BinOp(V.RSHIFT, e1,
					    V.Constant(V.Int(V.REG_32, 1L
							    )))))))))))
	| _ -> failwith "Unhandled character in decode_exp"
  in
  let (i2, e) = parse 0 in
    assert(i2 = String.length s);
    e

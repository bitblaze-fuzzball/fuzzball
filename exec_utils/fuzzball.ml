(*
 Owned and copyright BitBlaze, 2007,2009. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*)

module V = Vine;;

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

  val to_symbolic_1  : t -> V.exp
  val to_symbolic_8  : t -> V.exp
  val to_symbolic_16 : t -> V.exp
  val to_symbolic_32 : t -> V.exp
  val to_symbolic_64 : t -> V.exp

  val from_symbolic : V.exp -> t

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
end

let fix_u1  x = Int64.logand x 0x1L
let fix_u8  x = Int64.logand x 0xffL
let fix_u16 x = Int64.logand x 0xffffL
let fix_u32 x = Int64.logand x 0xffffffffL

let fix_s1  x = Int64.shift_right (Int64.shift_left x 63) 63
let fix_s8  x = Int64.shift_right (Int64.shift_left x 56) 56
let fix_s16 x = Int64.shift_right (Int64.shift_left x 48) 48
let fix_s32 x = Int64.shift_right (Int64.shift_left x 32) 32

module ConcreteDomain : DOMAIN = struct
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

  let divide1  v v2 = Vine_util.int64_udiv (fix_u1  v) (fix_u1  v2)
  let divide8  v v2 = Vine_util.int64_udiv (fix_u8  v) (fix_u8  v2)
  let divide16 v v2 = Vine_util.int64_udiv (fix_u16 v) (fix_u16 v2)
  let divide32 v v2 = Vine_util.int64_udiv (fix_u32 v) (fix_u32 v2)
  let divide64 v v2 = Vine_util.int64_udiv          v           v2

  let sdivide1  v v2 = Int64.div (fix_s1  v) (fix_s1  v2)
  let sdivide8  v v2 = Int64.div (fix_s8  v) (fix_s8  v2)
  let sdivide16 v v2 = Int64.div (fix_s16 v) (fix_s16 v2)
  let sdivide32 v v2 = Int64.div (fix_s32 v) (fix_s32 v2)
  let sdivide64 v v2 = Int64.div          v           v2 

  let mod1  v v2 = Vine_util.int64_urem (fix_u1  v) (fix_u1  v2)
  let mod8  v v2 = Vine_util.int64_urem (fix_u8  v) (fix_u8  v2)
  let mod16 v v2 = Vine_util.int64_urem (fix_u16 v) (fix_u16 v2)
  let mod32 v v2 = Vine_util.int64_urem (fix_u32 v) (fix_u32 v2)
  let mod64 v v2 = Vine_util.int64_urem          v           v2 

  let smod1  v v2 = Int64.rem (fix_s1  v) (fix_s1  v2)
  let smod8  v v2 = Int64.rem (fix_s8  v) (fix_s8  v2)
  let smod16 v v2 = Int64.rem (fix_s16 v) (fix_s16 v2)
  let smod32 v v2 = Int64.rem (fix_s32 v) (fix_s32 v2)
  let smod64 v v2 = Int64.rem          v           v2 

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

  let slt1  v v2 = bool ((fix_u1  v) < (fix_u1  v2))
  let slt8  v v2 = bool ((fix_u8  v) < (fix_u8  v2))
  let slt16 v v2 = bool ((fix_u16 v) < (fix_u16 v2))
  let slt32 v v2 = bool ((fix_u32 v) < (fix_u32 v2))
  let slt64 v v2 = bool ((        v) < (        v2))

  let sle1  v v2 = bool ((fix_u1  v) <= (fix_u1  v2))
  let sle8  v v2 = bool ((fix_u8  v) <= (fix_u8  v2))
  let sle16 v v2 = bool ((fix_u16 v) <= (fix_u16 v2))
  let sle32 v v2 = bool ((fix_u32 v) <= (fix_u32 v2))
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
end

exception NotConcrete of V.exp

let rec constant_fold_rec e =
  match e with
    | V.BinOp(op, e1, e2) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.BinOp(op, constant_fold_rec e1, constant_fold_rec e2))
    | V.UnOp(op, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.UnOp(op, constant_fold_rec e))
    | V.Cast(op, ty, e) ->
	Vine_opt.constant_fold_more (fun _ -> None)
	  (V.Cast(op, ty, constant_fold_rec e))
    | V.Lval(lv) -> V.Lval(constant_fold_rec_lv lv)
    | _ -> e
and constant_fold_rec_lv lv =
  match lv with
    | V.Mem(v, e, ty) -> V.Mem(v, constant_fold_rec e, ty)
    | _ -> lv

let rec expr_size e =
  match e with
    | V.BinOp(_, e1, e2) -> 1 + (expr_size e1) + (expr_size e2)
    | V.UnOp(_, e1) -> 1 + (expr_size e1)
    | V.Constant(_) -> 1
    | V.Lval(V.Temp(_)) -> 2
    | V.Lval(V.Mem(_, e1, _)) -> 2 + (expr_size e1)
    | V.Name(_) -> 1
    | V.Cast(_, _, e1) -> 1 + (expr_size e1)
    | V.Unknown(_) -> 1
    | V.Let(_, _, _) -> failwith "Unexpected let in expr_size"

let rec stmt_size = function
  | V.Jmp(e) -> 1 + (expr_size e)
  | V.CJmp(e1, e2, e3) -> 1 + (expr_size e1) + (expr_size e2) + (expr_size e3)
  | V.Move(lv, e) -> 1 + (expr_size (V.Lval(lv))) + (expr_size e)
  | V.Special(_) -> 1
  | V.Label(_) -> 1
  | V.ExpStmt(e) -> 1 + (expr_size e)
  | V.Comment(_) -> 1
  | V.Block(dl, sl) -> 1 + (List.length dl) +
      (List.fold_left (+) 0 (List.map stmt_size sl))
  | V.Function(_, _, dl, _, st_o) ->
      3 + (List.length dl) +
	(match st_o with None -> 0 | Some st -> stmt_size st)
  | V.Return(None) -> 1
  | V.Return(Some e) -> (1+ expr_size e)
  | V.Call(lv_o, e1, e_l) -> 1 +
      (match lv_o with None -> 0 | Some(lv) -> expr_size (V.Lval(lv)))
      + (expr_size e1) + (List.fold_left (+) 0 (List.map expr_size e_l))
  | V.Attr(st, _) -> 1 + (stmt_size st)
  | V.Assert(e) -> 1 + (expr_size e)
  | V.Halt(e) -> 1 + (expr_size e)

let opt_trace_temps = ref false

module FormulaManagerFunctor =
  functor (D : DOMAIN) ->
struct
  class formula_manager = object(self)
    val input_vars = Hashtbl.create 30

    method private fresh_symbolic_var str ty =
      try Hashtbl.find input_vars str with
	  Not_found ->
	    Hashtbl.replace input_vars str (V.newvar str ty);
	    Hashtbl.find input_vars str

    method private fresh_symbolic_vexp str ty =
      V.Lval(V.Temp(self#fresh_symbolic_var str ty))

    method private fresh_symbolic str ty =
	D.from_symbolic (self#fresh_symbolic_vexp str ty)

    method fresh_symbolic_1  s = self#fresh_symbolic s V.REG_1
    method fresh_symbolic_8  s = self#fresh_symbolic s V.REG_8
    method fresh_symbolic_16 s = self#fresh_symbolic s V.REG_16
    method fresh_symbolic_32 s = self#fresh_symbolic s V.REG_32
    method fresh_symbolic_64 s = self#fresh_symbolic s V.REG_64

    method get_input_vars = Hashtbl.fold (fun s v l -> v :: l) input_vars []

    val region_vars = Hashtbl.create 30

    method private fresh_symbolic_mem ty str addr =
      let v = try Hashtbl.find region_vars str with
	  Not_found ->
	    Hashtbl.replace region_vars str
	      (V.newvar str (V.TMem(V.REG_32, V.Little)));
	    Hashtbl.find region_vars str
      in
	D.from_symbolic
	  (V.Lval(V.Mem(v, V.Constant(V.Int(V.REG_32, addr)), ty)))

    method fresh_symbolic_mem_1  = self#fresh_symbolic_mem V.REG_1
    method fresh_symbolic_mem_8  = self#fresh_symbolic_mem V.REG_8
    method fresh_symbolic_mem_16 = self#fresh_symbolic_mem V.REG_16
    method fresh_symbolic_mem_32 = self#fresh_symbolic_mem V.REG_32
    method fresh_symbolic_mem_64 = self#fresh_symbolic_mem V.REG_64

    method private mem_var region_str ty addr =
      let ty_str = (match ty with
		      | V.REG_8 -> "byte"
		      | V.REG_16 -> "short"
		      | V.REG_32 -> "word"
		      | V.REG_64 -> "long"
		      | _ -> failwith "Bad size in mem_var")
      in
      let name = Printf.sprintf "%s_%s_0x%08Lx" region_str ty_str addr
      in
	self#fresh_symbolic_var name ty

    method private mem_axioms_short region_str addr svar =
      let bvar0 = self#mem_var region_str V.REG_8 addr and
	  bvar1 = self#mem_var region_str V.REG_8 (Int64.add addr 1L) in
	[svar, D.to_symbolic_16
	   (D.assemble16 (D.from_symbolic (V.Lval(V.Temp(bvar0))))
	      (D.from_symbolic (V.Lval(V.Temp(bvar1)))))]

    method private mem_axioms_word region_str addr wvar =
      let svar0 = self#mem_var region_str V.REG_16 addr and
	  svar1 = self#mem_var region_str V.REG_16 (Int64.add addr 2L) in
	[wvar, D.to_symbolic_32
	   (D.assemble32 (D.from_symbolic (V.Lval(V.Temp(svar0))))
	      (D.from_symbolic (V.Lval(V.Temp(svar1)))))]
	@ (self#mem_axioms_short region_str addr svar0)
	@ (self#mem_axioms_short region_str (Int64.add addr 2L) svar1)

    method private mem_axioms_long region_str addr lvar =
      let wvar0 = self#mem_var region_str V.REG_32 addr and
	  wvar1 = self#mem_var region_str V.REG_32 (Int64.add addr 4L) in
	[lvar, D.to_symbolic_64
	   (D.assemble32 (D.from_symbolic (V.Lval(V.Temp(wvar0))))
	      (D.from_symbolic (V.Lval(V.Temp(wvar1)))))]
	@ (self#mem_axioms_word region_str addr wvar0)
	@ (self#mem_axioms_word region_str (Int64.add addr 4L) wvar1)

    val mem_axioms = V.VarHash.create 30

    method private add_mem_axioms region_str ty addr =
      if ty <> V.REG_8 && 
	not (V.VarHash.mem mem_axioms (self#mem_var region_str ty addr))
      then
	let var = self#mem_var region_str ty addr in
	let al = (match ty with
		    | V.REG_8  -> []
		    | V.REG_16 -> self#mem_axioms_short region_str addr var
		    | V.REG_32 -> self#mem_axioms_word region_str addr var
		    | V.REG_64 -> self#mem_axioms_long region_str addr var
		    | _ -> failwith "Unexpected type in add_mem_axioms") in
	  List.iter
	    (fun (lhs, rhs) -> V.VarHash.replace mem_axioms lhs rhs)
	    al;
	  assert(ty = V.REG_8 || V.VarHash.mem mem_axioms var);

    method rewrite_mem_expr e =
      match e with
	| V.Lval(V.Mem((_,region_str,ty1),
		       V.Constant(V.Int(V.REG_32, addr)), ty2))
	  -> (self#add_mem_axioms region_str ty2 addr;
	      V.Lval(V.Temp(self#mem_var region_str ty2 addr)))
	| _ -> failwith "Bad expression in rewrite_mem_expr"

    method rewrite_for_stp e =
      let rec loop e =
	match e with
	  | V.BinOp(op, e1, e2) -> V.BinOp(op, (loop e1), (loop e2))
	  | V.UnOp(op, e1) -> V.UnOp(op, (loop e1))
	  | V.Constant(_) -> e
	  | V.Lval(V.Temp(_)) -> e
	  | V.Lval(V.Mem(_, _, _)) -> self#rewrite_mem_expr e
	  | V.Name(_) -> e
	  | V.Cast(kind, ty, e1) -> V.Cast(kind, ty, (loop e1))
	  | V.Unknown(_) -> e
	  | V.Let(_, _, _) -> failwith "Unexpected let in rewrite_for_stp"
      in
	loop e

    method get_mem_axioms =
      let of_type ty ((n,s,ty'),e) = (ty = ty') in
      let l = V.VarHash.fold
	(fun lhs rhs l -> (lhs, rhs) :: l) mem_axioms [] in
      let shorts = List.filter (of_type V.REG_16) l and
	  words  = List.filter (of_type V.REG_32) l and
	  longs  = List.filter (of_type V.REG_64) l in
	shorts @ words @ longs

    method reset_mem_axioms = V.VarHash.clear mem_axioms

    (* subexpression cache *)
    val subexpr_to_temp_var = Hashtbl.create 1001
    val temp_var_to_subexpr = V.VarHash.create 1001
    val mutable temp_var_num = 0

    method simplify (e:D.t) ty =
      let e' = constant_fold_rec (D.to_symbolic_32 e) in
	if expr_size e' < 10 then
	  D.from_symbolic e'
	else
	  let var =
	    (try
	       Hashtbl.find subexpr_to_temp_var e'
	     with Not_found ->
	       let s = "t" ^ (string_of_int temp_var_num) in
		 temp_var_num <- temp_var_num + 1;
		 let var = V.newvar s ty in
 		   Hashtbl.replace subexpr_to_temp_var e' var;
 		   V.VarHash.replace temp_var_to_subexpr var e';
		   if !opt_trace_temps then
		     Printf.printf "%s = %s\n" s (V.exp_to_string e');
		   var) in
	    D.from_symbolic (V.Lval(V.Temp(var)))
	      
    method simplify1  e = self#simplify e V.REG_1
    method simplify8  e = self#simplify e V.REG_8
    method simplify16 e = self#simplify e V.REG_16
    method simplify32 e = self#simplify e V.REG_32
    method simplify64 e = self#simplify e V.REG_64

    method if_expr_temp_unit var fn_t =
      try
	let e = V.VarHash.find temp_var_to_subexpr var in
	  (fn_t e)
      with Not_found -> ()
  end

  (* This has to be outside the class because I want it to have
     polymorphic type. *)
  let if_expr_temp form_man var fn_t else_val =
    let box = ref else_val in
      form_man#if_expr_temp_unit var
	(fun e -> box := fn_t e);
      !box
end

module SymbolicDomain : DOMAIN = struct
  type t = V.exp

  let from_concrete_1 v  = V.Constant(V.Int(V.REG_1,  (Int64.of_int v)))
  let from_concrete_8 v  = V.Constant(V.Int(V.REG_8,  (Int64.of_int v)))
  let from_concrete_16 v = V.Constant(V.Int(V.REG_16, (Int64.of_int v)))
  let from_concrete_32 v = V.Constant(V.Int(V.REG_32,               v ))
  let from_concrete_64 v = V.Constant(V.Int(V.REG_64,               v ))

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

  let measure_size e = expr_size e

  let make_extract t which e =
    V.Cast(V.CAST_LOW, t, 
	   V.BinOp(V.RSHIFT, e,
		   V.Constant(V.Int(V.REG_8,
				    (Int64.mul 8L (Int64.of_int which))))))
      
  let extract_8_from_64 e which =
    match which with
      | 0 -> V.Cast(V.CAST_LOW, V.REG_8, e)
      | 7 -> V.Cast(V.CAST_HIGH, V.REG_8, e)
      | _ -> make_extract V.REG_8 which e

  let extract_8_from_32 e which =
    match which with
      | 0 -> V.Cast(V.CAST_LOW, V.REG_8, e)
      | 3 -> V.Cast(V.CAST_HIGH, V.REG_8, e)
      | _ -> make_extract V.REG_8 which e

  let extract_8_from_16 e which =
    match which with
      | 0 -> V.Cast(V.CAST_LOW, V.REG_8, e)
      | 1 -> V.Cast(V.CAST_HIGH, V.REG_8, e)
      | _ -> failwith "bad which in extract_8_from_16"

  let extract_16_from_64 e which =
    match which with
      | 0 -> V.Cast(V.CAST_LOW, V.REG_16, e)
      | 6 -> V.Cast(V.CAST_HIGH, V.REG_16, e)
      | _ -> make_extract V.REG_16 which e

  let extract_16_from_32 e which =
    match which with
      | 0 -> V.Cast(V.CAST_LOW, V.REG_16, e)
      | 2 -> V.Cast(V.CAST_HIGH, V.REG_16, e)
      | _ -> failwith "bad which in extract_16_from_32"

  let extract_32_from_64 e which =
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

  let divide1  = binop V.DIVIDE
  let divide8  = binop V.DIVIDE
  let divide16 = binop V.DIVIDE
  let divide32 = binop V.DIVIDE
  let divide64 = binop V.DIVIDE

  let sdivide1  = binop V.SDIVIDE
  let sdivide8  = binop V.SDIVIDE
  let sdivide16 = binop V.SDIVIDE
  let sdivide32 = binop V.SDIVIDE
  let sdivide64 = binop V.SDIVIDE

  let mod1  = binop V.MOD
  let mod8  = binop V.MOD
  let mod16 = binop V.MOD
  let mod32 = binop V.MOD
  let mod64 = binop V.MOD

  let smod1  = binop V.SMOD
  let smod8  = binop V.SMOD
  let smod16 = binop V.SMOD
  let smod32 = binop V.SMOD
  let smod64 = binop V.SMOD

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
end

class virtual query_engine = object(self)
  method virtual prepare : V.var list -> V.var list -> unit
  method virtual assert_eq : V.var -> V.exp -> unit
  method virtual query : V.exp -> bool * ((string * int64) list)
  method virtual unprepare : unit
end

class stpvc_engine = object(self)
  inherit query_engine

  val vc = Stpvc.create_validity_checker ()
  val mutable ctx = None
  val mutable free_vars = []

  method private ctx =
    match ctx with
      | Some c -> c
      | None -> failwith "Missing ctx in stpvc_engine"

  method prepare free_vars_a temp_vars =
    Libstp.vc_push vc;
    free_vars <- free_vars_a;
    ctx <- Some(Vine_stpvc.new_ctx vc (free_vars_a @ temp_vars))

  method assert_eq var rhs =
    let form = V.BinOp(V.EQ, V.Lval(V.Temp(var)), rhs) in
      (* Printf.printf "Asserting %s\n" (V.exp_to_string form); *)
      Stpvc.do_assert vc
	(Stpvc.e_bvbitextract vc
	   (Vine_stpvc.vine_to_stp vc self#ctx form) 0)

  method query e =
    let s = (Stpvc.e_simplify vc
	       (Stpvc.e_not vc
		  (Stpvc.e_bvbitextract vc
		     (Vine_stpvc.vine_to_stp vc self#ctx e) 0)))
    in
      (* Printf.printf "STP formula is %s\n" (Stpvc.to_string s);
	 flush stdout; *)
    let result = Stpvc.query vc s in
    let ce = if result then [] else
      (*
      List.map
	(fun (sym, stp_exp) ->
	   ((Stpvc.to_string sym), (fun () -> (Stpvc.int64_of_e stp_exp))))
	(Stpvc.get_true_counterexample vc) *)
      let wce = Stpvc.get_whole_counterexample vc in
	List.map (fun (var, e) -> (var, Stpvc.int64_of_e e))
	  (List.filter (fun (var, e) ->
			  (Libstp.getExprKind e) = Libstp.BVCONST)
	     (List.map
		(fun ((n,s,t) as var) ->
		   let var_e = V.Lval(V.Temp(var)) in
		   let var_s = Vine_stpvc.vine_to_stp vc self#ctx var_e
		   in
		     (s ^ "_" ^ (string_of_int n), 
		      (Stpvc.get_term_from_counterexample vc var_s wce)))
		free_vars))
    in
      (result, ce)

  method unprepare =
    Libstp.vc_clearDecls vc;
    Libstp.vc_pop vc;
    ctx <- None;
    Libstp.vc_push vc;
    Libstp.vc_pop vc;
end

let opt_stp_path = ref "stp"

let map_lines f chan =
  let results = ref [] in
    (try while true do
       results := (f (input_line chan)) :: !results
     done
     with End_of_file -> ());
    List.rev !results

let parse_counterex line =
  assert((String.sub line 0 8) = "ASSERT( ");
  assert((String.sub line ((String.length line) - 4) 4) = "  );");
  let trimmed = String.sub line 8 ((String.length line) - 12) in
  let eq_loc = String.index trimmed '=' in
  let lhs = String.sub trimmed 0 eq_loc and
      rhs = (String.sub trimmed (eq_loc + 1)
	       ((String.length trimmed) - eq_loc - 1)) in
  (* let uscore_loc = String.index lhs '_' in
     let varname = String.sub lhs 0 uscore_loc in *)
    assert((String.sub lhs ((String.length lhs) - 2) 2) = "  ");
    let varname = String.sub lhs 0 ((String.length lhs) - 2) in
      assert((String.sub rhs 0 5) = " 0hex");
      let hex_val = String.sub rhs 5 ((String.length rhs) - 5) in
	(* Printf.printf "%s = %Lx\n" varname
	   (Int64.of_string ("0x" ^ hex_val)); *)
	(varname, (Int64.of_string ("0x" ^ hex_val)))

class stp_external_engine fname = object(self)
  inherit query_engine

  val mutable chan = None
  val mutable visitor = None

  method private chan =
    match chan with
      | Some c -> c
      | None -> failwith "Missing output channel in stp_external_engine"

  method private visitor =
    match visitor with
      | Some v -> v
      | None -> failwith "Missing visitor in stp_external_engine"

  method prepare free_vars temp_vars =
    chan <- Some(open_out fname);
    visitor <- Some(new Stp.vine_cvcl_print_visitor (output_string self#chan));
    List.iter self#visitor#declare_var free_vars

  method assert_eq var rhs =
    try
      self#visitor#declare_var_value var rhs
    with
      | V.TypeError(err) ->
	  Printf.printf "Typecheck failure on %s: %s\n"
	    (V.exp_to_string rhs) err;
	  failwith "Typecheck failure in assert_eq"

  method query e =
    output_string self#chan "QUERY(NOT ";
    ignore(V.exp_accept (self#visitor :> V.vine_visitor) e);
    output_string self#chan ");\n";
    output_string self#chan "COUNTEREXAMPLE;\n";
    close_out self#chan;
    chan <- None;
    let rcode = Sys.command (!opt_stp_path ^ " " ^ fname 
			     ^ " >" ^ fname ^ ".out") in
    let results = open_in (fname ^ ".out") in
      if rcode <> 0 then
	(ignore(Sys.command ("cat " ^ fname ^ ".out"));
	 failwith "Fatal STP error");
      let result_s = input_line results in
	assert(result_s = "Valid." or result_s = "Invalid.");
	let result = (result_s = "Valid.") in
	let ce = map_lines parse_counterex results in
	  close_in results;
	  (result, ce)

  method unprepare =
    visitor <- None
end

class virtual concrete_memory = object(self)
  method virtual store_byte : Int64.t -> int -> unit
  method virtual maybe_load_byte : Int64.t -> int option
  method virtual clear : unit -> unit

  method measure_size = 0

  method load_byte addr =
    match (self#maybe_load_byte addr) with
      | Some b -> b
      | None -> 0

  method store_short addr s =
    self#store_byte addr (s land 0xFF);
    self#store_byte (Int64.add addr 1L) ((s lsr 8) land 0xFF)

  method store_word addr w =
    self#store_byte addr (Int64.to_int (Int64.logand 0xFFL w));
    self#store_byte (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 8)));
    self#store_byte (Int64.add addr 2L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 16)));
    self#store_byte (Int64.add addr 3L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right w 24)))

  method store_long addr l =
    self#store_byte addr (Int64.to_int (Int64.logand 0xFFL l));
    self#store_byte (Int64.add addr 1L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 8)));
    self#store_byte (Int64.add addr 2L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 16)));
    self#store_byte (Int64.add addr 3L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 24)));
    self#store_byte (Int64.add addr 4L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 32)));
    self#store_byte (Int64.add addr 5L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 40)));
    self#store_byte (Int64.add addr 6L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 48)));
    self#store_byte (Int64.add addr 7L)
      (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right l 56)))

  method store_page addr pagestr =
    assert(Int64.logand addr 0xfffL = 0L);
    assert(String.length pagestr = 4096);
    for i = 0 to 4096 do
      self#store_byte (Int64.add addr (Int64.of_int i))
	(Char.code pagestr.[i])
    done

  method load_short addr =
    let b1 = self#load_byte addr
    and b2 = self#load_byte (Int64.add addr 1L)
    in
      b1 lor (b2 lsl 8)

  method maybe_load_short addr = Some (self#load_short addr)
  method maybe_load_word  addr = Some (self#load_word  addr)
  method maybe_load_long  addr = Some (self#load_long  addr)

  method load_word addr =
    let b1 = Int64.of_int (self#load_byte addr)
    and b2 = Int64.of_int (self#load_byte (Int64.add addr 1L))
    and b3 = Int64.of_int (self#load_byte (Int64.add addr 2L))
    and b4 = Int64.of_int (self#load_byte (Int64.add addr 3L))
    in
      Int64.logor 
	(Int64.logor b1 (Int64.shift_left b2 8))
	(Int64.logor (Int64.shift_left b3 16) (Int64.shift_left b4 24))

  method load_long addr =
    let b1 = Int64.of_int (self#load_byte addr)
    and b2 = Int64.of_int (self#load_byte (Int64.add addr 1L))
    and b3 = Int64.of_int (self#load_byte (Int64.add addr 2L))
    and b4 = Int64.of_int (self#load_byte (Int64.add addr 3L))
    and b5 = Int64.of_int (self#load_byte (Int64.add addr 4L))
    and b6 = Int64.of_int (self#load_byte (Int64.add addr 5L))
    and b7 = Int64.of_int (self#load_byte (Int64.add addr 6L))
    and b8 = Int64.of_int (self#load_byte (Int64.add addr 7L))
    in
      Int64.logor
	(Int64.logor 
	   (Int64.logor b1 (Int64.shift_left b2 8))
	   (Int64.logor (Int64.shift_left b3 16) (Int64.shift_left b4 24)))
	(Int64.logor 
	   (Int64.logor (Int64.shift_left b5 32) (Int64.shift_left b6 40))
	   (Int64.logor (Int64.shift_left b7 48) (Int64.shift_left b8 56)))
end

class concrete_string_memory = object(self)
  inherit concrete_memory

  (* The extra page is a hacky way to not crash on address wrap-around *)
  val mem = Array.init 0x100001 (fun _ -> None)

  method store_byte addr b =
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let page_str = match mem.(page) with
      | Some page_str -> page_str
      | None ->
	  let new_page = String.make 4096 '\x00' in
	    mem.(page) <- Some new_page;
	    new_page
    in
      page_str.[idx] <- Char.chr b

  method store_page addr newstr =
    assert(Int64.logand addr 0xfffL = 0L);
    assert(String.length newstr = 4096);
    let page = Int64.to_int (Int64.shift_right addr 12) in
      mem.(page) <- Some newstr

  method load_byte addr =
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
    let page_str = match mem.(page) with
      | Some page_str -> page_str
      | None ->
	  let new_page = String.make 4096 '\x00' in
	    mem.(page) <- Some new_page;
	    new_page
    in
      Char.code page_str.[idx]

  method maybe_load_byte addr = Some (self#load_byte addr)

  method clear () =
    Array.fill mem 0 0x100001 None

  method measure_size = 
    Array.fold_right
      (fun page c -> c + match page with None -> 0 | Some _ -> 4096)
      mem 0
end

class concrete_hash_memory = object(self)
  inherit concrete_memory

  val mem = Hashtbl.create 1000

  method store_byte addr b =
    Hashtbl.replace mem addr b

  method maybe_load_byte addr =
    try
      Some (Hashtbl.find mem addr)
    with
	Not_found -> None

  method clear () =
    Hashtbl.clear mem
end

class concrete_snapshot_memory main diff = object(self)
  inherit concrete_memory
    
  val mutable have_snap = false

  method store_byte addr b =
    (if have_snap then diff else main)#store_byte addr b

  method maybe_load_byte addr =
    if have_snap then
      match diff#maybe_load_byte addr with
	| Some b -> Some b
	| None -> main#maybe_load_byte addr
    else
      main#maybe_load_byte addr

  method clear () = 
    diff#clear ();
    main#clear ()

  method make_snap () =
    have_snap <- true

  method reset () = 
    diff#clear ()
end

class parallel_check_memory mem1 mem2 = object(self)
  inherit concrete_memory

  method store_byte addr b =
    Printf.printf "mem[%08Lx]:b := %02x\n" addr b;
    mem1#store_byte addr b;
    mem2#store_byte addr b

  method store_short addr s =
    Printf.printf "mem[%08Lx]:s := %04x\n" addr s;
    mem1#store_short addr s;
    mem2#store_short addr s

  method store_word addr w =
    Printf.printf "mem[%08Lx]:w := %08Lx\n" addr w;
    mem1#store_word addr w;
    mem2#store_word addr w

  method store_long addr l =
    Printf.printf "mem[%08Lx]:l := %016Lx\n" addr l;
    mem1#store_long addr l;
    mem2#store_long addr l

  method load_byte addr =
    let b1 = mem1#load_byte addr and
	b2 = mem2#load_byte addr in
      if b1 = b2 then
	Printf.printf "mem[%08Lx] is %02x\n" addr b1
      else
	(Printf.printf "mem[%08Lx] mismatch %02x vs %02x\n" addr b1 b2;
	 failwith "Mismatch in load_byte");
      b1

  method load_short addr =
    let s1 = mem1#load_short addr and
	s2 = mem2#load_short addr in
      if s1 = s2 then
	Printf.printf "mem[%08Lx] is %04x\n" addr s1
      else
	(Printf.printf "mem[%08Lx] mismatch %04x vs %04x\n" addr s1 s2;
	 failwith "Mismatch in load_short");
      s1

  method load_word addr =
    let w1 = mem1#load_word addr and
	w2 = mem2#load_word addr in
      if w1 = w2 then
	Printf.printf "mem[%08Lx] is %08Lx\n" addr w1
      else
	(Printf.printf "mem[%08Lx] mismatch %08Lx vs %08Lx\n" addr w1 w2;
	 failwith "Mismatch in load_word");
      w1

  method load_long addr =
    let l1 = mem1#load_long addr and
	l2 = mem2#load_long addr in
      if l1 = l2 then
	Printf.printf "mem[%08Lx] is %016Lx\n" addr l1
      else
	(Printf.printf "mem[%08Lx] mismatch %016Lx vs %016Lx\n" addr l1 l2;
	 failwith "Mismatch in load_long");
      l1

  method maybe_load_byte addr = Some (self#load_byte addr)

  method clear     () = mem1#clear ();     mem2#clear ();
    Printf.printf "-------- clear --------\n"

  method make_snap () = mem1#make_snap (); mem2#make_snap ();
    Printf.printf "-------- make_snap --------\n"

  method reset     () = mem1#reset ();     mem2#reset ();
    Printf.printf "-------- reset --------\n"
end

module GranularMemoryFunctor =
  functor (D : DOMAIN) ->
struct
  let split64 l = ((D.extract_32_from_64 l 0), (D.extract_32_from_64 l 4))
  let split32 l = ((D.extract_16_from_32 l 0), (D.extract_16_from_32 l 2))
  let split16 l = ((D. extract_8_from_16 l 0), (D. extract_8_from_16 l 1))
    
  (* At the moment, there are still some calls to endian_i, but there
     are other places where endianness checking is missing, so assume
     little-endian for the time being.
     
     let endianness = V.Little
     
     let endian_i n k = 
     match endianness with
     | V.Little -> k
     | V.Big -> n - k  
  *)

  let endian_i n k = k


  type gran8 = Byte of D.t
	       | Absent8

  type gran16 = Short of D.t
		| Gran8s of gran8 * gran8
		| Absent16
      
  type gran32 = Word of D.t
		| Gran16s of gran16 * gran16
		| Absent32

  type gran64 = Long of D.t
		| Gran32s of gran32 * gran32
		| Absent64

  let  gran8_get_byte  g8  missing addr =
    match g8 with
      | Byte l -> (l, g8)
      | Absent8 ->
	  let l = missing 8 addr in
	    (l, Byte l)

  let gran16_get_byte  g16 missing addr which =
    assert(which >= 0); assert(which < 2);
    match g16, Absent8, Absent8 with
      | Short(l),_,_ -> (D.extract_8_from_16 l (endian_i 2 which), g16)
      | Gran8s(g1, g2),_,_
      | Absent16, g1, g2 ->
	  if which < 1 then
	    let (l, g1') = gran8_get_byte g1 missing addr in
	      (l, Gran8s(g1', g2))
	  else
	    let (l, g2') = gran8_get_byte g2 missing (Int64.add addr 1L) in
	      (l, Gran8s(g1, g2'))
		
  let gran32_get_byte  g32 missing addr which =
    assert(which >= 0); assert(which < 4);
    match g32, Absent16, Absent16 with
      | Word(l),_,_ -> (D.extract_8_from_32 l (endian_i 4 which), g32)
      | Gran16s(g1, g2),_,_
      | Absent32, g1, g2 ->
	  if which < 2 then
	    let (l, g1') = gran16_get_byte g1 missing addr which in
	      (l, Gran16s(g1', g2))
	  else
	    let (l, g2') = gran16_get_byte g2 missing (Int64.add addr 2L) 
	      (which - 2) in
	      (l, Gran16s(g1, g2'))

  let gran64_get_byte  g64 missing addr which =
    assert(which >= 0); assert(which < 8);
    match g64, Absent32, Absent32 with
      | Long(l),_,_ -> (D.extract_8_from_64 l (endian_i 8 which), g64)
      | Gran32s(g1, g2),_,_
      | Absent64, g1, g2 ->
	  if which < 4 then
	    let (l, g1') = gran32_get_byte g1 missing addr which in
	      (l, Gran32s(g1', g2))
	  else
	    let (l, g2') = gran32_get_byte g2 missing (Int64.add addr 4L)
	      (which - 4) in
	      (l, Gran32s(g1, g2'))

  let gran16_get_short g16 missing addr =
    match g16 with
      | Short(l) -> (l, g16)
      | Gran8s(g1, g2) ->
	  let (b1, g1') = gran8_get_byte g1 missing addr and
	      (b2, g2') = gran8_get_byte g2 missing (Int64.add addr 1L) in
	    (D.reassemble16 b1 b2, Gran8s(g1', g2'))
      | Absent16 ->
	  let l = missing 16 addr in
	    (l, Short l)

  let gran32_get_short g32 missing addr which =
    assert(which = 0 or which = 2);
    match g32, Absent16, Absent16 with
      | Word(l),_,_ -> (D.extract_16_from_32 l (endian_i 4 which), g32)
      | Gran16s(g1, g2),_,_
      | Absent32, g1, g2 ->
	  if which < 2 then
	    let (l, g1') = gran16_get_short g1 missing addr in
	      (l, Gran16s(g1', g2))
	  else
	    let (l, g2') = gran16_get_short g2 missing (Int64.add addr 2L) in
	      (l, Gran16s(g1, g2'))

  let gran64_get_short g64 missing addr which =
    assert(which = 0 or which = 2 or which = 4 or which = 6);
    match g64, Absent32, Absent32 with
      | Long(l),_,_ -> (D.extract_16_from_64 l (endian_i 8 which), g64)
      | Gran32s(g1, g2),_,_
      | Absent64, g1, g2 ->
	  if which < 4 then
	    let (l, g1') = gran32_get_short g1 missing addr which in
	      (l, Gran32s(g1', g2))
	  else
	    let (l, g2') = gran32_get_short g2 missing (Int64.add addr 4L) 
	      (which - 4) in
	      (l, Gran32s(g1, g2'))
		
  let gran32_get_word  g32 missing addr =
    match g32 with
      | Word(l) -> (l, g32)
      | Gran16s(g1, g2) ->
	  let (s1, g1') = gran16_get_short g1 missing addr and
	      (s2, g2') = gran16_get_short g2 missing (Int64.add addr 2L) in
	    (D.assemble32 s1 s2, Gran16s(g1', g2'))
      | Absent32 ->
	  let l = missing 32 addr in
	    (l, Word l)
	      
  let gran64_get_word  g64 missing addr which =
    assert(which = 0 or which = 4);
    match g64, Absent32, Absent32 with
      | Long(l),_,_ -> (D.extract_32_from_64 l (endian_i 8 which), g64)
      | Gran32s(g1, g2),_,_
      | Absent64, g1, g2 ->
	  if which < 4 then
	    let (l, g1') = gran32_get_word g1 missing addr in
	      (l, Gran32s(g1', g2))
	  else
	    let (l, g2') = gran32_get_word g2 missing (Int64.add addr 4L) in
	      (l, Gran32s(g1, g2'))

  let gran64_get_long  g64 missing addr  =
    match g64 with
      | Long(l) -> (l, g64)
      | Gran32s(g1, g2) ->
	  let (w1, g1') = gran32_get_word g1 missing addr and
	      (w2, g2') = gran32_get_word g2 missing (Int64.add addr 4L) in
	    (D.assemble64 w1 w2, Gran32s(g1', g2'))
      | Absent64 -> 
	  let l = missing 64 addr in
	    (l, Long l)
	      
  let gran64_split g64 = 
    match g64 with
      | Gran32s(g1, g2) -> (g1, g2)
      | Long(l) -> let (w1, w2) = split64 l in (Word(w1), Word(w2))
      | Absent64 -> (Absent32, Absent32)

  let gran32_split g32 = 
    match g32 with
      | Gran16s(g1, g2) -> (g1, g2)
      | Word(l) ->
	  let (s1, s2) = split32 l in (Short(s1), Short(s2))
      | Absent32 -> (Absent16, Absent16)
	  
  let gran16_split g16 = 
    match g16 with
      | Gran8s(g1, g2) -> (g1, g2)
      | Short(l) ->
	  let (b1, b2) = split16 l in (Byte(b1), Byte(b2))
      | Absent16 -> (Absent8, Absent8)

  let gran16_put_byte g16 which b =
    assert(which = 0 or which = 1);
    let (g1, g2) = gran16_split g16 in
      if which < 1 then
	Gran8s(Byte(b), g2)
      else
	Gran8s(g1, Byte(b))

  let gran32_put_byte g32 which b =
    assert(which >= 0); assert(which < 4);
    let (g1, g2) = gran32_split g32 in
      if which < 2 then
	Gran16s((gran16_put_byte g1 which b), g2)
      else
	Gran16s(g1, (gran16_put_byte g2 (which - 2) b))
	  
  let gran64_put_byte g64 which b =
    assert(which >= 0); assert(which < 8);
    let (g1, g2) = gran64_split g64 in
      if which < 4 then
	Gran32s((gran32_put_byte g1 which b), g2)
      else
	Gran32s(g1, (gran32_put_byte g2 (which - 4) b))

  let gran32_put_short g32 which s =
    assert(which = 0 or which = 2);
    let (g1, g2) = gran32_split g32 in
      if which < 2 then
	Gran16s(Short(s), g2)
      else
	Gran16s(g1, Short(s))
	  
  let gran64_put_short g64 which s =
    assert(which = 0 or which = 2 or which = 4 or which = 6);
    let (g1, g2) = gran64_split g64 in
      if which < 4 then
	Gran32s((gran32_put_short g1 which s), g2)
      else
	Gran32s(g1, (gran32_put_short g2 (which - 4) s))

  let gran64_put_word g64 which w =
    assert(which = 0 or which = 4);
    let (g1, g2) = gran64_split g64 in
      if which < 4 then
	Gran32s(Word(w), g2)
      else
	Gran32s(g1, Word(w))

  let gran8_to_string g8 =
    match g8 with
      | Byte(b) -> D.to_string_8 b
      | Absent8 -> "__"

  let gran16_to_string g16 =
    match g16 with
      | Short(s) -> D.to_string_16 s
      | Gran8s(g1, g2) -> (gran8_to_string g1) ^ "|" ^ (gran8_to_string g2)
      | Absent16 -> "____"

  let gran32_to_string g32 =
    match g32 with
      | Word(w) -> D.to_string_32 w 
      | Gran16s(g1, g2) -> (gran16_to_string g1) ^ "|" ^ (gran16_to_string g2)
      | Absent32 -> "________"
	  
  let gran64_to_string g64 =
    match g64 with
      | Long(l) -> D.to_string_64 l
      | Gran32s(g1, g2) -> (gran32_to_string g1) ^ "|" ^ (gran32_to_string g2)
      | Absent64 -> "________________"

  let gran8_size g8 =
    match g8 with
      | Byte(b) -> D.measure_size b
      | Absent8 -> 1
	  
  let gran16_size g16 =
    match g16 with
      | Short(s) -> D.measure_size s
      | Gran8s(g1, g2) -> (gran8_size g1) + (gran8_size g2)
      | Absent16 -> 1

  let gran32_size g32 =
    match g32 with
      | Word(w) -> D.measure_size w
      | Gran16s(g1, g2) -> (gran16_size g1) + (gran16_size g2)
      | Absent32 -> 1

  let gran64_size g64 =
    match g64 with
      | Long(l) -> D.measure_size l
      | Gran32s(g1, g2) -> (gran32_size g1) + (gran32_size g2)
      | Absent64 -> 1


  class virtual granular_memory = object(self)
    val mutable missing : (int -> int64 -> D.t) =
      (fun _ -> failwith "Must call on_missing")
	
    method on_missing m = missing <- m
      
    method private virtual with_chunk : int64 ->
      (gran64 -> int64 -> int -> (D.t * gran64)) -> D.t option

    method private maybe_load_divided addr bits bytes load assemble =
      let mb0 = load addr and
	  mb1 = load (Int64.add addr bytes) in
	match (mb0, mb1) with
	  | (None, None) -> None
	  | _ ->
	      let b0 = (match mb0 with
			  | Some b -> b
			  | None -> (missing bits addr)) and
		  b1 = (match mb1 with
			  | Some b -> b
			  | None -> (missing bits (Int64.add addr bytes))) in
		Some (assemble b0 b1)

    method maybe_load_byte addr =
      self#with_chunk addr
	(fun chunk caddr which -> gran64_get_byte chunk missing caddr which)

    method maybe_load_short addr =
      if (Int64.logand addr 1L) = 0L then
	self#with_chunk addr
	  (fun chunk caddr which -> gran64_get_short chunk missing caddr which)
      else
	self#maybe_load_divided addr 8 1L self#maybe_load_byte D.reassemble16

    method maybe_load_word addr =
      if (Int64.logand addr 3L) = 0L then
	self#with_chunk addr
	  (fun chunk caddr which -> gran64_get_word chunk missing caddr which)
      else
	self#maybe_load_divided addr 16 2L self#maybe_load_short D.reassemble32

    method maybe_load_long addr =
      if (Int64.logand addr 7L) = 0L then
	self#with_chunk addr
	  (fun chunk caddr _ -> gran64_get_long chunk missing caddr)
      else
	self#maybe_load_divided addr 32 4L self#maybe_load_word D.reassemble64

    method load_byte addr =
      match self#maybe_load_byte addr with
	| Some b -> b
	| None ->
	    let b = missing 8 addr in 
	      self#store_byte addr b;
	      b

    method load_short addr =
      match self#maybe_load_short addr with
	| Some s -> s
	| None ->
	    let s = missing 16 addr in
	      self#store_short addr s;
	      s

    method load_word addr =
      match self#maybe_load_word addr with
	| Some w -> w
	| None ->
	    let w = missing 32 addr in
	      self#store_word addr w;
	      w

    method load_long addr =
      match self#maybe_load_long addr with
	| Some l -> l
	| None ->
	    let l = missing 64 addr in
	      self#store_word addr l;
	      l

    method private virtual store_common_fast : int64 ->
      (gran64 -> int -> gran64) -> unit

    method store_byte addr b =
      self#store_common_fast addr
	(fun chunk which -> gran64_put_byte chunk which b)
	
    method store_short addr s =
      if (Int64.logand addr 1L) = 0L then
	self#store_common_fast addr
	  (fun chunk which -> gran64_put_short chunk which s)
      else
	(* unaligned slow path *)
	let (b0, b1) = split16 s in
	  self#store_byte addr b0;
	  self#store_byte (Int64.add addr 1L) b1

    method store_word addr w =
      if (Int64.logand addr 3L) = 0L then
	self#store_common_fast addr
	  (fun chunk which -> gran64_put_word chunk which w)
      else
	(* unaligned slow path *)
	let (s0, s1) = split32 w in
	  self#store_short addr s0;
	  self#store_short (Int64.add addr 2L) s1

    method store_long addr l =
      if (Int64.logand addr 7L) = 0L then
	self#store_common_fast addr
	  (fun _ _ -> Long(l))
      else
	(* unaligned slow path *)
	let (w0, w1) = split64 l in
	  self#store_word addr w0;
	  self#store_word (Int64.add addr 4L) w1
	    
    method store_page (addr:int64) (p:string) : unit
      = failwith "store_page not supported"
	    
    method virtual clear : unit -> unit

    method virtual measure_size : int

  (* method make_snap () = failwith "make_snap unsupported"; ()
     method reset () = failwith "reset unsupported"; () *)
  end
    
  class granular_page_memory = object(self)
    inherit granular_memory

    (* The extra page is a hacky way to not crash on address wrap-around *)
    val mem = Array.init 0x100001 (fun _ -> None)

    method private with_chunk addr fn =
      let page = Int64.to_int (Int64.shift_right addr 12) and
	  idx = Int64.to_int (Int64.logand addr 0xfffL) in
	match mem.(page) with
	  | None -> None
	  | Some page ->
	      let chunk_n = idx asr 3 and
		  which = idx land 0x7 in
	      let caddr = (Int64.sub addr (Int64.of_int which)) and
		  chunk = page.(chunk_n) in 
		match chunk with
		  | Absent64 -> None
		  | g64 ->
		      let (l, chunk') = fn page.(chunk_n) caddr which in
			page.(chunk_n) <- chunk';
			Some l

    method private get_page addr =
      let page_n = Int64.to_int (Int64.shift_right addr 12) in
	match mem.(page_n) with
	  | Some page -> page
	  | None ->
	      let new_page = Array.init 512 (fun _ -> Absent64) in
		mem.(page_n) <- Some new_page;
		new_page

    method private store_common_fast addr fn =
      let page = self#get_page addr and
	  idx = Int64.to_int (Int64.logand addr 0xfffL) in
      let chunk = idx asr 3 and
	  which = idx land 0x7 in
	page.(chunk) <- fn page.(chunk) which

    method private chunk_to_string addr =
      let page = self#get_page addr and
	  idx = Int64.to_int (Int64.logand addr 0xfffL) in
      let chunk = idx asr 3 in
	"[" ^ (gran64_to_string page.(chunk)) ^ "]"

    method clear () =
      Array.fill mem 0 0x100001 None

    method measure_size =
      let sum_some f ary =
	Array.fold_left
	  (fun n x -> n + match x with None -> 0 | Some(x') -> f x') 0 ary
      in
	sum_some
	  (fun page -> Array.fold_left 
	     (fun n g64 -> n+ gran64_size g64) 0 page) mem
  end

  class granular_hash_memory = object(self)
    inherit granular_memory

    val mem = Hashtbl.create 101

    method private with_chunk addr fn =
      let which = Int64.to_int (Int64.logand addr 0x7L) in
      let caddr = Int64.sub addr (Int64.of_int which) in
	try
	  let chunk = Hashtbl.find mem caddr in
	    match chunk with
	      | Absent64 -> None
	      | g64 -> 
		  let (l, chunk') = fn chunk caddr which in
		    Hashtbl.replace mem caddr chunk';
		    Some l
	with
	    Not_found -> None

    method private store_common_fast addr fn =
      let which = Int64.to_int (Int64.logand addr 0x7L) in
      let caddr = Int64.sub addr (Int64.of_int which) in
      let chunk = try
	Hashtbl.find mem caddr
      with Not_found ->
	Absent64
      in
	Hashtbl.replace mem caddr (fn chunk which)

    method private chunk_to_string addr =
      let caddr = Int64.logand addr (Int64.lognot 0x7L) in
      let chunk = Hashtbl.find mem caddr in
	"[" ^ (gran64_to_string chunk) ^ "]"

    method clear () =
      Hashtbl.clear mem

    method measure_size =
      Hashtbl.fold (fun k v sum -> sum + gran64_size v) mem 0
  end

  class granular_snapshot_memory
    (main:granular_memory) (diff:granular_memory) =
  object(self)
    val mutable have_snap = false

    method on_missing main_missing =
      main#on_missing main_missing;
      diff#on_missing
	(fun size addr ->
	   match size with
	     | 8 -> main#load_byte addr
	     | 16 -> main#load_short addr
	     | 32 -> main#load_word addr
	     | 64 -> main#load_long addr
	     | _ -> failwith "Bad size in missing")

    method store_byte addr b =
      if have_snap then
	diff#store_byte addr b
      else
	main#store_byte addr b

    method store_short addr s =
      if have_snap then
	diff#store_short addr s
      else
	main#store_short addr s

    method store_word addr w =
      if have_snap then
	diff#store_word addr w
      else
	main#store_word addr w

    method store_long addr l =
      if have_snap then
	diff#store_long addr l
      else
	main#store_long addr l

    method store_page addr p =
      assert(not have_snap);
      main#store_page addr p

    method maybe_load_byte addr =
      if have_snap then
	match diff#maybe_load_byte addr with
	  | Some b -> Some b
	  | None -> main#maybe_load_byte addr
      else
	main#maybe_load_byte addr

    method load_byte addr =
      if have_snap then
	match diff#maybe_load_byte addr with
	  | Some b -> b
	  | None -> main#load_byte addr
      else
	main#load_byte addr

    method maybe_load_short addr =
      if have_snap then
	match diff#maybe_load_short addr with
	  | Some s -> Some s
	  | None -> main#maybe_load_short addr
      else
	main#maybe_load_short addr

    method load_short addr =
      if have_snap then
	match diff#maybe_load_short addr with
	  | Some s -> s
	  | None -> main#load_short addr
      else
	main#load_short addr

    method maybe_load_word addr =
      if have_snap then
	match diff#maybe_load_word addr with
	  | Some w -> Some w
	  | None -> main#maybe_load_word addr
      else
	main#maybe_load_word addr

    method load_word addr =
      if have_snap then
	match diff#maybe_load_word addr with
	  | Some w -> w
	  | None -> main#load_word addr
      else
	main#load_word addr

    method maybe_load_long addr =
      if have_snap then
	match diff#maybe_load_long addr with
	  | Some l -> Some l
	  | None -> main#maybe_load_long addr
      else
	main#maybe_load_long addr

    method load_long addr =
      if have_snap then
	match diff#maybe_load_long addr with
	  | Some l -> l
	  | None -> main#load_long addr
      else
	main#load_long addr

    method measure_size = diff#measure_size + main#measure_size + 1

    method clear () = 
      diff#clear ();
      main#clear ()
	
    method make_snap () =
      have_snap <- true
	
    method reset () = 
      diff#clear (); ()
  end

  class concrete_adaptor_memory (mem:concrete_memory) = object(self)
    method on_missing (m:int -> int64 -> D.t) = ()

    method store_byte  addr b = mem#store_byte  addr (D.to_concrete_8 b)
    method store_short addr s = mem#store_short addr (D.to_concrete_16 s)
    method store_word  addr w = mem#store_word  addr (D.to_concrete_32 w)
    method store_long  addr l = mem#store_word  addr (D.to_concrete_64 l)

    method load_byte  addr = D.from_concrete_8 (mem#load_byte  addr)
    method load_short addr = D.from_concrete_16(mem#load_short addr)
    method load_word  addr = D.from_concrete_32(mem#load_word  addr)
    method load_long  addr = D.from_concrete_64(mem#load_long  addr)

    method maybe_load_byte  addr = match mem#maybe_load_byte addr with
      | None -> None | Some b -> Some(D.from_concrete_8 b)
    method maybe_load_short  addr = match mem#maybe_load_short addr with
      | None -> None | Some s -> Some(D.from_concrete_16 s)
    method maybe_load_word  addr = match mem#maybe_load_word addr with
      | None -> None | Some w -> Some(D.from_concrete_32 w)
    method maybe_load_long  addr = match mem#maybe_load_long addr with
      | None -> None | Some l -> Some(D.from_concrete_64 l)
  
    method measure_size = mem#measure_size

    method clear () = mem#clear ()
  end

  class concrete_maybe_adaptor_memory
    (mem:concrete_memory) = object(self)
      val mutable missing : (int -> int64 -> D.t) =
	(fun _ -> failwith "Must call on_missing")

      method on_missing m = missing <- m

      method store_byte  addr b = mem#store_byte  addr (D.to_concrete_8 b)
      method store_short addr s = mem#store_short addr (D.to_concrete_16 s)
      method store_word  addr w = mem#store_word  addr (D.to_concrete_32 w)
      method store_long  addr l = mem#store_long  addr (D.to_concrete_64 l)
      method store_page  addr p = mem#store_page  addr p

      method maybe_load_byte  addr =
	match mem#maybe_load_byte addr with
	  | Some b -> Some(D.from_concrete_8 b)
	  | None -> None

      method private unmaybe mb addr = match mb with
	| None -> missing 8 addr
	| Some b -> D.from_concrete_8 b

      method maybe_load_short  addr =
	let mb0 = mem#maybe_load_byte addr and
	    mb1 = mem#maybe_load_byte (Int64.add addr 1L) in
	  match (mb0, mb1) with
	    | (None, None) -> None
	    | _ ->
		let b0 = self#unmaybe mb0 addr and
		    b1 = self#unmaybe mb1 (Int64.add addr 1L) in
		  Some(D.reassemble16 b0 b1)

      method maybe_load_word  addr =
	let mb0 = mem#maybe_load_byte addr and
	    mb1 = mem#maybe_load_byte (Int64.add addr 1L) and
	    mb2 = mem#maybe_load_byte (Int64.add addr 2L) and
	    mb3 = mem#maybe_load_byte (Int64.add addr 3L) in
	  match (mb0, mb1, mb2, mb3) with
	    | (None, None, None, None) -> None
	    | _ ->
		let b0 = self#unmaybe mb0 addr and
		    b1 = self#unmaybe mb1 (Int64.add addr 1L) and
		    b2 = self#unmaybe mb2 (Int64.add addr 2L) and
		    b3 = self#unmaybe mb3 (Int64.add addr 3L) in
		  Some(D.reassemble32 (D.reassemble16 b0 b1)
			 (D.reassemble16 b2 b3))

      method maybe_load_long  addr =
	let mb0 = mem#maybe_load_byte addr and
	    mb1 = mem#maybe_load_byte (Int64.add addr 1L) and
	    mb2 = mem#maybe_load_byte (Int64.add addr 2L) and
	    mb3 = mem#maybe_load_byte (Int64.add addr 3L) and
	    mb4 = mem#maybe_load_byte (Int64.add addr 4L) and
	    mb5 = mem#maybe_load_byte (Int64.add addr 5L) and
	    mb6 = mem#maybe_load_byte (Int64.add addr 6L) and
	    mb7 = mem#maybe_load_byte (Int64.add addr 7L) in
	  match (mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7) with
	    | (None, None, None, None, None, None, None, None) -> None
	    | _ ->
		let b0 = self#unmaybe mb0 addr and
		    b1 = self#unmaybe mb1 (Int64.add addr 1L) and
		    b2 = self#unmaybe mb2 (Int64.add addr 2L) and
		    b3 = self#unmaybe mb3 (Int64.add addr 3L) and
		    b4 = self#unmaybe mb4 (Int64.add addr 4L) and
		    b5 = self#unmaybe mb5 (Int64.add addr 5L) and
		    b6 = self#unmaybe mb6 (Int64.add addr 6L) and
		    b7 = self#unmaybe mb7 (Int64.add addr 7L) in
		  Some
		    (D.reassemble64
		       (D.reassemble32 (D.reassemble16 b0 b1)
			  (D.reassemble16 b2 b3))
		       (D.reassemble32 (D.reassemble16 b4 b5)
			  (D.reassemble16 b6 b7)))

      method load_byte  addr  = 
	match self#maybe_load_byte addr with
	  | Some b -> b
	  | None -> missing 8 addr

      method load_short addr  = 
	match self#maybe_load_short addr with
	  | Some s -> s
	  | None -> missing 16 addr

      method load_word  addr  = 
	match self#maybe_load_word addr with
	  | Some w -> w
	  | None -> missing 32 addr

      method load_long  addr  = 
	match self#maybe_load_long addr with
	  | Some l -> l
	  | None -> missing 64 addr

      method measure_size = mem#measure_size

      method clear () = mem#clear ()
    end
end

let all_present = String.make 512 '\xff'

class string_maybe_memory = object(self)
  inherit concrete_memory

  (* The extra page is a hacky way to not crash on address wrap-around *)
  val mem = Array.init 0x100001 (fun _ -> None)
  val bitmaps = Array.init 0x100001 (fun _ -> None)

  method private maybe_get_pages addr = 
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
      match (mem.(page), bitmaps.(page)) with
	| (Some page_str, Some bitmap) -> Some (page_str, bitmap, idx)
	| (None, None) -> None
	| _ -> failwith "mem vs. bitmaps inconsistency in string_maybe_memory"

  method private get_pages addr = 
    let page = Int64.to_int (Int64.shift_right addr 12) and
	idx = Int64.to_int (Int64.logand addr 0xfffL) in
      match (mem.(page), bitmaps.(page)) with
	| (Some page_str, Some bitmap) -> (page_str, bitmap, idx)
	| (None, None) ->
	    let new_page = String.make 4096 '\x00' and
		new_bitmap = String.make 512 '\x00' in
	      mem.(page) <- Some new_page;
	      bitmaps.(page) <- Some new_bitmap;
	      (new_page, new_bitmap, idx)
	| _ -> failwith "mem vs. bitmaps inconsistency in string_maybe_memory"

  method store_byte addr b =
    let (page_str, bitmap, idx) = self#get_pages addr in
      page_str.[idx] <- Char.chr b;
      let bit = 1 lsl (idx land 7) and
	  bidx = idx lsr 3 in
	bitmap.[bidx] <- (Char.chr ((Char.code bitmap.[bidx]) lor bit))
	
  method store_page addr newstr =
    assert(Int64.logand addr 0xfffL = 0L);
    assert(String.length newstr = 4096);
    let page = Int64.to_int (Int64.shift_right addr 12) in
      mem.(page) <- Some newstr;
      bitmaps.(page) <- Some all_present

  method maybe_load_byte addr =
    match (self#maybe_get_pages addr) with
      | None -> None
      | Some(page_str, bitmap, idx) ->
	  let bit = 1 lsl (idx land 7) and
	      bidx = idx lsr 3 in
	    if (Char.code bitmap.[bidx]) land bit = 0 then
	      None
	    else
	      Some (Char.code page_str.[idx])

  method load_byte addr =
    let (page_str, _, idx) = self#get_pages addr in
      Char.code page_str.[idx]

  method clear () =
    Array.fill mem 0 0x100001 None;
    Array.fill bitmaps 0 0x100001 None

  method measure_size = 
    (Array.fold_right
       (fun page c -> c + match page with None -> 0 | Some _ -> 4096)
       mem 0) +
    (Array.fold_right
       (fun page c -> c + match page with None -> 0 | Some _ -> 512)
       bitmaps 0)
end

let bool64 f = fun a b -> if (f a b) then 1L else 0L

let move_hash src dest =
  V.VarHash.clear dest;
  V.VarHash.iter (fun a b -> V.VarHash.add dest a b) src

class virtual special_handler = object(self)
  method virtual handle_special : string -> bool
end

type register_name = 
  | R_EBP | R_ESP | R_ESI | R_EDI | R_EIP | R_EAX | R_EBX | R_ECX | R_EDX
  | EFLAGSREST | R_CF | R_PF | R_AF | R_ZF | R_SF | R_OF
  | R_CC_OP | R_CC_DEP1 | R_CC_DEP2 | R_CC_NDEP
  | R_DFLAG | R_IDFLAG | R_ACFLAG | R_EMWARN
  | R_LDT | R_GDT | R_CS | R_DS| R_ES | R_FS | R_GS | R_SS
  | R_FTOP | R_FPROUND | R_FC3210 | R_SSEROUND | R_IP_AT_SYSCALL

let reg_to_regstr reg = match reg with
  | R_EBP -> "R_EBP" | R_ESP -> "R_ESP" | R_ESI -> "R_ESI"
  | R_EDI -> "R_EDI" | R_EIP -> "R_EIP" | R_EAX -> "R_EAX" | R_EBX -> "R_EBX"
  | R_ECX -> "R_ECX" | R_EDX -> "R_EDX"
  | EFLAGSREST -> "EFLAGSREST" | R_CF -> "R_CF" | R_PF -> "R_PF"
  | R_AF -> "R_AF"| R_ZF -> "R_ZF" | R_SF -> "R_SF" | R_OF -> "R_OF"
  | R_CC_OP -> "R_CC_OP" | R_CC_DEP1 -> "R_CC_DEP2"
  | R_CC_DEP2 -> "R_CC_DEP2" | R_CC_NDEP -> "R_CC_NDEP"
  | R_DFLAG -> "R_DFLAG" | R_IDFLAG -> "R_IDFLAG" | R_ACFLAG -> "R_ACFLAG"
  | R_EMWARN -> "R_EMWARN"
  | R_LDT -> "R_LDT" | R_GDT -> "R_GDT" | R_CS -> "R_CS" | R_DS -> "R_DS"
  | R_ES -> "R_ES" | R_FS -> "R_FS" | R_GS -> "R_GS"| R_SS -> "R_SS"
  | R_FTOP -> "R_FTOP" | R_FPROUND -> "R_FPROUND" | R_FC3210  -> "R_FC3210"
  | R_SSEROUND -> "R_SSEROUND" | R_IP_AT_SYSCALL -> "R_IP_AT_SYSCALL"

let regstr_to_reg s = match s with
  | "R_EBP" -> R_EBP | "R_ESP" -> R_ESP | "R_ESI" -> R_ESI
  | "R_EDI" -> R_EDI | "R_EIP" -> R_EIP | "R_EAX" -> R_EAX | "R_EBX" -> R_EBX
  | "R_ECX" -> R_ECX | "R_EDX" -> R_EDX
  | "EFLAGSREST" -> EFLAGSREST | "R_CF" -> R_CF | "R_PF" -> R_PF
  | "R_AF" -> R_AF| "R_ZF" -> R_ZF | "R_SF" -> R_SF | "R_OF" -> R_OF
  | "R_CC_OP" -> R_CC_OP | "R_CC_DEP1" -> R_CC_DEP2
  | "R_CC_DEP2" -> R_CC_DEP2 | "R_CC_NDEP" -> R_CC_NDEP
  | "R_DFLAG" -> R_DFLAG | "R_IDFLAG" -> R_IDFLAG | "R_ACFLAG" -> R_ACFLAG
  | "R_EMWARN" -> R_EMWARN
  | "R_LDT" -> R_LDT | "R_GDT" -> R_GDT | "R_CS" -> R_CS | "R_DS" -> R_DS
  | "R_ES" -> R_ES | "R_FS" -> R_FS | "R_GS" -> R_GS| "R_SS" -> R_SS
  | "R_FTOP" -> R_FTOP | "R_FPROUND" -> R_FPROUND | "R_FC3210"  -> R_FC3210
  | "R_SSEROUND" -> R_SSEROUND | "R_IP_AT_SYSCALL" -> R_IP_AT_SYSCALL
  | _ -> failwith ("Unrecognized register name " ^ s)

exception TooManyIterations
exception IllegalInstruction

module FragmentMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module GM = GranularMemoryFunctor(D)
  module FormMan = FormulaManagerFunctor(D)

  class frag_machine = object(self)
    val mem = (new GM.granular_snapshot_memory
		 (new GM.concrete_maybe_adaptor_memory
		    (new string_maybe_memory))
		 (new GM.granular_hash_memory))

    val form_man = new FormMan.formula_manager

    val reg_store = V.VarHash.create 100
    val reg_to_var = Hashtbl.create 100
    val temps = V.VarHash.create 100
    val mutable frag = ([], [])
    val mutable insns = []
    val mutable loop_cnt = 0

    val mutable snap = (V.VarHash.create 1, V.VarHash.create 1)

    method init_prog (dl, sl) =
      List.iter
	(fun ((n,s,t) as v) ->
	   if s <> "mem" then
	     (V.VarHash.add reg_store v (D.uninit);
	      Hashtbl.add reg_to_var (regstr_to_reg s) v)) dl;
      self#set_frag (dl, sl);
      let result = self#run () in
	match result with
	  | "fallthrough" -> ()
	  | _ -> failwith "Initial program should fall through"

    method set_frag (dl, sl) =
      frag <- (dl, sl);
      V.VarHash.clear temps;
      loop_cnt <- 0;
      self#concretize_misc;
      insns <- sl

    method concretize_misc = ()

    method private on_missing_zero_m (m:GM.granular_memory) =
      m#on_missing
	(fun size _ -> match size with
	   | 8  -> D.from_concrete_8  0
	   | 16 -> D.from_concrete_16 0
	   | 32 -> D.from_concrete_32 0L
	   | 64 -> D.from_concrete_64 0L
	   | _ -> failwith "Bad size in on_missing_zero")

    method on_missing_zero =
      self#on_missing_zero_m (mem :> GM.granular_memory)

    method private on_missing_symbol_m (m:GM.granular_memory) name =
      m#on_missing
	(fun size addr -> 
	   match size with
	     | 8  -> form_man#fresh_symbolic_mem_8  name addr
	     | 16 -> form_man#fresh_symbolic_mem_16 name addr
	     | 32 -> form_man#fresh_symbolic_mem_32 name addr
	     | 64 -> form_man#fresh_symbolic_mem_64 name addr
	     | _ -> failwith "Bad size in on_missing_symbol")

    method on_missing_symbol =
      self#on_missing_symbol_m (mem :> GM.granular_memory) "mem"

    method make_x86_regs_symbolic =
      let reg r v =
	self#set_int_var (Hashtbl.find reg_to_var r) v
      in
	reg R_EBP (form_man#fresh_symbolic_32 "initial_ebp");
	reg R_ESP (form_man#fresh_symbolic_32 "initial_esp");
	reg R_ESI (form_man#fresh_symbolic_32 "initial_esi");
	reg R_EDI (form_man#fresh_symbolic_32 "initial_edi");
	reg R_EAX (form_man#fresh_symbolic_32 "initial_eax");
	reg R_EBX (form_man#fresh_symbolic_32 "initial_ebx");
	reg R_ECX (form_man#fresh_symbolic_32 "initial_ecx");
	reg R_EDX (form_man#fresh_symbolic_32 "initial_edx");
	reg R_CS (D.from_concrete_16 0x23);
	reg R_DS (D.from_concrete_16 0x2b);
	reg R_ES (D.from_concrete_16 0x2b);
	reg R_FS (D.from_concrete_16 0x0);
	reg R_GS (D.from_concrete_16 0x63);
	reg R_GDT (D.from_concrete_32 0x60000000L);
	reg R_LDT (D.from_concrete_32 0x61000000L);
	reg R_DFLAG (D.from_concrete_32 1L);
	reg R_ACFLAG (D.from_concrete_32 0L);
	reg R_IDFLAG (D.from_concrete_32 0L);
	reg EFLAGSREST (D.from_concrete_32 0L);
	reg R_PF (D.from_concrete_1 0);
	reg R_CF (D.from_concrete_1 0);
	reg R_AF (D.from_concrete_1 0);
	reg R_SF (D.from_concrete_1 0);
	reg R_OF (D.from_concrete_1 0);
	reg R_ZF (D.from_concrete_1 0);
	(* reg EFLAGSREST (form_man#fresh_symbolic_32 "initial_eflagsrest");*)
	(* Linux user space CS segment: *)
	self#store_byte_conc 0x60000020L 0xff;
	self#store_byte_conc 0x60000021L 0xff;
	self#store_byte_conc 0x60000022L 0x00;
	self#store_byte_conc 0x60000023L 0x00;
	self#store_byte_conc 0x60000024L 0x00;
	self#store_byte_conc 0x60000025L 0xfb;
	self#store_byte_conc 0x60000026L 0xcf;
	self#store_byte_conc 0x60000027L 0x00;
	(* Linux user space DS/ES segment: *)
	self#store_byte_conc 0x60000028L 0xff;
	self#store_byte_conc 0x60000029L 0xff;
	self#store_byte_conc 0x6000002aL 0x00;
	self#store_byte_conc 0x6000002bL 0x00;
	self#store_byte_conc 0x6000002cL 0x00;
	self#store_byte_conc 0x6000002dL 0xf3;
	self#store_byte_conc 0x6000002eL 0xcf;
	self#store_byte_conc 0x6000002fL 0x00;
	(* Linux user space GS segment: *)
	self#store_byte_conc 0x60000060L 0xff;
	self#store_byte_conc 0x60000061L 0xff;
	self#store_byte_conc 0x60000062L 0x00;
	self#store_byte_conc 0x60000063L 0x00;
	self#store_byte_conc 0x60000064L 0x00;
	self#store_byte_conc 0x60000065L 0xf3;
	self#store_byte_conc 0x60000066L 0xcf;
	self#store_byte_conc 0x60000067L 0x62;
	(* Linux kernel space CS segment: *)
	self#store_byte_conc 0x60000070L 0xff;
	self#store_byte_conc 0x60000071L 0xff;
	self#store_byte_conc 0x60000072L 0x00;
	self#store_byte_conc 0x60000073L 0x00;
	self#store_byte_conc 0x60000074L 0x00;
	self#store_byte_conc 0x60000075L 0xfb;
	self#store_byte_conc 0x60000076L 0xcf;
	self#store_byte_conc 0x60000077L 0x00;
	(* Linux kernel space DS/ES segment: *)
	self#store_byte_conc 0x60000078L 0xff;
	self#store_byte_conc 0x60000079L 0xff;
	self#store_byte_conc 0x6000007aL 0x00;
	self#store_byte_conc 0x6000007bL 0x00;
	self#store_byte_conc 0x6000007cL 0x00;
	self#store_byte_conc 0x6000007dL 0xf3;
	self#store_byte_conc 0x6000007eL 0xcf;
	self#store_byte_conc 0x6000007fL 0x00;
	(* ReactOS kernel space FS segment: *)
(* 	self#store_byte_conc 0x60000030L 0x02; (* limit low *) *)
(* 	self#store_byte_conc 0x60000031L 0x00; (* limit mid *) *)
(* 	self#store_byte_conc 0x60000032L 0x00; (* base low *) *)
(* 	self#store_byte_conc 0x60000033L 0xf0; (* base mid-low *) *)
(* 	self#store_byte_conc 0x60000034L 0xdf; (* base mid-high *) *)
(* 	self#store_byte_conc 0x60000035L 0xf3; (* flags *) *)
(* 	self#store_byte_conc 0x60000036L 0xc0; (* flags, limit high *) *)
(* 	self#store_byte_conc 0x60000037L 0xff; (* base high *) *)
	(* Windows 7 kernel space FS segment: *)
	self#store_byte_conc 0x60000030L 0x04; (* limit low *)
	self#store_byte_conc 0x60000031L 0x00; (* limit mid *)
	self#store_byte_conc 0x60000032L 0x00; (* base low *)
	self#store_byte_conc 0x60000033L 0xec; (* base mid-low *)
	self#store_byte_conc 0x60000034L 0x92; (* base mid-high *)
	self#store_byte_conc 0x60000035L 0xf3; (* flags *)
	self#store_byte_conc 0x60000036L 0xc0; (* flags, limit high *)
	self#store_byte_conc 0x60000037L 0x82; (* base high *)
	(* Windows 7 user space FS segment: *)
	self#store_byte_conc 0x60000038L 0x01; (* limit low *)
	self#store_byte_conc 0x60000039L 0x00; (* limit mid *)
	self#store_byte_conc 0x6000003aL 0x00; (* base low *)
	self#store_byte_conc 0x6000003bL 0xe0; (* base mid-low *)
	self#store_byte_conc 0x6000003cL 0x92; (* base mid-high *)
	self#store_byte_conc 0x6000003dL 0xf3; (* flags *)
	self#store_byte_conc 0x6000003eL 0xfd; (* flags, limit high *)
	self#store_byte_conc 0x6000003fL 0x7f; (* base high *)

    method print_x86_regs =
      let reg str r =
	Printf.printf "%s: " str;
	Printf.printf "%s\n"
	  (D.to_string_32 (self#get_int_var (Hashtbl.find reg_to_var r)))
      in
	reg "%eax" R_EAX;
	reg "%ebx" R_EBX;
	reg "%ecx" R_ECX;
	reg "%edx" R_EDX;
	reg "%esi" R_ESI;
	reg "%edi" R_EDI;
	reg "%esp" R_ESP;
	reg "%ebp" R_EBP

    method store_byte  addr b = mem#store_byte  addr b
    method store_short addr s = mem#store_short addr s
    method store_word  addr w = mem#store_word  addr w
    method store_long  addr l = mem#store_long  addr l

    method store_byte_conc  addr b = mem#store_byte addr (D.from_concrete_8 b)
    method store_short_conc addr s = mem#store_short addr(D.from_concrete_16 s)
    method store_word_conc  addr w = mem#store_word addr (D.from_concrete_32 w)
    method store_long_conc  addr l = mem#store_long addr (D.from_concrete_64 l)

    method store_page_conc  addr p = mem#store_page addr p

    method load_byte  addr = mem#load_byte  addr
    method load_short addr = mem#load_short addr
    method load_word  addr = mem#load_word  addr
    method load_long  addr = mem#load_long  addr

    method load_byte_conc  addr = D.to_concrete_8  (mem#load_byte  addr)
    method load_short_conc addr = D.to_concrete_16 (mem#load_short addr)
    method load_word_conc  addr = D.to_concrete_32 (mem#load_word  addr)
    method load_long_conc  addr = D.to_concrete_64 (mem#load_long  addr)

    method make_mem_snap () = mem#make_snap ()
    method mem_reset () = mem#reset ()

    method make_snap () =
      self#make_mem_snap ();
      snap <- (V.VarHash.copy reg_store, V.VarHash.copy temps)

    method reset () =
      self#mem_reset ();
      match snap with (r, t) ->
	move_hash r reg_store;
	move_hash t temps;

    val mutable special_handler_list = ([] : #special_handler list)

    method add_special_handler (h:special_handler) =
      special_handler_list <- h :: special_handler_list

    method handle_special str =
      try
	ignore(List.find (fun h -> (h#handle_special str))
		 special_handler_list);
	true
      with
	  Not_found -> false

    method get_int_var ((_,_,ty) as var) =
      try
	V.VarHash.find reg_store var
      with
	| Not_found ->
	    (try 
	       V.VarHash.find temps var
	     with
	       | Not_found -> V.pp_var print_string var; 
		   failwith "Unknown variable")

    method get_word_var reg =
      D.to_concrete_32 (self#get_int_var (Hashtbl.find reg_to_var reg))

    method set_int_var ((_,_,ty) as var) value =
      try
	ignore(V.VarHash.find reg_store var);
	V.VarHash.replace reg_store var value
      with
	  Not_found ->
	    V.VarHash.replace temps var value

    method set_word_var reg v =
      self#set_int_var (Hashtbl.find reg_to_var reg) (D.from_concrete_32 v)

    val mutable symbol_uniq = 0
      
    method set_word_reg_symbolic reg s =
      self#set_int_var (Hashtbl.find reg_to_var reg)
	(form_man#fresh_symbolic_32 (s ^ "_" ^ (string_of_int symbol_uniq)));
      symbol_uniq <- symbol_uniq + 1

    method handle_load addr_e ty =
      let addr = self#eval_addr_exp addr_e in
      let v =
	(match ty with
	   | V.REG_8 -> self#load_byte addr
	   | V.REG_16 -> self#load_short addr
	   | V.REG_32 -> self#load_word addr
	   | V.REG_64 -> self#load_long addr
	   | _ -> failwith "Unsupported memory type") in
	(v, ty)

    method handle_store addr_e ty rhs_e =
      let addr = self#eval_addr_exp addr_e and
	  value = self#eval_int_exp_simplify rhs_e in
	match ty with
	  | V.REG_8 -> self#store_byte addr value
	  | V.REG_16 -> self#store_short addr value
	  | V.REG_32 -> self#store_word addr value
	  | V.REG_64 -> self#store_long addr value
	  | _ -> failwith "Unsupported type in memory move"

    method eval_int_exp_ty exp =
      match exp with
	| V.BinOp(op, e1, e2) ->
	    let (v1, ty1) = self#eval_int_exp_ty e1 and
		(v2, ty2) = self#eval_int_exp_ty e2 in
	    let ty = 
	      (match op with
		 | V.PLUS | V.MINUS | V.TIMES
		 | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
		 | V.BITAND | V.BITOR | V.XOR
		     -> assert(ty1 = ty2); ty1
		 | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		     -> ty1
		 | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		     -> assert(ty1 = ty2); V.REG_1) in
	    let func =
	      (match (op, ty1) with
		 | (V.PLUS, V.REG_1)  -> D.plus1 
		 | (V.PLUS, V.REG_8)  -> D.plus8 
		 | (V.PLUS, V.REG_16) -> D.plus16
		 | (V.PLUS, V.REG_32) -> D.plus32
		 | (V.PLUS, V.REG_64) -> D.plus64
		 | (V.MINUS, V.REG_1)  -> D.minus1 
		 | (V.MINUS, V.REG_8)  -> D.minus8 
		 | (V.MINUS, V.REG_16) -> D.minus16
		 | (V.MINUS, V.REG_32) -> D.minus32
		 | (V.MINUS, V.REG_64) -> D.minus64
		 | (V.TIMES, V.REG_1)  -> D.times1 
		 | (V.TIMES, V.REG_8)  -> D.times8 
		 | (V.TIMES, V.REG_16) -> D.times16
		 | (V.TIMES, V.REG_32) -> D.times32
		 | (V.TIMES, V.REG_64) -> D.times64
		 | (V.DIVIDE, V.REG_1)  -> D.divide1 
		 | (V.DIVIDE, V.REG_8)  -> D.divide8 
		 | (V.DIVIDE, V.REG_16) -> D.divide16
		 | (V.DIVIDE, V.REG_32) -> D.divide32
		 | (V.DIVIDE, V.REG_64) -> D.divide64
		 | (V.SDIVIDE, V.REG_1)  -> D.sdivide1 
		 | (V.SDIVIDE, V.REG_8)  -> D.sdivide8 
		 | (V.SDIVIDE, V.REG_16) -> D.sdivide16
		 | (V.SDIVIDE, V.REG_32) -> D.sdivide32
		 | (V.SDIVIDE, V.REG_64) -> D.sdivide64
		 | (V.MOD, V.REG_1)  -> D.mod1 
		 | (V.MOD, V.REG_8)  -> D.mod8 
		 | (V.MOD, V.REG_16) -> D.mod16
		 | (V.MOD, V.REG_32) -> D.mod32
		 | (V.MOD, V.REG_64) -> D.mod64
		 | (V.SMOD, V.REG_1)  -> D.smod1 
		 | (V.SMOD, V.REG_8)  -> D.smod8 
		 | (V.SMOD, V.REG_16) -> D.smod16
		 | (V.SMOD, V.REG_32) -> D.smod32
		 | (V.SMOD, V.REG_64) -> D.smod64
		 | (V.LSHIFT, V.REG_1)  -> D.lshift1 
		 | (V.LSHIFT, V.REG_8)  -> D.lshift8 
		 | (V.LSHIFT, V.REG_16) -> D.lshift16
		 | (V.LSHIFT, V.REG_32) -> D.lshift32
		 | (V.LSHIFT, V.REG_64) -> D.lshift64
		 | (V.RSHIFT, V.REG_1)  -> D.rshift1 
		 | (V.RSHIFT, V.REG_8)  -> D.rshift8 
		 | (V.RSHIFT, V.REG_16) -> D.rshift16
		 | (V.RSHIFT, V.REG_32) -> D.rshift32
		 | (V.RSHIFT, V.REG_64) -> D.rshift64
		 | (V.ARSHIFT, V.REG_1)  -> D.arshift1 
		 | (V.ARSHIFT, V.REG_8)  -> D.arshift8 
		 | (V.ARSHIFT, V.REG_16) -> D.arshift16
		 | (V.ARSHIFT, V.REG_32) -> D.arshift32
		 | (V.ARSHIFT, V.REG_64) -> D.arshift64
		 | (V.BITAND, V.REG_1)  -> D.bitand1 
		 | (V.BITAND, V.REG_8)  -> D.bitand8 
		 | (V.BITAND, V.REG_16) -> D.bitand16
		 | (V.BITAND, V.REG_32) -> D.bitand32
		 | (V.BITAND, V.REG_64) -> D.bitand64
		 | (V.BITOR, V.REG_1)  -> D.bitor1 
		 | (V.BITOR, V.REG_8)  -> D.bitor8 
		 | (V.BITOR, V.REG_16) -> D.bitor16
		 | (V.BITOR, V.REG_32) -> D.bitor32
		 | (V.BITOR, V.REG_64) -> D.bitor64
		 | (V.XOR, V.REG_1)  -> D.xor1 
		 | (V.XOR, V.REG_8)  -> D.xor8 
		 | (V.XOR, V.REG_16) -> D.xor16
		 | (V.XOR, V.REG_32) -> D.xor32
		 | (V.XOR, V.REG_64) -> D.xor64
		 | (V.EQ, V.REG_1)  -> D.eq1 
		 | (V.EQ, V.REG_8)  -> D.eq8 
		 | (V.EQ, V.REG_16) -> D.eq16
		 | (V.EQ, V.REG_32) -> D.eq32
		 | (V.EQ, V.REG_64) -> D.eq64
		 | (V.NEQ, V.REG_1)  -> D.neq1 
		 | (V.NEQ, V.REG_8)  -> D.neq8 
		 | (V.NEQ, V.REG_16) -> D.neq16
		 | (V.NEQ, V.REG_32) -> D.neq32
		 | (V.NEQ, V.REG_64) -> D.neq64
		 | (V.LT, V.REG_1)  -> D.lt1 
		 | (V.LT, V.REG_8)  -> D.lt8 
		 | (V.LT, V.REG_16) -> D.lt16
		 | (V.LT, V.REG_32) -> D.lt32
		 | (V.LT, V.REG_64) -> D.lt64
		 | (V.LE, V.REG_1)  -> D.le1 
		 | (V.LE, V.REG_8)  -> D.le8 
		 | (V.LE, V.REG_16) -> D.le16
		 | (V.LE, V.REG_32) -> D.le32
		 | (V.LE, V.REG_64) -> D.le64
		 | (V.SLT, V.REG_1)  -> D.slt1 
		 | (V.SLT, V.REG_8)  -> D.slt8 
		 | (V.SLT, V.REG_16) -> D.slt16
		 | (V.SLT, V.REG_32) -> D.slt32
		 | (V.SLT, V.REG_64) -> D.slt64
		 | (V.SLE, V.REG_1)  -> D.sle1 
		 | (V.SLE, V.REG_8)  -> D.sle8 
		 | (V.SLE, V.REG_16) -> D.sle16
		 | (V.SLE, V.REG_32) -> D.sle32
		 | (V.SLE, V.REG_64) -> D.sle64
		 | _ -> failwith "unexpected binop/type in eval_int_exp_ty")
	    in
	      (func v1 v2), ty
	| V.UnOp(op, e1) ->
	    let (v1, ty1) = self#eval_int_exp_ty e1 in
	    let result = 
	      (match (op, ty1) with
		 | (V.NEG, V.REG_1)  -> D.neg1 v1
		 | (V.NEG, V.REG_8)  -> D.neg8 v1
		 | (V.NEG, V.REG_16) -> D.neg16 v1
		 | (V.NEG, V.REG_32) -> D.neg32 v1
		 | (V.NEG, V.REG_64) -> D.neg64 v1
		 | (V.NOT, V.REG_1)  -> D.not1 v1
		 | (V.NOT, V.REG_8)  -> D.not8 v1
		 | (V.NOT, V.REG_16) -> D.not16 v1
		 | (V.NOT, V.REG_32) -> D.not32 v1
		 | (V.NOT, V.REG_64) -> D.not64 v1
		 | _ -> failwith "unexpected unop/type in eval_int_exp_ty")
	  in
	    result, ty1
	| V.Constant(V.Int(V.REG_1, i)) ->
	    (D.from_concrete_1 (Int64.to_int i)), V.REG_1
	| V.Constant(V.Int(V.REG_8, i)) ->
	    (D.from_concrete_8 (Int64.to_int i)), V.REG_8
	| V.Constant(V.Int(V.REG_16,i)) -> 
	    (D.from_concrete_16 (Int64.to_int i)),V.REG_16
	| V.Constant(V.Int(V.REG_32,i)) -> (D.from_concrete_32 i),V.REG_32
	| V.Constant(V.Int(V.REG_64,i)) -> (D.from_concrete_64 i),V.REG_64
	| V.Constant(V.Int(_,_)) -> failwith "unexpected integer constant type"
	| V.Lval(V.Temp((_,_,ty) as var)) -> (self#get_int_var var), ty
	| V.Lval(V.Mem(memv, idx, ty)) ->
	    self#handle_load idx ty
	| V.Cast(kind, ty, e) ->
	    let (v1, ty1) = self#eval_int_exp_ty e in
	    let func =
	      match (kind, ty1, ty) with
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_8)  -> D.cast1u8
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_16) -> D.cast1u16
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_32) -> D.cast1u32
		| (V.CAST_UNSIGNED, V.REG_1,  V.REG_64) -> D.cast1u64
		| (V.CAST_UNSIGNED, V.REG_8,  V.REG_16) -> D.cast8u16
		| (V.CAST_UNSIGNED, V.REG_8,  V.REG_32) -> D.cast8u32
		| (V.CAST_UNSIGNED, V.REG_8,  V.REG_64) -> D.cast8u64
		| (V.CAST_UNSIGNED, V.REG_16, V.REG_32) -> D.cast16u32
		| (V.CAST_UNSIGNED, V.REG_16, V.REG_64) -> D.cast16u64
		| (V.CAST_UNSIGNED, V.REG_32, V.REG_64) -> D.cast32u64
		| (V.CAST_SIGNED, V.REG_1,  V.REG_8)  -> D.cast1s8
		| (V.CAST_SIGNED, V.REG_1,  V.REG_16) -> D.cast1s16
		| (V.CAST_SIGNED, V.REG_1,  V.REG_32) -> D.cast1s32
		| (V.CAST_SIGNED, V.REG_1,  V.REG_64) -> D.cast1s64
		| (V.CAST_SIGNED, V.REG_8,  V.REG_16) -> D.cast8s16
		| (V.CAST_SIGNED, V.REG_8,  V.REG_32) -> D.cast8s32
		| (V.CAST_SIGNED, V.REG_8,  V.REG_64) -> D.cast8s64
		| (V.CAST_SIGNED, V.REG_16, V.REG_32) -> D.cast16s32
		| (V.CAST_SIGNED, V.REG_16, V.REG_64) -> D.cast16s64
		| (V.CAST_SIGNED, V.REG_32, V.REG_64) -> D.cast32s64
		| (V.CAST_LOW, V.REG_64, V.REG_1)  -> D.cast64l1
		| (V.CAST_LOW, V.REG_64, V.REG_8)  -> D.cast64l8
		| (V.CAST_LOW, V.REG_64, V.REG_16) -> D.cast64l16
		| (V.CAST_LOW, V.REG_64, V.REG_32) -> D.cast64l32
		| (V.CAST_LOW, V.REG_32, V.REG_1)  -> D.cast32l1
		| (V.CAST_LOW, V.REG_32, V.REG_8)  -> D.cast32l8
		| (V.CAST_LOW, V.REG_32, V.REG_16) -> D.cast32l16
		| (V.CAST_LOW, V.REG_16, V.REG_8)  -> D.cast16l8
		| (V.CAST_LOW, V.REG_16, V.REG_1)  -> D.cast16l1
		| (V.CAST_LOW, V.REG_8,  V.REG_1)  -> D.cast8l1
		| (V.CAST_HIGH, V.REG_64, V.REG_1)  -> D.cast64h1
		| (V.CAST_HIGH, V.REG_64, V.REG_8)  -> D.cast64h8
		| (V.CAST_HIGH, V.REG_64, V.REG_16) -> D.cast64h16
		| (V.CAST_HIGH, V.REG_64, V.REG_32) -> D.cast64h32
		| (V.CAST_HIGH, V.REG_32, V.REG_1)  -> D.cast32h1
		| (V.CAST_HIGH, V.REG_32, V.REG_8)  -> D.cast32h8
		| (V.CAST_HIGH, V.REG_32, V.REG_16) -> D.cast32h16
		| (V.CAST_HIGH, V.REG_16, V.REG_8)  -> D.cast16h8
		| (V.CAST_HIGH, V.REG_16, V.REG_1)  -> D.cast16h1
		| (V.CAST_HIGH, V.REG_8,  V.REG_1)  -> D.cast8h1
		| _ -> failwith "bad cast kind in eval_int_exp_ty"
	    in
	      ((func v1), ty)
	(* XXX move this to something like a special handler: *)
	| V.Unknown("rdtsc") -> ((D.from_concrete_64 1L), V.REG_64) 
	| _ -> failwith "Unsupported (or non-int) expr type in eval_int_exp_ty"
	  
    method eval_int_exp exp =
      let (v, _) = self#eval_int_exp_ty exp in
	v

    method eval_int_exp_simplify exp =
      match self#eval_int_exp_ty exp with
	| (v, V.REG_1) -> form_man#simplify1 v
	| (v, V.REG_8) -> form_man#simplify8 v
	| (v, V.REG_16) -> form_man#simplify16 v
	| (v, V.REG_32) -> form_man#simplify32 v
	| (v, V.REG_64) -> form_man#simplify64 v
	| _ -> failwith "Unexpected type in eval_int_exp_simplify"

    method eval_bool_exp exp =
      let v = self#eval_int_exp exp in
	if (D.to_concrete_1 v) = 1 then true else false

    method eval_addr_exp exp =
      let v = self#eval_int_exp exp in
	(D.to_concrete_32 v)

    method eval_label_exp e =
      match e with
	| V.Name(lab) -> lab
	| _ ->
	    let addr = self#eval_addr_exp e in
	      Printf.sprintf "pc_0x%Lx" addr

    method jump lab =
      let rec find_label lab sl =
	match sl with
	  | [] -> None
	  | V.Label(l) :: rest when l = lab -> Some sl
	  | st :: rest -> find_label lab rest
      in
	loop_cnt <- loop_cnt + 1;
	if loop_cnt > 50 then raise TooManyIterations;
	let (_, sl) = frag in
	  match find_label lab sl with
	    | None -> lab
	    | Some sl ->
		self#run_sl sl
	      
    method run_sl sl =
      match sl with
	| [] -> "fallthrough"
	| st :: rest ->
	    (match st with
	       | V.Jmp(l) -> self#jump (self#eval_label_exp l)
	       | V.CJmp(cond, l1, l2) ->
		   let cond_v = self#eval_bool_exp cond in
		     if cond_v then
		       self#jump (self#eval_label_exp l1)
		     else
		       self#jump (self#eval_label_exp l2)
	       | V.Move(V.Temp(v), e) ->
		   self#set_int_var v (self#eval_int_exp_simplify e);
		   self#run_sl rest
	       | V.Move(V.Mem(memv, idx_e, ty), rhs_e) ->
		   self#handle_store idx_e ty rhs_e;
		   self#run_sl rest
	       | V.Special("VEX decode error") ->
		   raise IllegalInstruction
	       | V.Special(str) ->
		   if self#handle_special str then
		     self#run_sl rest
		   else
		     (Printf.printf "Unhandled special %s\n" str;
		      failwith "Unhandled special")
	       | V.Label(_) -> self#run_sl rest
	       | V.ExpStmt(e) ->
		   let v = self#eval_int_exp e in
		     ignore(v);
		     self#run_sl rest
	       | V.Comment(_) -> self#run_sl rest
	       | V.Block(_,_) -> failwith "Block unsupported"
	       | V.Function(_,_,_,_,_) -> failwith "Function unsupported"
	       | V.Return(_) -> failwith "Return unsupported"
	       | V.Call(_,_,_) -> failwith "Call unsupported"
	       | V.Attr(st, _) -> self#run_sl (st :: rest)
	       | V.Assert(e) ->
		   let v = self#eval_bool_exp e in
		     assert(v);
		     self#run_sl rest
	       | V.Halt(e) ->
		   let v = D.to_concrete_32 (self#eval_int_exp e) in
		     Printf.sprintf "halt_%Ld" v)

    method run () = self#run_sl insns

    method measure_size =
      let measure_add k v n = n + (D.measure_size v) in
	mem#measure_size
	+ (V.VarHash.fold measure_add reg_store 0)
	+ (V.VarHash.fold measure_add temps 0)

    method store_byte_idx base idx b =
      self#store_byte (Int64.add base (Int64.of_int idx)) 
	(D.from_concrete_8 b)

    method store_str base idx str =
      for i = 0 to (String.length str - 1) do
	self#store_byte_idx (Int64.add base idx) i (Char.code str.[i])
      done

    method store_symbolic_cstr base len =
      for i = 0 to len - 1 do
	self#store_byte (Int64.add base (Int64.of_int i))
	  (form_man#fresh_symbolic_8 ("input" ^ (string_of_int i)))
      done;
      self#store_byte_idx base len 0

    method store_cstr base idx str =
      self#store_str base idx str;
      self#store_byte_idx (Int64.add base idx) (String.length str) 0

    method read_buf addr len =
      Array.init len
	(fun i -> Char.chr
	   (D.to_concrete_8 (mem#load_byte (Int64.add addr (Int64.of_int i)))))

    method read_cstr addr =
      let rec bytes_loop i =
	let b = D.to_concrete_8 (mem#load_byte
				   (Int64.add addr (Int64.of_int i)))
	in
	  if b = 0 then [] else b :: bytes_loop (i + 1)
      in
	String.concat ""
	  (List.map (fun b -> String.make 1 (Char.chr b))
	     (bytes_loop 0))

    method print_backtrace =
      let read_addr addr =
	try
	  let v = self#load_word_conc addr in
	  (v, Printf.sprintf "0x%08Lx" v)
	with NotConcrete(s) -> (0L, "<symbolic " ^ (V.exp_to_string s) ^ ">")
      in
      let rec loop ebp =
	let (prev_ebp, prev_ebp_s) = read_addr ebp and
	    (_, ret_addr_s) = read_addr (Int64.add ebp 4L) in
	  Printf.printf "0x%08Lx %s %s\n" ebp prev_ebp_s ret_addr_s;
	  if (prev_ebp <> 0L) then
	    loop prev_ebp
      in
	loop (self#get_word_var R_EBP)
  end
end

type decision_tree_node = {
  mutable parent : decision_tree_node option;
  (* None: unexplored; Some None: unsat; Some Some n: sat *)
  mutable f_child : decision_tree_node option option;
  mutable t_child : decision_tree_node option option;
  mutable all_seen : bool;
  mutable ident : int }

let next_dt_ident = ref 1

let new_dt_node the_parent =
  next_dt_ident := !next_dt_ident + 1;
  {parent = the_parent;
   f_child = None; t_child = None;
   all_seen = false; ident = !next_dt_ident}

(* This hash algorithm is FNV-1a,
   c.f. http://www.isthe.com/chongo/tech/comp/fnv/index.html *)
let hash_round h x =
  let h' = Int32.logxor h (Int32.of_int x) in
    Int32.mul h' (Int32.of_int 0x1000193)

exception BoringPath

class decision_tree = object(self)
  val root = new_dt_node None
  val mutable cur = new_dt_node None (* garbage *)
  val mutable depth = 0
  val mutable path_hash = Int64.to_int32 0x811c9dc5L

  method init = cur <- root; self

  method get_hist =
    let child_is kid n =
      match kid with
	| Some (Some n') -> n' == n
	| _ -> false
    in
    let last_choice n p =
      if child_is p.f_child n then
	false
      else if child_is p.t_child n then
	true
      else
	failwith "Parent invariant failure in get_hist"
    in
    let rec loop n =
      match n.parent with
	| None -> []
	| Some p -> (last_choice n p) :: loop p
    in
      loop cur

  method add_kid b =
    match (b, cur.f_child, cur.t_child) with
      | (false, Some(Some kid), _)
      | (true,  _, Some(Some kid)) -> () (* already there *)
      | (false, None, _) ->
	  let new_kid = new_dt_node (Some cur) in
	    cur.f_child <- Some (Some new_kid)
      | (true,  _, None) ->
	  let new_kid = new_dt_node (Some cur) in
	    cur.t_child <- Some (Some new_kid)
      | (false, Some None, _)
      | (true,  _, Some None) ->
	  failwith "Tried to extend an unsat branch"

  method extend b =
    self#add_kid b;
    depth <- depth + 1;
    if depth > 2000 then
      raise BoringPath;
    path_hash <- hash_round path_hash (if b then 49 else 48);
    Random.init (Int32.to_int path_hash);
    match (b, cur.f_child, cur.t_child) with
      | (false, Some(Some kid), _) -> cur <- kid
      | (true,  _, Some(Some kid)) -> cur <- kid
      | (false, None, _)
      | (true,  _, None)
      | (false, Some None, _)
      | (true,  _, Some None) ->
	  failwith "Add_kid failed in extend"

  method set_iter_seed i =
    path_hash <- hash_round path_hash i

  method random_bit =
    Random.bool ()

  method record_unsat b =
    match (b, cur.f_child, cur.t_child) with
      | (false, None, _) ->
	  cur.f_child <- Some None
      | (true,  _, None) ->
	  cur.t_child <- Some None
      | (false, Some None, _)
      | (true,  _, Some None) -> () (* already recorded *)
      | (false, Some (Some _), _)
      | (true,  _, Some (Some _)) ->
	  failwith "Trying to make sat branch unsat in record_unsat"

  method try_extend (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) =
    let known b = 
      non_try_func b;
      self#extend b;
      (b, (trans_func b))
    in
    let known_check b =
      let c = trans_func b in
	if try_func b c then
	  (self#extend b; (b, c))
	else
	  failwith "Unexpected unsat in try_extend"
    in
    let try_or_boring b =
      let c = trans_func b in
	if try_func b c then
	  (self#extend b; (b, c))
	else
	  (self#record_unsat b;
	   self#mark_all_seen_node cur;
	   known (not b))
    in
    let try_both () =
      let b = self#random_bit in
      let c = trans_func b and
	  c' = trans_func (not b) in
	if try_func b c then
	  (if try_func (not b) c' then
	     self#add_kid (not b)
	   else
	     self#record_unsat (not b);
	   self#extend b;
	   (b, c))
        else
	  (self#record_unsat b;
	   if try_func (not b) c' then
	     (self#extend (not b);
	      ((not b), c'))
	   else
	     failwith "Both branches unsat in try_extend")
    in
      assert(not cur.all_seen);
      match (cur.f_child, cur.t_child) with
	| (Some(Some f_kid), Some(Some t_kid)) ->
	    (match (f_kid.all_seen, t_kid.all_seen) with
	       | (true, true) -> 
		   if cur.all_seen then
		     known (self#random_bit)
		   else
		     failwith "all_seen invariant failure"
	       | (false, true) -> known false
	       | (true, false) -> known true
	       | (false, false) -> known (self#random_bit))
	| (Some(Some f_kid), Some None) ->
	    assert(not f_kid.all_seen);
	    known false
	| (Some None, Some(Some t_kid)) ->
	    assert(not t_kid.all_seen);
	    known true
	| (Some None, Some None) -> failwith "Unsat node in try_extend"
	| (Some(Some f_kid), None) ->
	    if f_kid.all_seen then
	      try_or_boring true
	    else
	      try_both ()
	| (None, Some(Some t_kid)) ->
	    if t_kid.all_seen then
	      try_or_boring false
	    else
	      try_both ()
	| (None, Some None) -> known_check false
	| (Some None, None) -> known_check true
	| (None, None) ->
	    try_both ()

  method try_extend_memoryless (trans_func : bool -> V.exp)
    try_func (non_try_func : bool -> unit) =
    let known b = 
      non_try_func b;
      self#extend b;
      (b, (trans_func b))
    in
    let try_branch b c field fn = 
      match field with
	| (Some(Some f_kid)) -> known b
	| Some None -> raise BoringPath
	| None ->
	    if try_func b c then
	      (self#extend b; (b, c))
	    else
	      (self#record_unsat b; (fn b))
    in
    if self#random_bit then
      try_branch false (trans_func false) cur.f_child
	(fun _ -> try_branch true (trans_func true) cur.t_child
	   (fun _ -> failwith "Both branches unsat in try_extend"))
    else
      try_branch true (trans_func true) cur.t_child
	(fun _ -> try_branch false (trans_func false) cur.f_child
	   (fun _ -> failwith "Both branches unsat in try_extend"))

  method private mark_all_seen_node node =
    let rec loop n = 
      n.all_seen <- true;
      match n.parent with
	| None -> ()
	| Some p ->
	    (match (p.all_seen, p.t_child, p.f_child) with
	       | (false, Some(Some f_kid), Some(Some t_kid))
		   when f_kid.all_seen && t_kid.all_seen ->
		   loop p
	       | (false, Some(Some f_kid), Some None)
		   when f_kid.all_seen ->
		   loop p
	       | (false, Some None, Some(Some t_kid))
		   when t_kid.all_seen ->
		   loop p
	       | _ -> ())
    in
      loop node

  method mark_all_seen = self#mark_all_seen_node cur

  method try_again_p = not root.all_seen

  method print_tree chan =
    let kid_to_string mmn =
      match mmn with
	| None -> "unknown"
	| Some None -> "none"
	| Some(Some kid) -> string_of_int kid.ident
    in
    let print_node n =
      Printf.fprintf chan "%d: " n.ident;
      Printf.fprintf chan "%s %s " (kid_to_string n.f_child)
	(kid_to_string n.t_child);
      Printf.fprintf chan "%s %s\n" (if n.all_seen then "*" else "?")
	(kid_to_string (Some n.parent))
    in
    let rec loop n =
      print_node n;
      (match n.f_child with
	 | Some(Some kid) -> loop kid
	 | _ -> ());
      (match n.t_child with
	 | Some(Some kid) -> loop kid
	 | _ -> ());
    in
      loop root

  method reset =
    cur <- root;
    depth <- 0;
    path_hash <- Int64.to_int32 0x811c9dc5L

end

let opt_trace_assigns = ref false
let opt_trace_decisions = ref false
let opt_trace_binary_paths = ref false

module SymPathFragMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module FormMan = FormulaManagerFunctor(D)
  module FM = FragmentMachineFunctor(D)

  class sym_path_frag_machine = object(self)
    inherit FM.frag_machine as fm

    val mutable path_cond = []
    val dt = (new decision_tree)#init

    method add_to_path_cond cond =
      path_cond <- cond :: path_cond
      
    method restore_path_cond f =
      let saved_pc = path_cond in
      let ret = f () in
	path_cond <- saved_pc;
	ret

    (* val query_engine = new stpvc_engine *)
    val query_engine = new stp_external_engine "fuzz.stp"

    method match_input_var s =
      try
	if (String.sub s 0 5) = "input" then
	  let wo_input = String.sub s 5 ((String.length s) - 5) in
	    try
	      let uscore_loc = String.index wo_input '_' in
	      let n_str = String.sub wo_input 0 uscore_loc in
		Some (int_of_string n_str)
	    with
	      | Not_found ->
		  Some (int_of_string wo_input)
	else
	  None
      with
	| Not_found -> None
	| Failure "int_of_string" -> None

    method walk_temps f exp =
      let h = V.VarHash.create 21 in
      let temps = ref [] in
      let rec walk = function
	| V.BinOp(_, e1, e2) -> walk e1; walk e2
	| V.UnOp(_, e1) -> walk e1
	| V.Constant(_) -> ()
	| V.Lval(V.Temp(var)) ->
	    if (try V.VarHash.find h var; true with Not_found -> false) then
	      ()
	    else
	      FormMan.if_expr_temp form_man var
		(fun e ->
		   V.VarHash.replace h var ();
		   walk e;
		   temps := (f var e) :: !temps) ()
	| V.Lval(V.Mem(_, e1, _)) -> walk e1
	| V.Name(_) -> ()
	| V.Cast(_, _, e1) -> walk e1
	| V.Unknown(_) -> ()
	| V.Let(_, _, _) -> failwith "Unhandled let in walk_temps"
      in
	walk exp;
	List.rev !temps

    method private ce_to_input_str ce =
      let str = String.make 30 ' ' in
	List.iter
	  (fun (var_s, value) ->
	     match self#match_input_var var_s with
	       | Some n -> str.[n] <- 
		   char_of_int (Int64.to_int value)
	       | None -> ())
	  ce;
	let str' = ref str in
	  (try 
	     while String.rindex !str' ' ' = (String.length !str') - 1 do
	       str' := String.sub !str' 0 ((String.length !str') - 1)
	     done;
	   with Not_found -> ());
	  (try
	     while String.rindex !str' '\000' = (String.length !str') - 1
	     do
	       str' := String.sub !str' 0 ((String.length !str') - 1)
	     done;
	   with Not_found -> ());
	  !str'

    method print_ce ce =
      List.iter
	(fun (var_s, value) ->
	   let nice_name = (try String.sub var_s 0 (String.rindex var_s '_') 
			    with Not_found -> var_s) in
	     if value <> 0L then
	       Printf.printf "%s=0x%Lx " nice_name value)
	ce;
      Printf.printf "\n";

    method query_with_path_cond cond verbose =
      let pc = form_man#rewrite_for_stp
	(constant_fold_rec
	   (List.fold_left (fun a b -> V.BinOp(V.BITAND, a, b))
	      V.exp_true (List.rev (cond :: path_cond)))) in
      let temps = 
	List.map (fun (var, e) -> (var, form_man#rewrite_for_stp e))
	  (self#walk_temps (fun var e -> (var, e)) pc) in
      let i_vars = form_man#get_input_vars in
      let m_axioms = form_man#get_mem_axioms in
      let t_vars = List.map (fun (v, _) -> v) temps in
      let m_vars = List.map (fun (v, _) -> v) m_axioms in
	query_engine#prepare (Vine_util.list_difference i_vars m_vars)
	  (m_vars @ t_vars);
	List.iter (fun (v, exp) -> (query_engine#assert_eq v exp)) m_axioms;
	List.iter (fun (v, exp) -> (query_engine#assert_eq v exp)) temps;
	(* Printf.printf "PC is %s\n" (V.exp_to_string pc); *)
	(* Printf.printf "Condition is %s\n" (V.exp_to_string cond); *)
	let time_before = Sys.time () in
	let (result, ce) = query_engine#query pc in
	let is_sat = not result in
	  if verbose then
	    (if is_sat then
	       (if !opt_trace_decisions then
		  (Printf.printf "Satisfiable";
		   if !opt_trace_assigns then
		     (Printf.printf " by \"%s\"\n"
			(String.escaped (self#ce_to_input_str ce));
		      self#print_ce ce)
		   else
		     Printf.printf ".\n"))
	     else
	       if !opt_trace_decisions then Printf.printf "Unsatisfiable.\n");
	  if ((Sys.time ()) -. time_before) > 1.0 then
	    Printf.printf "Slow query (%f sec)\n"
	      ((Sys.time ()) -. time_before);
	  flush stdout;
	  query_engine#unprepare;
	  is_sat

    method private try_extend trans_func try_func non_try_func =
      dt#try_extend trans_func try_func non_try_func

    method query_with_pc_random cond verbose =
      let trans_func b =
	if b then cond else V.UnOp(V.NOT, cond)
      in
      let try_func b cond' =
	if verbose && !opt_trace_decisions then
	  Printf.printf "Trying %B: " b;
	self#query_with_path_cond cond' verbose
      in
      let non_try_func b =
	if verbose && !opt_trace_decisions then
	  Printf.printf "Known %B\n" b
      in
	self#try_extend trans_func try_func non_try_func

    method extend_pc_random cond verbose =
      let (result, cond') = self#query_with_pc_random cond verbose in
	self#add_to_path_cond cond';
	result

    method random_case_split verbose =
      let trans_func b = V.Unknown("unused") in
      let try_func b _ =
	if verbose then Printf.printf "Trying %B: " b;
	true
      in
      let non_try_func b =
	if verbose then Printf.printf "Known %B\n" b
      in
      let (result, _) = self#try_extend trans_func try_func non_try_func in
	result

    method eval_bool_exp exp =
      let v = self#eval_int_exp exp in
	try
	  if (D.to_concrete_1 v) = 1 then true else false
	with
	    NotConcrete _ ->
	      (* Printf.printf "Symbolic branch condition %s\n"
		 (V.exp_to_string v#to_symbolic_1); *)
	      self#extend_pc_random (D.to_symbolic_1 v) true

    method eval_addr_exp exp =
      let c32 x = V.Constant(V.Int(V.REG_32, x)) in
      let v = self#eval_int_exp_simplify exp in
	try (D.to_concrete_32 v)
	with
	    NotConcrete _ ->
	      let e = (D.to_symbolic_32 v) in
		Printf.printf "Symbolic address %s\n" (V.exp_to_string e);
		let bits = ref 0L in
		  self#restore_path_cond
		    (fun () ->
		       for b = 31 downto 0 do
			 let bit = self#extend_pc_random
			   (V.Cast(V.CAST_LOW, V.REG_1,
				   (V.BinOp(V.ARSHIFT, e,
					    (c32 (Int64.of_int b)))))) false
			 in
			   bits := (Int64.logor (Int64.shift_left !bits 1)
				      (if bit then 1L else 0L));
		       done);
		  Printf.printf "Picked concrete value 0x%Lx\n" !bits;
		  self#add_to_path_cond (V.BinOp(V.EQ, e, (c32 !bits)));
		  !bits

    method finish_path =
      dt#mark_all_seen;
      if !opt_trace_binary_paths then
	(let path_str = String.concat ""
	   (List.map (fun b -> if b then "1" else "0") (List.rev dt#get_hist));
	 in
	   Printf.printf "Path: %s\n" path_str);
      dt#try_again_p

    method print_tree chan = dt#print_tree chan

    method set_iter_seed i = dt#set_iter_seed i
      
    method reset () =
      fm#reset ();
      form_man#reset_mem_axioms;
      path_cond <- [];
      dt#reset
  end
end

exception SymbolicJump
exception NullDereference

let opt_trace_loads = ref false
let opt_trace_stores = ref false
let opt_trace_regions = ref false
let opt_trace_sym_addrs = ref false

module SymRegionFragMachineFunctor =
  functor (D : DOMAIN) ->
struct
  module FormMan = FormulaManagerFunctor(D)
  module GM = GranularMemoryFunctor(D)
  module SPFM = SymPathFragMachineFunctor(D)

  let is_high_mask ty v =
    let is_power_of_2_or_zero x =
      Int64.logand x (Int64.pred x) = 0L
    in
    let mask64 =
      match ty with
	| V.REG_1 -> fix_s1 v
	| V.REG_8 -> fix_s8 v
	| V.REG_16 -> fix_s16 v
	| V.REG_32 -> fix_s32 v
	| V.REG_64 -> v
	| _ -> failwith "Bad type in is_high_mask"
    in
      is_power_of_2_or_zero (Int64.succ (Int64.lognot mask64))

  let rec ulog2 i =
    match i with
      | 0L -> -1
      | 1L -> 0
      | 2L|3L -> 1
      | 4L|5L|6L|7L -> 2
      | i when i < 16L -> 2 + ulog2(Int64.shift_right i 2)
      | i when i < 256L -> 4 + ulog2(Int64.shift_right i 4)
      | i when i < 65536L -> 8 + ulog2(Int64.shift_right i 8)
      | i when i < 0x100000000L -> 16 + ulog2(Int64.shift_right i 16)
      | _ -> 32 + ulog2(Int64.shift_right i 32)

  let rec narrow_bitwidth e =
    match e with
      | V.Constant(V.Int(ty, v)) -> ulog2 v
      | V.BinOp(V.BITAND, e1, e2) ->
	  min (narrow_bitwidth e1) (narrow_bitwidth e2)
      | V.BinOp(V.BITOR, e1, e2) ->
	  max (narrow_bitwidth e1) (narrow_bitwidth e2)
      | _ -> 64

  let split_terms e form_man =
    let rec loop e =
      match e with
	| V.BinOp(V.PLUS, e1, e2) -> (loop e1) @ (loop e2)
	| V.BinOp(V.BITAND, e, V.Constant(V.Int(ty, v)))
	    when is_high_mask ty v ->
	    (* x & 0xfffffff0 = x - (x & 0xf), etc. *)
	    (loop e) @
	      (loop
		 (V.UnOp(V.NEG,
			 V.BinOp(V.BITAND, e,
				 V.UnOp(V.NOT, V.Constant(V.Int(ty, v)))))))
	| V.BinOp(V.BITOR, e1, e2) ->
	    let (w1, w2) = (narrow_bitwidth e1), (narrow_bitwidth e2) in
(* 	      Printf.printf "In %s (OR) %s, widths are %d and %d\n" *)
(* 		(V.exp_to_string e1) (V.exp_to_string e2) w1 w2; *)
	      if min w1 w2 <= 8 then
		(* x | y = x - (x & m) + ((x & m) | y)
		   where m is a bitmask >= y. *)
		let (e_x, e_y, w) = 
		  if w1 < w2 then
		    (e2, e1, w1)
		  else
		    (e1, e2, w2)
		in
		  assert(w >= 0); (* x & 0 should have been optimized away *)
		  let mask = Int64.pred (Int64.shift_left 1L w) in
		  let ty_y = Vine_typecheck.infer_type None e_y in
		  let masked = V.BinOp(V.BITAND, e_y,
				       V.Constant(V.Int(ty_y, mask))) in
		    (loop e_x) @ 
		      [V.UnOp(V.NEG, masked);
		       V.BinOp(V.BITOR, masked, e_y)]
	      else
		[e]
	| V.Lval(V.Temp(var)) ->
	    FormMan.if_expr_temp form_man var
	      (fun e' -> loop e') [e]
	| e -> [e]
    in
      loop e

  type term_kind = | ConstantBase of int64
		   | ConstantOffset of int64
		   | ExprOffset of V.exp
		   | Symbol of V.exp

  let classify_term e =
    match e with
      | V.Constant(V.Int(V.REG_32, off))
	  when (Int64.abs (fix_s32 off)) < 0x4000L
	    -> ConstantOffset(off)
      | V.Constant(V.Int(V.REG_32, off)) when (fix_s32 off) > 0x8000000L
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0xc0000000L && off < 0xe1000000L (* Linux kernel *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x80800000L && off < 0x88000000L (* ReactOS kernel *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x82800000L && off < 0x94000000L (* Windows 7 kernel *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0xf88f0000L && off < 0xf88fffffL
	    (* ReactOS kernel stack *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x9b200000L && off < 0x9b300000L
	    (* Windows 7 kernel stack *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0xff400000L && off < 0xffc00000L
	    (* Windows 7 kernel something *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x7ff00000L && off < 0x80000000L
	    (* Windows 7 shared user/kernel something *)
	  -> ConstantBase(off)
      | V.Constant(V.Int(V.REG_32, off))
	  when off >= 0x80000000L && off < 0xffffffffL
	    (* XXX let Windows 7 wander over the whole top half *)
	  -> ConstantBase(off)
      | V.UnOp(V.NEG, _) -> ExprOffset(e)
      | V.BinOp(V.LSHIFT, _, V.Constant(V.Int(V.REG_8, (1L|2L|3L|4L|5L))))
	  -> ExprOffset(e)
      | V.BinOp(V.TIMES, _, _)
	  -> ExprOffset(e)
      | e when (narrow_bitwidth e) < 23
	  -> ExprOffset(e)
      | V.BinOp(V.ARSHIFT, _, _)
	  -> ExprOffset(e)
      | V.BinOp(V.RSHIFT, _, _)
	  -> ExprOffset(e)
      | V.BinOp(V.LSHIFT, _, _)
	  -> ExprOffset(e)
      | V.BinOp(V.BITOR, _, _) (* XXX happens in Windows 7, don't know why *)
	  -> ExprOffset(e)
      | V.Cast(V.CAST_UNSIGNED, V.REG_32,
	       V.Lval(V.Mem(_, _, V.REG_16)))
	  -> ExprOffset(e)
      | V.Cast(V.CAST_UNSIGNED, V.REG_32,
	       V.Lval(V.Mem(_, _, V.REG_8)))
	  -> ExprOffset(e)
      | V.Lval(_) -> Symbol(e)
      | _ -> failwith ("Strange term "^(V.exp_to_string e)^" in address")

  let classify_terms e form_man =
    let l = List.map classify_term (split_terms e form_man) in
    let (cbases, coffs, eoffs, syms) = (ref [], ref [], ref [], ref []) in
      List.iter
	(function
	   | ConstantBase(o) ->  cbases := o :: !cbases
	   | ConstantOffset(o) -> coffs := o :: !coffs
	   | ExprOffset(e) ->     eoffs := e :: !eoffs
	   | Symbol(v) ->          syms := v :: !syms)
	l;
      (!cbases, !coffs, !eoffs, !syms)

  let select_one l rand_func =
    let split_list l =
      let a = Array.of_list l in
      let len = Array.length a in 
      let k = len / 2 in
	((Array.to_list (Array.sub a 0 k)),
	 (Array.to_list (Array.sub a k (len - k))))
    in
    let rec loop l =
      match l with
	| [] -> failwith "Empty list in select_one"
	| [a] -> (a, [])
	| [a; b] -> if rand_func () then (a, [b]) else (b, [a])
	| l -> let (h1, h2) = split_list l in
	    if rand_func () then
	      let (e, h1r) = loop h1 in
		(e, h1r @ h2)
	    else
	      let (e, h2r) = loop h2 in
		(e, h1 @ h2r)
    in
      loop l

  let sum_list l = 
    match l with 
      | [] -> V.Constant(V.Int(V.REG_32, 0L))
      | [a] -> a
      | e :: r -> List.fold_left (fun a b -> V.BinOp(V.PLUS, a, b))
	  e r

  class sym_region_frag_machine = object(self)
    inherit SPFM.sym_path_frag_machine as spfm

    val mutable regions = []
    val region_vals = Hashtbl.create 101

    method private region r =
      match r with
	| None -> (mem :> (GM.granular_memory))
	| Some r_num -> List.nth regions r_num

    method private fresh_region =
      let new_idx = List.length regions in
      let region = (new GM.granular_hash_memory)  and
	  name = "region_" ^ (string_of_int new_idx) in
	regions <- regions @ [region];
	spfm#on_missing_symbol_m region name;
	new_idx

    method private region_for e =
      try
	Hashtbl.find region_vals e
      with Not_found ->
	let new_region = self#fresh_region in
	  Hashtbl.replace region_vals e new_region;
	  if !opt_trace_regions then
	    Printf.printf "Address %s is region %d\n"
	      (V.exp_to_string e) new_region;
	  new_region

    method private choose_conc_offset_uniform e =
      let c32 x = V.Constant(V.Int(V.REG_32, x)) in
      let bits = ref 0L in
	self#restore_path_cond
	  (fun () ->
	     for b = 31 downto 0 do
	       let bit = self#extend_pc_random
		 (V.Cast(V.CAST_LOW, V.REG_1,
			 (V.BinOp(V.ARSHIFT, e,
				  (c32 (Int64.of_int b)))))) false
	       in
		 bits := (Int64.logor (Int64.shift_left !bits 1)
			    (if bit then 1L else 0L));
	     done);
	!bits

    method private choose_conc_offset_biased e =
      let c32 x = V.Constant(V.Int(V.REG_32, x)) in
      let rec try_list l =
	match l with
	  | [] -> self#choose_conc_offset_uniform e
	  | v :: r ->
	      if self#extend_pc_random (V.BinOp(V.EQ, e, (c32 v))) false then
		v
	      else
		try_list r
      in
      let bits = ref 0L in
	self#restore_path_cond
	  (fun () ->
	     bits := try_list
	       [0L; 1L; 2L; 4L; 8L; 16L; 32L; 64L;
		0xffffffffL; 0xfffffffeL; 0xfffffffcL; 0xfffffff8L]);
	!bits

    val mutable concrete_cache = Hashtbl.create 101

    method private choose_conc_offset_cached e =
      let c32 x = V.Constant(V.Int(V.REG_32, x)) in
      let (bits, verb) = 
	if Hashtbl.mem concrete_cache e then
	  (Hashtbl.find concrete_cache e, "Reused")
	else
	  let bits = self#choose_conc_offset_biased e in
	    Hashtbl.replace concrete_cache e bits;
	    (bits, "Picked") in
	if !opt_trace_sym_addrs then
	  Printf.printf "%s concrete offset value 0x%Lx for %s\n"
	    verb bits (V.exp_to_string e);
	self#add_to_path_cond (V.BinOp(V.EQ, e, (c32 bits)));
	bits

    method eval_addr_exp_region exp =
      let v = self#eval_int_exp_simplify exp in
	try
	  (None, D.to_concrete_32 v)
	with NotConcrete _ ->
	  let e = D.to_symbolic_32 v in
	    if !opt_trace_sym_addrs then
	      Printf.printf "Symbolic address %s\n" (V.exp_to_string e);
	    let (cbases, coffs, eoffs, syms) = classify_terms e form_man in
	    let cbase = List.fold_left Int64.add 0L cbases in
	    let (base, off_syms) = match (cbase, syms) with
	      | (0L, []) -> raise NullDereference
	      | (0L, [v]) -> (Some(self#region_for v), [])
	      | (0L, vl) ->
		  let (bvar, rest_vars) =
		    select_one vl (fun () -> self#random_case_split true)
		  in
		    if !opt_trace_sym_addrs then
		      Printf.printf "Choosing %s as the base address\n"
			(V.exp_to_string bvar);
		    (Some(self#region_for bvar), rest_vars)
	      | (off, vl) ->
		  (None, vl)
	    in
	    let coff = List.fold_left Int64.add 0L coffs in
	    let offset = Int64.add (Int64.add cbase coff)
	      (match (eoffs, off_syms) with
		 | ([], []) -> 0L
		 | (el, vel) -> 
		     (self#choose_conc_offset_cached (sum_list (el @ vel))))
	    in
	      (base, (fix_u32 offset))
		  
    method eval_addr_exp exp =
      let (r, addr) = self#eval_addr_exp_region exp in
	match r with
	  | None -> addr
	  | Some r_num -> raise SymbolicJump

    method store_byte_region  r addr b = (self#region r)#store_byte  addr b
    method store_short_region r addr s = (self#region r)#store_short addr s
    method store_word_region  r addr w = (self#region r)#store_word  addr w
    method store_long_region  r addr l = (self#region r)#store_long  addr l

    method load_byte_region  r addr = (self#region r)#load_byte  addr
    method load_short_region r addr = (self#region r)#load_short addr
    method load_word_region  r addr = (self#region r)#load_word  addr
    method load_long_region  r addr = (self#region r)#load_long  addr

    method handle_load addr_e ty =
      let (r, addr) = self#eval_addr_exp_region addr_e in
      let v =
	(match ty with
	   | V.REG_8 -> self#load_byte_region r addr
	   | V.REG_16 -> self#load_short_region r addr
	   | V.REG_32 -> self#load_word_region r addr
	   | V.REG_64 -> self#load_long_region r addr
	   | _ -> failwith "Unsupported memory type") in
	if !opt_trace_loads then
	  (Printf.printf "Load from %s "
	     (match r with | None -> "conc. mem"
		| Some r_num -> "region " ^ (string_of_int r_num));
	   Printf.printf "%08Lx = %s\n" addr (D.to_string_32 v));
	if r = None && (Int64.abs (fix_s32 addr)) < 4096L then
	  raise NullDereference;
	(v, ty)

    method handle_store addr_e ty rhs_e =
      let (r, addr) = self#eval_addr_exp_region addr_e and
	  value = self#eval_int_exp_simplify rhs_e in
	if r = None && (Int64.abs (fix_s32 addr)) < 4096L then
	  raise NullDereference;
	(match ty with
	   | V.REG_8 -> self#store_byte_region r addr value
	   | V.REG_16 -> self#store_short_region r addr value
	   | V.REG_32 -> self#store_word_region r addr value
	   | V.REG_64 -> self#store_long_region r addr value
	   | _ -> failwith "Unsupported type in memory move");
	if !opt_trace_stores then
	  if not (ty = V.REG_8 && r = None) then
	    (Printf.printf "Store to %s "
	       (match r with | None -> "conc. mem"
		  | Some r_num -> "region " ^ (string_of_int r_num));
	     Printf.printf "%08Lx = %s\n" addr (D.to_string_32 value))

    method concretize_misc =
      let var = Hashtbl.find reg_to_var R_DFLAG in
      let d = self#get_int_var var in
	try ignore(D.to_concrete_32 d)
	with NotConcrete _ ->
	  let e = D.to_symbolic_32 d in
	    if e <> V.Unknown("uninit") then
	      self#set_int_var var
		(D.from_concrete_32 
		   (self#choose_conc_offset_cached e))

    method reset () =
      spfm#reset ();
      regions <- [];
      Hashtbl.clear region_vals;
      Hashtbl.clear concrete_cache
  end
end

exception Simplify_failure of string

let cfold_exprs (dl, sl) =
  let sl' = List.map
    (fun st -> match st with
       | V.CJmp(e, l1, l2) -> V.CJmp(constant_fold_rec e, l1, l2)
       | V.Move(lval, e) ->
	   V.Move(constant_fold_rec_lv lval, constant_fold_rec e)
       | V. ExpStmt(e) -> 
	   V.ExpStmt(constant_fold_rec e)
       | _ -> st)
    sl
  in
    (dl, sl')

let rec cfold_with_type e =
  match e with
    | V.UnOp(op, e1) ->
	let (e1', ty1) = cfold_with_type e1 in
	  (V.UnOp(op, e1'), ty1)
    | V.BinOp(op, e1, e2) ->
	let (e1', ty1) = cfold_with_type e1 and
	    (e2', ty2) = cfold_with_type e2 in
	let ty =
	  (match op with
	     | V.PLUS | V.MINUS | V.TIMES
	     | V.DIVIDE | V.SDIVIDE | V.MOD | V.SMOD
	     | V.BITAND | V.BITOR | V.XOR
		 -> assert(ty1 = ty2); ty1
	     | V.LSHIFT | V.RSHIFT | V.ARSHIFT
		 -> ty1
	     | V.EQ | V.NEQ | V.LT | V.LE | V.SLT | V.SLE
		 -> assert(ty1 = ty2); V.REG_1)
	in
	  (V.BinOp(op, e1', e2'), ty)
    | V.Constant(V.Int(ty, _)) -> (e, ty)
    | V.Constant(V.Str(_)) -> (e, V.TString)
    | V.Lval(V.Temp(_,_,ty)) -> (e, ty)
    | V.Lval(V.Mem(mem,e1,ty)) ->
	let (e1', _) = cfold_with_type e1 in
	  (V.Lval(V.Mem(mem,e1',ty)), ty)
    | V.Name(_) -> (e, V.addr_t)
    | V.Cast(kind, ty, e1) ->
	let (e1', ty1) = cfold_with_type e1 in
	let e' = if ty = ty1 then e1' else V.Cast(kind, ty, e1') in
	  (e', ty)
    | V.Unknown("rdtsc") -> (e, V.REG_64)
    | V.Unknown("Unknown: Dirty") ->
	raise IllegalInstruction
    | V.Unknown(s) ->
	raise (Simplify_failure ("unhandled unknown "^s^" in cfold_with_type"))
    | V.Let(_) -> failwith "unhandled let in cfold_with_type"

let cfold_exprs_w_type (dl, sl) =
  let fold e =
    match cfold_with_type e with (e', _) -> e'
  in
  let sl' = List.map
    (fun st -> match st with
       | V.CJmp(e, l1, l2) -> V.CJmp(fold e, l1, l2)
       | V.Move(V.Temp(v), e) -> V.Move(V.Temp(v), fold e)
       | V.Move(V.Mem(mem, e1, ty), e2) ->
	   V.Move(V.Mem(mem, fold e1, ty), fold e2)
       | V. ExpStmt(e) -> V.ExpStmt(fold e)
       | _ -> st)
    sl
  in
    (dl, sl')

let rm_unused_stmts sl =
  List.filter
    (fun st -> match st with
       | V.Special("call") -> false
       | V.Special("ret") -> false
       (*| V.Move(V.Temp(_,"R_CC_OP",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP1",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_DEP2",_),_) -> false
       | V.Move(V.Temp(_,"R_CC_NDEP",_),_) -> false*)
       | _ -> true)
    sl

let uncond_jmps sl =
  List.map
    (fun st -> match st with
       | V.CJmp(V.Constant(V.Int(V.REG_1, 1L)), l1, l2) -> V.Jmp(l1)
       | V.CJmp(V.Constant(V.Int(V.REG_1, 0L)), l1, l2) -> V.Jmp(l2)
       | _ -> st
    ) sl

let rec rm_sequential_jmps sl =
  match sl with
    | V.Jmp(V.Name(lab1)) :: V.Label(lab2) :: rest when lab1 = lab2
	-> V.Label(lab2) :: rm_sequential_jmps rest
    | st :: rest -> st :: rm_sequential_jmps rest
    | [] -> []

let rm_unused_labels sl =
  let exp_labels e =
    match e with
      | V.Name(l) -> [l]
      | _ -> []
  in
  let used_labels = List.concat
    (List.map
       (fun st -> match st with
	  | V.Jmp(e) -> exp_labels e
	  | V.CJmp(cond, e1, e2) -> (exp_labels e1) @ (exp_labels e2)
	  | _ -> [])
       sl)
  in
    (* List.iter print_string used_labels; *)
    List.filter
      (fun st -> match st with
	 | V.Label(l) when not (List.mem l used_labels) -> false
	 | _ -> true)
      sl

let rec count_uses_e var e =
  match e with
    | V.BinOp(_, e1, e2) -> (count_uses_e var e1) + (count_uses_e var e2)
    | V.UnOp(_, e) -> count_uses_e var e
    | V.Cast(_, _, e) -> count_uses_e var e
    | V.Lval(lv) -> count_uses_lv var lv
    | _ -> 0
and count_uses_lv var lv =
  match lv with
    | V.Temp(v) -> (if v = var then 1 else 0)
    | V.Mem(v, e, _) -> (if v = var then 1 else 0) + (count_uses_e var e)

let count_temp_uses (dl, sl) =
  let count_uses_sl var =
    List.fold_left (+) 0
      (List.map 
	 (fun st -> match st with
	    | V.CJmp(e1, l1, l2) -> 
		(count_uses_e var e1) + (count_uses_e var l1)
		+ (count_uses_e var l2)
	    | V.Jmp(lab) -> count_uses_e var lab
	    | V.Move(V.Temp(var1), e) when var1 = var
		-> (count_uses_e var e)
	    | V.Move(lval, e) -> (count_uses_lv var lval)
		+ (count_uses_e var e)
	    | V.ExpStmt(e) -> count_uses_e var e
	    | V.Assert(e) -> count_uses_e var e
	    | _ -> 0)
	 sl)
  in
    List.map count_uses_sl dl

let rm_unused_vars (dl, sl) =
  let rm_defs var sl =
    List.filter
      (fun st -> match st with
	 | V.Move(V.Temp(v), _) when v = var -> false
	 | _ -> true)
      sl
  in
  let rec partition2 pred l1 l2 =
    match (l1, l2) with
      | ((item :: r1), (obj :: r2)) ->
	  let (rt, rf) = partition2 pred r1 r2 in
	  if pred obj then
	    (item :: rt, rf)
	  else
	    (rt, item :: rf)
      | ([], []) -> ([], [])
      | _ -> failwith "Mismatched lengths in partition2"
  in
  let uses = count_temp_uses (dl, sl) in
    (* print_string "{";
       List.iter print_int uses;
       print_string "}\n"; *)
  let (dl', to_rm) = partition2 (fun n -> n != 0) dl uses in
  let sl' = List.fold_right rm_defs to_rm sl in
    (dl', sl')

let contains hash ent =
  try
    V.VarHash.find hash ent;
    true;
  with
      Not_found -> false

let copy_const_prop (dl, sl) =
  let map = V.VarHash.create 10 in
  let use_counts = List.fold_left2 
    (fun hash key value -> V.VarHash.add hash key value; hash)
    (V.VarHash.create 10) dl (count_temp_uses (dl, sl)) in
  let rec replace_uses_e expr =
    match expr with
      | V.BinOp(op, e1, e2) ->
	  V.BinOp(op, replace_uses_e e1, replace_uses_e e2)
      | V.UnOp(op, e) ->
	  V.UnOp(op, replace_uses_e e)
      | V.Cast(op, ty, e) ->
	  V.Cast(op, ty, replace_uses_e e)
      | V.Lval(V.Temp(v)) when contains map v ->
	  V.VarHash.find map v
      | V.Lval(V.Mem(v, idx, ty)) ->
	  V.Lval(V.Mem(v, replace_uses_e idx, ty))
      | _ -> expr
  in
  let replace_uses st =
    match st with
      | V.CJmp(e1, l1, l2) -> 
	  V.CJmp(replace_uses_e e1, replace_uses_e l1, replace_uses_e l2)
      | V.Jmp(lab) ->
	  V.Jmp(replace_uses_e lab)
      | V.Move(V.Temp(var), e) -> 
	  V.Move(V.Temp(var), replace_uses_e e)
      | V.Move(V.Mem(var, idx, ty), e) -> 
	  V.Move(V.Mem(var, replace_uses_e idx, ty), replace_uses_e e)
      | V.ExpStmt(e) ->
	  V.ExpStmt(replace_uses_e e)
      | V.Assert(e) ->
	  V.Assert(replace_uses_e e)
      | _ -> st
  in
  let find_or_zero vh key = try V.VarHash.find vh key with Not_found -> 0
  in
  let invalidate_uses vh bad_var =
    V.VarHash.iter
      (fun lhs rhs ->
	 if (count_uses_e bad_var rhs) != 0 then	   
	   (V.VarHash.remove vh lhs))
      vh
  in
  let rec loop sl =
    match sl with
      | [] -> []
      | st :: rest ->
	  let st' = replace_uses st in
	    (match st' with
	       | V.Move(V.Temp(lhs_v), rhs)
		   -> invalidate_uses map lhs_v;
		     V.VarHash.remove map lhs_v;
	       | _ -> ());
	    (match st' with
	       | V.CJmp(_, _, _)
	       | V.Jmp(_)
	       | V.Label(_)
		 -> V.VarHash.clear map
	       | V.Move(V.Temp(lhs_v), V.Lval(V.Temp(rhs_v)))
		 -> V.VarHash.replace map lhs_v (V.Lval(V.Temp(rhs_v)));
		   (if List.mem rhs_v dl then
		      V.VarHash.replace map rhs_v (V.Lval(V.Temp(lhs_v))))
	       | V.Move(V.Temp(lhs_v), rhs)
		   when (find_or_zero use_counts lhs_v) = 1
		     -> V.VarHash.replace map lhs_v rhs
	       | V.Move(V.Temp(lhs_v), (V.Constant(_) as const))
		   -> V.VarHash.replace map lhs_v const
	       | _ -> ());
	    st' :: (loop rest)
  in
    (dl, loop sl)

let peephole_patterns (dl, sl) =
  let rec loop sl =
    match sl with
      (* No-op assignment *)
      | V.Move(V.Temp(r1_1), V.Lval(V.Temp(r1_2))) :: rest
	  when r1_1 = r1_2
	    -> loop rest
      (* Usually lets us avoid a temp in a push: *)
      | V.Move(V.Temp(t1_1), (V.BinOp(op, V.Lval(_), V.Constant(_)) as e1)) ::
	  V.Move(V.Temp(r1), V.Lval(V.Temp(t1_2))) ::
	  V.Move(V.Mem(mem, V.Lval(V.Temp(t1_3)), ty), e2) :: rest
	  when List.mem t1_1 dl && t1_1 = t1_2 && t1_2 = t1_3 &&
	    not (List.mem r1 dl)
	    ->
	  V.Move(V.Temp(t1_1), e1) ::
	    V.Move(V.Temp(r1), e1) ::
	    V.Move(V.Mem(mem, V.Lval(V.Temp(r1)), ty), e2) ::
	    loop rest
      (* Avoid a temp in a pop: *)
      | V.Move(V.Temp(t1_1),
	       (V.Lval(V.Mem(_, V.Lval(V.Temp(r1_1)), _)) as load)) ::
	  (V.Move(V.Temp(r1_2), V.BinOp(op, V.Lval(V.Temp(r1_3)),
					V.Constant(c)))
	     as update)::
	       V.Move(V.Temp(r2), V.Lval(V.Temp(t1_2))) :: rest
	       when List.mem t1_1 dl && t1_1 = t1_2 &&
		 not (List.mem r1_1 dl) && r1_1 = r1_2 && r1_2 = r1_3 &&
		 not (List.mem r2 dl)
	       ->
	  V.Move(V.Temp(t1_1), load) ::
	    V.Move(V.Temp(r2), load) ::
	    update ::
	    loop rest
      | st :: rest -> st :: loop rest
      | [] -> []
  in
    (dl, loop sl)

let simplify_frag (orig_dl, orig_sl) =
  (* V.pp_program print_string (orig_dl, orig_sl); *)
  let (dl, sl) = (orig_dl, orig_sl) in
  let sl = rm_unused_stmts sl in
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
  let (dl, sl) = cfold_exprs (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in 
  let (dl, sl) = cfold_exprs_w_type (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in
  let (dl, sl) = peephole_patterns (dl, sl) in
  let (dl, sl) = rm_unused_vars (dl, sl) in 
  let sl = uncond_jmps sl in
  let sl = rm_sequential_jmps sl in
  let sl = rm_unused_labels sl in
  let (dl, sl) = copy_const_prop (dl, sl) in
  let (dl, sl) = cfold_exprs (dl, sl) in 
  let (dl, sl) = rm_unused_vars (dl, sl) in
 let (dl', sl') = (dl, sl) in
    (* Vine_typecheck.typecheck (dl' @ Asmir.x86_regs, sl'); *)
    (* V.pp_program print_string (orig_dl, orig_sl);
       V.pp_program print_string (dl', sl');
       V.pp_program print_string (copy_prop (dl', sl')); *)
    (dl', sl')

exception NoSyscalls

class linux_special_nonhandler fm =
object(self)
  method handle_special str =
    match str with
      | "int 0x80" -> raise NoSyscalls
      | _ -> false
end

exception SimulatedExit of int64

class linux_special_handler fm =
  let put_reg reg v = fm#set_word_var reg v in
  let load_word addr = fm#load_word_conc addr in
  let lea base i step off =
    Int64.add base (Int64.add (Int64.mul (Int64.of_int i) (Int64.of_int step))
		      (Int64.of_int off)) in
  let store_word base idx v =
    let addr = Int64.add base (Int64.of_int idx) in
      fm#store_word_conc addr v
  in
  let zero_region base len =
    for i = 0 to len -1 do fm#store_byte_idx base i 0 done
  in
object(self)
  val unix_fds = 
    let a = Array.make 1024 None in
      Array.set a 0 (Some Unix.stdin);
      Array.set a 1 (Some Unix.stdout);
      Array.set a 2 (Some Unix.stderr);
      a

  method fresh_fd () =
    let rec loop i = match unix_fds.(i) with
      | None -> i
      | Some _ -> loop (i + 1)
    in loop 0

  method get_fd vt_fd =
    match unix_fds.(vt_fd) with
      | Some fd -> fd
      | None -> raise
	  (Unix.Unix_error(Unix.EBADF, "Bad (virtual) file handle", ""))

  method errno err =
    match err with
      | Unix.E2BIG -> 7
      | Unix.EACCES -> 13
      | Unix.EAGAIN -> 11
      | Unix.EBADF -> 9
      | Unix.EBUSY -> 16
      | Unix.ECHILD -> 10
      | Unix.EDEADLK -> 35
      | Unix.EDOM -> 33
      | Unix.EEXIST -> 17
      | Unix.EFAULT -> 14
      | Unix.EFBIG -> 27
      | Unix.EINTR -> 4
      | Unix.EINVAL -> 22
      | Unix.EIO -> 5
      | Unix.EISDIR -> 21
      | Unix.EMFILE -> 24
      | Unix.EMLINK -> 31
      | Unix.ENAMETOOLONG -> 36
      | Unix.ENFILE -> 23
      | Unix.ENODEV -> 19
      | Unix.ENOENT -> 2
      | Unix.ENOEXEC -> 8
      | Unix.ENOLCK -> 37
      | Unix.ENOMEM -> 12
      | Unix.ENOSPC -> 28
      | Unix.ENOSYS -> 38
      | Unix.ENOTDIR -> 20
      | Unix.ENOTEMPTY -> 39
      | Unix.ENOTTY -> 25
      | Unix.ENXIO -> 6
      | Unix.EPERM -> 1
      | Unix.EPIPE -> 32
      | Unix.ERANGE -> 34
      | Unix.EROFS -> 30
      | Unix.ESPIPE -> 29
      | Unix.ESRCH -> 3
      | Unix.EXDEV -> 18
      | Unix.EWOULDBLOCK -> 11
      | Unix.EINPROGRESS -> 115
      | Unix.EALREADY -> 114
      | Unix.ENOTSOCK -> 88
      | Unix.EDESTADDRREQ -> 89
      | Unix.EMSGSIZE -> 90
      | Unix.EPROTOTYPE -> 91
      | Unix.ENOPROTOOPT -> 92
      | Unix.EPROTONOSUPPORT -> 93
      | Unix.ESOCKTNOSUPPORT -> 94
      | Unix.EOPNOTSUPP -> 95
      | Unix.EPFNOSUPPORT -> 96
      | Unix.EAFNOSUPPORT -> 97
      | Unix.EADDRINUSE -> 98
      | Unix.EADDRNOTAVAIL -> 99
      | Unix.ENETDOWN -> 100
      | Unix.ENETUNREACH -> 101
      | Unix.ENETRESET -> 102
      | Unix.ECONNABORTED -> 103
      | Unix.ECONNRESET -> 104
      | Unix.ENOBUFS -> 105
      | Unix.EISCONN -> 106
      | Unix.ENOTCONN -> 107
      | Unix.ESHUTDOWN -> 108
      | Unix.ETOOMANYREFS -> 109
      | Unix.ETIMEDOUT -> 110
      | Unix.ECONNREFUSED -> 111
      | Unix.EHOSTDOWN -> 112
      | Unix.EHOSTUNREACH -> 113
      | Unix.ELOOP -> 40
      | Unix.EOVERFLOW -> 75
      | Unix.EUNKNOWNERR(i) -> i

  method put_errno err =
    put_reg R_EAX (Int64.of_int ~-(self#errno err))

  val mutable next_fresh_addr = 0x50000000L

  method fresh_addr size = 
    let ret = next_fresh_addr in
      next_fresh_addr <- Int64.add next_fresh_addr size;
      next_fresh_addr <- Int64.logand 0xffff_ffff_ffff_f000L
	(Int64.add next_fresh_addr 0x0fffL); (* page align *)
      ret

  val the_break = ref 0x08100000L

  method do_write fd bytes count =
    (try
       (match fd with
	  | 1 -> Array.iter print_char bytes;
	      put_reg R_EAX (Int64.of_int count)
	  | _ ->
	      let str = Array.fold_left (^) ""
		(Array.map (String.make 1) bytes)
	      in
		match Unix.write (self#get_fd fd) str 0 (String.length str)
		with
		  | i when i = count -> put_reg R_EAX (Int64.of_int count)
		  | _ -> raise (Unix.Unix_error(Unix.EINTR, "", "")))
     with
       | Unix.Unix_error(err, _, _) -> self#put_errno err);
    ()

  method do_unix_read fd addr count =
    let rec loop left a =
      if (left <= 0) then 0 else
	let chunk = if (left < 16384) then left else 16384 in
	let str = String.create chunk in
	let num_read = Unix.read fd str 0 chunk in
	  fm#store_str a 0L (String.sub str 0 num_read);
	  num_read +
	    (loop (left - chunk) (Int64.add a (Int64.of_int chunk)))
    in
      loop count addr

  method setup_tcb_seg new_ent base limit =
    let new_gs = Int64.logor (Int64.shift_left new_ent 3) 3L in
    let new_gdt = 0x60000000L in
    let descr = Int64.add new_gdt (Int64.shift_left new_ent 3) in
      put_reg R_GDT new_gdt;
      put_reg R_GS new_gs;
      store_word descr 0 (Int64.logand limit 0xffL);
      store_word descr 1 (Int64.logand 
			    (Int64.shift_right limit 8) 0xffL);
      store_word descr 2 (Int64.logand base 0xffL);
      store_word descr 3 (Int64.logand
			    (Int64.shift_right base 8) 0xffL);
      store_word descr 4 (Int64.logand
			    (Int64.shift_right base 16) 0xffL);
      store_word descr 5 0xf3L; (* pres., ring 3, app, r/w a *)
      store_word descr 6 (Int64.logor 0xc0L
			    (Int64.shift_right limit 16));
      (* page-gran limit, 32-bit, high nibble of limit *)
      store_word descr 7 (Int64.logand
			    (Int64.shift_right base 24) 0xffL)
	
  method oc_kind_to_mode kind = match kind with
    | Unix.S_REG  -> 0o0100000
    | Unix.S_DIR  -> 0o0040000
    | Unix.S_CHR  -> 0o0020000
    | Unix.S_BLK  -> 0o0060000
    | Unix.S_LNK  -> 0o0120000
    | Unix.S_FIFO -> 0o0010000
    | Unix.S_SOCK -> 0o0140000

  method flags_to_oc_flags flags =
    (if (flags land 0x3) = 0        then [Unix.O_RDONLY]   else []) @
      (if (flags land 0x3)= 1       then [Unix.O_WRONLY]   else []) @
      (if (flags land 0x3) = 2      then [Unix.O_RDWR]     else []) @
      (if (flags land 0o4000) != 0  then [Unix.O_NONBLOCK] else []) @
      (if (flags land 0o2000) != 0  then [Unix.O_APPEND]   else []) @
      (if (flags land 0o100) != 0   then [Unix.O_CREAT]    else []) @
      (if (flags land 0o1000) != 0  then [Unix.O_TRUNC]    else []) @
      (if (flags land 0o200) != 0   then [Unix.O_EXCL]     else []) @
      (if (flags land 0o10000) != 0 then [Unix.O_SYNC]     else [])

  method write_oc_statbuf addr oc_buf =
    let dev = Int64.of_int oc_buf.Unix.st_dev and
	ino = Int64.of_int oc_buf.Unix.st_ino and
	mode = Int64.of_int (oc_buf.Unix.st_perm lor 
			       (self#oc_kind_to_mode oc_buf.Unix.st_kind)) and
	nlink = Int64.of_int oc_buf.Unix.st_nlink and
	uid = Int64.of_int oc_buf.Unix.st_uid and
	gid = Int64.of_int oc_buf.Unix.st_gid and
	rdev = Int64.of_int oc_buf.Unix.st_rdev and
	size = Int64.of_int oc_buf.Unix.st_size and
	atime = Int64.of_float oc_buf.Unix.st_atime and
	mtime = Int64.of_float oc_buf.Unix.st_mtime and
	ctime = Int64.of_float oc_buf.Unix.st_ctime and
	blksize = 4096L and
	blocks = Int64.of_int (oc_buf.Unix.st_size/4096)
    in
      store_word addr 0 dev;
      store_word addr 4 0L;       (* high bits of dev *)
      store_word addr 12 ino;     (* 32-bit inode *)
      store_word addr 16 mode;
      store_word addr 20 nlink;
      store_word addr 24 uid;
      store_word addr 28 gid;
      store_word addr 32 rdev;
      store_word addr 36 0L;      (* high bits of rdev *)
      store_word addr 44 size;
      store_word addr 48 0L;      (* high bits of size *)
      store_word addr 52 blksize;
      store_word addr 56 blocks;
      store_word addr 64 atime;
      store_word addr 68 0L;      (* atime nanosecs *)
      store_word addr 72 mtime;
      store_word addr 76 0L;      (* mtime naonsecs *)
      store_word addr 80 ctime;
      store_word addr 84 0L;      (* ctime nanosecs *)
      store_word addr 89 ino;     (* low bits of 64-bit inode *)
      store_word addr 92 0L;      (* high bits of 64-bit inode *)

  method write_fake_statfs64buf addr =
    (* OCaml's Unix module doesn't provide an interface for this
       information, so we just make it up. *)
    let f_type   = 0x52654973L (* REISERFS_SUPER_MAGIC *) and
	f_bsize  = 4096L and
	f_blocks = 244182546L and
	f_bfree  = 173460244L and
	f_bavail = 173460244L and
	f_files  = 0L and
	f_ffree  = 0L and
	f_fsid_0 = 0L and
	f_fsid_1 = 0L and
	f_namelen = 255L and
	f_frsize = 4096L
    in
      store_word addr  0 f_type;
      store_word addr  4 f_bsize;
      store_word addr  8 f_blocks;
      store_word addr 12 0L;     (* high word of f_blocks *)
      store_word addr 16 f_bfree;
      store_word addr 20 0L;     (* high word of f_bfree *)
      store_word addr 24 f_bavail;
      store_word addr 28 0L;     (* high word of f_bavail *)
      store_word addr 32 f_files;
      store_word addr 36 0L;     (* high word of f_files *)
      store_word addr 40 f_ffree;
      store_word addr 44 0L;     (* high word of f_ffree *)
      store_word addr 48 f_fsid_0;
      store_word addr 52 f_fsid_1;
      store_word addr 56 f_namelen;
      store_word addr 60 f_frsize
      (* offsets 64, 68, 72, 76, 80: f_spare[5] reserved *)

  method sys_access path mode =
    let oc_mode =
      (if   (mode land 0x7)= 0 then [Unix.F_OK] else []) @
	(if (mode land 0x1)!=0 then [Unix.X_OK] else []) @
	(if (mode land 0x2)!=0 then [Unix.W_OK] else []) @
	(if (mode land 0x4)!=0 then [Unix.R_OK] else []) 
    in
      try
	Unix.access path oc_mode;
	put_reg R_EAX 0L
      with
	| Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_brk addr =
    if addr < !the_break then
      ()
    else
      the_break := addr;
    put_reg R_EAX !the_break;

  method sys_close fd =
    try
      Unix.close (self#get_fd fd);
      Array.set unix_fds fd None;
      put_reg R_EAX 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_exit_group status =
    raise (SimulatedExit(status))

  method sys_futex uaddr op value timebuf uaddr2 val3 =
    let ret = 
      match (op, value) with
	| (129 (* FUTEX_WAKE_PRIVATE *), _) ->
	    0L (* never anyone to wake *)
	| _ -> failwith "Unhandled futex operation"
    in
      put_reg R_EAX ret

  method sys_ugetrlimit rsrc buf =
    store_word buf 0 0xffffffffL; (* infinity *)
    store_word buf 4 0xffffffffL; (* infinity *)
    put_reg R_EAX 0L (* success *)

  method sys_getgid32 () = 
    put_reg R_EAX (Int64.of_int (Unix.getgid ()))

  method sys_getegid32 () = 
    put_reg R_EAX (Int64.of_int (Unix.getegid ()))

  method sys_getuid32 () = 
    put_reg R_EAX (Int64.of_int (Unix.getuid ()))

  method sys_geteuid32 () = 
    put_reg R_EAX (Int64.of_int (Unix.geteuid ()))

  method sys_ioctl fd req argp =
    match req with
      | 0x5401L -> 
	  (* let attrs = Unix.tcgetattr (get_fd fd) in *)
	  failwith "Unhandled TCGETS ioctl"
      | _ -> failwith "Unknown ioctl"

  method sys_mmap2 addr length prot flags fd pgoffset =
    let do_read addr = 
      let len = Int64.to_int length in
      let old_loc = Unix.lseek (self#get_fd fd) 0 Unix.SEEK_CUR in
      let _ = Unix.lseek (self#get_fd fd) (4096*pgoffset) Unix.SEEK_SET in
      let _ = self#do_unix_read (self#get_fd fd) addr len in
      let _ = Unix.lseek (self#get_fd fd) old_loc Unix.SEEK_SET in
	(* assert(nr = len); *)
	addr
    in
    let ret =
      match (addr, length, prot, flags, fd, pgoffset) with
	| (0L, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x22L (* MAP_PRIVATE|MAP_ANONYMOUS *), -1, 0) ->
	    let fresh = self#fresh_addr length in
	      zero_region fresh (Int64.to_int length);
	      fresh
	| (_, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x32L (* MAP_PRIVATE|FIXED|ANONYMOUS *), -1, 0) ->
	    zero_region addr (Int64.to_int length);
	    addr
	| (0L, _, 
	   (0x1L|0x5L) (* PROT_READ|PROT_EXEC *),
	   (0x802L|0x2L) (* MAP_PRIVATE|MAP_DENYWRITE *), _, _) ->
	    let dest_addr = self#fresh_addr length in
	      do_read dest_addr
	| (_, _, 0x3L (* PROT_READ|PROT_WRITE *),
	   0x812L (* MAP_DENYWRITE|PRIVATE|FIXED *), _, _) ->
	    do_read addr
	| _ -> failwith "Unhandled mmap operation"
    in
      put_reg R_EAX ret

  method sys_mprotect addr len prot =
    (* treat as no-op *)
    put_reg R_EAX 0L;

  method sys_munmap addr len =
    (* treat as no-op *)
    put_reg R_EAX 0L

  method sys_open path flags mode =
    try
      let oc_flags = self#flags_to_oc_flags flags in
      let oc_fd = Unix.openfile path oc_flags mode and
	  vt_fd = self#fresh_fd () in
	Array.set unix_fds vt_fd (Some oc_fd);
	put_reg R_EAX (Int64.of_int vt_fd)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_read fd buf count =
    let str = String.create count in
    let num_read = Unix.read (self#get_fd fd) str 0 count in
      fm#store_str buf 0L (String.sub str 0 num_read);
      put_reg R_EAX (Int64.of_int num_read)

  method sys_readlink path out_buf buflen =
    let real = Unix.readlink path in
    let written = min buflen (String.length real) in
      fm#store_str out_buf 0L (String.sub real 0 written);
      put_reg R_EAX (Int64.of_int written);

  method sys_set_robust_list addr len =
    put_reg R_EAX 0L (* success *)

  method sys_set_thread_area uinfo =
    let old_ent = Int64.to_int (load_word (lea uinfo 0 0 0))
    and
	base  = load_word (lea uinfo 0 0 4) and
	limit = load_word (lea uinfo 0 0 8) in
      Printf.printf "set_thread_area({entry: %d, base: %Lx, limit: %Ld})\n"
	old_ent base limit;
      (match (old_ent, base, limit) with
	 | (-1, _, _) ->
	     let new_ent = 12L in
	       self#setup_tcb_seg new_ent base limit;
	       store_word uinfo 0 new_ent;
	       put_reg R_EAX 0L (* success *)
	 | _ -> failwith "Unhandled args to set_thread_area")

  method sys_set_tid_address addr =
    let pid = Unix.getpid () in
      put_reg R_EAX (Int64.of_int pid)

  method sys_rt_sigaction signum newbuf oldbuf setlen =
    (if oldbuf = 0L then () else
      let (action, mask_low, mask_high, flags) =
	match signum with
	  | 8 (* SIGFPE *) -> (0L, 0L, 0L, 0L)
	  | _ -> failwith
	      "Unhandled old signal in rt_sigaction";
      in
	store_word oldbuf 0 action;
	store_word oldbuf 4 flags;
	store_word oldbuf 8 0L; (* restorer *)
	store_word oldbuf 12 mask_low;
	store_word oldbuf 12 mask_high);
    put_reg R_EAX 0L; (* success *)

  method sys_rt_sigprocmask how newset oldset setlen =
    (if oldset = 0L then () else
       failwith "Can't report old mask");
    put_reg R_EAX 0L (* success *)

  method sys_stat64 path buf_addr =
    try
      let oc_buf = Unix.stat path in
	self#write_oc_statbuf buf_addr oc_buf;
	put_reg R_EAX 0L (* success *)
    with
      | Unix.Unix_error(err, _, _) -> self#put_errno err

  method sys_fstat64 fd buf_addr =
    let oc_buf =
      (if (fd <> 1) then
	 Unix.fstat (self#get_fd fd)
       else
	 Unix.stat "/etc/group") (* pretend stdout is always redirected *) in
      self#write_oc_statbuf buf_addr oc_buf;
      put_reg R_EAX 0L (* success *)

  method sys_statfs64 path buf_len struct_buf =
    assert(buf_len = 84);
    self#write_fake_statfs64buf struct_buf;
    put_reg R_EAX 0L (* success *)

  method sys_time addr =
    let time = Int64.of_float (Unix.time ()) in
      if addr != 0L then
	store_word addr 0 time else ();
      put_reg R_EAX time

  method sys_times addr =
    let float_to_clocks f = Int64.of_float (f *. 100.0) in
    let pt = Unix.times () in
    let ut = float_to_clocks (pt.Unix.tms_utime) and
	st = float_to_clocks (pt.Unix.tms_stime) and
	cut = float_to_clocks (pt.Unix.tms_cutime) and
	cst = float_to_clocks (pt.Unix.tms_cstime) in
      (* Printf.printf "times: %Ld %Ld %Ld %Ld\n" ut st cut cst; *)
      store_word addr 0 ut;
      store_word addr 4 st;		 
      store_word addr 8 cut;
      store_word addr 12 cst;
      put_reg R_EAX (Int64.add ut st)

  method sys_uname buf =
    List.iter2
      (fun i str ->
	 fm#store_cstr buf (Int64.mul 65L i) str)
      [0L; 1L; 2L; 3L; 4L; 5L]
      ["Linux"; (* sysname *)
       "amuro"; (* nodename *)
       "2.6.26-2-amd64"; (* release *)
       "#1 SMP Fri Mar 27 04:02:59 UTC 2009"; (*version*)
       "i686"; (* machine *)
       "cs.berkeley.edu" (* domain *)
      ];
    put_reg R_EAX 0L (* success *)

  method sys_write fd bytes count =
    self#do_write fd bytes count

  method sys_writev fd iov cnt =
    let bytes =
      Array.concat
	(Vine_util.mapn
	   (fun i -> fm#read_buf
	      (load_word (lea iov i 8 0)) (* iov_base *)
	      (Int64.to_int (load_word (lea iov i 8 4))))
	   (* iov_len *)
	   (cnt - 1)) in
      self#do_write fd bytes (Array.length bytes)

  method handle_linux_syscall () =
    (let syscall_num = Int64.to_int (fm#get_word_var R_EAX) and
	 ebx = fm#get_word_var R_EBX and
	 ecx = fm#get_word_var R_ECX and
	 edx = fm#get_word_var R_EDX and
	 esi = fm#get_word_var R_ESI and
	 edi = fm#get_word_var R_EDI and
	 ebp = fm#get_word_var R_EBP in
       match syscall_num with
	 | 0 -> (* restart_syscall *)
	     failwith "Unhandled Linux system call restart_syscall (0)"
	 | 1 -> (* exit *)
	     failwith "Unhandled Linux system call exit (1)"
	 | 2 -> (* fork *)
	     failwith "Unhandled Linux system call fork (2)"
	 | 3 -> (* read *)		    
	     let fd    = Int64.to_int ebx and
		 buf   = ecx and
		 count = Int64.to_int edx in
	       Printf.printf "read(%d, 0x%08Lx, %d)" fd buf count;
	       self#sys_read fd buf count;
	 | 4 -> (* write *)
	     let fd    = Int64.to_int ebx and
		 buf   = ecx and
		 count = Int64.to_int edx in
	       Printf.printf "write(%d, 0x%08Lx, %d)\n" fd buf count;
	       let bytes = fm#read_buf buf count in
		 self#sys_write fd bytes count
	 | 5 -> (* open *)
	     let path_buf = ebx and
		 flags    = Int64.to_int ecx and
		 mode     = Int64.to_int edx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "open(\"%s\", 0x%x, 0o%o)" path flags mode;
	       self#sys_open path flags mode
	 | 6 -> (* close *)
	     let fd = Int64.to_int ebx in
	       Printf.printf "close(%d)" fd;
	       self#sys_close fd
	 | 7 -> (* waitpid *)
	     failwith "Unhandled Linux system call waitpid (7)"
	 | 8 -> (* creat *)
	     failwith "Unhandled Linux system call creat (8)"
	 | 9 -> (* link *)
	     failwith "Unhandled Linux system call link (9)"
	 | 10 -> (* unlink *)
	     failwith "Unhandled Linux system call unlink (10)"
	 | 11 -> (* execve *)
	     failwith "Unhandled Linux system call execve (11)"
	 | 12 -> (* chdir *)
	     failwith "Unhandled Linux system call chdir (12)"
	 | 13 -> (* time *)
	     let addr = ebx in
	       Printf.printf "time(0x%08Lx)" addr;
	       self#sys_time addr
	 | 14 -> (* mknod *)
	     failwith "Unhandled Linux system call mknod (14)"
	 | 15 -> (* chmod *)
	     failwith "Unhandled Linux system call chmod (15)"
	 | 16 -> (* lchown *)
	     failwith "Unhandled Linux system call lchown (16)"
	 | 17 -> (* break *)
	     failwith "Unhandled Linux system call break (17)"
	 | 18 -> (* oldstat *)
	     failwith "Unhandled Linux system call oldstat (18)"
	 | 19 -> (* lseek *)
	     failwith "Unhandled Linux system call lseek (19)"
	 | 20 -> (* getpid *)
	     failwith "Unhandled Linux system call getpid (20)"
	 | 21 -> (* mount *)
	     failwith "Unhandled Linux system call mount (21)"
	 | 22 -> (* umount *)
	     failwith "Unhandled Linux system call umount (22)"
	 | 23 -> (* setuid *)
	     failwith "Unhandled Linux system call setuid (23)"
	 | 24 -> (* getuid *)
	     failwith "Unhandled Linux system call getuid (24)"
	 | 25 -> (* stime *)
	     failwith "Unhandled Linux system call stime (25)"
	 | 26 -> (* ptrace *)
	     failwith "Unhandled Linux system call ptrace (26)"
	 | 27 -> (* alarm *)
	     failwith "Unhandled Linux system call alarm (27)"
	 | 28 -> (* oldfstat *)
	     failwith "Unhandled Linux system call oldfstat (28)"
	 | 29 -> (* pause *)
	     failwith "Unhandled Linux system call pause (29)"
	 | 30 -> (* utime *)
	     failwith "Unhandled Linux system call utime (30)"
	 | 31 -> (* stty *)
	     failwith "Unhandled Linux system call stty (31)"
	 | 32 -> (* gtty *)
	     failwith "Unhandled Linux system call gtty (32)"
	 | 33 -> (* access *)
	     let path_buf = ebx and
		 mode     = Int64.to_int ecx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "access(\"%s\", 0x%x)" path mode;
	       self#sys_access path mode
	 | 34 -> (* nice *)
	     failwith "Unhandled Linux system call nice (34)"
	 | 35 -> (* ftime *)
	     failwith "Unhandled Linux system call ftime (35)"
	 | 36 -> (* sync *)
	     failwith "Unhandled Linux system call sync (36)"
	 | 37 -> (* kill *)
	     failwith "Unhandled Linux system call kill (37)"
	 | 38 -> (* rename *)
	     failwith "Unhandled Linux system call rename (38)"
	 | 39 -> (* mkdir *)
	     failwith "Unhandled Linux system call mkdir (39)"
	 | 40 -> (* rmdir *)
	     failwith "Unhandled Linux system call rmdir (40)"
	 | 41 -> (* dup *)
	     failwith "Unhandled Linux system call dup (41)"
	 | 42 -> (* pipe *)
	     failwith "Unhandled Linux system call pipe (42)"
	 | 43 -> (* times *)
	     let addr = ebx in
	       Printf.printf "times(0x%08Lx)" addr;
	       self#sys_times addr
	 | 44 -> (* prof *)
	     failwith "Unhandled Linux system call prof (44)"
	 | 45 -> (* brk *)
	     let addr = ebx in
	       Printf.printf "brk(0x%08Lx)" addr;
	       self#sys_brk addr
	 | 46 -> (* setgid *)
	     failwith "Unhandled Linux system call setgid (46)"
	 | 47 -> (* getgid *)
	     failwith "Unhandled Linux system call getgid (47)"
	 | 48 -> (* signal *)
	     failwith "Unhandled Linux system call signal (48)"
	 | 49 -> (* geteuid *)
	     failwith "Unhandled Linux system call geteuid (49)"
	 | 50 -> (* getegid *)
	     failwith "Unhandled Linux system call getegid (50)"
	 | 51 -> (* acct *)
	     failwith "Unhandled Linux system call acct (51)"
	 | 52 -> (* umount2 *)
	     failwith "Unhandled Linux system call umount2 (52)"
	 | 53 -> (* lock *)
	     failwith "Unhandled Linux system call lock (53)"
	 | 54 -> (* ioctl *)
	     let fd   = Int64.to_int ebx and
		 req  = ecx and
		 argp = edx in
	       Printf.printf "ioctl(%d, 0x%Lx, 0x%08Lx)" fd req argp;
	       self#sys_ioctl fd req argp;
	 | 55 -> (* fcntl *)
	     failwith "Unhandled Linux system call fcntl (55)"
	 | 56 -> (* mpx *)
	     failwith "Unhandled Linux system call mpx (56)"
	 | 57 -> (* setpgid *)
	     failwith "Unhandled Linux system call setpgid (57)"
	 | 58 -> (* ulimit *)
	     failwith "Unhandled Linux system call ulimit (58)"
	 | 59 -> (* oldolduname *)
	     failwith "Unhandled Linux system call oldolduname (59)"
	 | 60 -> (* umask *)
	     failwith "Unhandled Linux system call umask (60)"
	 | 61 -> (* chroot *)
	     failwith "Unhandled Linux system call chroot (61)"
	 | 62 -> (* ustat *)
	     failwith "Unhandled Linux system call ustat (62)"
	 | 63 -> (* dup2 *)
	     failwith "Unhandled Linux system call dup2 (63)"
	 | 64 -> (* getppid *)
	     failwith "Unhandled Linux system call getppid (64)"
	 | 65 -> (* getpgrp *)
	     failwith "Unhandled Linux system call getpgrp (65)"
	 | 66 -> (* setsid *)
	     failwith "Unhandled Linux system call setsid (66)"
	 | 67 -> (* sigaction *)
	     failwith "Unhandled Linux system call sigaction (67)"
	 | 68 -> (* sgetmask *)
	     failwith "Unhandled Linux system call sgetmask (68)"
	 | 69 -> (* ssetmask *)
	     failwith "Unhandled Linux system call ssetmask (69)"
	 | 70 -> (* setreuid *)
	     failwith "Unhandled Linux system call setreuid (70)"
	 | 71 -> (* setregid *)
	     failwith "Unhandled Linux system call setregid (71)"
	 | 72 -> (* sigsuspend *)
	     failwith "Unhandled Linux system call sigsuspend (72)"
	 | 73 -> (* sigpending *)
	     failwith "Unhandled Linux system call sigpending (73)"
	 | 74 -> (* sethostname *)
	     failwith "Unhandled Linux system call sethostname (74)"
	 | 75 -> (* setrlimit *)
	     failwith "Unhandled Linux system call setrlimit (75)"
	 | 76 -> (* getrlimit *)
	     failwith "Unhandled Linux system call getrlimit (76)"
	 | 77 -> (* getrusage *)
	     failwith "Unhandled Linux system call getrusage (77)"
	 | 78 -> (* gettimeofday *)
	     failwith "Unhandled Linux system call gettimeofday (78)"
	 | 79 -> (* settimeofday *)
	     failwith "Unhandled Linux system call settimeofday (79)"
	 | 80 -> (* getgroups *)
	     failwith "Unhandled Linux system call getgroups (80)"
	 | 81 -> (* setgroups *)
	     failwith "Unhandled Linux system call setgroups (81)"
	 | 82 -> (* select *)
	     failwith "Unhandled Linux system call select (82)"
	 | 83 -> (* symlink *)
	     failwith "Unhandled Linux system call symlink (83)"
	 | 84 -> (* oldlstat *)
	     failwith "Unhandled Linux system call oldlstat (84)"
	 | 85 -> (* readlink *)
	     let path_buf = ebx and
		 out_buf  = ecx and
		 buflen   = Int64.to_int edx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "readlink(\"%s\", 0x%08Lx, %d)"
		 path out_buf buflen;
	       self#sys_readlink path out_buf buflen
	 | 86 -> (* uselib *)
	     failwith "Unhandled Linux system call uselib (86)"
	 | 87 -> (* swapon *)
	     failwith "Unhandled Linux system call swapon (87)"
	 | 88 -> (* reboot *)
	     failwith "Unhandled Linux system call reboot (88)"
	 | 89 -> (* readdir *)
	     failwith "Unhandled Linux system call readdir (89)"
	 | 90 -> (* mmap *)
	     failwith "Unhandled Linux system call mmap (90)"
	 | 91 -> (* munmap *)
	     let addr = ebx and
		 len  = ecx in
	       Printf.printf "munmap(0x%08Lx, %Ld)" addr len;
	       self#sys_munmap addr len
	 | 92 -> (* truncate *)
	     failwith "Unhandled Linux system call truncate (92)"
	 | 93 -> (* ftruncate *)
	     failwith "Unhandled Linux system call ftruncate (93)"
	 | 94 -> (* fchmod *)
	     failwith "Unhandled Linux system call fchmod (94)"
	 | 95 -> (* fchown *)
	     failwith "Unhandled Linux system call fchown (95)"
	 | 96 -> (* getpriority *)
	     failwith "Unhandled Linux system call getpriority (96)"
	 | 97 -> (* setpriority *)
	     failwith "Unhandled Linux system call setpriority (97)"
	 | 98 -> (* profil *)
	     failwith "Unhandled Linux system call profil (98)"
	 | 99 -> (* statfs *)
	     failwith "Unhandled Linux system call statfs (99)"
	 | 100 -> (* fstatfs *)
	     failwith "Unhandled Linux system call fstatfs (100)"
	 | 101 -> (* ioperm *)
	     failwith "Unhandled Linux system call ioperm (101)"
	 | 102 -> (* socketcall *)
	     failwith "Unhandled Linux system call socketcall (102)"
	 | 103 -> (* syslog *)
	     failwith "Unhandled Linux system call syslog (103)"
	 | 104 -> (* setitimer *)
	     failwith "Unhandled Linux system call setitimer (104)"
	 | 105 -> (* getitimer *)
	     failwith "Unhandled Linux system call getitimer (105)"
	 | 106 -> (* stat *)
	     failwith "Unhandled Linux system call stat (106)"
	 | 107 -> (* lstat *)
	     failwith "Unhandled Linux system call lstat (107)"
	 | 108 -> (* fstat *)
	     failwith "Unhandled Linux system call fstat (108)"
	 | 109 -> (* olduname *)
	     failwith "Unhandled Linux system call olduname (109)"
	 | 110 -> (* iopl *)
	     failwith "Unhandled Linux system call iopl (110)"
	 | 111 -> (* vhangup *)
	     failwith "Unhandled Linux system call vhangup (111)"
	 | 112 -> (* idle *)
	     failwith "Unhandled Linux system call idle (112)"
	 | 113 -> (* vm86old *)
	     failwith "Unhandled Linux system call vm86old (113)"
	 | 114 -> (* wait4 *)
	     failwith "Unhandled Linux system call wait4 (114)"
	 | 115 -> (* swapoff *)
	     failwith "Unhandled Linux system call swapoff (115)"
	 | 116 -> (* sysinfo *)
	     failwith "Unhandled Linux system call sysinfo (116)"
	 | 117 -> (* ipc *)
	     failwith "Unhandled Linux system call ipc (117)"
	 | 118 -> (* fsync *)
	     failwith "Unhandled Linux system call fsync (118)"
	 | 119 -> (* sigreturn *)
	     failwith "Unhandled Linux system call sigreturn (119)"
	 | 120 -> (* clone *)
	     failwith "Unhandled Linux system call clone (120)"
	 | 121 -> (* setdomainname *)
	     failwith "Unhandled Linux system call setdomainname (121)"
	 | 122 -> (* uname *)
	     let buf = ebx in
	       Printf.printf "uname(0x%08Lx)" buf;
	       self#sys_uname buf
	 | 123 -> (* modify_ldt *)
	     failwith "Unhandled Linux system call modify_ldt (123)"
	 | 124 -> (* adjtimex *)
	     failwith "Unhandled Linux system call adjtimex (124)"
	 | 125 -> (* mprotect *)
	     let addr = ebx and
		 len  = ecx and
		 prot = edx in
	       Printf.printf "mprotect(0x%08Lx, %Ld, %Ld)" addr len prot;
	       self#sys_mprotect addr len prot
	 | 126 -> (* sigprocmask *)
	     failwith "Unhandled Linux system call sigprocmask (126)"
	 | 127 -> (* create_module *)
	     failwith "Unhandled Linux system call create_module (127)"
	 | 128 -> (* init_module *)
	     failwith "Unhandled Linux system call init_module (128)"
	 | 129 -> (* delete_module *)
	     failwith "Unhandled Linux system call delete_module (129)"
	 | 130 -> (* get_kernel_syms *)
	     failwith "Unhandled Linux system call get_kernel_syms (130)"
	 | 131 -> (* quotactl *)
	     failwith "Unhandled Linux system call quotactl (131)"
	 | 132 -> (* getpgid *)
	     failwith "Unhandled Linux system call getpgid (132)"
	 | 133 -> (* fchdir *)
	     failwith "Unhandled Linux system call fchdir (133)"
	 | 134 -> (* bdflush *)
	     failwith "Unhandled Linux system call bdflush (134)"
	 | 135 -> (* sysfs *)
	     failwith "Unhandled Linux system call sysfs (135)"
	 | 136 -> (* personality *)
	     failwith "Unhandled Linux system call personality (136)"
	 | 137 -> (* afs_syscall *)
	     failwith "Unhandled Linux system call afs_syscall (137)"
	 | 138 -> (* setfsuid *)
	     failwith "Unhandled Linux system call setfsuid (138)"
	 | 139 -> (* setfsgid *)
	     failwith "Unhandled Linux system call setfsgid (139)"
	 | 140 -> (* _llseek *)
	     failwith "Unhandled Linux system call _llseek (140)"
	 | 141 -> (* getdents *)
	     failwith "Unhandled Linux system call getdents (141)"
	 | 142 -> (* _newselect *)
	     failwith "Unhandled Linux system call _newselect (142)"
	 | 143 -> (* flock *)
	     failwith "Unhandled Linux system call flock (143)"
	 | 144 -> (* msync *)
	     failwith "Unhandled Linux system call msync (144)"
	 | 145 -> (* readv *)
	     failwith "Unhandled Linux system call readv (145)"
	 | 146 -> (* writev *)
	     let fd  = Int64.to_int ebx and
		 iov = ecx and
		 cnt = Int64.to_int edx in
	       Printf.printf "writev(%d, 0x%08Lx, %d)" fd iov cnt;
	       self#sys_writev fd iov cnt
	 | 147 -> (* getsid *)
	     failwith "Unhandled Linux system call getsid (147)"
	 | 148 -> (* fdatasync *)
	     failwith "Unhandled Linux system call fdatasync (148)"
	 | 149 -> (* _sysctl *)
	     failwith "Unhandled Linux system call _sysctl (149)"
	 | 150 -> (* mlock *)
	     failwith "Unhandled Linux system call mlock (150)"
	 | 151 -> (* munlock *)
	     failwith "Unhandled Linux system call munlock (151)"
	 | 152 -> (* mlockall *)
	     failwith "Unhandled Linux system call mlockall (152)"
	 | 153 -> (* munlockall *)
	     failwith "Unhandled Linux system call munlockall (153)"
	 | 154 -> (* sched_setparam *)
	     failwith "Unhandled Linux system call sched_setparam (154)"
	 | 155 -> (* sched_getparam *)
	     failwith "Unhandled Linux system call sched_getparam (155)"
	 | 156 -> (* sched_setscheduler *)
	     failwith "Unhandled Linux system call sched_setscheduler (156)"
	 | 157 -> (* sched_getscheduler *)
	     failwith "Unhandled Linux system call sched_getscheduler (157)"
	 | 158 -> (* sched_yield *)
	     failwith "Unhandled Linux system call sched_yield (158)"
	 | 159 -> (* sched_get_priority_max *)
	     failwith "Unhandled Linux system call sched_get_priority_max (159)"
	 | 160 -> (* sched_get_priority_min *)
	     failwith "Unhandled Linux system call sched_get_priority_min (160)"
	 | 161 -> (* sched_rr_get_interval *)
	     failwith "Unhandled Linux system call sched_rr_get_interval (161)"
	 | 162 -> (* nanosleep *)
	     failwith "Unhandled Linux system call nanosleep (162)"
	 | 163 -> (* mremap *)
	     failwith "Unhandled Linux system call mremap (163)"
	 | 164 -> (* setresuid *)
	     failwith "Unhandled Linux system call setresuid (164)"
	 | 165 -> (* getresuid *)
	     failwith "Unhandled Linux system call getresuid (165)"
	 | 166 -> (* vm86 *)
	     failwith "Unhandled Linux system call vm86 (166)"
	 | 167 -> (* query_module *)
	     failwith "Unhandled Linux system call query_module (167)"
	 | 168 -> (* poll *)
	     failwith "Unhandled Linux system call poll (168)"
	 | 169 -> (* nfsservctl *)
	     failwith "Unhandled Linux system call nfsservctl (169)"
	 | 170 -> (* setresgid *)
	     failwith "Unhandled Linux system call setresgid (170)"
	 | 171 -> (* getresgid *)
	     failwith "Unhandled Linux system call getresgid (171)"
	 | 172 -> (* prctl *)
	     failwith "Unhandled Linux system call prctl (172)"
	 | 173 -> (* rt_sigreturn *)
	     failwith "Unhandled Linux system call rt_sigreturn (173)"
	 | 174 -> (* rt_sigaction *)
	     let signum = Int64.to_int ebx and
		 newbuf = ecx and
		 oldbuf = edx and
		 setlen = Int64.to_int esi in
	       Printf.printf "rt_sigaction(%d, 0x%08Lx, 0x%08Lx, %d)"
		 signum newbuf oldbuf setlen;
	       self#sys_rt_sigaction signum newbuf oldbuf setlen
	 | 175 -> (* rt_sigprocmask *)
	     let how    = Int64.to_int ebx and
		 newset = ecx and
		 oldset = edx and
		 setlen = Int64.to_int esi in
	       Printf.printf "rt_sigprocmask(%d, 0x%08Lx, 0x%08Lx, %d)"
		 how newset oldset setlen;
	       self#sys_rt_sigprocmask how newset oldset setlen
	 | 176 -> (* rt_sigpending *)
	     failwith "Unhandled Linux system call rt_sigpending (176)"
	 | 177 -> (* rt_sigtimedwait *)
	     failwith "Unhandled Linux system call rt_sigtimedwait (177)"
	 | 178 -> (* rt_sigqueueinfo *)
	     failwith "Unhandled Linux system call rt_sigqueueinfo (178)"
	 | 179 -> (* rt_sigsuspend *)
	     failwith "Unhandled Linux system call rt_sigsuspend (179)"
	 | 180 -> (* pread64 *)
	     failwith "Unhandled Linux system call pread64 (180)"
	 | 181 -> (* pwrite64 *)
	     failwith "Unhandled Linux system call pwrite64 (181)"
	 | 182 -> (* chown *)
	     failwith "Unhandled Linux system call chown (182)"
	 | 183 -> (* getcwd *)
	     failwith "Unhandled Linux system call getcwd (183)"
	 | 184 -> (* capget *)
	     failwith "Unhandled Linux system call capget (184)"
	 | 185 -> (* capset *)
	     failwith "Unhandled Linux system call capset (185)"
	 | 186 -> (* sigaltstack *)
	     failwith "Unhandled Linux system call sigaltstack (186)"
	 | 187 -> (* sendfile *)
	     failwith "Unhandled Linux system call sendfile (187)"
	 | 188 -> (* getpmsg *)
	     failwith "Unhandled Linux system call getpmsg (188)"
	 | 189 -> (* putpmsg *)
	     failwith "Unhandled Linux system call putpmsg (189)"
	 | 190 -> (* vfork *)
	     failwith "Unhandled Linux system call vfork (190)"
	 | 191 -> (* ugetrlimit *)
	     let rsrc = Int64.to_int ebx and
		 buf  = ecx in
	       Printf.printf "ugetrlimit(%d, 0x%08Lx)" rsrc buf;
	       self#sys_ugetrlimit rsrc buf
	 | 192 -> (* mmap2 *)
	     let addr     = ebx and
		 length   = ecx and
		 prot     = edx and
		 flags    = esi and
		 fd       = Int64.to_int edi and
		 pgoffset = Int64.to_int ebp in
	       Printf.printf "mmap2(0x%08Lx, %Ld, 0x%Lx, 0x%0Lx, %d, %d)"
		 addr length prot flags fd pgoffset;
	       self#sys_mmap2 addr length prot flags fd pgoffset
	 | 193 -> (* truncate64 *)
	     failwith "Unhandled Linux system call truncate64 (193)"
	 | 194 -> (* ftruncate64 *)
	     failwith "Unhandled Linux system call ftruncate64 (194)"
	 | 195 -> (* stat64 *)
	     let path_buf = ebx and
		 buf_addr = ecx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "stat64(\"%s\", 0x%08Lx)" path buf_addr;
	       self#sys_stat64 path buf_addr
	 | 196 -> (* lstat64 *)
	     failwith "Unhandled Linux system call lstat64 (196)"
	 | 197 -> (* fstat64 *)
	     let fd = Int64.to_int ebx and
		 buf_addr = ecx in
	       Printf.printf "fstat64(%d, 0x%08Lx)" fd buf_addr;
	       self#sys_fstat64 fd buf_addr
	 | 198 -> (* lchown32 *)
	     failwith "Unhandled Linux system call lchown32 (198)"
	 | 199 -> (* getuid32 *)
	     Printf.printf "getuid32()";
	     self#sys_getuid32 ()
	 | 200 -> (* getgid32 *)
	     Printf.printf "getgid32()";
	     self#sys_getgid32 ()
	 | 201 -> (* geteuid32 *)
	     Printf.printf "geteuid32()";
	     self#sys_geteuid32 ()
	 | 202 -> (* getegid32 *)
	     Printf.printf "getegid32()";
	     self#sys_getegid32 ()
	 | 203 -> (* setreuid32 *)
	     failwith "Unhandled Linux system call setreuid32 (203)"
	 | 204 -> (* setregid32 *)
	     failwith "Unhandled Linux system call setregid32 (204)"
	 | 205 -> (* getgroups32 *)
	     failwith "Unhandled Linux system call getgroups32 (205)"
	 | 206 -> (* setgroups32 *)
	     failwith "Unhandled Linux system call setgroups32 (206)"
	 | 207 -> (* fchown32 *)
	     failwith "Unhandled Linux system call fchown32 (207)"
	 | 208 -> (* setresuid32 *)
	     failwith "Unhandled Linux system call setresuid32 (208)"
	 | 209 -> (* getresuid32 *)
	     failwith "Unhandled Linux system call getresuid32 (209)"
	 | 210 -> (* setresgid32 *)
	     failwith "Unhandled Linux system call setresgid32 (210)"
	 | 211 -> (* getresgid32 *)
	     failwith "Unhandled Linux system call getresgid32 (211)"
	 | 212 -> (* chown32 *)
	     failwith "Unhandled Linux system call chown32 (212)"
	 | 213 -> (* setuid32 *)
	     failwith "Unhandled Linux system call setuid32 (213)"
	 | 214 -> (* setgid32 *)
	     failwith "Unhandled Linux system call setgid32 (214)"
	 | 215 -> (* setfsuid32 *)
	     failwith "Unhandled Linux system call setfsuid32 (215)"
	 | 216 -> (* setfsgid32 *)
	     failwith "Unhandled Linux system call setfsgid32 (216)"
	 | 217 -> (* pivot_root *)
	     failwith "Unhandled Linux system call pivot_root (217)"
	 | 218 -> (* mincore *)
	     failwith "Unhandled Linux system call mincore (218)"
	 | 219 -> (* madvise *)
	     failwith "Unhandled Linux system call madvise (219)"
	 | 220 -> (* getdents64 *)
	     failwith "Unhandled Linux system call getdents64 (220)"
	 | 221 -> (* fcntl64 *)
	     failwith "Unhandled Linux system call fcntl64 (221)"
	 | 224 -> (* gettid *)
	     failwith "Unhandled Linux system call gettid (224)"
	 | 225 -> (* readahead *)
	     failwith "Unhandled Linux system call readahead (225)"
	 | 226 -> (* setxattr *)
	     failwith "Unhandled Linux system call setxattr (226)"
	 | 227 -> (* lsetxattr *)
	     failwith "Unhandled Linux system call lsetxattr (227)"
	 | 228 -> (* fsetxattr *)
	     failwith "Unhandled Linux system call fsetxattr (228)"
	 | 229 -> (* getxattr *)
	     failwith "Unhandled Linux system call getxattr (229)"
	 | 230 -> (* lgetxattr *)
	     failwith "Unhandled Linux system call lgetxattr (230)"
	 | 231 -> (* fgetxattr *)
	     failwith "Unhandled Linux system call fgetxattr (231)"
	 | 232 -> (* listxattr *)
	     failwith "Unhandled Linux system call listxattr (232)"
	 | 233 -> (* llistxattr *)
	     failwith "Unhandled Linux system call llistxattr (233)"
	 | 234 -> (* flistxattr *)
	     failwith "Unhandled Linux system call flistxattr (234)"
	 | 235 -> (* removexattr *)
	     failwith "Unhandled Linux system call removexattr (235)"
	 | 236 -> (* lremovexattr *)
	     failwith "Unhandled Linux system call lremovexattr (236)"
	 | 237 -> (* fremovexattr *)
	     failwith "Unhandled Linux system call fremovexattr (237)"
	 | 238 -> (* tkill *)
	     failwith "Unhandled Linux system call tkill (238)"
	 | 239 -> (* sendfile64 *)
	     failwith "Unhandled Linux system call sendfile64 (239)"
	 | 240 -> (* futex *)
	     let uaddr    = ebx and
		 op       = Int64.to_int ecx and
		 value    = edx and
		 timebuf  = esi and
		 uaddr2   = edi and
		 val3     = ebp in
	       Printf.printf "futex(0x%08Lx, %d, %Ld, 0x%08Lx, 0x%08Lx, %Ld)"
		 uaddr op value timebuf uaddr2 val3;
	       self#sys_futex uaddr op value timebuf uaddr2 val3
	 | 241 -> (* sched_setaffinity *)
	     failwith "Unhandled Linux system call sched_setaffinity (241)"
	 | 242 -> (* sched_getaffinity *)
	     failwith "Unhandled Linux system call sched_getaffinity (242)"
	 | 243 -> (* set_thread_area *)
	     let uinfo = ebx in
	       Printf.printf "set_thread_area(0x%08Lx)" uinfo;
	       self#sys_set_thread_area uinfo
	 | 244 -> (* get_thread_area *)
	     failwith "Unhandled Linux system call get_thread_area (244)"
	 | 245 -> (* io_setup *)
	     failwith "Unhandled Linux system call io_setup (245)"
	 | 246 -> (* io_destroy *)
	     failwith "Unhandled Linux system call io_destroy (246)"
	 | 247 -> (* io_getevents *)
	     failwith "Unhandled Linux system call io_getevents (247)"
	 | 248 -> (* io_submit *)
	     failwith "Unhandled Linux system call io_submit (248)"
	 | 249 -> (* io_cancel *)
	     failwith "Unhandled Linux system call io_cancel (249)"
	 | 250 -> (* fadvise64 *)
	     failwith "Unhandled Linux system call fadvise64 (250)"
	 | 252 -> (* exit_group *)
	     let status = ebx in
	       Printf.printf "exit_group(%Ld) (no return)\n" status;
	       self#sys_exit_group status
	 | 253 -> (* lookup_dcookie *)
	     failwith "Unhandled Linux system call lookup_dcookie (253)"
	 | 254 -> (* epoll_create *)
	     failwith "Unhandled Linux system call epoll_create (254)"
	 | 255 -> (* epoll_ctl *)
	     failwith "Unhandled Linux system call epoll_ctl (255)"
	 | 256 -> (* epoll_wait *)
	     failwith "Unhandled Linux system call epoll_wait (256)"
	 | 257 -> (* remap_file_pages *)
	     failwith "Unhandled Linux system call remap_file_pages (257)"
	 | 258 -> (* set_tid_address *)
	     let addr = ebx in
	       Printf.printf "set_tid_address(0x08%Lx)" addr;
	       self#sys_set_tid_address addr
	 | 259 -> (* timer_create *)
	     failwith "Unhandled Linux system call timer_create (259)"
	 | 268 -> (* statfs64 *)
	     let path_buf = ebx and
		 buf_len = Int64.to_int ecx and
		 struct_buf = edx in
	     let path = fm#read_cstr path_buf in
	       Printf.printf "statfs64(\"%s\", %d, 0x%08Lx)"
		 path buf_len struct_buf;
	       self#sys_statfs64 path buf_len struct_buf
	 | 269 -> (* fstatfs64 *)
	     failwith "Unhandled Linux system call fstatfs64 (269)"
	 | 270 -> (* tgkill *)
	     failwith "Unhandled Linux system call tgkill (270)"
	 | 271 -> (* utimes *)
	     failwith "Unhandled Linux system call utimes (271)"
	 | 272 -> (* fadvise64_64 *)
	     failwith "Unhandled Linux system call fadvise64_64 (272)"
	 | 273 -> (* vserver *)
	     failwith "Unhandled Linux system call vserver (273)"
	 | 274 -> (* mbind *)
	     failwith "Unhandled Linux system call mbind (274)"
	 | 275 -> (* get_mempolicy *)
	     failwith "Unhandled Linux system call get_mempolicy (275)"
	 | 276 -> (* set_mempolicy *)
	     failwith "Unhandled Linux system call set_mempolicy (276)"
	 | 277 -> (* mq_open *)
	     failwith "Unhandled Linux system call mq_open (277)"
	 | 283 -> (* kexec_load *)
	     failwith "Unhandled Linux system call kexec_load (283)"
	 | 284 -> (* waitid *)
	     failwith "Unhandled Linux system call waitid (284)"
	 | 286 -> (* add_key *)
	     failwith "Unhandled Linux system call add_key (286)"
	 | 287 -> (* request_key *)
	     failwith "Unhandled Linux system call request_key (287)"
	 | 288 -> (* keyctl *)
	     failwith "Unhandled Linux system call keyctl (288)"
	 | 289 -> (* ioprio_set *)
	     failwith "Unhandled Linux system call ioprio_set (289)"
	 | 290 -> (* ioprio_get *)
	     failwith "Unhandled Linux system call ioprio_get (290)"
	 | 291 -> (* inotify_init *)
	     failwith "Unhandled Linux system call inotify_init (291)"
	 | 292 -> (* inotify_add_watch *)
	     failwith "Unhandled Linux system call inotify_add_watch (292)"
	 | 293 -> (* inotify_rm_watch *)
	     failwith "Unhandled Linux system call inotify_rm_watch (293)"
	 | 294 -> (* migrate_pages *)
	     failwith "Unhandled Linux system call migrate_pages (294)"
	 | 295 -> (* openat *)
	     failwith "Unhandled Linux system call openat (295)"
	 | 296 -> (* mkdirat *)
	     failwith "Unhandled Linux system call mkdirat (296)"
	 | 297 -> (* mknodat *)
	     failwith "Unhandled Linux system call mknodat (297)"
	 | 298 -> (* fchownat *)
	     failwith "Unhandled Linux system call fchownat (298)"
	 | 299 -> (* futimesat *)
	     failwith "Unhandled Linux system call futimesat (299)"
	 | 300 -> (* fstatat64 *)
	     failwith "Unhandled Linux system call fstatat64 (300)"
	 | 301 -> (* unlinkat *)
	     failwith "Unhandled Linux system call unlinkat (301)"
	 | 302 -> (* renameat *)
	     failwith "Unhandled Linux system call renameat (302)"
	 | 303 -> (* linkat *)
	     failwith "Unhandled Linux system call linkat (303)"
	 | 304 -> (* symlinkat *)
	     failwith "Unhandled Linux system call symlinkat (304)"
	 | 305 -> (* readlinkat *)
	     failwith "Unhandled Linux system call readlinkat (305)"
	 | 306 -> (* fchmodat *)
	     failwith "Unhandled Linux system call fchmodat (306)"
	 | 307 -> (* faccessat *)
	     failwith "Unhandled Linux system call faccessat (307)"
	 | 308 -> (* pselect6 *)
	     failwith "Unhandled Linux system call pselect6 (308)"
	 | 309 -> (* ppoll *)
	     failwith "Unhandled Linux system call ppoll (309)"
	 | 310 -> (* unshare *)
	     failwith "Unhandled Linux system call unshare (310)"
	 | 311 -> (* set_robust_list *)
	     let addr = ebx and
		 len  = ecx in
	       Printf.printf "set_robust_list(0x08%Lx, %Ld)" addr len;
	       self#sys_set_robust_list addr len
	 | 312 -> (* get_robust_list *)
	     failwith "Unhandled Linux system call get_robust_list (312)"
	 | 313 -> (* splice *)
	     failwith "Unhandled Linux system call splice (313)"
	 | 314 -> (* sync_file_range *)
	     failwith "Unhandled Linux system call sync_file_range (314)"
	 | 315 -> (* tee *)
	     failwith "Unhandled Linux system call tee (315)"
	 | 316 -> (* vmsplice *)
	     failwith "Unhandled Linux system call vmsplice (316)"
	 | 317 -> (* move_pages *)
	     failwith "Unhandled Linux system call move_pages (317)"
	 | 318 -> (* getcpu *)
	     failwith "Unhandled Linux system call getcpu (318)"
	 | 319 -> (* epoll_pwait *)
	     failwith "Unhandled Linux system call epoll_pwait (319)"
	 | 320 -> (* utimensat *)
	     failwith "Unhandled Linux system call utimensat (320)"
	 | 321 -> (* signalfd *)
	     failwith "Unhandled Linux system call signalfd (321)"
	 | 322 -> (* timerfd_create *)
	     failwith "Unhandled Linux system call timerfd_create (322)"
	 | 323 -> (* eventfd *)
	     failwith "Unhandled Linux system call eventfd (323)"
	 | 324 -> (* fallocate *)
	     failwith "Unhandled Linux system call fallocate (324)"
	 | 325 -> (* timerfd_settime *)
	     failwith "Unhandled Linux system call timerfd_settime (325)"
	 | 326 -> (* timerfd_gettime *)
	     failwith "Unhandled Linux system call timerfd_gettime (326)"
	 | _ ->
	     Printf.printf "Unhandled system call %d\n" syscall_num;
	     failwith "Unhandled Linux system call");
    Printf.printf " = %Ld (0x%08Lx)\n"
      (fix_s32 (fm#get_word_var R_EAX)) (fm#get_word_var R_EAX);
    flush stdout

  method handle_special str =
    match str with
      | "int 0x80" -> self#handle_linux_syscall (); true
      | _ -> false

end

exception UnhandledTrap

class trap_special_nonhandler fm =
object(self)
  method handle_special str =
    match str with
      | "trap" -> raise UnhandledTrap
      | _ -> false
end

let opt_trace_setup = ref false

module LinuxLoader = struct
  type elf_header = {
    eh_type : int;
    machine : int;
    version : int64;
    entry : int64;
    phoff : int64;
    shoff : int64;
    eh_flags : int64;
    ehsize : int;
    phentsize : int;
    phnum : int;
    shentsize : int;
    shnum : int;
    shstrndx : int
  }

  (* "Program header" is the standard ELF terminology, but it would be
     more natural to call this a "segment header". (However the
     abbreviation "SH" in ELF always stands for "section header", which
     is something else.) *)
  type program_header = {
    ph_type : int64;
    offset : int64;
    vaddr : int64;
    paddr : int64;
    filesz : int64;
    memsz : int64;
    ph_flags : int64;
    align : int64
  }

  let read_ui32 i =
    Int64.logand 0xffffffffL (Int64.of_int32 (IO.read_real_i32 i))

  let read_elf_header ic =
    let i = IO.input_channel ic in
    let ident = IO.really_nread i 16 in
      assert(ident = 
	  "\x7fELF\001\001\001\000\000\000\000\000\000\000\000\000");
      (* OCaml structure initialization isn't guaranteed to happen
	 left to right, so we need to use a bunch of lets here: *)
      let eh_type = IO.read_ui16 i in
      let machine = IO.read_ui16 i in
      let version = read_ui32 i in
      let entry = read_ui32 i in
      let phoff = read_ui32 i in
      let shoff = read_ui32 i in
      let eh_flags = read_ui32 i in
      let ehsize = IO.read_ui16 i in
      let phentsize = IO.read_ui16 i in
      let phnum = IO.read_ui16 i in
      let shentsize = IO.read_ui16 i in
      let shnum = IO.read_ui16 i in
      let shstrndx = IO.read_ui16 i in
      let eh = {
	eh_type = eh_type; machine = machine; version = version;
	entry = entry; phoff = phoff; shoff = shoff; eh_flags = eh_flags;
	ehsize = ehsize; phentsize = phentsize; phnum = phnum;
	shentsize = shentsize; shnum = shnum; shstrndx = shstrndx }
      in
	assert(eh.ehsize = 16 + 36);
	eh

  let read_program_headers ic eh =
    assert(eh.phentsize = 32);
    seek_in ic (Int64.to_int eh.phoff);
    let i = IO.input_channel ic in
      ExtList.List.init eh.phnum
	(fun _ -> 
	   let ph_type = read_ui32 i in
	   let offset = read_ui32 i in
	   let vaddr = read_ui32 i in
	   let paddr = read_ui32 i in
	   let filesz = read_ui32 i in
	   let memsz = read_ui32 i in
	   let ph_flags = read_ui32 i in
	   let align = read_ui32 i in
	     { ph_type = ph_type; offset = offset; vaddr = vaddr;
	       paddr = paddr; filesz = filesz; memsz = memsz;
	       ph_flags = ph_flags;
	       align = align
	     })

  let store_page fm vaddr str =
    if Int64.rem vaddr 0x1000L = 0L then
      fm#store_page_conc vaddr
	(str ^ (String.make (4096 - String.length str) '\000'))
    else
      fm#store_str vaddr 0L str

  let zero_fill fm vaddr n =
    for i = 0 to n - 1 do
      fm#store_byte_conc (Int64.add vaddr (Int64.of_int i)) 0
    done

   let load_segment fm ic phr virt_off =
    let i = IO.input_channel ic in
    let type_str = match (phr.ph_type, phr.ph_flags) with
      | (1L, 5L) -> "text"
      | (1L, 6L)
      | (1L, 7L) -> "data"
      | (2L, _) -> "DYNAMIC"
      | (3L, _) -> "INTERP"
      | (4L, _) -> "NOTE"
      | (6L, _) -> "PHDR"
      | (0x6474e550L, _) -> "EH_FRAME"
      | (0x6474e552L, _) -> "RELRO"
      | _ -> "???"
    in
    let partial = Int64.rem phr.filesz 0x1000L in
    let vbase = Int64.add phr.vaddr virt_off in
      if !opt_trace_setup then
	Printf.printf "Loading %8s segment from %08Lx to %08Lx\n"
	  type_str vbase
	  (Int64.add vbase phr.filesz);
      seek_in ic (Int64.to_int phr.offset);
      for page_num = 0 to Int64.to_int (Int64.div phr.filesz 4096L) do
	let page = IO.really_nread i 4096 and
	    va = (Int64.add vbase
		    (Int64.mul (Int64.of_int page_num) 4096L)) in
	store_page fm va page
      done;
      (if partial <> 0L then
	 let page = IO.really_nread i (Int64.to_int partial) and
	     va = (Int64.add vbase
		     (Int64.sub phr.filesz partial)) in
	   store_page fm va page);
      if phr.memsz > phr.filesz then
	(* E.g., a BSS region. Zero fill to avoid uninit-value errors. *)
	(if !opt_trace_setup then
	   Printf.printf "            Zero filling from %08Lx to %08Lx\n"
	     (Int64.add vbase phr.filesz) (Int64.add vbase phr.memsz);
	 let va = ref (Int64.add vbase phr.filesz) in
	 let first_full = (Int64.logand (Int64.add !va 4095L)
			     (Int64.lognot 4095L)) in
	 let remaining = ref (Int64.sub phr.memsz phr.filesz) in
	 let partial1 = min (Int64.sub first_full !va) !remaining in
	   zero_fill fm !va (Int64.to_int partial1);
	   va := Int64.add !va partial1;
	   remaining := Int64.sub !remaining partial1;
	   while !remaining >= 4096L do
	     store_page fm !va (String.make 4096 '\000');
	     va := Int64.add !va 4096L;
	     remaining := Int64.sub !remaining 4096L
	   done;
	   zero_fill fm !va (Int64.to_int !remaining);
	   va := Int64.add !va !remaining;
	   (* ld.so knows that it can use beyond its BSS to the end of
	      the page that it's stored on as the first part of its heap
	      without asking from an allocation from the OS. So 0-fill
	      the rest of the page too to be compatible with this. *)
	   let last_aligned = (Int64.logand (Int64.add !va 4095L)
				 (Int64.lognot 4095L)) in
	     if !opt_trace_setup then
	       Printf.printf "      Extra zero filling from %08Lx to %08Lx\n"
		 !va last_aligned;
	     let last_space = Int64.to_int (Int64.sub last_aligned !va) in
	       zero_fill fm !va last_space)
	  
   let load_partial_segment fm ic phr vbase size =
     let i = IO.input_channel ic in
     let file_base = Int64.sub vbase phr.vaddr in
       if !opt_trace_setup then
	 Printf.printf "Loading     extra region from %08Lx to %08Lx\n"
	   vbase (Int64.add vbase size);
       assert(size <= 4096L);
       seek_in ic (Int64.to_int (Int64.add phr.offset file_base));
       let data = IO.really_nread i (Int64.to_int size) in
	 store_page fm vbase data
	  
   let load_ldso fm dso vaddr =
    let ic = open_in dso in
    let dso_eh = read_elf_header ic in
      assert(dso_eh.eh_type = 3);
      List.iter
	(fun phr ->
	   if phr.ph_type = 1L || phr.memsz <> 0L then
	     load_segment fm ic phr vaddr)
	(read_program_headers ic dso_eh);
      close_in ic

  let load_dynamic_program fm fname load_base data_too extras =
    let ic = open_in fname in
    let i = IO.input_channel ic in
    let ldso = ref None in
    let eh = read_elf_header ic in
      assert(eh.eh_type = 2);
      List.iter
	(fun phr ->
	   if phr.ph_type = 1L then (* PT_LOAD *)
	     (if phr.ph_flags = 5L then assert(phr.vaddr = load_base);
	      if data_too || (phr.ph_flags <> 6L && phr.ph_flags <> 7L) then
		load_segment fm ic phr 0L)
	   else if phr.ph_type = 3L then (* PT_INTERP *)
	     (seek_in ic (Int64.to_int phr.offset);
	      let interp = IO.really_nread i (Int64.to_int phr.filesz) in
	      let ldso_base = 0xb7f00000L in
		ldso := Some (ldso_base, (load_ldso fm interp ldso_base));
		load_segment fm ic phr 0L)
	   else if phr.memsz != 0L then
	     load_segment fm ic phr 0L;
	   List.iter
	     (fun (base, size) ->
		if base >= phr.vaddr && 
		  base < (Int64.add phr.vaddr phr.memsz)
		then
		  (assert(Int64.add base size < Int64.add phr.vaddr phr.memsz);
		   load_partial_segment fm ic phr base size))
	     extras)
	(read_program_headers ic eh);
      close_in ic;
      !ldso
end

module StateLoader = struct
  let load_mem_ranges fm fname areas =
    let si = Temu_state.open_state fname in
      List.iter
	(fun (base,size) ->
	   let last = Int64.pred (Int64.add base size) in
	     List.iter
	       (fun (addr, ch) ->
		  fm#store_byte_conc addr (Char.code ch))
	       (si#get_memrange base last))
	areas;
      Temu_state.close_state si

  let load_mem_state fm fname =
    let ic = open_in fname in
    let i = IO.input_channel ic in
    let si = Temu_state.open_state fname in
      List.iter
	(fun blk ->
	   assert(Int64.logand blk#first 0xfffL = 0L);
	   assert(Int64.sub blk#last blk#first = 0xfffL);
	   LargeFile.seek_in ic blk#file_pos;
	   let page = IO.really_nread i 4096 in
	     fm#store_page_conc blk#first page)
	si#blocks;
      Temu_state.close_state si
    
end


let call_replacements fm eip =
  match eip with
      (* vx_alloc: *)
(*     | 0x0804836cL (* malloc *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "malloc") *)
(*     | 0x0804834cL (* __assert_fail *) -> *)
(* 	Some (fun () -> raise (SimulatedExit(-1L))) *)

    (* hv_fetch v1: *)
(*     | 0x08063550L (* pthread_getspecific *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "pthread_thingie") *)
(*     | 0x08063b80L (* malloc *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "malloc") *)
(*     | 0x080633b0L (* calloc *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "calloc") *)
(*     | 0x080638f0L (* strlen *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "strlen") *)
(*     | 0x080634d0L (* memset *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "memset") *)
(*     | 0x08063cd0L (* memmove *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "memmove") *)
(*     | 0x08063160L (* __errno_location *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "errno_loc") *)
(*     | 0x08063410L (* write *) -> *)
(* 	Some (fun () -> raise (SimulatedExit(-1L))) *)

    (* hv_fetch v2: *)
(*     | 0x08302d70L (* __libc_malloc *) ->  *)
(*  	Some (fun () -> fm#set_word_reg_symbolic R_EAX "malloc") *)
(*     | 0x082ee3e0L (* __assert_fail *) -> *)
(*  	Some (fun () -> raise (SimulatedExit(-1L))) *)
(*     | 0x08302a50L (* __calloc *) -> *)
(* 	Some (fun () -> fm#set_word_reg_symbolic R_EAX "calloc") *)
(*     | 0x08303180L (* __libc_realloc *) -> *)
(*  	Some (fun () -> fm#set_word_reg_symbolic R_EAX "realloc")	 *)
(*     | 0x08300610L (* __cfree *) -> *)
(*  	Some (fun () -> fm#set_word_var R_EAX 1L)	 *)

    (* bst_find *)
(*     | 0x08048304L (* __assert_fail *) -> *)
(*  	Some (fun () -> raise (SimulatedExit(-1L))) *)

    (* vmlinux *)
    | 0xc0103700L (* native_iret *) ->
  	Some (fun () -> raise (SimulatedExit(0L)))

    | _ -> None

let trans_cache = Hashtbl.create 100000 

let loop_detect = Hashtbl.create 1000

let opt_trace_eip = ref false
let opt_trace_ir = ref false
let opt_trace_orig_ir = ref false
let opt_trace_iterations = ref false
let opt_trace_stopping = ref false
let opt_coverage_stats = ref false
let opt_gc_stats = ref false
let opt_time_stats = ref false

let rec runloop fm eip asmir_gamma until =
  let load_byte addr = fm#load_byte_conc addr in
  let decode_insn eip insn_bytes =
    let asmp = Libasmir.byte_insn_to_asmp
      Libasmir.Bfd_arch_i386 eip insn_bytes in
    let sl = Asmir.asm_addr_to_vine asmir_gamma asmp eip in
      Libasmir.free_asm_program asmp;
      match sl with 
	| [V.Block(dl', sl')] -> (dl', sl')
	| _ -> failwith "expected asm_addr_to_vine to give single block"
  in
  let decode_insn_at eip =
    try
      let bytes = Array.init 16
	(fun i -> Char.chr (load_byte (Int64.add eip (Int64.of_int i))))
      in
	decode_insn eip bytes
    with
	NotConcrete(_) ->
	  Printf.printf "Jump to symbolic memory 0x%08Lx\n" eip;
	  raise IllegalInstruction
  in
  let label_to_eip s =
    let len = String.length s in
    let hex = String.sub s 3 (len - 3) in (* remove "pc_" *)
      Int64.of_string hex
  in
  let rec last l =
    match l with
      | [e] -> e
      | a :: r -> last r
      | [] -> failwith "Empty list in last"
  in
  let rec decode_insns eip k first =
    if k = 0 then ([], []) else
      let (dl, sl) = decode_insn_at eip in
	if
	  List.exists (function V.Special("int 0x80") -> true | _ -> false) sl
	then
	  (* Make a system call be alone in its basic block *)
	  if first then (dl, sl) else ([], [])
	else
	  match last (rm_unused_stmts sl) with
	    | V.Jmp(V.Name(lab)) ->
		let next_eip = label_to_eip lab in
		let (dl', sl') = decode_insns next_eip (k - 1) false in
		  (dl @ dl', sl @ sl')
	    | _ -> (dl, sl) (* end of basic block, e.g. indirect jump *)
  in
  let decode_insns_cached eip =
    try
      Hashtbl.find trans_cache eip
    with
	Not_found ->
	  let (dl, sl) = (decode_insns eip 1 true) in
	    if !opt_trace_orig_ir then
	      V.pp_program print_string (dl, sl);
	    Hashtbl.add trans_cache eip (simplify_frag (dl, sl));
	    Hashtbl.find trans_cache eip
  in
  (* Remove "unknown" statments it seems safe to ignore *)
  let remove_known_unknowns sl =
    List.filter
      (function
	 | V.Move(_, V.Unknown("CCall: x86g_create_fpucw")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_check_fldcw")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_create_mxcsr")) -> false
	 | V.Move(_, V.Unknown("CCall: x86g_check_ldmxcsr")) -> false
	 | V.Move(_, V.Unknown("Unknown: GetI")) -> false
	     (* e.g., FPU load *)
	 | V.Move(_, V.Unknown("Floating point binop")) -> false
	 | V.Move(_, V.Unknown("Floating point triop")) -> false
	 | V.Move(_, V.Unknown("floatcast")) -> false
	 | V.ExpStmt(V.Unknown("Unknown: PutI")) -> false
	     (* e.g., FPU store *)
	 | V.ExpStmt(V.Unknown("Unknown: Dirty")) -> false
	     (* XXX too broad? *)
	 | _ -> true)
      sl
  in
  let rec loop eip =
    (let old_count =
       (try
	  Hashtbl.find loop_detect eip
	with Not_found ->
	  Hashtbl.replace loop_detect eip 1;
	  1)
     in
       Hashtbl.replace loop_detect eip (1 + old_count);
       if old_count > 10 then raise TooManyIterations);
    let (dl, sl) = decode_insns_cached eip in
    let prog = (dl, (remove_known_unknowns sl)) in
      (* Libasmir.print_disasm_rawbytes Libasmir.Bfd_arch_i386 eip insn_bytes;
	 print_string "\n"; *)
      (* fm#print_x86_regs; *)
      if !opt_trace_eip then
	Printf.printf "EIP is %08Lx\n" eip;
      (* Printf.printf "EFLAGSREST is %08Lx\n" (fm#get_word_var EFLAGSREST);*)
      (* Printf.printf "Watchpoint val is %02x\n" (load_byte 0x501650e8L); *)
      (* Printf.printf ("Insn bytes are %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\n") (load_byte eip)
	 (load_byte (Int64.add eip (Int64.of_int 1)))
	 (load_byte (Int64.add eip (Int64.of_int 2)))
	 (load_byte (Int64.add eip (Int64.of_int 3)))
	 (load_byte (Int64.add eip (Int64.of_int 4)))
	 (load_byte (Int64.add eip (Int64.of_int 5)))
	 (load_byte (Int64.add eip (Int64.of_int 6)))
	 (load_byte (Int64.add eip (Int64.of_int 7)))
	 (load_byte (Int64.add eip (Int64.of_int 8)))
	 (load_byte (Int64.add eip (Int64.of_int 9)))
	 (load_byte (Int64.add eip (Int64.of_int 10)))
	 (load_byte (Int64.add eip (Int64.of_int 11)))
	 (load_byte (Int64.add eip (Int64.of_int 12)))
	 (load_byte (Int64.add eip (Int64.of_int 13)))
	 (load_byte (Int64.add eip (Int64.of_int 14)))
	 (load_byte (Int64.add eip (Int64.of_int 15))); *)
      let prog' = match call_replacements fm eip with
	| None -> prog
	| Some thunk ->
	    thunk ();
	    decode_insn eip [|'\xc3'|] (* fake "ret" *)
      in
	if !opt_trace_ir then
	  V.pp_program print_string prog';
	fm#set_frag prog';
	(* flush stdout; *)
	let new_eip = label_to_eip (fm#run ()) in
	  match (new_eip, until) with
	    | (e1, e2) when e2 e1 -> ()
	    | (0L, _) -> failwith "Jump to 0"
	    | _ -> loop new_eip
  in
    Hashtbl.clear loop_detect;
    loop eip

let random_regex maxlen =
  let len = Random.int maxlen in
  let str = String.create len in
    for i = 0 to len - 1 do
      let c = match (Random.int 25) with
	| 0 -> 'a'
	| 1 -> 'b'
	| 2 -> 'c'
	| 3 -> '|'
	| 4 -> '+'
	| 5 -> '*'
	| 6 -> '('
	| 7 -> ')'
	| 8 -> '['
	| 9 -> ']'
	| 10 -> '{'
	| 11 -> '}'
	| 12 -> ','
	| 13 -> '0'
	| 14 -> '1'
	| 15 -> '2'
	| 16 -> '4'
	| 17 -> '^'
	| 18 -> '$'
	| 19 -> '?'
	| 20 -> '.'
	| 21 -> '\\'
	| 22 -> 'w'
	| 23 -> 'd'
	| 24 -> 's'
	| _ -> failwith "random integer too big"
      in
	str.[i] <- c
    done;
    str

let check_memory_size () =
  let chan = open_in "/proc/self/status" in
    for i = 1 to 11 do ignore(input_line chan) done;
    let line = input_line chan in 
      close_in chan;
      assert((String.sub line 0 7) = "VmSize:");
      String.sub line 7 ((String.length line) - 7)

let check_memory_usage fm =
  Printf.printf "Counted size is %d\n"
    (fm#measure_size +
       (Hashtbl.fold
	  (fun k (dl, sl) s -> s + (stmt_size (V.Block(dl,sl))))
	  trans_cache 0));
  Printf.printf "/proc size is %s\n" (check_memory_size ());
  flush stdout;
  Gc.print_stat stdout

exception LastIteration

let loop_w_stats count fn =
  let iter = ref 0L and
      start_wtime = Unix.gettimeofday () and
      start_ctime = Sys.time () in
    (try
       while (match count with
		| None -> true
		| Some i -> (Int64.to_int !iter) < i)
       do
	 iter := Int64.add !iter 1L;
	 let old_wtime = Unix.gettimeofday () and
             old_ctime = Sys.time () in
	   if !opt_trace_iterations then 
	     Printf.printf "Iteration %Ld:\n" !iter;
	   fn !iter;
	   if !opt_time_stats then
	     ((let ctime = Sys.time() in
		 Printf.printf "CPU time %f sec, %f total\n"
		   (ctime -. old_ctime) (ctime -. start_ctime));
	      (let wtime = Unix.gettimeofday() in
		 Printf.printf "Wall time %f sec, %f total\n"
		   (wtime -. old_wtime) (wtime -. start_wtime)));
	   flush stdout
       done
     with
	 LastIteration -> ());
    if !opt_gc_stats then
      Gc.full_major () (* for the benefit of leak checking *)

let fuzz_sym_str start_eip end_eip buf_addr buf_len fm asmir_gamma
    =
  let eip = fm#get_word_var R_EIP
  in
    runloop fm eip asmir_gamma (fun a -> a = start_eip);
    fm#make_snap ();
    if !opt_trace_setup then Printf.printf "Took snapshot\n";
    loop_w_stats None
      (fun iter ->
	 let old_tcs = Hashtbl.length trans_cache in
	   fm#set_iter_seed (Int64.to_int iter);
	   fm#store_symbolic_cstr buf_addr buf_len;
	   (try
	      runloop fm start_eip asmir_gamma (fun a -> a = end_eip);
	    with
	      | Failure("Jump to 0") -> () (* equivalent of segfault *)
	      | SimulatedExit(_) -> ()
	      | BoringPath -> Printf.printf "\n"
	   );
	   if not fm#finish_path then raise LastIteration;
	   if !opt_coverage_stats && 
	     (Hashtbl.length trans_cache - old_tcs > 0) then
	       (* Printf.printf "Coverage increased to %d with %s on %Ld\n"
		  (Hashtbl.length trans_cache) regex iter *)
	       Printf.printf "Coverage increased to %d on %Ld\n"
		 (Hashtbl.length trans_cache) iter;
	   if !opt_gc_stats then
	     check_memory_usage fm;
	   fm#reset ()
      );
    Gc.print_stat stdout

module SRFM = SymRegionFragMachineFunctor(SymbolicDomain)
type machine = SRFM.sym_region_frag_machine

let fuzz_pcre (fm : machine) =
  fuzz_sym_str 0x08048656L 0x080486d2L 0x08063c20L 20 fm

let print_tree fm =
  let chan = open_out "fuzz.tree" in
    fm#print_tree chan;
    close_out chan

let fuzz_static start_eip end_eips fm asmir_gamma =
  fm#make_snap ();
  if !opt_trace_setup then Printf.printf "Took snapshot\n";
  loop_w_stats None
    (fun iter ->
       let old_tcs = Hashtbl.length trans_cache in
       let stop str = if !opt_trace_stopping then
	 Printf.printf "Stopping %s\n" str
       in
	 fm#set_iter_seed (Int64.to_int iter);
	 (try
	    runloop fm start_eip asmir_gamma (fun a -> List.mem a end_eips);
	  with
	    | Failure("Jump to 0") -> () (* equivalent of segfault *)
	    | SimulatedExit(_) -> ()
	    | BoringPath -> stop "on boring path"
	    | SymbolicJump -> stop "at symbolic jump"
	    | NoSyscalls -> stop "at system call"
	    | NullDereference -> stop "at null deref"
	    | TooManyIterations -> stop "after too many loop iterations"
	    | UnhandledTrap -> stop "at trap"
	    | IllegalInstruction -> stop "at bad instruction"
	    (* | NotConcrete(_) -> () (* shouldn't happen *)
	    | Simplify_failure(_) -> () (* shouldn't happen *)*)
	 ); 
	 if not fm#finish_path then raise LastIteration;
	 if !opt_coverage_stats && 
	   (Hashtbl.length trans_cache - old_tcs > 0) then
	     (* Printf.printf "Coverage increased to %d with %s on %Ld\n"
		(Hashtbl.length trans_cache) regex iter *)
	     Printf.printf "Coverage increased to %d on %Ld\n"
	       (Hashtbl.length trans_cache) iter;
	 if Int64.rem iter 50L = 0L then
	   print_tree fm;
	   if !opt_gc_stats then
	     check_memory_usage fm;
	 fm#reset ()
    );
  print_tree fm;
  if !opt_gc_stats then
    Gc.print_stat stdout

(* let fuzz_vxalloc (fm : machine) = *)
(*   fuzz_static 0x08048434L [0x080485f7L] fm *)

(* let fuzz_hv_fetch (fm : machine) = *)
(*   fuzz_static 0x08293573L [0x0829361dL] fm *)

(* let fuzz_list_find (fm : machine) = *)
(*   fuzz_static 0x08048394L [0x080483d1L] fm *)

(* let fuzz_bst_find (fm : machine) = *)
(*   fuzz_static 0x080483d4L [0x080485a8L] fm *)

let opt_load_base = ref 0x08048000L (* Linux user space default *)

let opt_fuzz_start_addr = ref None
let opt_fuzz_end_addrs = ref []
let opt_initial_eax = ref None
let opt_initial_ebx = ref None
let opt_initial_ecx = ref None
let opt_initial_edx = ref None
let opt_initial_esi = ref None
let opt_initial_edi = ref None
let opt_initial_esp = ref None
let opt_initial_ebp = ref None
let opt_initial_eflagsrest = ref None
let opt_load_extra_regions = ref []
let opt_store_words = ref []
let opt_state_file = ref None

let add_delimited_pair opt char s =
  let delim_loc = String.index s char in
  let v1 = Int64.of_string (String.sub s 0 delim_loc) in
  let v2 = Int64.of_string (String.sub s (delim_loc + 1)
			      ((String.length s) - delim_loc - 1)) in
    opt := (v1, v2) :: !opt

let main argv = 
  let fm = new SRFM.sym_region_frag_machine in
    Arg.parse
      (Arg.align [
	 ("-fuzz-start-addr", Arg.String
	    (fun s -> opt_fuzz_start_addr := Some(Int64.of_string s)),
	  "addr Code address to start fuzzing");
	 ("-fuzz-end-addr", Arg.String
	    (fun s -> opt_fuzz_end_addrs :=
	       (Int64.of_string s) :: !opt_fuzz_end_addrs),
	  "addr Code address to finish fuzzing, may be repeated");
	 ("-initial-eax", Arg.String
	    (fun s -> opt_initial_eax := Some(Int64.of_string s)),
	  "word Concrete initial value for %eax register");
	 ("-initial-ebx", Arg.String
	    (fun s -> opt_initial_ebx := Some(Int64.of_string s)),
	  "word Concrete initial value for %ebx register");
	 ("-initial-ecx", Arg.String
	    (fun s -> opt_initial_ecx := Some(Int64.of_string s)),
	  "word Concrete initial value for %ecx register");
	 ("-initial-edx", Arg.String
	    (fun s -> opt_initial_edx := Some(Int64.of_string s)),
	  "word Concrete initial value for %edx register");
	 ("-initial-esi", Arg.String
	    (fun s -> opt_initial_esi := Some(Int64.of_string s)),
	  "word Concrete initial value for %esi register");
	 ("-initial-edi", Arg.String
	    (fun s -> opt_initial_edi := Some(Int64.of_string s)),
	  "word Concrete initial value for %edi register");
	 ("-initial-esp", Arg.String
	    (fun s -> opt_initial_esp := Some(Int64.of_string s)),
	  "word Concrete initial value for %esp (stack pointer)");
	 ("-initial-ebp", Arg.String
	    (fun s -> opt_initial_ebp := Some(Int64.of_string s)),
	  "word Concrete initial value for %ebp (frame pointer)");
	 ("-initial-eflagsrest", Arg.String
	    (fun s -> opt_initial_eflagsrest := Some(Int64.of_string s)),
	  "word Concrete initial value for %eflags, less [CPAZSO]F");
	 ("-load-base", Arg.String
	    (fun s -> opt_load_base := Int64.of_string s),
	  "addr Base address for program image");
	 ("-load-region", Arg.String
	    (add_delimited_pair opt_load_extra_regions '+'),
	  "base+size Load an additional region from program image");
	 ("-state", Arg.String
	    (fun s -> opt_state_file := Some s),
	  "file Load memory state from TEMU state file");
	 ("-store-word", Arg.String
	    (add_delimited_pair opt_store_words '='),
	  "addr=val Fix an address to a concrete value");
	 ("-stp-path", Arg.Set_string(opt_stp_path),
	  "path Location of external STP binary");
 	 ("-trace-assigns", Arg.Set(opt_trace_assigns),
	  " Print satisfying assignments");
	 ("-trace-basic",
	  (Arg.Unit
	     (fun () ->
		opt_trace_binary_paths := true;
		opt_trace_decisions := true;
		opt_trace_iterations := true;
		opt_trace_setup := true;
		opt_trace_stopping := true;
		opt_trace_sym_addrs := true;
		opt_coverage_stats := true;
		opt_time_stats := true)),
	   " Enable several common trace and stats options");
 	 ("-trace-binary-paths", Arg.Set(opt_trace_binary_paths),
	  " Print decision paths as bit strings");
 	 ("-trace-decisions", Arg.Set(opt_trace_decisions),
	  " Print symbolic branch choices");
	 ("-trace-eip", Arg.Set(opt_trace_eip),
	  " Print PC of each insn executed");
	 ("-trace-ir", Arg.Set(opt_trace_ir),
	  " Print Vine IR before executing it");
	 ("-trace-orig-ir", Arg.Set(opt_trace_orig_ir),
	  " Print Vine IR as produced by Asmir");
	 ("-trace-iterations", Arg.Set(opt_trace_iterations),
	  " Print iteration count");
	 ("-trace-loads", Arg.Set(opt_trace_loads),
	  " Print each memory load");
	 ("-trace-stores", Arg.Set(opt_trace_stores),
	  " Print each memory store");
	 ("-trace-regions", Arg.Set(opt_trace_regions),
	  " Print symbolic memory regions");
	 ("-trace-setup", Arg.Set(opt_trace_setup),
	  " Print progress of program loading");
	 ("-trace-stopping", Arg.Set(opt_trace_stopping),
	  " Print why paths terminate");
	 ("-trace-sym-addrs", Arg.Set(opt_trace_sym_addrs),
	  " Print symbolic address values");
	 ("-trace-temps", Arg.Set(opt_trace_temps),
	  " Print intermediate formulas");
 	 ("-coverage-stats", Arg.Set(opt_coverage_stats),
	  " Print pseudo-BB coverage statistics");
	 ("-gc-stats", Arg.Set(opt_gc_stats),
	  " Print memory usage statistics");
	 ("-time-stats", Arg.Set(opt_gc_stats),
	  " Print running time statistics");
       ])
      (* (fun arg -> prog := Vine_parser.parse_file arg) *)
      (fun arg ->
	 ignore(LinuxLoader.load_dynamic_program fm arg !opt_load_base
		  false !opt_load_extra_regions))
      "trans_eval [options]* program\n";
    let dl = Asmir.decls_for_arch Asmir.arch_i386 in
    let asmir_gamma = Asmir.gamma_create
      (List.find (fun (i, s, t) -> s = "mem") dl) dl in
      fm#on_missing_symbol;
      fm#init_prog (dl, []);
      fm#add_special_handler
	((new linux_special_nonhandler fm) :> special_handler);
      fm#add_special_handler
	((new trap_special_nonhandler fm) :> special_handler);
      fm#make_x86_regs_symbolic;
      (match !opt_state_file with
	 | Some s -> StateLoader.load_mem_state fm s
	 | None -> ());
      (match !opt_initial_eax with
	 | Some v -> fm#set_word_var R_EAX v
	 | None -> ());
      (match !opt_initial_ebx with
	 | Some v -> fm#set_word_var R_EBX v
	 | None -> ());
      (match !opt_initial_ecx with
	 | Some v -> fm#set_word_var R_ECX v
	 | None -> ());
      (match !opt_initial_edx with
	 | Some v -> fm#set_word_var R_EDX v
	 | None -> ());
      (match !opt_initial_esi with
	 | Some v -> fm#set_word_var R_ESI v
	 | None -> ());
      (match !opt_initial_edi with
	 | Some v -> fm#set_word_var R_EDI v
	 | None -> ());
      (match !opt_initial_esp with
	 | Some v -> fm#set_word_var R_ESP v
	 | None -> ());
      (match !opt_initial_ebp with
	 | Some v -> fm#set_word_var R_EBP v
	 | None -> ());
      (match !opt_initial_eflagsrest with
	 | Some v -> fm#set_word_var EFLAGSREST v
	 | None -> ());
      List.iter (fun (addr,v) -> fm#store_word_conc addr v) !opt_store_words;
      
      let start_addr = match !opt_fuzz_start_addr with
	| None -> failwith "Missing starting address"
	| Some l -> l
      in
	
	try
	  fuzz_static start_addr !opt_fuzz_end_addrs fm asmir_gamma
	with
	  | Simplify_failure s ->
	      Printf.printf "Simplify failure <%s> at:\n" s;
	      fm#print_backtrace;
	      
      (* let eip = fm#get_word_var R_EIP in
	runloop fm eip asmir_gamma (fun _ -> true) (* run until exit *) *)
;;

main Sys.argv;;

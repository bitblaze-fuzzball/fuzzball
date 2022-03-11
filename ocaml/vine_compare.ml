module V = Vine
module VU = Vine_util

(* The functions in this module are written in a quite stylized way,
so you might wish that the OCaml compiler or some other tool could
write them for you. *)

let rec compare_list c l1 l2 =
  match (l1, l2) with
  | (a1 :: r1, a2 :: r2) ->
      (match c a1 a2 with
      | 0 -> compare_list c r1 r2
      | _ as r -> r)
  | ([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1

let compare_option c o1 o2 =
  match (o1, o2) with
  | (Some x1, Some x2) -> c x1 x2
  | (None, None) -> 0
  | (None, _) -> -1
  | (_, None) -> 1

let compare_endian e1 e2 =
  match (e1, e2) with
  | (V.Little, V.Little) -> 0
  | (V.Little, V.Big) -> -1
  | (V.Big, V.Little) -> 1
  | (V.Big, V.Big) -> 0

let rec compare_typ t1 t2 =
  match (t1, t2) with
  | (V.REG_1, V.REG_1) -> 0
  | (V.REG_1, _) -> -1
  | (_, V.REG_1) -> 1
  | (V.REG_8, V.REG_8) -> 0
  | (V.REG_8, _) -> -1
  | (_, V.REG_8) -> 1
  | (V.REG_16, V.REG_16) -> 0
  | (V.REG_16, _) -> -1
  | (_, V.REG_16) -> 1
  | (V.REG_32, V.REG_32) -> 0
  | (V.REG_32, _) -> -1
  | (_, V.REG_32) -> 1
  | (V.REG_64, V.REG_64) -> 0
  | (V.REG_64, _) -> -1
  | (_, V.REG_64) -> 1
  | (V.TString, V.TString) -> 0
  | (V.TString, _) -> -1
  | (_, V.TString) -> 1
  | (V.TMem(ty1, e1), V.TMem(ty2, e2)) ->
      (match compare_typ ty1 ty2 with
      | 0 -> compare_endian e1 e2
      | _ as r -> r)
  | (V.TMem(_), _) -> -1
  | (_, V.TMem(_)) -> 1
  | (V.TFun(tyo1, tyl1, b1), V.TFun(tyo2, tyl2, b2)) ->
      (match compare_option compare_typ tyo1 tyo2 with
      | 0 ->
	  (match compare_list compare_typ tyl1 tyl2 with
	  | 0 -> Bool.compare b1 b2
	  | _ as r -> r)
      | _ as r -> r)
  | (V.TFun(_), _) -> -1
  | (_, V.TFun(_)) -> 1
  | (V.Array(ty1, i1), V.Array(ty2, i2)) ->
      (match compare_typ ty1 ty2 with
      | 0 -> Int64.compare i1 i2
      | _ as r -> r)
  | (V.Array(_), _) -> -1
  | (_, V.Array(_)) -> 1
  | (V.TAttr(ty1, attrl1), V.TAttr(ty2, attrl2)) ->
      (match compare_typ ty1 ty2 with
      | 0 -> compare_list String.compare attrl1 attrl2
      | _ as r -> r)

let compare_var (n1, s1, ty1) (n2, s2, ty2) =
  match Int.compare n1 n2 with
  | 0 ->
      (match String.compare s1 s2 with
      | 0 -> compare_typ ty1 ty2
      | _ as r -> r)
  | _ as r -> r

let compare_cast_type ct1 ct2 =
  match (ct1, ct2) with
  | (V.CAST_UNSIGNED, V.CAST_UNSIGNED) -> 0
  | (V.CAST_UNSIGNED, _) -> -1
  | (_, V.CAST_UNSIGNED) -> 1
  | (V.CAST_SIGNED, V.CAST_SIGNED) -> 0
  | (V.CAST_SIGNED, _) -> -1
  | (_, V.CAST_SIGNED) -> 1
  | (V.CAST_HIGH, V.CAST_HIGH) -> 0
  | (V.CAST_HIGH, _) -> -1
  | (_, V.CAST_HIGH) -> 1
  | (V.CAST_LOW, V.CAST_LOW) -> 0

let compare_fcast_type fct1 fct2 =
  match (fct1, fct2) with
  | (V.CAST_SFLOAT, V.CAST_SFLOAT) -> 0
  | (V.CAST_SFLOAT, _) -> -1
  | (_, V.CAST_SFLOAT) -> 1
  | (V.CAST_UFLOAT, V.CAST_UFLOAT) -> 0
  | (V.CAST_UFLOAT, _) -> -1
  | (_, V.CAST_UFLOAT) -> 1
  | (V.CAST_SFIX, V.CAST_SFIX) -> 0
  | (V.CAST_SFIX, _) -> -1
  | (_, V.CAST_SFIX) -> 1
  | (V.CAST_UFIX, V.CAST_UFIX) -> 0
  | (V.CAST_UFIX, _) -> -1
  | (_, V.CAST_UFIX) -> 1
  | (V.CAST_FWIDEN, V.CAST_FWIDEN) -> 0
  | (V.CAST_FWIDEN, _) -> -1
  | (_, V.CAST_FWIDEN) -> 1
  | (V.CAST_FNARROW, V.CAST_FNARROW) -> 0

let compare_binop_type bt1 bt2 =
  match (bt1, bt2) with
  | (V.PLUS, V.PLUS) -> 0
  | (V.PLUS, _) -> -1
  | (_, V.PLUS) -> 1
  | (V.MINUS, V.MINUS) -> 0
  | (V.MINUS, _) -> -1
  | (_, V.MINUS) -> 1
  | (V.TIMES, V.TIMES) -> 0
  | (V.TIMES, _) -> -1
  | (_, V.TIMES) -> 1
  | (V.DIVIDE, V.DIVIDE) -> 0
  | (V.DIVIDE, _) -> -1
  | (_, V.DIVIDE) -> 1
  | (V.SDIVIDE, V.SDIVIDE) -> 0
  | (V.SDIVIDE, _) -> -1
  | (_, V.SDIVIDE) -> 1
  | (V.MOD, V.MOD) -> 0
  | (V.MOD, _) -> -1
  | (_, V.MOD) -> 1
  | (V.SMOD, V.SMOD) -> 0
  | (V.SMOD, _) -> -1
  | (_, V.SMOD) -> 1
  | (V.LSHIFT, V.LSHIFT) -> 0
  | (V.LSHIFT, _) -> -1
  | (_, V.LSHIFT) -> 1
  | (V.RSHIFT, V.RSHIFT) -> 0
  | (V.RSHIFT, _) -> -1
  | (_, V.RSHIFT) -> 1
  | (V.ARSHIFT, V.ARSHIFT) -> 0
  | (V.ARSHIFT, _) -> -1
  | (_, V.ARSHIFT) -> 1
  | (V.BITAND, V.BITAND) -> 0
  | (V.BITAND, _) -> -1
  | (_, V.BITAND) -> 1
  | (V.BITOR, V.BITOR) -> 0
  | (V.BITOR, _) -> -1
  | (_, V.BITOR) -> 1
  | (V.XOR, V.XOR) -> 0
  | (V.XOR, _) -> -1
  | (_, V.XOR) -> 1
  | (V.EQ, V.EQ) -> 0
  | (V.EQ, _) -> -1
  | (_, V.EQ) -> 1
  | (V.NEQ, V.NEQ) -> 0
  | (V.NEQ, _) -> -1
  | (_, V.NEQ) -> 1
  | (V.LT, V.LT) -> 0
  | (V.LT, _) -> -1
  | (_, V.LT) -> 1
  | (V.LE, V.LE) -> 0
  | (V.LE, _) -> -1
  | (_, V.LE) -> 1
  | (V.SLT, V.SLT) -> 0
  | (V.SLT, _) -> -1
  | (_, V.SLT) -> 1
  | (V.SLE, V.SLE) -> 0
  | (V.SLE, _) -> -1
  | (_, V.SLE) -> 1
  | (V.CONCAT, V.CONCAT) -> 0

let compare_fbinop_type ft1 ft2 =
  match (ft1, ft2) with
  | (V.FPLUS, V.FPLUS) -> 0
  | (V.FPLUS, _) -> -1
  | (_, V.FPLUS) -> 1
  | (V.FMINUS, V.FMINUS) -> 0
  | (V.FMINUS, _) -> -1
  | (_, V.FMINUS) -> 1
  | (V.FTIMES, V.FTIMES) -> 0
  | (V.FTIMES, _) -> -1
  | (_, V.FTIMES) -> 1
  | (V.FDIVIDE, V.FDIVIDE) -> 0
  | (V.FDIVIDE, _) -> -1
  | (_, V.FDIVIDE) -> 1
  | (V.FEQ, V.FEQ) -> 0
  | (V.FEQ, _) -> -1
  | (_, V.FEQ) -> 1
  | (V.FNEQ, V.FNEQ) -> 0
  | (V.FNEQ, _) -> -1
  | (_, V.FNEQ) -> 1
  | (V.FLT, V.FLT) -> 0
  | (V.FLT, _) -> -1
  | (_, V.FLT) -> 1
  | (V.FLE, V.FLE) -> 0

let compare_unop_type ut1 ut2 =
  match (ut1, ut2) with
  | (V.NEG, V.NEG) -> 0
  | (V.NEG, V.NOT) -> -1
  | (V.NOT, V.NEG) -> 1
  | (V.NOT, V.NOT) -> 0

let compare_funop_type ft1 ft2 =
  match (ft1, ft2) with
  | (V.FNEG, V.FNEG) -> 0

let compare_round_mode rm1 rm2 =
  match (rm1, rm2) with
  | (VU.ROUND_NEAREST, VU.ROUND_NEAREST) -> 0
  | (VU.ROUND_NEAREST, _) -> -1
  | (_, VU.ROUND_NEAREST) -> 1
  | (VU.ROUND_NEAREST_AWAY_ZERO, VU.ROUND_NEAREST_AWAY_ZERO) -> 0
  | (VU.ROUND_NEAREST_AWAY_ZERO, _) -> -1
  | (_, VU.ROUND_NEAREST_AWAY_ZERO) -> 1
  | (VU.ROUND_POSITIVE, VU.ROUND_POSITIVE) -> 0
  | (VU.ROUND_POSITIVE, _) -> -1
  | (_, VU.ROUND_POSITIVE) -> 1
  | (VU.ROUND_NEGATIVE, VU.ROUND_NEGATIVE) -> 0
  | (VU.ROUND_NEGATIVE, _) -> -1
  | (_, VU.ROUND_NEGATIVE) -> 1
  | (VU.ROUND_ZERO, VU.ROUND_ZERO) -> 0

let compare_value v1 v2 =
  match (v1, v2) with
  | (V.Int(ty1, i1), V.Int(ty2, i2)) ->
      (match compare_typ ty1 ty2 with
      | 0 -> Int64.compare i1 i2
      | _ as r -> r)
  | (V.Int(_), _) -> -1
  | (_, V.Int(_)) -> 1
  | (V.Str(s1), V.Str(s2)) ->
      String.compare s1 s2

let compare_attribute at1 at2 =
  match (at1, at2) with
  | (V.Pos(s1, i1), V.Pos(s2, i2)) ->
      (match String.compare s1 s2 with
      | 0 -> Int.compare i1 i2
      | _ as r -> r)
  | (V.Pos(_), _) -> -1
  | (_, V.Pos(_)) -> 1
  | (V.ACall, V.ACall) -> 0
  | (V.ACall, _) -> -1
  | (_, V.ACall) -> 1
  | (V.AReturn, V.AReturn) -> 0

let rec compare_lvalue lv1 lv2 =
  match (lv1, lv2) with
  | (V.Temp(v1), V.Temp(v2)) -> compare_var v1 v2
  | (V.Temp(_), _) -> -1
  | (_, V.Temp(_)) -> 1
  | (V.Mem(v1, e1, ty1), V.Mem(v2, e2, ty2)) ->
      (match compare_var v1 v2 with
      | 0 ->
	  (match compare_exp e1 e2 with
	  | 0 -> compare_typ ty1 ty2
	  | _ as r -> r)
      | _ as r -> r)
and compare_exp e1 e2 =
  match (e1, e2) with
  | (V.BinOp(bt1, el1, er1), V.BinOp(bt2, el2, er2)) ->
      (match compare_binop_type bt1 bt2 with
      | 0 ->
	  (match compare_exp el1 el2 with
	  | 0 -> compare_exp er1 er2
	  | _ as r -> r)
      | _ as r -> r)
  | (V.BinOp(_), _) -> -1
  | (_, V.BinOp(_)) -> 1
  | (V.FBinOp(ft1, rm1, el1, er1), V.FBinOp(ft2, rm2, el2, er2)) ->
      (match compare_fbinop_type ft1 ft2 with
      | 0 ->
	  (match compare_round_mode rm1 rm2 with
	  | 0 ->
	      (match compare_exp el1 el2 with
	      | 0 -> compare_exp er1 er2
	      | _ as r -> r)
	  | _ as r -> r)
      | _ as r -> r)
  | (V.FBinOp(_), _) -> -1
  | (_, V.FBinOp(_)) -> 1
  | (V.UnOp(ut1, se1), V.UnOp(ut2, se2)) ->
      (match compare_unop_type ut1 ut2 with
      | 0 -> compare_exp se1 se2
      | _ as r -> r)
  | (V.UnOp(_), _) -> -1
  | (_, V.UnOp(_)) -> 1
  | (V.FUnOp(ft1, rm1, se1), V.FUnOp(ft2, rm2, se2)) ->
      (match compare_funop_type ft1 ft2 with
      | 0 ->
	  (match compare_round_mode rm1 rm2 with
	  | 0 -> compare_exp se1 se2
	  | _ as r -> r)
      | _ as r -> r)
  | (V.FUnOp(_), _) -> -1
  | (_, V.FUnOp(_)) -> 1
  | (V.Constant(v1), V.Constant(v2)) -> compare_value v1 v2
  | (V.Constant(_), _) -> -1
  | (_, V.Constant(_)) -> 1
  | (V.Lval(lv1), V.Lval(lv2)) -> compare_lvalue lv1 lv2
  | (V.Lval(_), _) -> -1
  | (_, V.Lval(_)) -> 1
  | (V.Name(lb1), V.Name(lb2)) -> String.compare lb1 lb2
  | (V.Name(_), _) -> -1
  | (_, V.Name(_)) -> 1
  | (V.Cast(ct1, ty1, se1), V.Cast(ct2, ty2, se2)) ->
      (match compare_cast_type ct1 ct2 with
      | 0 ->
	  (match compare_typ ty1 ty2 with
	  | 0 -> compare_exp se1 se2
	  | _ as r -> r)
      | _ as r -> r)
  | (V.Cast(_), _) -> -1
  | (_, V.Cast(_)) -> 1
  | (V.FCast(fct1, rm1, ty1, se1), V.FCast(fct2, rm2, ty2, se2)) ->
      (match compare_fcast_type fct1 fct2 with
      | 0 ->
	  (match compare_round_mode rm1 rm2 with
	  | 0 ->
	      (match compare_typ ty1 ty2 with
	      | 0 -> compare_exp se1 se2
	      | _ as r -> r)
	  | _ as r -> r)
      | _ as r -> r)
  | (V.FCast(_), _) -> -1
  | (_, V.FCast(_)) -> 1
  | (V.Unknown(s1), V.Unknown(s2)) -> String.compare s1 s2
  | (V.Unknown(_), _) -> -1
  | (_, V.Unknown(_)) -> 1
  | (V.Let(lv1, ve1, be1), V.Let(lv2, ve2, be2)) ->
      (match compare_lvalue lv1 lv2 with
      | 0 ->
	  (match compare_exp ve1 ve2 with
	  | 0 -> compare_exp be1 be2
	  | _ as r -> r)
      | _ as r -> r)
  | (V.Let(_), _) -> -1
  | (_, V.Let(_)) -> 1
  | (V.Ite(ce1, te1, fe1), V.Ite(ce2, te2, fe2)) ->
      (match compare_exp ce1 ce2 with
      | 0 ->
	  (match compare_exp te1 te2 with
	  | 0 -> compare_exp fe1 fe2
	  | _ as r -> r)
      | _ as r -> r)

#include "const_simplify_exp.h"



Exp *add_constant(Constant *c1, Constant *c2, reg_t ret_type)
{

  uint64_t u64;
  u64 = c1->val + c2->val;
  u64 = Exp::cast_value(ret_type, u64);
  return new Constant(ret_type, u64);
}

Exp *mul_constant(Constant *c1, Constant *c2, reg_t ret_type)
{
  uint64_t u64;
  u64 = c1->val * c2->val;
  u64 = Exp::cast_value(ret_type, u64);
  return new Constant(ret_type, u64);
}

Exp *sub_constant(Constant *c1, Constant *c2, reg_t ret_type)
{
  uint64_t u64;
  u64 = c1->val - c2->val;
  u64 = Exp::cast_value(ret_type, u64);
  return new Constant(ret_type, u64);
}


bool R1R3R5(Exp *&e)
{
  if(e->exp_type != BINOP){
    return false;
  }
  BinOp *b = (BinOp *) e;

  if(b->lhs->exp_type != CONSTANT ||
     b->rhs->exp_type != CONSTANT){
    return false;
  }
  Exp *ret;
  Constant *c1 = (Constant *) b->lhs;
  Constant *c2 = (Constant *) b->rhs;

  reg_t ret_type;
  ret_type = ( (Exp::reg_to_bits(c1->typ) > Exp::reg_to_bits(c2->typ)) ? c1->typ : c2->typ);

  switch(b->binop_type){
  case PLUS:
    print_debug("canon", "   ++ applying R1 to %s", e->tostring().c_str());
    ret = add_constant(c1, c2, ret_type);
    if(ret == NULL) return false;
    break;
  case TIMES:
    print_debug("canon", "   ++ applying R3 to %s", e->tostring().c_str());
    ret = mul_constant(c1, c2,ret_type);
    if(ret == NULL) return false;
    break;
  case MINUS:
    print_debug("canon", "   ++ applying R5 to %s", e->tostring().c_str());
    ret = sub_constant(c1, c2,ret_type);
    if(ret == NULL) return false;
    break;
  default:
    e = b;
    return false;
    break;
  }

  e = ret;
  print_debug("canon", "   -- applied R{1,3,5} to %s", ret->tostring().c_str());    
  return true;
}

bool R2R4R6(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->lhs->exp_type != CONSTANT &&
     b->rhs->exp_type == CONSTANT){
    Constant *c = (Constant *) b->rhs;

    switch(b->binop_type){
    case PLUS: // R2
      print_debug("canon", "   ++ applying R2 to %s", e->tostring().c_str());
      b->rhs = b->lhs;
      b->lhs = c;
      break;
    case TIMES: //R4 
      print_debug("canon", "   ++ applying R4 to %s", e->tostring().c_str());
      b->rhs = b->lhs;
      b->lhs = c;
      break;
    case MINUS: //R6
      // Switch, but need to add minus sign to lhs of tree.
      print_debug("canon", "   ++ applying R6 to %s", e->tostring().c_str());
      b->binop_type = PLUS;
      b->rhs = b->lhs;
      print_debug("warning", "Switching from unsigned to signed"
		  " during constant folding");
      c->val = 0-(c->val);
      break;
    default:
      e = b;
      return false;
      break;
    }

    e = b;
    print_debug("canon", "   -- applied R{2,4,6} to %s", b->tostring().c_str());    
    return true;      
  } 
  e = b;
  return false;
}

bool R7(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;
  if(b->binop_type != PLUS)
    return false;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != PLUS)
    return false;
  print_debug("canon", "   ++ applying R7 to %s", e->tostring().c_str());
  Exp *t1 = b->lhs;
  Exp *t2 = rhs->lhs;
  Exp *t3 = rhs->rhs;
  rhs->lhs = t1;
  rhs->rhs = t2;
  b->lhs = rhs;
  b->rhs = t3;
  print_debug("canon", "   -- applied R7 to %s", b->tostring().c_str());
  e = b;
  return true;
}

bool R8(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;
  if(b->binop_type != TIMES)
    return false;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != TIMES)
    return false;
  print_debug("canon", "   ++ applying R8 to %s", e->tostring().c_str());
  Exp *t1 = b->lhs;
  Exp *t2 = rhs->lhs;
  Exp *t3 = rhs->rhs;
  rhs->lhs = t1;
  rhs->rhs = t2;
  b->lhs = rhs;
  b->rhs = t3;
  print_debug("canon", "   -- applied R8 to %s", b->tostring().c_str());
  e = b;
  return true;
}



bool R9(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != PLUS)
    return false;
  if(b->lhs->exp_type != BINOP)
    return false;
  if(b->rhs->exp_type != CONSTANT)
    return false;
  BinOp *lhs = (BinOp *) lhs;
  if(lhs->binop_type != PLUS)
    return false;
  if(lhs->lhs->exp_type != CONSTANT)
    return false;
  Constant *c2 = (Constant *) b->rhs;
  Constant *c1= (Constant *) lhs->lhs;

  reg_t ret_type;
  ret_type = ( (Exp::reg_to_bits(c1->typ) > Exp::reg_to_bits(c2->typ)) ? c1->typ : c2->typ);


  print_debug("canon", "   ++ applying R9 to %s", e->tostring().c_str());    
  Exp *ret = add_constant(c2, c1, ret_type);
  if(ret == NULL ) return false;
  Exp *tmp = lhs->rhs;
  b->lhs = ret;
  b->rhs = tmp;
  Exp::destroy(c1);
  Exp::destroy(c2);
  print_debug("canon", "   -- applied R9 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R10(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != BINOP)
    return false;
  if(b->rhs->exp_type != CONSTANT)
    return false;
  BinOp *lhs = (BinOp *) lhs;
  if(lhs->binop_type != TIMES)
    return false;
  if(lhs->lhs->exp_type != CONSTANT)
    return false;
  Constant *c2 = (Constant *) b->rhs;
  Constant *c1= (Constant *) lhs->lhs;

  reg_t ret_type;

  ret_type = ( (Exp::reg_to_bits(c1->typ) > Exp::reg_to_bits(c2->typ))
  ? c1->typ : c2->typ); 


  print_debug("canon", "   ++ applying R10 to %s", e->tostring().c_str());    
  Exp *ret = mul_constant(c2, c1, ret_type);
  if(ret== NULL) return false;
  Exp *tmp = lhs->rhs;
  b->lhs = ret;
  b->rhs = tmp;
  Exp::destroy(c1);
  Exp::destroy(c2);
  print_debug("canon", "   -- applied R10 to %s", b->tostring().c_str());    
  e = b;
  return true;
}


bool R11(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != BINOP)
    return false;
  if(b->rhs->exp_type != CONSTANT)
    return false;
  BinOp *lhs = (BinOp *) lhs;
  if(lhs->binop_type != PLUS)
    return false;
  if(lhs->lhs->exp_type != CONSTANT)
    return false;
  Constant *c2 = (Constant *) b->rhs;
  Constant *c1= (Constant *) lhs->lhs;

  reg_t ret_type;
  ret_type = ( (Exp::reg_to_bits(c1->typ) > Exp::reg_to_bits(c2->typ)) ? c1->typ : c2->typ);


  Exp *ret = mul_constant(c2, c1,ret_type);
  if(ret == NULL) return false;
  print_debug("canon", "   ++ applying R11 to %s", e->tostring().c_str());    
  b->binop_type = PLUS;
  b->lhs = ret;
  b->rhs = new BinOp(TIMES, c2, lhs->rhs);

  Exp::destroy(c1);
  print_debug("canon", "   -- applied R11 to %s", b->tostring().c_str());    
  e = b;
  return true;
}


bool R12(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != CONSTANT)
    return false;
  Constant *c1 = (Constant *) b->lhs;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != PLUS)
    return false;
  if(rhs->lhs->exp_type != CONSTANT)
    return false;
  Constant *c2 = (Constant *) rhs->lhs;

  reg_t ret_type;
  ret_type = ( (Exp::reg_to_bits(c1->typ) > Exp::reg_to_bits(c2->typ)) ? c1->typ : c2->typ);


  Exp *ret =  mul_constant(c1, c2,ret_type);
  if(ret == NULL) return false;
  print_debug("canon", "   ++ applying R12 to %s", e->tostring().c_str());    
  b->lhs = ret;
  rhs->binop_type = TIMES;
  b->binop_type = PLUS;
  rhs->lhs = c1;
  Exp::destroy(c2);
  print_debug("canon", "   -- applied R12 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R13(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != BINOP)
    return false;
  BinOp *lhs = (BinOp *) b->lhs;
  if(lhs->binop_type != PLUS)
    return false;
  if(b->rhs->exp_type != CONSTANT)
    return false;
  Constant *c = (Constant *) b->rhs;
  print_debug("canon", "   ++ applying R13 to %s", e->tostring().c_str());    
  Exp *t1 = lhs->lhs;
  Exp *t2 = lhs->rhs;
  b->binop_type = PLUS;
  lhs->binop_type = TIMES;
  lhs->lhs = c;
  lhs->rhs = t1;
  b->rhs = new BinOp(TIMES, c->clone(), t2);
  print_debug("canon", "   -- applied R13 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R14(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != CONSTANT)
    return false;
  Constant *c = (Constant *) b->lhs;
  //  if(c->typ.kind == REG_FLOAT) return b;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != PLUS)
    return false;
  print_debug("canon", "   ++ applying R14 to %s", e->tostring().c_str());    
  Exp *t1 = rhs->lhs;
  Exp *t2 = rhs->rhs;
  b->binop_type = PLUS;
  b->lhs = new BinOp(TIMES, c, t1);
  rhs->binop_type = TIMES;
  rhs->lhs = c->clone();
  rhs->rhs = t2;
  print_debug("canon", "   -- applied R14 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R15(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != BINOP)
    return false;
  BinOp *lhs = (BinOp *) b->lhs;
  if(lhs->binop_type != MINUS)
    return false;
  Exp *t1 = lhs->lhs;
  Exp *t2 = lhs->rhs;
  if(b->rhs->exp_type != CONSTANT)
    return false;

  Constant *c = (Constant *) b->rhs;
  //  if(c->typ.kind == REG_FLOAT) return b;
  print_debug("canon", "   ++ applying R15 to %s", e->tostring().c_str());    
  lhs->binop_type = TIMES;
  b->binop_type = MINUS;
  lhs->lhs = c;
  lhs->rhs = t1;

  b->rhs = new BinOp(TIMES, c->clone(), t2);
  print_debug("canon", "   -- applied R15 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R16(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != CONSTANT)
    return false;
  Constant *c = (Constant *) b->lhs;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != MINUS)
    return false;
  Exp *t1 = rhs->lhs;
  Exp *t2 = rhs->rhs;
  print_debug("canon", "   ++ applying R16 to %s", e->tostring().c_str());    
  b->binop_type = MINUS;
  b->lhs = new BinOp(TIMES, c, t1);
  rhs->binop_type = TIMES;
  rhs->rhs = c->clone();
  rhs->lhs = t2;
  print_debug("canon", "   -- applied R16 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R17(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->lhs->exp_type != BINOP)
    return false;
  BinOp *lhs = (BinOp *) b->lhs;
  if(lhs->binop_type != PLUS)
    return false;
  Exp *t1 = lhs->lhs;
  Exp *t2 = lhs->rhs;
  Exp *t3 = b->rhs;
  print_debug("canon", "   ++ applying R17 to %s", e->tostring().c_str());    
  b->binop_type = PLUS;
  lhs->binop_type = TIMES;
  lhs->lhs = t1;
  lhs->rhs = t3;
  b->rhs = new BinOp(TIMES, t2, t3->clone());
  print_debug("canon", "   -- applied R17 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R18(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != PLUS)
    return false;
  Exp *t1 = b->lhs;
  Exp *t2 = rhs->lhs;
  Exp *t3 = rhs->rhs;
  print_debug("canon", "   ++ applying R18 to %s", e->tostring().c_str());    
  b->binop_type = PLUS;
  b->lhs = new BinOp(TIMES, t1, t2);
  rhs->binop_type = TIMES;
  rhs->lhs = t1->clone();
  rhs->rhs = t3;
  print_debug("canon", "   -- applied R18 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R19(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  Exp *t3 = b->rhs;
  if(b->lhs->exp_type != BINOP)
    return false;
  BinOp *lhs = (BinOp *) b->lhs;
  if(lhs->binop_type != MINUS)
    return false;
  print_debug("canon", "   ++ applying R19 to %s", e->tostring().c_str());    
  Exp *t1 = lhs->lhs;
  Exp *t2 = lhs->rhs;
  b->binop_type = MINUS;
  lhs->binop_type = TIMES;
  lhs->lhs = t1;
  lhs->rhs = t3;
  b->rhs = new BinOp(TIMES, t2, t3->clone());
  print_debug("canon", "   -- applied R19 to %s", b->tostring().c_str());    
  e = b;
  return true;
}

bool R20(Exp *&e)
{
  if(e->exp_type != BINOP)
    return false;
  BinOp *b = (BinOp *) e;

  if(b->binop_type != TIMES)
    return false;
  Exp *t1 = b->lhs;
  if(b->rhs->exp_type != BINOP)
    return false;
  BinOp *rhs = (BinOp *) b->rhs;
  if(rhs->binop_type != MINUS)
    return false;
  print_debug("canon", "   ++ applying R20 to %s", e->tostring().c_str());    
  Exp *t2 = rhs->lhs;
  Exp *t3 = rhs->rhs;
  b->binop_type = MINUS;
  b->lhs = new BinOp(TIMES, t1, t2);
  rhs->binop_type = TIMES;
  rhs->lhs = t1->clone();
  rhs->rhs = t3;
  print_debug("canon", "   -- applied R20 to %s", b->tostring().c_str());    
  e =b ;
  return true;
}

ConstantSimplifyExp::ConstantSimplifyExp()
{
  change = false;
}

ConstantSimplifyExp::~ConstantSimplifyExp()
{
}

Exp * 
ConstantSimplifyExp::simplify(Exp *exp)
{
  if (simplified.find(exp) != simplified.end())
    return exp;

  bool outer_change = change;
  do {
    change = false;
    Exp *o = exp;
    exp->accept(this);
    if(ret != o){
      Exp::destroy(o);
    }
    exp = ret;
    outer_change = (outer_change || change);
  }    while(change);
  change = outer_change;

  simplified.insert(ret);
  return ret;
}

void
ConstantSimplifyExp::visitBinOp(BinOp *b)
{
  b->lhs = simplify(b->lhs);
  b->rhs = simplify(b->rhs);

//   Exp *o;
//   o = b->rhs;
//   ret = NULL;
//   b->rhs->accept(this);
//   if(ret != o){
//     Exp::destroy(o);
//   }
//   b->rhs = ret;

//   o = b->lhs;
//   ret = NULL;
//   b->lhs->accept(this);
//   if(ret != o){
//     Exp::destroy(o);
//   }
//   b->lhs = ret;

  ret = b;
  //  if(change) return;
  change = change || R1R3R5(ret);
  //  if(change) return;

  change = change || R2R4R6(ret);
  //  if(change) return;
  change = change || R7(ret);
  //  if(change) return;
  change = change || R8(ret);
  //  if(change) return;
  change = change || R9(ret);
  //  if(change) return;
  change = change || R10(ret);
  //  if(change) return;
  change = change || R11(ret);
  //  if(change) return;
  change = change || R12(ret);
  //  if(change) return;
  change = change || R13(ret);
  //  if(change) return;
  change = change || R14(ret);
  //  if(change) return;
  change = change || R15(ret);
  //  if(change) return;
  change = change || R16(ret);
  //  if(change) return;
  change = change || R17(ret);
  //  if(change) return;
  change = change || R18(ret);
  //  if(change) return;
  change = change || R19(ret);
  //  if(change) return;
  change = change || R20(ret);
  return;
}


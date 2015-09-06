#include "exp.h"
#include "stmt.h"
#include <iostream>
#include <map>
#include <cassert>

using namespace std;

// binopnames and strs used to be statically constructed arrays of
// std::strings, but Valgrind claimed that that caused a memory leak
// in the string constructor. I couldn't tell whether that was a
// real error or not, but I was able to make it go away (and presumably
// save a trivial amount of time at startup) by constructing the
// std::strings only on request. -SMcC

static const char *binopnames_strs[] = {
  "PLUS",
  "MINUS",
  "TIMES",
  "DIVIDE",
  "MOD",
  "LSHIFT",
  "RSHIFT",
  "ARSHIFT",
  "LROTATE",
  "RROTATE",
  "LOGICAND",
  "LOGICOR",
  "BITAND",
  "BITOR",
  "XOR",
  "EQ",
  "NEQ",
  "GT",
  "LT",
  "GE",
  "LE",
  "SDIVIDE",
  "SMOD",
  "SLT",
  "SLE",
};

static const char *strs_strs[] = {
  "+",
  "-",
  "*",
  "/",
  "%",
  "<<",
  ">>",
  "@>>",
  "<<",
  ">>",
  "&&",
  "||",
  "&",
  "|",
  "^",
  "==",
  "<>",
  ">",
  "<",
  ">=",
  "<=",
  "/$",
  "%$",
  "<$",
  "<=$",
};

uint64_t
Exp::cast_value(reg_t t, uint64_t v)
{
  // Makes debugging easier actually assigning to correct type.
  uint64_t u64;
  uint32_t u32;
  uint16_t u16;
  uint8_t u8;
  switch(t){
  case REG_1: if(v == 0) return 0; else return 1; break;
  case REG_8: u8 = (uint8_t) v; return u8; break;
  case REG_16: u16 = (uint16_t) v; return u16; break;
  case REG_32: u32 = (uint32_t) v; return u32; break;
  case REG_64: u64 = (uint64_t) v; return u64; break;
  }
  assert(0); // eliminates warnings.
}

reg_t reg_address_t;

uint32_t 
Exp::reg_to_bits(const reg_t &reg)
{
  switch(reg)
    {
    case REG_1: return 1; break;
    case REG_8: return 8; break;
    case REG_16: return 16; break;
    case REG_32: return 32; break;
    case REG_64: return 64; break;
    }
  assert(0); // eliminates warnings.
}


Exp*
Exp::clone(Exp* copy)
{
  return copy->clone();
}


void Exp::destroy( Exp *expr )
{
    switch ( expr->exp_type )
    {
    case BINOP:     BinOp::destroy((BinOp *)expr);          break;
    case FBINOP:    FBinOp::destroy((FBinOp *)expr);        break;
    case UNOP:      UnOp::destroy((UnOp *)expr);            break;
    case FUNOP:     FUnOp::destroy((FUnOp *)expr);          break;
    case CONSTANT:  Constant::destroy((Constant *)expr);    break;
    case MEM:       Mem::destroy((Mem *)expr);              break;
    case TEMP:      Temp::destroy((Temp *)expr);            break;
    case PHI:       Phi::destroy((Phi *)expr);              break;
    case UNKNOWN:   Unknown::destroy((Unknown *)expr);      break;
    case CAST:      Cast::destroy((Cast *)expr);            break;
    case FCAST:     FCast::destroy((FCast *)expr);          break;
    case VECTOR:    Vector::destroy((Vector *)expr);        break;
    case NAME:      Name::destroy((Name *)expr);            break;
    case LET:       Let::destroy((Let *)expr);              break; 
    case ITE:       Ite::destroy((Ite *)expr);              break;
    case EXTENSION:   
      // Fixme: need to make a destroy virtual function that all
      // exp have. 
      print_debug("warning", "Memory lost on unhandled destroy"); break;
    }

}
string
Exp::string_type(const reg_t &typ)
{
  //  return string_type(typ.kind, typ.width);
  string s;
  switch(typ)
    {
    case REG_1: s= "reg1_t"; break;
    case REG_8: s = "reg8_t"; break;
    case REG_16: s = "reg16_t"; break;
    case REG_32: s = "reg32_t"; break;
    case REG_64: s = "reg64_t"; break;
    }
  return s;
}


BinOp::BinOp(binop_type_t t, Exp *l, Exp *r) 
  : Exp(BINOP), lhs(l), rhs(r), binop_type(t)
{ 

}

BinOp::BinOp(const BinOp& copy)
  : Exp(BINOP), binop_type(copy.binop_type)
{
  lhs = copy.lhs->clone();
  rhs = copy.rhs->clone();
}

BinOp *
BinOp::clone() const
{
  return new BinOp(*this);
}

string
BinOp::tostring() const
{
  string ret;
  ret =  lhs->tostring() + optype_to_string(binop_type) + rhs->tostring();
  ret = "(" + ret + ")";
  return ret;
}

string
BinOp::optype_to_string(const binop_type_t binop_type)
{
  return string(strs_strs[binop_type]);
}

string
BinOp::optype_to_name(const binop_type_t binop_type)
{
  return string(binopnames_strs[binop_type]);
}

binop_type_t
BinOp::string_to_optype(const string s)
{
  for(unsigned i = 0; i < sizeof(strs_strs); i++){
    if(optype_to_string((binop_type_t)i) == s)
      return (binop_type_t) i;
  }
  assert(1 == 0);
  return (binop_type_t) 0;
}

void BinOp::destroy( BinOp *expr )
{
    assert(expr);

    Exp::destroy(expr->lhs);
    Exp::destroy(expr->rhs);

    delete expr;
}

FBinOp::FBinOp(fbinop_type_t t, rounding_mode_t rm, Exp *l, Exp *r)
  : Exp(FBINOP), lhs(l), rhs(r), fbinop_type(t), rounding_mode(rm)
{

}

FBinOp::FBinOp(const FBinOp& copy)
  : Exp(FBINOP), fbinop_type(copy.fbinop_type),
    rounding_mode(copy.rounding_mode)
{
  lhs = copy.lhs->clone();
  rhs = copy.rhs->clone();
}

FBinOp *
FBinOp::clone() const
{
  return new FBinOp(*this);
}

string
FBinOp::optype_to_string(const fbinop_type_t fbinop_type) {
  string s;
  switch (fbinop_type) {
  case FPLUS:   s = "+.";  break;
  case FMINUS:  s = "-.";  break;
  case FTIMES:  s = "*.";  break;
  case FDIVIDE: s = "/.";  break;
  case FEQ:     s = "==."; break;
  case FNEQ:    s = "<>."; break;
  case FLT:     s = "<.";  break;
  case FLE:     s = "<=."; break;
  }
  return s;
}

string
rounding_mode_to_letter_string(const rounding_mode_t rm) {
  char c = '?';
  switch (rm) {
  case ROUND_NEAREST:           c = 'e'; break;
  case ROUND_NEAREST_AWAY_ZERO: c = 'a'; break;
  case ROUND_POSITIVE:          c = 'P'; break;
  case ROUND_NEGATIVE:          c = 'N'; break;
  case ROUND_ZERO:              c = 'Z'; break;
  }
  return string(1, c);
}

string
FBinOp::tostring() const
{
  string ret;
  ret = lhs->tostring() + " " + optype_to_string(fbinop_type)
    + rounding_mode_to_letter_string(rounding_mode) + " " + rhs->tostring();
  ret = "(" + ret + ")";
  return ret;
}

string
FBinOp::optype_to_name(const fbinop_type_t fbinop_type)
{
  string s;
  switch (fbinop_type) {
  case FPLUS:   s = "FPLUS";   break;
  case FMINUS:  s = "FMINUS";  break;
  case FTIMES:  s = "FTIMES";  break;
  case FDIVIDE: s = "FDIVIDE"; break;
  case FEQ:     s = "FEQ";     break;
  case FNEQ:    s = "FNEQ";    break;
  case FLT:     s = "FLT";     break;
  case FLE:     s = "FLE";     break;
  }
  return s;
}

void FBinOp::destroy( FBinOp *expr )
{
    assert(expr);

    Exp::destroy(expr->lhs);
    Exp::destroy(expr->rhs);

    delete expr;
}

UnOp::UnOp(const UnOp& copy)
  : Exp(UNOP), unop_type(copy.unop_type)
{
  exp = copy.exp->clone();
}

UnOp::UnOp(unop_type_t typ, Exp *e)  
  : Exp(UNOP), unop_type(typ), exp(e)
{ 
}

UnOp *
UnOp::clone() const
{
  return new UnOp(*this);
}


string
UnOp::tostring() const
{
   string ret;
   switch(unop_type){
   case NEG:
     ret = "-" + exp->tostring();
    break;
   case NOT:
     ret = "!" + exp->tostring();
     break;
   }
   ret = "(" + ret + ")";
   return ret;
}

string
UnOp::optype_to_string(const unop_type_t op)
{
  // Do NOT change this. It is used in producing XML output.
  string ret;
  switch(op){
  case NEG:
    ret = "NEG"; break;
  case NOT:
    ret = "NOT"; break;
  }
  return ret;
}

unop_type_t
UnOp::string_to_optype(const string s)
{
  if(s == "NEG") return NEG;
  if(s == "NOT") return NOT;
  assert(1 == 0);
  return NEG;
}

void UnOp::destroy( UnOp *expr )
{
    assert(expr);

    Exp::destroy(expr->exp);

    delete expr;
}

FUnOp::FUnOp(const FUnOp& copy)
  : Exp(FUNOP), funop_type(copy.funop_type), rounding_mode(copy.rounding_mode)
{
  exp = copy.exp->clone();
}

FUnOp::FUnOp(funop_type_t typ, rounding_mode_t rm, Exp *e)
  : Exp(FUNOP), funop_type(typ), rounding_mode(rm), exp(e)
{
}

FUnOp *
FUnOp::clone() const
{
  return new FUnOp(*this);
}

string
FUnOp::tostring() const
{
   string ret;
   string rm_str = rounding_mode_to_letter_string(rounding_mode);
   switch(funop_type){
   case FNEG:
     ret = "-." + rm_str + " " + exp->tostring();
     break;
   }
   ret = "(" + ret + ")";
   return ret;
}

string
FUnOp::optype_to_string(const funop_type_t op)
{
  string ret;
  switch(op){
  case FNEG:
    ret = "FNEG"; break;
  }
  return ret;
}

void FUnOp::destroy( FUnOp *expr )
{
    assert(expr);

    Exp::destroy(expr->exp);

    delete expr;
}

/*
Mem::Mem(Exp *a)
  : Exp(MEM), addr(a)
{
  typ = reg_address_t;
}
*/

Mem::Mem(Exp *a, reg_t t)
  : Exp(MEM), addr(a), typ(t)
{
}


Mem::Mem(const Mem& copy)
  : Exp(MEM)
{
  addr = copy.addr->clone();
  typ = copy.typ;
}

Mem *
Mem::clone() const
{
  return new Mem(*this);
}

string
Mem::tostring() const
{
  return "mem[" + addr->tostring() + "]";
// + Exp::string_type(typ);
  //os << "mem[" << addr->tostring() << "]";
}

void Mem::destroy( Mem *expr )
{
    assert(expr);

    Exp::destroy(expr->addr);

    delete expr;    
}

Constant::Constant(reg_t t, const_val_t v)
  : Exp(CONSTANT), typ(t), val(v)
{
}



Constant::Constant(const Constant& other)
  : Exp(CONSTANT),typ(other.typ), val(other.val)
{
}


Constant *
Constant::clone() const
{
  return new Constant(*this);
}

string
Constant::tostring() const
{
  ostringstream os;
  uint8_t u8;
  uint16_t u16;
  uint32_t u32;
  uint64_t u64;
  
  switch(typ){
  case REG_1: if(val == 0) os << "0"; else os << "1"; break;
  case REG_8: u8 = (uint8_t) val; os << dec << (int) u8; break;
  case REG_16: u16 = (uint16_t) val; os << u16; break;
  case REG_32: u32 = (uint32_t) val; os << u32; break;
  case REG_64: u64 = (uint64_t) val; os << u64; break;
  }
  os << ":" << Exp::string_type(typ);
  return os.str();
}

void Constant::destroy( Constant *expr )
{
    assert(expr);

    delete expr;
}

Constant Constant::t = Constant(REG_1,
				(const_val_t)1);
Constant Constant::f = Constant(REG_1,
				(const_val_t)0);


// ------------------------------------------------------------
// Class Phi
//         Phi functions
// ------------------------------------------------------------
Phi::Phi(const Phi& copy)
  : Exp(PHI)
{
  for(vector<Temp *>::const_iterator it = copy.vars.begin();
      it != copy.vars.end(); it++){
    Temp *t = *it;
    this->vars.push_back(t->clone());
  }
  phi_name = copy.phi_name;
}

Phi::Phi(string orig_name, vector<Temp*> v)
  : Exp(PHI), vars(v), phi_name(orig_name)
{ 
}

Phi *
Phi::clone() const
{
  return new Phi(*this);
}

string
Phi::tostring() const
{ 
  string ret = "PHI(";
  string comma = " ";
  for (vector<Temp *>::const_iterator it = vars.begin();
       it != vars.end(); it++) {
    ret += comma;
    ret += (*it)->tostring();
    comma = ",";
  }
  ret += " )";
  return ret;
}

void Phi::destroy( Phi *expr )
{
    assert(expr);

    unsigned int i;

    for ( i = 0; i < expr->vars.size(); i++ )
    {
        Exp::destroy(expr->vars.at(i));
    }

    delete expr;
}

Ite::Ite(Exp *c, Exp *t, Exp *f)
  : Exp(ITE), cond(c), true_e(t), false_e(f)
{

}

Ite::Ite(const Ite &copy)
  : Exp(ITE)
{
  cond = copy.cond->clone();
  true_e = copy.true_e->clone();
  false_e = copy.false_e->clone();
}

Ite *
Ite::clone() const
{
  return new Ite(*this);
}

string
Ite::tostring() const
{
  string ret = "(";
  ret += cond->tostring();
  ret += " ? ";
  ret += true_e->tostring();
  ret += " : ";
  ret += false_e->tostring();
  ret += ")";
  return ret;
}

void Ite::destroy( Ite *expr )
{
    assert(expr);

    Exp::destroy(expr->cond);
    Exp::destroy(expr->true_e);
    Exp::destroy(expr->false_e);

    delete expr;
}

Temp::Temp(reg_t t, string n) 
  : Exp(TEMP), typ(t), name(n)
{ }


Temp::Temp(const Temp &other)
  : Exp(TEMP), typ(other.typ), name(other.name)
{
}


Temp *
Temp::clone() const
{
  return new Temp(*this);
}

string 
Temp::tostring() const
{
  // Argh! Stop removing useful error checking and debugging information.
  // It doesn't hurt anyone.
  return name + ":" + Exp::string_type(typ);
  //return name;
}



void Temp::destroy( Temp *expr )
{
    assert(expr);

    delete expr;
}

Unknown::Unknown(string s) : Exp(UNKNOWN), str(s)
{ }

Unknown::Unknown(const Unknown &other) : Exp(UNKNOWN), str(other.str)
{ }

Unknown *
Unknown::clone() const
{
  return new Unknown(*this);
}

void Unknown::destroy( Unknown *expr )
{
    assert(expr);

    delete expr;
}

Name::Name( string s ) : Exp(NAME), name(s)
{ 
  //typ = reg_address_t;
}

Name::Name( const Name &other ) : Exp(NAME), name(other.name)
{ 
  //typ = reg_address_t;
}

Name *
Name::clone() const
{
  return new Name(*this);
}

string Name::tostring() const
{
  //  return "name(L_" + name + ")";
  return "name(" + name + ")";
}

void Name::destroy( Name *expr )
{
    assert(expr);

    delete expr;
}

Cast::Cast( Exp *e, reg_t w, cast_t t ) 
  : Exp(CAST), exp(e), typ(w), cast_type(t)
{ }

Cast::Cast( const Cast &other ) 
  : Exp(CAST), typ(other.typ), cast_type(other.cast_type)
{
  exp = other.exp->clone();
}

Cast *
Cast::clone() const
{
  return new Cast(*this);
}

void Cast::destroy( Cast *expr )
{
    assert(expr);

    Exp::destroy(expr->exp);

    delete expr;
}

string Cast::tostring() const
{
  string wstr, tstr;

  wstr = Exp::string_type(this->typ);

  switch ( cast_type )
  {
    case CAST_UNSIGNED: tstr = "U"; break;
    case CAST_SIGNED:   tstr = "S"; break;
    case CAST_HIGH:     tstr = "H"; break;
    case CAST_LOW:      tstr = "L"; break;
    default: 
      cout << "Unrecognized cast type" << endl;
      assert(0);
  }
  string ret = "cast(" + exp->tostring() + ")" + tstr + ":" + wstr;
  return ret;
}

string Cast::cast_type_to_string( const cast_t ctype )
{
  string  tstr;
  switch ( ctype )
  {
    case CAST_UNSIGNED: tstr = "Unsigned"; break;
    case CAST_SIGNED:   tstr = "Signed"; break;
    case CAST_HIGH:     tstr = "High"; break;
    case CAST_LOW:      tstr = "Low"; break;
    default:
      cout << "Unrecognized cast type" << endl;
      assert(0);
  }

  return tstr;
}

FCast::FCast( Exp *e, reg_t w, fcast_t t, rounding_mode_t rm )
  : Exp(FCAST), exp(e), typ(w), fcast_type(t), rounding_mode(rm)
{ }

FCast::FCast( const FCast &other )
  : Exp(FCAST), typ(other.typ), fcast_type(other.fcast_type),
    rounding_mode(other.rounding_mode)
{
  exp = other.exp->clone();
}

FCast *
FCast::clone() const
{
  return new FCast(*this);
}

void FCast::destroy( FCast *expr )
{
    assert(expr);

    Exp::destroy(expr->exp);

    delete expr;
}

string FCast::tostring() const
{
  string wstr, tstr;

  wstr = Exp::string_type(this->typ);
  string rm_str = rounding_mode_to_letter_string(rounding_mode);

  switch ( fcast_type )
  {
    case CAST_SFLOAT:  tstr = "SFloat";  break;
    case CAST_UFLOAT:  tstr = "UFloat";  break;
    case CAST_SFIX:    tstr = "SFix";    break;
    case CAST_UFIX:    tstr = "UFix";    break;
    case CAST_FWIDEN:  tstr = "FWiden";  break;
    case CAST_FNARROW: tstr = "FNarrow"; break;
    default:
      cout << "Unrecognized FP cast type" << fcast_type << endl;
      assert(0);
  }
  string ret = "cast." + rm_str + "(" + exp->tostring() + ")"
    + tstr + ":" + wstr;
  return ret;
}

string FCast::fcast_type_to_string( const fcast_t ctype )
{
  string  tstr;
  switch ( ctype )
  {
    case CAST_SFLOAT:  tstr = "SignedFloat";   break;
    case CAST_UFLOAT:  tstr = "UnsignedFloat"; break;
    case CAST_SFIX:    tstr = "SignedFix";     break;
    case CAST_UFIX:    tstr = "UnsignedFix";   break;
    case CAST_FWIDEN:  tstr = "FloatWiden";    break;
    case CAST_FNARROW: tstr = "FloatNarrow";   break;
    default:
      cout << "Unrecognized FP cast type" << ctype << endl;
      assert(0);
  }

  return tstr;
}


Vector::Vector(Exp *h, Exp *l)
  : Exp(VECTOR)
{
  lanes[1] = h;
  lanes[0] = l;
}

Vector::Vector(const Vector &copy)
  : Exp(VECTOR)
{
  lanes[0] = copy.lanes[0]->clone();
  lanes[1] = copy.lanes[1]->clone();
}

Vector *
Vector::clone() const
{
  return new Vector(*this);
}

string
Vector::tostring() const
{
  string ret = "[";
  ret += lanes[1]->tostring();
  ret += ", ";
  ret += lanes[0]->tostring();
  ret += "]";
  return ret;
}

void Vector::destroy( Vector *expr )
{
    assert(expr);

    Exp::destroy(expr->lanes[1]);
    Exp::destroy(expr->lanes[0]);

    delete expr;
}


///////////////////////////// LET ////////////////////////////////
Let::Let(Exp *v, Exp *e, Exp *i) : Exp(LET), var(v), exp(e), in(i)
{
  /* empty */
}

Let *Let::clone() const
{

  return new Let(*this);
}


Let::Let(const Let &other) : Exp(LET)
{
  var = other.var->clone();
  exp = other.exp->clone();
  in = other.in->clone();
}

void
Let::destroy(Exp *exp)
{
  assert(LET == exp->exp_type);
  Let *let = (Let *)exp;
  Exp::destroy(let->var);
  Exp::destroy(let->exp);
  Exp::destroy(let->in);
  delete exp;
}

string
Let::tostring() const
{
  string s = "(let " + var->tostring() + " = " + exp->tostring()
    + " in " + in->tostring() + ")";
  return s;
}

//======================================================================
// 
// Shorthand functions for creating expressions
//
// Functions preceded with a _ are versions that do not make deep
// copies of their expression arguments before using them.
// 
//======================================================================

Exp *ecl( Exp *exp )
{
    assert(exp);
    return exp->clone();
}

Constant *ex_const(uint32_t value )
{
  return new Constant(REG_32, value);
}

Constant *ex_const64(uint64_t value )
{
  return new Constant(REG_64, value);
}

Constant *ex_addr_const(int64_t value) {
  return new Constant(reg_address_t, value);
}

Constant *ex_const(reg_t t, const_val_t value )
{
    return new Constant(t, value);
}


Name *ex_name( string name )
{
    return new Name(name);
}

UnOp *_ex_not( Exp *arg )
{
    return new UnOp(NOT, arg);
}

UnOp *ex_not( Exp *arg )
{
    arg = arg->clone();
    return _ex_not(arg);
}

UnOp *_ex_neg( Exp *arg )
{
    return new UnOp(NEG, arg);
}

UnOp *ex_neg( Exp *arg )
{
    arg = arg->clone();
    return _ex_neg(arg);
}

BinOp *_ex_add( Exp *arg1, Exp *arg2 )
{
    return new BinOp(PLUS, arg1, arg2);
}

BinOp *ex_add( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
    return _ex_add(arg1, arg2);
}

BinOp *_ex_sub( Exp *arg1, Exp *arg2 )
{
    return new BinOp(MINUS, arg1, arg2);
}

BinOp *ex_sub( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
    return _ex_sub(arg1, arg2);
}

BinOp *_ex_mul( Exp *arg1, Exp *arg2 )
{
    return new BinOp(TIMES, arg1, arg2);
}

BinOp *ex_mul( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
    return _ex_mul(arg1, arg2);
}

BinOp *_ex_div( Exp *arg1, Exp *arg2 )
{
    return new BinOp(DIVIDE, arg1, arg2);
}

BinOp *ex_div( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
  return _ex_div(arg1, arg2);
}

BinOp *_ex_mod( Exp *arg1, Exp *arg2 )
{
    return new BinOp(MOD, arg1, arg2);
}

BinOp *ex_mod( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
  return _ex_mod(arg1, arg2);
}

BinOp *_ex_and( Exp *arg1, Exp *arg2 )
{
    return new BinOp(BITAND, arg1, arg2);
}

BinOp *_ex_and( Exp *arg1, Exp *arg2, Exp *arg3 )
{
    return _ex_and(arg1, _ex_and(arg2, arg3));
}

BinOp *_ex_and( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4, Exp *arg5, Exp *arg6, Exp *arg7 )
{
    return _ex_and( _ex_and(arg1, arg2, arg3), _ex_and(arg4, arg5, arg6), arg7 );
}

BinOp *ex_and( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
    return _ex_and(arg1, arg2);
}

BinOp *ex_and( Exp *arg1, Exp *arg2, Exp *arg3 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
  arg3 = arg3->clone();
    return _ex_and(arg1, arg2, arg3);
}

BinOp *ex_and( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4, Exp *arg5, Exp *arg6, Exp *arg7 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
  arg3 = arg3->clone();
  arg4 = arg4->clone();
  arg5 = arg5->clone();
  arg6 = arg6->clone();
  arg7 = arg7->clone();
    return _ex_and(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

BinOp *_ex_or( Exp *arg1, Exp *arg2 )
{
    return new BinOp(BITOR, arg1, arg2);
}

BinOp *_ex_or( Exp *arg1, Exp *arg2, Exp *arg3 )
{
    return _ex_or(arg1, _ex_or(arg2, arg3));
}

BinOp *_ex_or( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4 )
{
    return _ex_or(_ex_or(arg1, arg2), _ex_or(arg3, arg4));
}

BinOp *_ex_or( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4, Exp *arg5, Exp *arg6 )
{
    return _ex_or( _ex_or(arg1, arg2, arg3), _ex_or(arg4, arg5, arg6) );
}

BinOp *_ex_or( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4, Exp *arg5, Exp *arg6, Exp *arg7 )
{
    return _ex_or( _ex_or(arg1, arg2, arg3), _ex_or(arg4, arg5, arg6), arg7 );
}

BinOp *ex_or( Exp *arg1, Exp *arg2 )
{
  arg1 = arg1->clone();
  arg2 = arg2->clone();
    return _ex_or(arg1, arg2);
}

BinOp *ex_or( Exp *arg1, Exp *arg2, Exp *arg3 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    arg3 = arg3->clone();
    return _ex_or(arg1, arg2, arg3);
}

BinOp *ex_or( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4, Exp *arg5, Exp *arg6 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    arg3 = arg3->clone();
    arg4 = arg4->clone();
    arg5 = arg5->clone();
    arg6 = arg6->clone();
    return _ex_or(arg1, arg2, arg3, arg4, arg5, arg6);
}

BinOp *ex_or( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4, Exp *arg5, Exp *arg6, Exp *arg7 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    arg3 = arg3->clone();
    arg4 = arg4->clone();
    arg5 = arg5->clone();
    arg6 = arg6->clone();
    arg7 = arg7->clone();
    return _ex_or(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

BinOp *_ex_xor( Exp *arg1, Exp *arg2 )
{
    return new BinOp(XOR, arg1, arg2);
}

BinOp *_ex_xor( Exp *arg1, Exp *arg2, Exp *arg3 )
{
    return _ex_xor(arg1, _ex_xor(arg2, arg3));
}

BinOp *_ex_xor( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4 )
{
    return _ex_xor( _ex_xor(arg1, arg2), _ex_xor(arg3, arg4) );
}

BinOp *_ex_xor( Exp *arg1, Exp *arg2, Exp *arg3, Exp *arg4,
                       Exp *arg5, Exp *arg6, Exp *arg7, Exp *arg8 )
{
    return _ex_xor( _ex_xor(arg1, arg2, arg3, arg4), _ex_xor(arg5, arg6, arg7, arg8) );
}

BinOp *ex_xor( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return _ex_xor(arg1, arg2);
}

BinOp *ex_xor( Exp *arg1, Exp *arg2, Exp *arg3 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    arg3 = arg3->clone();
    return _ex_xor(arg1, arg2, arg3);
}

BinOp *_ex_shl( Exp *arg1, Exp *arg2 )
{
    return new BinOp(LSHIFT, arg1, arg2);
}

BinOp *_ex_shl( Exp *arg1, int arg2 )
{
    return new BinOp(LSHIFT, arg1, ex_const(arg2));
}

BinOp *ex_shl( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(LSHIFT, arg1, arg2);
}

BinOp *ex_shl( Exp *arg1, int arg2 )
{
    arg1 = arg1->clone();
    return new BinOp(LSHIFT, arg1, ex_const(arg2));
}

BinOp *_ex_shr( Exp *arg1, Exp *arg2 )
{
    return new BinOp(RSHIFT, arg1, arg2);
}

BinOp *_ex_shr( Exp *arg1, int arg2 )
{
    return new BinOp(RSHIFT, arg1, ex_const(arg2));
}

BinOp *ex_shr( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(RSHIFT, arg1, arg2);
}

BinOp *ex_shr( Exp *arg1, int arg2 )
{
    arg1 = arg1->clone();
    return new BinOp(RSHIFT, arg1, ex_const(arg2));
}

BinOp *_ex_sar( Exp *arg1, Exp *arg2 )
{
    return new BinOp(ARSHIFT, arg1, arg2);
}

BinOp *ex_sar( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(ARSHIFT, arg1, arg2);
}

BinOp *ex_sar( Exp *arg1, int arg2 )
{
    arg1 = arg1->clone();
    return new BinOp(ARSHIFT, arg1, ex_const(arg2));
}

BinOp *_ex_eq( Exp *arg1, Exp *arg2 )
{
    return new BinOp(EQ, arg1, arg2);
}

BinOp *ex_eq( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(EQ, arg1, arg2);
}

BinOp *_ex_neq( Exp *arg1, Exp *arg2 )
{
    return new BinOp(NEQ, arg1, arg2);
}

BinOp *ex_neq( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(NEQ, arg1, arg2);
}

BinOp *ex_gt( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(GT, arg1, arg2);
}

BinOp *_ex_lt( Exp *arg1, Exp *arg2 )
{
    return new BinOp(LT, arg1, arg2);
}

BinOp *ex_lt( Exp *arg1, Exp *arg2 )
{   
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(LT, arg1, arg2);
}

BinOp *ex_ge( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(GE, arg1, arg2);
}

BinOp *_ex_le( Exp *arg1, Exp *arg2 )
{
    return new BinOp(LE, arg1, arg2);
}

BinOp *ex_le( Exp *arg1, Exp *arg2 )
{   
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(LE, arg1, arg2);
}

BinOp *_ex_slt( Exp *arg1, Exp *arg2 )
{
    return new BinOp(SLT, arg1, arg2);
}

BinOp *ex_slt( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(SLT, arg1, arg2);
}

BinOp *_ex_sle( Exp *arg1, Exp *arg2 )
{
    return new BinOp(SLE, arg1, arg2);
}

BinOp *ex_sle( Exp *arg1, Exp *arg2 )
{
    arg1 = arg1->clone();
    arg2 = arg2->clone();
    return new BinOp(SLE, arg1, arg2);
}

Cast *ex_u_cast( Exp *arg, reg_t width )
{
  arg = arg->clone();
  return new Cast(arg, width, CAST_UNSIGNED);
}

Cast *_ex_u_cast( Exp *arg, reg_t width )
{
    return new Cast(arg, width, CAST_UNSIGNED);
}

Cast *ex_s_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new Cast(arg, width, CAST_SIGNED);
}

Cast *_ex_s_cast( Exp *arg, reg_t width )
{
    return new Cast(arg, width, CAST_SIGNED);
}

Cast *ex_h_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new Cast(arg, width, CAST_HIGH);
}

Cast *_ex_h_cast( Exp *arg, reg_t width )
{
    return new Cast(arg, width, CAST_HIGH);
}

Cast *ex_l_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new Cast(arg, width, CAST_LOW);
}

Cast *_ex_l_cast( Exp *arg, reg_t width )
{
    return new Cast(arg, width, CAST_LOW);
}

FCast *ex_is_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new FCast(arg, width, CAST_SFIX, ROUND_NEAREST);
}

FCast *ex_iu_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new FCast(arg, width, CAST_UFIX, ROUND_NEAREST);
}

FCast *ex_fs_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new FCast(arg, width, CAST_SFLOAT, ROUND_NEAREST);
}

FCast *ex_fu_cast( Exp *arg, reg_t width )
{
    arg = arg->clone();
    return new FCast(arg, width, CAST_UFLOAT, ROUND_NEAREST);
}

Vector *ex_vector2x64(Exp *h, Exp *l) {
    h = h->clone();
    l = l->clone();
    return new Vector(h, l);
}

Vector *_ex_vector2x64(Exp *h, Exp *l) {
    return new Vector(h, l);
}

Ite *_ex_ite( Exp *c, Exp *t, Exp *f) {
    return new Ite(c, t, f);
}

Ite *ex_ite( Exp *c, Exp *t, Exp *f) {
    return _ex_ite(c->clone(), t->clone(), f->clone());
}

Exp *_ex_get_bit(Exp *e, int which) {
    return _ex_l_cast(_ex_shr(e, ex_const(REG_32, which)), REG_1);
}

Exp *ex_get_bit(Exp *e, int which) {
    return _ex_get_bit(ecl(e), which);
}

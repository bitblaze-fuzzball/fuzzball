#include "irtoir.h"
#include "exp.h"
#include "stmt.h"
#include <assert.h>
#include <iostream>
using namespace std;

extern bool use_eflags_thunks;

typedef vector<Stmt *> Mod_Func_0 (void);
typedef vector<Stmt *> Mod_Func_2 (reg_t, Exp *, Exp *);
typedef vector<Stmt *> Mod_Func_3 (reg_t, Exp *, Exp *, Exp *);

Func* gen_thunk(string name, int argnum, Mod_Func_0 *mod_eflags_func, 
                reg_t type)
{

  Exp *DEP1 = new Temp(REG_32, "R_CC_DEP1");
  Exp *DEP2 = new Temp(REG_32, "R_CC_DEP2");
  Exp *NDEP = new Temp(REG_32, "R_CC_NDEP");

  vector<Stmt*> mods;

  if (argnum == 2) {
    Mod_Func_2 *mod_func = (Mod_Func_2 *)mod_eflags_func;
    mods = mod_func(type, DEP1, DEP2);
  } else if (argnum == 3) {
    Mod_Func_3 *mod_func = (Mod_Func_3 *)mod_eflags_func;
    mods = mod_func(type, DEP1, DEP2, NDEP);
  } else
    assert(0);

  mods.push_back(new Return(NULL));

  vector<VarDecl*> params;
  return new Func(name, false, REG_1, params, false, mods);
}

#define gt(vec, name, argnum, helper) \
vec.push_back(gen_thunk("X86G_CC_OP_" #name "B_thunk", argnum, (Mod_Func_0*)mod_eflags_##helper, REG_8)); \
vec.push_back(gen_thunk("X86G_CC_OP_" #name "W_thunk", argnum, (Mod_Func_0*)mod_eflags_##helper, REG_16)); \
vec.push_back(gen_thunk("X86G_CC_OP_" #name "L_thunk", argnum, (Mod_Func_0*)mod_eflags_##helper, REG_32)); 

void cc_op_chk(vector<Stmt*> &stmts, int cc_op, string cc_op_s)
{
  Exp *cond = _ex_eq(new Temp(REG_32, "R_CC_OP"), 
                     ex_const(REG_32, cc_op));
  string tname = cc_op_s + "_t";
  string fname = cc_op_s + "_f";
  string fnname = cc_op_s + "_thunk";

  stmts.push_back(new CJmp(cond, ex_name(tname), ex_name(fname)));
  stmts.push_back(new Label(tname));
  vector<Exp*> params;
  stmts.push_back(new Call(NULL, fnname, params));
  stmts.push_back(new Return(NULL));
  stmts.push_back(new Label(fname));
}

#define CC_OP_CHK(stmts, op) cc_op_chk(stmts, op, #op)

#define CC_OP_CHK3(stmts, op) \
CC_OP_CHK(stmts, op##B); \
CC_OP_CHK(stmts, op##W); \
CC_OP_CHK(stmts, op##L)

Func* gen_x86g_calculate_eflags_all() 
{
  vector<Stmt*> body;

  CC_OP_CHK(body, X86G_CC_OP_COPY);

  CC_OP_CHK3(body, X86G_CC_OP_ADD);
  CC_OP_CHK3(body, X86G_CC_OP_SUB);
  CC_OP_CHK3(body, X86G_CC_OP_ADC);
  CC_OP_CHK3(body, X86G_CC_OP_SBB);
  CC_OP_CHK3(body, X86G_CC_OP_LOGIC);
  CC_OP_CHK3(body, X86G_CC_OP_INC);
  CC_OP_CHK3(body, X86G_CC_OP_DEC);
  CC_OP_CHK3(body, X86G_CC_OP_SHL);
  CC_OP_CHK3(body, X86G_CC_OP_SHR);
  CC_OP_CHK3(body, X86G_CC_OP_ROL);
  CC_OP_CHK3(body, X86G_CC_OP_ROR);
  CC_OP_CHK3(body, X86G_CC_OP_UMUL);
  CC_OP_CHK3(body, X86G_CC_OP_SMUL);
  body.push_back(new Special("BAD_CC_OP"));
  body.push_back(new Return(NULL));
  
  vector<VarDecl*> params;
  return new Func("x86g_calculate_eflags_all", 
                  false, REG_1, params, false, body);
}

Func* gen_x86g_calculate_eflags_c() 
{
  vector<Stmt*> body;
  vector<Exp*> params;
  body.push_back(new Call(NULL, "x86g_calculate_eflags_all", params));
  body.push_back(new Return(NULL));

  vector<VarDecl*> func_params;
  return new Func("x86g_calculate_eflags_c", 
                  false, REG_1, func_params, false, body);
}

vector<Stmt*> gen_eflags_helpers()
{
  vector <Stmt*> stmts;

  if(!use_eflags_thunks)
    return stmts;

  stmts.push_back(gen_thunk("X86G_CC_OP_COPY_thunk", 
                            2, 
                            (Mod_Func_0*)mod_eflags_copy, REG_32));

  gt(stmts, ADD, 2, add);
  gt(stmts, SUB, 2, sub);
  gt(stmts, ADC, 3, adc);
  gt(stmts, SBB, 3, sbb);
  gt(stmts, LOGIC, 2, logic);
  gt(stmts, INC, 3, inc);
  gt(stmts, DEC, 3, dec);
  gt(stmts, SHL, 2, shl);
  gt(stmts, SHR, 2, shr);
  gt(stmts, ROL, 3, rol);
  gt(stmts, ROR, 3, ror);
  gt(stmts, UMUL, 2, umul);
  gt(stmts, SMUL, 2, smul);

  stmts.push_back(gen_x86g_calculate_eflags_all());
  stmts.push_back(gen_x86g_calculate_eflags_c());

  return stmts;
}

// cut and paste from exectrace_c. should probably put in a utility file.
template< class T >
static T* vec_to_list(const vector<T> &vec)
{
  //  T* rv = new T[vec.size()+1];
  T* rv = (T*)malloc((vec.size()+1) * sizeof(T));
  assert(rv != NULL);

  for(unsigned int i = 0; i < vec.size(); i++) {
    rv[i] = vec[i];
  }
  rv[vec.size()] = NULL;

  return rv;
}

Stmt** gen_eflags_helpers_c()
{
  vector<Stmt*> stmts = gen_eflags_helpers();
  return vec_to_list(stmts);
}

#ifndef __IRTOIR_INTERNAL_H
#define __IRTOIR_INTERNAL_H

#include "irtoir.h"

#include "vex_version.h"

#if LIBASMIR_VEX_VERSION >= 1793
#define Ist_MFence Ist_MBE
#endif
#ifndef VEX_VERSION
#define VEX_VERSION LIBASMIR_VEX_VERSION
#endif

// functions internal to irtoir.cpp and irtoir-*.cpp

__attribute((noreturn)) void panic( string msg );

reg_t IRType_to_reg_type( IRType type );

reg_t get_exp_type( Exp *exp );

inline int get_type_size(reg_t typ) {
  return Exp::reg_to_bits(typ);
}


Temp *mk_temp( string name, IRType ty );
Temp *mk_temp( reg_t type, vector<Stmt *> *stmts );
Temp *mk_temp( IRType ty, vector<Stmt *> *stmts );

Stmt *mk_assign_tmp(IRTemp tmp, Exp *rhs_e, IRSB *irbb,
		    vector<Stmt *> *irout );

Exp *translate_expr( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );

string get_op_str(asm_program_t *prog, Instruction *inst );

Exp *emit_ite(vector<Stmt *> *irout, reg_t type,
	      Exp *cond, Exp *exp_t, Exp *exp_f);
int match_ite(vector<Stmt*> *ir, unsigned int i,
	      Exp **cond, Exp **exp_t, Exp **exp_f, Exp **res);

extern bool use_eflags_thunks;
extern Exp * count_opnd;


//
// arch specific functions used in irtoir.cpp
//

// defined in irtoir-i386.cpp
vector<VarDecl *> i386_get_reg_decls();
IRStmt *i386_make_pc_put_stmt(Addr64 addr);
Exp  *i386_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *i386_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
Exp  *i386_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *i386_translate_dirty( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
void  i386_modify_flags( asm_program_t *prog, vine_block_t *block );

// defined in irtoir-x64.cpp
vector<VarDecl *> x64_get_reg_decls();
IRStmt *x64_make_pc_put_stmt(Addr64 addr);
Exp  *x64_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *x64_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
Exp  *x64_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *x64_translate_dirty( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
void  x64_modify_flags( asm_program_t *prog, vine_block_t *block );

// defined in irtoir-arm.cpp
vector<VarDecl *> arm_get_reg_decls();
IRStmt *arm_make_pc_put_stmt(Addr64 addr);
Exp  *arm_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
Stmt *arm_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout );
Exp  *arm_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout );
void  arm_modify_flags( asm_program_t *prog, vine_block_t *block );


#endif

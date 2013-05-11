#ifndef _IR_PROGRAM_H
#define _IR_PROGRAM_H
#include "common.h"
#include "stmt.h"
#include "exp.h"
#include "asm_program.h"
#include "irtoir.h"

using namespace std;


typedef struct _ir_bb_t {
  vector<Stmt *> stmts;
} ir_bb_t;

typedef struct _ir_function_t {
  string name;
  set<string> calls;
  set<ir_bb_t *> blocks;
} ir_function_t;

typedef struct _ir_program_t {
  set<ir_function_t *> functions;
} ir_program_t;

// Returns a vector of x86 register declarations
vector<VarDecl *> get_reg_decls();

/// Name says it all. Translates all functions in the asm_program_t to
/// the IR, recreating the CFG for each function, and the CG.
ir_program_t *asm_program_to_ir(asm_program_t *p);

/// Name says it all. Translates a single asm_function_t to the IR,
/// and generates the new CFG.
ir_function_t *asm_function_to_ir(asm_program_t *p, asm_function_t *asm_f);

/// Translate an instruction, given as a raw sequence of bytes
/// to the IR, create a control flwo graph, and return the
/// "function".
ir_function_t * asm_insn_to_ir(address_t addr, 
			       unsigned char *bb, 
			       unsigned len);

/// Translate a list of addresses in the program into the IR
map<address_t, vector<Stmt *> > 
asm_addrs_to_ir(asm_program_t *p, 
		const vector<address_t> &addrs);

/// Translate an address into a vine_block
extern "C" {
extern vine_block_t* asm_addr_to_ir(asm_program_t *p, address_t addr);
}

/// Translate a vector of Instructions into the IR 
map<address_t, vector<Stmt *> >
instruction_to_ir(asm_program_t *prog, vector<Instruction *> &instrs);

/// Build the initial basic blocks and cfg from the new IR stmts
/// f must point to an allocated ir_function_t
void build_initial_blocks_and_cfg(ir_function_t *f, 
				  vector<Stmt *> &stmts);


void print_function(ir_function_t *f, ostream &out = cout);

/// Unroll any loops in a function @param unroll_cnt times.
/// @returns a new function object with each loop unrolled.
ir_function_t *
unroll_function(ir_function_t *f, const unsigned &unroll_cnt);


class IRProgramVisitor : public virtual IRVisitor {
  public:
   virtual void visitProgram(ir_program_t *) = 0;
   virtual void visitFunction(ir_function_t *) = 0;
   virtual void visitBlock(ir_bb_t *) = 0;
};

class DefaultIRProgramVisitor: public IRProgramVisitor, public DefaultIRVisitor {
  public:
   DefaultIRProgramVisitor(){};
   virtual ~DefaultIRProgramVisitor(){};
   virtual void visitProgram(ir_program_t *);
   virtual void visitFunction(ir_function_t *);
   virtual void visitBlock(ir_bb_t *);

   /*
   virtual void visitBinOp(BinOp *);
   virtual void visitUnOp(UnOp *);
   virtual void visitConstant(Constant *);
   virtual void visitTemp(Temp *);
   virtual void visitPhi(Phi *);
   virtual void visitSpecial(Special *);
   virtual void visitMem(Mem *);
   virtual void visitUnknown(Unknown *);
   virtual void visitCast(Cast *);
   virtual void visitName(Name *);
   virtual void visitJmp(Jmp *);
   virtual void visitCJmp(CJmp *);
   virtual void visitLabel(Label *);
   virtual void visitMove(Move *);
   virtual void visitComment(Comment *);
   virtual void visitExpStmt(ExpStmt *);
   */
};

#endif

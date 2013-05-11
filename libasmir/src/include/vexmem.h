//======================================================================
//
// Vexmem is a memory management module created for Vexir. See Notes
// for more details.
//
//======================================================================

#ifndef __VEXMEM_H
#define __VEXMEM_H



extern "C"
{
#include "libvex_basictypes.h"
  /*
#include "libvex_guest_x86.h"
#include "libvex_guest_arm.h"
  */
#include "libvex.h"
#include "libvex_ir.h"
}


void *vx_Alloc( Int nbytes );
void vx_FreeAll();
IRConst* vx_IRConst_U1 ( Bool bit );
IRConst* vx_IRConst_U8 ( UChar u8 );
IRConst* vx_IRConst_U16 ( UShort u16 );
IRConst* vx_IRConst_U32 ( UInt u32 );
IRConst* vx_IRConst_U64 ( ULong u64 );
IRConst* vx_IRConst_F64 ( Double f64 );
IRConst* vx_IRConst_F64i ( ULong f64i );
IRConst* vx_IRConst_V128 ( UShort con );
IRCallee* vx_mkIRCallee ( Int regparms, HChar* name, void* addr );
IRRegArray* vx_mkIRRegArray ( Int base, IRType elemTy, Int nElems );
IRExpr* vx_IRExpr_Binder ( Int binder ); 
IRExpr* vx_IRExpr_Get ( Int off, IRType ty ); 
IRExpr* vx_IRExpr_GetI ( IRRegArray* descr, IRExpr* ix, Int bias ); 
IRExpr* vx_IRExpr_Tmp ( IRTemp tmp ); 
IRExpr* vx_IRExpr_Qop ( IROp op, IRExpr* arg1, IRExpr* arg2, 
                              IRExpr* arg3, IRExpr* arg4 );
IRExpr* vx_IRExpr_Triop  ( IROp op, IRExpr* arg1, 
                                 IRExpr* arg2, IRExpr* arg3 );
IRExpr* vx_IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 );
IRExpr* vx_IRExpr_Unop ( IROp op, IRExpr* arg ); 
IRExpr* vx_IRExpr_Load ( IREndness end, IRType ty, IRExpr* addr ); 
IRExpr* vx_IRExpr_Const ( IRConst* con ); 
IRExpr* vx_IRExpr_CCall ( IRCallee* cee, IRType retty, IRExpr** args ); 
IRExpr* vx_IRExpr_Mux0X ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX ); 
IRDirty* vx_emptyIRDirty ( void ); 
IRStmt* vx_IRStmt_NoOp ( void );
IRStmt* vx_IRStmt_IMark ( Addr64 addr, Int len ); 
IRStmt* vx_IRStmt_AbiHint ( IRExpr* base, Int len ); 
IRStmt* vx_IRStmt_Put ( Int off, IRExpr* data ); 
IRStmt* vx_IRStmt_PutI ( IRRegArray* descr, IRExpr* ix,
                      Int bias, IRExpr* data );
IRStmt* vx_IRStmt_Tmp ( IRTemp tmp, IRExpr* data ); 
IRStmt* vx_IRStmt_Store ( IREndness end, IRExpr* addr, IRExpr* data ); 
IRStmt* vx_IRStmt_Dirty ( IRDirty* d );
IRStmt* vx_IRStmt_MFence ( void );
IRStmt* vx_IRStmt_Exit ( IRExpr* guard, IRJumpKind jk, IRConst* dst ); 
IRTypeEnv* vx_emptyIRTypeEnv ( void );
IRSB* vx_emptyIRSB ( void );
IRExpr** vx_sopyIRExprVec ( IRExpr** vec );
IRExpr** vx_dopyIRExprVec ( IRExpr** vec );
IRConst* vx_dopyIRConst ( IRConst* c );
IRCallee* vx_dopyIRCallee ( IRCallee* ce );
IRRegArray* vx_dopyIRRegArray ( IRRegArray* d );
IRExpr* vx_dopyIRExpr ( IRExpr* e );
IRDirty* vx_dopyIRDirty ( IRDirty* d );
IRStmt* vx_dopyIRStmt ( IRStmt* s );
IRTypeEnv* vx_dopyIRTypeEnv ( IRTypeEnv* src );
IRSB* vx_dopyIRSB ( IRSB* bb );


#endif


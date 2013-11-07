//======================================================================
//
// This file contains the functions for memory management needed by 
// the binary to VEX IR translation interface. A large part of this
// file was copied from irdefs.c in VEX.
//
//======================================================================

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "vexmem.h"

#include "vex_version.h"
#if LIBASMIR_VEX_VERSION >= 1793
#define Ist_MFence Ist_MBE
#endif
#define VEX_VERSION LIBASMIR_VEX_VERSION

//
// Function for panicking
//
void vx_panic ( const HChar* str )
{
    printf("\nvex: the `impossible' happened:\n   %s\n", str);
    exit(-1);
}

//======================================================================
//
// Mem management functions
//
//======================================================================
//
// VEX uses an arena allocator (via LibVEX_Alloc) so that the IR data
// structures it builds are automatically freed after translation.
// Unfortunately, we want to use them after that, so we write deep
// copy routines to move the structures to memory we control. Since we
// don't want to write destructors either, we use our own arena-like
// scheme where all the allocated memory can be freed in one fell
// swoop.
//
// So that the allocation can be fast and sequential without wasting
// too much space, we use a sequence of blocks of exponentially
// increasing size.

const unsigned long vx_first_block_size = 1024;
const int vx_max_blocks = 22;
unsigned char *vx_arena_blocks[vx_max_blocks];
int vx_current_block = -1;
unsigned long vx_block_free = -1;

void *vx_Alloc( Int nbytes )
{
    assert(nbytes > 0);

    if (vx_current_block == -1) {
	vx_current_block++;
	unsigned long next_size = vx_first_block_size << vx_current_block;
	vx_arena_blocks[vx_current_block] = (unsigned char *)malloc(next_size);
	assert(vx_arena_blocks[vx_current_block] != 0);
	vx_block_free = next_size;
    }
    
    if ((unsigned)nbytes > vx_block_free) {
	vx_current_block++;
	assert(vx_current_block < vx_max_blocks);
	unsigned long next_size = vx_first_block_size << vx_current_block;
	vx_arena_blocks[vx_current_block] = (unsigned char *)malloc(next_size);
	// If this assertion fails, it means we ran out of memory when
	// growing the arena. Make sure you're calling vx_FreeAll() at
	// an appropriate time.
	assert(vx_arena_blocks[vx_current_block] != 0);
	vx_block_free = next_size;
    }
    
    assert((unsigned)nbytes <= vx_block_free);

    void *this_block = vx_arena_blocks[vx_current_block] +
	(vx_first_block_size << vx_current_block) - vx_block_free;

    vx_block_free -= nbytes;

    return this_block;
}

void vx_FreeAll()
{
    for (; vx_current_block >= 0; vx_current_block--) {
	free(vx_arena_blocks[vx_current_block]);
	vx_arena_blocks[vx_current_block] = 0;
    }
    vx_block_free = -1;
}

//======================================================================
//
// Constructors
//
//======================================================================

/* Constructors -- IRConst */

IRConst* vx_IRConst_U1 ( Bool bit )
{
   IRConst* c = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag     = Ico_U1;
   c->Ico.U1  = bit;
   /* call me paranoid; I don't care :-) */
   assert(bit == False || bit == True);
   return c;
}
IRConst* vx_IRConst_U8 ( UChar u8 )
{
   IRConst* c = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag     = Ico_U8;
   c->Ico.U8  = u8;
   return c;
}
IRConst* vx_IRConst_U16 ( UShort u16 )
{
   IRConst* c = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag     = Ico_U16;
   c->Ico.U16 = u16;
   return c;
}
IRConst* vx_IRConst_U32 ( UInt u32 )
{
   IRConst* c = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag     = Ico_U32;
   c->Ico.U32 = u32;
   return c;
}
IRConst* vx_IRConst_U64 ( ULong u64 )
{
   IRConst* c = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag     = Ico_U64;
   c->Ico.U64 = u64;
   return c;
}
IRConst* vx_IRConst_F64 ( Double f64 )
{
   IRConst* c = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag     = Ico_F64;
   c->Ico.F64 = f64;
   return c;
}
IRConst* vx_IRConst_F64i ( ULong f64i )
{
   IRConst* c  = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag      = Ico_F64i;
   c->Ico.F64i = f64i;
   return c;
}
IRConst* vx_IRConst_V128 ( UShort con )
{
   IRConst* c  = (IRConst *)vx_Alloc(sizeof(IRConst));
   c->tag      = Ico_V128;
   c->Ico.V128 = con;
   return c;
}

/* Constructors -- IRCallee */

IRCallee* vx_mkIRCallee ( Int regparms, const HChar* name, void* addr )
{
   IRCallee* ce = (IRCallee *)vx_Alloc(sizeof(IRCallee));
   ce->regparms = regparms;
#if VEX_VERSION < 2555
   ce->name     = (HChar *)name;
#else
   ce->name     = name;
#endif
   ce->addr     = addr;
   ce->mcx_mask = 0;
   assert(regparms >= 0 && regparms <= 3);
   assert(name != NULL);
   assert(addr != 0);
   return ce;
}


/* Constructors -- IRRegArray */

IRRegArray* vx_mkIRRegArray ( Int base, IRType elemTy, Int nElems )
{
   IRRegArray* arr = (IRRegArray *)vx_Alloc(sizeof(IRRegArray));
   arr->base    = base;
   arr->elemTy  = elemTy;
   arr->nElems  = nElems;
   assert(!(arr->base < 0 || arr->base > 10000 /* somewhat arbitrary */));
   assert(!(arr->elemTy == Ity_I1));
   assert(!(arr->nElems <= 0 || arr->nElems > 500 /* somewhat arbitrary */));
   return arr;
}


/* Constructors -- IRExpr */

IRExpr* vx_IRExpr_Binder ( Int binder ) {
   IRExpr* e            = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag               = Iex_Binder;
   e->Iex.Binder.binder = binder;
   return e;
}
IRExpr* vx_IRExpr_Get ( Int off, IRType ty ) {
   IRExpr* e         = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Get;
   e->Iex.Get.offset = off;
   e->Iex.Get.ty     = ty;
   return e;
}
IRExpr* vx_IRExpr_GetI ( IRRegArray* descr, IRExpr* ix, Int bias ) {
   IRExpr* e         = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag            = Iex_GetI;
   e->Iex.GetI.descr = descr;
   e->Iex.GetI.ix    = ix;
   e->Iex.GetI.bias  = bias;
   return e;
}
IRExpr* vx_IRExpr_Tmp ( IRTemp tmp ) {
   IRExpr* e      = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag         = Iex_RdTmp;
   e->Iex.RdTmp.tmp = tmp;
   return e;
}
IRExpr* vx_IRExpr_Qop ( IROp op, IRExpr* arg1, IRExpr* arg2, 
                              IRExpr* arg3, IRExpr* arg4 ) {
   IRExpr* e       = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag          = Iex_Qop;
#if VEX_VERSION < 2363
   e->Iex.Qop.op   = op;
   e->Iex.Qop.arg1 = arg1;
   e->Iex.Qop.arg2 = arg2;
   e->Iex.Qop.arg3 = arg3;
   e->Iex.Qop.arg4 = arg4;
#else
   e->Iex.Qop.details       = (IRQop *)vx_Alloc(sizeof(IRQop));
   e->Iex.Qop.details->op   = op;
   e->Iex.Qop.details->arg1 = arg1;
   e->Iex.Qop.details->arg2 = arg2;
   e->Iex.Qop.details->arg3 = arg3;
   e->Iex.Qop.details->arg4 = arg4;
#endif
   return e;
}
IRExpr* vx_IRExpr_Triop  ( IROp op, IRExpr* arg1, 
                                 IRExpr* arg2, IRExpr* arg3 ) {
   IRExpr* e         = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Triop;
#if VEX_VERSION < 2366
   e->Iex.Triop.op   = op;
   e->Iex.Triop.arg1 = arg1;
   e->Iex.Triop.arg2 = arg2;
   e->Iex.Triop.arg3 = arg3;
#else
   e->Iex.Triop.details       = (IRTriop *)vx_Alloc(sizeof(IRTriop));
   e->Iex.Triop.details->op   = op;
   e->Iex.Triop.details->arg1 = arg1;
   e->Iex.Triop.details->arg2 = arg2;
   e->Iex.Triop.details->arg3 = arg3;
#endif
   return e;
}
IRExpr* vx_IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr* e         = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Binop;
   e->Iex.Binop.op   = op;
   e->Iex.Binop.arg1 = arg1;
   e->Iex.Binop.arg2 = arg2;
   return e;
}
IRExpr* vx_IRExpr_Unop ( IROp op, IRExpr* arg ) {
   IRExpr* e       = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag          = Iex_Unop;
   e->Iex.Unop.op  = op;
   e->Iex.Unop.arg = arg;
   return e;
}
IRExpr* vx_IRExpr_Load ( IREndness end, IRType ty, IRExpr* addr ) {
   IRExpr* e        = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag           = Iex_Load;
   e->Iex.Load.end  = end;
   e->Iex.Load.ty   = ty;
   e->Iex.Load.addr = addr;
   assert(end == Iend_LE || end == Iend_BE);
   return e;
}
IRExpr* vx_IRExpr_Const ( IRConst* con ) {
   IRExpr* e        = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag           = Iex_Const;
   e->Iex.Const.con = con;
   return e;
}
IRExpr* vx_IRExpr_CCall ( IRCallee* cee, IRType retty, IRExpr** args ) {
   IRExpr* e          = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag             = Iex_CCall;
   e->Iex.CCall.cee   = cee;
   e->Iex.CCall.retty = retty;
   e->Iex.CCall.args  = args;
   return e;
}
#if VEX_VERSION < 2668
IRExpr* vx_IRExpr_Mux0X ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX ) {
   IRExpr* e          = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag             = Iex_Mux0X;
   e->Iex.Mux0X.cond  = cond;
   e->Iex.Mux0X.expr0 = expr0;
   e->Iex.Mux0X.exprX = exprX;
   return e;
}
#else
IRExpr* vx_IRExpr_ITE ( IRExpr* cond, IRExpr* expr_t, IRExpr* expr_f ) {
   IRExpr* e          = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag             = Iex_ITE;
   e->Iex.ITE.cond    = cond;
   e->Iex.ITE.iftrue  = expr_t;
   e->Iex.ITE.iffalse = expr_f;
   return e;
}
#endif
#if VEX_VERSION >= 2742
IRExpr* vx_IRExpr_VECRET ( void ) {
   IRExpr* e = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag    = Iex_VECRET;
   return e;
}
IRExpr* vx_IRExpr_BBPTR ( void ) {
   IRExpr* e = (IRExpr *)vx_Alloc(sizeof(IRExpr));
   e->tag    = Iex_BBPTR;
   return e;
}
#endif


/* Constructors -- IRDirty */

IRDirty* vx_emptyIRDirty ( void ) {
   IRDirty* d = (IRDirty *)vx_Alloc(sizeof(IRDirty));
   d->cee      = NULL;
   d->guard    = NULL;
   d->args     = NULL;
   d->tmp      = IRTemp_INVALID;
   d->mFx      = Ifx_None;
   d->mAddr    = NULL;
   d->mSize    = 0;
#if VEX_VERSION < 2739
   d->needsBBP = False;
#endif
   d->nFxState = 0;
   return d;
}

#if VEX_VERSION >= 1901
/* Constructors -- IRCAS */

IRCAS* vx_emptyIRCAS ( void ) {
   IRCAS* cas = (IRCAS *)vx_Alloc(sizeof(IRCAS));
   cas->oldHi  = IRTemp_INVALID;
   cas->oldLo  = IRTemp_INVALID;
   cas->end    = Iend_LE;
   cas->addr   = NULL;
   cas->expdHi = NULL;
   cas->expdLo = NULL;
   cas->dataHi = NULL;
   cas->dataLo = NULL;
   return cas;
}
#endif

/* Constructors -- IRStmt */

IRStmt* vx_IRStmt_NoOp ( void )
{
   /* Just use a single static closure. */
   static IRStmt static_closure;
   static_closure.tag = Ist_NoOp;
   return &static_closure;
}
IRStmt* vx_IRStmt_IMark ( Addr64 addr, Int len, UChar delta ) {
   IRStmt* s          = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag             = Ist_IMark;
   s->Ist.IMark.addr  = addr;
   s->Ist.IMark.len   = len;
#if VEX_VERSION >= 2153
   s->Ist.IMark.delta = delta;
#endif
   return s;
}
IRStmt* vx_IRStmt_AbiHint ( IRExpr* base, Int len ) {
   IRStmt* s           = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag              = Ist_AbiHint;
   s->Ist.AbiHint.base = base;
   s->Ist.AbiHint.len  = len;
   return s;
}
IRStmt* vx_IRStmt_Put ( Int off, IRExpr* data ) {
   IRStmt* s         = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->Ist.Put.offset = off;
   s->Ist.Put.data   = data;
   return s;
}
IRStmt* vx_IRStmt_PutI ( IRRegArray* descr, IRExpr* ix,
                      Int bias, IRExpr* data ) {
   IRStmt* s         = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag            = Ist_PutI;
#if VEX_VERSION < 2361
   s->Ist.PutI.descr = descr;
   s->Ist.PutI.ix    = ix;
   s->Ist.PutI.bias  = bias;
   s->Ist.PutI.data  = data;
#else
   s->Ist.PutI.details        = (IRPutI *)vx_Alloc(sizeof(IRPutI));
   s->Ist.PutI.details->descr = descr;
   s->Ist.PutI.details->ix    = ix;
   s->Ist.PutI.details->bias  = bias;
   s->Ist.PutI.details->data  = data;
#endif
   return s;
}
IRStmt* vx_IRStmt_Tmp ( IRTemp tmp, IRExpr* data ) {
   IRStmt* s       = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag          = Ist_WrTmp;
   s->Ist.WrTmp.tmp  = tmp;
   s->Ist.WrTmp.data = data;
   return s;
}
IRStmt* vx_IRStmt_Store ( IREndness end, IRExpr* addr, IRExpr* data ) {
   IRStmt* s         = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag            = Ist_Store;
   s->Ist.Store.end  = end;
   s->Ist.Store.addr = addr;
   s->Ist.Store.data = data;
   assert(end == Iend_LE || end == Iend_BE);
   return s;
}
IRStmt* vx_IRStmt_Dirty ( IRDirty* d )
{
   IRStmt* s            = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag               = Ist_Dirty;
   s->Ist.Dirty.details = d;
   return s;
}
IRStmt* vx_IRStmt_MFence ( void )
{
   /* Just use a single static closure. */
   static IRStmt static_closure;
   static_closure.tag = Ist_MFence;
   return &static_closure;
}
IRStmt* vx_IRStmt_Exit ( IRExpr* guard, IRJumpKind jk, IRConst* dst ) {
   IRStmt* s         = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag            = Ist_Exit;
   s->Ist.Exit.guard = guard;
   s->Ist.Exit.jk    = jk;
   s->Ist.Exit.dst   = dst;
   return s;
}
#if VEX_VERSION >= 1901
IRStmt* vx_IRStmt_CAS ( IRCAS* details ) {
   IRStmt* s          = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag             = Ist_CAS;
   s->Ist.CAS.details = details;
   return s;
}
#endif
#if VEX_VERSION > 1930
IRStmt* vx_IRStmt_LLSC ( IREndness end, IRTemp result, IRExpr* addr,
			 IRExpr *storedata ) {
   IRStmt* s             = (IRStmt *)vx_Alloc(sizeof(IRStmt));
   s->tag                = Ist_LLSC;
   s->Ist.LLSC.end       = end;
   s->Ist.LLSC.result    = result;
   s->Ist.LLSC.addr      = addr;
   s->Ist.LLSC.storedata = storedata;
   return s;
}
#endif

/* Constructors -- IRTypeEnv */

IRTypeEnv* vx_emptyIRTypeEnv ( void )
{
   IRTypeEnv* env   = (IRTypeEnv *)vx_Alloc(sizeof(IRTypeEnv));
   env->types       = (IRType *)vx_Alloc(8 * sizeof(IRType));
   env->types_size  = 8;
   env->types_used  = 0;
   return env;
}


/* Constructors -- IRSB */

IRSB* vx_emptyIRSB ( void )
{
   IRSB* bb       = (IRSB *)vx_Alloc(sizeof(IRSB));
   bb->tyenv      = vx_emptyIRTypeEnv();
   bb->stmts_used = 0;
   bb->stmts_size = 8;
   bb->stmts      = (IRStmt **)vx_Alloc(bb->stmts_size * sizeof(IRStmt*));
   bb->next       = NULL;
   bb->jumpkind   = Ijk_Boring;
   return bb;
}


/*---------------------------------------------------------------*/
/*--- (Deep) copy constructors.  These make complete copies   ---*/
/*--- the original, which can be modified without affecting   ---*/
/*--- the original.                                           ---*/
/*---------------------------------------------------------------*/

/* Copying IR Expr vectors (for call args). */

/* Shallow copy of an IRExpr vector */

IRExpr** vx_sopyIRExprVec ( IRExpr** vec )
{
   Int      i;
   IRExpr** newvec;
   for (i = 0; vec[i]; i++)
      ;
   newvec = (IRExpr **)vx_Alloc((i+1)*sizeof(IRExpr*));
   for (i = 0; vec[i]; i++)
      newvec[i] = vec[i];
   newvec[i] = NULL;
   return newvec;
}

/* Deep copy of an IRExpr vector */

IRExpr** vx_dopyIRExprVec ( IRExpr** vec )
{
   Int      i;
   IRExpr** newvec = vx_sopyIRExprVec( vec );
   for (i = 0; newvec[i]; i++)
      newvec[i] = vx_dopyIRExpr(newvec[i]);
   return newvec;
}

/* Deep copy constructors for all heap-allocated IR types follow. */

IRConst* vx_dopyIRConst ( IRConst* c )
{
   switch (c->tag) {
      case Ico_U1:   return vx_IRConst_U1(c->Ico.U1);
      case Ico_U8:   return vx_IRConst_U8(c->Ico.U8);
      case Ico_U16:  return vx_IRConst_U16(c->Ico.U16);
      case Ico_U32:  return vx_IRConst_U32(c->Ico.U32);
      case Ico_U64:  return vx_IRConst_U64(c->Ico.U64);
      case Ico_F64:  return vx_IRConst_F64(c->Ico.F64);
      case Ico_F64i: return vx_IRConst_F64i(c->Ico.F64i);
      case Ico_V128: return vx_IRConst_V128(c->Ico.V128);
      default: vx_panic("vx_dopyIRConst");
   }

   return NULL;
}

IRCallee* vx_dopyIRCallee ( IRCallee* ce )
{
   IRCallee* ce2 = vx_mkIRCallee(ce->regparms, (const HChar *)ce->name,
				 ce->addr);
   ce2->mcx_mask = ce->mcx_mask;
   return ce2;
}

IRRegArray* vx_dopyIRRegArray ( IRRegArray* d )
{
   return vx_mkIRRegArray(d->base, d->elemTy, d->nElems);
}

IRExpr* vx_dopyIRExpr ( IRExpr* e )
{
   switch (e->tag) {
      case Iex_Get: 
         return vx_IRExpr_Get(e->Iex.Get.offset, e->Iex.Get.ty);
      case Iex_GetI: 
         return vx_IRExpr_GetI(vx_dopyIRRegArray(e->Iex.GetI.descr), 
                            vx_dopyIRExpr(e->Iex.GetI.ix),
                            e->Iex.GetI.bias);
      case Iex_RdTmp: 
        return vx_IRExpr_Tmp(e->Iex.RdTmp.tmp);
      case Iex_Qop: 
#if VEX_VERSION < 2363
	return vx_IRExpr_Qop(e->Iex.Qop.op,
                           vx_dopyIRExpr(e->Iex.Qop.arg1),
                           vx_dopyIRExpr(e->Iex.Qop.arg2),
                           vx_dopyIRExpr(e->Iex.Qop.arg3),
                           vx_dopyIRExpr(e->Iex.Qop.arg4));
#else
	return vx_IRExpr_Qop(e->Iex.Qop.details->op,
                           vx_dopyIRExpr(e->Iex.Qop.details->arg1),
                           vx_dopyIRExpr(e->Iex.Qop.details->arg2),
                           vx_dopyIRExpr(e->Iex.Qop.details->arg3),
                           vx_dopyIRExpr(e->Iex.Qop.details->arg4));
#endif
      case Iex_Triop: 
#if VEX_VERSION < 2366
        return vx_IRExpr_Triop(e->Iex.Triop.op,
                             vx_dopyIRExpr(e->Iex.Triop.arg1),
                             vx_dopyIRExpr(e->Iex.Triop.arg2),
                             vx_dopyIRExpr(e->Iex.Triop.arg3));
#else
        return vx_IRExpr_Triop(e->Iex.Triop.details->op,
                             vx_dopyIRExpr(e->Iex.Triop.details->arg1),
                             vx_dopyIRExpr(e->Iex.Triop.details->arg2),
                             vx_dopyIRExpr(e->Iex.Triop.details->arg3));
#endif
      case Iex_Binop: 
         return vx_IRExpr_Binop(e->Iex.Binop.op,
                             vx_dopyIRExpr(e->Iex.Binop.arg1),
                             vx_dopyIRExpr(e->Iex.Binop.arg2));
      case Iex_Unop: 
         return vx_IRExpr_Unop(e->Iex.Unop.op,
                            vx_dopyIRExpr(e->Iex.Unop.arg));
      case Iex_Load: 
         return vx_IRExpr_Load(e->Iex.Load.end,
                            e->Iex.Load.ty,
                            vx_dopyIRExpr(e->Iex.Load.addr));
      case Iex_Const: 
         return vx_IRExpr_Const(vx_dopyIRConst(e->Iex.Const.con));
      case Iex_CCall:
         return vx_IRExpr_CCall(vx_dopyIRCallee(e->Iex.CCall.cee),
                             e->Iex.CCall.retty,
                             vx_dopyIRExprVec(e->Iex.CCall.args));

#if VEX_VERSION < 2668
      case Iex_Mux0X: 
         return vx_IRExpr_Mux0X(vx_dopyIRExpr(e->Iex.Mux0X.cond),
                             vx_dopyIRExpr(e->Iex.Mux0X.expr0),
                             vx_dopyIRExpr(e->Iex.Mux0X.exprX));
#else
      case Iex_ITE: 
         return vx_IRExpr_ITE(vx_dopyIRExpr(e->Iex.ITE.cond),
                             vx_dopyIRExpr(e->Iex.ITE.iftrue),
                             vx_dopyIRExpr(e->Iex.ITE.iffalse));
#endif
#if VEX_VERSION >= 2742
      case Iex_VECRET:
         return vx_IRExpr_VECRET();
      case Iex_BBPTR:
         return vx_IRExpr_BBPTR();
#endif
      default:
         vx_panic("Unhandled type in vx_dopyIRExpr");
   }

   return NULL;
}

IRDirty* vx_dopyIRDirty ( IRDirty* d )
{
   Int      i;
   IRDirty* d2 = vx_emptyIRDirty();
   d2->cee   = vx_dopyIRCallee(d->cee);
   d2->guard = vx_dopyIRExpr(d->guard);
   d2->args  = vx_dopyIRExprVec(d->args);
   d2->tmp   = d->tmp;
   d2->mFx   = d->mFx;
   d2->mAddr = d->mAddr==NULL ? NULL : vx_dopyIRExpr(d->mAddr);
   d2->mSize = d->mSize;
#if VEX_VERSION < 2739
   d2->needsBBP = d->needsBBP;
#endif
   d2->nFxState = d->nFxState;
   for (i = 0; i < d2->nFxState; i++)
      d2->fxState[i] = d->fxState[i];
   return d2;
}

#if VEX_VERSION >= 1901
IRCAS* vx_dopyIRCAS ( IRCAS* cas )
{
   IRCAS* cas2 = vx_emptyIRCAS();
   cas2->oldHi  = cas->oldHi;
   cas2->oldLo  = cas->oldLo;
   cas2->end    = cas->end;
   cas2->addr   = cas->addr==NULL   ? NULL : vx_dopyIRExpr(cas->addr);
   cas2->expdHi = cas->expdHi==NULL ? NULL : vx_dopyIRExpr(cas->expdHi);
   cas2->expdLo = cas->expdLo==NULL ? NULL : vx_dopyIRExpr(cas->expdLo);
   cas2->dataHi = cas->dataHi==NULL ? NULL : vx_dopyIRExpr(cas->dataHi);
   cas2->dataLo = cas->dataLo==NULL ? NULL : vx_dopyIRExpr(cas->dataLo);
   return cas2;
}
#endif

IRStmt* vx_dopyIRStmt ( IRStmt* s )
{
   switch (s->tag) {
      case Ist_NoOp:
         return vx_IRStmt_NoOp();
      case Ist_AbiHint:
         return vx_IRStmt_AbiHint(vx_dopyIRExpr(s->Ist.AbiHint.base),
                               s->Ist.AbiHint.len);
      case Ist_IMark:
         return vx_IRStmt_IMark(s->Ist.IMark.addr, s->Ist.IMark.len,
#if VEX_VERSION >= 2153
				s->Ist.IMark.delta
#else
				0
#endif
				);
      case Ist_Put: 
         return vx_IRStmt_Put(s->Ist.Put.offset, 
                           vx_dopyIRExpr(s->Ist.Put.data));
      case Ist_PutI: 
#if VEX_VERSION < 2361
         return vx_IRStmt_PutI(vx_dopyIRRegArray(s->Ist.PutI.descr),
                            vx_dopyIRExpr(s->Ist.PutI.ix),
                            s->Ist.PutI.bias, 
                            vx_dopyIRExpr(s->Ist.PutI.data));
#else
         return vx_IRStmt_PutI(vx_dopyIRRegArray(s->Ist.PutI.details->descr),
                            vx_dopyIRExpr(s->Ist.PutI.details->ix),
                            s->Ist.PutI.details->bias, 
                            vx_dopyIRExpr(s->Ist.PutI.details->data));
#endif
      case Ist_WrTmp:
         return vx_IRStmt_Tmp(s->Ist.WrTmp.tmp,
                           vx_dopyIRExpr(s->Ist.WrTmp.data));
      case Ist_Store: 
         return vx_IRStmt_Store(s->Ist.Store.end,
                             vx_dopyIRExpr(s->Ist.Store.addr),
                             vx_dopyIRExpr(s->Ist.Store.data));
      case Ist_Dirty: 
         return vx_IRStmt_Dirty(vx_dopyIRDirty(s->Ist.Dirty.details));
      case Ist_MFence: /* AKA Ist_MBE */
         return vx_IRStmt_MFence();
      case Ist_Exit: 
         return vx_IRStmt_Exit(vx_dopyIRExpr(s->Ist.Exit.guard),
                            s->Ist.Exit.jk,
                            vx_dopyIRConst(s->Ist.Exit.dst));
#if VEX_VERSION >= 1901
      case Ist_CAS:
	 return vx_IRStmt_CAS(vx_dopyIRCAS(s->Ist.CAS.details));
#endif
#if VEX_VERSION >= 1930
      case Ist_LLSC:
	 {
	    IRExpr* addr = s->Ist.LLSC.addr;
	    IRExpr* addr2 = addr ? vx_dopyIRExpr(addr) : NULL;
	    IRExpr* storedata = s->Ist.LLSC.storedata;
	    IRExpr* storedata2 = storedata ? vx_dopyIRExpr(storedata) : NULL;
	    return vx_IRStmt_LLSC(s->Ist.LLSC.end, s->Ist.LLSC.result,
				  addr2, storedata2);
	 }
#endif
      default: 
         vx_panic("vx_dopyIRStmt");
   }

   return NULL;
}

IRTypeEnv* vx_dopyIRTypeEnv ( IRTypeEnv* src )
{
   Int        i;
   IRTypeEnv* dst = (IRTypeEnv *)vx_Alloc(sizeof(IRTypeEnv));
   dst->types_size = src->types_size;
   dst->types_used = src->types_used;
   dst->types = (IRType *)vx_Alloc(dst->types_size * sizeof(IRType));
   for (i = 0; i < src->types_used; i++)
      dst->types[i] = src->types[i];
   return dst;
}

IRSB* vx_dopyIRSB ( IRSB* bb )
{
   Int      i;
   IRStmt** sts2;
   IRSB* bb2 = vx_emptyIRSB();
   bb2->tyenv = vx_dopyIRTypeEnv(bb->tyenv);
   bb2->stmts_used = bb2->stmts_size = bb->stmts_used;
   sts2 = (IRStmt **)vx_Alloc(bb2->stmts_used * sizeof(IRStmt*));
   for (i = 0; i < bb2->stmts_used; i++)
      sts2[i] = vx_dopyIRStmt(bb->stmts[i]);
   bb2->stmts    = sts2;
   bb2->next     = vx_dopyIRExpr(bb->next);
   bb2->jumpkind = bb->jumpkind;
   return bb2;
}



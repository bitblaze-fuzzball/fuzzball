/* Because we use offsetof() for values that go in switch statement
   cases, we need a definition in terms of __builtin_offsetof so that
   GCC >= 4.6 can accept the results of offsetof() as a constant
   expression. They've stopped supporting the traditional definition
   as a macro &((type*)0)->memb, which appears in VEX's headers. */
#include <stddef.h>

#include "irtoir-internal.h"
#include "libvex_guest_arm.h"



//
// Register offsets, copied from VEX/priv/guest_arm/toIR.c
//

/*------------------------------------------------------------*/
/*--- Offsets of various parts of the arm guest state.     ---*/
/*------------------------------------------------------------*/

#define OFFB_R0       offsetof(VexGuestARMState,guest_R0)
#define OFFB_R1       offsetof(VexGuestARMState,guest_R1)
#define OFFB_R2       offsetof(VexGuestARMState,guest_R2)
#define OFFB_R3       offsetof(VexGuestARMState,guest_R3)
#define OFFB_R4       offsetof(VexGuestARMState,guest_R4)
#define OFFB_R5       offsetof(VexGuestARMState,guest_R5)
#define OFFB_R6       offsetof(VexGuestARMState,guest_R6)
#define OFFB_R7       offsetof(VexGuestARMState,guest_R7)
#define OFFB_R8       offsetof(VexGuestARMState,guest_R8)
#define OFFB_R9       offsetof(VexGuestARMState,guest_R9)
#define OFFB_R10      offsetof(VexGuestARMState,guest_R10)
#define OFFB_R11      offsetof(VexGuestARMState,guest_R11)
#define OFFB_R12      offsetof(VexGuestARMState,guest_R12)
#define OFFB_R13      offsetof(VexGuestARMState,guest_R13)
#define OFFB_R14      offsetof(VexGuestARMState,guest_R14)
#if VEX_VERSION < 2013
#define OFFB_R15      offsetof(VexGuestARMState,guest_R15)
#else
#define OFFB_R15T      offsetof(VexGuestARMState,guest_R15T)
#endif

// CAB: ? guest_SYSCALLNO;

// Replace with "#if VEX_VERSION < XXXX" if the patches ever make it
// into the VEX SVN.
#ifndef AIJ_VEX_ARM_PATCHES

#define ARM_THUNKS
#define OFFB_CC_OP    offsetof(VexGuestARMState,guest_CC_OP)
#define OFFB_CC_DEP1  offsetof(VexGuestARMState,guest_CC_DEP1)
#define OFFB_CC_DEP2  offsetof(VexGuestARMState,guest_CC_DEP2)
#if VEX_VERSION >= 1949
#define OFFB_CC_NDEP  offsetof(VexGuestARMState,guest_CC_NDEP)
#endif

#else

// This was the field name in Ivan's patched VEX.
#define OFFB_CC    offsetof(VexGuestARMState,guest_CC)

#endif

#if VEX_VERSION >= 2013
#define OFFB_QFLAG32    offsetof(VexGuestARMState,guest_QFLAG32)
#endif

#if VEX_VERSION >= 2020
#define OFFB_GEFLAG0    offsetof(VexGuestARMState,guest_GEFLAG0)
#define OFFB_GEFLAG1    offsetof(VexGuestARMState,guest_GEFLAG1)
#define OFFB_GEFLAG2    offsetof(VexGuestARMState,guest_GEFLAG2)
#define OFFB_GEFLAG3    offsetof(VexGuestARMState,guest_GEFLAG3)
#endif

#if VEX_VERSION >= 1949
#if VEX_VERSION < 2484
#define OFFB_EMWARN        offsetof(VexGuestARMState,guest_EMWARN)
#else
#define OFFB_EMWARN        offsetof(VexGuestARMState,guest_EMNOTE)
#endif

#if VEX_VERSION < 2852
#define OFFB_TISTART       offsetof(VexGuestARMState,guest_TISTART)
#define OFFB_TILEN         offsetof(VexGuestARMState,guest_TILEN)
#else
#define OFFB_TISTART       offsetof(VexGuestARMState,guest_CMSTART)
#define OFFB_TILEN         offsetof(VexGuestARMState,guest_CMLEN)
#endif

#define OFFB_NRADDR        offsetof(VexGuestARMState,guest_NRADDR)
#define OFFB_IP_AT_SYSCALL offsetof(VexGuestARMState,guest_IP_AT_SYSCALL)
#endif

#if VEX_VERSION >= 1949
#define OFFB_D0            offsetof(VexGuestARMState,guest_D0)
#define OFFB_D1            offsetof(VexGuestARMState,guest_D1)
#define OFFB_D2            offsetof(VexGuestARMState,guest_D2)
#define OFFB_D3            offsetof(VexGuestARMState,guest_D3)
#define OFFB_D4            offsetof(VexGuestARMState,guest_D4)
#define OFFB_D5            offsetof(VexGuestARMState,guest_D5)
#define OFFB_D6            offsetof(VexGuestARMState,guest_D6)
#define OFFB_D7            offsetof(VexGuestARMState,guest_D7)
#define OFFB_D8            offsetof(VexGuestARMState,guest_D8)
#define OFFB_D9            offsetof(VexGuestARMState,guest_D9)
#define OFFB_D10           offsetof(VexGuestARMState,guest_D10)
#define OFFB_D11           offsetof(VexGuestARMState,guest_D11)
#define OFFB_D12           offsetof(VexGuestARMState,guest_D12)
#define OFFB_D13           offsetof(VexGuestARMState,guest_D13)
#define OFFB_D14           offsetof(VexGuestARMState,guest_D14)
#define OFFB_D15           offsetof(VexGuestARMState,guest_D15)
#endif

#if VEX_VERSION >= 2013
#define OFFB_D16           offsetof(VexGuestARMState,guest_D16)
#define OFFB_D17           offsetof(VexGuestARMState,guest_D17)
#define OFFB_D18           offsetof(VexGuestARMState,guest_D18)
#define OFFB_D19           offsetof(VexGuestARMState,guest_D19)
#define OFFB_D20           offsetof(VexGuestARMState,guest_D20)
#define OFFB_D21           offsetof(VexGuestARMState,guest_D21)
#define OFFB_D22           offsetof(VexGuestARMState,guest_D22)
#define OFFB_D23           offsetof(VexGuestARMState,guest_D23)
#define OFFB_D24           offsetof(VexGuestARMState,guest_D24)
#define OFFB_D25           offsetof(VexGuestARMState,guest_D25)
#define OFFB_D26           offsetof(VexGuestARMState,guest_D26)
#define OFFB_D27           offsetof(VexGuestARMState,guest_D27)
#define OFFB_D28           offsetof(VexGuestARMState,guest_D28)
#define OFFB_D29           offsetof(VexGuestARMState,guest_D29)
#define OFFB_D30           offsetof(VexGuestARMState,guest_D30)
#define OFFB_D31           offsetof(VexGuestARMState,guest_D31)
#endif

#if VEX_VERSION >= 1949
#define OFFB_FPSCR        offsetof(VexGuestARMState,guest_FPSCR)
#define OFFB_TPIDRURO     offsetof(VexGuestARMState,guest_TPIDRURO)
#endif

#if VEX_VERSION >= 2013
#define OFFB_ITSTATE      offsetof(VexGuestARMState,guest_ITSTATE)
#endif

vector<VarDecl *> arm_get_reg_decls()
{
  vector<VarDecl *> ret;
  reg_t r32 = REG_32;
  reg_t r64 = REG_64;

  ret.push_back(new VarDecl("R0",      r32));
  ret.push_back(new VarDecl("R1",      r32));
  ret.push_back(new VarDecl("R2",      r32));
  ret.push_back(new VarDecl("R3",      r32));
  ret.push_back(new VarDecl("R4",      r32));
  ret.push_back(new VarDecl("R5",      r32));
  ret.push_back(new VarDecl("R6",      r32));
  ret.push_back(new VarDecl("R7",      r32));
  ret.push_back(new VarDecl("R8",      r32));
  ret.push_back(new VarDecl("R9",      r32));
  ret.push_back(new VarDecl("R10",     r32));
  ret.push_back(new VarDecl("R11",     r32));
  ret.push_back(new VarDecl("R12",     r32));
  ret.push_back(new VarDecl("R13",     r32));
  ret.push_back(new VarDecl("R14",     r32));
#if VEX_VERSION < 2013
  ret.push_back(new VarDecl("R15",     r32));
#else
  ret.push_back(new VarDecl("R15T",    r32));
#endif

  ret.push_back(new VarDecl("R_CC_OP",   r32));
  ret.push_back(new VarDecl("R_CC_DEP1", r32));
  ret.push_back(new VarDecl("R_CC_DEP2", r32));
#if VEX_VERSION >= 1949
  ret.push_back(new VarDecl("R_CC_NDEP", r32));
#endif

#if VEX_VERSION >= 2013
  ret.push_back(new VarDecl("R_QFLAG32",       r32));
#endif

#if VEX_VERSION >= 2020
  ret.push_back(new VarDecl("R_GEFLAG0",       r32));
  ret.push_back(new VarDecl("R_GEFLAG1",       r32));
  ret.push_back(new VarDecl("R_GEFLAG2",       r32));
  ret.push_back(new VarDecl("R_GEFLAG3",       r32));
#endif

#if VEX_VERSION >= 1949
  ret.push_back(new VarDecl("R_EMWARN",        r32));
  ret.push_back(new VarDecl("R_TISTART",       r32));
  ret.push_back(new VarDecl("R_TILEN",         r32));
  ret.push_back(new VarDecl("R_NRADDR",        r32));
  ret.push_back(new VarDecl("R_IP_AT_SYSCALL", r32));
#endif

#if VEX_VERSION >= 1949
  ret.push_back(new VarDecl("R_D0",  r64));
  ret.push_back(new VarDecl("R_D1",  r64));
  ret.push_back(new VarDecl("R_D2",  r64));
  ret.push_back(new VarDecl("R_D3",  r64));
  ret.push_back(new VarDecl("R_D4",  r64));
  ret.push_back(new VarDecl("R_D5",  r64));
  ret.push_back(new VarDecl("R_D6",  r64));
  ret.push_back(new VarDecl("R_D7",  r64));
  ret.push_back(new VarDecl("R_D8",  r64));
  ret.push_back(new VarDecl("R_D9",  r64));
  ret.push_back(new VarDecl("R_D10", r64));
  ret.push_back(new VarDecl("R_D11", r64));
  ret.push_back(new VarDecl("R_D12", r64));
  ret.push_back(new VarDecl("R_D13", r64));
  ret.push_back(new VarDecl("R_D14", r64));
  ret.push_back(new VarDecl("R_D15", r64));
#endif

#if VEX_VERSION >= 2013
  ret.push_back(new VarDecl("R_D16", r64));
  ret.push_back(new VarDecl("R_D17", r64));
  ret.push_back(new VarDecl("R_D18", r64));
  ret.push_back(new VarDecl("R_D19", r64));
  ret.push_back(new VarDecl("R_D20", r64));
  ret.push_back(new VarDecl("R_D21", r64));
  ret.push_back(new VarDecl("R_D22", r64));
  ret.push_back(new VarDecl("R_D23", r64));
  ret.push_back(new VarDecl("R_D24", r64));
  ret.push_back(new VarDecl("R_D25", r64));
  ret.push_back(new VarDecl("R_D26", r64));
  ret.push_back(new VarDecl("R_D27", r64));
  ret.push_back(new VarDecl("R_D28", r64));
  ret.push_back(new VarDecl("R_D29", r64));
  ret.push_back(new VarDecl("R_D30", r64));
  ret.push_back(new VarDecl("R_D31", r64));
#endif

#if VEX_VERSION >= 1949
  ret.push_back(new VarDecl("R_FPSCR",         r32));
  ret.push_back(new VarDecl("R_TPIDRURO",      r32));
#endif

#if VEX_VERSION >= 2013
  ret.push_back(new VarDecl("R_ITSTATE",       r32));
#endif

  /* These flags don't exist in VEX, because it represents them
     lazily, but we make their computation explicit at the time of the
     relevant operation. The ARM documentation just calls these the "N
     bit", etc., but we use x86-style names like "NF" to be a bit more
     explicit. */
  ret.push_back(new VarDecl("R_NF", REG_1)); /* like x86 R_SF */
  ret.push_back(new VarDecl("R_ZF", REG_1)); /* like x86 R_ZF */
  ret.push_back(new VarDecl("R_CF", REG_1)); /* like x86 R_CF */
  ret.push_back(new VarDecl("R_VF", REG_1)); /* like x86 R_OF */

  return ret;
}

IRStmt* arm_make_pc_put_stmt(Addr64 addr) {
#if VEX_VERSION < 2013
  return IRStmt_Put(OFFB_R15, IRExpr_Const(IRConst_U32((UInt)addr)));
#else
  // Not sure if we should munge this value somehow to account for
  // the T encoding.
  return IRStmt_Put(OFFB_R15T, IRExpr_Const(IRConst_U32((UInt)addr)));
#endif
}

/* Figure out what guest state register, if any, contains the given
   guest state offset. On a match, returns 1 and returns the register
   name, the register size in bits, and the offset within the register
   in bytes via *name, *size, and *inner. If no match, returns 0 with
   *name, *size, and *inner unchanged. */
#define REG32(name_, off_) \
    case (off_): case (off_) + 1: case (off_) + 2: case (off_) + 3: \
    the_name = name_; the_inner = offset - off_; the_size = 32; break;
#define REG64(name_, off_) \
    case (off_):     case (off_) + 1: case (off_) + 2: case (off_) + 3: \
    case (off_) + 4: case (off_) + 5: case (off_) + 6: case (off_) + 7: \
    the_name = name_; the_inner = offset - off_; the_size = 64; break;
static int lookup_regoffset(int offset, const char **name,
			    int *size, int *inner) {
    int the_size, the_inner;
    const char *the_name = 0;
    switch (offset) {
	REG32("R0",  OFFB_R0);
	REG32("R1",  OFFB_R1);
	REG32("R2",  OFFB_R2);
	REG32("R3",  OFFB_R3);
	REG32("R4",  OFFB_R4);
	REG32("R5",  OFFB_R5);
	REG32("R6",  OFFB_R6);
	REG32("R7",  OFFB_R7);
	REG32("R8",  OFFB_R8);
	REG32("R9",  OFFB_R9);
	REG32("R10", OFFB_R10);
	REG32("R11", OFFB_R11);
	REG32("R12", OFFB_R12);
	REG32("R13", OFFB_R13);
	REG32("R14", OFFB_R14);

#if VEX_VERSION < 2013
	REG32("R15", OFFB_R15);
#else
	REG32("R15T", OFFB_R15T);
#endif

#ifdef ARM_THUNKS
	REG32("R_CC_OP",   OFFB_CC_OP);
	REG32("R_CC_DEP1", OFFB_CC_DEP1);
	REG32("R_CC_DEP2", OFFB_CC_DEP2);
#if VEX_VERSION >= 1949
	REG32("R_CC_NDEP", OFFB_CC_NDEP);
#endif
#else
	REG32("R_CC",   OFFB_CC);
#endif

#if VEX_VERSION >= 2013
	REG32("R_QFLAG32",       OFFB_QFLAG32);
#endif

#if VEX_VERSION >= 2020
	REG32("R_GEFLAG0",       OFFB_GEFLAG0);
	REG32("R_GEFLAG1",       OFFB_GEFLAG1);
	REG32("R_GEFLAG2",       OFFB_GEFLAG2);
	REG32("R_GEFLAG3",       OFFB_GEFLAG3);
#endif

#if VEX_VERSION >= 1949
	REG32("R_EMWARN",        OFFB_EMWARN);
	REG32("R_TISTART",       OFFB_TISTART);
	REG32("R_TILEN",         OFFB_TILEN);
	REG32("R_NRADDR",        OFFB_NRADDR);
	REG32("R_IP_AT_SYSCALL", OFFB_IP_AT_SYSCALL); 
	REG32("R_TPIDRURO",      OFFB_TPIDRURO);
	REG32("R_FPSCR",         OFFB_FPSCR);
#endif

#if VEX_VERSION >= 2013
	REG32("R_ITSTATE",       OFFB_ITSTATE);
#endif

#if VEX_VERSION >= 1949
	REG64("R_D0",  OFFB_D0);
	REG64("R_D1",  OFFB_D1);
	REG64("R_D2",  OFFB_D2);
	REG64("R_D3",  OFFB_D3);
	REG64("R_D4",  OFFB_D4);
	REG64("R_D5",  OFFB_D5);
	REG64("R_D6",  OFFB_D6);
	REG64("R_D7",  OFFB_D7);
	REG64("R_D8",  OFFB_D8);
	REG64("R_D9",  OFFB_D9);
	REG64("R_D10", OFFB_D10);
	REG64("R_D11", OFFB_D11);
	REG64("R_D12", OFFB_D12);
	REG64("R_D13", OFFB_D13);
	REG64("R_D14", OFFB_D14);
	REG64("R_D15", OFFB_D15);
#endif
#if VEX_VERSION >= 2013
	REG64("R_D16", OFFB_D16);
	REG64("R_D17", OFFB_D17);
	REG64("R_D18", OFFB_D18);
	REG64("R_D19", OFFB_D19);
	REG64("R_D20", OFFB_D20);
	REG64("R_D21", OFFB_D21);
	REG64("R_D22", OFFB_D22);
	REG64("R_D23", OFFB_D23);
	REG64("R_D24", OFFB_D24);
	REG64("R_D25", OFFB_D25);
	REG64("R_D26", OFFB_D26);
	REG64("R_D27", OFFB_D27);
	REG64("R_D28", OFFB_D28);
	REG64("R_D29", OFFB_D29);
	REG64("R_D30", OFFB_D30);
	REG64("R_D31", OFFB_D31);
#endif
    default:
	the_name = 0;
	break;
    }

    if (the_name) {
	*name = the_name;
	*size = the_size;
	*inner = the_inner;
	return 1;
    } else {
	return 0;
    }
}
#undef REG32
#undef REG64

static inline Temp *mk_reg( string name, reg_t width )
{
    return new Temp(width, name);
}

 
static Exp *translate_get_reg_32( int offset )
{
    assert(offset >= 0);
    const char *name;
    int size, inner;
    Exp *result;
    
    if (lookup_regoffset(offset, &name, &size, &inner)) {
	if (size == 32 && inner == 0) {
	    result = mk_reg(name, REG_32);
	} else if (size == 64 && inner == 0) {
	    /* XXX endianness? */
	    result = _ex_l_cast(mk_reg(name, REG_64), REG_32);
	} else if (size == 64 && inner == 4) {
	    /* XXX endianness? */
	    result = _ex_h_cast(mk_reg(name, REG_64), REG_32);
	} else {
	    panic("translate_get_reg_32: unsupported size/inner");
	}
    } else {
	fprintf(stderr, "bad offset: %d\n", offset);
	panic("translate_get_reg_32: unrecognized offset");
    }
    
    return result;
}

static Exp *translate_get_reg_64( int offset )
{
    assert(offset >= 0);
    const char *name;
    int size, inner;
    Temp *reg;
    
    if (lookup_regoffset(offset, &name, &size, &inner)) {
	if (size == 64 && inner == 0) {
	    reg = mk_reg(name, REG_64);
	} else {
	    panic("translate_get_reg_64: unsupported size/inner");
	}
    } else {
	panic("translate_get_reg_64: unrecognized offset");
    }
    
    return reg;
}

Exp  *arm_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
  int offset = expr->Iex.Get.offset;
  IRType ty = typeOfIRExpr(irbb->tyenv, expr);

  if (ty == Ity_I32 ||ty == Ity_F32)
      return translate_get_reg_32(offset);
  else if (ty == Ity_I64 || ty == Ity_F64)
      return translate_get_reg_64(offset);
  else if (ty == Ity_V128)
      return new Unknown("register type (V128)");
  else
      panic("arm_translate_get: unexpected register size");
}


static Stmt *translate_put_reg_32( int offset, Exp *data, IRSB *irbb )
{
    assert(data);
    const char *name;
    int size, inner;
    Temp *reg;
    Exp *rhs;
    
    if (lookup_regoffset(offset, &name, &size, &inner)) {
	if (size == 32 && inner == 0) {
	    reg = mk_reg(name, REG_32);
	    rhs = data;
	} else if (size == 64 && inner == 0) {
	    /* XXX endianness? */
	    reg = mk_reg(name, REG_64);
	    Exp *masked = _ex_and(new Temp(*reg),
				  ex_const(REG_64, 0xffffffff00000000ULL));
	    Exp *widened = _ex_u_cast(data, REG_64);
	    rhs = new BinOp(BITOR, masked, widened);
	} else if (size == 64 && inner == 4) {
	    /* XXX endianness? */
	    reg = mk_reg(name, REG_64);
	    Exp *masked = _ex_and(new Temp(*reg),
				  ex_const(REG_64, 0x00000000ffffffffULL));
	    Exp *shifted = _ex_shl(_ex_u_cast(data, REG_64), ex_const(32));
	    rhs = _ex_or(masked, shifted);
	} else {
	    panic("translate_put_reg_32: unsupported size/inner");
	}
    } else {
	panic("translate_put_reg_32: unrecognized offset");
    }
    
    return new Move( reg, rhs );
}

static Stmt *translate_put_reg_64( int offset, Exp *data, IRSB *irbb )
{
    assert(data);
    const char *name;
    int size, inner;
    Temp *reg;
    Exp *rhs;
    
    if (lookup_regoffset(offset, &name, &size, &inner)) {
	if (size == 64 && inner == 0) {
	    reg = mk_reg(name, REG_64);
	    rhs = data;
	} else {
	    panic("translate_put_reg_64: unsupported size/inner");
	}
    } else {
	panic("translate_put_reg_64: unrecognized offset");
    }
        
    return new Move( reg, rhs );
}

Stmt *arm_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
  int offset = stmt->Ist.Put.offset;
  IRType ty = typeOfIRExpr(irbb->tyenv, stmt->Ist.Put.data);

  Exp *data = translate_expr(stmt->Ist.Put.data, irbb, irout);

  if (ty == Ity_I32 || ty == Ity_F32)
      return translate_put_reg_32(offset, data, irbb);
  else if (ty == Ity_I64 || ty == Ity_F64)
      return translate_put_reg_64(offset, data, irbb);
  else if (ty == Ity_V128) {
      Exp::destroy(data);
      return new Special("Unsupported register type (V128)");
  } else
      panic("arm_translate_put: unexpected register size");
}

/* This definition is copied from VEX's priv/guest_arm_defs.h, but
   since it matches the architectural encoding, we can hope it's not
   likely to change. */
enum {
    ARMCondEQ     = 0,  /* equal                         : Z=1 */
    ARMCondNE     = 1,  /* not equal                     : Z=0 */
    
    ARMCondHS     = 2,  /* >=u (higher or same)          : C=1 */
    ARMCondLO     = 3,  /* <u  (lower)                   : C=0 */

    ARMCondMI     = 4,  /* minus (negative)              : N=1 */
    ARMCondPL     = 5,  /* plus (zero or +ve)            : N=0 */

    ARMCondVS     = 6,  /* overflow                      : V=1 */
    ARMCondVC     = 7,  /* no overflow                   : V=0 */

    ARMCondHI     = 8,  /* >u   (higher)                 : C=1 && Z=0 */
    ARMCondLS     = 9,  /* <=u  (lower or same)          : C=0 || Z=1 */

    ARMCondGE     = 10, /* >=s (signed greater or equal) : N=V */
    ARMCondLT     = 11, /* <s  (signed less than)        : N!=V */

    ARMCondGT     = 12, /* >s  (signed greater)          : Z=0 && N=V */
    ARMCondLE     = 13, /* <=s (signed less or equal)    : Z=1 || N!=V */

    ARMCondAL     = 14, /* always (unconditional)        : 1 */
    ARMCondNV     = 15  /* never (unconditional):        : 0 */
};

#define CC_SHIFT_N  31
#define CC_SHIFT_Z  30
#define CC_SHIFT_C  29
#define CC_SHIFT_V  28
#define CC_SHIFT_Q  27

#define CC_MASK_N    (1 << ARMG_CC_SHIFT_N)
#define CC_MASK_Z    (1 << ARMG_CC_SHIFT_Z)
#define CC_MASK_C    (1 << ARMG_CC_SHIFT_C)
#define CC_MASK_V    (1 << ARMG_CC_SHIFT_V)
#define CC_MASK_Q    (1 << ARMG_CC_SHIFT_Q)

// If "e" is a VEX temporary, replace it with its definition by
// seaching the basic block. Because of the VEX IR's SSA property, we
// don't actually care where the definition is.
static IRExpr *expand_temp(IRExpr *e, IRSB *irbb) {
    while (e->tag == Iex_RdTmp) {
	IRTemp temp = e->Iex.RdTmp.tmp;
	IRExpr *defn = 0;
	for (int i = 0; i < irbb->stmts_used; i++) {
	    IRStmt *st = irbb->stmts[i];
	    if (st->tag == Ist_WrTmp) {
		if (temp == st->Ist.WrTmp.tmp) {
		    defn = st->Ist.WrTmp.data;
		    break;
		}
	    }
	}
	if (defn) {
	    e = defn;
	} else {
	    panic("Can't find temp definition in expand_temp");
	}
    }
    return e;
}

// Build the IR expression corresponding to an ARM condition code.
static Exp *arm_cond_formula(int cond) {
    Exp *result;
    switch (cond) {
    case ARMCondEQ: result =         new Temp(REG_1, "R_ZF");  break;
    case ARMCondNE: result = _ex_not(new Temp(REG_1, "R_ZF")); break;
    case ARMCondHS: result =         new Temp(REG_1, "R_CF");  break;
    case ARMCondLO: result = _ex_not(new Temp(REG_1, "R_CF")); break;
    case ARMCondMI: result =         new Temp(REG_1, "R_NF");  break;
    case ARMCondPL: result = _ex_not(new Temp(REG_1, "R_NF")); break;
    case ARMCondVS: result =         new Temp(REG_1, "R_VF");  break;
    case ARMCondVC: result = _ex_not(new Temp(REG_1, "R_VF")); break;
    case ARMCondHI:
	result = _ex_and(new Temp(REG_1, "R_CF"),
			 _ex_not(new Temp(REG_1, "R_ZF")));
	break;
    case ARMCondLS:
	result = _ex_or(_ex_not(new Temp(REG_1, "R_CF")),
			new Temp(REG_1, "R_ZF"));
	break;
    case ARMCondGE:
	result = _ex_eq(new Temp(REG_1, "R_NF"), new Temp(REG_1, "R_VF"));
	break;
    case ARMCondLT:
	result = _ex_xor(new Temp(REG_1, "R_NF"), new Temp(REG_1, "R_VF"));
	break;
    case ARMCondGT:
	result = _ex_and(_ex_not(new Temp(REG_1, "R_ZF")),
			 _ex_eq(new Temp(REG_1, "R_NF"),
				new Temp(REG_1, "R_VF")));
	break;
    case ARMCondLE:
	result = _ex_or(new Temp(REG_1, "R_ZF"),
			_ex_xor(new Temp(REG_1, "R_NF"),
				new Temp(REG_1, "R_VF")));
	break;
    case ARMCondAL: result = ex_const(REG_1, 1); break;
    case ARMCondNV: result = ex_const(REG_1, 0); break;
    default:
	panic("Unrecognized condition for armg_calculate_condition");
    }
    return result;
}

Exp  *arm_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    Exp *result = NULL;
    const char *func = expr->Iex.CCall.cee->name;

    if (!strcmp(func, "armg_calculate_flag_c")) {
	result = _ex_u_cast(new Temp(REG_1, "R_CF"), REG_32);
    } else if (!strcmp(func, "armg_calculate_flag_v")) {
	result = _ex_u_cast(new Temp(REG_1, "R_VF"), REG_32);
    } else if (!strcmp(func, "armg_calculate_flags_nzcv")) {
	Exp *wNF = _ex_u_cast(new Temp(REG_1, "R_NF"), REG_32);
	Exp *wZF = _ex_u_cast(new Temp(REG_1, "R_ZF"), REG_32);
	Exp *wCF = _ex_u_cast(new Temp(REG_1, "R_CF"), REG_32);
	Exp *wVF = _ex_u_cast(new Temp(REG_1, "R_VF"), REG_32);

	Exp *sNF = _ex_shl(wNF, ex_const(REG_32, CC_SHIFT_N));
	Exp *sZF = _ex_shl(wZF, ex_const(REG_32, CC_SHIFT_Z));
	Exp *sCF = _ex_shl(wCF, ex_const(REG_32, CC_SHIFT_C));
	Exp *sVF = _ex_shl(wVF, ex_const(REG_32, CC_SHIFT_V));

	result = _ex_or(sNF, sZF, sCF, sVF);
    } else if (!strcmp(func, "armg_calculate_condition")) {
	IRExpr *arg0 = expand_temp(expr->Iex.CCall.args[0], irbb);
	if (arg0->tag == Iex_Binop) {
	    IRExpr *left = expand_temp(arg0->Iex.Binop.arg1, irbb);
	    IRExpr *right = expand_temp(arg0->Iex.Binop.arg2, irbb);
	    IROp op = arg0->Iex.Binop.op;
	    if (op == Iop_Or32 &&
		left->tag == Iex_Get && left->Iex.Get.offset == OFFB_CC_OP) {
		if (right->tag == Iex_Const) {
		    assert(right->Iex.Const.con->tag == Ico_U32);
		    int cval = right->Iex.Const.con->Ico.U32;
		    if ((cval & 15) == 0 &&
			(cval >> 4) >= 0 && (cval >> 4) < 16) {
			// t5 = Or32(CC_OP,0x50:I32) or similar.
			// Assume that CC_OP is giving the low 4 bits, but the
			// 0x50 constant is all we care about.
			arg0 = right;
		    }
		} else if (right->tag == Iex_Binop &&
			   right->Iex.Binop.op == Iop_Xor32) {
		    IRExpr *rr = right->Iex.Binop.arg2;
		    assert(rr->tag == Iex_Const);
		    assert(rr->Iex.Const.con->tag == Ico_U32);
		    assert(rr->Iex.Const.con->Ico.U32 == 0xE0);
		    // Xor32(t12,0xE0:I32), where t12 is the condition in
		    // Vex's "xor 0xE" encoding.
		    arg0 = arg0->Iex.Binop.arg2;
		} else {
		    panic("Unsupported cond. (1) in armg_calculate_condition");
		}
	    } else {
		panic("Unsupported cond. (2) in armg_calculate_condition");
	    }
	}
	if (arg0->tag == Iex_Const) {
	    assert(arg0->Iex.Const.con->tag == Ico_U32);
	    int cond_n_op = arg0->Iex.Const.con->Ico.U32;
	    result = arm_cond_formula(cond_n_op >> 4);
	} else if (arg0->tag == Iex_Get &&
		   arg0->Iex.Get.offset == OFFB_CC_OP) {
	    // Assume that the Get(CC_OP) is supplying the low 4 bits,
	    // so the next 4 bits were 0 (and optimized away)
	    result = arm_cond_formula(0);
	} else {
	    // Non-constant case. We need to create IR code to do the shift,
	    // and then interpret the condition.
	    Exp *cond_0 = translate_expr(arg0, irbb, irout);
	    Exp *cond_exp = _ex_shr(cond_0, ex_const(REG_32, 4));
	    Temp *cond_temp = mk_temp(REG_32, irout);
	    irout->push_back(new Move(cond_temp, cond_exp));
	    Exp *flip_bit = ex_l_cast(cond_temp, REG_1);
	    Exp *sel0_exp = ex_get_bit(cond_temp, 1);
	    Temp *sel0_temp = mk_temp(REG_1, irout);
	    irout->push_back(new Move(sel0_temp, sel0_exp));
	    Exp *sel1_exp = ex_get_bit(cond_temp, 2);
	    Temp *sel1_temp = mk_temp(REG_1, irout);
	    irout->push_back(new Move(sel1_temp, sel1_exp));
	    Exp *sel2_exp = ex_get_bit(cond_temp, 3);
	    // Of the 16 conditions, use the fact that the odd-numbered
	    // ones are the inverses of the preceding even-numbered ones
	    // so that we only need an 8-way selection.
	    Exp *cases[8];
	    for (int i = 0; i < 8; i++)
		cases[i] = arm_cond_formula(2*i);
	    Exp *choice01 = emit_ite(irout, REG_1, ecl(sel0_temp),
				     cases[1], cases[0]);
	    Exp *choice23 = emit_ite(irout, REG_1, ecl(sel0_temp),
				     cases[3], cases[2]);
	    Exp *choice45 = emit_ite(irout, REG_1, ecl(sel0_temp),
				     cases[5], cases[4]);
	    Exp *choice67 = emit_ite(irout, REG_1, ecl(sel0_temp),
				     cases[7], cases[6]);
	    Exp *choice03 = emit_ite(irout, REG_1, ecl(sel1_temp),
				     choice23, choice01);
	    Exp *choice47 = emit_ite(irout, REG_1, ecl(sel1_temp),
				     choice67, choice45);
	    Exp *choice = emit_ite(irout, REG_1, sel2_exp,
				   choice47, choice03);
	    result = _ex_xor(choice, flip_bit);
	}
	result = _ex_u_cast(result, REG_32);
    } else {
	result = new Unknown("CCall: " + string(func));
    }
    return result;
}

/* These values need to match VEX's priv/guest_arm_defs.h */
enum {
   ARMG_CC_OP_COPY=0,
   ARMG_CC_OP_ADD,
   ARMG_CC_OP_SUB,
   ARMG_CC_OP_ADC,
   ARMG_CC_OP_SBB,
   ARMG_CC_OP_LOGIC,
   ARMG_CC_OP_MUL,
   ARMG_CC_OP_MULL
};

// Parse our statement vector to see if we can find how VEX is setting
// the "thunk" that lazily represents flags operations. The thunk consists
// of 4 pseudo-registers CC_{OP,DEP1,DEP2,NDEP}, so basically we look for
// assignments to these, and return their statment indexes via the first
// four int * arguments. (An index of "-1" means "not present".)
// An additional complication is that the updates to the thunk may be
// conditional (say for a shift with a non-constant amount), or
// effectively-NOP'ed (say for a shift by a constant 0), and we have to
// recognize what this looks like after VEX's simplifications and our
// translation of VEX's "mux0x" conditionals. These cases are signified by
// also setting one of the remaining int * arguments.
// This is a clone of code from irtoir-i386.c. Once it's stable we may
// wish to reunify them.
static void
get_thunk_index(vector<Stmt *> *ir,
		int *op,
		int *dep1,
		int *dep2,
		int *ndep,
		int *mux0x,
		int *nop)
{
    assert(ir);

    unsigned int i;

    Temp *cc_op_copy = 0;

    *op = -1;

    for ( i = 0; i < ir->size(); i++ )
    {
        Stmt *stmt = ir->at(i);
        if ( stmt->stmt_type != MOVE
	     || ((Move*)stmt)->lhs->exp_type != TEMP )
	    continue;

	Move *mv = (Move*)stmt;
	Temp *temp = (Temp *)mv->lhs;
	if (temp->name == "R_CC_OP") {
	    *op = i;
	    /* The statement offsets -1. -2, -3, and -6 here have to do with
	       matching the output of emit_ite. */
#ifdef MUX_AS_CJMP
	    if (match_ite(ir, i-6, NULL, NULL, NULL, NULL) >= 0)
		*mux0x = i-6;
#else
	    Exp *cond, *exp_t, *exp_f, *res;
	    if (match_ite(ir, i-3, NULL, NULL, NULL, NULL) >= 0) {
		if (i >= 3) {
		    Stmt *prev_stmt = ir->at(i - 1);
		    if ( prev_stmt->stmt_type == MOVE ) {
			Move *prev_mv = (Move*)prev_stmt;
			if ( prev_mv->lhs->exp_type == TEMP ) {
			    Temp *prev_temp = (Temp *)prev_mv->lhs;
			    if ( mv->rhs->exp_type == TEMP ) {
				Temp *rhs_temp = (Temp *)mv->rhs;
				if ( prev_temp->name == rhs_temp->name ) {
				    *op = i-2;
				    *mux0x = i-3;
				}
			    }
			}
		    }
		}
	    } else if (i >= 2 &&
		       match_ite(ir, i-2, &cond, &exp_t, &exp_f, &res) >= 0) {
                // i-2: res = (cond ? exp_t : exp_f);
		// i-1: t = res;
		// i  : R_CC_OP = t;
		Stmt *prev_stmt = ir->at(i - 1);
		if ( prev_stmt->stmt_type == MOVE ) {
		    Move *prev_mv = (Move*)prev_stmt;
		    if ( prev_mv->lhs->exp_type == TEMP ) {
			Temp *prev_temp = (Temp *)prev_mv->lhs;
			if ( mv->rhs->exp_type == TEMP ) {
			    Temp *rhs_temp = (Temp *)mv->rhs;
			    if ( prev_temp->name == rhs_temp->name ) {
				*op = i-2;
				*mux0x = i-2;
			    }
			}
		    }
		}

            } else if (match_ite(ir, i, NULL, NULL, NULL, NULL) >= 0) {
                *op = i;
	    } else if (cc_op_copy && mv->rhs->exp_type == TEMP) {
		Temp *rhs_temp = (Temp *)mv->rhs;
		if (rhs_temp->name == cc_op_copy->name) {
		    // We saw t = CC_OP; ...; CC_OP = t, so the thunk
		    // is actually a no-op.
		    *nop = i;
		}
	    }
#endif
	}
	else if (temp->name == "R_CC_DEP1")
	    *dep1 = i;
	else if (temp->name == "R_CC_DEP2")
	    *dep2 = i;
	else if (temp->name == "R_CC_NDEP")
	    *ndep = i;
	else if (mv->rhs->exp_type == TEMP) {
	    Temp *rhs_temp = (Temp *)mv->rhs;
	    if (rhs_temp->name == "R_CC_OP") {
		cc_op_copy = temp;
	    }
	}
    }
}

void arm_modify_flags(asm_program_t *prog, vine_block_t *block)
{
    int op_st = -1, dep1_st = -1, dep2_st = -1, ndep_st = -1,
	mux0x_st = -1, nop_st = -1;
    vector<Stmt *> *ir = block->vine_ir;
    get_thunk_index(ir, &op_st, &dep1_st, &dep2_st, &ndep_st,
		    &mux0x_st, &nop_st);

    if (op_st == -1)
	// doesn't set flags
	return;

    Stmt *op_stmt = ir->at(op_st);

    bool got_op;
    int op = -1;
    if(!(op_stmt->stmt_type == MOVE)) {
	got_op = false;
    } else {
	Move *op_mov = (Move*)op_stmt;
	Exp *cond, *exp_t, *exp_f, *res;
	if(op_mov->rhs->exp_type == CONSTANT) {
	    Constant *op_const = (Constant*)op_mov->rhs;
	    op = op_const->val;
	    got_op = true;
	} else if (match_ite(ir, op_st, &cond, &exp_t, &exp_f, &res) >= 0) {
	    if (exp_t->exp_type == CONSTANT) {
                Constant *op_const = (Constant*)exp_t;
                op = op_const->val;
                got_op = true;
            } else {
                got_op = false;
            }
	} else if (op_mov->rhs->exp_type == BINOP) {
	    BinOp *bin_or = (BinOp*)op_mov->rhs;
	    if (bin_or->binop_type == BITOR
		&& bin_or->rhs->exp_type == BINOP) {
		BinOp *bin_and = (BinOp*)bin_or->rhs;
		if (bin_and->binop_type == BITAND
		    && bin_and->lhs->exp_type == CONSTANT) {
		    Constant *op_const = (Constant*)bin_and->lhs;
		    op = op_const->val;
		    got_op = true;
		} else {
		    got_op = false;
		}
	    } else {
		got_op = false;
	    }
	} else {
	    got_op = false;
	}
    }

    if (0 && op_st != -1) {
	for (unsigned int i = 0; i < ir->size(); i++) {
	    printf("  %2d: %s\n", i, (*ir)[i]->tostring().c_str());
	}
	printf("Found thunk at op:%d dep1:%d dep2:%d ndep:%d mux:%d nop:%d\n",
	       op_st, dep1_st, dep2_st, ndep_st, mux0x_st, nop_st);
    } 

    if (nop_st != -1) {
	return;
    }

    assert(got_op);

    Exp *dep1_expr, *dep2_expr, *ndep_expr; 
    assert(dep1_st != -1);
    assert(dep2_st != -1);
    assert(ndep_st != -1);
    if (mux0x_st == -1) {
	// Unconditional case
	assert(ir->at(dep1_st)->stmt_type == MOVE);
	dep1_expr = ((Move *)(ir->at(dep1_st)))->rhs;

	assert(ir->at(dep2_st)->stmt_type == MOVE);
	dep2_expr = ((Move *)(ir->at(dep2_st)))->rhs;

	assert(ir->at(ndep_st)->stmt_type == MOVE);
	ndep_expr = ((Move *)(ir->at(ndep_st)))->rhs;
    } else {
	// Conditional case
	Exp *cond, *exp_t, *exp_f, *res;
	int matched;
	matched = match_ite(ir, dep1_st - 2, &cond, &exp_t, &exp_f, &res);
	assert(matched != -1);
	assert(exp_f);
	dep1_expr = exp_f;

	matched = match_ite(ir, dep2_st - 2, &cond, &exp_t, &exp_f, &res);
	assert(matched != -1);
	assert(exp_f);
	dep2_expr = exp_f;

	matched = match_ite(ir, ndep_st - 2, &cond, &exp_t, &exp_f, &res);
	assert(matched != -1);
	assert(exp_f);
	ndep_expr = exp_f;
    }

    Temp *NF = new Temp(REG_1, "R_NF");
    Temp *ZF = new Temp(REG_1, "R_ZF");
    Temp *CF = new Temp(REG_1, "R_CF");
    Temp *VF = new Temp(REG_1, "R_VF");

    vector<Stmt *> new_ir;
    Exp *nf, *zf, *cf, *vf;

    switch (op) {
    case ARMG_CC_OP_COPY:
	nf = ex_get_bit(dep1_expr, CC_SHIFT_N);
	zf = ex_get_bit(dep1_expr, CC_SHIFT_Z);
	cf = ex_get_bit(dep1_expr, CC_SHIFT_C);
	vf = ex_get_bit(dep1_expr, CC_SHIFT_V);
	break;
    case ARMG_CC_OP_ADD: {
	Temp *result = mk_temp(REG_32, &new_ir);
	new_ir.push_back(new Move(result, ex_add(dep1_expr, dep2_expr)));
	Temp *sign_bit = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(sign_bit, ex_h_cast(result, REG_1)));
	nf = ecl(sign_bit);
	zf = _ex_eq(ecl(result), ex_const(REG_32, 0));
	cf = ex_lt(result, dep1_expr);
	vf = _ex_and(_ex_xor(ecl(sign_bit), ex_h_cast(dep1_expr, REG_1)),
		     _ex_xor(ecl(sign_bit), ex_h_cast(dep2_expr, REG_1)));
	break;   }
    case ARMG_CC_OP_SUB: {
	Temp *result = mk_temp(REG_32, &new_ir);
	new_ir.push_back(new Move(result, ex_sub(dep1_expr, dep2_expr)));
	Temp *sign_bit = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(sign_bit, ex_h_cast(result, REG_1)));
	nf = ecl(sign_bit);
	zf = _ex_eq(ecl(result), ex_const(REG_32, 0));
	cf = ex_ge(dep1_expr, dep2_expr);
	vf = _ex_and(_ex_xor(ex_h_cast(dep1_expr, REG_1),
			     ex_h_cast(dep2_expr, REG_1)),
		     _ex_xor(ecl(sign_bit), ex_h_cast(dep1_expr, REG_1)));
	break;   }
    case ARMG_CC_OP_ADC: {
	Temp *result = mk_temp(REG_32, &new_ir);	
	new_ir.push_back(new Move(result,
				  _ex_add(ex_add(dep1_expr, dep2_expr),
					  ecl(ndep_expr))));
	Temp *sign_bit = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(sign_bit, ex_h_cast(result, REG_1)));
	nf = ecl(sign_bit);
	zf = _ex_eq(ecl(result), ex_const(REG_32, 0));
	cf = emit_ite(&new_ir, REG_1, ex_l_cast(ndep_expr, REG_1),
		      ex_le(result, dep1_expr),
		      ex_lt(result, dep1_expr));
	vf = _ex_and(_ex_xor(ecl(sign_bit), ex_h_cast(dep1_expr, REG_1)),
		     _ex_xor(ecl(sign_bit), ex_h_cast(dep2_expr, REG_1)));
	break;   }
    case ARMG_CC_OP_SBB:  {
	Temp *result = mk_temp(REG_32, &new_ir);	
	new_ir.push_back(new Move(result,
				  _ex_sub(ex_sub(dep1_expr, dep2_expr),
					  _ex_xor(ecl(ndep_expr),
						  ex_const(REG_32, 1)))));
	Temp *sign_bit = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(sign_bit, ex_h_cast(result, REG_1)));
	nf = ecl(sign_bit);
	zf = _ex_eq(ecl(result), ex_const(REG_32, 0));
	cf = emit_ite(&new_ir, REG_1, ex_l_cast(ndep_expr, REG_1),
		      ex_ge(dep1_expr, dep2_expr),
		      ex_gt(dep1_expr, dep2_expr));
	vf = _ex_and(_ex_xor(ex_h_cast(dep1_expr, REG_1),
			     ex_h_cast(dep2_expr, REG_1)),
		     _ex_xor(ecl(sign_bit), ex_h_cast(dep1_expr, REG_1)));
	break;   }
    case ARMG_CC_OP_LOGIC:
	nf = ex_h_cast(dep1_expr, REG_1);
	zf = _ex_eq(ecl(dep1_expr), ex_const(REG_32, 0));
	cf = ex_l_cast(dep2_expr, REG_1);
	vf = ex_l_cast(ndep_expr, REG_1);
	break;
    case ARMG_CC_OP_MUL:
	nf = ex_h_cast(dep1_expr, REG_1);
	zf = _ex_eq(ecl(dep1_expr), ex_const(REG_32, 0));
	cf = ex_get_bit(ndep_expr, 1);
	vf = ex_l_cast(ndep_expr, REG_1);
	break;
    case ARMG_CC_OP_MULL:
	nf = ex_h_cast(dep2_expr, REG_1);
	zf = _ex_eq(ex_or(dep1_expr, dep2_expr), ex_const(REG_32, 0));
	cf = ex_get_bit(ndep_expr, 1);
	vf = ex_l_cast(ndep_expr, REG_1);
	break;
    default:
	panic("Unexpected ARM CC OP in arm_modify_flags");

    }

    if (dep1_st == op_st + 1 && dep2_st == op_st + 2 &&
	ndep_st == op_st + 3) {
	for (int i = 0; i < 4; i++)
	    Stmt::destroy(ir->at(op_st + i));
	ir->erase(ir->begin() + op_st, ir->begin() + ndep_st + 1);
	new_ir.push_back(new Move(NF, nf));
	new_ir.push_back(new Move(ZF, zf));
	new_ir.push_back(new Move(CF, cf));
	new_ir.push_back(new Move(VF, vf));
	ir->insert(ir->begin() + op_st, new_ir.begin(), new_ir.end());
    } else {
	if (mux0x_st != -1 && ndep_st - (mux0x_st - 1) + 1 == 4 * 4) {
	    // Looks like a conditional thunk; replace with new
	    // conditional assignments using the same condition
	    int start = mux0x_st - 1;
	    assert(start >= 0);
	    Exp *cond, *exp_t, *exp_f, *res;
	    int matched = match_ite(ir, mux0x_st, &cond, &exp_t, &exp_f, &res);
	    assert(matched != -1);
	    assert(cond);
	    cond = ecl(cond);
	    for (int i = start; i <= ndep_st; i++)
		Stmt::destroy(ir->at(i));
	    ir->erase(ir->begin() + start, ir->begin() + ndep_st + 1);
	    Exp *ite = emit_ite(&new_ir, REG_1, cond, ecl(NF), nf);
	    new_ir.push_back(new Move(NF, ite));
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(ZF), zf);
	    new_ir.push_back(new Move(ZF, ite));
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(CF), cf);
	    new_ir.push_back(new Move(CF, ite));
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(VF), vf);
	    new_ir.push_back(new Move(VF, ite));
	    ir->insert(ir->begin() + start, new_ir.begin(), new_ir.end());
	} else {
	    printf("Unexpected non-contiguous thunk: (%d) %d %d %d %d\n",
		   mux0x_st, op_st, dep1_st, dep2_st, ndep_st);
	    panic("Unexpected non-contiguous thunk");
	}
    }
}

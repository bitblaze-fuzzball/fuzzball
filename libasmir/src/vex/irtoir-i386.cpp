/* Because we use offsetof() for values that go in switch statement
   cases, we need a definition in terms of __builtin_offsetof so that
   GCC >= 4.6 can accept the results of offsetof() as a constant
   expression. They've stopped supporting the traditional definition
   as a macro &((type*)0)->memb, which appears in VEX's headers. */
#include <stddef.h>

#include <string>
#include <vector>
#include <iostream>
#include <assert.h>

#include "irtoir-internal.h"
#include "libvex_guest_x86.h"


//
// Register offsets, copied from VEX/priv/guest_x86/toIR.c
//

#define OFFB_EAX       offsetof(VexGuestX86State,guest_EAX)
#define OFFB_EBX       offsetof(VexGuestX86State,guest_EBX)
#define OFFB_ECX       offsetof(VexGuestX86State,guest_ECX)
#define OFFB_EDX       offsetof(VexGuestX86State,guest_EDX)
#define OFFB_ESP       offsetof(VexGuestX86State,guest_ESP)
#define OFFB_EBP       offsetof(VexGuestX86State,guest_EBP)
#define OFFB_ESI       offsetof(VexGuestX86State,guest_ESI)
#define OFFB_EDI       offsetof(VexGuestX86State,guest_EDI)

#define OFFB_EIP       offsetof(VexGuestX86State,guest_EIP)

#define OFFB_CC_OP     offsetof(VexGuestX86State,guest_CC_OP)
#define OFFB_CC_DEP1   offsetof(VexGuestX86State,guest_CC_DEP1)
#define OFFB_CC_DEP2   offsetof(VexGuestX86State,guest_CC_DEP2)
#define OFFB_CC_NDEP   offsetof(VexGuestX86State,guest_CC_NDEP)

#define OFFB_FPREGS    offsetof(VexGuestX86State,guest_FPREG[0])
#define OFFB_FPTAGS    offsetof(VexGuestX86State,guest_FPTAG[0])
#define OFFB_DFLAG     offsetof(VexGuestX86State,guest_DFLAG)
#define OFFB_IDFLAG    offsetof(VexGuestX86State,guest_IDFLAG)
#define OFFB_ACFLAG    offsetof(VexGuestX86State,guest_ACFLAG)
#define OFFB_FTOP      offsetof(VexGuestX86State,guest_FTOP)
#define OFFB_FC3210    offsetof(VexGuestX86State,guest_FC3210)
#define OFFB_FPROUND   offsetof(VexGuestX86State,guest_FPROUND)

#define OFFB_CS        offsetof(VexGuestX86State,guest_CS)
#define OFFB_DS        offsetof(VexGuestX86State,guest_DS)
#define OFFB_ES        offsetof(VexGuestX86State,guest_ES)
#define OFFB_FS        offsetof(VexGuestX86State,guest_FS)
#define OFFB_GS        offsetof(VexGuestX86State,guest_GS)
#define OFFB_SS        offsetof(VexGuestX86State,guest_SS)
#define OFFB_LDT       offsetof(VexGuestX86State,guest_LDT)
#define OFFB_GDT       offsetof(VexGuestX86State,guest_GDT)

#define OFFB_SSEROUND  offsetof(VexGuestX86State,guest_SSEROUND)
#define OFFB_XMM0      offsetof(VexGuestX86State,guest_XMM0)
#define OFFB_XMM1      offsetof(VexGuestX86State,guest_XMM1)
#define OFFB_XMM2      offsetof(VexGuestX86State,guest_XMM2)
#define OFFB_XMM3      offsetof(VexGuestX86State,guest_XMM3)
#define OFFB_XMM4      offsetof(VexGuestX86State,guest_XMM4)
#define OFFB_XMM5      offsetof(VexGuestX86State,guest_XMM5)
#define OFFB_XMM6      offsetof(VexGuestX86State,guest_XMM6)
#define OFFB_XMM7      offsetof(VexGuestX86State,guest_XMM7)

#if VEX_VERSION < 2484
#define OFFB_EMWARN    offsetof(VexGuestX86State,guest_EMWARN)
#else
#define OFFB_EMWARN    offsetof(VexGuestX86State,guest_EMNOTE)
#endif

#define OFFB_TISTART   offsetof(VexGuestX86State,guest_TISTART)
#define OFFB_TILEN     offsetof(VexGuestX86State,guest_TILEN)
#define OFFB_NRADDR    offsetof(VexGuestX86State,guest_NRADDR)
#if VEX_VERSION >= 1536
#define OFFB_SC_CLASS  offsetof(VexGuestX86State,guest_SC_CLASS)
#endif
#if VEX_VERSION >= 1886
#define OFFB_IP_AT_SYSCALL  offsetof(VexGuestX86State,guest_IP_AT_SYSCALL)
#endif

//
// Sub register offsets, calculated manually
//

#define OFFB_AX         (OFFB_EAX)
#define OFFB_AL         (OFFB_EAX)
#define OFFB_AH         (OFFB_EAX+1)
#define OFFB_BX         (OFFB_EBX)
#define OFFB_BL         (OFFB_EBX)
#define OFFB_BH         (OFFB_EBX+1)
#define OFFB_CX         (OFFB_ECX)
#define OFFB_CL         (OFFB_ECX)
#define OFFB_CH         (OFFB_ECX+1)
#define OFFB_DX         (OFFB_EDX)
#define OFFB_DL         (OFFB_EDX)
#define OFFB_DH         (OFFB_EDX+1)
#define OFFB_DI         (OFFB_EDI)
#define OFFB_SI         (OFFB_ESI)
#define OFFB_BP         (OFFB_EBP)
#define OFFB_SP         (OFFB_ESP)

//
// Some unusual register offsets
//
#define OFFB_CC_DEP1_0  (OFFB_CC_DEP1)

//
// EFLAGS masks 
//
#define CF_MASK (1)
#define PF_MASK (1 << 2)
#define AF_MASK (1 << 4)
#define ZF_MASK (1 << 6)
#define SF_MASK (1 << 7)
#define OF_MASK (1 << 11)

//
// EFLAGS positions
//
#define CF_POS  0
#define PF_POS  2
#define AF_POS  4
#define ZF_POS  6
#define SF_POS  7
#define OF_POS  11

//
// Condition code enum copied from VEX/priv/guest-x86/gdefs.h
// Note: If these constants are ever changed, then they would
//       need to be re-copied from the newer version of VEX.
//

typedef
   enum {
      X86CondO      = 0,  /* overflow           */
      X86CondNO     = 1,  /* no overflow        */

      X86CondB      = 2,  /* below              */
      X86CondNB     = 3,  /* not below          */

      X86CondZ      = 4,  /* zero               */
      X86CondNZ     = 5,  /* not zero           */

      X86CondBE     = 6,  /* below or equal     */
      X86CondNBE    = 7,  /* not below or equal */

      X86CondS      = 8,  /* negative           */
      X86CondNS     = 9,  /* not negative       */

      X86CondP      = 10, /* parity even        */
      X86CondNP     = 11, /* not parity even    */

      X86CondL      = 12, /* jump less          */
      X86CondNL     = 13, /* not less           */

      X86CondLE     = 14, /* less or equal      */
      X86CondNLE    = 15, /* not less or equal  */

      X86CondAlways = 16  /* HACK */
   }
   X86Condcode;



using namespace std;

//======================================================================
// 
// Helper functions for the translation
//
//======================================================================





vector<VarDecl *> i386_get_reg_decls()
{
  vector<VarDecl *> ret;
  reg_t r32 = REG_32;
  reg_t r16 = REG_16;
  reg_t r8 = REG_8;
  reg_t r1 = REG_1;


  /* Since the most important contents of EFLAGS are in R_CF .. R_OF,
     we don't want to keep a separate copy of those in another
     register. But for the occasional code that uses all of EFLAGS
     (e.g., pushf and popf), we keep a pseudoregister EFLAGSREST
     which has all the bits of EFLAGS except for the 6 main condition
     code bits, which are zero. */
  ret.push_back(new VarDecl("EFLAGSREST", r32));
  ret.push_back(new VarDecl("R_LDT", r32)); 
  ret.push_back(new VarDecl("R_GDT", r32)); 
  ret.push_back(new VarDecl("R_DFLAG", r32)); 

  ret.push_back(new VarDecl("R_CS", r16)); 
  ret.push_back(new VarDecl("R_DS", r16)); 
  ret.push_back(new VarDecl("R_ES", r16)); 
  ret.push_back(new VarDecl("R_FS", r16)); 
  ret.push_back(new VarDecl("R_GS", r16)); 
  ret.push_back(new VarDecl("R_SS", r16)); 

  // Status bit flags
  ret.push_back(new VarDecl("R_CF", r1));
  ret.push_back(new VarDecl("R_PF", r1));  
  ret.push_back(new VarDecl("R_AF", r1));  
  ret.push_back(new VarDecl("R_ZF", r1));  
  ret.push_back(new VarDecl("R_SF", r1));  
  ret.push_back(new VarDecl("R_OF", r1));  
  ret.push_back(new VarDecl("R_CC_OP", r32));  
  ret.push_back(new VarDecl("R_CC_DEP1", r32));  
  ret.push_back(new VarDecl("R_CC_DEP2", r32));  
  ret.push_back(new VarDecl("R_CC_NDEP", r32));  

  // other flags
  ret.push_back(new VarDecl("R_DFLAG", r32));
  ret.push_back(new VarDecl("R_IDFLAG", r32));
  ret.push_back(new VarDecl("R_ACFLAG", r32));
  ret.push_back(new VarDecl("R_EMWARN", r32));

  // General purpose 32-bit registers
  ret.push_back(new VarDecl("R_EAX", r32));  
  ret.push_back(new VarDecl("R_EBX", r32));  
  ret.push_back(new VarDecl("R_ECX", r32));  
  ret.push_back(new VarDecl("R_EDX", r32));  
  ret.push_back(new VarDecl("R_ESI", r32));  
  ret.push_back(new VarDecl("R_EDI", r32));  
  ret.push_back(new VarDecl("R_EBP", r32));  
  ret.push_back(new VarDecl("R_ESP", r32));  

  // 16-bit registers (bits 0-15)
  ret.push_back(new VarDecl("R_AX", r16));  
  ret.push_back(new VarDecl("R_BX", r16));  
  ret.push_back(new VarDecl("R_CX", r16));  
  ret.push_back(new VarDecl("R_DX", r16));  
  ret.push_back(new VarDecl("R_BP", r16));  
  ret.push_back(new VarDecl("R_SI", r16));  
  ret.push_back(new VarDecl("R_DI", r16));  
  ret.push_back(new VarDecl("R_SP", r16));  


  // 8-bit registers (bits 0-7)
  ret.push_back(new VarDecl("R_AL", r8));  
  ret.push_back(new VarDecl("R_BL", r8));  
  ret.push_back(new VarDecl("R_CL", r8));  
  ret.push_back(new VarDecl("R_DL", r8));  

  // 8-bit registers (bits 8-15)
  ret.push_back(new VarDecl("R_AH", r8));  
  ret.push_back(new VarDecl("R_BH", r8));  
  ret.push_back(new VarDecl("R_CH", r8));  
  ret.push_back(new VarDecl("R_DH", r8));  
  return ret;
}

IRStmt* i386_make_pc_put_stmt(Addr64 addr) {
  return IRStmt_Put(OFFB_EIP, IRExpr_Const(IRConst_U32((UInt)addr)));
}

//----------------------------------------------------------------------
// Translate VEX IR offset into x86 register name
// This is only called for 32-bit registers.
//----------------------------------------------------------------------
static string reg_offset_to_name( int offset, bool *is_good )
{
    assert(offset >= 0);

    const char *name;
    bool good;

    switch ( offset )
    {

        case OFFB_EAX:      name = "EAX";       good=true; break;
        case OFFB_EBX:      name = "EBX";       good=true; break;
        case OFFB_ECX:      name = "ECX";       good=true; break;
        case OFFB_EDX:      name = "EDX";       good=true; break;
        case OFFB_ESP:      name = "ESP";       good=true; break;
        case OFFB_EBP:      name = "EBP";       good=true; break;
        case OFFB_ESI:      name = "ESI";       good=true; break;
        case OFFB_EDI:      name = "EDI";       good=true; break;

        case OFFB_EIP:      name = "EIP";       good=true; break;

        case OFFB_CC_OP:    name = "CC_OP";     good=true; break;
        case OFFB_CC_DEP1:  name = "CC_DEP1";   good=true; break;
        case OFFB_CC_DEP2:  name = "CC_DEP2";   good=true; break;
        case OFFB_CC_NDEP:  name = "CC_NDEP";   good=true; break;

        case OFFB_FPREGS:   name = "FPREGS";    good=true; break;
        case OFFB_FPTAGS:   name = "FPTAGS";    good=true; break;
        case OFFB_DFLAG:    name = "DFLAG";     good=true; break;
        case OFFB_IDFLAG:   name = "IDFLAG";    good=true; break;
        case OFFB_ACFLAG:   name = "ACFLAG";    good=true; break;
        case OFFB_FTOP:     name = "FTOP";      good=true; break;
        case OFFB_FC3210:   name = "FC3210";    good=true; break;
        case OFFB_FPROUND:  name = "FPROUND";   good=true; break;

        case OFFB_LDT:      name = "LDT";       good=true; break;
        case OFFB_GDT:      name = "GDT";       good=true; break;

        case OFFB_SSEROUND: name = "SSEROUND";  good=true; break;
        case OFFB_EMWARN:   name = "EMWARN";    good=true; break;

        case OFFB_TISTART:  name = "TISTART";   good=true; break;
        case OFFB_TILEN:    name = "TILEN";     good=true; break;
#if VEX_VERSION >= 1874
        case OFFB_NRADDR:   name = "NRADDR";    good=true; break;
#endif

#if VEX_VERSION >= 1874
        case OFFB_SC_CLASS: name = "SC_CLASS";  good=true; break;
#endif

#if VEX_VERSION >= 1886
        case OFFB_IP_AT_SYSCALL: name = "IP_AT_SYSCALL";  good=true; break;
#endif

	// The real XMM registers are 128-bit, so these cases only 
	// show up when the code is accessing the low 4 bytes.
        case OFFB_XMM0:     name = "XMM0";      good=false; break;
        case OFFB_XMM1:     name = "XMM1";      good=false; break;
        case OFFB_XMM2:     name = "XMM2";      good=false; break;
        case OFFB_XMM3:     name = "XMM3";      good=false; break;
        case OFFB_XMM4:     name = "XMM4";      good=false; break;
        case OFFB_XMM5:     name = "XMM5";      good=false; break;
        case OFFB_XMM6:     name = "XMM6";      good=false; break;
        case OFFB_XMM7:     name = "XMM7";      good=false; break;

	// Sometimes VEX's translation accesses parts of a XMM register
	// at an offset, just like %ah is part of %eax. If we want to
	// properly support SSE instructions, we'll need to apply a similar
	// strategy as we use say with %ah, but for now we'll just punt.
        case OFFB_XMM0+4:     name = "XMM0";      good=false; break;
        case OFFB_XMM1+4:     name = "XMM1";      good=false; break;
        case OFFB_XMM2+4:     name = "XMM2";      good=false; break;
        case OFFB_XMM3+4:     name = "XMM3";      good=false; break;
        case OFFB_XMM4+4:     name = "XMM4";      good=false; break;
        case OFFB_XMM5+4:     name = "XMM5";      good=false; break;
        case OFFB_XMM6+4:     name = "XMM6";      good=false; break;
        case OFFB_XMM7+4:     name = "XMM7";      good=false; break;

        case OFFB_XMM0+8:     name = "XMM0";      good=false; break;
        case OFFB_XMM1+8:     name = "XMM1";      good=false; break;
        case OFFB_XMM2+8:     name = "XMM2";      good=false; break;
        case OFFB_XMM3+8:     name = "XMM3";      good=false; break;
        case OFFB_XMM4+8:     name = "XMM4";      good=false; break;
        case OFFB_XMM5+8:     name = "XMM5";      good=false; break;
        case OFFB_XMM6+8:     name = "XMM6";      good=false; break;
        case OFFB_XMM7+8:     name = "XMM7";      good=false; break;

        case OFFB_XMM0+12:    name = "XMM0";      good=false; break;
        case OFFB_XMM1+12:    name = "XMM1";      good=false; break;
        case OFFB_XMM2+12:    name = "XMM2";      good=false; break;
        case OFFB_XMM3+12:    name = "XMM3";      good=false; break;
        case OFFB_XMM4+12:    name = "XMM4";      good=false; break;
        case OFFB_XMM5+12:    name = "XMM5";      good=false; break;
        case OFFB_XMM6+12:    name = "XMM6";      good=false; break;
        case OFFB_XMM7+12:    name = "XMM7";      good=false; break;
        default:            
            panic("Unrecognized register name");
    }

    *is_good = good;

    return string(name);
}

static inline Temp *mk_reg( string name, reg_t width )
{
    return new Temp(width, "R_" + name);
}



//======================================================================
// 
// Actual translation functions
//
//======================================================================


static Exp *translate_get_reg_8( unsigned int offset )
{
    bool low;
    string name;

    if (offset >= OFFB_XMM0 && offset < OFFB_XMM7+16) {
	// SSE sub-register: not supported.
	return new Unknown("Unhandled 8-bit XMM lane");
    }

    // Determine which 32 bit register this 8 bit sub
    // register is a part of
    switch ( offset )
    {
        case OFFB_AL:   name = "EAX";    low = true;    break;
        case OFFB_AH:   name = "EAX";    low = false;   break;
        case OFFB_BL:   name = "EBX";    low = true;    break;
        case OFFB_BH:   name = "EBX";    low = false;   break;
        case OFFB_CL:   name = "ECX";    low = true;    break;
        case OFFB_CH:   name = "ECX";    low = false;   break;
        case OFFB_DL:   name = "EDX";    low = true;    break;
        case OFFB_DH:   name = "EDX";    low = false;   break;
        default:
	    assert(0);
    }   

    // Create the corresponding named register
    Temp *reg = mk_reg(name, REG_32);
    

    if ( low )
    {
        return new Cast(reg, REG_8, CAST_LOW);
    }
    else
    {
      Exp *value = new Cast(reg, REG_16, CAST_LOW);
      return new Cast(value, REG_8, CAST_HIGH);
    }
}

static Exp *translate_get_reg_16( unsigned int offset )
{
    string name;
    bool sub;

    if (offset >= OFFB_XMM0 && offset < OFFB_XMM7+16) {
	// SSE sub-register: not supported.
	assert(((offset - OFFB_XMM0) & 1) == 0);
	return new Unknown("Unhandled 16-bit XMM lane");
    }

    switch ( offset )
    {
        //
        // These are 16 bit sub registers
        //
        case OFFB_AX:   name = "EAX";   sub = true; break;
        case OFFB_BX:   name = "EBX";   sub = true; break;
        case OFFB_CX:   name = "ECX";   sub = true; break;
        case OFFB_DX:   name = "EDX";   sub = true; break;
        case OFFB_DI:   name = "EDI";   sub = true; break;
        case OFFB_SI:   name = "ESI";   sub = true; break;
        case OFFB_BP:   name = "EBP";   sub = true; break;
        case OFFB_SP:   name = "ESP";   sub = true; break;

        // 
        // These are regular 16 bit registers
        //
        case OFFB_CS:   name = "CS";    sub = false; break;
        case OFFB_DS:   name = "DS";    sub = false; break;
        case OFFB_ES:   name = "ES";    sub = false; break;
        case OFFB_FS:   name = "FS";    sub = false; break;
        case OFFB_GS:   name = "GS";    sub = false; break;
        case OFFB_SS:   name = "SS";    sub = false; break;

        default:
	    assert(0);
    }

    Exp *value = NULL;

    if ( sub )
    {
        Temp *reg = mk_reg(name, REG_32);

        value = new Cast(reg, REG_16, CAST_LOW);
    }
    else
    {
        Temp *reg = mk_reg(name, REG_16);

        value = reg;
    }

    return value;
}

static Exp *translate_get_reg_32( int offset )
{
    assert(offset >= 0);

    bool is_good;
    string name = reg_offset_to_name(offset, &is_good);
    Exp *result;

    if (is_good)
        result = mk_reg(name, REG_32);
    else
        result = new Unknown("Unknown 32-bit register " + name);

    return result;
}

Exp *i386_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *result = NULL;

    int offset;
    IRType type;

    type = typeOfIRExpr(irbb->tyenv, expr);
    offset = expr->Iex.Get.offset;

    if ( type == Ity_I8 )
    {
        result = translate_get_reg_8(offset);
    }

    else if ( type == Ity_I16 )
    {
        result = translate_get_reg_16(offset);
    }

    else if ( type == Ity_I32 )
    {
        result = translate_get_reg_32(offset);
    }

    else if ( offset == OFFB_LDT || offset == OFFB_GDT )
    {
	// On a 64-bit host, this is 64-bit, because it has type HWord,
	// but pretend that the register is still 32.
	result = new Cast(translate_get_reg_32(offset), REG_64, CAST_UNSIGNED);
    }

    else if ( type == Ity_I64 )
    {
	// We could handle this, but there shouldn't be any 64-bit integer
	// registers.
        result = new Unknown("register type (I64)");
    }
    else if ( type == Ity_F32 )
    {
        result = new Unknown("register type (F32)");
    }
    else if ( type == Ity_F64 )
    {
        result = new Unknown("register type (F64)");
    }

    else if ( type == Ity_I128 )
    {
        result = new Unknown("register type (I128)");
    }
    else if ( type == Ity_V128 )
    {
        result = new Unknown("register type (V128)");
    }

    else
    {
        panic("Unrecognized register type");
    }

    return result;
}

Stmt *i386_translate_dirty( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    Stmt *result = NULL;

    IRDirty *dirty = stmt->Ist.Dirty.details;

    string func = string(dirty->cee->name);

    if ( func == "x86g_dirtyhelper_RDTSC" )
    {
	IRTemp lhs = dirty->tmp;
	assert(lhs != IRTemp_INVALID);
	result = mk_assign_tmp(lhs, new Unknown("rdtsc"), irbb, irout);
    }
    else if ( func == "x86g_dirtyhelper_CPUID_sse2" 
	      || func == "x86g_dirtyhelper_CPUID_sse1"
	      || func == "x86g_dirtyhelper_CPUID_sse0") 
    {
	result = new Special("cpuid");
    }
    else
    {
        result = new ExpStmt(new Unknown("Unknown: Dirty"));
    }
    return result;
}

Exp *i386_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *result = NULL;

    string func = string(expr->Iex.CCall.cee->name);

    Constant    c_CF_POS(REG_32,CF_POS), c_CF_MASK(REG_32,CF_MASK),
      c_ZF_POS(REG_32,ZF_POS),
      c_PF_POS(REG_32,PF_POS),
      c_AF_POS(REG_32,AF_POS),
      c_SF_POS(REG_32,SF_POS),
      c_OF_POS(REG_32,OF_POS),
      c_1(REG_32,1);
    Constant c_REST_MASK(REG_32,
			 ~(CF_MASK|PF_MASK|AF_MASK|ZF_MASK|SF_MASK|OF_MASK));

    Temp EFLAGSREST(REG_32,"EFLAGSREST");

    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    
    if ( func == "x86g_calculate_condition" )
    {
        int arg = expr->Iex.CCall.args[0]->Iex.Const.con->Ico.U32;

        if (use_eflags_thunks) {
          // call eflags thunk
          vector<Exp*> params;
          irout->push_back(new Call(NULL, "x86g_calculate_eflags_all",params));
        }

        switch ( arg )
        {
            case X86CondO:  result =         ecl(OF);  break;
            case X86CondNO: result = ex_not(OF); break;
            case X86CondB:  result =         ecl(CF);  break;
            case X86CondNB: result = ex_not(CF); break;
            case X86CondZ:  result =         ecl(ZF);  break;
            case X86CondNZ: result = ex_not(ZF); break;
            case X86CondS:  result =         ecl(SF);  break;
            case X86CondNS: result = ex_not(SF); break;
            case X86CondP:  result =         ecl(PF);  break;
            case X86CondNP: result = ex_not(PF); break;
            case X86CondBE: result =         ex_or(CF, ZF);  break;
            case X86CondNBE:result = _ex_not(ex_or(CF, ZF)); break;
            case X86CondL:  result =         ex_xor(SF, OF); break;
            case X86CondNL: result = _ex_not(ex_xor(SF, OF));break;
            case X86CondLE: result =         _ex_or(ex_xor(SF, OF), ecl(ZF)); break;
            case X86CondNLE:result = _ex_not(_ex_or(ex_xor(SF, OF), ecl(ZF)));break;
            default:
                panic("Unrecognized condition for x86g_calculate_condition");
        }
	result = _ex_u_cast(result, REG_32);
    }
    else if ( func == "x86g_calculate_eflags_c" )
    {
        if (use_eflags_thunks) {
          // call eflags thunk
          vector<Exp*> params;
          irout->push_back(new Call(NULL, "x86g_calculate_eflags_c",params));
        }

        result = ex_u_cast(CF, REG_32);
    }
    else if ( func == "x86g_calculate_eflags_all" )
    {
        if (use_eflags_thunks) {
          // call eflags thunk
          vector<Exp*> params;
          irout->push_back(new Call(NULL, "x86g_calculate_eflags_all",params));
        }
	
	Exp *wCF = new Cast(ecl(CF), REG_32, CAST_UNSIGNED );
	Exp *wPF = new Cast(ecl(PF), REG_32, CAST_UNSIGNED );
	Exp *wAF = new Cast(ecl(AF), REG_32, CAST_UNSIGNED );
	Exp *wZF = new Cast(ecl(ZF), REG_32, CAST_UNSIGNED );
	Exp *wSF = new Cast(ecl(SF), REG_32, CAST_UNSIGNED );
	Exp *wOF = new Cast(ecl(OF), REG_32, CAST_UNSIGNED );

        result = _ex_or( _ex_shl(wCF, ecl(&c_CF_POS)),
			 _ex_shl(wPF, ecl(&c_PF_POS)),
			 _ex_shl(wAF, ecl(&c_AF_POS)),
			 _ex_shl(wZF, ecl(&c_ZF_POS)),
			 _ex_shl(wSF, ecl(&c_SF_POS)),
			 _ex_shl(wOF, ecl(&c_OF_POS)));
    }
    else if ( func == "x86g_use_seg_selector" ) 
    {
        // refer to valgrind/VEX/priv/guest-x86/ghelpers.c
	// x86g_use_seg_selector(ldt, gdt, seg_selector, virtual_addr)

	// data structure defined in valgrind/VEX/pub/libvex_ir.h

	irout->push_back(new Comment("x86g_use_seg_selector"));
        Exp *ldt = translate_expr((IRExpr*)expr->Iex.CCall.args[0], 
                                  irbb, irout); 
	Exp *gdt = translate_expr((IRExpr*)expr->Iex.CCall.args[1], 
                                  irbb, irout); 
	Exp *seg_selector32 = translate_expr((IRExpr*)expr->Iex.CCall.args[2],
					     irbb, irout); 
        Exp *virtual_addr = translate_expr(expr->Iex.CCall.args[3], irbb, irout);

	if (ldt->exp_type == TEMP && ((Temp *)ldt)->typ == REG_64) {
	    // On a 64-bit host, LDT and GDT get translated as 64-bit.
	    // Switch them back so that the rest of this code works
	    // as expected.
	    ldt = new Cast(ldt, REG_32, CAST_LOW);
	    gdt = new Cast(gdt, REG_32, CAST_LOW);
	}

	Exp *seg_selector16 = 0;
#if 0
	/* This doesn't work, because there's another level of Temp */
	if (seg_selector32->exp_type == CAST) {
	  Cast *cast = (Cast *)seg_selector32;
	  if (cast->cast_type == CAST_UNSIGNED &&
	      cast->exp->exp_type == TEMP) {
	    Temp *temp = (Temp *)cast->exp;
	    if (temp->typ == REG_16) {
	      seg_selector16 = temp;
	    }
	  }
	}
#endif
	if (!seg_selector16) {
	  Temp *t = mk_temp(REG_16, irout);
	  Cast *cast = new Cast(seg_selector32, REG_16, CAST_LOW);
	  irout->push_back(new Move(t, cast));
	  seg_selector16 = t;
	}

// 	if (seg_selector & ~0xFFFF)
// 	    goto done; 

// 	seg_selector &= 0x0000FFFF; 
	
// // 	if ((seg_selector & 3) != 3)
// // 	    goto done; 
	
// 	tibit = (seg_selector >> 2) & 1; 
	Exp *tibit = _ex_and(seg_selector16->clone(), ex_const(REG_16, 4));

// 	if (0 == tibit) {
// 	    if (0 == gdt)
// 		goto done; 
// 	    if (seg_selector >= VEX_GUEST_X86_GDT_NENT)
// 		goto done; 
	    
// 	    // now gdt point to the segment descriptor table
// 	    descriptor = gdt+seg_selector*64; 
// 	} else {
// 	    if (0 == ldt)
// 		goto done; 
	    
// 	    if (seg_selector >= VEX_GUEST_X86_GDT_NENT)
// 		goto done; 
	    
// 	    // now ldt point to the segment descriptor table
// 	    descriptor = ldt+seg_selector*64; 
// 	}
	// offset = (selector >> 3) * 8;
        Exp *seg_offset = _ex_and(seg_selector32->clone(), ex_const(0xFFF8));
	
	Exp *cond = _ex_eq(tibit, ex_const(REG_16, 0));
	Temp *dt = mk_temp(REG_32, irout);
	Exp *ite = emit_ite(irout, REG_32, cond, gdt, ldt);
	irout->push_back(new Move(dt, ite));
	
	Temp *desc = mk_temp(REG_32, irout);
	irout->push_back(new Move(desc, _ex_add(dt->clone(), seg_offset)));

	// get base for segment selector
	// see Intel manual 3A: page 3-12 for descriptor format
	// descriptor+2, descriptor+3, descriptor+4, descriptor+7

	Exp *byte0 = new Cast(new Mem(_ex_add(desc->clone(), ex_const(2)), REG_8), REG_32, CAST_UNSIGNED); 
	Exp *byte1 = new Cast(new Mem(_ex_add(desc->clone(), ex_const(3)), REG_8), REG_32, CAST_UNSIGNED); 
	Exp *byte2 = new Cast(new Mem(_ex_add(desc->clone(), ex_const(4)), REG_8), REG_32, CAST_UNSIGNED); 
	Exp *byte3 = new Cast(new Mem(_ex_add(desc->clone(), ex_const(7)), REG_8), REG_32, CAST_UNSIGNED); 

	Exp *addr = 
	    _ex_add(_ex_or(byte0, 
			   _ex_or(_ex_shl(byte1, ex_const(8)),
				  _ex_or(_ex_shl(byte2, ex_const(16)),
					 _ex_shl(byte3, ex_const(24))))), 
		    virtual_addr);
			    
//	Constant *desc = new Constant(REG_32, descriptor); 
	
	// return an address, high 32 bits are zero, indicating success
	
	result = _ex_u_cast(addr, REG_64); 
    }
    else if ( func == "x86g_calculate_RCL" || func == "x86g_calculate_RCR" ) 
    {
      bool is_left = (func == "x86g_calculate_RCL");
      Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
      Exp *rot_amt = translate_expr(expr->Iex.CCall.args[1], irbb, irout);
      Exp *eflags_in = translate_expr(expr->Iex.CCall.args[2], irbb, irout);

      assert(expr->Iex.CCall.args[3]->tag == Iex_Const);
      assert(expr->Iex.CCall.args[3]->Iex.Const.con->tag == Ico_U32);
      int sz = expr->Iex.CCall.args[3]->Iex.Const.con->Ico.U32;

      const char *helper_name;
      BinOp *(*ex_sh)(Exp*, Exp*);  // same direction shift (rcl -> shl)
      //BinOp *(*_ex_sh)(Exp*, Exp*); // same direction shift (rcl -> shl)
      //BinOp *(*ex_rsh)(Exp*, Exp*); // reverse direction shift (rcl -> shr)
      BinOp *(*_ex_rsh)(Exp*, Exp*);// reverse direction shift (rcl -> shr)
      if (is_left) {
	// Note: some comments and variable names below are specific to this
	// RCL case.
	helper_name = "x86g_calculate_RCL";
	ex_sh = ex_shl;
	//_ex_sh = _ex_shl;
	//ex_rsh = ex_shr;
	_ex_rsh = _ex_shr;
      } else {
	helper_name = "x86g_calculate_RCR";
	ex_sh = ex_shr;
	//_ex_sh = _ex_shr;
	//ex_rsh = ex_shl;
	_ex_rsh = _ex_shl;
      }
      irout->push_back(new Comment(helper_name));

      // normalize rot_amt
      {
        Exp *old_rot_amt = rot_amt;
        rot_amt = mk_temp(REG_32, irout);
        irout->push_back(new Move(rot_amt->clone(),
                                  new BinOp(MOD, 
					    new BinOp(BITAND,
						      old_rot_amt,
						      ex_const(31)),
                                            ex_const(sz*8+1))));
      }

      Temp *answer = mk_temp(REG_32, irout);
      Temp *new_eflags = mk_temp(REG_32, irout);
      
      // check for rot by zero
      Label *non_zero = mk_label();
      Label *zero = mk_label();
      Label *out = mk_label();
      irout->push_back(new CJmp(_ex_eq(rot_amt->clone(), ex_const(REG_32, 0)),
                                ex_name(zero->label),
                                ex_name(non_zero->label)));

      // normal handling
      {
        irout->push_back(non_zero);

        // arg >> (sz*8+1 - rot_amt)
        Exp *new_right_part = 
          _ex_rsh(arg->clone(),
                  _ex_sub(ex_const(sz*8+1),
                          rot_amt->clone()));

        Exp *cf = new Cast(mk_reg("CF", REG_1), REG_32, CAST_UNSIGNED);

	Exp *carry_in_pos;
	Exp *carry_out_pos;
	// Note the symmetry here: RCL and RCR are inverse operations.
	if (is_left) {
	  carry_in_pos = _ex_sub(rot_amt->clone(), ex_const(1));
	  carry_out_pos = _ex_sub(ex_const(sz*8), rot_amt->clone());
	} else {
	  carry_in_pos = _ex_sub(ex_const(sz*8), rot_amt->clone());
	  carry_out_pos = _ex_sub(rot_amt->clone(), ex_const(1));
	}
        // (arg << rot_amt) | (cf << carry_in_pos) | new_right_part
        irout->push_back
          (new Move(answer->clone(),
                _ex_or(ex_sh(arg, rot_amt),
                       _ex_or(_ex_shl(cf, carry_in_pos),
                              new_right_part 
                              )
                       )));
        new_right_part = NULL;
        cf = NULL;

        // calculate new eflags
        // arg >> (sz*8-rot_amt)
        Exp *new_cf = new Cast(_ex_shr(arg->clone(), carry_out_pos), 
                               REG_1, CAST_LOW);
	// (msb(answer) ^ cf) & 1;
	Exp *of = _ex_xor(new Cast(_ex_shr(answer->clone(),
					   ex_const(sz*8-1)),
				   REG_1,
				   CAST_LOW),
			  new_cf->clone());
        irout->push_back
          (new Move(new_eflags->clone(),
                    _ex_or(_ex_and(eflags_in->clone(), 
                                   ex_const(~(CF_MASK | OF_MASK))),
                           _ex_or(_ex_shl(new Cast(of, 
                                                   REG_32, 
                                                   CAST_UNSIGNED), 
                                          ex_const(OF_POS)),
                                  _ex_shl(new Cast(new_cf, 
                                                   REG_32, 
                                                   CAST_UNSIGNED),
                                          ex_const(CF_POS))))));
        of = NULL;
        new_cf = NULL;

        irout->push_back(new Jmp(ex_name(out->label)));
      }
      // rot by zero
      {
        irout->push_back(zero);
        irout->push_back(new Move(answer->clone(), arg->clone()));
        irout->push_back(new Move(new_eflags->clone(), eflags_in->clone()));
      }

      // put result together
      irout->push_back(out);
      result = mk_temp(REG_64, irout);
      irout->push_back
        (new Move(result->clone(),
                  _ex_or(_ex_shl(new Cast(new_eflags, REG_64, CAST_UNSIGNED), 
                                 ex_const(32)),
                         new Cast(answer, REG_64, CAST_UNSIGNED))));

      // clean up
      Exp::destroy(arg);
      Exp::destroy(eflags_in);
      Exp::destroy(rot_amt);
    }
    else if ( func == "x86g_create_mxcsr" )
    {
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	result = _ex_or(ex_const(0x1f80),
			_ex_shl(arg, ex_const(13)));
    }
    else if ( func == "x86g_check_ldmxcsr" )
    {
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	/* Extract the rounding mode */
	Exp *rmode = _ex_and(_ex_shr(arg, ex_const(13)),
			     ex_const(3));
	/* The high word is for emulation warnings: skip it */
	result = _ex_u_cast(rmode, REG_64);
    }
    else
    {
        result = new Unknown("CCall: " + func);
    }


    delete CF;
    delete ZF;
    delete PF;
    delete SF;
    delete OF;
    delete AF;

    return result;
}


static Stmt *translate_put_reg_8( unsigned int offset, Exp *data, IRSB *irbb )
{
    assert(data);

    bool low;
    string name;
    Temp *reg;

    if (offset >= OFFB_XMM0 && offset < OFFB_XMM7+16) {
	// SSE sub-register: not supported.
	Exp::destroy(data);
	return new Special("Unhandled store to 8-bit XMM lane");
    }

    // Determine which 32 bit register this 8 bit sub
    // register is a part of
    switch ( offset )
    {
        case OFFB_AL:   name = "EAX";    low = true;    break;
        case OFFB_AH:   name = "EAX";    low = false;   break;
        case OFFB_BL:   name = "EBX";    low = true;    break;
        case OFFB_BH:   name = "EBX";    low = false;   break;
        case OFFB_CL:   name = "ECX";    low = true;    break;
        case OFFB_CH:   name = "ECX";    low = false;   break;
        case OFFB_DL:   name = "EDX";    low = true;    break;
        case OFFB_DH:   name = "EDX";    low = false;   break;

          // Special case for EFLAGS thunk
    case OFFB_CC_DEP1:
      reg = mk_reg("CC_DEP1", REG_32);
      return new Move( reg, _ex_u_cast(data, REG_32) );

        default:
	    assert(0);
    }   

    // Create the corresponding named register
    reg = mk_reg(name, REG_32);

    Exp *masked = NULL;
    Exp *value = NULL;

    // Assignment to 8 bit sub registers use a combination of bit
    // shifting and masking on the corresponding 32 bit registers
    // to achieve the effect. 
    if ( low )
    {
        masked = new BinOp(BITAND, reg, ex_const(0xffffff00));
        value = new Cast(data, REG_32, CAST_UNSIGNED);
    }
    else
    {
        masked = new BinOp(BITAND, reg, ex_const(0xffff00ff));
        value = new BinOp(LSHIFT, new Cast(data, REG_32, CAST_UNSIGNED), ex_const(8));
    }

    value = new BinOp(BITOR, masked, value); 

    return new Move( new Temp(*reg), value );
}

static Stmt *translate_put_reg_16( unsigned int offset, Exp *data, IRSB *irbb )
{
    assert(data);

    string name;
    bool sub;
    Temp *reg;

    if (offset >= OFFB_XMM0 && offset < OFFB_XMM7+16) {
	// SSE sub-register: not supported.
	assert(((offset - OFFB_XMM0) & 1) == 0);
	Exp::destroy(data);
	return new Special("Unhandled store to 16-bit XMM lane");
    }

    switch ( offset )
    {
        //
        // These are 16 bit sub registers
        //
        case OFFB_AX:   name = "EAX";   sub = true; break;
        case OFFB_BX:   name = "EBX";   sub = true; break;
        case OFFB_CX:   name = "ECX";   sub = true; break;
        case OFFB_DX:   name = "EDX";   sub = true; break;
        case OFFB_DI:   name = "EDI";   sub = true; break;
        case OFFB_SI:   name = "ESI";   sub = true; break;
        case OFFB_BP:   name = "EBP";   sub = true; break;
        case OFFB_SP:   name = "ESP";   sub = true; break;

        // 
        // These are regular 16 bit registers
        //
        case OFFB_CS:   name = "CS";    sub = false; break;
        case OFFB_DS:   name = "DS";    sub = false; break;
        case OFFB_ES:   name = "ES";    sub = false; break;
        case OFFB_FS:   name = "FS";    sub = false; break;
        case OFFB_GS:   name = "GS";    sub = false; break;
        case OFFB_SS:   name = "SS";    sub = false; break;

          // Special case for EFLAGS thunk
    case OFFB_CC_DEP1:
      reg = mk_reg("CC_DEP1", REG_32);
      return new Move( reg, _ex_u_cast(data, REG_32) );

        default:
	    assert(0);
    }

    Exp *masked;
    Exp *value;
    
    if ( sub )
    {
        reg = mk_reg(name, REG_32);

        masked = new BinOp(BITAND, new Temp(*reg), ex_const(0xffff0000));
        value = new Cast(data, REG_32, CAST_UNSIGNED);

        value = new BinOp(BITOR, masked, value);
    }
    else
    {
        reg = mk_reg(name, REG_16);

        value = data;
    }

    return new Move( reg, value );
}

static Stmt *translate_put_reg_32( int offset, Exp *data, IRSB *irbb )
{
    assert(data);
    
    bool is_good;
    string name = reg_offset_to_name(offset, &is_good);

    Stmt *st;

    if (is_good)
    {
        Temp *reg = mk_reg(name, REG_32);
        st = new Move( reg, data );
    }
    else
    {
	Exp::destroy(data);
        st = new Special("Unknown 32-bit register " + name);
    }
    
    return st;
}

Stmt *i386_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    Stmt *result = NULL;

    int offset;
    IRType type;
    Exp *data;

    offset = stmt->Ist.Put.offset;
    type = typeOfIRExpr(irbb->tyenv, stmt->Ist.Put.data);
    data = translate_expr(stmt->Ist.Put.data, irbb, irout);

    // 
    // 8 bit sub registers
    //
    if ( type == Ity_I8 )
    {
        result = translate_put_reg_8(offset, data, irbb);
    }

    //
    // 16 bit registers
    //
    else if ( type == Ity_I16 )
    {
        result = translate_put_reg_16(offset, data, irbb);
    }

    //
    // Regular 32 bit registers
    //
    else if ( type == Ity_I32 )
    {
        result = translate_put_reg_32(offset, data, irbb);
    }

    else if ( offset == OFFB_LDT || offset == OFFB_GDT )
    {
	// On a 64-bit host, this is 64-bit, because it has type HWord,
	// but pretend that the register is still 32.
	Exp *ndata = new Cast(data, REG_32, CAST_LOW);
	result = translate_put_reg_32(offset, ndata, irbb);
    }

    else
    {
	Exp::destroy(data);
        result = new Special("Unrecognized register type");
    }

    return result;
}





/* FIXME: These are arch specific
//----------------------------------------------------------------------
// Determines if a given instruction is a "special" instruction, 
// basically ones that VEX does not handle
//----------------------------------------------------------------------
bool is_special( Instruction *inst )
{
    assert(inst);

    bool result = false;

    // opcode extension
    char reg = (inst->modrm >> 3) & 7;

    switch ( inst->opcode[0] )
    {
        //
        // HLT
        //
        case 0xF4:  
            result = true;
            break;

        //
        // STMXCSR
        // 
        case 0xAE:
            if ( inst->opcode[1] == 0xF )
                result = true;
            break;

        //
        // CPUID
        //
        case 0xA2:
            if ( inst->opcode[1] == 0x0F )
                result = true;
            break;

        // long indirect jmp
        case 0xFF:
          if (reg == 5)
            result = true;
          break;
    }

    return result;
}

//----------------------------------------------------------------------
// Translate special instructions straight from asm to Vine IR
//----------------------------------------------------------------------
vector<Stmt *> *translate_special( Instruction *inst )
{
    assert(inst);

    vector<Stmt *> *irout = new vector<Stmt *>();    
    assert(irout);

    // opcode extension
    char reg = (inst->modrm >> 3) & 7;

    Stmt *st = NULL;

    switch ( inst->opcode[0] )
    {
        //
        // HLT
        //
        case 0xF4:
            st = new Special("hlt");
            break;

        //
        // STMXCSR
        //
        case 0xAE:
            if ( inst->opcode[1] == 0xF )
                st = new Special("stmxcsr");
            break;

        //
        // CPUID
        //
        case 0xA2:
            if ( inst->opcode[1] == 0x0F )
                st = new Special("cpuid");
            break;

        // long indirect jmp
        case 0xFF:
          if (reg == 5)
            st = new Special("ljmpi");
          break;

        default:
            panic("Why would you call translate_special on something that isn't?");
    }
    irout->push_back(mk_dest_label(inst->address));
    irout->push_back(st);

    return irout;
}
*/


/*
static void add_special_returns(vine_block_t *block)
{
  if(block->inst == NULL) return;
  // If this is a return statement, make note of it
  if(block->inst->opcode[0] == 0xC2 ||
     block->inst->opcode[0] == 0xC3 ||
     block->inst->opcode[0] == 0xCA ||
     block->inst->opcode[0] == 0xCB){
    block->vine_ir->push_back(new Special("ret"));
    block->vine_ir->push_back(mk_label());
  }

}
*/



//======================================================================
//
// Code that deals with the setting of EFLAGS 
//
//======================================================================

//----------------------------------------------------------------------
// 
// Helpers
//
//----------------------------------------------------------------------

void del_get_thunk( vector<Stmt *> *ir )
{
    assert(ir);

    vector<Stmt *> rv;
    for (vector<Stmt*>::iterator
           i = ir->begin(); i != ir->end(); i++)
    {
      Stmt *stmt = (*i);
      rv.push_back(stmt);
        if ( stmt->stmt_type == MOVE )
        {
            Move *move = (Move *)stmt;
            if ( move->rhs->exp_type == TEMP )
            {
                Temp *temp = (Temp *)(move->rhs);
                if (    temp->name.find("CC_OP") != string::npos 
                     || temp->name.find("CC_DEP1") != string::npos 
                     || temp->name.find("CC_DEP2") != string::npos 
                     || temp->name.find("CC_NDEP") != string::npos )
                {
                  // remove and Free the Stmt
                  Stmt::destroy(rv.back());
                  rv.pop_back();
                }
            }
        }
    }
    ir->clear();
    ir->insert(ir->begin(), rv.begin(), rv.end());
}


// Parse our statement vector to see if we can find how VEX is setting
// the "thunk" that lazily represents EFLAGS operations. The thunk consists
// of 4 pseudo-registers CC_{OP,DEP1,DEP2,NDEP}, so basically we look for
// assignments to these, and return their statment indexes via the first
// four int * arguments. (An index of "-1" means "not present".)
// An additional complication is that the updates to the thunk may be
// conditional (say for a shift with a non-constant amount), or
// effectively-NOP'ed (say for a shift by a constant 0), and we have to
// recognize what this looks like after VEX's simplifications and our
// translation of VEX's "mux0x" conditionals. These cases are signified by
// also setting one of the remaining int * arguments.
void get_thunk_index(vector<Stmt *> *ir,
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
	if ( temp->name.find("CC_OP") != string::npos ) {
	  *op = i;
	  /* The statement offsets -1. -2, -3, and -6 here have to do with
	     matching the output of emit_ite. */
#ifdef MUX_AS_CJMP
	  if (match_ite(ir, i-6, NULL, NULL, NULL, NULL) >= 0)
	    *mux0x = i-6;
#elif defined(MUX_AS_BITS)
	  if (i >= 3 && match_ite(ir, i-3, NULL, NULL, NULL, NULL) >= 0) {
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
#else
	  // i-2: res = (cond ? exp_t : exp_f);
	  // i-1: t = res;
	  // i  : R_CC_OP = t;
	  Exp *cond, *exp_t, *exp_f, *res;
	  if (i >= 2 && match_ite(ir, i-2, &cond, &exp_t, &exp_f, &res) >= 0) {
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
	  }
#endif
	  if (cc_op_copy && mv->rhs->exp_type == TEMP) {
	    Temp *rhs_temp = (Temp *)mv->rhs;
	    if (rhs_temp->name == cc_op_copy->name) {
	      // We saw t = CC_OP; ...; CC_OP = t, so the thunk is actually
	      // a no-op.
	      *nop = i;
	    }
	  }
	}
	else if ( temp->name.find("CC_DEP1") != string::npos )
	  *dep1 = i;
	else if ( temp->name.find("CC_DEP2") != string::npos )
	  *dep2 = i;
	else if ( temp->name.find("CC_NDEP") != string::npos )
	  *ndep = i;
	else if (mv->rhs->exp_type == TEMP) {
	  Temp *rhs_temp = (Temp *)mv->rhs;
	  if (rhs_temp->name.find("CC_OP") != string::npos ) {
	    cc_op_copy = temp;
	  }
	}
    }
}

void get_eflags_bits(vector<Stmt *> *irout, Exp *from)
{
  Exp *CF = mk_reg("CF", REG_1);
  Exp *PF = mk_reg("PF", REG_1);
  Exp *AF = mk_reg("AF", REG_1);
  Exp *ZF = mk_reg("ZF", REG_1);
  Exp *SF = mk_reg("SF", REG_1);
  Exp *OF = mk_reg("OF", REG_1);

  irout->push_back(new Move(CF, _ex_l_cast(_ex_shr(_ex_and(ecl(from),
							   ex_const(CF_MASK)),
						   ex_const(CF_POS)),
					   REG_1)));

  irout->push_back(new Move(PF, _ex_l_cast(_ex_shr(_ex_and(ecl(from),
							   ex_const(PF_MASK)),
						   ex_const(PF_POS)),
					   REG_1)));

  irout->push_back(new Move(AF, _ex_l_cast(_ex_shr(_ex_and(ecl(from),
							   ex_const(AF_MASK)),
						   ex_const(AF_POS)),
					   REG_1)));

  irout->push_back(new Move(ZF, _ex_l_cast(_ex_shr(_ex_and(ecl(from),
							   ex_const(ZF_MASK)),
						   ex_const(ZF_POS)),
					   REG_1)));

  irout->push_back(new Move(SF, _ex_l_cast(_ex_shr(_ex_and(ecl(from),
							   ex_const(SF_MASK)),
						   ex_const(SF_POS)),
					   REG_1)));

  irout->push_back(new Move(OF, _ex_l_cast(_ex_shr(_ex_and(ecl(from),
							   ex_const(OF_MASK)),
						   ex_const(OF_POS)),
					   REG_1)));
}

//
// This function CONSUMES the cond and flag expressions passed in, i.e.
// they are not cloned before use.
//
void set_flag( vector<Stmt *> *irout, reg_t type, Temp *flag, Exp *cond )
{
  // set flag directly to condition
  //    irout->push_back( new Move(flag, emit_ite(irout, type, cond, ex_const(0), ex_const(1))) );
  irout->push_back( new Move(flag, cond) );
}

//----------------------------------------------------------------------
// 
// Functions that generate IR to modify EFLAGS. These functions all 
// have the form mod_eflags_* where * is the name of an instruction 
// type. They're all similar, yet different enough that they couldn't
// be combined into one function. Their implementation, specifically, 
// the way that the flags are set is based off of the ACTIONS_* macros
// found in VEX/priv/guest-x86/ghelpers.c
//
// The arguments are all COPIED before being used in expression trees.
//
//----------------------------------------------------------------------


/* From the Intel Architecture Software Developer's manual:
   PF (bit 2) Parity flag.
   Set if the least-significant byte of the result contains an even number of
   1 bits; cleared otherwise.
 */
#define CALC_COND_PF(PF8) (_ex_not(_ex_l_cast(_ex_xor(  ex_shr(PF8, &c_7),\
						       ex_shr(PF8, &c_6),\
						       ex_shr(PF8, &c_5),\
						       ex_shr(PF8, &c_4),\
						       ex_shr(PF8, &c_3),\
						       ex_shr(PF8, &c_2),\
						       ex_shr(PF8, &c_1),\
						       ecl(PF8) ),\
					     REG_1)))

// used in eflags helpers to get rid of overflowed bits.
// VEX passes REG_32s even if original operands were smaller.
// e should always be of type REG_32.
Exp* mask_overflow(Exp *e, reg_t to) {
  if (REG_32 == to) 
    return e;

  const_val_t mask;
  switch (to) {
  case REG_8:
    mask = 0xff;
    break;
  case REG_16:
    mask = 0xffff;
    break;

    // should have returned if REG_32.
    // should never call with greater than REG_32
  default:
    assert(0);
  }

  return _ex_and(e, ex_const(REG_32, mask));
}

vector<Stmt *> mod_eflags_copy( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    // set status flags from arg1
    get_eflags_bits(&irout, arg1);

    return irout;
}

vector<Stmt *> mod_eflags_add( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    Temp *res = mk_temp(REG_32,&irout);

    // The operation itself
    irout.push_back(new Move(res, mask_overflow(ex_add(arg1, arg2), type)));

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), c_N1(REG_32,((address_t)-1)), c_TYPE_SIZE_LESS_1(REG_32,get_type_size(type) - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10);

    // Calculate flags


    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);


    Exp *condCF = ex_lt(res, arg1);
    set_flag(&irout, type, CF, condCF);
    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    Move *m =  new Move(PF8, ex_l_cast(res, REG_8) );
    irout.push_back(m );
    /*
    Exp *condPF = _ex_eq( ecl(&c_0), _ex_and( ecl(&c_1), _ex_xor(  ex_shr(PF8, &c_7),
    Exp *condPF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), _ex_xor(  ex_shr(PF8, &c_7),
                                                                   ex_shr(PF8, &c_6),
                                                                   ex_shr(PF8, &c_5),
                                                                   ex_shr(PF8, &c_4),
                                                                   ex_shr(PF8, &c_3),
                                                                   ex_shr(PF8, &c_2),
                                                                   ex_shr(PF8, &c_1),
                                                                      ecl(PF8) ) ) );
    */
    Exp *condPF = CALC_COND_PF(PF8);

    set_flag(&irout, type, PF, condPF);

    Exp *condAF = _ex_eq( ecl(&c_0x10), _ex_and( ecl(&c_0x10), ex_xor( res, arg1, arg2) ));
    set_flag(&irout, type, AF, condAF);

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), 
                    _ex_shr( _ex_and( ex_xor(arg1, arg2, &c_N1), ex_xor(arg1, res) ), ecl(&c_TYPE_SIZE_LESS_1) )) );
    set_flag(&irout, type, OF, condOF);

    return irout;
}

vector<Stmt *> mod_eflags_sub( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    Temp *res = mk_temp(REG_32,&irout);

    // The operation itself
    irout.push_back( new Move(res, mask_overflow(ex_sub(arg1, arg2), type)));

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), c_TYPE_SIZE_LESS_1(REG_32,get_type_size(type) - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10);
    Constant    c_N1(REG_32,((address_t) -1));

    // Calculate flags

    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);
    Exp *condCF = ex_lt(arg1, arg2);
    set_flag(&irout, type, CF, condCF);
    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Exp *condAF = _ex_eq( ecl(&c_0x10), _ex_and( ecl(&c_0x10), ex_xor( res, arg1, arg2) ));
    set_flag(&irout, type, AF, condAF);

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), 
                    _ex_shr( _ex_and( ex_xor(arg1, arg2), ex_xor(arg1, res) ), ecl(&c_TYPE_SIZE_LESS_1) )) );
    set_flag(&irout, type, OF, condOF);

    return irout;
}

vector<Stmt *> mod_eflags_adc( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 )
{
    vector<Stmt *> irout;

    Constant c_CF_MASK(REG_32,CF_MASK);

    Temp *res = mk_temp(REG_32,&irout);

    // (from VEX's gdefs.h) 
    // arg1 = first arg          
    // arg2 = (second arg) XOR old_carry
    // arg3 = old_carry

    // Recover the args
    arg3 = _ex_and( arg3, &c_CF_MASK );
    arg2 = _ex_xor( arg2, arg3 );

    // The operation itself
    irout.push_back(new Move(res, mask_overflow(_ex_add(ex_add(arg1, arg2), 
                                                        ecl(arg3)), type)));

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), c_N1(REG_32,((address_t) -1)), 
      c_TYPE_SIZE_LESS_1(REG_32,get_type_size(type) - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10);

    // Calculate flags

//     Temp *CF = mk_temp("CF", Ity_I1);
//     Exp *condOldC = ex_eq(arg3, &c_0);
//     Exp *condCF = ex_le(res, arg1);
//     set_flag(&irout, type, CF, condCF);
//     Label *doneCF = mk_label();
//     Label *resetCF = mk_label();
//     irout.push_back( new CJmp(condOldC, new Name(doneCF->label), new Name(resetCF->label)) );
//     irout.push_back( resetCF );
//     condCF = ex_lt(res, arg1);
//     set_flag(&irout, type, CF, condCF);
//     irout.push_back( doneCF );

    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);


    // (arg3==1) & (res <= arg1) | (arg3==0) & (res<arg1)
    Exp *condCF = _ex_or(_ex_and(ex_eq(arg3,&c_1), 
				 ex_le(res,arg1)), 
			 _ex_and(ex_eq(arg3,&c_0), 
				 ex_lt(res,arg1)));
    set_flag(&irout, type, CF, condCF);


    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Exp *condAF = _ex_eq( ecl(&c_0x10), _ex_and( ecl(&c_0x10), ex_xor( res, arg1, arg2) ));
    set_flag(&irout, type, AF, condAF);

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), 
                    _ex_shr( _ex_and( ex_xor(arg1, arg2, &c_N1), ex_xor(arg1, res) ), ecl(&c_TYPE_SIZE_LESS_1) )) );
    set_flag(&irout, type, OF, condOF);

    delete arg2;
    delete arg3;

    return irout;
}

vector<Stmt *> mod_eflags_sbb( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 )
{
    vector<Stmt *> irout;

    Constant c_CF_MASK(REG_32,CF_MASK);

    Temp *res = mk_temp(REG_32,&irout);

    // Recover the args
    arg3 = _ex_and( arg3, &c_CF_MASK );
    arg2 = _ex_xor( arg2, arg3 );

    // The operation itself
    irout.push_back( new Move(res, mask_overflow(_ex_sub(ex_sub(arg1, arg2), 
                                                         ecl(arg3)), type)));

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), 
      c_N1(REG_32,((address_t)-1)), 
      c_TYPE_SIZE_LESS_1(REG_32,get_type_size(type) - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10);

    // Calculate flags

//     Temp *CF = mk_temp("CF", Ity_I1);
//     Exp *condOldC = ex_eq(arg3, &c_0);
//     Exp *condCF = ex_le(arg1, arg2);
//     set_flag(&irout, type, CF, condCF);
//     Label *doneCF = mk_label();
//     Label *resetCF = mk_label();
//     irout.push_back( new CJmp(condOldC, new Name(doneCF->label), new Name(resetCF->label)) );
//     irout.push_back( resetCF );
//     condCF = ex_lt(arg1, arg2);
//     set_flag(&irout, type, CF, condCF);
//     irout.push_back( doneCF );
    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);


    // (arg3==0) & (arg1 < arg2) | (not(arg3==0)) & (arg1<=arg2)
    // check: the arg3 case is the same as a regular "sub".
    Exp *condCF = _ex_or(_ex_and(ex_eq(arg3,&c_0), 
				 ex_lt(arg1,arg2)), 
			 _ex_and(_ex_not(ex_eq(arg3,&c_0)), 
				 ex_le(arg1,arg2)));
    set_flag(&irout, type, CF, condCF);

    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Exp *condAF = _ex_eq( ecl(&c_0x10), _ex_and( ecl(&c_0x10), ex_xor( res, arg1, arg2) ));
    set_flag(&irout, type, AF, condAF);

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), 
                    _ex_shr( _ex_and( ex_xor(arg1, arg2), ex_xor(arg1, res) ), ecl(&c_TYPE_SIZE_LESS_1) )) );
    set_flag(&irout, type, OF, condOF);

    delete arg2;
    delete arg3;

    return irout;
}

vector<Stmt *> mod_eflags_logic( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    Exp *res = arg1;

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), c_N1(REG_32,((address_t) -1)), 
      c_TYPE_SIZE_LESS_1(REG_32,get_type_size(type) - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10);

    // Calculate flags

    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);

    irout.push_back(new Move(CF, Constant::f.clone()));
    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    irout.push_back( new Move(AF, Constant::f.clone()) );    

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    irout.push_back( new Move(OF, Constant::f.clone()) );    

    return irout;
}

vector<Stmt *> mod_eflags_inc( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);
    Exp *res = arg1;

    // All the static constants we'll ever need
    Constant    
      c_0(REG_32,0), 
      //      c_CF_POS(REG_32,CF_POS),
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), 
      c_7(REG_32,7), 
      c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
      c_0x10(REG_32,0x10);

    Constant    c_SIGN_MASK = 
      Constant(REG_32, 1 << (type_size - 1) );
    Constant    c_DATA_MASK = 
      Constant(REG_32, 
	       type_size == 8 
	       ? 0xff 
	       : (type_size == 16 
		  ? 0xffff 
		  : 0xffffffff) );

    Exp *argL = _ex_sub(res, &c_1);
    Exp *argR = &c_1;

    // Calculate flags

    // no-op
    //    irout.push_back( new Move(CF, _ex_and(ex_shr(arg3, &c_CF_POS), ecl(&c_1))));
    //    irout.push_back( new Move(CF, 
    //			      new Cast(ex_shr(arg3, &c_CF_POS), 
    //				       I1,
    //				       CAST_LOW)));

    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);

    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Exp *condAF = _ex_eq( ecl(&c_0x10), _ex_and( ecl(&c_0x10), ex_xor( res, argL, argR) ));
    set_flag(&irout, type, AF, condAF);

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_eq( ex_and(res, &c_DATA_MASK), ecl(&c_SIGN_MASK) );
    set_flag(&irout, type, OF, condOF);

    delete argL;

    return irout;
}

vector<Stmt *> mod_eflags_dec( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);
    Exp *res = arg1;

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), c_CF_POS(REG_32,CF_POS),
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), c_N1(REG_32,((address_t)-1)), 
      c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10);

    Constant    c_SIGN_MASK = Constant(REG_32, 1 << (type_size - 1) );
    Constant    c_DATA_MASK = Constant(REG_32, type_size == 8 ? 0xff : (type_size == 16 ? 0xffff : 0xffffffff) );

    //    res = _ex_u_cast(res, REG_32);
    Exp *argL = _ex_add(res, &c_1);
    Exp *argR = &c_1;

    // Calculate flags

    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);

    irout.push_back( new Move(CF, _ex_and(_ex_l_cast(ex_shr(arg3, &c_CF_POS), REG_1), Constant::t.clone()) ) );    
    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Exp *condAF = _ex_eq( ecl(&c_0x10), _ex_and( ecl(&c_0x10), ex_xor( res, argL, argR) ));
    set_flag(&irout, type, AF, condAF);

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_eq( ex_and(res, &c_DATA_MASK), ex_sub(&c_SIGN_MASK, &c_1) );
    set_flag(&irout, type, OF, condOF);

    delete argL;

    return irout;
}

vector<Stmt *> mod_eflags_shl( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);
    Exp *res = arg1;

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), c_CF_MASK(REG_32,CF_MASK),
      c_1(REG_32,1), 
      c_2(REG_32,2), 
      c_3(REG_32,3), 
      c_4(REG_32,4), 
      c_5(REG_32,5), 
      c_6(REG_32,6), c_N1(REG_32,((address_t)-1)),
      c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
      c_7(REG_32,7), c_0x10(REG_32,0x10),
      c_TYPE_SIZE_MASK(REG_32, (1 << type_size) - 1);

    // Calculate flags
    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);

    Exp *to_destroy = 0;
    if (type == REG_8 || type == REG_16) {
	/* VEX computes the 32-bit version of the result, but the flags
	   should only be based on the narrow bits. */
	res = ex_and(res, &c_TYPE_SIZE_MASK);
	to_destroy = res;
    }
    
    irout.push_back( new Move(CF, _ex_and(_ex_l_cast(ex_shr(arg2, &c_TYPE_SIZE_LESS_1), REG_1), Constant::t.clone()) ) );    
    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    irout.push_back( new Move(AF, Constant::f.clone()) );    

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    Exp *condOF = _ex_and(_ex_l_cast(_ex_shr(ex_xor(arg1, arg2), ecl(&c_TYPE_SIZE_LESS_1)), REG_1), Constant::t.clone());
    set_flag(&irout, type, OF, condOF);

    if (to_destroy)
	Exp::destroy(to_destroy);

    return irout;
}

vector<Stmt *> mod_eflags_shr( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);
    Exp *res = arg1;

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), c_CF_MASK(REG_32,CF_MASK),
                c_1(REG_32,1), 
                c_2(REG_32,2), 
                c_3(REG_32,3), 
                c_4(REG_32,4), 
                c_5(REG_32,5), 
                c_6(REG_32,6), c_N1(REG_32,((address_t)-1)), c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
                c_7(REG_32,7), c_0x10(REG_32,0x10);


    // Calculate flags

    Temp *CF = mk_reg("CF", REG_1);
    Temp *ZF = mk_reg("ZF", REG_1);
    Temp *PF = mk_reg("PF", REG_1);
    Temp *SF = mk_reg("SF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);
    Temp *AF = mk_reg("AF", REG_1);

    irout.push_back( new Move(CF, ex_l_cast(arg2, REG_1) ) );    
    
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    irout.push_back( new Move(AF, Constant::f.clone()) );    

    Exp *condZF = ex_eq( res, &c_0 );
    set_flag(&irout, type, ZF, condZF);
    
    Exp *condSF = _ex_eq( ecl(&c_1), _ex_and( ecl(&c_1), ex_shr( res, &c_TYPE_SIZE_LESS_1)) );
    set_flag(&irout, type, SF, condSF);

    // FIXME: this is wrong for sar. should set OF to 0.
    Exp *condOF = _ex_and(_ex_l_cast(_ex_shr(ex_xor(arg1, arg2), ecl(&c_TYPE_SIZE_LESS_1)), REG_1), Constant::t.clone());
    set_flag(&irout, type, OF, condOF);

    return irout;
}

vector<Stmt *> mod_eflags_rol( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), c_CF_MASK(REG_32,CF_MASK),
                c_1(REG_32,1), c_OF_MASK(REG_32,OF_MASK),
                c_11(REG_32,11),
                c_OF_MAGIC_NUMBER(REG_32,11 - (type_size - 1));

    // Calculate flags

    Temp *CF = mk_reg("CF", REG_1);
    Temp *OF = mk_reg("OF", REG_1);

    // lsb of result
    set_flag(&irout, type, CF, _ex_l_cast(ecl(arg1),
    					  REG_1));

    //    Exp *OF = _ex_and( ecl(&c_OF_MASK), _ex_xor( ex_shl(arg1, &c_OF_MAGIC_NUMBER), ex_shl(arg1, &c_11) ) );
    // new OF flag xor msb of result
    set_flag(&irout, type, OF, _ex_xor(ecl(CF),
				       _ex_l_cast(_ex_shr(ecl(arg1),
							  ex_const(type_size-1)),
						  REG_1)));
    //    set_flag(&irout, type, OF, _ex_l_cast(_ex_and( ecl(&c_OF_MASK), 
    //						   _ex_xor( ex_shl(arg1, 
    //								   &c_OF_MAGIC_NUMBER), 
    //							    ex_shl(arg1, &c_11))),
    //					  REG_1));

//    destroy(oldFlags);
//    destroy(CF);
//    destroy(OF);

    return irout;
}

vector<Stmt *> mod_eflags_ror( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), c_CF_MASK(REG_32,CF_MASK),
                c_1(REG_32,1), c_OF_MASK(REG_32,OF_MASK),
                c_11(REG_32,11),
                c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
                c_OF_MAGIC_NUMBER(REG_32,11 - (type_size - 1)),
                c_OF_MAGIC_NUMBER_2(REG_32,11 - (type_size - 1) + 1);

    // Calculate flags

    // CF is set to msb of result
    Temp *CF = mk_reg("CF", REG_1);
    set_flag(&irout, type, CF, _ex_l_cast(ex_shr(arg1, &c_TYPE_SIZE_LESS_1),
					  REG_1));

    // OF is set to xor of two most significant bits of result
    //    Exp *OF = _ex_and( ecl(&c_OF_MASK), _ex_xor( ex_shl(arg1, &c_OF_MAGIC_NUMBER), ex_shl(arg1, &c_OF_MAGIC_NUMBER_2) ) );
    Temp *OF = mk_reg("OF", REG_1);
    //    set_flag(&irout, type, OF, _ex_and( ecl(&c_OF_MASK), _ex_xor( ex_shl(arg1, &c_OF_MAGIC_NUMBER), ex_shl(arg1, &c_OF_MAGIC_NUMBER_2) ) ));
    set_flag(&irout, type, OF, _ex_xor(ecl(CF),
				       _ex_l_cast(_ex_shr(ecl(arg1),
							  ex_const(type_size-2)
							  ),
						  REG_1)
				       ));

//    destroy(oldFlags);
//    destroy(CF);
//    destroy(OF);

    return irout;
}

vector<Stmt *> mod_eflags_umul( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
                c_1(REG_32,1), 
                c_2(REG_32,2), 
                c_3(REG_32,3), 
                c_4(REG_32,4), 
                c_5(REG_32,5), 
                c_6(REG_32,6), c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
                c_7(REG_32,7);

    // Calculate flags
    Temp *res = NULL;
    Temp *lo = NULL;
    Temp *hi = NULL;

    reg_t res_type;

    // Figure out what types to use
    if ( type == REG_8 )
    {
      res_type = REG_16;
    }
    else if ( type == REG_16 )
    {
      res_type = REG_32;
    }
    else if ( type == REG_32 )
    {
      res_type = REG_64;
    }
    else
    {
      panic("Unexpected type in mod_eflags_umul");
    }
    res = mk_temp(res_type,&irout);
    lo = mk_temp(type,&irout);
    hi = mk_temp(type,&irout);

    irout.push_back( new Move(res, _ex_mul(_ex_u_cast(ex_l_cast(arg1, type), 
                                                      res->typ),
                                           _ex_u_cast(ex_l_cast(arg2, type), 
                                                      res->typ) )));
    irout.push_back( new Move(lo, ex_l_cast(res, lo->typ)) );
    irout.push_back( new Move(hi, ex_h_cast(res, hi->typ)) );

    Temp *CF = mk_reg("CF", REG_1);
    Exp *condCF = _ex_neq( ecl(hi), ex_const(type, 0));
    set_flag(&irout, type, CF, condCF);
    
    Temp *PF = mk_reg("PF", REG_1);
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Temp *AF = mk_reg("AF", REG_1);
    irout.push_back( new Move(AF, Constant::f.clone()) );    

    Temp *ZF = mk_reg("ZF", REG_1);
    Exp *condZF = _ex_eq( ecl(lo), ex_const(type, 0));
    set_flag(&irout, type, ZF, condZF);
    
    Temp *SF = mk_reg("SF", REG_1);
    Exp *condSF = _ex_l_cast(ex_shr( lo, &c_TYPE_SIZE_LESS_1), REG_1);
    set_flag(&irout, type, SF, condSF);

    Temp *OF = mk_reg("OF", REG_1);
    irout.push_back( new Move(OF, new Temp(*CF)) );

    return irout;
}

vector<Stmt *> mod_eflags_smul( reg_t type, Exp *arg1, Exp *arg2 )
{
    vector<Stmt *> irout;

    int type_size = get_type_size(type);

    // All the static constants we'll ever need
    Constant    c_0(REG_32,0), 
                c_1(REG_32,1), 
                c_2(REG_32,2), 
                c_3(REG_32,3), 
                c_4(REG_32,4), 
                c_5(REG_32,5), 
                c_6(REG_32,6), c_TYPE_SIZE_LESS_1(REG_32,type_size - 1),
                c_7(REG_32,7);

    // Calculate flags
    Temp *res = NULL;
    Temp *lo = NULL;
    Temp *hi = NULL;

    // Figure out what types to use
    if ( type == REG_8 )
    {
      res = mk_temp(REG_16, &irout);
      lo = mk_temp(REG_8,&irout);
      hi = mk_temp(REG_8,&irout);
    }
    else if ( type == REG_16 )
    {
      res = mk_temp(REG_32,&irout);
      lo = mk_temp(REG_16,&irout);
      hi = mk_temp(REG_16,&irout);
    }
    else if ( type == REG_32 )
    {
      res = mk_temp(REG_64,&irout);
      lo = mk_temp(REG_32,&irout);
      hi = mk_temp(REG_32,&irout);
    }

    irout.push_back( new Move(res, _ex_mul(_ex_s_cast(ex_l_cast(arg1, type), 
                                                     res->typ),
                                           _ex_s_cast(ex_l_cast(arg2, type), 
                                                     res->typ) )));

    irout.push_back( new Move(lo, ex_l_cast(res, lo->typ)) );
    irout.push_back( new Move(hi, ex_h_cast(res, hi->typ)) );

    Temp *CF = mk_reg("CF", REG_1);
    Exp *condCF = _ex_neq( ecl(hi), ex_sar(lo, &c_TYPE_SIZE_LESS_1) );
    set_flag(&irout, type, CF, condCF);
    
    Temp *PF = mk_reg("PF", REG_1);
    Temp *PF8 = mk_temp(Ity_I8,&irout);
    irout.push_back( new Move(PF8, ex_l_cast(res, REG_8) ) );
    Exp *condPF = CALC_COND_PF(PF8);
    set_flag(&irout, type, PF, condPF);

    Temp *AF = mk_reg("AF", REG_1);
    irout.push_back( new Move(AF, Constant::f.clone()) );    

    Temp *ZF = mk_reg("ZF", REG_1);
    Exp *condZF = _ex_eq( ecl(lo), ex_const(lo->typ, 0));
    set_flag(&irout, type, ZF, condZF);
    
    Temp *SF = mk_reg("SF", REG_1);
    Exp *condSF = _ex_l_cast(ex_shr( lo, &c_TYPE_SIZE_LESS_1), REG_1);
    set_flag(&irout, type, SF, condSF);

    Temp *OF = mk_reg("OF", REG_1);
    irout.push_back( new Move(OF, new Temp(*CF)) );

    return irout;
}

int del_put_thunk(vector<Stmt *> *ir,
		   int opi, int dep1, int dep2, int ndep, int mux0x)
{
    assert(ir);
    assert(opi >= 0 && dep1 >= 0 && dep2 >= 0 && ndep >= 0);

    int end = -1;
    int len = 0;

    // Delete statements assigning to flag thunk temps
    vector<Stmt *> rv;
    for (vector<Stmt*>::iterator
           i = ir->begin(); i != ir->end(); i++)
    {
      Stmt *stmt = (*i);
      rv.push_back(stmt);
      len++;

        if ( stmt->stmt_type == MOVE )
        {
            Move *move = (Move *)stmt;
            if ( move->lhs->exp_type == TEMP )
            {
              Temp *temp = (Temp *)(move->lhs);
                if (    temp->name.find("CC_OP") != string::npos 
                     || temp->name.find("CC_DEP1") != string::npos 
                     || temp->name.find("CC_DEP2") != string::npos 
                     || temp->name.find("CC_NDEP") != string::npos )
                {
                  // XXX: don't delete for now.
                  // remove and Free the Stmt
                  //                  Stmt::destroy(rv.back());
                  //                  rv.pop_back();
                  //                  len--;
                  end = len;
                }
            }
        }
    }
    assert(end >= 0);
    ir->clear();
    ir->insert(ir->begin(), rv.begin(), rv.end());
    return end;
}


//
// These typedefs are specifically for casting mod_eflags_func to the 
// function that accepts the right number of arguments
//
typedef vector<Stmt *> Mod_Func_0 (void);
typedef vector<Stmt *> Mod_Func_2 (reg_t, Exp *, Exp *);
typedef vector<Stmt *> Mod_Func_3 (reg_t, Exp *, Exp *, Exp *);

static void modify_eflags_helper( string op, reg_t type, vector<Stmt *> *ir, int argnum, Mod_Func_0 *mod_eflags_func )
{
    assert(ir);
    assert(argnum == 2 || argnum == 3);
    assert(mod_eflags_func);

    // Look for occurrence of CC_OP assignment
    // These will have the indices of the CC_OP stmts
    int opi, dep1, dep2, ndep, mux0x, nop;
    opi = dep1 = dep2 = ndep = mux0x = nop = -1;
    get_thunk_index(ir, &opi, &dep1, &dep2, &ndep, &mux0x, &nop);

    if ( opi >= 0 )
    {
        vector<Stmt *> mods;

	if ( nop != -1 )
	{
	    // Do-nothing thunk update, just delete it (mods is empty)
	}
	else if ( argnum == 2 )
        {
            // Get the arguments we need from these Stmt's
            Exp *arg1 = ((Move *)(ir->at(dep1)))->rhs;
            Exp *arg2 = ((Move *)(ir->at(dep2)))->rhs;

            // Do the translation
            // To figure out the type, we assume the rhs of the 
            // assignment to CC_DEP is always either a Constant or a Temp
            // Otherwise, we have no way of figuring out the expression type
            Mod_Func_2 *mod_func = (Mod_Func_2 *)mod_eflags_func;
            mods = mod_func(type, arg1, arg2);
        }
        else // argnum == 3
        {
            Exp *arg1 = ((Move *)(ir->at(dep1)))->rhs;
            Exp *arg2 = ((Move *)(ir->at(dep2)))->rhs;
            Exp *arg3 = ((Move *)(ir->at(ndep)))->rhs;

            Mod_Func_3 *mod_func = (Mod_Func_3 *)mod_eflags_func;
            mods = mod_func(type, arg1, arg2, arg3);
        }
	
	// If we saw that VEX made its thunk update conditional, make
	// our flags updates conditional with the same condition. It's
	// easier and more reliable to do this here than to figure out
	// the condition ourselves in the individual flags functions.
	if (mux0x != -1) {
	  // Note that there's no check here that we're getting the
	  // sense of the condition correct: we don't look at exp_t or
	  // exp_f. For now we just assume that VEX uses the convention
	  // that true means "update the flags", and no intermediate
	  // layer negates the condition.
	  Exp *cond, *exp_t, *exp_f, *res;
	  match_ite(ir, mux0x, &cond, &exp_t, &exp_f, &res);
	  Label *mod = mk_label();
	  Label *nomod = mk_label();
	  mods.insert(mods.begin(), mod);
	  mods.insert(mods.begin(),
		      new CJmp(ecl(cond),
			       new Name(mod->label), new Name(nomod->label)) );
	  mods.push_back(nomod);
	}

        // Delete the thunk
	int pos = del_put_thunk(ir, opi, dep1, dep2, ndep, mux0x);
        // Insert the eflags mods in this position
        ir->insert( ir->begin()+pos, mods.begin(), mods.end() );
        ir->insert( ir->begin()+pos, new Comment("eflags thunk: "+op));
    }
    else {
      cerr << "Warning! No EFLAGS thunk was found for \"" 
           << op << "\"!" << endl;
      for(vector<Stmt*>::iterator
            i=ir->begin(); i!=ir->end(); i++) {
        cerr << (*i)->tostring() << endl;
      }
      //      panic("No EFLAGS thunk was found for \"" + op + "\"!");
    }
}

void i386_modify_flags( asm_program_t *prog, vine_block_t *block )
{
    assert(block);

    vector<Stmt *> *ir = block->vine_ir;    

    // Look for occurrence of CC_OP assignment
    // These will have the indices of the CC_OP stmts
    // FIXME: cut-and-paste from modify_eflags_helper, which 
    //        will do this again
    int opi, dep1, dep2, ndep, mux0x, nop;
    opi = dep1 = dep2 = ndep = mux0x = nop = -1;
    get_thunk_index(ir, &opi, &dep1, &dep2, &ndep, &mux0x, &nop);

    if (opi == -1)
      // doesn't set flags
      return;

    Stmt *op_stmt = ir->at(opi);

    bool got_op;
    Exp *op_exp = 0;
    int op = -1;
    if(!(op_stmt->stmt_type == MOVE)) {
      got_op = false;
    } else {
      Move *op_mov = (Move*)op_stmt;
      op_exp = op_mov->rhs;
    }
    if (op_exp) {
      if (op_exp->exp_type == ITE) {
	// We're assuming here that the CC_OP value we want to look at
	// is on the true side of the ITE. For now that should be safe
	// because true means "update the flags" (an assumption we make
	// elsewhere too).
	op_exp = ((Ite*)op_exp)->true_e;
      }
      if(op_exp->exp_type == CONSTANT) {
        Constant *op_const = (Constant*)op_exp;
        op = op_const->val;
        got_op = true;
      } else if (op_exp->exp_type == BINOP) {
	BinOp *bin_or = (BinOp*)op_exp;
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

    if (nop != -1) {
      // Leave flags unchanged
    } else if (got_op) {
      reg_t type;
      switch(op) {
      case X86G_CC_OP_ADDB:
      case X86G_CC_OP_ADCB:
      case X86G_CC_OP_SUBB:
      case X86G_CC_OP_SBBB:
      case X86G_CC_OP_LOGICB:
      case X86G_CC_OP_INCB:   
      case X86G_CC_OP_DECB:   
      case X86G_CC_OP_SHLB:   
      case X86G_CC_OP_SHRB:   
      case X86G_CC_OP_ROLB:   
      case X86G_CC_OP_RORB:   
      case X86G_CC_OP_UMULB:  
      case X86G_CC_OP_SMULB:
        type = REG_8;
        break;

      case X86G_CC_OP_ADDW:
      case X86G_CC_OP_ADCW:
      case X86G_CC_OP_SUBW:
      case X86G_CC_OP_SBBW:
      case X86G_CC_OP_LOGICW:
      case X86G_CC_OP_INCW:   
      case X86G_CC_OP_DECW:   
      case X86G_CC_OP_SHLW:   
      case X86G_CC_OP_SHRW:   
      case X86G_CC_OP_ROLW:   
      case X86G_CC_OP_RORW:   
      case X86G_CC_OP_UMULW:  
      case X86G_CC_OP_SMULW:
        type = REG_16;
        break;

      case X86G_CC_OP_ADDL:
      case X86G_CC_OP_ADCL:
      case X86G_CC_OP_SUBL:
      case X86G_CC_OP_SBBL:
      case X86G_CC_OP_LOGICL:
      case X86G_CC_OP_INCL:   
      case X86G_CC_OP_DECL:   
      case X86G_CC_OP_SHLL:   
      case X86G_CC_OP_SHRL:   
      case X86G_CC_OP_ROLL:   
      case X86G_CC_OP_RORL:   
      case X86G_CC_OP_UMULL:  
      case X86G_CC_OP_SMULL:
        type = REG_32;
        break;

      case X86G_CC_OP_COPY:
        type = REG_32;
        break;

      case -1:
      default:
        assert(0);
      }

      string op_s;
      Mod_Func_0 *cb;
      int num_params;
      
      switch (op) {
      case X86G_CC_OP_COPY:
        op_s = "copy";
        num_params = 2;
        cb = (Mod_Func_0*) mod_eflags_copy;
        break;

      case X86G_CC_OP_ADDB:
      case X86G_CC_OP_ADDW:
      case X86G_CC_OP_ADDL:
        op_s = "add";
        num_params = 2;
        cb = (Mod_Func_0*) mod_eflags_add;
        break;

      case X86G_CC_OP_ADCB:
      case X86G_CC_OP_ADCW:
      case X86G_CC_OP_ADCL:
        op_s = "adc";
        num_params = 3;
        cb = (Mod_Func_0*) mod_eflags_adc;
        break;

      case X86G_CC_OP_SUBB:
      case X86G_CC_OP_SUBW:
      case X86G_CC_OP_SUBL:
        op_s = "sub";
        num_params = 2;
        cb = (Mod_Func_0*) mod_eflags_sub;
        break;

      case X86G_CC_OP_SBBB:
      case X86G_CC_OP_SBBW:
      case X86G_CC_OP_SBBL:
        op_s = "sbb";
        num_params = 3;
        cb = (Mod_Func_0*) mod_eflags_sbb;
        break;

      case X86G_CC_OP_LOGICB:
      case X86G_CC_OP_LOGICW: 
      case X86G_CC_OP_LOGICL: 
        op_s = "logic";
        num_params = 2;
        cb = (Mod_Func_0 *)mod_eflags_logic;
        break;

      case X86G_CC_OP_INCB:   
      case X86G_CC_OP_INCW:   
      case X86G_CC_OP_INCL:   
        op_s = "inc";
        num_params = 3;
        cb = (Mod_Func_0*) mod_eflags_inc;
        break;

      case X86G_CC_OP_DECB:   
      case X86G_CC_OP_DECW:   
      case X86G_CC_OP_DECL:
        op_s = "dec";
        num_params = 3;
        cb = (Mod_Func_0 *)mod_eflags_dec;
        break;

      case X86G_CC_OP_SHLB:   
      case X86G_CC_OP_SHLW:   
      case X86G_CC_OP_SHLL:   
        op_s = "shl";
        num_params = 2;
        cb = (Mod_Func_0 *)mod_eflags_shl;
        break;

      case X86G_CC_OP_SHRB:   
      case X86G_CC_OP_SHRW:   
      case X86G_CC_OP_SHRL:   
        op_s = "shr";
        num_params = 2;
        cb = (Mod_Func_0 *)mod_eflags_shr;
        break;

      case X86G_CC_OP_ROLB:   
      case X86G_CC_OP_ROLW:   
      case X86G_CC_OP_ROLL:   
        op_s = "rol";
        num_params = 3;
        cb = (Mod_Func_0 *)mod_eflags_rol;
        break;

      case X86G_CC_OP_RORB:   
      case X86G_CC_OP_RORW:   
      case X86G_CC_OP_RORL:   
        op_s = "ror";
        num_params = 3;
        cb = (Mod_Func_0 *)mod_eflags_ror;
        break;

      case X86G_CC_OP_UMULB:  
      case X86G_CC_OP_UMULW:  
      case X86G_CC_OP_UMULL:  
        op_s = "umul";
        num_params = 2;
        cb = (Mod_Func_0 *)mod_eflags_umul;
        break;

      case X86G_CC_OP_SMULB:
      case X86G_CC_OP_SMULW:
      case X86G_CC_OP_SMULL:
        op_s = "smul";
        num_params = 2;
        cb = (Mod_Func_0 *)mod_eflags_smul;
        break;

      default:
        panic("unhandled cc_op!");
      }
      modify_eflags_helper(op_s, type, ir, num_params, cb);
      return;

    } else {
      Instruction *inst = block->inst;    
      if (inst == NULL) {
        cerr << "Can't modify eflags for undisasmed instr" << endl;
        return;
      }
      string op = get_op_str(prog, inst);
      cerr << "Warning: non-constant cc_op for " << op
	   << ". falling back on old eflag code." << endl;

      // DEBUG
      //ostream_i386_insn(block->inst, cout);


      // FIXME: how to figure out types?
      if ( op.find("rol",0) == 0)
        modify_eflags_helper(op, REG_32, ir, 3, (Mod_Func_0 *)mod_eflags_rol);
      else if ( op.find("ror",0) == 0)
        modify_eflags_helper(op, REG_32, ir, 3, (Mod_Func_0 *)mod_eflags_ror);
      else if ( op.find("shr",0) == 0
                || op.find("sar",0) == 0)
        modify_eflags_helper(op, REG_32, ir, 2, (Mod_Func_0 *)mod_eflags_shr);
      else if ( op.find("shl",0) == 0)
        modify_eflags_helper(op, REG_32, ir, 2, (Mod_Func_0 *)mod_eflags_shl);
      else {
        cerr << "Warning! Flags not handled for " << op 
             << hex 
             << " at " << inst->address
             << endl;
      }


//       if ( op == "sahf" || op == "bsr" || op == "bsf" || op == "popf" ||
//            op == "popfl" || 
//            inst->opcode[0] == 0xc7 && inst->opcode[1] == 0x0f || // cmpxchg8b TMP
//            op == "cmpxchg8b") // VEX calcs flags inline and copies
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_copy);
//         }
//       else if ( op == "add" || op == "addb" || op == "addw" || op == "addl" ||
//                 op == "xadd")
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_add);
//         }
//       else if ( op == "sub" || op == "subb" || op == "subw" || op == "subl" || 
//                 op == "cmp" || op == "cmpb" || op == "cmpw" || op == "cmpl" ||
//                 op == "cmpxchg" ||
//                 op == "neg" || op == "negl" || 
//                 op.find("scas",0) != string::npos || // scas and variants
//                 op.find("cmps",0) != string::npos  // cmps and variants
//                 )
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_sub);
//         }
//       else if ( op == "adc" || op == "adcl" )
//         {
//           modify_eflags_helper(op, ir, 3, (Mod_Func_0 *)mod_eflags_adc);
//         }
//       else if ( op == "sbb" || op == "sbbl" )
//         {
//           modify_eflags_helper(op, ir, 3, (Mod_Func_0 *)mod_eflags_sbb);
//         }   
//       else if ( op == "and"  || op == "or"  || op == "xor"  || op == "test"  ||
//                 op == "andl" || op == "orl" || op == "xorl" || op == "testl" ||
//                 op == "andw" || op == "orw" || op == "xorw" || op == "testw" ||
//                 op == "andb" || op == "orb" || op == "xorb" || op == "testb" )
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_logic);
//         }   
//       else if ( op == "inc" || op == "incl" || op == "incb" || op == "incw" )
//         {
//           modify_eflags_helper(op, ir, 3, (Mod_Func_0 *)mod_eflags_inc);
//         }   
//       else if ( op == "dec" || op == "decl" || op == "decb" || op == "decw" )
//         {
//           modify_eflags_helper(op, ir, 3, (Mod_Func_0 *)mod_eflags_dec);
//         }   
//       else if ( op == "shl" || op == "shll" || op == "shlb")
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_shl);
//         }   
//       else if ( op == "shr" || op == "shrl" || op == "sar" || op == "sarl" ||
//                 op == "shrd" || op == "sarw")
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_shr);
//         }   
//       else if ( op == "rol" )
//         {
//           modify_eflags_helper(op, ir, 3, (Mod_Func_0 *)mod_eflags_rol);
//         }   
//       else if ( op == "ror" )
//         {
//           modify_eflags_helper(op, ir, 3, (Mod_Func_0 *)mod_eflags_ror);
//         }   
//       else if ( op == "mul" || op == "mull" )
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_umul);
//         }
//       else if ( op == "imul" || op == "imull" )
//         {
//           modify_eflags_helper(op, ir, 2, (Mod_Func_0 *)mod_eflags_smul);
//         }
//       else if ( op == "call" ||
//                 op.find("div",0) == 0 || // div + variants
//                 op == "int" ||
//                 op[0] == 'j' || // jmp and all conditional jumps)
//                 op.find(" j",0) != string::npos || // prefixed jmps
//                 op == "lea" ||
//                 op == "leave" ||
//                 op.find("mov") != string::npos || // all movs
//                 op == "nop" ||
//                 op == "not" ||
//                 op == "pop" || op == "popa" || op == "popad" ||
//                 op == "push" || op == "pusha" || op == "pushad" ||
//                 op == "pushl" ||
//                 op == "pushf" || op == "pushfl" ||
//                 op == "rdtsc" || 
//                 op == "ret" ||
//                 op.find("set",0) == 0 || // setCC
//                 op == "std" || // dflag is assigned explicitly in trans.
//                 op.find("stos",0) != string::npos || // stos variants & prefixes
//                 op == "xchg" )
//         {
//           // flags unaffected
//         }
//       else
//         {
//           //panic("No flag handler for \"" + op + "\"!");
//           cerr << "Warning! Flags not handled for " << op 
//                << hex 
//                << " " << (int)inst->opcode[0]
//                << " " << (int)inst->opcode[1]
//                << " " << (int)inst->opcode[2]
//                << endl;
//         }
    }
}




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
#include "libvex_guest_amd64.h"


//
// Register offsets, copied from VEX/priv/guest_amd64_toIR.c
//

#define OFFB_RAX       offsetof(VexGuestAMD64State,guest_RAX)
#define OFFB_RBX       offsetof(VexGuestAMD64State,guest_RBX)
#define OFFB_RCX       offsetof(VexGuestAMD64State,guest_RCX)
#define OFFB_RDX       offsetof(VexGuestAMD64State,guest_RDX)
#define OFFB_RSP       offsetof(VexGuestAMD64State,guest_RSP)
#define OFFB_RBP       offsetof(VexGuestAMD64State,guest_RBP)
#define OFFB_RSI       offsetof(VexGuestAMD64State,guest_RSI)
#define OFFB_RDI       offsetof(VexGuestAMD64State,guest_RDI)
#define OFFB_R8        offsetof(VexGuestAMD64State,guest_R8)
#define OFFB_R9        offsetof(VexGuestAMD64State,guest_R9)
#define OFFB_R10       offsetof(VexGuestAMD64State,guest_R10)
#define OFFB_R11       offsetof(VexGuestAMD64State,guest_R11)
#define OFFB_R12       offsetof(VexGuestAMD64State,guest_R12)
#define OFFB_R13       offsetof(VexGuestAMD64State,guest_R13)
#define OFFB_R14       offsetof(VexGuestAMD64State,guest_R14)
#define OFFB_R15       offsetof(VexGuestAMD64State,guest_R15)

#define OFFB_RIP       offsetof(VexGuestAMD64State,guest_RIP)

#define OFFB_FS_ZERO   offsetof(VexGuestAMD64State,guest_FS_ZERO)
#define OFFB_GS_0x60   offsetof(VexGuestAMD64State,guest_GS_0x60)

#define OFFB_CC_OP     offsetof(VexGuestAMD64State,guest_CC_OP)
#define OFFB_CC_DEP1   offsetof(VexGuestAMD64State,guest_CC_DEP1)
#define OFFB_CC_DEP2   offsetof(VexGuestAMD64State,guest_CC_DEP2)
#define OFFB_CC_NDEP   offsetof(VexGuestAMD64State,guest_CC_NDEP)

#define OFFB_FPREGS    offsetof(VexGuestAMD64State,guest_FPREG[0])
#define OFFB_FPTAGS    offsetof(VexGuestAMD64State,guest_FPTAG[0])
#define OFFB_DFLAG     offsetof(VexGuestAMD64State,guest_DFLAG)
#define OFFB_ACFLAG    offsetof(VexGuestAMD64State,guest_ACFLAG)
#define OFFB_IDFLAG    offsetof(VexGuestAMD64State,guest_IDFLAG)
#define OFFB_FTOP      offsetof(VexGuestAMD64State,guest_FTOP)
#define OFFB_FC3210    offsetof(VexGuestAMD64State,guest_FC3210)
#define OFFB_FPROUND   offsetof(VexGuestAMD64State,guest_FPROUND)

#define OFFB_SSEROUND  offsetof(VexGuestAMD64State,guest_SSEROUND)
#if VEX_VERSION >= 2330
#define OFFB_YMM0      offsetof(VexGuestAMD64State,guest_YMM0)
#define OFFB_YMM1      offsetof(VexGuestAMD64State,guest_YMM1)
#define OFFB_YMM2      offsetof(VexGuestAMD64State,guest_YMM2)
#define OFFB_YMM3      offsetof(VexGuestAMD64State,guest_YMM3)
#define OFFB_YMM4      offsetof(VexGuestAMD64State,guest_YMM4)
#define OFFB_YMM5      offsetof(VexGuestAMD64State,guest_YMM5)
#define OFFB_YMM6      offsetof(VexGuestAMD64State,guest_YMM6)
#define OFFB_YMM7      offsetof(VexGuestAMD64State,guest_YMM7)
#define OFFB_YMM8      offsetof(VexGuestAMD64State,guest_YMM8)
#define OFFB_YMM9      offsetof(VexGuestAMD64State,guest_YMM9)
#define OFFB_YMM10     offsetof(VexGuestAMD64State,guest_YMM10)
#define OFFB_YMM11     offsetof(VexGuestAMD64State,guest_YMM11)
#define OFFB_YMM12     offsetof(VexGuestAMD64State,guest_YMM12)
#define OFFB_YMM13     offsetof(VexGuestAMD64State,guest_YMM13)
#define OFFB_YMM14     offsetof(VexGuestAMD64State,guest_YMM14)
#define OFFB_YMM15     offsetof(VexGuestAMD64State,guest_YMM15)
#define OFFB_YMM16     offsetof(VexGuestAMD64State,guest_YMM16)
#endif

#define OFFB_EMNOTE    offsetof(VexGuestAMD64State,guest_EMNOTE)
#define OFFB_TISTART   offsetof(VexGuestAMD64State,guest_TISTART)
#define OFFB_TILEN     offsetof(VexGuestAMD64State,guest_TILEN)

#define OFFB_NRADDR    offsetof(VexGuestAMD64State,guest_NRADDR)


//
// Sub register offsets, calculated manually
//

#define OFFB_AX         (OFFB_RAX)
#define OFFB_AL         (OFFB_RAX)
#define OFFB_AH         (OFFB_RAX+1)
#define OFFB_BX         (OFFB_RBX)
#define OFFB_BL         (OFFB_RBX)
#define OFFB_BH         (OFFB_RBX+1)
#define OFFB_CX         (OFFB_RCX)
#define OFFB_CL         (OFFB_RCX)
#define OFFB_CH         (OFFB_RCX+1)
#define OFFB_DX         (OFFB_RDX)
#define OFFB_DL         (OFFB_RDX)
#define OFFB_DH         (OFFB_RDX+1)
#define OFFB_DI         (OFFB_RDI)
#define OFFB_SI         (OFFB_RSI)
#define OFFB_BP         (OFFB_RBP)
#define OFFB_SP         (OFFB_RSP)
#define OFFB_EAX        (OFFB_RAX)
#define OFFB_EBX        (OFFB_RBX)
#define OFFB_ECX        (OFFB_RCX)
#define OFFB_EDX        (OFFB_RDX)
#define OFFB_EDI        (OFFB_RDI)
#define OFFB_ESI        (OFFB_RSI)
#define OFFB_EBP        (OFFB_RBP)
#define OFFB_ESP        (OFFB_RSP)
#define OFFB_DIL         (OFFB_RDI)
#define OFFB_SIL        (OFFB_RSI)
#define OFFB_BPL        (OFFB_RBP)
#define OFFB_SPL        (OFFB_RSP)
#define OFFB_R8L        (OFFB_R8)
#define OFFB_R9L        (OFFB_R9)
#define OFFB_R10L       (OFFB_R10)
#define OFFB_R11L       (OFFB_R11)
#define OFFB_R12L       (OFFB_R12)
#define OFFB_R13L       (OFFB_R13)
#define OFFB_R14L       (OFFB_R14)
#define OFFB_R15L       (OFFB_R15)
#define OFFB_R8W        (OFFB_R8)
#define OFFB_R9W        (OFFB_R9)
#define OFFB_R10W       (OFFB_R10)
#define OFFB_R11W       (OFFB_R11)
#define OFFB_R12W       (OFFB_R12)
#define OFFB_R13W       (OFFB_R13)
#define OFFB_R14W       (OFFB_R14)
#define OFFB_R15W       (OFFB_R15)
#define OFFB_R8D        (OFFB_R8)
#define OFFB_R9D        (OFFB_R9)
#define OFFB_R10D       (OFFB_R10)
#define OFFB_R11D       (OFFB_R11)
#define OFFB_R12D       (OFFB_R12)
#define OFFB_R13D       (OFFB_R13)
#define OFFB_R14D       (OFFB_R14)
#define OFFB_R15D       (OFFB_R15)

using namespace std;

//======================================================================
// 
// Helper functions for the translation
//
//======================================================================





vector<VarDecl *> x64_get_reg_decls()
{
  vector<VarDecl *> ret;
  reg_t r64 = REG_64;
  reg_t r32 = REG_32;
  /* reg_t r16 = REG_16; */
  reg_t r8 = REG_8;
  reg_t r1 = REG_1;

  // General purpose 64-bit registers
  ret.push_back(new VarDecl("R_RAX", r64));
  ret.push_back(new VarDecl("R_RCX", r64));
  ret.push_back(new VarDecl("R_RDX", r64));
  ret.push_back(new VarDecl("R_RBX", r64));
  ret.push_back(new VarDecl("R_RSP", r64));
  ret.push_back(new VarDecl("R_RBP", r64));
  ret.push_back(new VarDecl("R_RSI", r64));
  ret.push_back(new VarDecl("R_RDI", r64));
  ret.push_back(new VarDecl("R_R8",  r64));
  ret.push_back(new VarDecl("R_R9",  r64));
  ret.push_back(new VarDecl("R_R10", r64));
  ret.push_back(new VarDecl("R_R11", r64));
  ret.push_back(new VarDecl("R_R12", r64));
  ret.push_back(new VarDecl("R_R13", r64));
  ret.push_back(new VarDecl("R_R14", r64));
  ret.push_back(new VarDecl("R_R15", r64));

  // Program counter
  ret.push_back(new VarDecl("R_RIP", r64));

  // EFLAGS translation thunks
  ret.push_back(new VarDecl("R_CC_OP", r64));  
  ret.push_back(new VarDecl("R_CC_DEP1", r64));  
  ret.push_back(new VarDecl("R_CC_DEP2", r64));  
  ret.push_back(new VarDecl("R_CC_NDEP", r64));  

  // VEX flags
  ret.push_back(new VarDecl("R_DFLAG", r64));
  ret.push_back(new VarDecl("R_ACFLAG", r64));
  ret.push_back(new VarDecl("R_IDFLAG", r64));

  /* Since the most important contents of RFLAGS are in R_CF .. R_OF,
     we don't want to keep a separate copy of those in another
     register. But for the occasional code that uses all of RFLAGS
     (e.g., pushf and popf), we keep a pseudoregister RFLAGSREST
     which has all the bits of RFLAGS except for the 6 main condition
     code bits, which are zero. */
  ret.push_back(new VarDecl("R_RFLAGSREST", r64));
  ret.push_back(new VarDecl("R_CF", r1));
  ret.push_back(new VarDecl("R_PF", r1));  
  ret.push_back(new VarDecl("R_AF", r1));  
  ret.push_back(new VarDecl("R_ZF", r1));  
  ret.push_back(new VarDecl("R_SF", r1));  
  ret.push_back(new VarDecl("R_OF", r1));  

  // VEX represetations of segment information
  ret.push_back(new VarDecl("R_FS_ZERO", r64));
  ret.push_back(new VarDecl("R_GS_0x60", r64));

  // SIMD registers. We don't yet support 256-bit registers, so break
  // them up a 4x64. 
  ret.push_back(new VarDecl("R_SSEROUND", r64));
#if VEX_VERSION >= 2330
  ret.push_back(new VarDecl("R_YMM0_0", r64));
  ret.push_back(new VarDecl("R_YMM0_1", r64));
  ret.push_back(new VarDecl("R_YMM0_2", r64));
  ret.push_back(new VarDecl("R_YMM0_3", r64));
  ret.push_back(new VarDecl("R_YMM1_0", r64));
  ret.push_back(new VarDecl("R_YMM1_1", r64));
  ret.push_back(new VarDecl("R_YMM1_2", r64));
  ret.push_back(new VarDecl("R_YMM1_3", r64));
  ret.push_back(new VarDecl("R_YMM2_0", r64));
  ret.push_back(new VarDecl("R_YMM2_1", r64));
  ret.push_back(new VarDecl("R_YMM2_2", r64));
  ret.push_back(new VarDecl("R_YMM2_3", r64));
  ret.push_back(new VarDecl("R_YMM3_0", r64));
  ret.push_back(new VarDecl("R_YMM3_1", r64));
  ret.push_back(new VarDecl("R_YMM3_2", r64));
  ret.push_back(new VarDecl("R_YMM3_3", r64));
  ret.push_back(new VarDecl("R_YMM4_0", r64));
  ret.push_back(new VarDecl("R_YMM4_1", r64));
  ret.push_back(new VarDecl("R_YMM4_2", r64));
  ret.push_back(new VarDecl("R_YMM4_3", r64));
  ret.push_back(new VarDecl("R_YMM5_0", r64));
  ret.push_back(new VarDecl("R_YMM5_1", r64));
  ret.push_back(new VarDecl("R_YMM5_2", r64));
  ret.push_back(new VarDecl("R_YMM5_3", r64));
  ret.push_back(new VarDecl("R_YMM6_0", r64));
  ret.push_back(new VarDecl("R_YMM6_1", r64));
  ret.push_back(new VarDecl("R_YMM6_2", r64));
  ret.push_back(new VarDecl("R_YMM6_3", r64));
  ret.push_back(new VarDecl("R_YMM7_0", r64));
  ret.push_back(new VarDecl("R_YMM7_1", r64));
  ret.push_back(new VarDecl("R_YMM7_2", r64));
  ret.push_back(new VarDecl("R_YMM7_3", r64));
  ret.push_back(new VarDecl("R_YMM8_0", r64));
  ret.push_back(new VarDecl("R_YMM8_1", r64));
  ret.push_back(new VarDecl("R_YMM8_2", r64));
  ret.push_back(new VarDecl("R_YMM8_3", r64));
  ret.push_back(new VarDecl("R_YMM9_0", r64));
  ret.push_back(new VarDecl("R_YMM9_1", r64));
  ret.push_back(new VarDecl("R_YMM9_2", r64));
  ret.push_back(new VarDecl("R_YMM9_3", r64));
  ret.push_back(new VarDecl("R_YMM10_0", r64));
  ret.push_back(new VarDecl("R_YMM10_1", r64));
  ret.push_back(new VarDecl("R_YMM10_2", r64));
  ret.push_back(new VarDecl("R_YMM10_3", r64));
  ret.push_back(new VarDecl("R_YMM11_0", r64));
  ret.push_back(new VarDecl("R_YMM11_1", r64));
  ret.push_back(new VarDecl("R_YMM11_2", r64));
  ret.push_back(new VarDecl("R_YMM11_3", r64));
  ret.push_back(new VarDecl("R_YMM12_0", r64));
  ret.push_back(new VarDecl("R_YMM12_1", r64));
  ret.push_back(new VarDecl("R_YMM12_2", r64));
  ret.push_back(new VarDecl("R_YMM12_3", r64));
  ret.push_back(new VarDecl("R_YMM13_0", r64));
  ret.push_back(new VarDecl("R_YMM13_1", r64));
  ret.push_back(new VarDecl("R_YMM13_2", r64));
  ret.push_back(new VarDecl("R_YMM13_3", r64));
  ret.push_back(new VarDecl("R_YMM14_0", r64));
  ret.push_back(new VarDecl("R_YMM14_1", r64));
  ret.push_back(new VarDecl("R_YMM14_2", r64));
  ret.push_back(new VarDecl("R_YMM14_3", r64));
  ret.push_back(new VarDecl("R_YMM15_0", r64));
  ret.push_back(new VarDecl("R_YMM15_1", r64));
  ret.push_back(new VarDecl("R_YMM15_2", r64));
  ret.push_back(new VarDecl("R_YMM15_3", r64));
  ret.push_back(new VarDecl("R_YMM16_0", r64)); /* YMM16 exists only in */
  ret.push_back(new VarDecl("R_YMM16_1", r64)); /* VEX's imagination */
  ret.push_back(new VarDecl("R_YMM16_2", r64));
  ret.push_back(new VarDecl("R_YMM16_3", r64));
#endif

  // x87-style floating point
  ret.push_back(new VarDecl("R_FTOP", r32));
  ret.push_back(new VarDecl("R_FPREG0", r64));
  ret.push_back(new VarDecl("R_FPREG1", r64));
  ret.push_back(new VarDecl("R_FPREG2", r64));
  ret.push_back(new VarDecl("R_FPREG3", r64));
  ret.push_back(new VarDecl("R_FPREG4", r64));
  ret.push_back(new VarDecl("R_FPREG5", r64));
  ret.push_back(new VarDecl("R_FPREG6", r64));
  ret.push_back(new VarDecl("R_FPREG7", r64));
  ret.push_back(new VarDecl("R_FPTAG0", r8));
  ret.push_back(new VarDecl("R_FPTAG1", r8));
  ret.push_back(new VarDecl("R_FPTAG2", r8));
  ret.push_back(new VarDecl("R_FPTAG3", r8));
  ret.push_back(new VarDecl("R_FPTAG4", r8));
  ret.push_back(new VarDecl("R_FPTAG5", r8));
  ret.push_back(new VarDecl("R_FPTAG6", r8));
  ret.push_back(new VarDecl("R_FPTAG7", r8));
  ret.push_back(new VarDecl("R_FPROUND", r64));
  ret.push_back(new VarDecl("R_FC3210", r64));

  // Some other random VEX things
  ret.push_back(new VarDecl("R_EMNOTE", r32));
  ret.push_back(new VarDecl("R_NRADDR", r64));
  ret.push_back(new VarDecl("R_SC_CLASS", r64));
  ret.push_back(new VarDecl("R_IP_AT_SYSCALL", r64));

  return ret;
}

IRStmt* x64_make_pc_put_stmt(Addr64 addr) {
  return IRStmt_Put(OFFB_RIP, IRExpr_Const(IRConst_U32((UInt)addr)));
}

//----------------------------------------------------------------------
// Translate VEX IR offset into x64 register name
// This is only called for 64-bit registers.
//----------------------------------------------------------------------
static string reg_offset_to_name( int offset, bool *is_good )
{
    assert(offset >= 0);

    const char *name;
    bool good;

    switch ( offset )
    {

        case OFFB_RAX:      name = "RAX";       good=true; break;
        case OFFB_RBX:      name = "RBX";       good=true; break;
        case OFFB_RCX:      name = "RCX";       good=true; break;
        case OFFB_RDX:      name = "RDX";       good=true; break;
        case OFFB_RSP:      name = "RSP";       good=true; break;
        case OFFB_RBP:      name = "RBP";       good=true; break;
        case OFFB_RSI:      name = "RSI";       good=true; break;
        case OFFB_RDI:      name = "RDI";       good=true; break;
        case OFFB_R8:       name = "R8";        good=true; break;
        case OFFB_R9:       name = "R9";        good=true; break;
        case OFFB_R10:      name = "R10";       good=true; break;
        case OFFB_R11:      name = "R11";       good=true; break;
        case OFFB_R12:      name = "R12";       good=true; break;
        case OFFB_R13:      name = "R13";       good=true; break;
        case OFFB_R14:      name = "R14";       good=true; break;
        case OFFB_R15:      name = "R15";       good=true; break;

        case OFFB_RIP:      name = "RIP";       good=true; break;

        case OFFB_CC_OP:    name = "CC_OP";     good=true; break;
        case OFFB_CC_DEP1:  name = "CC_DEP1";   good=true; break;
        case OFFB_CC_DEP2:  name = "CC_DEP2";   good=true; break;
        case OFFB_CC_NDEP:  name = "CC_NDEP";   good=true; break;

        case OFFB_DFLAG:    name = "DFLAG";     good=true; break;
        case OFFB_IDFLAG:   name = "IDFLAG";    good=true; break;
        case OFFB_ACFLAG:   name = "ACFLAG";    good=true; break;
        case OFFB_FC3210:   name = "FC3210";    good=true; break;
        case OFFB_FPROUND:  name = "FPROUND";   good=true; break;

        case OFFB_SSEROUND: name = "SSEROUND";  good=true; break;

        case OFFB_NRADDR:   name = "NRADDR";    good=true; break;

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

#if VEX_VERSION >= 2330
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM16+32) {
	// SSE sub-register: not supported.
	return new Unknown("Unhandled 8-bit YMM lane");
    }
#endif

    // Determine which 64-bit register this 8-bit sub
    // register is a part of
    switch ( offset )
    {
        case OFFB_AL:   name = "RAX";    low = true;    break;
        case OFFB_AH:   name = "RAX";    low = false;   break;
        case OFFB_BL:   name = "RBX";    low = true;    break;
        case OFFB_BH:   name = "RBX";    low = false;   break;
        case OFFB_CL:   name = "RCX";    low = true;    break;
        case OFFB_CH:   name = "RCX";    low = false;   break;
        case OFFB_DL:   name = "RDX";    low = true;    break;
        case OFFB_DH:   name = "RDX";    low = false;   break;
        case OFFB_DIL:  name = "RDI";    low = true;    break;
        case OFFB_SIL:  name = "RSI";    low = true;    break;
        case OFFB_SPL:  name = "RSP";    low = true;    break;
        case OFFB_BPL:  name = "RBP";    low = true;    break;
        case OFFB_R8L:  name = "R8";     low = true;    break;
        case OFFB_R9L:  name = "R9";     low = true;    break;
        case OFFB_R10L: name = "R10";    low = true;    break;
        case OFFB_R11L: name = "R11";    low = true;    break;
        case OFFB_R12L: name = "R12";    low = true;    break;
        case OFFB_R13L: name = "R13";    low = true;    break;
        case OFFB_R14L: name = "R14";    low = true;    break;
        case OFFB_R15L: name = "R16";    low = true;    break;
        default:
	    assert(0);
    }   

    // Create the corresponding named register
    Temp *reg = mk_reg(name, REG_64);
    

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

#if VEX_VERSION >= 2330
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM16+64) {
	// SSE sub-register: not supported.
	return new Unknown("Unhandled 16-bit YMM lane");
    }
#endif

    switch ( offset )
    {
        //
        // These are 16 bit sub registers
        //
        case OFFB_AX:   name = "RAX";   sub = true; break;
        case OFFB_BX:   name = "RBX";   sub = true; break;
        case OFFB_CX:   name = "RCX";   sub = true; break;
        case OFFB_DX:   name = "RDX";   sub = true; break;
        case OFFB_DI:   name = "RDI";   sub = true; break;
        case OFFB_SI:   name = "RSI";   sub = true; break;
        case OFFB_BP:   name = "RBP";   sub = true; break;
        case OFFB_SP:   name = "RSP";   sub = true; break;
        case OFFB_R8W:  name = "R8";    sub = true; break;
        case OFFB_R9W:  name = "R9";    sub = true; break;
        case OFFB_R10W: name = "R10";   sub = true; break;
        case OFFB_R11W: name = "R11";   sub = true; break;
        case OFFB_R12W: name = "R12";   sub = true; break;
        case OFFB_R13W: name = "R13";   sub = true; break;
        case OFFB_R14W: name = "R14";   sub = true; break;
        case OFFB_R15W: name = "R15";   sub = true; break;

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

static Exp *translate_get_reg_32( unsigned int offset )
{
    string name;
    bool sub;

#if VEX_VERSION >= 2330
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM16+64) {
	// SSE sub-register: not supported.
	return new Unknown("Unhandled 32-bit YMM lane");
    }
#endif

    switch ( offset )
    {
        //
        // These are 32-bit sub registers
        //
        case OFFB_EAX:  name = "RAX";   sub = true; break;
        case OFFB_EBX:  name = "RBX";   sub = true; break;
        case OFFB_ECX:  name = "RCX";   sub = true; break;
        case OFFB_EDX:  name = "RDX";   sub = true; break;
        case OFFB_EDI:  name = "RDI";   sub = true; break;
        case OFFB_ESI:  name = "RSI";   sub = true; break;
        case OFFB_EBP:  name = "RBP";   sub = true; break;
        case OFFB_ESP:  name = "RSP";   sub = true; break;
        case OFFB_R8D:  name = "R8";    sub = true; break;
        case OFFB_R9D:  name = "R9";    sub = true; break;
        case OFFB_R10D: name = "R10";   sub = true; break;
        case OFFB_R11D: name = "R11";   sub = true; break;
        case OFFB_R12D: name = "R12";   sub = true; break;
        case OFFB_R13D: name = "R13";   sub = true; break;
        case OFFB_R14D: name = "R14";   sub = true; break;
        case OFFB_R15D: name = "R15";   sub = true; break;

        default:
	    assert(0);
    }

    Exp *value = NULL;

    if ( sub )
    {
        Temp *reg = mk_reg(name, REG_64);

        value = new Cast(reg, REG_32, CAST_LOW);
    }
    else
    {
        Temp *reg = mk_reg(name, REG_32);

        value = reg;
    }

    return value;
}

static Exp *translate_get_reg_64( int offset )
{
    assert(offset >= 0);

    bool is_good;
    string name = reg_offset_to_name(offset, &is_good);
    Exp *result;

    if (is_good)
        result = mk_reg(name, REG_64);
    else
        result = new Unknown("Unknown 64-bit register " + name);

    return result;
}

Exp *x64_translate_get( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
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
    else if ( type == Ity_I64 )
    {
        result = translate_get_reg_64(offset);
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

Stmt *x64_translate_dirty( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    Stmt *result = NULL;

    IRDirty *dirty = stmt->Ist.Dirty.details;

    string func = string(dirty->cee->name);

    {
        result = new ExpStmt(new Unknown("Unknown: Dirty"));
    }
    return result;
}

Exp *x64_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *result = NULL;
    string func = string(expr->Iex.CCall.cee->name);

    {
        result = new Unknown("CCall: " + func);
    }

    return result;
}


static Stmt *translate_put_reg_8( unsigned int offset, Exp *data, IRSB *irbb )
{
    assert(data);

    bool low;
    string name;
    Temp *reg;

#if VEX_VERSION >= 2330
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM16+64) {
	// SSE sub-register: not supported.
	Exp::destroy(data);
	return new Special("Unhandled store to 8-bit YMM lane");
    }
#endif

    // Determine which 32 bit register this 8 bit sub
    // register is a part of
    switch ( offset )
    {
        case OFFB_AL:   name = "RAX";    low = true;    break;
        case OFFB_AH:   name = "RAX";    low = false;   break;
        case OFFB_BL:   name = "RBX";    low = true;    break;
        case OFFB_BH:   name = "RBX";    low = false;   break;
        case OFFB_CL:   name = "RCX";    low = true;    break;
        case OFFB_CH:   name = "RCX";    low = false;   break;
        case OFFB_DL:   name = "RDX";    low = true;    break;
        case OFFB_DH:   name = "RDX";    low = false;   break;
        case OFFB_DIL:  name = "RDI";    low = true;    break;
        case OFFB_SIL:  name = "RSI";    low = true;    break;
        case OFFB_SPL:  name = "RSP";    low = true;    break;
        case OFFB_BPL:  name = "RBP";    low = true;    break;
        case OFFB_R8L:  name = "R8";     low = true;    break;
        case OFFB_R9L:  name = "R9";     low = true;    break;
        case OFFB_R10L: name = "R10";    low = true;    break;
        case OFFB_R11L: name = "R11";    low = true;    break;
        case OFFB_R12L: name = "R12";    low = true;    break;
        case OFFB_R13L: name = "R13";    low = true;    break;
        case OFFB_R14L: name = "R14";    low = true;    break;
        case OFFB_R15L: name = "R16";    low = true;    break;

        default:
	    assert(0);
    }   

    // Create the corresponding named register
    reg = mk_reg(name, REG_64);

    Exp *masked = NULL;
    Exp *value = NULL;

    // Assignment to 8 bit sub registers use a combination of bit
    // shifting and masking on the corresponding 32 bit registers
    // to achieve the effect. 
    if ( low )
    {
        masked = new BinOp(BITAND, reg, ex_const64(0xffffffffffffff00ULL));
        value = new Cast(data, REG_64, CAST_UNSIGNED);
    }
    else
    {
        masked = new BinOp(BITAND, reg, ex_const64(0xffffffffffff00ffULL));
        value = new BinOp(LSHIFT, new Cast(data, REG_64, CAST_UNSIGNED), ex_const(8));
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

#if VEX_VERSION >= 2330
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM7+64) {
	// SSE sub-register: not supported.
	Exp::destroy(data);
	return new Special("Unhandled store to 16-bit YMM lane");
    }
#endif

    switch ( offset )
    {
        //
        // These are 16 bit sub registers
        //
        case OFFB_AX:   name = "RAX";   sub = true; break;
        case OFFB_BX:   name = "RBX";   sub = true; break;
        case OFFB_CX:   name = "RCX";   sub = true; break;
        case OFFB_DX:   name = "RDX";   sub = true; break;
        case OFFB_DI:   name = "RDI";   sub = true; break;
        case OFFB_SI:   name = "RSI";   sub = true; break;
        case OFFB_BP:   name = "RBP";   sub = true; break;
        case OFFB_SP:   name = "RSP";   sub = true; break;
        case OFFB_R8W:  name = "R8";    sub = true; break;
        case OFFB_R9W:  name = "R9";    sub = true; break;
        case OFFB_R10W: name = "R10";   sub = true; break;
        case OFFB_R11W: name = "R11";   sub = true; break;
        case OFFB_R12W: name = "R12";   sub = true; break;
        case OFFB_R13W: name = "R13";   sub = true; break;
        case OFFB_R14W: name = "R14";   sub = true; break;
        case OFFB_R15W: name = "R15";   sub = true; break;

        default:
	    assert(0);
    }

    Exp *masked;
    Exp *value;
    
    if ( sub )
    {
        reg = mk_reg(name, REG_64);

        masked = new BinOp(BITAND, new Temp(*reg),\
			   ex_const64(0xffffffffffff0000ULL));
        value = new Cast(data, REG_64, CAST_UNSIGNED);

        value = new BinOp(BITOR, masked, value);
    }
    else
    {
        reg = mk_reg(name, REG_16);

        value = data;
    }

    return new Move( reg, value );
}

static Stmt *translate_put_reg_32( unsigned int offset, Exp *data, IRSB *irbb )
{
    assert(data);

    string name;
    bool sub;
    Temp *reg;

#if VEX_VERSION >= 2330
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM7+64) {
	// SSE sub-register: not supported.
	Exp::destroy(data);
	return new Special("Unhandled store to 32-bit YMM lane");
    }
#endif

    switch ( offset )
    {
        case OFFB_EAX:  name = "RAX";   sub = true; break;
        case OFFB_EBX:  name = "RBX";   sub = true; break;
        case OFFB_ECX:  name = "RCX";   sub = true; break;
        case OFFB_EDX:  name = "RDX";   sub = true; break;
        case OFFB_EDI:  name = "RDI";   sub = true; break;
        case OFFB_ESI:  name = "RSI";   sub = true; break;
        case OFFB_EBP:  name = "RBP";   sub = true; break;
        case OFFB_ESP:  name = "RSP";   sub = true; break;
        case OFFB_R8D:  name = "R8";    sub = true; break;
        case OFFB_R9D:  name = "R9";    sub = true; break;
        case OFFB_R10D: name = "R10";   sub = true; break;
        case OFFB_R11D: name = "R11";   sub = true; break;
        case OFFB_R12D: name = "R12";   sub = true; break;
        case OFFB_R13D: name = "R13";   sub = true; break;
        case OFFB_R14D: name = "R14";   sub = true; break;
        case OFFB_R15D: name = "R15";   sub = true; break;

        default:
	    assert(0);
    }

    Exp *masked;
    Exp *value;
    
    if ( sub )
    {
        reg = mk_reg(name, REG_64);

        masked = new BinOp(BITAND, new Temp(*reg),\
			   ex_const64(0xffffffff00000000ULL));
        value = new Cast(data, REG_64, CAST_UNSIGNED);

        value = new BinOp(BITOR, masked, value);
    }
    else
    {
        reg = mk_reg(name, REG_32);

        value = data;
    }

    return new Move( reg, value );
}

static Stmt *translate_put_reg_64( int offset, Exp *data, IRSB *irbb )
{
    assert(data);
    
    bool is_good;
    string name = reg_offset_to_name(offset, &is_good);

    Stmt *st;

    if (is_good)
    {
        Temp *reg = mk_reg(name, REG_64);
        st = new Move( reg, data );
    }
    else
    {
	Exp::destroy(data);
        st = new Special("Unknown 64-bit register " + name);
    }
    
    return st;
}

Stmt *x64_translate_put( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
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

    //
    // Regular 64-bit registers
    //
    else if ( type == Ity_I64 )
    {
        result = translate_put_reg_64(offset, data, irbb);
    }

    else
    {
	Exp::destroy(data);
        result = new Special("Unrecognized register type");
    }

    return result;
}

//======================================================================
//
// Code that deals with the setting of EFLAGS 
//
//======================================================================


void x64_modify_flags( asm_program_t *prog, vine_block_t *block )
{
    assert(block);

    /* to implement */
}




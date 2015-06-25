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
#define OFFB_IDFLAG    offsetof(VexGuestAMD64State,guest_IDFLAG)
#if VEX_VERSION >= 2050
#define OFFB_ACFLAG    offsetof(VexGuestAMD64State,guest_ACFLAG)
#endif
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
  ret.push_back(new VarDecl("R_IDFLAG", r64));
#if VEX_VERSION >= 2050
  ret.push_back(new VarDecl("R_ACFLAG", r64));
#endif

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
#if VEX_VERSION >= 2050
        case OFFB_ACFLAG:   name = "ACFLAG";    good=true; break;
#endif
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

/* These enumerated definitions are copied from VEX's
   priv/guest_amd64_defs.h, but since they matche the architectural
   encoding, we can hope they're not likely to change. */
enum {
  AMD64CondO      = 0,  /* overflow           */
  AMD64CondNO     = 1,  /* no overflow        */

  AMD64CondB      = 2,  /* below              */
  AMD64CondNB     = 3,  /* not below          */

  AMD64CondZ      = 4,  /* zero               */
  AMD64CondNZ     = 5,  /* not zero           */

  AMD64CondBE     = 6,  /* below or equal     */
  AMD64CondNBE    = 7,  /* not below or equal */

  AMD64CondS      = 8,  /* negative           */
  AMD64CondNS     = 9,  /* not negative       */

  AMD64CondP      = 10, /* parity even        */
  AMD64CondNP     = 11, /* not parity even    */

  AMD64CondL      = 12, /* jump less          */
  AMD64CondNL     = 13, /* not less           */

  AMD64CondLE     = 14, /* less or equal      */
  AMD64CondNLE    = 15, /* not less or equal  */

  AMD64CondAlways = 16  /* HACK */
};

#define CC_SHIFT_O   11
#define CC_SHIFT_S   7
#define CC_SHIFT_Z   6
#define CC_SHIFT_A   4
#define CC_SHIFT_C   0
#define CC_SHIFT_P   2

#define CC_MASK_O    (1ULL << CC_SHIFT_O)
#define CC_MASK_S    (1ULL << CC_SHIFT_S)
#define CC_MASK_Z    (1ULL << CC_SHIFT_Z)
#define CC_MASK_A    (1ULL << CC_SHIFT_A)
#define CC_MASK_C    (1ULL << CC_SHIFT_C)
#define CC_MASK_P    (1ULL << CC_SHIFT_P)

/* This enumerated definition is also copied from the same place; alas
   there's no specific reason to be sure it won't change in the
   future, in which case we may need more #ifdefs. It has been pretty
   stable so far, though. */

enum {
    CC_OP_COPY=0,  /* DEP1 = current flags, DEP2 = 0, NDEP = unused */
                          /* just copy DEP1 to output */

    CC_OP_ADDB,    /* 1 */
    CC_OP_ADDW,    /* 2 DEP1 = argL, DEP2 = argR, NDEP = unused */
    CC_OP_ADDL,    /* 3 */
    CC_OP_ADDQ,    /* 4 */

    CC_OP_SUBB,    /* 5 */
    CC_OP_SUBW,    /* 6 DEP1 = argL, DEP2 = argR, NDEP = unused */
    CC_OP_SUBL,    /* 7 */
    CC_OP_SUBQ,    /* 8 */

    CC_OP_ADCB,    /* 9 */
    CC_OP_ADCW,    /* 10 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    CC_OP_ADCL,    /* 11 */
    CC_OP_ADCQ,    /* 12 */

    CC_OP_SBBB,    /* 13 */
    CC_OP_SBBW,    /* 14 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    CC_OP_SBBL,    /* 15 */
    CC_OP_SBBQ,    /* 16 */

    CC_OP_LOGICB,  /* 17 */
    CC_OP_LOGICW,  /* 18 DEP1 = result, DEP2 = 0, NDEP = unused */
    CC_OP_LOGICL,  /* 19 */
    CC_OP_LOGICQ,  /* 20 */

    CC_OP_INCB,    /* 21 */
    CC_OP_INCW,    /* 22 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    CC_OP_INCL,    /* 23 */
    CC_OP_INCQ,    /* 24 */

    CC_OP_DECB,    /* 25 */
    CC_OP_DECW,    /* 26 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    CC_OP_DECL,    /* 27 */
    CC_OP_DECQ,    /* 28 */

    CC_OP_SHLB,    /* 29 DEP1 = res, DEP2 = res', NDEP = unused */
    CC_OP_SHLW,    /* 30 where res' is like res but shifted one bit less */
    CC_OP_SHLL,    /* 31 */
    CC_OP_SHLQ,    /* 32 */

    CC_OP_SHRB,    /* 33 DEP1 = res, DEP2 = res', NDEP = unused */
    CC_OP_SHRW,    /* 34 where res' is like res but shifted one bit less */
    CC_OP_SHRL,    /* 35 */
    CC_OP_SHRQ,    /* 36 */

    CC_OP_ROLB,    /* 37 */
    CC_OP_ROLW,    /* 38 DEP1 = res, DEP2 = 0, NDEP = old flags */
    CC_OP_ROLL,    /* 39 */
    CC_OP_ROLQ,    /* 40 */

    CC_OP_RORB,    /* 41 */
    CC_OP_RORW,    /* 42 DEP1 = res, DEP2 = 0, NDEP = old flags */
    CC_OP_RORL,    /* 43 */
    CC_OP_RORQ,    /* 44 */

    CC_OP_UMULB,   /* 45 */
    CC_OP_UMULW,   /* 46 DEP1 = argL, DEP2 = argR, NDEP = unused */
    CC_OP_UMULL,   /* 47 */
    CC_OP_UMULQ,   /* 48 */

    CC_OP_SMULB,   /* 49 */
    CC_OP_SMULW,   /* 50 DEP1 = argL, DEP2 = argR, NDEP = unused */
    CC_OP_SMULL,   /* 51 */
    CC_OP_SMULQ,   /* 52 */

    CC_OP_ANDN32,  /* 53 */
    CC_OP_ANDN64,  /* 54 DEP1 = res, DEP2 = 0, NDEP = unused */

    CC_OP_BLSI32,  /* 55 */
    CC_OP_BLSI64,  /* 56 DEP1 = res, DEP2 = arg, NDEP = unused */

    CC_OP_BLSMSK32,/* 57 */
    CC_OP_BLSMSK64,/* 58 DEP1 = res, DEP2 = arg, NDEP = unused */

    CC_OP_BLSR32,  /* 59 */
    CC_OP_BLSR64,  /* 60 DEP1 = res, DEP2 = arg, NDEP = unused */

    CC_OP_NUMBER
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
// This is a clone of code from irtoir-arm.c, in turn from
// irtoir-i386.c. It would be good to reunify them.
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

/* e can be anything from a REG_8 to a REG_64, but only the partity of the
   low 8 bits is computed, like the x64 PF flag. */
Exp *parity8(Exp *e) {
    Exp *shift0 = ecl(e);
    Exp *shift1 = ex_shr(e, new Constant(REG_32, 1));
    Exp *shift2 = ex_shr(e, new Constant(REG_32, 2));
    Exp *shift3 = ex_shr(e, new Constant(REG_32, 3));
    Exp *shift4 = ex_shr(e, new Constant(REG_32, 4));
    Exp *shift5 = ex_shr(e, new Constant(REG_32, 5));
    Exp *shift6 = ex_shr(e, new Constant(REG_32, 6));
    Exp *shift7 = ex_shr(e, new Constant(REG_32, 7));
    Exp *xor_e = _ex_xor(shift7, shift6, shift5, shift4,
			 shift3, shift2, shift1, shift0);
    return _ex_not(_ex_l_cast(xor_e, REG_1));
}

void x64_modify_flags( asm_program_t *prog, vine_block_t *block )
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
        if(op_mov->rhs->exp_type == CONSTANT) {
            Constant *op_const = (Constant*)op_mov->rhs;
            op = op_const->val;
            got_op = true;
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

    Exp *dep1_expr, *dep2_expr;
    Exp *ndep_expr = 0;
    assert(dep1_st != -1);
    assert(dep2_st != -1);
    if (mux0x_st == -1) {
        // Unconditional case
        assert(ir->at(dep1_st)->stmt_type == MOVE);
        dep1_expr = ((Move *)(ir->at(dep1_st)))->rhs;

        assert(ir->at(dep2_st)->stmt_type == MOVE);
        dep2_expr = ((Move *)(ir->at(dep2_st)))->rhs;

	if (ndep_st != -1) {
	    assert(ir->at(ndep_st)->stmt_type == MOVE);
	    ndep_expr = ((Move *)(ir->at(ndep_st)))->rhs;
	}
    } else {
        // Conditional case
        Exp *cond, *exp_t, *exp_f, *res;
        int matched;
        matched = match_ite(ir, dep1_st - 3, &cond, &exp_t, &exp_f, &res);
        assert(matched != -1);
        assert(exp_f);
        dep1_expr = exp_f;

        matched = match_ite(ir, dep2_st - 3, &cond, &exp_t, &exp_f, &res);
        assert(matched != -1);
        assert(exp_f);
        dep2_expr = exp_f;

	if (ndep_st != -1) {
	    matched = match_ite(ir, ndep_st - 3, &cond, &exp_t, &exp_f, &res);
	    assert(matched != -1);
	    assert(exp_f);
	    ndep_expr = exp_f;
	}
    }
    /* All the code above here was cloned from arm_modify_flags. */

    Temp *CF = new Temp(REG_1, "R_CF");
    Temp *PF = new Temp(REG_1, "R_PF");
    Temp *AF = new Temp(REG_1, "R_AF");
    Temp *ZF = new Temp(REG_1, "R_ZF");
    Temp *SF = new Temp(REG_1, "R_SF");
    Temp *OF = new Temp(REG_1, "R_OF");

    vector<Stmt *> new_ir;
    Exp *cf, *pf, *af, *zf, *sf, *of;

    switch (op) {
    case CC_OP_COPY:
	cf = ex_get_bit(dep1_expr, CC_SHIFT_C);
	pf = ex_get_bit(dep1_expr, CC_SHIFT_P);
	af = ex_get_bit(dep1_expr, CC_SHIFT_A);
	zf = ex_get_bit(dep1_expr, CC_SHIFT_Z);
	sf = ex_get_bit(dep1_expr, CC_SHIFT_S);
	of = ex_get_bit(dep1_expr, CC_SHIFT_O);
	break;
    case CC_OP_LOGICL:
	cf = ecl(&Constant::f);
	of = ecl(&Constant::f);
	af = 0;
	zf = _ex_eq(ecl(dep1_expr), ex_const(REG_64, 0));
	sf = _ex_h_cast(ex_l_cast(dep1_expr, REG_32), REG_1);
	pf = parity8(dep1_expr);
	break;
    default:
	printf("Unsupported x86-64 CC OP %d\n", op);
        /* panic("Unsupported x86-64 CC OP in x64_modify_flags"); */
	return; /* also a memory leak */
    }

    /* For now, don't try to remove the thunk, just like the x86 code
       doesn't. */
    int insert_loc = max(max(op_st, dep1_st), max(dep2_st, ndep_st));
    if (mux0x_st != -1) {
	// Looks like a conditional thunk; make new conditional
	// assignments using the same condition
	int start = mux0x_st - 2;
	assert(start >= 0);
	Exp *cond, *exp_t, *exp_f, *res;
	int matched = match_ite(ir, mux0x_st, &cond, &exp_t, &exp_f, &res);
	assert(matched != -1);
	assert(cond);
	Exp *ite;
	if (cf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(CF), cf);
	    new_ir.push_back(new Move(CF, ite));
	}
	if (pf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(PF), pf);
	    new_ir.push_back(new Move(PF, ite));
	}
	if (af) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(AF), af);
	    new_ir.push_back(new Move(AF, ite));
	}
	if (zf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(ZF), zf);
	    new_ir.push_back(new Move(ZF, ite));
	}
	if (sf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(SF), sf);
	    new_ir.push_back(new Move(SF, ite));
	}
	if (of) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), ecl(OF), of);
	    new_ir.push_back(new Move(OF, ite));
	}
    } else {
	if (cf)
	    new_ir.push_back(new Move(CF, cf));
	if (pf)
	    new_ir.push_back(new Move(PF, pf));
	if (af)
	    new_ir.push_back(new Move(AF, af));
	if (zf)
	    new_ir.push_back(new Move(ZF, zf));
	if (sf)
	    new_ir.push_back(new Move(SF, sf));
	if (of)
	    new_ir.push_back(new Move(OF, of));
    }
    ir->insert(ir->begin() + insert_loc, new_ir.begin(), new_ir.end());
}




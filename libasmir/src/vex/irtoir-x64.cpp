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

#if VEX_VERSION < 3043
#define OFFB_FS_CONST   offsetof(VexGuestAMD64State,guest_FS_ZERO)
#define OFFB_GS_CONST   offsetof(VexGuestAMD64State,guest_GS_0x60)
#else
#define OFFB_FS_CONST   offsetof(VexGuestAMD64State,guest_FS_CONST)
#define OFFB_GS_CONST   offsetof(VexGuestAMD64State,guest_GS_CONST)
#endif

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
#else
/* Older versions of VEX only modeled the SSE state as 128-bit
   XMM registers, like x86-32. We'll try to cover up for this
   difference, but the YMM-style code path is better tested. */
#define OFFB_XMM0      offsetof(VexGuestAMD64State,guest_XMM0)
#define OFFB_XMM1      offsetof(VexGuestAMD64State,guest_XMM1)
#define OFFB_XMM2      offsetof(VexGuestAMD64State,guest_XMM2)
#define OFFB_XMM3      offsetof(VexGuestAMD64State,guest_XMM3)
#define OFFB_XMM4      offsetof(VexGuestAMD64State,guest_XMM4)
#define OFFB_XMM5      offsetof(VexGuestAMD64State,guest_XMM5)
#define OFFB_XMM6      offsetof(VexGuestAMD64State,guest_XMM6)
#define OFFB_XMM7      offsetof(VexGuestAMD64State,guest_XMM7)
#define OFFB_XMM8      offsetof(VexGuestAMD64State,guest_XMM8)
#define OFFB_XMM9      offsetof(VexGuestAMD64State,guest_XMM9)
#define OFFB_XMM10     offsetof(VexGuestAMD64State,guest_XMM10)
#define OFFB_XMM11     offsetof(VexGuestAMD64State,guest_XMM11)
#define OFFB_XMM12     offsetof(VexGuestAMD64State,guest_XMM12)
#define OFFB_XMM13     offsetof(VexGuestAMD64State,guest_XMM13)
#define OFFB_XMM14     offsetof(VexGuestAMD64State,guest_XMM14)
#define OFFB_XMM15     offsetof(VexGuestAMD64State,guest_XMM15)
#define OFFB_XMM16     offsetof(VexGuestAMD64State,guest_XMM16)
#endif

#if VEX_VERSION < 2484
#define OFFB_EMNOTE    offsetof(VexGuestAMD64State,guest_EMWARN)
#else
#define OFFB_EMNOTE    offsetof(VexGuestAMD64State,guest_EMNOTE)
#endif

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
  ret.push_back(new VarDecl("R_FS_CONST", r64));
  ret.push_back(new VarDecl("R_GS_CONST", r64));

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

#if VEX_VERSION >= 919
        case OFFB_FS_CONST: name = "FS_BASE";   good=true; break;
#endif
#if VEX_VERSION >= 1874
        case OFFB_GS_CONST: name = "GS_BASE";   good=true; break;
#endif

#if VEX_VERSION >= 2330
        case OFFB_YMM0:     name = "YMM0_0";    good=true; break;
        case OFFB_YMM1:     name = "YMM1_0";    good=true; break;
        case OFFB_YMM2:     name = "YMM2_0";    good=true; break;
        case OFFB_YMM3:     name = "YMM3_0";    good=true; break;
        case OFFB_YMM4:     name = "YMM4_0";    good=true; break;
        case OFFB_YMM5:     name = "YMM5_0";    good=true; break;
        case OFFB_YMM6:     name = "YMM6_0";    good=true; break;
        case OFFB_YMM7:     name = "YMM7_0";    good=true; break;
        case OFFB_YMM8:     name = "YMM8_0";    good=true; break;
        case OFFB_YMM9:     name = "YMM9_0";    good=true; break;
        case OFFB_YMM10:    name = "YMM10_0";   good=true; break;
        case OFFB_YMM11:    name = "YMM11_0";   good=true; break;
        case OFFB_YMM12:    name = "YMM12_0";   good=true; break;
        case OFFB_YMM13:    name = "YMM13_0";   good=true; break;
        case OFFB_YMM14:    name = "YMM14_0";   good=true; break;
        case OFFB_YMM15:    name = "YMM15_0";   good=true; break;

        case OFFB_YMM0+8:   name = "YMM0_1";    good=true; break;
        case OFFB_YMM1+8:   name = "YMM1_1";    good=true; break;
        case OFFB_YMM2+8:   name = "YMM2_1";    good=true; break;
        case OFFB_YMM3+8:   name = "YMM3_1";    good=true; break;
        case OFFB_YMM4+8:   name = "YMM4_1";    good=true; break;
        case OFFB_YMM5+8:   name = "YMM5_1";    good=true; break;
        case OFFB_YMM6+8:   name = "YMM6_1";    good=true; break;
        case OFFB_YMM7+8:   name = "YMM7_1";    good=true; break;
        case OFFB_YMM8+8:   name = "YMM8_1";    good=true; break;
        case OFFB_YMM9+8:   name = "YMM9_1";    good=true; break;
        case OFFB_YMM10+8:  name = "YMM10_1";   good=true; break;
        case OFFB_YMM11+8:  name = "YMM11_1";   good=true; break;
        case OFFB_YMM12+8:  name = "YMM12_1";   good=true; break;
        case OFFB_YMM13+8:  name = "YMM13_1";   good=true; break;
        case OFFB_YMM14+8:  name = "YMM14_1";   good=true; break;
        case OFFB_YMM15+8:  name = "YMM15_1";   good=true; break;
#else
        case OFFB_XMM0:     name = "YMM0_0";    good=true; break;
        case OFFB_XMM1:     name = "YMM1_0";    good=true; break;
        case OFFB_XMM2:     name = "YMM2_0";    good=true; break;
        case OFFB_XMM3:     name = "YMM3_0";    good=true; break;
        case OFFB_XMM4:     name = "YMM4_0";    good=true; break;
        case OFFB_XMM5:     name = "YMM5_0";    good=true; break;
        case OFFB_XMM6:     name = "YMM6_0";    good=true; break;
        case OFFB_XMM7:     name = "YMM7_0";    good=true; break;
        case OFFB_XMM8:     name = "YMM8_0";    good=true; break;
        case OFFB_XMM9:     name = "YMM9_0";    good=true; break;
        case OFFB_XMM10:    name = "YMM10_0";   good=true; break;
        case OFFB_XMM11:    name = "YMM11_0";   good=true; break;
        case OFFB_XMM12:    name = "YMM12_0";   good=true; break;
        case OFFB_XMM13:    name = "YMM13_0";   good=true; break;
        case OFFB_XMM14:    name = "YMM14_0";   good=true; break;
        case OFFB_XMM15:    name = "YMM15_0";   good=true; break;

        case OFFB_XMM0+8:   name = "YMM0_1";    good=true; break;
        case OFFB_XMM1+8:   name = "YMM1_1";    good=true; break;
        case OFFB_XMM2+8:   name = "YMM2_1";    good=true; break;
        case OFFB_XMM3+8:   name = "YMM3_1";    good=true; break;
        case OFFB_XMM4+8:   name = "YMM4_1";    good=true; break;
        case OFFB_XMM5+8:   name = "YMM5_1";    good=true; break;
        case OFFB_XMM6+8:   name = "YMM6_1";    good=true; break;
        case OFFB_XMM7+8:   name = "YMM7_1";    good=true; break;
        case OFFB_XMM8+8:   name = "YMM8_1";    good=true; break;
        case OFFB_XMM9+8:   name = "YMM9_1";    good=true; break;
        case OFFB_XMM10+8:  name = "YMM10_1";   good=true; break;
        case OFFB_XMM11+8:  name = "YMM11_1";   good=true; break;
        case OFFB_XMM12+8:  name = "YMM12_1";   good=true; break;
        case OFFB_XMM13+8:  name = "YMM13_1";   good=true; break;
        case OFFB_XMM14+8:  name = "YMM14_1";   good=true; break;
        case OFFB_XMM15+8:  name = "YMM15_1";   good=true; break;
#endif

        case OFFB_FPREGS:       name = "FPREG0";good=true; break;
        case OFFB_FPREGS+(1*8): name = "FPREG1";good=true; break;
        case OFFB_FPREGS+(2*8): name = "FPREG2";good=true; break;
        case OFFB_FPREGS+(3*8): name = "FPREG3";good=true; break;
        case OFFB_FPREGS+(4*8): name = "FPREG4";good=true; break;
        case OFFB_FPREGS+(5*8): name = "FPREG5";good=true; break;
        case OFFB_FPREGS+(6*8): name = "FPREG6";good=true; break;
        case OFFB_FPREGS+(7*8): name = "FPREG7";good=true; break;

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

    if (offset >= OFFB_FPTAGS && offset <= OFFB_FPTAGS + 7) {
        switch (offset) {
        case OFFB_FPTAGS+0: name = "FPTAG0"; break;
        case OFFB_FPTAGS+1: name = "FPTAG1"; break;
        case OFFB_FPTAGS+2: name = "FPTAG2"; break;
        case OFFB_FPTAGS+3: name = "FPTAG3"; break;
        case OFFB_FPTAGS+4: name = "FPTAG4"; break;
        case OFFB_FPTAGS+5: name = "FPTAG5"; break;
        case OFFB_FPTAGS+6: name = "FPTAG6"; break;
        case OFFB_FPTAGS+7: name = "FPTAG7"; break;
        default:
            assert(0);
        }
        return mk_reg(name, REG_8);
    }

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
        case OFFB_R15L: name = "R15";    low = true;    break;
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
        Temp *reg = mk_reg(name, REG_64);

        value = new Cast(reg, REG_16, CAST_LOW);
    }
    else
    {
        Temp *reg = mk_reg(name, REG_16);

        value = reg;
    }

    return value;
}

#if VEX_VERSION >= 2330
static bool lookup_32_in_ymm(unsigned int offset, string &name, int &lane) {
    bool is_good = false;
    switch (offset) {
    case OFFB_YMM0:     name =  "YMM0_0"; lane = 0; is_good = true; break;
    case OFFB_YMM1:     name =  "YMM1_0"; lane = 0; is_good = true; break;
    case OFFB_YMM2:     name =  "YMM2_0"; lane = 0; is_good = true; break;
    case OFFB_YMM3:     name =  "YMM3_0"; lane = 0; is_good = true; break;
    case OFFB_YMM4:     name =  "YMM4_0"; lane = 0; is_good = true; break;
    case OFFB_YMM5:     name =  "YMM5_0"; lane = 0; is_good = true; break;
    case OFFB_YMM6:     name =  "YMM6_0"; lane = 0; is_good = true; break;
    case OFFB_YMM7:     name =  "YMM7_0"; lane = 0; is_good = true; break;
    case OFFB_YMM8:     name =  "YMM8_0"; lane = 0; is_good = true; break;
    case OFFB_YMM9:     name =  "YMM9_0"; lane = 0; is_good = true; break;
    case OFFB_YMM10:    name = "YMM10_0"; lane = 0; is_good = true; break;
    case OFFB_YMM11:    name = "YMM11_0"; lane = 0; is_good = true; break;
    case OFFB_YMM12:    name = "YMM12_0"; lane = 0; is_good = true; break;
    case OFFB_YMM13:    name = "YMM13_0"; lane = 0; is_good = true; break;
    case OFFB_YMM14:    name = "YMM14_0"; lane = 0; is_good = true; break;
    case OFFB_YMM15:    name = "YMM15_0"; lane = 0; is_good = true; break;
    case OFFB_YMM0+4:   name =  "YMM0_0"; lane = 1; is_good = true; break;
    case OFFB_YMM1+4:   name =  "YMM1_0"; lane = 1; is_good = true; break;
    case OFFB_YMM2+4:   name =  "YMM2_0"; lane = 1; is_good = true; break;
    case OFFB_YMM3+4:   name =  "YMM3_0"; lane = 1; is_good = true; break;
    case OFFB_YMM4+4:   name =  "YMM4_0"; lane = 1; is_good = true; break;
    case OFFB_YMM5+4:   name =  "YMM5_0"; lane = 1; is_good = true; break;
    case OFFB_YMM6+4:   name =  "YMM6_0"; lane = 1; is_good = true; break;
    case OFFB_YMM7+4:   name =  "YMM7_0"; lane = 1; is_good = true; break;
    case OFFB_YMM8+4:   name =  "YMM8_0"; lane = 1; is_good = true; break;
    case OFFB_YMM9+4:   name =  "YMM9_0"; lane = 1; is_good = true; break;
    case OFFB_YMM10+4:  name = "YMM10_0"; lane = 1; is_good = true; break;
    case OFFB_YMM11+4:  name = "YMM11_0"; lane = 1; is_good = true; break;
    case OFFB_YMM12+4:  name = "YMM12_0"; lane = 1; is_good = true; break;
    case OFFB_YMM13+4:  name = "YMM13_0"; lane = 1; is_good = true; break;
    case OFFB_YMM14+4:  name = "YMM14_0"; lane = 1; is_good = true; break;
    case OFFB_YMM15+4:  name = "YMM15_0"; lane = 1; is_good = true; break;

    case OFFB_YMM0+8:   name =  "YMM0_1"; lane = 0; is_good = true; break;
    case OFFB_YMM1+8:   name =  "YMM1_1"; lane = 0; is_good = true; break;
    case OFFB_YMM2+8:   name =  "YMM2_1"; lane = 0; is_good = true; break;
    case OFFB_YMM3+8:   name =  "YMM3_1"; lane = 0; is_good = true; break;
    case OFFB_YMM4+8:   name =  "YMM4_1"; lane = 0; is_good = true; break;
    case OFFB_YMM5+8:   name =  "YMM5_1"; lane = 0; is_good = true; break;
    case OFFB_YMM6+8:   name =  "YMM6_1"; lane = 0; is_good = true; break;
    case OFFB_YMM7+8:   name =  "YMM7_1"; lane = 0; is_good = true; break;
    case OFFB_YMM8+8:   name =  "YMM8_1"; lane = 0; is_good = true; break;
    case OFFB_YMM9+8:   name =  "YMM9_1"; lane = 0; is_good = true; break;
    case OFFB_YMM10+8:  name = "YMM10_1"; lane = 0; is_good = true; break;
    case OFFB_YMM11+8:  name = "YMM11_1"; lane = 0; is_good = true; break;
    case OFFB_YMM12+8:  name = "YMM12_1"; lane = 0; is_good = true; break;
    case OFFB_YMM13+8:  name = "YMM13_1"; lane = 0; is_good = true; break;
    case OFFB_YMM14+8:  name = "YMM14_1"; lane = 0; is_good = true; break;
    case OFFB_YMM15+8:  name = "YMM15_1"; lane = 0; is_good = true; break;
    case OFFB_YMM0+12:  name =  "YMM0_1"; lane = 1; is_good = true; break;
    case OFFB_YMM1+12:  name =  "YMM1_1"; lane = 1; is_good = true; break;
    case OFFB_YMM2+12:  name =  "YMM2_1"; lane = 1; is_good = true; break;
    case OFFB_YMM3+12:  name =  "YMM3_1"; lane = 1; is_good = true; break;
    case OFFB_YMM4+12:  name =  "YMM4_1"; lane = 1; is_good = true; break;
    case OFFB_YMM5+12:  name =  "YMM5_1"; lane = 1; is_good = true; break;
    case OFFB_YMM6+12:  name =  "YMM6_1"; lane = 1; is_good = true; break;
    case OFFB_YMM7+12:  name =  "YMM7_1"; lane = 1; is_good = true; break;
    case OFFB_YMM8+12:  name =  "YMM8_1"; lane = 1; is_good = true; break;
    case OFFB_YMM9+12:  name =  "YMM9_1"; lane = 1; is_good = true; break;
    case OFFB_YMM10+12: name = "YMM10_1"; lane = 1; is_good = true; break;
    case OFFB_YMM11+12: name = "YMM11_1"; lane = 1; is_good = true; break;
    case OFFB_YMM12+12: name = "YMM12_1"; lane = 1; is_good = true; break;
    case OFFB_YMM13+12: name = "YMM13_1"; lane = 1; is_good = true; break;
    case OFFB_YMM14+12: name = "YMM14_1"; lane = 1; is_good = true; break;
    case OFFB_YMM15+12: name = "YMM15_1"; lane = 1; is_good = true; break;

    case OFFB_YMM0+16:  name =  "YMM0_2"; lane = 0; is_good = true; break;
    case OFFB_YMM1+16:  name =  "YMM1_2"; lane = 0; is_good = true; break;
    case OFFB_YMM2+16:  name =  "YMM2_2"; lane = 0; is_good = true; break;
    case OFFB_YMM3+16:  name =  "YMM3_2"; lane = 0; is_good = true; break;
    case OFFB_YMM4+16:  name =  "YMM4_2"; lane = 0; is_good = true; break;
    case OFFB_YMM5+16:  name =  "YMM5_2"; lane = 0; is_good = true; break;
    case OFFB_YMM6+16:  name =  "YMM6_2"; lane = 0; is_good = true; break;
    case OFFB_YMM7+16:  name =  "YMM7_2"; lane = 0; is_good = true; break;
    case OFFB_YMM8+16:  name =  "YMM8_2"; lane = 0; is_good = true; break;
    case OFFB_YMM9+16:  name =  "YMM9_2"; lane = 0; is_good = true; break;
    case OFFB_YMM10+16: name = "YMM10_2"; lane = 0; is_good = true; break;
    case OFFB_YMM11+16: name = "YMM11_2"; lane = 0; is_good = true; break;
    case OFFB_YMM12+16: name = "YMM12_2"; lane = 0; is_good = true; break;
    case OFFB_YMM13+16: name = "YMM13_2"; lane = 0; is_good = true; break;
    case OFFB_YMM14+16: name = "YMM14_2"; lane = 0; is_good = true; break;
    case OFFB_YMM15+16: name = "YMM15_2"; lane = 0; is_good = true; break;
    case OFFB_YMM0+20:  name =  "YMM0_2"; lane = 1; is_good = true; break;
    case OFFB_YMM1+20:  name =  "YMM1_2"; lane = 1; is_good = true; break;
    case OFFB_YMM2+20:  name =  "YMM2_2"; lane = 1; is_good = true; break;
    case OFFB_YMM3+20:  name =  "YMM3_2"; lane = 1; is_good = true; break;
    case OFFB_YMM4+20:  name =  "YMM4_2"; lane = 1; is_good = true; break;
    case OFFB_YMM5+20:  name =  "YMM5_2"; lane = 1; is_good = true; break;
    case OFFB_YMM6+20:  name =  "YMM6_2"; lane = 1; is_good = true; break;
    case OFFB_YMM7+20:  name =  "YMM7_2"; lane = 1; is_good = true; break;
    case OFFB_YMM8+20:  name =  "YMM8_2"; lane = 1; is_good = true; break;
    case OFFB_YMM9+20:  name =  "YMM9_2"; lane = 1; is_good = true; break;
    case OFFB_YMM10+20: name = "YMM10_2"; lane = 1; is_good = true; break;
    case OFFB_YMM11+20: name = "YMM11_2"; lane = 1; is_good = true; break;
    case OFFB_YMM12+20: name = "YMM12_2"; lane = 1; is_good = true; break;
    case OFFB_YMM13+20: name = "YMM13_2"; lane = 1; is_good = true; break;
    case OFFB_YMM14+20: name = "YMM14_2"; lane = 1; is_good = true; break;
    case OFFB_YMM15+20: name = "YMM15_2"; lane = 1; is_good = true; break;

    case OFFB_YMM0+24:  name =  "YMM0_3"; lane = 0; is_good = true; break;
    case OFFB_YMM1+24:  name =  "YMM1_3"; lane = 0; is_good = true; break;
    case OFFB_YMM2+24:  name =  "YMM2_3"; lane = 0; is_good = true; break;
    case OFFB_YMM3+24:  name =  "YMM3_3"; lane = 0; is_good = true; break;
    case OFFB_YMM4+24:  name =  "YMM4_3"; lane = 0; is_good = true; break;
    case OFFB_YMM5+24:  name =  "YMM5_3"; lane = 0; is_good = true; break;
    case OFFB_YMM6+24:  name =  "YMM6_3"; lane = 0; is_good = true; break;
    case OFFB_YMM7+24:  name =  "YMM7_3"; lane = 0; is_good = true; break;
    case OFFB_YMM8+24:  name =  "YMM8_3"; lane = 0; is_good = true; break;
    case OFFB_YMM9+24:  name =  "YMM9_3"; lane = 0; is_good = true; break;
    case OFFB_YMM10+24: name = "YMM10_3"; lane = 0; is_good = true; break;
    case OFFB_YMM11+24: name = "YMM11_3"; lane = 0; is_good = true; break;
    case OFFB_YMM12+24: name = "YMM12_3"; lane = 0; is_good = true; break;
    case OFFB_YMM13+24: name = "YMM13_3"; lane = 0; is_good = true; break;
    case OFFB_YMM14+24: name = "YMM14_3"; lane = 0; is_good = true; break;
    case OFFB_YMM15+24: name = "YMM15_3"; lane = 0; is_good = true; break;
    case OFFB_YMM0+28:  name =  "YMM0_3"; lane = 1; is_good = true; break;
    case OFFB_YMM1+28:  name =  "YMM1_3"; lane = 1; is_good = true; break;
    case OFFB_YMM2+28:  name =  "YMM2_3"; lane = 1; is_good = true; break;
    case OFFB_YMM3+28:  name =  "YMM3_3"; lane = 1; is_good = true; break;
    case OFFB_YMM4+28:  name =  "YMM4_3"; lane = 1; is_good = true; break;
    case OFFB_YMM5+28:  name =  "YMM5_3"; lane = 1; is_good = true; break;
    case OFFB_YMM6+28:  name =  "YMM6_3"; lane = 1; is_good = true; break;
    case OFFB_YMM7+28:  name =  "YMM7_3"; lane = 1; is_good = true; break;
    case OFFB_YMM8+28:  name =  "YMM8_3"; lane = 1; is_good = true; break;
    case OFFB_YMM9+28:  name =  "YMM9_3"; lane = 1; is_good = true; break;
    case OFFB_YMM10+28: name = "YMM10_3"; lane = 1; is_good = true; break;
    case OFFB_YMM11+28: name = "YMM11_3"; lane = 1; is_good = true; break;
    case OFFB_YMM12+28: name = "YMM12_3"; lane = 1; is_good = true; break;
    case OFFB_YMM13+28: name = "YMM13_3"; lane = 1; is_good = true; break;
    case OFFB_YMM14+28: name = "YMM14_3"; lane = 1; is_good = true; break;
    case OFFB_YMM15+28: name = "YMM15_3"; lane = 1; is_good = true; break;
    }
    return is_good;
}
#endif

static Exp *translate_get_reg_32( unsigned int offset )
{
    string name;
    bool sub, is_good = false;
    int lane = -1;

#if VEX_VERSION >= 2330
    is_good = lookup_32_in_ymm(offset, name, lane);
    if (is_good)
	sub = true;

    if (!is_good && offset >= OFFB_YMM0 && offset < OFFB_YMM15+64) {
	// unsupported SSE sub-register?
	return new Unknown("Unhandled (misaligned?) 32-bit YMM lane");
    }
#endif

    if (!is_good)
	switch ( offset )
	    {
		//
		// These are 32-bit sub registers
		//
	    case OFFB_EAX:  name = "RAX";   sub = true; lane = 0; break;
	    case OFFB_EBX:  name = "RBX";   sub = true; lane = 0; break;
	    case OFFB_ECX:  name = "RCX";   sub = true; lane = 0; break;
	    case OFFB_EDX:  name = "RDX";   sub = true; lane = 0; break;
	    case OFFB_EDI:  name = "RDI";   sub = true; lane = 0; break;
	    case OFFB_ESI:  name = "RSI";   sub = true; lane = 0; break;
	    case OFFB_EBP:  name = "RBP";   sub = true; lane = 0; break;
	    case OFFB_ESP:  name = "RSP";   sub = true; lane = 0; break;
	    case OFFB_R8D:  name = "R8";    sub = true; lane = 0; break;
	    case OFFB_R9D:  name = "R9";    sub = true; lane = 0; break;
	    case OFFB_R10D: name = "R10";   sub = true; lane = 0; break;
	    case OFFB_R11D: name = "R11";   sub = true; lane = 0; break;
	    case OFFB_R12D: name = "R12";   sub = true; lane = 0; break;
	    case OFFB_R13D: name = "R13";   sub = true; lane = 0; break;
	    case OFFB_R14D: name = "R14";   sub = true; lane = 0; break;
	    case OFFB_R15D: name = "R15";   sub = true; lane = 0; break;

	    case OFFB_EMNOTE: name = "EMNOTE"; sub = false; break;
	    case OFFB_FTOP:   name = "FTOP";   sub = false; break;

	    default:
		assert(0);
	    }

    Exp *value = NULL;

    if ( sub )
    {
	assert(lane >= 0 && lane < 2);
        Temp *reg = mk_reg(name, REG_64);
        switch (lane) {
        case 0: value = _ex_l_cast(reg, REG_32); break;
        case 1: value = _ex_h_cast(reg, REG_32); break;
        }
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

static Exp *translate_get_reg_128( unsigned int offset )
{
    string name;

    switch ( offset )
    {
#if VEX_VERSION >= 2330
    case OFFB_YMM0: name = "YMM0"; break;
    case OFFB_YMM1: name = "YMM1"; break;
    case OFFB_YMM2: name = "YMM2"; break;
    case OFFB_YMM3: name = "YMM3"; break;
    case OFFB_YMM4: name = "YMM4"; break;
    case OFFB_YMM5: name = "YMM5"; break;
    case OFFB_YMM6: name = "YMM6"; break;
    case OFFB_YMM7: name = "YMM7"; break;
    case OFFB_YMM8: name = "YMM8"; break;
    case OFFB_YMM9: name = "YMM9"; break;
    case OFFB_YMM10: name = "YMM10"; break;
    case OFFB_YMM11: name = "YMM11"; break;
    case OFFB_YMM12: name = "YMM12"; break;
    case OFFB_YMM13: name = "YMM13"; break;
    case OFFB_YMM14: name = "YMM14"; break;
    case OFFB_YMM15: name = "YMM15"; break;
#else
    case OFFB_XMM0: name = "YMM0"; break;
    case OFFB_XMM1: name = "YMM1"; break;
    case OFFB_XMM2: name = "YMM2"; break;
    case OFFB_XMM3: name = "YMM3"; break;
    case OFFB_XMM4: name = "YMM4"; break;
    case OFFB_XMM5: name = "YMM5"; break;
    case OFFB_XMM6: name = "YMM6"; break;
    case OFFB_XMM7: name = "YMM7"; break;
    case OFFB_XMM8: name = "YMM8"; break;
    case OFFB_XMM9: name = "YMM9"; break;
    case OFFB_XMM10: name = "YMM10"; break;
    case OFFB_XMM11: name = "YMM11"; break;
    case OFFB_XMM12: name = "YMM12"; break;
    case OFFB_XMM13: name = "YMM13"; break;
    case OFFB_XMM14: name = "YMM14"; break;
    case OFFB_XMM15: name = "YMM15"; break;
#endif
    default:
        assert(0);
    }

    string name_l = name + "_0";
    string name_h = name + "_1";

    Exp *value = new Vector(mk_reg(name_h, REG_64),
                            mk_reg(name_l, REG_64));

    return value;
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
    else if ( type == Ity_I64 || type == Ity_F64 )
    {
        result = translate_get_reg_64(offset);
    }
    else if ( type == Ity_F32 )
    {
        result = new Unknown("register type (F32)");
    }

    else if ( type == Ity_I128 || type == Ity_V128 )
    {
        result = translate_get_reg_128(offset);
    }

    else
    {
        panic("Unrecognized register type");
    }

    return result;
}

Exp *x64_translate_geti( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    IRType type = typeOfIRExpr(irbb->tyenv, expr);
    IRRegArray* descr = expr->Iex.GetI.descr;
    IRExpr *ix = expr->Iex.GetI.ix;
    int bias = expr->Iex.GetI.bias;
    int elt_size;
    reg_t elt_t;

    assert(type == descr->elemTy);
    if (descr->base == OFFB_FPREGS && descr->elemTy == Ity_F64 &&
        descr->nElems == 8) {
        /* x87 FP registers, in VEX's 64-bit simulation */
        elt_size = 8;
        elt_t = REG_64;
    } else if (descr->base == OFFB_FPTAGS && descr->elemTy == Ity_I8 &&
        descr->nElems == 8) {
        /* In-use tags for x87 FP registers */
        elt_size = 1;
        elt_t = REG_8;
    } else {
        return new Unknown("Unrecognized GetI region");
    }

    int mask = descr->nElems - 1; /* NB must be a power of two */
    Exp *ix_e = translate_expr(ix, irbb, irout);
    Exp *index_e = _ex_and(_ex_add(ix_e, ex_const(bias)), ex_const(mask));

    Exp **gets = (Exp **)malloc(descr->nElems * sizeof(Exp *));
    for (int i = 0; i < descr->nElems; i++) {
        if (elt_size == 1) {
            gets[i] = translate_get_reg_8(descr->base + i * elt_size);
        } else if (elt_size == 8) {
            gets[i] = translate_get_reg_64(descr->base + i * elt_size);
        }
    }

    /* Generate a tree of if-then-else choices */
    assert(descr->nElems == 8);
    Temp *cond_temp = mk_temp(REG_32, irout);
    irout->push_back(new Move(cond_temp, index_e));
    Exp *sel0_exp = ex_l_cast(cond_temp, REG_1);
    Temp *sel0_temp = mk_temp(REG_1, irout);
    irout->push_back(new Move(sel0_temp, sel0_exp));
    Exp *sel1_exp = ex_get_bit(cond_temp, 1);
    Temp *sel1_temp = mk_temp(REG_1, irout);
    irout->push_back(new Move(sel1_temp, sel1_exp));
    Exp *sel2_exp = ex_get_bit(cond_temp, 2);
    Exp *choice01 = emit_ite(irout, elt_t, ecl(sel0_temp), gets[1], gets[0]);
    Exp *choice23 = emit_ite(irout, elt_t, ecl(sel0_temp), gets[3], gets[2]);
    Exp *choice45 = emit_ite(irout, elt_t, ecl(sel0_temp), gets[5], gets[4]);
    Exp *choice67 = emit_ite(irout, elt_t, ecl(sel0_temp), gets[7], gets[6]);
    Exp *choice03 = emit_ite(irout, elt_t, ecl(sel1_temp), choice23, choice01);
    Exp *choice47 = emit_ite(irout, elt_t, ecl(sel1_temp), choice67, choice45);
    Exp *choice = emit_ite(irout, elt_t, sel2_exp, choice47, choice03);

    free(gets);

    return choice;
}


Stmt *x64_translate_dirty( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    Stmt *result = NULL;

    IRDirty *dirty = stmt->Ist.Dirty.details;

    string func = string(dirty->cee->name);

    if (func == "amd64g_dirtyhelper_CPUID_baseline" ||
	func == "amd64g_dirtyhelper_CPUID_sse3_and_cx16" ||
	func == "amd64g_dirtyhelper_CPUID_sse42_and_cx16" ||
	func == "amd64g_dirtyhelper_CPUID_avx_and_cx16") {
	result = new Special("cpuid");
    } else if (func == "amd64g_dirtyhelper_RDTSC") {
	IRTemp lhs = dirty->tmp;
        assert(lhs != IRTemp_INVALID);
        result = mk_assign_tmp(lhs, new Unknown("rdtsc"), irbb, irout);
    } else if (func == "amd64g_dirtyhelper_loadF80le") {
	IRTemp lhs = dirty->tmp;
        assert(lhs != IRTemp_INVALID);
        result = mk_assign_tmp(lhs, new Unknown("loadF80"), irbb, irout);
    } else if (func == "amd64g_dirtyhelper_storeF80le") {
        result = new ExpStmt(new Unknown("Unknown: storeF80"));
    } else
    {
        result = new ExpStmt(new Unknown("Unknown: Dirty"));
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
    if (offset >= OFFB_YMM0 && offset < OFFB_YMM15+64) {
	// SSE sub-register: not supported.
	Exp::destroy(data);
	return new Special("Unhandled store to 8-bit YMM lane");
    }
#endif

    if (offset >= OFFB_FPTAGS && offset <= OFFB_FPTAGS + 7) {
        switch (offset) {
        case OFFB_FPTAGS+0: name = "FPTAG0"; break;
        case OFFB_FPTAGS+1: name = "FPTAG1"; break;
        case OFFB_FPTAGS+2: name = "FPTAG2"; break;
        case OFFB_FPTAGS+3: name = "FPTAG3"; break;
        case OFFB_FPTAGS+4: name = "FPTAG4"; break;
        case OFFB_FPTAGS+5: name = "FPTAG5"; break;
        case OFFB_FPTAGS+6: name = "FPTAG6"; break;
        case OFFB_FPTAGS+7: name = "FPTAG7"; break;
        default:
            assert(0);
        }
        return new Move(mk_reg(name, REG_8), data);
    }

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
        case OFFB_R15L: name = "R15";    low = true;    break;

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

// Basically the same as translate_32HLto64
static Exp *assemble64(Exp *arg1, Exp *arg2) {
    Exp *high = new Cast(arg1, REG_64, CAST_UNSIGNED);
    Exp *low = new Cast(arg2, REG_64, CAST_UNSIGNED);
    Exp *high_s = new BinOp(LSHIFT, high, ex_const(32));
    return new BinOp(BITOR, high_s, low);
}

static Stmt *translate_put_reg_32( unsigned int offset, Exp *data, IRSB *irbb )
{
    assert(data);

    string name;
    bool sub, is_good = false;
    int lane = -1;
    Temp *reg;

#if VEX_VERSION >= 2330
    is_good = lookup_32_in_ymm(offset, name, lane);
    if (is_good)
	sub = true;

    if (!is_good && offset >= OFFB_YMM0 && offset < OFFB_YMM7+64) {
	Exp::destroy(data);
	return new Special("Unhandled store to (unaligned?) 32-bit YMM lane");
    }
#endif

    if (!is_good)
	switch ( offset )
	    {
	    case OFFB_EAX:  name = "RAX";   sub = true; lane = 0; break;
	    case OFFB_EBX:  name = "RBX";   sub = true; lane = 0; break;
	    case OFFB_ECX:  name = "RCX";   sub = true; lane = 0; break;
	    case OFFB_EDX:  name = "RDX";   sub = true; lane = 0; break;
	    case OFFB_EDI:  name = "RDI";   sub = true; lane = 0; break;
	    case OFFB_ESI:  name = "RSI";   sub = true; lane = 0; break;
	    case OFFB_EBP:  name = "RBP";   sub = true; lane = 0; break;
	    case OFFB_ESP:  name = "RSP";   sub = true; lane = 0; break;
	    case OFFB_R8D:  name = "R8";    sub = true; lane = 0; break;
	    case OFFB_R9D:  name = "R9";    sub = true; lane = 0; break;
	    case OFFB_R10D: name = "R10";   sub = true; lane = 0; break;
	    case OFFB_R11D: name = "R11";   sub = true; lane = 0; break;
	    case OFFB_R12D: name = "R12";   sub = true; lane = 0; break;
	    case OFFB_R13D: name = "R13";   sub = true; lane = 0; break;
	    case OFFB_R14D: name = "R14";   sub = true; lane = 0; break;
	    case OFFB_R15D: name = "R15";   sub = true; lane = 0; break;

	    case OFFB_EMNOTE: name = "EMNOTE"; sub = false; break;
	    case OFFB_FTOP:   name = "FTOP";   sub = false; break;

	    default:
		Exp::destroy(data);
		return new Special("Unhandled 32-bit register");
	    }

    Exp *value;
    
    if ( sub )
    {
	assert(lane >= 0 && lane < 2);
	reg = mk_reg(name, REG_64);
        Exp *old_val = ecl(reg);
	Exp *high, *low;
        if (lane == 0) {
            high = _ex_h_cast(old_val, REG_32);
            low = data;
        } else {
            high = data;
            low = _ex_l_cast(old_val, REG_32);
        }
        Exp *new_val = assemble64(high, low);
	value = new_val;
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

static Stmt *translate_put_reg_128(unsigned int offset, Exp *data, IRSB *irbb,
                                   vector<Stmt *> *irout)
{
    assert(data);

    string name;

    switch ( offset )
    {
#if VEX_VERSION >= 2330
    case OFFB_YMM0: name = "YMM0"; break;
    case OFFB_YMM1: name = "YMM1"; break;
    case OFFB_YMM2: name = "YMM2"; break;
    case OFFB_YMM3: name = "YMM3"; break;
    case OFFB_YMM4: name = "YMM4"; break;
    case OFFB_YMM5: name = "YMM5"; break;
    case OFFB_YMM6: name = "YMM6"; break;
    case OFFB_YMM7: name = "YMM7"; break;
    case OFFB_YMM8: name = "YMM8"; break;
    case OFFB_YMM9: name = "YMM9"; break;
    case OFFB_YMM10: name = "YMM10"; break;
    case OFFB_YMM11: name = "YMM11"; break;
    case OFFB_YMM12: name = "YMM12"; break;
    case OFFB_YMM13: name = "YMM13"; break;
    case OFFB_YMM14: name = "YMM14"; break;
    case OFFB_YMM15: name = "YMM15"; break;
#else
    case OFFB_XMM0: name = "YMM0"; break;
    case OFFB_XMM1: name = "YMM1"; break;
    case OFFB_XMM2: name = "YMM2"; break;
    case OFFB_XMM3: name = "YMM3"; break;
    case OFFB_XMM4: name = "YMM4"; break;
    case OFFB_XMM5: name = "YMM5"; break;
    case OFFB_XMM6: name = "YMM6"; break;
    case OFFB_XMM7: name = "YMM7"; break;
    case OFFB_XMM8: name = "YMM8"; break;
    case OFFB_XMM9: name = "YMM9"; break;
    case OFFB_XMM10: name = "YMM10"; break;
    case OFFB_XMM11: name = "YMM11"; break;
    case OFFB_XMM12: name = "YMM12"; break;
    case OFFB_XMM13: name = "YMM13"; break;
    case OFFB_XMM14: name = "YMM14"; break;
    case OFFB_XMM15: name = "YMM15"; break;
#endif
        default:
            assert(0);
    }

    // The value should be a Vector; deconstruct it
    assert(data->exp_type == VECTOR);
    Vector *v = (Vector *)data;
    Exp *high = v->lanes[1];
    Exp *low = v->lanes[0];
    delete v; // Shallow delete, since we reuse high and low

    string name_l = name + "_0";
    string name_h = name + "_1";

    irout->push_back(new Move(mk_reg(name_h, REG_64), high));
    return new Move(mk_reg(name_l, REG_64), low);
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

    else if ( type == Ity_I64 || type == Ity_F64 )
    {
        result = translate_put_reg_64(offset, data, irbb);
    }

    else if ( type == Ity_I128 || type == Ity_V128 )
    {
        result = translate_put_reg_128(offset, data, irbb, irout);
    }

    else
    {
	Exp::destroy(data);
        result = new Special("Unrecognized register type");
    }

    return result;
}

#if VEX_VERSION >= 2361
#define PutI_details(x) PutI.details->x
#else
#define PutI_details(x) PutI.x
#endif

Stmt *x64_translate_puti( IRStmt *stmt, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(stmt);
    assert(irbb);
    assert(irout);

    IRRegArray* descr = stmt->Ist.PutI_details(descr);
    IRExpr *ix = stmt->Ist.PutI_details(ix);
    int bias = stmt->Ist.PutI_details(bias);
    int elt_size;
    reg_t elt_t;

    Exp *data = translate_expr(stmt->Ist.PutI_details(data), irbb, irout);

    if (descr->base == OFFB_FPREGS && descr->elemTy == Ity_F64 &&
        descr->nElems == 8) {
        /* x87 FP registers, in VEX's 64-bit simulation */
        elt_size = 8;
        elt_t = REG_64;
    } else if (descr->base == OFFB_FPTAGS && descr->elemTy == Ity_I8 &&
        descr->nElems == 8) {
        /* In-use tags for x87 FP registers */
        elt_size = 1;
        elt_t = REG_8;
    } else {
        return new ExpStmt(new Unknown("Unrecognized PutI region"));
    }

    int mask = descr->nElems - 1;
    Exp *ix_e = translate_expr(ix, irbb, irout);
    Exp *index_e = _ex_and(_ex_add(ix_e, ex_const(bias)), ex_const(mask));
    Temp *index_temp = mk_temp(REG_32, irout);
    irout->push_back(new Move(index_temp, index_e));

    Stmt *last_stmt = 0;
    for (int i = 0; i < descr->nElems; i++) {
        if (last_stmt) {
            irout->push_back(last_stmt);
        }
        Exp *get = 0;
        int offset = descr->base + i * elt_size;
        if (elt_size == 1) {
            get = translate_get_reg_8(offset);
        } else if (elt_size == 8) {
            get = translate_get_reg_64(offset);
        } else {
            assert(0);
        }
        Exp *sel = _ex_eq(ecl(index_temp), ex_const(i));
        Exp *data_clone = last_stmt ? ecl(data) : data;
        Exp *maybe_up = emit_ite(irout, elt_t, sel, data_clone, get);
        Stmt *put;
        if (elt_size == 1) {
            put = translate_put_reg_8(offset, maybe_up, irbb);
        } else if (elt_size == 8) {
            put = translate_put_reg_64(offset, maybe_up, irbb);
        } else {
            assert(0);
        }
        last_stmt = put;
    }
    assert(last_stmt);
    return last_stmt;
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

Exp *x64_translate_ccall( IRExpr *expr, IRSB *irbb, vector<Stmt *> *irout )
{
    assert(expr);
    assert(irbb);
    assert(irout);

    Exp *result = NULL;
    string func = string(expr->Iex.CCall.cee->name);

    if (func == "amd64g_calculate_rflags_c") {
	result = _ex_u_cast(mk_reg("CF", REG_1), REG_64);
    } else if (func == "amd64g_calculate_rflags_all") {
	Exp *wCF = _ex_u_cast(mk_reg("CF", REG_1), REG_64);
	Exp *wPF = _ex_u_cast(mk_reg("PF", REG_1), REG_64);
	Exp *wAF = _ex_u_cast(mk_reg("AF", REG_1), REG_64);
	Exp *wZF = _ex_u_cast(mk_reg("ZF", REG_1), REG_64);
	Exp *wSF = _ex_u_cast(mk_reg("SF", REG_1), REG_64);
	Exp *wOF = _ex_u_cast(mk_reg("OF", REG_1), REG_64);
	assert(CC_SHIFT_C == 0);
	result = _ex_or(wCF,
			_ex_shl(wPF, CC_SHIFT_P),
			_ex_shl(wAF, CC_SHIFT_A),
			_ex_shl(wZF, CC_SHIFT_Z),
			_ex_shl(wSF, CC_SHIFT_S),
			_ex_shl(wOF, CC_SHIFT_O));
    } else if (func == "amd64g_calculate_condition") {
	assert(expr->Iex.CCall.args[0]->tag == Iex_Const);
	assert(expr->Iex.CCall.args[0]->Iex.Const.con->tag == Ico_U64);
	int arg = expr->Iex.CCall.args[0]->Iex.Const.con->Ico.U64;
	Exp *cond;
	switch (arg) {
	case AMD64CondO:
	    cond = mk_reg("OF", REG_1);
	    break;
	case AMD64CondNO:
	    cond = _ex_not(mk_reg("OF", REG_1));
	    break;
	case AMD64CondB:
	    cond = mk_reg("CF", REG_1);
	    break;
	case AMD64CondNB:
	    cond = _ex_not(mk_reg("CF", REG_1));
	    break;
	case AMD64CondZ:
	    cond = mk_reg("ZF", REG_1);
	    break;
	case AMD64CondNZ:
	    cond = _ex_not(mk_reg("ZF", REG_1));
	    break;
	case AMD64CondS:
	    cond = mk_reg("SF", REG_1);
	    break;
	case AMD64CondNS:
	    cond = _ex_not(mk_reg("SF", REG_1));
	    break;
	case AMD64CondP:
	    cond = mk_reg("PF", REG_1);
	    break;
	case AMD64CondNP:
	    cond = _ex_not(mk_reg("PF", REG_1));
	    break;
	case AMD64CondBE: {
	    Temp *CF = mk_reg("CF", REG_1);
	    Temp *ZF = mk_reg("ZF", REG_1);
	    cond = _ex_or(CF, ZF);
	    break;
	}
	case AMD64CondNBE: {
	    Temp *CF = mk_reg("CF", REG_1);
	    Temp *ZF = mk_reg("ZF", REG_1);
	    cond = _ex_not(_ex_or(CF, ZF));
	    break;
	}
	case AMD64CondL: {
	    Temp *SF = mk_reg("SF", REG_1);
	    Temp *OF = mk_reg("OF", REG_1);
	    cond = _ex_xor(SF, OF);
	    break;
	}
	case AMD64CondNL: {
	    Temp *SF = mk_reg("SF", REG_1);
	    Temp *OF = mk_reg("OF", REG_1);
	    cond = _ex_not(_ex_xor(SF, OF));
	    break;
	}
	case AMD64CondLE: {
	    Temp *SF = mk_reg("SF", REG_1);
	    Temp *ZF = mk_reg("ZF", REG_1);
	    Temp *OF = mk_reg("OF", REG_1);
	    cond = _ex_or(_ex_xor(SF, OF), ZF);
	    break;
	}
	case AMD64CondNLE: {
	    Temp *SF = mk_reg("SF", REG_1);
	    Temp *ZF = mk_reg("ZF", REG_1);
	    Temp *OF = mk_reg("OF", REG_1);
	    cond = _ex_not(_ex_or(_ex_xor(SF, OF), ZF));
	    break;
	}
	default:
	    panic("Unrecognized condition for amd64g_calculate_condition");
	}
	result = _ex_u_cast(cond, REG_64);
    } else if (func == "amd64g_calculate_RCL" ||
	       func == "amd64g_calculate_RCR") {
	bool is_left = (func == "amd64g_calculate_RCL");
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	Exp *rot_amt = translate_expr(expr->Iex.CCall.args[1], irbb, irout);
	Exp *flags_in = translate_expr(expr->Iex.CCall.args[2], irbb, irout);

	assert(expr->Iex.CCall.args[3]->tag == Iex_Const);
	assert(expr->Iex.CCall.args[3]->Iex.Const.con->tag == Ico_U64);
	int sz = expr->Iex.CCall.args[3]->Iex.Const.con->Ico.U64;
	int ret_flags = (sz < 0);
	sz = (ret_flags ? -sz : sz);

	const char *helper_name;
	BinOp *(*ex_sh)(Exp*, Exp*);  // same direction shift (rcl -> shl)
	//BinOp *(*_ex_sh)(Exp*, Exp*); // same direction shift (rcl -> shl)
	//BinOp *(*ex_rsh)(Exp*, Exp*); // reverse direction shift (rcl -> shr)
	BinOp *(*_ex_rsh)(Exp*, Exp*);// reverse direction shift (rcl -> shr)
	if (is_left) {
	    // Note: some comments and variable names below are
	    // specific to this RCL case.
	    helper_name = "amd64g_calculate_RCL";
	    ex_sh = ex_shl;
	    //_ex_sh = _ex_shl;
	    //ex_rsh = ex_shr;
	    _ex_rsh = _ex_shr;
	} else {
	    helper_name = "amd64g_calculate_RCR";
	    ex_sh = ex_shr;
	    //_ex_sh = _ex_shr;
	    //ex_rsh = ex_shl;
	    _ex_rsh = _ex_shl;
	}
	irout->push_back(new Comment(helper_name));

	// Having the shift amounts be 32 bits makes all the ex_consts
	// below simpler. However I think it might trigger an issue
	// with the later subtraction in which the values is not
	// properly sign extended.
	rot_amt = _ex_l_cast(rot_amt, REG_32);

	// normalize rot_amt
	{
	    Exp *old_rot_amt = rot_amt;
	    rot_amt = mk_temp(REG_32, irout);
	    Exp *masked = _ex_and(old_rot_amt, ex_const(sz == 8 ? 63 : 31));
	    Exp *modded;
	    switch (sz) {
	    case 8: modded = masked; break;
	    case 4: modded = masked; break;
	    case 2: modded = _ex_mod(masked, ex_const(17));
	    case 1: modded = _ex_mod(masked, ex_const(9));
	    default: panic("Unexpected size in RCL/RCR");
	    }
	    irout->push_back(new Move(ecl(rot_amt), modded));
	}

	// arg >> (sz*8+1 - rot_amt)
	Exp *new_right_part =
	    _ex_rsh(arg, _ex_sub(ex_const(sz*8+1), rot_amt));

	Exp *wCF = new Cast(mk_reg("CF", REG_1), REG_64, CAST_UNSIGNED);

	Exp *new_left = ex_sh(arg, rot_amt);

	Exp *carry_in_pos;
        Exp *carry_out_pos;
        // Note the symmetry here: RCL and RCR are inverse operations.
	if (is_left) {
	    carry_in_pos = _ex_sub(ecl(rot_amt), ex_const(1));
	    carry_out_pos = _ex_sub(ex_const(sz*8), ecl(rot_amt));
	} else {
	    carry_in_pos = _ex_sub(ex_const(sz*8), ecl(rot_amt));
	    carry_out_pos = _ex_sub(ecl(rot_amt), ex_const(1));
	}

	// Wrap these shift amounts to be within the legal range for
	// the data type. They could be out of range when rot_amt is
	// 0.  In that case the shifted values won't be used, but
	// because we want to allow a functional ITE for that choice,
	// the shift should be well-defined anyway.
	Exp *amt_mask = ex_const(sz*8 - 1);
	carry_in_pos = _ex_and(carry_in_pos, amt_mask);
	carry_out_pos = _ex_and(carry_out_pos, ecl(amt_mask));

	Exp *moved_carry = _ex_shl(wCF, carry_in_pos);
	// (arg << rot_amt) | (cf << carry_in_pos) | new_right_part
	Exp *answer_e = _ex_or(new_left, moved_carry, new_right_part);

	Exp *amt_zero = _ex_eq(ecl(rot_amt), ex_const(0));

	if (ret_flags) {
	    Temp *new_cf = mk_temp(REG_1, irout);
	    Exp *new_cf_e = _ex_l_cast(_ex_shr(ecl(arg), carry_out_pos),REG_1);
	    irout->push_back(new Move(new_cf, new_cf_e));
	    Exp *answer_bit =
		_ex_l_cast(_ex_shr(answer_e, ex_const(sz*8-1)), REG_1);
	    Exp *new_of = _ex_xor(answer_bit, ecl(new_cf));
	    Exp *unch_flags =
		_ex_and(flags_in, ex_const(REG_64, ~(CC_MASK_C | CC_MASK_O)));
	    assert(CC_SHIFT_C == 0);
	    Exp *wnCF = _ex_u_cast(ecl(new_cf), REG_64);
	    Exp *wnOF = _ex_shl(_ex_u_cast(new_of, REG_64), CC_SHIFT_O);
	    Exp *new_flags = _ex_or(unch_flags, wnCF, wnOF);
	    result = emit_ite(irout, REG_64, amt_zero,
			      ecl(flags_in), new_flags);
	} else {
	    Exp::destroy(carry_out_pos); /* slightly wasteful */
	    result = emit_ite(irout, REG_64, amt_zero, ecl(arg), answer_e);
	}

    } else if ( func == "amd64g_create_mxcsr" ) {
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	result = _ex_or(ex_const64(0x1f80),
			_ex_shl(arg, ex_const64(13)));
    } else if ( func == "amd64g_create_fpucw" ) {
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	result = _ex_or(ex_const64(0x037f),
			_ex_shl(arg, ex_const64(10)));
    } else if ( func == "amd64g_check_ldmxcsr" ) {
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	/* Extract the rounding mode */
	Exp *rmode = _ex_and(_ex_shr(arg, ex_const64(13)),
			     ex_const64(3));
	/* The high word is for emulation warnings: skip it */
	result = rmode;
    } else if ( func == "amd64g_check_fldcw" ) {
	Exp *arg = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	/* Extract the rounding mode */
	Exp *rmode = _ex_and(_ex_shr(arg, ex_const64(10)),
			     ex_const64(3));
	/* The high word is for emulation warnings: skip it */
	result = rmode;
    } else if ( func == "amd64g_calculate_sse_pmovmskb" ) {
	Exp *arg_hi = translate_expr(expr->Iex.CCall.args[0], irbb, irout);
	Exp *arg_lo = translate_expr(expr->Iex.CCall.args[1], irbb, irout);
	Exp *hi_b = translate_GetMSBs8x8(arg_hi);
	Exp *lo_b = translate_GetMSBs8x8(arg_lo);
	Exp *hi_16 = _ex_u_cast(hi_b, REG_16);
	Exp *lo_16 = _ex_u_cast(lo_b, REG_16);
	Exp *res_16 = _ex_or(_ex_shl(hi_16, 8), lo_16);
	result = _ex_u_cast(res_16, REG_64);
    } else {
        result = new Unknown("CCall: " + func);
    }

    return result;
}

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

/* e can be anything from a REG_8 to a REG_64, but only the partity of the
   low 8 bits is computed, like the x64 PF flag. */
Exp *parity8(Exp *e) {
    Exp *shift0 = ecl(e);
    Exp *shift1 = _ex_shr(ecl(e), new Constant(REG_32, 1));
    Exp *shift2 = _ex_shr(ecl(e), new Constant(REG_32, 2));
    Exp *shift3 = _ex_shr(ecl(e), new Constant(REG_32, 3));
    Exp *shift4 = _ex_shr(ecl(e), new Constant(REG_32, 4));
    Exp *shift5 = _ex_shr(ecl(e), new Constant(REG_32, 5));
    Exp *shift6 = _ex_shr(ecl(e), new Constant(REG_32, 6));
    Exp *shift7 = _ex_shr(ecl(e), new Constant(REG_32, 7));
    Exp *xor_e = _ex_xor(shift7, shift6, shift5, shift4,
			 shift3, shift2, shift1, shift0);
    return _ex_not(_ex_l_cast(xor_e, REG_1));
}

Exp *_narrow64(Exp *e, reg_t type) {
    if (type == REG_64)
	return e;
    else
	return _ex_l_cast(e, type);
}

Exp *narrow64(Exp *e, reg_t type) {
    if (type == REG_64)
	return ecl(e);
    else
	return ex_l_cast(e, type);
}

Exp *sign_bit_of(Exp *e, reg_t type) {
    return _ex_h_cast(narrow64(e, type), REG_1);
}

void x64_modify_flags( asm_program_t *prog, vine_block_t *block )
{
    int op_st = -1, dep1_st = -1, dep2_st = -1, ndep_st = -1,
        mux0x_st = -1, nop_st = -1;
    Exp *ite_cond = 0;
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
        if (op_mov->rhs->exp_type == CONSTANT) {
            Constant *op_const = (Constant*)op_mov->rhs;
            op = op_const->val;
            got_op = true;
	} else if (match_ite(ir, op_st, &cond, &exp_t, &exp_f, &res) >= 0) {
	    if (exp_t->exp_type == CONSTANT) {
		Constant *op_const = (Constant*)exp_t;
		op = op_const->val;
		ite_cond = cond;
		got_op = true;
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
    if (mux0x_st == -1 && !ite_cond) {
        // Unconditional case
        assert(ir->at(dep1_st)->stmt_type == MOVE);
        dep1_expr = ((Move *)(ir->at(dep1_st)))->rhs;

        assert(ir->at(dep2_st)->stmt_type == MOVE);
        dep2_expr = ((Move *)(ir->at(dep2_st)))->rhs;

	if (ndep_st != -1) {
	    assert(ir->at(ndep_st)->stmt_type == MOVE);
	    ndep_expr = ((Move *)(ir->at(ndep_st)))->rhs;
	}
    } else if (ite_cond) {
	// Functional conditional case
	Exp *cond, *exp_t, *exp_f, *res;
	int match;
	match = match_ite(ir, dep1_st, &cond, &exp_t, &exp_f, &res);
	assert(match == 0);
	dep1_expr = exp_t;

	match = match_ite(ir, dep2_st, &cond, &exp_t, &exp_f, &res);
	assert(match == 0);
	dep2_expr = exp_t;

	if (ndep_st != -1) {
	    match = match_ite(ir, dep2_st, &cond, &exp_t, &exp_f, &res);
	    assert(match == 0);
	    ndep_expr = exp_t;
	}
    } else {
        // Multi-statement conditional case: probably mostly obsolete
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

    reg_t type;
    reg_t double_type;
    switch (op) {
    case CC_OP_ADDB:
    case CC_OP_SUBB:
    case CC_OP_ADCB:
    case CC_OP_SBBB:
    case CC_OP_LOGICB:
    case CC_OP_INCB:
    case CC_OP_DECB:
    case CC_OP_SHLB:
    case CC_OP_SHRB:
    case CC_OP_ROLB:
    case CC_OP_RORB:
    case CC_OP_UMULB:
    case CC_OP_SMULB:
	type = REG_8;
	double_type = REG_16;
	break;

    case CC_OP_ADDW:
    case CC_OP_SUBW:
    case CC_OP_ADCW:
    case CC_OP_SBBW:
    case CC_OP_LOGICW:
    case CC_OP_INCW:
    case CC_OP_DECW:
    case CC_OP_SHLW:
    case CC_OP_SHRW:
    case CC_OP_ROLW:
    case CC_OP_RORW:
    case CC_OP_UMULW:
    case CC_OP_SMULW:
	type = REG_16;
	double_type = REG_32;
	break;

    case CC_OP_ADDL:
    case CC_OP_SUBL:
    case CC_OP_ADCL:
    case CC_OP_SBBL:
    case CC_OP_LOGICL:
    case CC_OP_INCL:
    case CC_OP_DECL:
    case CC_OP_SHLL:
    case CC_OP_SHRL:
    case CC_OP_ROLL:
    case CC_OP_RORL:
    case CC_OP_UMULL:
    case CC_OP_SMULL:
    case CC_OP_ANDN32:
    case CC_OP_BLSI32:
    case CC_OP_BLSMSK32:
    case CC_OP_BLSR32:
	type = REG_32;
	double_type = REG_64;
	break;

    case CC_OP_ADDQ:
    case CC_OP_SUBQ:
    case CC_OP_ADCQ:
    case CC_OP_SBBQ:
    case CC_OP_LOGICQ:
    case CC_OP_INCQ:
    case CC_OP_DECQ:
    case CC_OP_SHLQ:
    case CC_OP_SHRQ:
    case CC_OP_ROLQ:
    case CC_OP_RORQ:
    case CC_OP_UMULQ:
    case CC_OP_SMULQ:
    case CC_OP_ANDN64:
    case CC_OP_BLSI64:
    case CC_OP_BLSMSK64:
    case CC_OP_BLSR64:
	type = REG_64;
	double_type = REG_64; /* XXX no REG_128 yet */
	break;

    case CC_OP_COPY:
	type = REG_8; /* really, no type: should be unused */
	break;

    case CC_OP_NUMBER:
	panic("CC_OP_NUMBER is not a real CC_OP code");

    default:
	panic("Unrecognized CC_OP code");
    }

    switch (op) {
    case CC_OP_COPY:
	cf = ex_get_bit(dep1_expr, CC_SHIFT_C);
	pf = ex_get_bit(dep1_expr, CC_SHIFT_P);
	af = ex_get_bit(dep1_expr, CC_SHIFT_A);
	zf = ex_get_bit(dep1_expr, CC_SHIFT_Z);
	sf = ex_get_bit(dep1_expr, CC_SHIFT_S);
	of = ex_get_bit(dep1_expr, CC_SHIFT_O);
	break;
    case CC_OP_ADDB:
    case CC_OP_ADDW:
    case CC_OP_ADDL:
    case CC_OP_ADDQ: {
	Temp *result_w = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(result_w, ex_add(dep1_expr, dep2_expr)));
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(result_w, type)));
	Temp *sign_bit = mk_temp(REG_1, &new_ir);
        new_ir.push_back(new Move(sign_bit, ex_h_cast(result, REG_1)));
	cf = _ex_lt(ecl(result), narrow64(dep1_expr, type));
	pf = parity8(result);
	af = _ex_get_bit(ex_xor(result_w, dep1_expr, dep2_expr), 4);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	sf = ecl(sign_bit);
	of = _ex_and(_ex_xor(ecl(sign_bit), sign_bit_of(dep1_expr, type)),
		     _ex_xor(ecl(sign_bit), sign_bit_of(dep2_expr, type)));
	break; }
    case CC_OP_SUBB:
    case CC_OP_SUBW:
    case CC_OP_SUBL:
    case CC_OP_SUBQ: {
	Temp *result_w = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(result_w, ex_sub(dep1_expr, dep2_expr)));
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(result_w, type)));
	cf = ex_lt(dep1_expr, dep2_expr);
	pf = parity8(result);
	af = _ex_get_bit(ex_xor(result_w, dep1_expr, dep2_expr), 4);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	sf = ex_h_cast(result, REG_1);
	of = _ex_xor(ecl(sf), _ex_slt(narrow64(dep1_expr, type),
				      narrow64(dep2_expr, type)));
	break; }
    case CC_OP_ADCB:
    case CC_OP_ADCW:
    case CC_OP_ADCL:
    case CC_OP_ADCQ: {
	Temp *arg2_w = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(arg2_w, ex_xor(dep2_expr, ndep_expr)));
	Temp *result_w = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(result_w, _ex_add(ex_add(dep1_expr, arg2_w),
						    ecl(ndep_expr))));
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(result_w, type)));
	Temp *arg1 = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(arg1, narrow64(dep1_expr, type)));
	Temp *arg1_sign = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(arg1_sign, ex_h_cast(arg1, REG_1)));
	Temp *res_sign = mk_temp(REG_1, &new_ir);
        new_ir.push_back(new Move(res_sign, ex_h_cast(result, REG_1)));
	cf = _ex_ite(ex_l_cast(ndep_expr, REG_1),
		     ex_le(result, arg1), ex_lt(result, arg1));
	pf = parity8(result);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	af = _ex_get_bit(ex_xor(result_w, dep1_expr, dep2_expr), 4);
	sf = ecl(res_sign);
	of = _ex_and(_ex_eq(ecl(arg1_sign), sign_bit_of(dep2_expr, type)),
		     ex_neq(arg1_sign, res_sign));
	break; }
    case CC_OP_SBBB:
    case CC_OP_SBBW:
    case CC_OP_SBBL:
    case CC_OP_SBBQ: {
	Temp *arg1 = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(arg1, narrow64(dep1_expr, type)));
	Temp *arg2_w = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(arg2_w, ex_xor(dep2_expr, ndep_expr)));
	Temp *arg2 = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(arg2, narrow64(arg2_w, type)));
	Temp *result_w = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(result_w, _ex_sub(ex_sub(dep1_expr, arg2_w),
						    ecl(ndep_expr))));
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(result_w, type)));
	Temp *res_sign = mk_temp(REG_1, &new_ir);
        new_ir.push_back(new Move(res_sign, ex_h_cast(result, REG_1)));
	Temp *cf_in = mk_temp(REG_1, &new_ir);
        new_ir.push_back(new Move(cf_in, ex_l_cast(ndep_expr, REG_1)));
	cf = _ex_ite(ecl(cf_in), ex_le(arg1, arg2), ex_lt(arg1, arg2));
	pf = parity8(result);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	af = _ex_get_bit(ex_xor(result_w, dep1_expr, dep2_expr), 4);
	sf = ecl(res_sign);
	of = _ex_xor(ecl(res_sign),
		     _ex_ite(ecl(cf_in),
			     ex_sle(arg1, arg2), ex_slt(arg1, arg2)));
	break; }
    case CC_OP_LOGICB:
    case CC_OP_LOGICW:
    case CC_OP_LOGICL:
    case CC_OP_LOGICQ:
	cf = ecl(&Constant::f);
	of = ecl(&Constant::f);
	af = ecl(&Constant::f); /* spec: undefined */
	zf = _ex_eq(ecl(dep1_expr), ex_const(REG_64, 0));
	sf = sign_bit_of(dep1_expr, type);
	pf = parity8(dep1_expr);
	break;
    case CC_OP_INCB:
    case CC_OP_INCW:
    case CC_OP_INCL:
    case CC_OP_INCQ: {
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(dep1_expr, type)));
	Exp *arg = _ex_sub(ecl(result), ex_const(type, 1));
	Exp *tmin;
	switch (type) {
	case REG_8:  tmin = ex_const(type,               0x80  ); break;
	case REG_16: tmin = ex_const(type,             0x8000  ); break;
	case REG_32: tmin = ex_const(type,         0x80000000  ); break;
	case REG_64: tmin = ex_const(type, 0x8000000000000000LL); break;
	default: panic("Unexpected type in inc condition codes");
	}
	cf = 0;
	pf = parity8(result);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	af = _ex_get_bit(_ex_xor(ecl(result), ecl(arg)), 4);
	sf = ex_h_cast(result, REG_1);
	of = _ex_eq(ecl(result), tmin);
	break; }
    case CC_OP_DECB:
    case CC_OP_DECW:
    case CC_OP_DECL:
    case CC_OP_DECQ: {
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(dep1_expr, type)));
	Exp *arg = _ex_add(ecl(result), ex_const(type, 1));
	Exp *tmax;
	switch (type) {
	case REG_8:  tmax = ex_const(type,               0x7f  ); break;
	case REG_16: tmax = ex_const(type,             0x7fff  ); break;
	case REG_32: tmax = ex_const(type,         0x7fffffff  ); break;
	case REG_64: tmax = ex_const(type, 0x7fffffffffffffffLL); break;
	default: panic("Unexpected type in inc condition codes");
	}
	cf = 0;
	pf = parity8(result);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	af = _ex_get_bit(_ex_xor(ecl(result), ecl(arg)), 4);
	sf = ex_h_cast(result, REG_1);
	of = _ex_eq(ecl(result), tmax);
	break; }
    case CC_OP_SHLB:
    case CC_OP_SHLW:
    case CC_OP_SHLL:
    case CC_OP_SHLQ: {
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(dep1_expr, type)));
	Temp *subshift = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(subshift, narrow64(dep2_expr, type)));
	cf = ex_h_cast(subshift, REG_1);
	pf = parity8(result);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	af = ecl(&Constant::f); /* spec: undefined */
	sf = ex_h_cast(result, REG_1);
	of = _ex_h_cast(ex_xor(result, subshift), REG_1);
	break; }
    case CC_OP_SHRB:
    case CC_OP_SHRW:
    case CC_OP_SHRL:
    case CC_OP_SHRQ: {
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(dep1_expr, type)));
	Temp *subshift = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(subshift, narrow64(dep2_expr, type)));
	cf = ex_l_cast(subshift, REG_1);
	pf = parity8(result);
	zf = _ex_eq(ecl(result), ex_const(type, 0));
	af = ecl(&Constant::f); /* spec: undefined */
	sf = ex_h_cast(result, REG_1);
	/* N.B.: OF = 0 if shift amount > 1; spec says it is only
	   defined if amt == 1 */
	of = _ex_h_cast(ex_xor(result, subshift), REG_1);
	break; }
    case CC_OP_ROLB:
    case CC_OP_ROLW:
    case CC_OP_ROLL:
    case CC_OP_ROLQ: {
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(dep1_expr, type)));
	Temp *last_moved_bit = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(last_moved_bit, ex_l_cast(result, REG_1)));
	cf = ecl(last_moved_bit);
	/* spec: OF defined only for amt == 1 */
	of = _ex_xor(ecl(last_moved_bit), ex_h_cast(result, REG_1));
	pf = zf = af = sf = 0;
	break; }
    case CC_OP_RORB:
    case CC_OP_RORW:
    case CC_OP_RORL:
    case CC_OP_RORQ: {
	Temp *result = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(result, narrow64(dep1_expr, type)));
	Temp *last_moved_bit = mk_temp(REG_1, &new_ir);
	new_ir.push_back(new Move(last_moved_bit, ex_h_cast(result, REG_1)));
	cf = ecl(last_moved_bit);
	/* spec: OF defined only for amt == 1 */
	Exp *penult_bit = _ex_h_cast(ex_shl(result, 1), REG_1);
	of = ex_xor(last_moved_bit, penult_bit);
	pf = zf = af = sf = 0;
	break; }
    case CC_OP_UMULB:
    case CC_OP_UMULW:
    case CC_OP_UMULL: {
	Temp *result = mk_temp(double_type, &new_ir);
	Exp *result_e = _ex_mul(narrow64(dep1_expr, double_type),
				narrow64(dep2_expr, double_type));
	new_ir.push_back(new Move(result, result_e));
	Temp *oflow = mk_temp(REG_1, &new_ir);
	Exp *oflow_e = _ex_neq(ex_h_cast(result, type), ex_const(type, 0));
	new_ir.push_back(new Move(oflow, oflow_e));
	Temp *low = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(low, ex_l_cast(result, type)));
	cf = ecl(oflow);
	of = ecl(oflow);
	af = ecl(&Constant::f); /* spec: undefined */
	zf = _ex_eq(ecl(low), ex_const(type, 0)); /* spec: undefined */
	sf = ex_h_cast(low, REG_1); /* spec: undefined */
	pf = parity8(low); /* spec: undefined */
	break; }
    case CC_OP_UMULQ: {
	/* It's noticeably inefficient that in this approach we do the
	   whole mulitplication twice, once for the real result
	   (elsewhere) and once for the flags. */
	Exp *result = translate_MullU64(ecl(dep1_expr),
					ecl(dep2_expr), &new_ir);
	Exp *high_e, *low_e;
	split_vector(result, &high_e, &low_e);
	Temp *high = mk_temp(REG_64, &new_ir);
	Temp *low = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(high, high_e));
	new_ir.push_back(new Move(low, low_e));
	Temp *oflow = mk_temp(REG_1, &new_ir);
	Exp *oflow_e = _ex_neq(ecl(high), ex_const(REG_64, 0));
	new_ir.push_back(new Move(oflow, oflow_e));
	cf = ecl(oflow);
	of = ecl(oflow);
	af = ecl(&Constant::f); /* spec: undefined */
	zf = _ex_eq(ecl(low), ex_const(type, 0)); /* spec: undefined */
	sf = ex_h_cast(low, REG_1); /* spec: undefined */
	pf = parity8(low); /* spec: undefined */
	break; }
    case CC_OP_SMULB:
    case CC_OP_SMULW:
    case CC_OP_SMULL: {
	Temp *result = mk_temp(double_type, &new_ir);
	Exp *arg1 = _ex_s_cast(narrow64(dep1_expr, type), double_type);
	Exp *arg2 = _ex_s_cast(narrow64(dep2_expr, type), double_type);
	new_ir.push_back(new Move(result, _ex_mul(arg1, arg2)));
	Temp *low = mk_temp(type, &new_ir);
	new_ir.push_back(new Move(low, ex_l_cast(result, type)));
	Exp *sx = _ex_s_cast(ex_h_cast(low, REG_1), type);
	Temp *oflow = mk_temp(REG_1, &new_ir);
	Exp *oflow_e = _ex_neq(ex_h_cast(result, type), sx);
	new_ir.push_back(new Move(oflow, oflow_e));
	cf = ecl(oflow);
	of = ecl(oflow);
	af = ecl(&Constant::f); /* spec: undefined */
	zf = _ex_eq(ecl(low), ex_const(type, 0)); /* spec: undefined */
	sf = ex_h_cast(low, REG_1); /* spec: undefined */
	pf = parity8(low); /* spec: undefined */
	break; }
    case CC_OP_SMULQ: {
	Exp *result = translate_MullS64(ecl(dep1_expr),
					ecl(dep2_expr), &new_ir);
	Exp *high_e, *low_e;
	split_vector(result, &high_e, &low_e);
	Temp *high = mk_temp(REG_64, &new_ir);
	Temp *low = mk_temp(REG_64, &new_ir);
	new_ir.push_back(new Move(high, high_e));
	new_ir.push_back(new Move(low, low_e));
	Exp *sx = _ex_s_cast(ex_h_cast(low, REG_1), REG_64);
	Temp *oflow = mk_temp(REG_1, &new_ir);
	Exp *oflow_e = _ex_neq(ecl(high), sx);
	new_ir.push_back(new Move(oflow, oflow_e));
	cf = ecl(oflow);
	of = ecl(oflow);
	af = ecl(&Constant::f); /* spec: undefined */
	zf = _ex_eq(ecl(low), ex_const(type, 0)); /* spec: undefined */
	sf = ex_h_cast(low, REG_1); /* spec: undefined */
	pf = parity8(low); /* spec: undefined */
	break; }
    default:
	printf("Unsupported x86-64 CC OP %d\n", op);
        /* panic("Unsupported x86-64 CC OP in x64_modify_flags"); */
	return; /* also a memory leak */
    }

    /* For now, don't try to remove the thunk, just like the x86 code
       doesn't. */
    int insert_loc = max(max(op_st, dep1_st), max(dep2_st, ndep_st));
    if (mux0x_st != -1 || ite_cond) {
	// Looks like a conditional thunk; make new
	// conditional assignments using the same condition
	Exp *cond;
	if (ite_cond) {
	    cond = ite_cond;
	} else {
	    Exp *exp_t, *exp_f, *res;
	    int matched = match_ite(ir, mux0x_st, &cond, &exp_t, &exp_f, &res);
	    assert(matched != -1);
	    assert(cond);
	}
	Exp *ite;
	if (cf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), cf, ecl(CF));
	    new_ir.push_back(new Move(CF, ite));
	}
	if (pf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), pf, ecl(PF));
	    new_ir.push_back(new Move(PF, ite));
	}
	if (af) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), af, ecl(AF));
	    new_ir.push_back(new Move(AF, ite));
	}
	if (zf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), zf, ecl(ZF));
	    new_ir.push_back(new Move(ZF, ite));
	}
	if (sf) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), sf, ecl(SF));
	    new_ir.push_back(new Move(SF, ite));
	}
	if (of) {
	    ite = emit_ite(&new_ir, REG_1, ecl(cond), of, ecl(OF));
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




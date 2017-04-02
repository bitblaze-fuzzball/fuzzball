//======================================================================
//
// This file provides the interface to VEX that allows block by block
// translation from binary to VEX IR. 
//
//======================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <set>

#include "asm_program.h"

extern "C" 
{
#include "libvex.h"
}

#include "vexmem.h"
#include "irtoir.h"

#include "vex_version.h"
#ifndef VEX_VERSION
#define VEX_VERSION LIBASMIR_VEX_VERSION
#endif

using namespace std;

//======================================================================
//
// Globals
//
//======================================================================

// Some info required for translation
VexArchInfo         vai_host;
VexGuestExtents     vge;
VexTranslateArgs    vta;
VexTranslateResult  vtr;

// Define a temp buffer to hold the translated bytes
// Not needed with patched VEX
#ifndef HAS_TMPBUF_PATCH
#define             TMPBUF_SIZE 2000
UChar               tmpbuf[TMPBUF_SIZE];
Int                 tmpbuf_used;
#endif

// Global for saving the intermediate results of translation from
// within the callback (instrument1)
IRSB *irbb_current = NULL;

// These choices are somewhat arbitrary. Our translation of SSE is
// far from complete, but it's helpful in debugging for VEX to
// give its semantics for an instruction, even if we can't translate
// them, rather than claiming that the instruction is illegal.
#ifndef VEX_HWCAPS_X86_MMXEXT /* Added in r2745 */
#define VEX_HWCAPS_X86_MMXEXT 0
#endif
#define X86_HWCAPS VEX_HWCAPS_X86_MMXEXT|VEX_HWCAPS_X86_SSE1|VEX_HWCAPS_X86_SSE2|VEX_HWCAPS_X86_SSE3
#define AMD64_HWCAPS VEX_HWCAPS_AMD64_SSE3
#if VEX_VERSION >= 2015
#define ARM_HWCAPS 5 /* ARMv5 */
#else
#define ARM_HWCAPS 0 /* baseline */
#endif

#if defined(__i386)
#define HOST_IS_X86
#define VEX_HOST_ARCH VexArchX86
#elif defined(__amd64)
#define HOST_IS_AMD64
#define VEX_HOST_ARCH VexArchAMD64
#elif defined(__arm__)
#define HOST_IS_ARM
#define VEX_HOST_ARCH VexArchARM
#else
#error Unsupported host architecture for VEX
#endif

//======================================================================
//
// Functions needed for the VEX translation
//
//======================================================================

jmp_buf vex_return;

// This is __attribute__((noreturn)), so we can't return normally.
__attribute__((noreturn))
void failure_exit( void )
{
    longjmp(vex_return, 1);
}

char vex_last_error[2048];
FILE *vex_debug_fp = 0;

FILE *change_vex_debug_out(FILE *new_fp) {
    FILE *old_fp = vex_debug_fp;
    vex_debug_fp = new_fp;
    return old_fp;
}

void log_bytes(
#if VEX_VERSION < 3047
		HChar* bytes, Int nbytes
#else
		const HChar* bytes, SizeT nbytes
#endif
		)
{
    if (vex_debug_fp) {
	fwrite(bytes, 1, nbytes, vex_debug_fp);
    } else {
	assert(strlen(vex_last_error) + nbytes < sizeof(vex_last_error));
	strcat(vex_last_error, bytes);
    }
}

Bool chase_into_ok( void *closureV,
#if VEX_VERSION < 3050
		    Addr64 addr
#else
		    Addr addr
#endif
		    )
{
    return False;
}

void *dispatch( void )
{
    return NULL;
}

//----------------------------------------------------------------------
// This is where we copy out the IRSB
//----------------------------------------------------------------------
#if VEX_VERSION > 2958
#define const_r2958 const
#else
#define const_r2958 /* not const */
#endif
IRSB *instrument1(  void *callback_opaque, 
                    IRSB *irbb,
                    const_r2958 VexGuestLayout *vgl,
                    const_r2958 VexGuestExtents *vge,
#if VEX_VERSION >= 2549
		    const_r2958 VexArchInfo *archinfo_host_unused,
#endif
                    IRType gWordTy,
                    IRType hWordTy )
{

    assert(irbb);
    
    irbb_current = vx_dopyIRSB(irbb);

    // Skip the remaining parts of LibVex_Translate; we got what we
    // came for.
    longjmp(vex_return, 2);

    // return irbb;
}


//----------------------------------------------------------------------
// Given a BB and its containing function, return the number
// of instructions in this BB
//----------------------------------------------------------------------
int count_bb_insns( asm_bb_t *bb, asm_function_t *func )
{
    assert(bb);
    assert(func);

    int count = 0;

    address_t addr = bb->first_addr;

    while ( addr <= bb->last_addr )
    {
        Instruction *inst = func->instmap[addr];

        // Avoid special bb's that have no instructions
        if ( inst == NULL )
            break;

        count++;
        addr += inst->length;
    }

    return count;
}

#if VEX_VERSION >= 2158
static UInt dont_need_self_check ( void* opaque,
#if VEX_VERSION >= 3084
				   VexRegisterUpdates* pxControl,
#endif
				   const_r2958 VexGuestExtents* vge ) {
   return 0;
}
#endif

//----------------------------------------------------------------------
// Initializes VEX 
// It must be called before using VEX for translation to Valgrind IR
//----------------------------------------------------------------------
void translate_init()
{
  static int initialized = 0;
  if (initialized)
    return;
  initialized = 1;

    //
    // Initialize VEX
    //
    VexControl vc;
    LibVEX_default_VexControl(&vc);
    vc.iropt_verbosity              = 0;
    //vc.iropt_level                  = 0;    // No optimization by default
    vc.iropt_level                  = 2;
#if VEX_VERSION < 2454
    vc.iropt_precise_memory_exns    = False;
#elif VEX_VERSION < 3084
    vc.iropt_register_updates       = VexRegUpdUnwindregsAtMemAccess;
#else
    vc.iropt_register_updates_default=VexRegUpdUnwindregsAtMemAccess;
#endif
    vc.iropt_unroll_thresh          = 0;
    vc.guest_max_insns              = 1;    // By default, we translate 1 instruction at a time
    vc.guest_chase_thresh           = 0;    
#if VEX_VERSION >= 1957
    vc.guest_chase_cond             = False;
#endif

    LibVEX_Init(&failure_exit, 
                &log_bytes, 
                0,              // Debug level
#if VEX_VERSION < 2955
                False,          // Valgrind support
#endif
                &vc );

    // Setup the translation args
    vta.arch_guest          = VexArch_INVALID; // to be assigned later
    // vta.archinfo_guest     is assigned after picking arch_guest
    vta.arch_host           = VEX_HOST_ARCH; // compile-time definition above
    LibVEX_default_VexArchInfo(&vai_host);
    switch (vta.arch_host) {
    case VexArchX86:   vai_host.hwcaps = X86_HWCAPS;   break;
    case VexArchAMD64: vai_host.hwcaps = AMD64_HWCAPS; break;
    case VexArchARM:   vai_host.hwcaps = ARM_HWCAPS;   break;
    default:           assert(0); /* unsupported arch. */
    }
#if VEX_VERSION >= 2910
    vai_host.endness = VexEndnessLE;
#endif
    vta.archinfo_host       = vai_host;
    vta.guest_bytes         = NULL;             // Set in translate_insns
    vta.guest_bytes_addr    = 0;                // Set in translate_insns
    vta.callback_opaque     = NULL;             // Used by chase_into_ok, but never actually called
    vta.chase_into_ok       = chase_into_ok;    // Always returns false
    vta.preamble_function   = NULL;
    vta.guest_extents       = &vge;
#ifdef HAS_TMPBUF_PATCH
    vta.host_bytes          = NULL;           // Buffer for storing the output binary
    vta.host_bytes_size     = 0;
    vta.host_bytes_used     = NULL;
#else
    vta.host_bytes          = tmpbuf;           // Buffer for storing the output binary
    vta.host_bytes_size     = TMPBUF_SIZE;
    vta.host_bytes_used     = &tmpbuf_used;
#endif
    vta.instrument1         = instrument1;      // Callback we defined to help us save the IR
    vta.instrument2         = NULL;
#if VEX_VERSION < 2158
    vta.do_self_check       = False;
#else
    vta.needs_self_check    = dont_need_self_check;
#endif
    vta.traceflags          = 0;                // Debug verbosity
#if VEX_VERSION < 2155
    vta.dispatch            = (void *)dispatch; // Not used
#elif VEX_VERSION < 2296
#ifdef HOST_IS_ARM
    vta.dispatch_assisted   = 0; // Return to dispatcher scheme
    vta.dispatch_unassisted = 0; // Return to dispatcher scheme
#else
    vta.dispatch_assisted   = (void *)dispatch; // Not used
    vta.dispatch_unassisted = (void *)dispatch; // Not used either
#endif
#else
    vta.disp_cp_xassisted          = (void *)dispatch; // Not used
    vta.disp_cp_chain_me_to_slowEP = 0;
    vta.disp_cp_chain_me_to_fastEP = 0;
    vta.disp_cp_xindir             = 0;
#endif
#if VEX_VERSION >= 1689
    /* At last check the default values are all zero bits, so this is
       a no-op. But it can't hurt. */
    LibVEX_default_VexAbiInfo(&vta.abiinfo_both);
#endif

}

//----------------------------------------------------------------------
// Translate 1 instruction to VEX IR.
//----------------------------------------------------------------------
IRSB *translate_insn( VexArch guest,
		      unsigned char *insn_start,
		      unsigned int insn_addr,
		      int arch_flags)
{
    VexArchInfo vai_guest;
    LibVEX_default_VexArchInfo(&vai_guest);
    switch (guest) {
    case VexArchX86:
      vai_guest.hwcaps = X86_HWCAPS;
      reg_address_t = REG_32;
      break;
    case VexArchAMD64:
      vai_guest.hwcaps = AMD64_HWCAPS;
      reg_address_t = REG_64;
      break;
    case VexArchARM:
      vai_guest.hwcaps = ARM_HWCAPS;
      reg_address_t = REG_32;
      break;
    default:           assert(0); /* unsupported arch. */
    }
#if VEX_VERSION >= 2910
    vai_guest.endness = VexEndnessLE;
#endif

    vta.arch_guest = guest;
    vta.archinfo_guest = vai_guest;
    vta.guest_bytes         = (UChar *)(insn_start);  // Ptr to actual bytes of start of instruction
    vta.guest_bytes_addr    = (Addr64)(insn_addr);
    vex_last_error[0] = '\0';
    int vex_return_val;

    if (guest == VexArchARM && arch_flags) {
      // VEX uses the low address bit here to represent the Thumb-mode
      // flag.  It's a little weird we have to increment the
      // pointer too, but apparently that's the way it works.
      vta.guest_bytes++;
      vta.guest_bytes_addr |= 1;
    }


#if VEX_VERSION >= 1689
    if (guest == VexArchAMD64) {
      // the only supported value
      vta.abiinfo_both.guest_stack_redzone_size = 128;

#if VEX_VERSION >= 1875
      // Allow both %fs and %gs overrides. This doesn't really assume
      // the partuclar values implied by the (original) field names,
      // just that we will always be able to get the base address out
      // of the guest state.
#if VEX_VERSION < 3043
      vta.abiinfo_both.guest_amd64_assume_fs_is_zero = 1;
      vta.abiinfo_both.guest_amd64_assume_gs_is_0x60 = 1;
#else
      vta.abiinfo_both.guest_amd64_assume_fs_is_const = 1;
      vta.abiinfo_both.guest_amd64_assume_gs_is_const = 1;
#endif
#endif
    }
#endif

    // Do the actual translation
    if (!(vex_return_val = setjmp(vex_return))) {
	vtr = LibVEX_Translate( &vta );
    } else if (vex_return_val == 1){
	// VEX panicked; fixup the state to look like its
	// bad-instruction error return case.
	IRSB *irsb = emptyIRSB();
	addStmtToIRSB(irsb,
#if VEX_VERSION < 2153
		      IRStmt_IMark((Addr64)(insn_addr), 0)
#else
		      IRStmt_IMark((Addr64)(insn_addr), 0, 0)
#endif
		      );
	IRStmt *pc_put = make_pc_put_stmt(guest, insn_addr);
	addStmtToIRSB(irsb, pc_put);
	assert(pc_put->tag == Ist_Put);
	irsb->next = pc_put->Ist.Put.data;
	irsb->jumpkind = Ijk_NoDecode;
	irbb_current = irsb;
    } else if (vex_return_val == 2) {
	// Normal exit
    } else {
	assert(0);
    }

    if (vex_last_error[0]) {
	// This is generally an error that caued VEX to give up
	// processing the instruction, such as "unhandled instruction
	// bytes", or to panic. Printing to stdout is our traditional
	// behavior, but in the future we might consider putting the
	// message in a Special() or something. Or at least providing
	// a way of disabling the printing.
	printf("%s", vex_last_error);
    }

    assert(irbb_current);

    return irbb_current;
}


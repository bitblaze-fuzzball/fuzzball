#ifndef __IRTOIR_H
#define __IRTOIR_H

typedef struct vine_block_s vine_block_t;

#include "ir_program.h"
#include "asm_program.h"
#include "vexmem.h"

//
// VEX headers (inside Valgrind/VEX/pub)
//
#ifdef __cplusplus
extern "C"
{
#endif

#include "libvex_basictypes.h"
#include "libvex_guest_x86.h"
#include "libvex.h"

#ifdef __cplusplus
}

//
// A struct that encapsulates the stages of translation of 1 asm instruction.
// inst is the original instruction.
// vex_ir is inst translated into a block of VEX IR.
// vine_ir is inst translated into a block of Vine IR from its VEX IR translation.
//
struct vine_block_s
{
  Instruction     *inst;
  IRSB            *vex_ir;
  vector<Stmt *>  *vine_ir;
};




//======================================================================
// 
// Translation functions. These should not be used directly unless you
// understand exactly how they work and have a specific need for them.
// If you just wanna translate a program, see generate_vex_ir and 
// generate_vine_ir at the bottom. They wrap these functions and
// provide an easier interface for translation. 
//
//======================================================================

//
// Translates a given IRSB (which is a BB of VEX IR statements), 
// into a vector of Vine IR statements. 
// 
// \param irbb A VEX IRSB, see libvex_ir.h
// \return A vector of Stmt's 
//
vector<Stmt *> *translate_irbb( Instruction *inst, IRSB *irbb );

//
// Translates a special instruction directly from asm to Vine IR.
// Passing in non-special instructions will cause the program to
// terminate with an error message. Instructions should be tested
// using is_special() before being passed in here.
//
// \param inst An asm instruction to be translated
// \return A vector of Stmt's 
//
vector<Stmt *> *translate_special( Instruction *inst );

//
// Tells if a given asm instruction is "special". 
// 
// \param inst An asm instruction
// \return true if the instruction is a "special" instruction.
//
bool is_special( Instruction *inst );

//
// Create a VEX IR statement representing storing a constant value
// into the architecture's program counter (e.g., %eip in x86).
// Used to manually create a no-op IR after catching a fatal VEX error.
// 
// \param arch The architecture
// \param addr The value to store in the PC
// \return A pointer to a VEX IRStmt object
//
IRStmt *make_pc_put_stmt(VexArch arch, Addr64 addr);

//
// Initializes VEX. This function must be called before translate_insn
// can be used. 
//
void translate_init();

//
// Translates 1 asm instruction (in byte form) into a block of VEX IR
//
// \param guest Guest architecture in VEX encoding
// \param insn_start Pointer to bytes of instruction
// \param insn_addr Address of the instruction in its own address space
// \param arch_flags Architecture-specific state, if any
// \return An IRSB containing the VEX IR translation of the given instruction
// 
IRSB *translate_insn( VexArch guest, unsigned char *insn_start, unsigned int insn_addr, int arch_flags );

//
// Given a bb and the function it belongs to, returns the number of
// instructions in this bb
//
// \param bb A basic block
// \param func The function that this bb belongs to
// \return Number of instructions in this bb
//
int count_bb_insns( asm_bb_t *bb, asm_function_t *func );

// 
// Counts the number of asm instructions in this program
//
// \param prog A program
// \return Number of instructions in this program
//
int count_prog_insns( asm_program_t *prog );

//
// Given a start address to a BB, return a pointer to 
// the raw instruction bytes of the BB
//
// \param addr Start address of a BB (not address of physical instruction bytes)
// \return Pointer to physical instruction bytes
//
unsigned char *get_bb_bytes( address_t addr );

//======================================================================
// 
// Debug functions
//
//======================================================================

//
// Print the instruction bytes of a BB in hex to stdout.
// This is a debug function.
//
// \param bb A basic block
//
void print_bb_hex( asm_bb_t *bb );

//
// Print the instructions of a BB in human readable format to stdout.
// This is a debug function.
//
// \param bb A basic block
// \param func The function that this BB belongs to
//
//void print_bb_insns( asm_bb_t *bb, asm_function_t *func );


//======================================================================
// 
// Utility functions that wrap the raw translation functions.
// These are what should be used to do the actual translation.
// See print-ir.cpp in ../../apps for examples of how to use these.
//
//======================================================================

// Take an instrs and translate it into a VEX IR block
// and store it in a vine block
vine_block_t* generate_vex_ir(VexArch guest, Instruction *inst);

//
// Take a vector of instrs and translate it into VEX IR blocks
// and store them in a vector of vine blocks
//
// \param instrs the list of instructions to translate
// \return A vector of vine blocks. Only the inst and vex_ir fields in
//         the vine block are valid at this point.
//

vector<vine_block_t *> generate_vex_ir(asm_program_t *prog, const vector<Instruction *> &instrs);

//
// Take a disassembled function and translate it into VEX IR blocks
// and store them in a vector of vine blocks
//
// \param func A disassembled function
// \return A vector of vine blocks. Only the inst and vex_ir fields in
//         the vine block are valid at this point.
//
vector<vine_block_t *> generate_vex_ir(asm_program_t *prog, asm_function_t *func);

//
// Take a disassembled program and translate it into VEX IR blocks
// and store them in a vector of vine blocks
//
// \param prog A disassembled program
// \return A vector of vine blocks. Only the inst and vex_ir fields in
//         the vine block are valid at this point.
//
vector<vine_block_t *> generate_vex_ir( asm_program_t *prog );

// Take a vine block that has gone through VEX translation and translate it
// to Vine IR.
void generate_vine_ir_block( asm_program_t *prog, vine_block_t *block );

//
// Take a vector of vine blocks that have gone through VEX translation
// and translate them to Vine IR
//
// \param vblocks Vector of vine blocks with valid VEX IR translations
// \return Vector of vine blocks with the vine_ir field filled in
//
vector<vine_block_t *> generate_vine_ir( asm_program_t *prog, vector<vine_block_t *> vblocks );


//
// Take a vector of vine blocks and merge them into a single vector of
// Vine IR statements
//
// \param vblocks Vector of vine blocks with valid vine_ir field
// \return Vector of Vine IR statements
//
vector<Stmt *> merge_ir( vector<vine_block_t *> vblocks );

// XXX: copied from VEX/priv/guest-x86/gdefs.h
enum {
    X86G_CC_OP_COPY=0,  /* DEP1 = current flags, DEP2 = 0, NDEP = unused */
                        /* just copy DEP1 to output */

    X86G_CC_OP_ADDB,    /* 1 */
    X86G_CC_OP_ADDW,    /* 2 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_ADDL,    /* 3 */

    X86G_CC_OP_SUBB,    /* 4 */
    X86G_CC_OP_SUBW,    /* 5 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_SUBL,    /* 6 */

    X86G_CC_OP_ADCB,    /* 7 */
    X86G_CC_OP_ADCW,    /* 8 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    X86G_CC_OP_ADCL,    /* 9 */

    X86G_CC_OP_SBBB,    /* 10 */
    X86G_CC_OP_SBBW,    /* 11 DEP1 = argL, DEP2 = argR ^ oldCarry, NDEP = oldCarry */
    X86G_CC_OP_SBBL,    /* 12 */

    X86G_CC_OP_LOGICB,  /* 13 */
    X86G_CC_OP_LOGICW,  /* 14 DEP1 = result, DEP2 = 0, NDEP = unused */
    X86G_CC_OP_LOGICL,  /* 15 */

    X86G_CC_OP_INCB,    /* 16 */
    X86G_CC_OP_INCW,    /* 17 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    X86G_CC_OP_INCL,    /* 18 */

    X86G_CC_OP_DECB,    /* 19 */
    X86G_CC_OP_DECW,    /* 20 DEP1 = result, DEP2 = 0, NDEP = oldCarry (0 or 1) */
    X86G_CC_OP_DECL,    /* 21 */

    X86G_CC_OP_SHLB,    /* 22 DEP1 = res, DEP2 = res', NDEP = unused */
    X86G_CC_OP_SHLW,    /* 23 where res' is like res but shifted one bit less */
    X86G_CC_OP_SHLL,    /* 24 */

    X86G_CC_OP_SHRB,    /* 25 DEP1 = res, DEP2 = res', NDEP = unused */
    X86G_CC_OP_SHRW,    /* 26 where res' is like res but shifted one bit less */
    X86G_CC_OP_SHRL,    /* 27 */

    X86G_CC_OP_ROLB,    /* 28 */
    X86G_CC_OP_ROLW,    /* 29 DEP1 = res, DEP2 = 0, NDEP = old flags */
    X86G_CC_OP_ROLL,    /* 30 */

    X86G_CC_OP_RORB,    /* 31 */
    X86G_CC_OP_RORW,    /* 32 DEP1 = res, DEP2 = 0, NDEP = old flags */
    X86G_CC_OP_RORL,    /* 33 */

    X86G_CC_OP_UMULB,   /* 34 */
    X86G_CC_OP_UMULW,   /* 35 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_UMULL,   /* 36 */

    X86G_CC_OP_SMULB,   /* 37 */
    X86G_CC_OP_SMULW,   /* 38 DEP1 = argL, DEP2 = argR, NDEP = unused */
    X86G_CC_OP_SMULL,   /* 39 */

    X86G_CC_OP_NUMBER
};

// eflags helpers
// (making these public to help generate thunks)
vector<Stmt *> mod_eflags_copy( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_add( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_sub( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_adc( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 );
vector<Stmt *> mod_eflags_sbb( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 );
vector<Stmt *> mod_eflags_logic( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_inc( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 );
vector<Stmt *> mod_eflags_dec( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 );
vector<Stmt *> mod_eflags_shl( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_shr( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_rol( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 );
vector<Stmt *> mod_eflags_ror( reg_t type, Exp *arg1, Exp *arg2, Exp *arg3 );
vector<Stmt *> mod_eflags_umul( reg_t type, Exp *arg1, Exp *arg2 );
vector<Stmt *> mod_eflags_smul( reg_t type, Exp *arg1, Exp *arg2 );

vector<Stmt*> gen_eflags_helpers();

/* Redirect the location of VEX debugging output. Returns the old
   location, if you want to restore it later. A null value will cause
   the output to be kept in a limited-size internal buffer. */
FILE *change_vex_debug_out(FILE *new_fp);

extern "C" {
  typedef struct vector<vine_block_t *> vine_blocks_t;

#else
  struct vine_block_s
  {
    Instruction     *inst;
    IRSB            *vex_ir;
    struct vector_Stmt  *vine_ir;
  };


  typedef struct vector_vine_block_t vine_blocks_t;
#endif  // __cplusplus

  /// Enable/disable eflags thunks code. 
  extern void set_use_eflags_thunks(bool value);
  extern bool get_use_eflags_thunks();
  /// If 1, calls/returns translated as Call/Return.
  /// If 0, calls/returns translated as Jmp
  extern void set_call_return_translation(int value);
  extern Stmt** gen_eflags_helpers_c();
  extern vine_blocks_t * asmir_asmprogram_to_vine(asm_program_t *prog);
  extern int asmir_vine_blocks_size(vine_blocks_t *bs);
  extern vine_block_t * asmir_vine_blocks_get(vine_blocks_t *bs, int i);
  extern void destroy_vine_blocks(vine_blocks_t *bs);
  extern void destroy_vine_block(vine_block_t *bs);
  extern int asmir_vine_block_size(vine_block_t *b);
  extern address_t asmir_vine_block_address(vine_block_t *b);
  extern Stmt * asmir_vine_block_get(vine_block_t *b, int i);
  extern asm_program_t* byte_insn_to_asmp(enum asmir_arch arch, address_t addr, unsigned char *bb_bytes, unsigned int len);
  extern char* string_blockinsn(asm_program_t *prog, vine_block_t *block);

#ifdef __cplusplus
}
#endif

#endif


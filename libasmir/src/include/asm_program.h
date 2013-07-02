#ifndef _ASM_PROGRAM_H
#define _ASM_PROGRAM_H

//#include "disasm.h"

#include <assert.h>

#ifdef __cplusplus
#include <vector>
#include <map>
#include <string>
#include <algorithm>
#include <utility>
#include <iostream>
#include <set>
#include <sstream>

// from binutils
#include "config.h"
#include <bfd.h>
#include <dis-asm.h>

#include "common.h"


using namespace std;

typedef struct raw_inst Instruction; // this used to be struct i386_instruction

typedef struct _segment
{
  bfd_byte *data;
  bfd_size_type datasize;
  
  bfd_vma start_addr;
  bfd_vma end_addr;

  asection *section;
  int is_code;

  //Instruction **insts;
  //unsigned char *status;

  //Queue functions;

  struct _segment *next;
} Segment;
typedef Segment section_t;

typedef struct asm_bb {
  address_t first_addr; // first inst address in the block
  address_t last_addr; // last inst address in the block
} asm_bb_t;

/// Use of asm_function
///  Instruction *inst = f->instmap[address]
///  asm_bb_t *block = f->blockmap[address]
///  use 'blocks' to iterate through all blocks
typedef struct asm_function {
  string name;
  //cfg_vertex_t cg_num; // boost vertex descriptor for fun in the callgraph.
  address_t start_addr;
  address_t end_addr;
  map<address_t, Instruction *> instmap; // map inst address to Instruction
  //map<address_t, asm_bb_t *> blockmap; // map inst address to block
  //set<asm_bb_t *> blocks;  // all blocks for this function
  //set<address_t> calls;
  //cfg_t cfg;
} asm_function_t;

// Information about dynamic function. Deprecated
typedef struct dyn_function {
   string name;
   address_t addr; 
} dyn_function_t;

typedef struct vine_symbol {
  string name;
  address_t addr;
  int is_function;
  int is_dynamic;
} vine_symbol_t;

enum asmir_arch {
  asmir_arch_x86,
  asmir_arch_x64, /* AKA AMD64, x86_64, x86-64, Intel 64 */
  asmir_arch_arm,
};

typedef struct asm_program {
  string name;
  set<asymbol *> symbols;
  map<address_t, section_t *> sections;
  map<address_t, asm_function_t *> functions;
  //cfg_t cg; /// callgraph
  Segment *segs; // linked list of segments
  bfd *abfd;
  enum asmir_arch asmir_arch;
  struct disassemble_info disasm_info;
  unsigned char *fake_asmp_bytes;
} asm_program_t;

extern bfd *immortal_bfd_for_x86;
extern bfd *immortal_bfd_for_x64;
extern bfd *immortal_bfd_for_arm;

typedef struct raw_inst {
  address_t address; // Instruction address
  int length;
  int arch_flags; // For ARM, 0 = ARM, 1 = Thumb
  uint8_t *bytes; // Instruction bytes. NOTE: these are not freshly
                  //  allocated bytes. The bytes are shared with
                  // bfd, and de-allocated by it.
} raw_inst_t;

typedef struct memory_cell_data {
    address_t address;
    int value; // suppose to have 256 possible byte values
} memory_cell_data_t;


#else // ndef __cplusplus

typedef struct asm_function asm_function_t;
typedef struct asm_program asm_program_t;
typedef struct dyn_function dyn_function_t;
typedef struct vine_symbol_info vine_symbol_info_t;
typedef struct raw_inst raw_inst_t;
#endif // __cplusplus



#ifdef __cplusplus
extern "C" {
#endif

  typedef struct vector<dyn_function_t *> dyn_functions_t;
  typedef struct vector<vine_symbol_t *> vine_symbols_t;

  typedef struct vector<raw_inst_t *> raw_insts_t;

  typedef struct vector<memory_cell_data_t *> memory_data_t;

/// Disassemble a single function. Note that @param f is assumed
/// to be allocated, and @param abfd is the bfd for the file.
  extern void disassemble_function(asm_function_t *f, asm_program_t *prog);
  extern asm_program_t *disassemble_program(const char *filename);

  // Choose whether disassembly is AT&T format (is_intel=false), or
  // Intel format (is_intel=true). Perhaps in the future this should
  // be changed to not be a global option, or generalized to set
  // other disassembler options.
  extern void set_disasm_format_intel(bool is_intel);

  address_t get_function_address(asm_program_t *prog, string name);

  extern asm_function_t *get_nearest_function(const asm_program_t *prog,
					      const address_t  &addr);
  extern void free_asm_function(asm_function_t *f);
  extern void free_asm_program(asm_program_t *p);

  extern dyn_functions_t *get_synthetic_symbols(const char *filename);
  extern int dyn_functions_size(dyn_functions_t *ds);
  extern dyn_function_t * dyn_functions_get(dyn_functions_t *ds, int i);
  extern const char* dyn_functions_name(dyn_function_t *d);
  extern address_t dyn_functions_addr(dyn_function_t *d);
  extern void destroy_dyn_functions(dyn_functions_t *ds);

  // Return static and dynamic symbols from filename
  extern vine_symbols_t *get_symbols_of_file(const char *filename);
  extern int symbols_size(vine_symbols_t *ds);
  extern vine_symbol_t *symbols_get(vine_symbols_t *ds, int i);
  extern void destroy_symbols(vine_symbols_t *ds);

  extern int raw_insts_size(raw_insts_t *r);
  extern raw_inst_t * raw_insts_get(raw_insts_t *r, int i);
  extern void destroy_raw_insts(raw_insts_t *t);
  extern raw_insts_t *raw_insts_of_file(const char *filename);

  extern void destroy_memory_data(memory_data_t *md);
  extern address_t memory_cell_data_address(memory_cell_data_t *md);
  extern int memory_cell_data_value(memory_cell_data_t *md);
  extern int memory_data_size(memory_data_t *md);
  extern memory_cell_data_t * memory_data_get(memory_data_t *md, int i);
  extern memory_data_t * get_rodata (asm_program_t *prog);
  extern memory_data_t * get_bssdata(asm_program_t *prog);


  /* create the instruction of memory address addr. Only for use
     when creating an asm_program_t */
  Instruction* get_inst_of(asm_program_t *prog, bfd_vma addr);
#ifdef __cplusplus
}
#endif

/// Take a list of instructions, and populate f with
///  - basic blocks
///  - cfg
/// @param f is assumed to be allocated.
void populate_function(asm_function_t *f, 
		       const vector<Instruction *> &instrs);


/// (re)-calculate the set of calls made by f.
set<address_t> calculate_callees(asm_function_t *f);

bfd* initialize_bfd(const char *filename);


Instruction* get_insn(struct disassemble_info *info, bfd_vma offset, struct _segment *segment);
void ostream_insn(asm_program_t *prog, Instruction *inst, ostream &out);

// returns a string which is valid until the next call to this function
char* string_of_insn(asm_program_t *prog, Instruction *inst);

// pulled from disasm.h
#ifndef bfd_get_section_size_before_reloc
#define bfd_get_section_size_before_reloc(x) bfd_get_section_size(x)
#endif



#endif

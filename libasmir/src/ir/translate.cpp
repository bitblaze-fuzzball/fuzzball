#include "translate.h"
#include <bfd.h>

extern bool use_eflags_thunks;


// from asm_program.cpp
void init_disasm_info(asm_program_t *prog);

asm_program_t *instmap_to_asm_program(instmap_t *map)
{
  return map->prog;
}

// Returns a fake asm_program_t for use when disassembling bits out of memory
asm_program_t* fake_prog_for_arch(enum asmir_arch arch)
{
  asm_program_t *prog = new asm_program_t;
  prog->segs = 0; /* no segments */
  prog->asmir_arch = arch;

  if (arch == asmir_arch_x86) {
    if (!immortal_bfd_for_x86) {
      bfd *bfd = bfd_openw("/dev/null", NULL);
      assert(bfd);
      bfd_set_arch_info(bfd, bfd_lookup_arch(bfd_arch_i386, 0));
      if (!bfd->arch_info) {
	fprintf(stderr, "BFD architecture lookup failed: does your Binutils support bfd_arch_i386?\n");
	assert(bfd->arch_info);
      }
      immortal_bfd_for_x86 = bfd;
    }
    prog->abfd = immortal_bfd_for_x86;
  } else if (arch == asmir_arch_arm) {
    if (!immortal_bfd_for_arm) {
      bfd *bfd = bfd_openw("/dev/null", NULL);
      assert(bfd);
      bfd_set_arch_info(bfd, bfd_lookup_arch(bfd_arch_arm, 0));
      if (!bfd->arch_info) {
	fprintf(stderr, "BFD architecture lookup failed: does your Binutils support bfd_arch_arm?\n");
	assert(bfd->arch_info);
      }
      immortal_bfd_for_arm = bfd;
    }
    prog->abfd = immortal_bfd_for_arm;
  } else if (arch == asmir_arch_x64) {
    if (!immortal_bfd_for_x64) {
      bfd *bfd = bfd_openw("/dev/null", NULL);
      assert(bfd);
      bfd_set_arch_info(bfd, bfd_lookup_arch(bfd_arch_i386, bfd_mach_x86_64));
      if (!bfd->arch_info) {
	fprintf(stderr, "BFD architecture lookup failed: does your Binutils support bfd_mach_x64_64?\n");
	assert(bfd->arch_info);
      }
      immortal_bfd_for_x64 = bfd;
    }
    prog->abfd = immortal_bfd_for_x64;
  } else {  
    fprintf(stderr, "Unrecognized architecture in fake_prog_for_arch\n");
    assert(0);
  }

  init_disasm_info(prog);
  return prog;
}


int instmap_has_address(instmap_t *insts,
			address_t addr)
{
  return (insts->imap.find(addr) != insts->imap.end());
}

/*
cflow_t *instmap_control_flow_type(instmap_t *insts,
		      address_t addr)
{
  cflow_t * ret = (cflow_t *) malloc(sizeof(cflow_t));

  assert(insts->imap.find(addr) != insts->imap.end());
  Instruction *inst = insts->imap.find(addr)->second;
  address_t target;
  ret->ctype = get_control_transfer_type(inst, &target);
  ret->target = target;
  return ret;
}
*/

vine_block_t *instmap_translate_address(instmap_t *insts,
					address_t addr)
{
  translate_init();
  vx_FreeAll();
  vector<Instruction *> foo;

  assert(insts->imap.find(addr) != insts->imap.end());

  foo.push_back(insts->imap.find(addr)->second);
  vector<vine_block_t *> blocks = generate_vex_ir(insts->prog, foo);
  blocks = generate_vine_ir(insts->prog, blocks);
  assert(blocks.size() == 1);
  vine_block_t *block = *(blocks.begin());
  block->vex_ir = NULL;
  vx_FreeAll();
  return block;
}


vine_blocks_t *instmap_translate_address_range(instmap_t *insts,
					   address_t start,
					   address_t end)
{
  translate_init();
  vx_FreeAll();
  vector<Instruction *> foo;
  vine_blocks_t *ret = new vine_blocks_t;

  for(address_t i = start; i <= end; i++){
    if(insts->imap.find(i) == insts->imap.end()) continue;
    foo.push_back(insts->imap.find(i)->second);
   }
  vector<vine_block_t *> blocks = generate_vex_ir(insts->prog, foo);
  blocks = generate_vine_ir(insts->prog, blocks);

  for(unsigned i = 0; i < blocks.size(); i++){
    vine_block_t *block = blocks.at(i);
    assert(block);
    
    if(block->inst == NULL)
      continue;
    
    /* This is broken as it removes the jumps from the end of
       repz instructions. Just because control flow eventually goes on
       to the following instruction doesn't mean that jump is at the end.
       It should simply be checking that the jump is to the following
       instruction since we know where that is.
    // If the statement isn't control flow, and not the last
    // remove the dummy jumps to the next block that vex inserts
    bfd_vma target;
    int ctype = get_control_transfer_type(block->inst, &target);
    Stmt *s;
    switch(ctype){
    case INST_CONT:
      s = block->vine_ir->back();
      if(s->stmt_type == JMP){
	block->vine_ir->pop_back();
      }
      break;
    default:
      break;
    }
    */
  }
  
  ret->insert(ret->end(), blocks.begin(), blocks.end());
  
  for(vector<vine_block_t *>::iterator it = ret->begin();
      it != ret->end(); it++){
    vine_block_t *block = *it;
    block->vex_ir = NULL; // this isn't available. 
  }

  return ret;
}



address_t get_last_segment_address(const char *filename, 
				   address_t addr)
{
  bfd *abfd = initialize_bfd(filename);
  unsigned int opb = bfd_octets_per_byte(abfd);

  address_t ret = addr;
  for(asection *section = abfd->sections; 
      section != (asection *)  NULL; section = section->next){

    bfd_vma datasize = bfd_get_section_size_before_reloc(section);
    if(datasize == 0) continue;

    address_t start = section->vma;
    address_t end = section->vma + datasize/opb;
    if(addr >= start && addr <= end)
      return end;
  }
  return ret;
}

instmap_t *
filename_to_instmap(const char *filename)
{
  instmap_t *ret = new instmap_t;
  asm_program_t *prog = disassemble_program(filename);
  ret->prog = prog;

  // iterate over functions
  for ( map<address_t, asm_function_t *>::const_iterator i = prog->functions.begin();
	i != prog->functions.end(); i++ )
    {
      asm_function_t *f = i->second;
      ret->imap.insert(f->instmap.begin(), f->instmap.end());
      //ret->insert(pair<address_t, Instruction *>(inst->address, inst));
    }
 
  return ret;
}


// from asm_program.cpp
void init_disasm_info(bfd *abfd, struct disassemble_info *disasm_info);


// Quick helper function for ocaml, since we don't have a proper libopcodes
// interface yet.
// This isn't really the right place for it...
extern "C" {
void print_disasm_rawbytes(enum asmir_arch arch,
			   bfd_vma addr,
			   const char *buf, int size)
{
  asm_program_t *prog = 
    byte_insn_to_asmp(arch, addr, (unsigned char*)buf, size);
  
  if (!prog)
    return;

  disassembler_ftype disas = disassembler(prog->abfd);
  disas(addr, &prog->disasm_info);
  free_asm_program(prog);
  fflush(stdout);
}

char* sprintf_disasm_rawbytes(enum asmir_arch arch,
			      bool is_intel,
			      bfd_vma addr,
			      const char *buf, int size)
{
  asm_program_t *prog = 
    byte_insn_to_asmp(arch, addr, (unsigned char*)buf, size);

  if (!prog)
    return NULL;

  if (is_intel)
    prog->disasm_info.disassembler_options = (char *)"intel";
  else
    prog->disasm_info.disassembler_options = (char *)"";

  Instruction *inst = new Instruction;
  if (!inst)
    return NULL;
  inst->address = addr;
  inst->bytes = (bfd_byte *)buf;
  inst->length = size;


  char *s = string_of_insn(prog, inst);
  free_asm_program(prog);
  delete inst;

  return s;
}

}



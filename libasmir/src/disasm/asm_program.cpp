#include <errno.h>
#include "debug.h"
#include "asm_program.h"
#include "objdump.h"

/* Like the one in GNU libiberty, but that's getting harder to link to
   these days, so just use our own version */
static void *xmalloc(size_t size);

static void *xalloc(size_t num, size_t size);
static void initialize_sections(bfd *, asm_program_t *p);
static map<address_t, asm_function_t *>
   identify_functions(const map<address_t, section_t *> sections, bfd *abfd);

//extern Segment *segs;

asm_function_t *get_nearest_function(const asm_program_t *prog,
				     const address_t  &addr)
{
  if(prog->functions.find(addr) != prog->functions.end())
    return prog->functions.find(addr)->second;
  
  for(map<address_t, asm_function_t *>::const_iterator it = 
	prog->functions.begin(); it != prog->functions.end();
      it++){
    asm_function_t *f = it->second;
    if(addr >= f->start_addr && addr <= f->end_addr)
      return f;
  }
  return NULL;
}


address_t get_function_address(asm_program_t *prog, string name)
{
  for(map<address_t, asm_function_t *>::const_iterator it =
	prog->functions.begin(); it != prog->functions.end();
      it++){
    asm_function_t *f = it->second;
    if(f->name == name){
      return it->first;
    }
  }
  return 0;
}

/* find the segment for memory address addr */
// pulled from disasm.cpp
Segment* get_segment_of(asm_program_t *prog, bfd_vma addr)
{
  Segment *segment;

  for (segment = prog->segs; segment != NULL; segment = segment->next)
    if ((addr >= segment->start_addr) && (addr < segment->end_addr))
      return segment;

  return NULL;
}


/// Code takes from get_byte in disasm.cpp
bfd_byte *get_ptr_to_instr(asm_program_t *prog, bfd_vma addr)
{
  Segment *seg = get_segment_of(prog, addr);
  if(seg == NULL){
    fprintf(stderr, "Segment for %llx not found\n", (unsigned long long)addr);
    return NULL;
    exit(-1);
  }
  bfd_byte *ptr = seg->data + (addr - seg->start_addr);
  return ptr;
}

int raw_insts_size(raw_insts_t *r)
{
  int size = r->size();
  return size;
}

raw_inst_t * raw_insts_get(raw_insts_t *r, int i)
{
  return r->at(i);
}


void destroy_raw_insts(raw_insts_t *t)
{
  for(vector<raw_inst_t *>::iterator i = t->begin();
      i != t->end(); i++){
    raw_inst_t *x = *i;
    delete x;
  }
  delete t;
}
/* do we need this? --aij
raw_insts_t *
raw_insts_of_file(const char *filename)
{
  //segs = NULL;
  raw_insts_t *ret = new raw_insts_t;
  asm_program_t *prog = new asm_program_t;

  bfd *abfd = initialize_bfd(filename);
  initialize_sections(abfd, prog);
  prog->functions = identify_functions(prog->sections, abfd);


  for(map<address_t, asm_function_t *>::const_iterator it = 
	prog->functions.begin(); it != prog->functions.end();
      it++){
    asm_function_t *f = it->second;
    bfd_vma pc = f->start_addr;

    // Disassemble the function
    while(pc <= f->end_addr){
      bfd_byte *ptr = get_ptr_to_instr(prog, pc);

      raw_inst_t *temp = new raw_inst_t;
      temp->length = instr->length;
      temp->bytes = ptr;
      temp->address = instr->address;
      ret->push_back(temp);
      pc += instr->length;
    }
  }
  return ret;
}
*/
asm_program_t *
disassemble_program(const char *filename)
{
  asm_program_t *prog = new asm_program_t;
  //segs = NULL;
  bfd *abfd = initialize_bfd(filename);
  prog->abfd = abfd;
  initialize_sections(abfd, prog);
  prog->fake_asmp_bytes = 0;
  prog->functions = identify_functions(prog->sections, abfd);
  unsigned long total_instrs =0;
  set<asm_function_t *> functions;
  for(map<address_t, asm_function_t *>::const_iterator it = 
	prog->functions.begin(); it != prog->functions.end();
      it++){
    // Give each function a unique ID (for BGL)
    // and fill in control flow graph, etc for function
    asm_function_t *f = it->second;
    //f->cg_num = cg_num++;
    disassemble_function(f, prog);
    functions.insert(f);
    if(is_debug_on("stats")){
      for(map<address_t, Instruction *>::const_iterator iit = 
	    f->instmap.begin(); iit != f->instmap.end(); iit++){
	total_instrs++;
      }
    }
  }
  
  print_debug("stats", "Size of program %u (instrs)\n", total_instrs);
  //create_callgraph(prog);
  return prog;
}


// vine_symbols_t * get_symbols_of_program(asm_program_t *p)
// {
//   vector<vine_symbol_t *> *ret = new vector<vine_symbol_t *>();

//   for(map<address_t,asm_function_t *>::const_iterator it= p->functions.begin();
//       it != p->functions.end(); it++){
//     asm_function_t *f = it->second;
//     vine_symbol_t *temp = new vine_symbol_t;
//     temp->name = strdup(f->name.c_str());
//     temp->addr = f->start_addr;
//     temp->is_function = true;
//     temp->is_dynamic = false;
//   }
//   return ret;
// }

bfd *immortal_bfd_for_x86 = 0;
bfd *immortal_bfd_for_x64 = 0;
bfd *immortal_bfd_for_arm = 0;

void free_asm_program(asm_program_t *p)
{
  for(map<address_t, section_t *>::const_iterator it = 
	p->sections.begin(); it != p->sections.end();
      it++) {
    // FIXME: is this all that's required to free sections?
    free(it->second);
  }  
  for(map<address_t, asm_function_t *>::const_iterator it = 
	p->functions.begin(); it != p->functions.end();
      it++) {
    free_asm_function(it->second);
  }  
  if (p->abfd != immortal_bfd_for_x86 && p->abfd != immortal_bfd_for_x64
      && p->abfd != immortal_bfd_for_arm)
    bfd_close_all_done(p->abfd);
  if (p->fake_asmp_bytes)
    free(p->fake_asmp_bytes);
  delete p;
}

void free_asm_function(asm_function_t *f)
{
  for(map<address_t, Instruction *>::const_iterator i = 
	f->instmap.begin(); i != f->instmap.end(); i++){
    delete i->second;
  }
  delete f; // FIXME: is that all?
}

static int ignore() {
  return 1;
}

int get_instr_length(asm_program_t *prog, bfd_vma addr)
{
  disassembler_ftype disas = disassembler(prog->abfd);
  fprintf_ftype old_fprintf_func = prog->disasm_info.fprintf_func;
  prog->disasm_info.fprintf_func = (fprintf_ftype)ignore;
  assert(disas);
  int len = disas(addr, &prog->disasm_info);
  prog->disasm_info.fprintf_func = old_fprintf_func;
  return len;
}

/* create the instruction of memory address addr */
Instruction* get_inst_of(asm_program_t *prog, bfd_vma addr)
{
  // FIXME: redesign this whole file with memory management
  bfd_byte *b = get_ptr_to_instr(prog,addr);
  if (!b)
    return NULL;

  int len = get_instr_length(prog,addr);
  if (-1 == len)
    return NULL;

  Instruction *inst = new Instruction;
  inst->address = addr;
  inst->bytes = b;
  inst->length = len;
  return inst;
}



void disassemble_function(asm_function_t *f, asm_program_t *prog)
{
  bfd_vma pc;
  Instruction *instr;
  pc = f->start_addr;
  vector<Instruction *> instrs;
  // Disassemble the function
  while(pc <= f->end_addr){
    instr = get_inst_of(prog, pc);
    if(instr == NULL){
      // FIXME: This means we would stop disassembling if someone inserted
      // giberish that never gets executed into a function
      cerr <<"Warning: disassemble_function: stopping early for function "
	   << f->name <<endl;
      f->end_addr = pc;
      break;
    }
    //instrs.push_back(instr);
    f->instmap.insert(pair<address_t, Instruction *>(instr->address, instr));
    pc += instr->length;
  }
  
  //populate_function(f, instrs);
}


map<address_t, asm_function_t *>
get_stripped_binary_functions(bfd *abfd)
{
  // For a stripped binary, we  treat an executable section as a
  // function. 
  unsigned int opb = bfd_octets_per_byte(abfd);
  print_debug("warning", "no symbol table. disassembling each section");
  map<address_t, asm_function_t *> ret;
  for(asection *section = abfd->sections; section != (asection *) NULL; 
      section = section->next){
    if(!(section->flags & SEC_CODE)) continue;
    bfd_size_type datasize = 0;
    datasize = bfd_get_section_size_before_reloc (section);
    if (datasize == 0)
      continue;      
    asm_function_t *f = new asm_function_t;
    f->start_addr =  section->vma;
    f->end_addr = section->vma + datasize / opb;
    ostringstream os;
    os << "section_" << hex << section->vma;
    f->name = os.str();
    ret.insert(pair<address_t, asm_function_t *>(f->start_addr, f));
  }
  return ret;
}

map<address_t, asm_function_t *>
identify_functions(const map<address_t, section_t *> sections, bfd *abfd)
{
  map<address_t, asm_function_t *> funcs;
  //set<asm_function_t *> funcs;
  long storage_needed;
  asymbol **symtbl;
  long symcount, sorted_symcount;
  /* 
     We could identify function boundries in stripped binaries by
     looking for the function prologue. TBD.
   */ 
  if(!(bfd_get_file_flags(abfd) & HAS_SYMS)){
    return get_stripped_binary_functions( abfd);
    //fatal("identify_functions", 
	  //"No support for stripped binaries (yet)");
  }

  //assert(bfd_get_flavour(abfd) == bfd_target_elf_flavour);
  /*
    instead of handling only elf format binary, allow other types 
    with a warning -- pongsin
  */
  if (bfd_get_flavour(abfd) != bfd_target_elf_flavour)
      cerr << "Warning! The binary is not in ELF format. " 
	   << "Other formats haven't been thoroughly tested."
	   << endl;

  storage_needed = bfd_get_symtab_upper_bound(abfd);
  if(storage_needed <= 0){
    fatal("identify_functions", "symbol table damaged");
  }
  
  symtbl = (asymbol **) xmalloc(storage_needed);
  symcount = bfd_canonicalize_symtab(abfd, symtbl);
  if(symcount <= 0)
    fatal("initialize_symtable", "symbol table damaged");

  sorted_symcount = remove_useless_symbols(symtbl, symcount);
  qsort(symtbl, sorted_symcount, sizeof(asymbol *), compare_symbols);
  
  long i, place;
  asymbol *sym, *nextsym;

  for(i = 0; i < sorted_symcount; i++){
    sym = symtbl[i];
    /*    if(sym->name != NULL){
      printf("name: %s %u F:%x\n", sym->name, sym->flags & BSF_FUNCTION,
    sym->flags);
    } */
    if(sym->flags & BSF_FUNCTION){
      asm_function_t *f = new asm_function_t;
      f->start_addr = sym->section->vma + sym->value;
      f->name = sym->name;
      //      memcpy(&(s->symbol), sym, sizeof(asymbol));
      place = i;
      // FIXME: this still doesn't do the right thing for _init on ARM
      // also, this doesn't work for the last symbol
      while(place < sorted_symcount && (bfd_asymbol_value(sym) >=
				bfd_asymbol_value(symtbl[place])) )
	place++;
      nextsym = symtbl[place];

      // FIXME: We could approximate end_addr better, eg, by looking at where
      // the section ends.
      /* end addrress is approximation. the last valid address
	 may be before this value */
      f->end_addr =  nextsym->value + nextsym->section->vma;

      //bfd_symbol_info(sym, &(s->info));
      // end_addr so far is really the start_addr of the next function
      // subtract one if possible.
      if(f->end_addr != f->start_addr)
	f->end_addr--;
      
      if(funcs.find(f->start_addr) != funcs.end()){
	asm_function_t *old = funcs[f->start_addr];
	// Replace old functions with the new one if the new
	// one is bigger.
	if( (f->end_addr - f->start_addr) >
	    (old->end_addr - old->start_addr)){
	  funcs[f->start_addr] =  f;
	}
      } else {
	funcs[f->start_addr] = f;
      }
    }
  }
   free(symtbl);  
   return funcs;
}

static int
my_read_memory (bfd_vma memaddr,
		bfd_byte *myaddr,
		unsigned int length,
		struct disassemble_info *info)
{
  int ret = buffer_read_memory(memaddr,myaddr,length,info);

  if (EIO == ret) {
    Segment *seg = get_segment_of((asm_program_t*)info->application_data, memaddr);
    if (NULL == seg)
      return EIO;

    info->buffer = seg->data;
    info->buffer_vma = seg->start_addr;
    info->buffer_length = seg->datasize;
    info->section = seg->section;

    ret = buffer_read_memory(memaddr,myaddr,length,info);
  }
  return ret;
}

static const char *disasm_options_att = "";
static const char *disasm_options_intel = "intel";
static const char *disassembler_options = disasm_options_att;

void set_disasm_format_intel(bool is_intel)
{
  if (is_intel)
    disassembler_options = disasm_options_intel;
  else
    disassembler_options = disasm_options_att;
}

void init_disasm_info(bfd *abfd, struct disassemble_info *disasm_info)
{
  init_disassemble_info (disasm_info, stdout, (fprintf_ftype) fprintf);
  disasm_info->flavour = bfd_get_flavour (abfd);
  disasm_info->arch = bfd_get_arch (abfd);
  disasm_info->mach = bfd_get_mach (abfd);
  disasm_info->disassembler_options = (char *)disassembler_options;
  disasm_info->octets_per_byte = bfd_octets_per_byte (abfd);
  //disasm_info->skip_zeroes = DEFAULT_SKIP_ZEROES;
  //disasm_info->skip_zeroes_at_end = DEFAULT_SKIP_ZEROES_AT_END;
  disasm_info->disassembler_needs_relocs = FALSE;

  if (bfd_big_endian (abfd))
    disasm_info->display_endian = disasm_info->endian = BFD_ENDIAN_BIG;
  else if (bfd_little_endian (abfd))
    disasm_info->display_endian = disasm_info->endian = BFD_ENDIAN_LITTLE;

  disassemble_init_for_target(disasm_info);

  disasm_info->read_memory_func = my_read_memory;
}

void init_disasm_info(asm_program_t *prog)
{
  init_disasm_info(prog->abfd, &prog->disasm_info);
  prog->disasm_info.application_data = prog;
}


// FIXME: the memory that gets allocated here is never freed.
void
initialize_sections(bfd *abfd, asm_program_t *prog)
{
  struct disassemble_info *disasm_info = &prog->disasm_info;
  unsigned int opb = bfd_octets_per_byte(abfd);
  disasm_info->octets_per_byte = opb;
  init_disasm_info(prog);
  Segment *segs = NULL;

  for(asection *section = abfd->sections; 
      section != (asection *)  NULL; section = section->next){
    Segment *seg, *ts;

    int is_code = 0;
    bfd_byte *data = NULL;

    bfd_vma datasize = bfd_get_section_size_before_reloc(section);
    if(datasize == 0) continue;

    data = (bfd_byte *) xalloc((size_t) datasize, (size_t)
			       sizeof(bfd_byte));
    bfd_get_section_contents(abfd, section, data, 0, datasize);
    seg = (Segment *) xalloc(1, sizeof(Segment));
    seg->data = data;
    seg->datasize = datasize;
    seg->start_addr = section->vma;
    seg->end_addr = section->vma + datasize/opb;
    seg->section = section;
    if((section->flags & SEC_CODE) == 0)
      is_code = 0;
    else
      is_code = 1;
    seg->is_code = is_code;
    if(is_code){
      /*seg->status = (unsigned char *)
	xalloc((seg->end_addr - seg->start_addr), sizeof(char)); */
      /*
      seg->insts = (Instruction **) xalloc((seg->end_addr - 
					    seg->start_addr),
					   sizeof(Instruction *));
       //init_disasm_info(&disasm_info, seg);
      for(bfd_vma pc = seg->start_addr; pc < seg->end_addr; ++pc){
	Instruction *inst = get_inst_of(prog, pc, seg);
	// DJB: Hack for stmxcsr and ldmxcsr instrs as found
	// in atphttpd instr 806a36e
	//if(inst->opcode[0] == 0xae && inst->opcode[1] == 0xf)
	//  inst->length++;
	seg->insts[pc - seg->start_addr] = inst;
      }
      */
      seg->next = NULL;
      if(segs == NULL)
	segs = seg;
      else{
	ts = segs;
	while(ts->next != NULL)
	  ts = ts->next;
	ts->next = seg;
      }
    }
    prog->sections[seg->start_addr] = seg;
  }
  prog->segs = segs;
}


bfd *
initialize_bfd(const char *filename)
{
  bfd * abfd;
  char **matching;
  const char *target = "i686-pc-linux-gnu";

  bfd_init();

  if(!bfd_set_default_target(target))
    fatal("program::initialize", "couldn't set default bfd target");
  
  abfd = bfd_openr(filename, target);
  if(abfd == NULL)
    fatal("initialize_bfd", "cannot open %s", filename);

  if (bfd_check_format (abfd, bfd_archive))
      fatal("initalize_bfd", "archive files  not supported\n");

  if(!bfd_check_format_matches(abfd, bfd_object, &matching))
    fatal("initialize_bfd", "bfd_check_format_matches failed");
  return abfd;
}



vine_symbols_t * get_symbols_of_file(const char *filename)
{
    asymbol** syms = NULL;
    long symcount = 0;
    long sorted_symcount = 0;
    asymbol** dynsyms = NULL;
    long dynsymcount = 0;

    asymbol* synthsyms = NULL;
    long synthcount = 0;
    long storage;

    vector<vine_symbol_t *> *ret = new vector<vine_symbol_t *>();

//     asm_program_t *prog = new asm_program_t;
//     printf("initializing bfd\n\n");
    bfd *abfd = initialize_bfd(filename);
//     printf("done\n\n");
//     printf("initializing sections...\n\n");
//     initialize_sections(abfd, prog);
//     printf("done\n\n");
    asymbol *sym;

    if (bfd_get_file_flags (abfd) & HAS_SYMS) {
	storage = bfd_get_symtab_upper_bound (abfd);


	assert (storage >= 0);
	if (storage > 0)
	    syms = (asymbol**) xmalloc(storage);
	symcount = bfd_canonicalize_symtab (abfd, syms);
	assert (symcount >= 0);
	sorted_symcount = remove_useless_symbols(syms, symcount);
	qsort(syms, sorted_symcount, sizeof(asymbol *), compare_symbols);
	for(int i= 0; i < sorted_symcount; i++){
	  vine_symbol_t *temp = new vine_symbol_t;
	  temp->name = string(bfd_asymbol_name((syms[i])));

	  temp->addr = bfd_asymbol_value((syms[i]));
	  sym = syms[i];
	  temp->is_function = (sym->flags & BSF_FUNCTION) != 0;
	  temp->is_dynamic = 0;
	  ret->push_back(temp);
	}
    }

    storage = bfd_get_dynamic_symtab_upper_bound (abfd);

    if (storage > 0) {
	dynsyms = (asymbol**) xmalloc(storage);
	dynsymcount = bfd_canonicalize_dynamic_symtab (abfd, dynsyms);
    }


    synthcount = bfd_get_synthetic_symtab (abfd, symcount, syms,
					   dynsymcount, dynsyms, &synthsyms);
    if (synthcount < 0)
        synthcount = 0;

    for (int i=0; i<synthcount; i++) {
      vine_symbol_t *temp = new vine_symbol_t;
      temp->name = string(bfd_asymbol_name(&(synthsyms[i])));
      temp->addr = bfd_asymbol_value(&(synthsyms[i]));

      temp->is_function = (synthsyms[i].flags & BSF_FUNCTION) != 0;
      temp->is_dynamic = 1;
      ret->push_back(temp);
    }
    if (synthsyms)
	free(synthsyms);
    if (dynsyms)
	free(dynsyms);
    if (syms)
    free(syms);
    bfd_close(abfd);
    return ret;
}

int symbols_size(vine_symbols_t *ds)
{
  return ds->size();
}

vine_symbol_t *symbols_get(vine_symbols_t *ds, int i)
{
  return ds->at(i);
}

char *symbol_name(vine_symbol_t *d)
{
  const char *ptr = (d->name).c_str();
  char *ret = (char *) malloc((strlen(ptr)+1) * sizeof(char));
  strcpy(ret, ptr);
  return ret;
}

extern void destroy_symbols(vine_symbols_t *ds)
{
    for (vector<vine_symbol_t *>::iterator i = ds->begin();
	 i != ds->end(); i++) {
	delete(*i);
    }
    delete ds;
}



/**
 * Obtain the information of dynamic symbol (library call)
 */
dyn_functions_t *get_synthetic_symbols(const char *filename) {
    asymbol** syms = NULL;
    long symcount = 0;
    asymbol** dynsyms = NULL;
    long dynsymcount = 0;
    asymbol* synthsyms = NULL;
    long synthcount = 0;
    long storage;

    vector<dyn_function_t *> *ret = new vector<dyn_function_t *>();

    asm_program_t *prog = new asm_program_t;
    bfd *abfd = initialize_bfd(filename);
    initialize_sections(abfd, prog);
    prog->fake_asmp_bytes = 0;
    
    if (bfd_get_file_flags (abfd) & HAS_SYMS) {
	storage = bfd_get_symtab_upper_bound (abfd);
	assert (storage >= 0);
	if (storage > 0)
	    syms = (asymbol**) xmalloc(storage);
	symcount = bfd_canonicalize_symtab (abfd, syms);
	assert (symcount >= 0);
    }

    storage = bfd_get_dynamic_symtab_upper_bound (abfd);
    
    if (storage > 0) {
	dynsyms = (asymbol**) xmalloc(storage);
	dynsymcount = bfd_canonicalize_dynamic_symtab (abfd, dynsyms);
    }

    synthcount = bfd_get_synthetic_symtab (abfd, symcount, syms,
					   dynsymcount, dynsyms, &synthsyms);
    if (synthcount < 0)
        synthcount = 0;

    for (int i=0; i<synthcount; ++i) {
	dyn_function_t *temp = new dyn_function_t;
	temp->name = string(bfd_asymbol_name(&(synthsyms[i])));
	temp->addr = bfd_asymbol_value(&(synthsyms[i]));
	ret->push_back(temp);
    }
    if (synthsyms)
	free(synthsyms);
    if (dynsyms)
	free(dynsyms);
    if (syms)
    free(syms);
    bfd_close(abfd);
    // XXX do we need to free prog here? -SMcC
    return ret;
}

int dyn_functions_size(dyn_functions_t *ds) {
    return ds->size();
}

dyn_function_t * dyn_functions_get(dyn_functions_t *ds, int i) {
    return ds->at(i);
}

const char* dyn_functions_name(dyn_function_t *d) {
  const char *ptr = (d->name).c_str();
  char *ret = (char *) malloc((strlen(ptr)+1) * sizeof(char));
  strcpy(ret, ptr);
  return ret;
}

address_t dyn_functions_addr(dyn_function_t *d) {
    return d->addr;
}

void destroy_dyn_functions(dyn_functions_t *ds) {
    for (vector<dyn_function_t *>::iterator i = ds->begin();
	 i != ds->end(); i++) {
	delete(*i);
    }
    delete ds;
}

void destroy_memory_data(memory_data_t *md) {
    if (md) {
        for (vector<memory_cell_data_t*>::iterator j = md->begin();
             j != md->end(); j++) {
            free(*j);
        }
        delete md;
    }
}

address_t memory_cell_data_address(memory_cell_data_t *md) {
    return md->address;
}

int memory_cell_data_value(memory_cell_data_t *md) {
    return md->value;
}

int memory_data_size(memory_data_t *md) {
    return md->size();
}

// FIXME: Write memory_cell_data_t to a given pointer so we don't need to
// have it allocated
memory_cell_data_t * memory_data_get(memory_data_t *md, int i) {
    return md->at(i);
}

/**
 * Obtain data from the .bss section(s).
 * The section(s) are identified by having ALLOC flag but
 * not LOAD nor READONLY flags.
 * The return value is in the form [(address, value),...,...].
 * Note that the value elements will always be zeros.
 * FIXME: Use a reasonable data structure.
 */
memory_data_t *
get_bssdata(asm_program_t *prog) {
    vector<memory_cell_data_t *> *bssdata = new vector<memory_cell_data_t *>();

    bfd *abfd = prog->abfd;

    unsigned int opb = bfd_octets_per_byte(abfd);
    assert(opb == 1);

    for(asection *section = abfd->sections;
        section != (asection *)  NULL; section = section->next){

        if(!(!(section->flags & SEC_READONLY) &&
             (section->flags & SEC_ALLOC) &&
             !(section->flags & SEC_LOAD))
            ) continue;
        bfd_vma datasize = bfd_get_section_size_before_reloc(section);
        if(datasize == 0) continue;
        bfd_vma start_addr = section->vma;
        bfd_vma end_addr = start_addr + datasize/opb;
        for (bfd_vma itr = start_addr; itr < end_addr; itr++) {
            memory_cell_data_t *mcd = (memory_cell_data_t *)
                xalloc((size_t) 1, (size_t) sizeof(memory_cell_data_t));
            mcd->address = itr;
            mcd->value = 0;
            bssdata->push_back(mcd);
        }
    }
    return bssdata;
}

/**
 * Obtain data from the section with readonly flags.
 * I.e. ALLOC, READONLY, and LOAD flags are set.
 * FIXME: Use a reasonable data structure.
 */
memory_data_t *
get_rodata(asm_program_t *prog) {
    vector<memory_cell_data_t *> *rodata = new vector<memory_cell_data_t *>();

    bfd *abfd = prog->abfd;

    unsigned int opb = bfd_octets_per_byte(abfd);
    assert(opb == 1);

    for(asection *section = abfd->sections;
        section != (asection *)  NULL; section = section->next){

        if(!((section->flags & SEC_READONLY) &&
             (section->flags & SEC_ALLOC) &&
             (section->flags & SEC_LOAD))
            ) continue;
        bfd_byte *data = NULL;
        bfd_vma datasize = bfd_get_section_size_before_reloc(section);
        if(datasize == 0) continue;
        data = (bfd_byte *) xalloc((size_t) datasize, (size_t)
                                   sizeof(bfd_byte));
        bfd_get_section_contents(abfd, section, data, 0, datasize);
        bfd_vma start_addr = section->vma;
        bfd_vma end_addr = start_addr + datasize/opb;
        for (bfd_vma itr = start_addr; itr < end_addr; itr++) {
            memory_cell_data_t *mcd = (memory_cell_data_t *)
                xalloc((size_t) 1, (size_t) sizeof(memory_cell_data_t));
            mcd->address = itr;
            mcd->value = data[itr-start_addr];
            rodata->push_back(mcd);
        }
        free(data);
    }
    /* FIXMEO: close the BFD */
    return rodata;
}


struct bprintf_buffer {
  char *str; // the start of the string
  char *end; // the null terminator at the end of the written string.
  size_t size; // the size of str
};

int bprintf(struct bprintf_buffer *dest, const char *fmt, ...) {
  va_list ap;
  size_t ret;
  size_t size_left = dest->size - (dest->end - dest->str);
  va_start(ap, fmt);
  ret = vsnprintf(dest->end, size_left, fmt, ap);
  va_end(ap);
  if (ret >= size_left) {
    // we seem to need to call va_start again... is this legal?
    dest->size = dest->size+ret+1-size_left;
    char *str = (char*)realloc(dest->str, dest->size);

    assert(str); // this code is full of xalloc anyways...

    dest->end = str + (dest->end - dest->str);
    dest->str = str;
    size_left = dest->size - (dest->end - dest->str);
    va_start(ap, fmt);
    ret = vsnprintf(dest->end, size_left, fmt, ap);
    va_end(ap);
    assert(ret == size_left-1 && ret > 0);
  }
  dest->end += ret;
  return (int)ret;
}

static const char *disasm_options_thumb = "force-thumb";
static const char *disasm_options_nonthumb = "no-force-thumb";

// returns a string which is valid until the next call to this function
char* string_of_insn(asm_program_t *prog, Instruction *inst)
{
  static struct bprintf_buffer bits = {NULL, NULL, 0};

  disassembler_ftype disas = disassembler(prog->abfd);
  fprintf_ftype old_fprintf_func = prog->disasm_info.fprintf_func;
  void *oldstream = prog->disasm_info.stream;
  char *old_opts = prog->disasm_info.disassembler_options;
  prog->disasm_info.fprintf_func = (fprintf_ftype)bprintf;
  prog->disasm_info.stream = &bits;

  bool change_opts = (bfd_get_arch(prog->abfd) == bfd_arch_arm);
  if (change_opts) {
    if (inst->arch_flags == 1)
      prog->disasm_info.disassembler_options = (char *)disasm_options_thumb;
    else
      prog->disasm_info.disassembler_options = (char *)disasm_options_nonthumb;
  }

  bits.end = bits.str;
  disas(inst->address, &prog->disasm_info);

  prog->disasm_info.fprintf_func = old_fprintf_func;
  prog->disasm_info.stream = oldstream;
  
  if (change_opts) {
    prog->disasm_info.disassembler_options = old_opts;
  }

  return bits.str;
}

void ostream_insn(asm_program_t *prog, Instruction *inst, ostream &out)
{
  out << string_of_insn(prog, inst) <<endl;
}

/* used to be in disasm.cpp */
static void *xalloc(size_t num, size_t size)
{
  void *chunk = calloc(num, size);
  if (chunk == NULL) {
    printf("memory allocation failed\n");
    exit(1);
  }
  return chunk;
}

static void *xmalloc(size_t size) {
  void *chunk = malloc(size);
  if (chunk == NULL) {
    printf("memory allocation failed\n");
    exit(1);
  }
  return chunk;
}

extern "C" {

  enum asmir_arch asmprogram_arch(asm_program_t *prog) {
    return prog->asmir_arch;
  }

}

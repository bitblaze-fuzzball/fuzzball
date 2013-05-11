//======================================================================
//
// This file contains a test of the VEX IR translation interface.
//
//======================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <set>

#include "asm_program.h"

#include "ir_printer.h"

extern "C" 
{
#include "libvex.h"
}

#include "irtoir.h"

using namespace std;

//======================================================================
//
// Miscellaneous functions
//
//======================================================================

void usage( char *progname )
{
    printf("Usage: %s filename [option]\n", progname);
    printf("Options:\n");
    printf("\t0 - Print only assembly\n");
    printf("\t1 - Print only VEX IR\n");
    printf("\t2 - Print only Vine IR [Default]\n");
    printf("\t3 - Print Vine IR as one long unbroken sequence of instructions\n");
}

// class declvis : public DefaultIRVisitor {
// public:
//   declvis() { };
//   void compute(vector<Stmt *> *stmts) { 
//     for(vector<Stmt *>::const_iterator it = stmts->begin();
// 	it != stmts->end(); it++){
//       Stmt *s = *it;
//       s->accept(this);
//     }
//   }
//   void visitTemp(Temp *t){
//     if(t->name[0] != 'T') return;
//     if(decls.find(t->name) != decls.end()){
//       reg_t typ1 = decls.find(t->name)->second;
//       assert(t->typ == typ1);
//     } else {
//       decls.insert(pair<string, reg_t>(t->name, t->typ));
//     }

//   }
//   map<string, reg_t> decls;
// };


//----------------------------------------------------------------------
// Print all the instructions of this program, separating them by
// function and BB
//----------------------------------------------------------------------
void print_prog_insns( asm_program_t *prog )
{
    /* int bb_count = 0; */

    for ( map<address_t, asm_function_t *>::const_iterator i = prog->functions.begin();
          i != prog->functions.end(); i++ )
    {
        asm_function_t *func = i->second;

        assert(func != NULL);

        cout << "========================== Function " << func->name << " start ============================" << endl;

	/*
        for ( set<asm_bb_t *>::const_iterator j = func->blocks.begin();
              j != func->blocks.end(); j++ )
        {
            asm_bb_t *block = *j;

            assert(block != NULL);

            int insns_count = count_bb_insns(block, func);

            if ( insns_count == 0 )
                continue;

            bb_count++;
            cout << "------------------------------ BB " << bb_count << " start ------------------------------" << endl;
            cout << "# of instructions: " << insns_count << endl;
            print_bb_insns(block, func);
            cout << "------------------------------ BB " << bb_count << " end --------------------------------" << endl;

        }
	*/
	Instruction *inst = NULL;
	for (address_t addr = func->start_addr; addr < func->end_addr; addr += inst->length) {
	  inst = func->instmap[addr];
	  if (!inst) {
	    cout << "No instruction for " << addr << ", stoping.\n";
	    break;
	  }
	  ostream_insn(prog, inst, cout);
	}

        cout << "========================== Function " << func->name << " end ==============================" << endl;
    }
}


void print_vex_ir(asm_program_t *prog, vector<vine_block_t *> vblocks )
{

    unsigned int i;

    for ( i = 0; i < vblocks.size(); i++ )
    {
        vine_block_t *block = vblocks.at(i);
        assert(block);

        cout << endl << "VEX Block " << i << endl;

        cout << "   ";
        ostream_insn(prog, block->inst, cout);
        cout << endl;

        if ( block->vex_ir != NULL )
            ppIRSB(block->vex_ir);
        else
            cout << "   Unhandled by VEX" << endl;

    }
}

void print_merged_ir( vector<Stmt *> ir )
{
    unsigned int i;

    for ( i = 0; i < ir.size(); i++ )
    {
        Stmt *stmt = ir.at(i);
        assert(stmt);

        cout << stmt->ir_address << "\t\t" << stmt->tostring() << endl;
    }
}

void print_prog_ir(asm_program_t *prog)
{
  int globals_printed = 0;
  translate_init();

  for(map<address_t, asm_function_t *>::const_iterator it =
	prog->functions.begin(); it != prog->functions.end(); it++){
    asm_function_t *f = it->second;

    vector<Instruction *> instrs;
    for(map<address_t, Instruction *>::const_iterator i2 = 
	  f->instmap.begin(); i2 != f->instmap.end(); i2++){
      instrs.push_back(i2->second);
    }

    vector<vine_block_t *> vine_blocks = generate_vex_ir(prog, instrs);    
    vine_blocks = generate_vine_ir(prog, vine_blocks);

    if (!globals_printed) {
      // We need to do this after some translation so that it prints the
      // globals for the right arch.
      print_globals();
      globals_printed = 1;
    }

    cout << "label L_" << f->name << ":" << endl;
    print_vine_ir(prog, vine_blocks);
  }
}

//======================================================================
//
// Main
//
//======================================================================

int main(int argc, char *argv[])
{
  debug_on("warning");
    //
    // Check args
    //
    if ( argc < 2 )
    {
        usage(argv[0]);
        return -1;
    }

    char print_option = '2';
    if ( argc > 2 )
    {
        print_option = *(argv[2]);
    }

    vector<vine_block_t *> vine_blocks;

    //
    // Disassemble the program 
    //
    cerr << "Disassembling binary." << endl;
    asm_program_t *prog = disassemble_program(argv[1]);
    assert(prog);


    if(print_option == '2'){
      print_prog_ir(prog);
      return 0;
    }

    //
    // Translate asm to VEX IR
    //
    cerr << "Translating asm to VEX IR." << endl;
    if ( print_option > '0' )
    {
        vine_blocks = generate_vex_ir(prog);
    }

    //
    // Translate VEX IR to Vine IR
    //
    cerr << "Translating VEX IR to Vine IR." << endl;
    if ( print_option > '1' )
    {
      vine_blocks = generate_vine_ir(prog, vine_blocks);
    }

    cerr << "Printing output:" << endl;
    // 
    // Print all the instructions in the disassembled program
    //
    if ( print_option == '0' )
        print_prog_insns(prog);

    //
    // Print the VEX IR output
    //
    else if ( print_option == '1' )
      print_vex_ir(prog, vine_blocks);

    //
    // Print the Vine IR output
    //
    else if ( print_option == '2' )
      print_vine_ir(prog, vine_blocks);

    //
    // Print the merged IR output
    //
    else if ( print_option == '3' )
        print_merged_ir( merge_ir(vine_blocks) );

    //
    // Free the translated block after we're done using it, otherwise,
    // we run the risk of running out of memory for subsequent blocks
    // since memory for translations is static and have a fixed limit.
    //
//    vx_FreeAll();   // Frees mem allocated inside generate_vex_ir()


    return 0;
}



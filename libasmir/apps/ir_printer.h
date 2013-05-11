#ifndef _IR_PRINTER_H
#define _IR_PRINTER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <set>

#include "asm_program.h"

extern "C" 
{
#include "libvex.h"
}

#include "irtoir.h"

using namespace std;

// Uncomment to enable typechecking.
//#define NOTYPECHECKING
void print_globals();
void print_vine_ir(asm_program_t *prog, vector<vine_block_t *> vblocks );
#endif

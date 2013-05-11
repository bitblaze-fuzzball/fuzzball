// objdump.cpp contains functions ripped off from objdump.c from
// the binutils distribution. 

#ifndef _OBJDUMP_H
#define _OBJDUMP_H
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern "C" {
#include "config.h"
#include <bfd.h>
}

int compare_symbols(const void *ap, const void *bp);
long
remove_useless_symbols (asymbol **symbols, long count);

#endif

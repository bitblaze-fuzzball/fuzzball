#ifndef _DEBUG_H
#define _DEBUG_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

void debug_on(const char *key);
bool is_debug_on(const char *key);
void _print_debug(const char *key, const char *fmt, ...);

#ifndef NODEBUG
#define print_debug(args...) _print_debug(args) 
#else 
#define print_debug(args...) while(0)
#endif

void fatal(const char *funcName, const char *fmt, ...);
#endif

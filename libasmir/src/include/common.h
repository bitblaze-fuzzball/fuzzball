#ifndef _COMMON_H
#define _COMMON_H
#include <stdint.h>

#ifdef __cplusplus
#include "debug.h"
extern "C" {
#endif

#include "config.h"
#include <bfd.h>

#define DEFAULT_IRMAPFILE  "./asm2re.map"
#define DEFAULT_TRACEFILE "proctrace.out"
#define DEFAULT_UNROLL 2
#define LIBASMIR_VERSION "0.1"
typedef bfd_vma address_t;


#ifdef __cplusplus
}
#endif

#endif

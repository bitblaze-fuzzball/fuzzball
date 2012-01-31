/*---------------------------------------------------------------------------+
 |  fpu_system.h                                                             |
 |                                                                           |
 | Copyright (C) 1992,1994,1997                                              |
 |                       W. Metzenthen, 22 Parker St, Ormond, Vic 3163,      |
 |                       Australia.  E-mail   billm@suburbia.net             |
 |                                                                           |
 +---------------------------------------------------------------------------*/

#ifndef _FPU_SYSTEM_H
#define _FPU_SYSTEM_H

/* system dependent definitions */

#ifdef KERNEL
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#endif

/* s is always from a cpu register, and the cpu does bounds checking
 * during register load --> no further bounds checks needed */
#define LDT_DESCRIPTOR(s)	(((struct desc_struct *)current->mm->context.ldt)[(s) >> 3])
#define SEG_D_SIZE(x)		((x).b & (3 << 21))
#define SEG_G_BIT(x)		((x).b & (1 << 23))
#define SEG_GRANULARITY(x)	(((x).b & (1 << 23)) ? 4096 : 1)
#define SEG_286_MODE(x)		((x).b & ( 0xff000000 | 0xf0000 | (1 << 23)))
#define SEG_BASE_ADDR(s)	(((s).b & 0xff000000) \
				 | (((s).b & 0xff) << 16) | ((s).a >> 16))
#define SEG_LIMIT(s)		(((s).b & 0xff0000) | ((s).a & 0xffff))
#define SEG_EXECUTE_ONLY(s)	(((s).b & ((1 << 11) | (1 << 9))) == (1 << 11))
#define SEG_WRITE_PERM(s)	(((s).b & ((1 << 11) | (1 << 9))) == (1 << 9))
#define SEG_EXPAND_DOWN(s)	(((s).b & ((1 << 11) | (1 << 10))) \
				 == (1 << 10))
#ifdef KERNEL
#define I387			(current->thread.fpu.state)
#else

#define X86_EFLAGS_CF   0x00000001 /* Carry Flag */
#define X86_EFLAGS_PF   0x00000004 /* Parity Flag */
#define X86_EFLAGS_AF   0x00000010 /* Auxiliary carry Flag */
#define X86_EFLAGS_ZF   0x00000040 /* Zero Flag */
#define X86_EFLAGS_SF   0x00000080 /* Sign Flag */
#define X86_EFLAGS_TF   0x00000100 /* Trap Flag */
#define X86_EFLAGS_IF   0x00000200 /* Interrupt Flag */
#define X86_EFLAGS_DF   0x00000400 /* Direction Flag */
#define X86_EFLAGS_OF   0x00000800 /* Overflow Flag */
#define X86_EFLAGS_IOPL 0x00003000 /* IOPL mask */
#define X86_EFLAGS_NT   0x00004000 /* Nested Task */
#define X86_EFLAGS_RF   0x00010000 /* Resume Flag */
#define X86_EFLAGS_VM   0x00020000 /* Virtual Mode */
#define X86_EFLAGS_AC   0x00040000 /* Alignment Check */
#define X86_EFLAGS_VIF  0x00080000 /* Virtual Interrupt Flag */
#define X86_EFLAGS_VIP  0x00100000 /* Virtual Interrupt Pending */
#define X86_EFLAGS_ID   0x00200000 /* CPUID detect */

#ifdef __ASSEMBLY__
#else
#define get_user(x, ptr) ((x) = (*(ptr)))
#define put_user(x, ptr) (*(ptr) = (x))
#define __copy_to_user(d,s,n)   (memmove((d),(s),(n)),0)
#define __copy_from_user(d,s,n) (memmove((d),(s),(n)),0)
#define copy_to_user(d,s,n)     (memmove((d),(s),(n)),0)
#define copy_from_user(d,s,n)   (memmove((d),(s),(n)),0)

struct pt_regs {
    unsigned long bx;      /*  0 */
    unsigned long cx;      /*  4 */
    unsigned long dx;      /*  8 */
    unsigned long si;      /*  c */
    unsigned long di;      /* 10 */
    unsigned long bp;      /* 14 */
    unsigned long ax;      /* 18 */
    unsigned long ds;      /* 1c */
    unsigned long es;      /* 20 */
    unsigned long fs;      /* 24 */
    unsigned long gs;      /* 28 */
    unsigned long orig_ax; /* 2c */
    unsigned long ip;      /* 30 */
    unsigned long cs;      /* 34 */
    unsigned long flags;   /* 38 */
    unsigned long sp;      /* 3c */
    unsigned long ss;      /* 40 */
};

struct kernel_vm86_regs {
/*
 * normal regs, with special meaning for the segment descriptors..
 */
        struct pt_regs pt;
/*
 * these are specific to v86 mode:
 */
        unsigned short es, __esh;
        unsigned short ds, __dsh;
        unsigned short fs, __fsh;
        unsigned short gs, __gsh;
};

struct math_emu_info {
        long ___orig_eip;
        union {
                struct pt_regs *regs;
	        struct kernel_vm86_regs *vm86;
        };
};

typedef unsigned char u8;
typedef unsigned int u32;

struct i387_soft_struct {
        u32                     cwd;
        u32                     swd;
        u32                     twd;
        u32                     fip;
        u32                     fcs;
        u32                     foo;
        u32                     fos;
        /* 8*10 bytes for each FP-reg = 80 bytes: */
        u32                     st_space[20];
        u8                      ftop;
        u8                      changed;
        u8                      lookahead;
        u8                      no_update;
        u8                      rm;
        u8                      alimit;
        struct math_emu_info    *info;
        u32                     entry_eip;
};

union thread_xstate {
    struct i387_soft_struct soft;
};

extern union thread_xstate i387_state;
#endif
#define I387                    (&i387_state)
#endif
#define FPU_info		(I387->soft.info)

#define FPU_CS			(*(unsigned short *) &(FPU_info->regs->cs))
#define FPU_SS			(*(unsigned short *) &(FPU_info->regs->ss))
#define FPU_DS			(*(unsigned short *) &(FPU_info->regs->ds))
#define FPU_EAX			(FPU_info->regs->ax)
#define FPU_EFLAGS		(FPU_info->regs->flags)
#define FPU_EIP			(FPU_info->regs->ip)
#define FPU_ORIG_EIP		(FPU_info->___orig_eip)

#define FPU_lookahead           (I387->soft.lookahead)

/* nz if ip_offset and cs_selector are not to be set for the current
   instruction. */
#define no_ip_update		(*(u_char *)&(I387->soft.no_update))
#define FPU_rm			(*(u_char *)&(I387->soft.rm))

/* Number of bytes of data which can be legally accessed by the current
   instruction. This only needs to hold a number <= 108, so a byte will do. */
#define access_limit		(*(u_char *)&(I387->soft.alimit))

#define partial_status		(I387->soft.swd)
#define control_word		(I387->soft.cwd)
#define fpu_tag_word		(I387->soft.twd)
#define registers		(I387->soft.st_space)
#define top			(I387->soft.ftop)

#define instruction_address	(*(struct address *)&I387->soft.fip)
#define operand_address		(*(struct address *)&I387->soft.foo)

#ifdef KERNEL
#define FPU_access_ok(x,y,z)	if ( !access_ok(x,y,z) ) \
				math_abort(FPU_info,SIGSEGV)
#else
#define FPU_access_ok(x,y,z)    /* always OK */
#endif
#define FPU_abort		math_abort(FPU_info, SIGSEGV)

#define FPU_IGNORE_CODE_SEGV
#ifdef FPU_IGNORE_CODE_SEGV
/* access_ok() is very expensive, and causes the emulator to run
   about 20% slower if applied to the code. Anyway, errors due to bad
   code addresses should be much rarer than errors due to bad data
   addresses. */
#define	FPU_code_access_ok(z)
#else
/* A simpler test than access_ok() can probably be done for
   FPU_code_access_ok() because the only possible error is to step
   past the upper boundary of a legal code area. */
#define	FPU_code_access_ok(z) FPU_access_ok(VERIFY_READ,(void __user *)FPU_EIP,z)
#endif

#define FPU_get_user(x,y)       get_user((x),(y))
#define FPU_put_user(x,y)       put_user((x),(y))

#endif

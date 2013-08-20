#
#               Makefile for wm-FPU-emu
#

#DEBUG	= -DDEBUGGING
DEBUG	=
PARANOID = -DPARANOID
EXTRA_CFLAGS	:= $(PARANOID) $(DEBUG) -fno-builtin $(MATH_EMULATION)
EXTRA_AFLAGS	:= $(PARANOID)
CC := gcc -m32
CFLAGS := $(PARANOID) $(DEBUG) -O3 -fno-strict-aliasing -g -Wall

# From 'C' language sources:
C_OBJS =fpu_entry.o errors.o \
	fpu_arith.o fpu_aux.o fpu_etc.o fpu_tags.o fpu_trig.o \
	load_store.o get_address.o \
	poly_atan.o poly_l2.o poly_2xm1.o poly_sin.o poly_tan.o \
	reg_add_sub.o reg_compare.o reg_constant.o reg_convert.o \
	reg_ld_str.o reg_divide.o reg_mul.o

# From 80x86 assembler sources:
A_OBJS =reg_u_add.o reg_u_div.o reg_u_mul.o reg_u_sub.o \
	div_small.o reg_norm.o reg_round.o \
	wm_shrx.o wm_sqrt.o \
	div_Xsig.o polynom_Xsig.o round_Xsig.o \
	shr_Xsig.o mul_Xsig.o \
	inline_call.o

obj-y =$(C_OBJS) $(A_OBJS)

# If your linker doesn't recognize "-Ttext-segment", try replacing it with
# just "-Ttext". "-Ttext-segment" has the right behavior in all GNU-ish
# linkers that support it, including old LD and modern GOLD. All versions
# of GOLD support "-Ttext" with the right behavior, but for old LD
# "-Ttext" has a different behavior.
# We could try to figure this out automatically, but I don't know if it's
# worth the effort: "-Ttext-segment" sounds like it will be the standard
# going forward. Right now this Makefile isn't hooked up with a configure
# script; if it is in the future, Valgrind has a similar check we could
# borrow. For now doing the LD version determination here seems a bit
# overkill.
math-emu.so: $(obj-y)
	$(CC) -Wl,--entry,inline_x87 -Wl,-Ttext-segment,0xc1048000 $(obj-y) -nostdlib -o $@

tester: tester.o $(obj-y)

tester.o: tester.S

%.o: %.S
	$(CC) -D__ASSEMBLY__ $< -c -o $@

clean:
	rm -f *.o tester

#proto:
#	cproto -e -DMAKING_PROTO *.c >fpu_proto.h

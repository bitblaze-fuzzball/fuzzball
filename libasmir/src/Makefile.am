RANLIB = @RANLIB@
lib_LIBRARIES = libasmir.a
#lib_LTLIBRARIES = libasmir.la
AM_CFLAGS = -I@VEX_DIR@/pub -I./include -fPIC -Wall # -Werror
AM_CXXFLAGS = $(AM_CFLAGS)


libasmir_a_SOURCES = \
	./util/debug.cpp \
	./disasm/objdump.cpp \
	./disasm/asm_program.cpp \
	./vex/vexmem.cpp \
	./vex/irtoir.cpp \
	./vex/irtoir-i386.cpp \
	./vex/irtoir-x64.cpp \
	./vex/irtoir-arm.cpp \
	./vex/irtoir_c.cpp \
	./vex/vexir.cpp \
	./vex/gen_thunks.cpp \
	./ir/stmt.cpp \
	./ir/stmt_c.cpp \
	./ir/exp.cpp \
	./ir/exp_c.cpp \
	./ir/translate.cpp

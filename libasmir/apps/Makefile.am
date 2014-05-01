bin_PROGRAMS = \
	disassembler \
	print-ir \
	insn_test

disassembler_SOURCES = disassembler.cpp
print_ir_SOURCES = print-ir.cpp ir_printer.cpp
insn_test_SOURCES = insn_test.cpp
LDADD =  -lasmir -lvex -lbfd -lopcodes -liberty -lz
AM_LDFLAGS = -L../src -L@VEX_DIR@
AM_CXXFLAGS = -I../src/include  -I@VEX_DIR@/pub -Wall # -Werror

# these executables should depend on libasmir, since they link it in
# statically.
$(bin_PROGRAMS): ../src/libasmir.a

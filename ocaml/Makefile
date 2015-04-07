export OCAMLMAKEFILE=OCamlMakefile
export LIBASMIR=../libasmir
include $(LIBASMIR)/makefile.inc
export INCDIRS += ../stp ../stp/ocaml
export EXTLIBDIRS += ../stp ../stp/ocaml
export LIBS += stpvc

# Include sources from ../stp/ocaml in the automatically generated
# dependencies.
export OCAMLDEP = ocamldep -I ../stp/ocaml

# Utility modules used throughout Vine
UTIL_SOURCES = \
	debug.mli debug.ml \
	vine_util.mli vine_util.ml

# The definition of the Vine IR
IR_SOURCES = vine.mli vine.ml

BASE_SOURCES = $(UTIL_SOURCES) $(IR_SOURCES)

# Core Vine modules:

# The Vine IR type checker
TYPECHECK_SOURCES = vine_typecheck.mli vine_typecheck.ml

# The Vine IR expression simplifier
OPT_SOURCES = vine_opt.ml

CORE_SOURCES = $(TYPECHECK_SOURCES) $(OPT_SOURCES)

# Non-core modules, which depend only on core modules:

# SMTLIB2 interface
SMTLIB_SOURCES = smt_lib2.ml

# The text-based STP interface
STP_SOURCES = stp.ml

# The library-based STP interface
STPVC_SOURCES = vine_stpvc.mli vine_stpvc.ml

# The LibASMIR interface (reading IR from VEX)
ASMIR_SOURCES = \
	libasmir.idl \
	asmir.mli asmir.ml

# The Vine IR parser
PARSER_SOURCES = \
	vine_absyn.mli vine_absyn.ml \
	vine_grammar.mly \
	vine_lexer.mll \
	vine_parser.mli vine_parser.ml

OTHER_SOURCES = $(STP_SOURCES) $(STPVC_SOURCES) $(ASMIR_SOURCES) \
		$(SMTLIB_SOURCES) $(PARSER_SOURCES)

export SOURCES = $(BASE_SOURCES) $(CORE_SOURCES) $(OTHER_SOURCES)

RESULT = vine
# DOC_FILES=$(SOURCES)


# ocamlfind packages required
export PACKS = str extlib unix
export YFLAGS = -v


export CLIBS = asmir vex opcodes bfd iberty z stdc++ 
export CFLAGS = -g
export LDFLAGS = $(LIBASMIR_LDFLAGS) 
export CLIBFLAGS= $(LDFLAGS)
export LIBDIRS = $(LIBASMIR)/src # ../appreplay ../stp ../stp/ocaml

OCAMLFLAGS = -warn-error Aelz-3  #-w A
# merge everything but descriptions.
OCAMLDOCFLAGS = -m avlsoper

STATIC=1    # Don't bother building an .so library

all: dncl

bytecode: dcl top

vine_lib_doc: all htdoc


# You may also tell "make" at the command-line what kind of target to
# produce (e.g. "make nc").  Here all the possibilities with shortcuts
# between parenthesis:
# 
#    * byte-code                     (bc)
#    * byte-code-nolink              (bcnl)   - no linking stage
#    * byte-code-library             (bcl)
#    * native-code                   (nc)
#    * native-code-nolink            (ncnl)   - no linking stage
#    * native-code-library           (ncl)
#    * debug-code                    (dc)
#    * debug-code-nolink             (dcnl)   - no linking stage
#    * debug-code-library            (dcl)
#    * profiling-byte-code           (pbc)
#    * profiling-byte-code-library   (pbcl)
#    * profiling-native-code         (pnc)
#    * profiling-native-code-library (pncl)
#    * byte-code-dll                 (bcd)
#    * native-code-dll               (ncd)
#    * pack-byte-code              (pabc)
#    * pack-native-code            (panc)
#    * toplevel interpreter          (top)
#    * subprjs



include $(OCAMLMAKEFILE)

export OCAMLMAKEFILE=OCamlMakefile

SOURCES=libstp_regerrorhandler.c libstp.idl stpvc.ml stpvc.mli 
RESULT=stpvc
STATIC=1    # Don't bother building an .so library

LIBDIRS = ..
CLIBS = stp stdc++

OCAMLFLAGS := -warn-error Aelz

ifdef DEBUG
CFLAGS += -g
endif

ifdef PROFILE
CFLAGS += -p -pg
OCAMLFLAGS += -p
OCAMLLDFLAGS += -p
endif
export OCAMLFLAGS

all: dncl

nodebug: ncl

bytecode: dcl top

include $(OCAMLMAKEFILE)

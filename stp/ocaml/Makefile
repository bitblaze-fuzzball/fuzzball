export OCAMLMAKEFILE=OCamlMakefile

SOURCES=libstp_regerrorhandler.c libstp.idl stpvc.ml stpvc.mli 
RESULT=stpvc
STATIC=1    # Don't bother building an .so library

LIBDIRS = ..
CLIBS = stp stdc++

all: dncl

bytecode: dcl top

include $(OCAMLMAKEFILE)

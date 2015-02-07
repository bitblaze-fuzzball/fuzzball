export OCAMLMAKEFILE=../ocaml/OCamlMakefile

export SOURCES = \
	int64Array.ml \
        interval_tree.ml \
	build_assoc.ml \

RESULT = structs

export PACKS =
export YFLAGS = -v
export LIBS =
export INCDIRS =
export OCAMLDEP =

OCAMLFLAGS := -warn-error Aelz
ifdef PROFILE
CFLAGS += -p -pg
OCAMLFLAGS += -p
OCAMLLDFLAGS += -p
LDFLAGS += -p -pg
endif
export OCAMLFLAGS

all: ncl

debug: dncl

bytecode: dcl top

include $(OCAMLMAKEFILE)
# Copyright (C) BitBlaze, 2009-2011, and copyright (C) 2010 Ensighta
# Security Inc.  All rights reserved.

export OCAMLMAKEFILE=../ocaml/OCamlMakefile
export VINE_OCAMLDIR=../ocaml
export VINE_TRACEDIR=../trace
export STP_OCAMLDIR=../stp/ocaml
export LIBASMIR=../libasmir
include $(LIBASMIR)/makefile.inc
export INCDIRS += ../stp ../stp/ocaml $(VINE_OCAMLDIR) $(VINE_TRACEDIR) ../logging
export EXTLIBDIRS += ../stp ../stp/ocaml
export LIBS += stpvc ../ocaml/vine ../trace/trace

export OCAMLDEP = ocamldep -I $(VINE_OCAMLDIR) -I $(STP_OCAMLDIR) \
				-I $(VINE_TRACEDIR) -I ../logging

# Source files. In a dependency-respecting order (OCaml doesn't allow
# circular dependencies).
export SOURCES = \
	exec_domain.mli \
	tagged_domain.mli tagged_domain.ml \
	exec_exceptions.mli exec_exceptions.ml \
	exec_utils.mli exec_utils.ml \
	concrete_domain.mli concrete_domain.ml \
	exec_options.mli exec_options.ml \
	frag_simplify.mli frag_simplify.ml \
	frag_marshal.mli frag_marshal.ml \
	formula_manager.mli formula_manager.ml \
	symbolic_domain.mli symbolic_domain.ml \
	query_engine.mli query_engine.ml \
	stpvc_engine.mli stpvc_engine.ml \
	stp_external_engine.mli stp_external_engine.ml \
	pointer_management.mli pointer_management.ml \
	concrete_memory.mli concrete_memory.ml \
	granular_memory.mli granular_memory.ml \
	fragment_machine.mli fragment_machine.ml \
	decision_tree.mli decision_tree.ml \
	linear_decision_tree.mli linear_decision_tree.ml \
	binary_decision_tree.mli binary_decision_tree.ml \
	exec_no_influence.mli exec_no_influence.ml \
	sym_path_frag_machine.mli sym_path_frag_machine.ml \
	sym_region_frag_machine.mli sym_region_frag_machine.ml \
	special_handlers.mli special_handlers.ml \
	cgc_syscalls.mli cgc_syscalls.ml \
	linux_syscalls.mli linux_syscalls.ml \
	linux_loader.mli linux_loader.ml \
	decree_loader.mli decree_loader.ml \
	state_loader.mli state_loader.ml \
	exec_run_common.mli exec_run_common.ml \
	exec_veritesting_general_search_components.ml \
	exec_veritesting_breadth_first_search.ml \
	exec_veritesting_depth_first_search.ml \
	exec_veritesting_special_structures.ml \
	exec_encode_veritesting_region.ml \
	exec_veritesting.ml \
	exec_runloop.mli exec_runloop.ml \
	exec_stats.mli exec_stats.ml \
	exec_fuzzloop.mli exec_fuzzloop.ml \
	options_solver.mli options_solver.ml \
	exec_influence.mli exec_influence.ml \
	options_linux.mli options_linux.ml \
	git_version.ml \
	svn_version.ml \
	exec_set_options.mli exec_set_options.ml \
	exec_options_table.mli exec_options_table.ml

RESULT = execution
# DOC_FILES=$(SOURCES)

# ocamlfind packages required
export PACKS = str extlib unix yojson
export YFLAGS = -v

export CLIBS = asmir vex opcodes bfd iberty stdc++ 

CFLAGS := 
ifdef DEBUG
CFLAGS += -g
endif
ifdef PROFILE
CFLAGS += -p
endif

export CFLAGS
export LDFLAGS = -p $(LIBASMIR_LDFLAGS) 
export CLIBFLAGS= -p $(LDFLAGS)
export LIBDIRS = $(LIBASMIR)/src # ../appreplay ../stp ../stp/ocaml

OCAMLFLAGS := -warn-error Aelz
ifdef PROFILE
OCAMLFLAGS += -p
endif
export OCAMLFLAGS

# merge everything but descriptions.
OCAMLDOCFLAGS = -m avlsoper

# Fast enough that it's better to just always remake it than
# worry about whether it's up to date.
UNUSED := $(shell perl update-git-version.pl >/dev/tty)
UNUSED := $(shell perl update-svn-version.pl >/dev/tty)

all: dncl

bytecode: dcl top

vine_lib_doc: all htdoc

include $(OCAMLMAKEFILE)

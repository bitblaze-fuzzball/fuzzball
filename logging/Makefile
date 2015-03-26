export OCAMLMAKEFILE=../ocaml/OCamlMakefile

# This is a terrible thing to do.  POV XML logger is non standard in
# every way, but I do want the channel parsing to be added to the
# argument list withhout a lot of fuss, and this definitely
# accomplishes that.  Refactor later. [JTT 10/28]
export SOURCES = \
	logger_config.ml \
	text_logger.ml \
	yojson_logger.ml \
	yojson_list_logger.ml \
	log_configs.ml \
	pov_xml.ml \
	loggers.ml \
	indirect_target_logger.ml indirect_target_logger.mli \

RESULT = logging

export PACKS = unix extlib yojson tyxml
export YFLAGS = -v
export LIBS = vine structs
export INCDIRS = ../ocaml ../structs
export OCAMLDEP = ocamldep -I ../ocaml ../structs

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

display: logger_config.ml log_configs.ml \
	 text_logger.ml test_logger.ml
	 ocamlfind ocamlc -package unix, yojson, tyxml -linkpkg $^ -o $@


include $(OCAMLMAKEFILE)

export OCAMLMAKEFILE=../ocaml/OCamlMakefile

# This is a terrible thing to do.  POV XML logger is non standard in
# every way, but I do want the channel parsing to be added to the
# argument list withhout a lot of fuss, and this definitely
# accomplishes that.  Refactor later. [JTT 10/28]
export SOURCES = \
	pov_xml.ml \
	logger_config.ml \
	text_logger.ml \
	yojson_logger.ml \
	log_configs.ml \
	loggers.ml \


RESULT = logging

export PACKS = unix yojson tyxml
export YFLAGS = -v
export LIBS = execution
export INCDIRS = ../execution
export OCAMLDEP = ocamldep -I ../execution

OCAMLFLAGS := -warn-error Aelz
ifdef PROFILE
OCAMLFLAGS += -p
endif
export OCAMLFLAGS

all: dncl

bytecode: dcl top

display: logger_config.ml log_configs.ml \
	 text_logger.ml test_logger.ml
	ocamlfind ocamlc -package unix, yojson, tyxml -linkpkg $^ -o $@


include $(OCAMLMAKEFILE)

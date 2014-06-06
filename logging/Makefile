export OCAMLMAKEFILE=../ocaml/OCamlMakefile

export SOURCES = \
	logger_config.ml \
	text_logger.ml \
	yojson_logger.ml \
	log_configs.ml \
	loggers.ml

RESULT = logging

export PACKS = unix yojson
export YFLAGS = -v

OCAMLFLAGS = -warn-error Aelz  #-w A

all: dncl

bytecode: dcl top

display: logger_config.ml log_configs.ml text_logger.ml test_logger.ml
	ocamlfind ocamlc -package unix,yojson -linkpkg $^ -o $@


include $(OCAMLMAKEFILE)

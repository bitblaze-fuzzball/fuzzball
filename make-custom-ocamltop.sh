#!/bin/bash
# Makes custom ocaml top level with all of hthe things fuzzball relies on pre-loaded
# Effectively, it gives you a repl that's equivalent to the operating environment of fuzzball

OCAML_LIB="/usr/lib/ocaml"
BATTERIES="$OCAML_LIB/batteries"
YOJSON="$OCAML_LIB/yojson"
BINIOU="$OCAML_LIB/biniou"
BATTERIES_CMOS="$BATTERIES/batteriesConfig.cmo $BATTERIES/batteriesHelp.cmo $BATTERIES/batteriesPrint.cmo"
FUZZBALL_CMOS="$OCAML_LIB/str.cma $OCAML_LIB/extlib/extLib.cma $OCAML_LIB/unix.cma $OCAML_LIB/threads/threads.cma"
YOJSON_CMOS="$YOJSON/yojson.cmo $YOJSON/yojson_biniou.cmo"
BINIOU_CMOS="$OCAML_LIB/easy-format/easy_format.cmo $BINIOU/bi_util.cmo $BINIOU/bi_share.cmo $BINIOU/bi_outbuf.cmo $BINIOU/bi_inbuf.cmo $BINIOU/bi_vint.cmo $BINIOU/bi_io.cmo"
TOP_NAME="ocaml-fuzzball"

echo ocamlmktop -o $TOP_NAME $FUZZBALL_CMOS $BINIOU_CMOS $YOJSON_CMOS
eval "ocamlmktop -o $TOP_NAME $FUZZBALL_CMOS $BINIOU_CMOS $YOJSON_CMOS"

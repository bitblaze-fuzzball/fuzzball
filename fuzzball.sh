#!/bin/bash
schroot -c wheezy-i386 -- $CGC_HOME/code/fuzzball/exec_utils/fuzzball $DEFAULT_FUZZ_ARGS -pov-xml-output povinfo/ --always CGC_POV info povinfo/ $@

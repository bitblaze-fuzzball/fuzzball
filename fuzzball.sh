#!/bin/bash

export LD_LIBRARY_PATH=$CGC_HOME/code/release/bin32/:$LD_LIBRARY_PATH

exec $CGC_HOME/code/fuzzball/exec_utils/fuzzball $DEFAULT_FUZZ_ARGS -pov-xml-output povinfo/ --always CGC_POV info povinfo/ $@ 


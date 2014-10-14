#!/bin/bash
schroot -c wheezy-i386 -- /home/fuzzbomb/cgc/trunk/code/fuzzball/exec_utils/fuzzball $DEFAULT_FUZZ_ARGS $@

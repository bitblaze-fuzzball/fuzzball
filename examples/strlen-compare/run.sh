#!/bin/sh
../../exec_utils/fuzzball -linux-syscalls -fuzz-start-addr 0x080485d5 -symbolic-cstring 0x08049880+10 -trace-iterations ./strlen-test -stp-path ../../stp/stp -- ./strlen-test string

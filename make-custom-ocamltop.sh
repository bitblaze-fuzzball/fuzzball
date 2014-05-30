#!/bin/bash
# Makes custom ocaml top level with all of hthe things fuzzball relies on pre-loaded
# Effectively, it gives you a repl that's equivalent to the operating environment of fuzzball

ocamlfind ocamlmktop -package yojson,str,unix,batteries -linkpkg -o ocaml-fuzzball

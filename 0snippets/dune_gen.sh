#!/bin/bash
ocaml dune_gen.ml > dune
cat ./dune
echo ===========
dune build

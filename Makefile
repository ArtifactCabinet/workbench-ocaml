.PHONY: default build test clean

default: build

build:
	dune build

clean:
	dune clean

installdeps:
	opam install batteries core ocaml-lsp-server

.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

clean:
	rm -f data/database/*.db
	dune clean

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	rm -f data/database/*.db
	OCAMLRUNPARAM=b dune exec test/main.exe

switch: 
	opam switch cs3110-project

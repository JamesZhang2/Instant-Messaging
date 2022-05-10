.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

clean:
	dune clean

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

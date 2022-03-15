.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

clean:
	dune clean

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

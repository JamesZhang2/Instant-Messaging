.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

clean:
	dune clean

run: 
	open -a "Google Chrome" _build/default/src/index.html

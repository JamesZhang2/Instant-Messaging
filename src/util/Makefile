.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

clean:
	dune clean

test:
	rm -f data/database/user.db
	OCAMLRUNPARAM=b dune exec test/main.exe

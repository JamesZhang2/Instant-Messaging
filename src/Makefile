.PHONY: test check

build:
	dune build

clean:
	rm -f server/data/database/*.db
	rm -f client/data/database/*.db
	dune clean

docs:
	dune build @doc

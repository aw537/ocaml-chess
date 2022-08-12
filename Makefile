.PHONY: test check

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

doc:
	dune build @doc

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

utop:
	OCAMLRUNPARAM=b dune utop src

zip:
	rm -f chess.zip
	zip -r chess.zip . -x@exclude.lst
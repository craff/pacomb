

.PHONY: all
all: lib/grammar.cmxa lib/grammar.cma ppx/parserPpx.exe

lib/grammar.cmxa:
	dune build $@

lib/grammar.cma:
	dune build $@

ppx/parserPpx.exe:
	dune build $@

.PHONY: tests
tests:
	bash ./test.sh

clean:
	dune clean

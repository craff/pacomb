

.PHONY: all
all: lib/grammar.cmxa

lib/grammar.cmxa:
	dune build lib/grammar.cmxa

.PHONY: tests
tests:
	bash ./test.sh

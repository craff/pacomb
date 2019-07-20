

.PHONY: all
all:
	dune build

.PHONY: tests
tests:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: install
install:
	dune install

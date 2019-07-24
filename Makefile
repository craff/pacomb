

.PHONY: all
all:
	dune build

.PHONY: tests
tests: all
	dune runtest

.PHONY: clean
clean:
	dune clean
	rm -rf doc

.PHONY: install
install:
	dune install

.PHONY: doc
doc:
	dune build @doc

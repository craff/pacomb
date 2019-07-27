

.PHONY: all
all:
	dune build

.PHONY: check
check:
	tools/sanity_check.sh

.PHONY: tests
tests: all check
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

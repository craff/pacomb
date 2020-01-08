

.PHONY: all
all:
	dune build

.PHONY: check
check:
	tools/sanity_check.sh

.PHONY: tests
tests: all check
	dune runtest

.PHONY: bench
bench: all check
	./benchmarks/bench.sh

.PHONY: clean
clean:
	dune clean
	rm -rf doc
	find -name \*.csv -exec rm {} \;

.PHONY: install
install:
	dune install

.PHONY: doc
doc:
	dune build @doc

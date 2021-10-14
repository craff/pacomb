

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
install: all
	dune install

.PHONY: doc
doc:
	dune build @doc

.PHONY: install_doc
install_doc: doc
	rsync -r _build/default/_doc/_html/pacomb   ${HOME}/WWW/
	rsync -r _build/default/_doc/_html/odoc.css ${HOME}/WWW/

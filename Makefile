

.PHONY: all
all:
	dune build

.PHONY: tests
tests:
	dune runtest

.PHONY: clean
clean:
	dune clean
	rm -rf doc

.PHONY: install
install:
	dune install

DOCMODS=lib/grammar.mli lib/combinator.mli lib/lex.mli lib/regexp.mli lib/input.mli \
  lib/position.mli lib/earley.mli

.PHONY: doc
doc:	all
	mkdir -p doc/html doc/latex
	ocamldoc -I _build/default/lib/.pacomb.objs/byte -latex -o doc/latex/pacomb.tex $(DOCMODS)
	cd doc/latex && pdflatex pacomb.tex
	ocamldoc -I _build/default/lib/.pacomb.objs/byte -html -d doc/html $(DOCMODS)

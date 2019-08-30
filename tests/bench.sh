#!/bin/bash

make

opts=--no-print-directory
cd tests

dune build $opts  ../examples/calc.exe ../examples/calc_prio.exe \
                 test.exe big_expr.exe hard.exe

echo general tests
dune exec $opts -- ./test.exe --catalan 12 --sequence 10000

echo testing a grammar with cache
dune exec $opts -- ./hard.exe 100000

echo "testing sexp (with grammar/combinator)"
dune exec $opts -- ./big_sexp.exe 5_000_000 | time dune exec $opts ./sexp.exe > /dev/null

echo "testing sexp with right recursion (with grammar/combinator)"
dune exec $opts -- ./big_sexp.exe 5_000_000 | time dune exec $opts ./sexp_rr.exe > /dev/null

echo "testing sexp (with ocamlyacc)"
dune exec $opts -- ./big_sexp.exe 5_000_000 | time dune exec $opts ./sexp_yacc/sexp.exe > /dev/null

echo "testing the calculator (with grammar/combinator)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ../examples/calc.exe > /dev/null

echo "testing the calculator (with grammar/combinator, utf8 mode)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ../tests/calc_utf8.exe > /dev/null

echo "testing the calculator (with grammar/combinator, using fammilies for prio)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ../examples/calc_prio.exe > /dev/null

echo "testing the calculator (with grammar/combinator, using fammilies for prio and left factorised)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc_factor.exe > /dev/null

echo "testing the calculator (with ocamlyacc)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc_yacc/calc.exe > /dev/null

echo "testing the calculator (with dseq and extensible)"
echo "slow ... be patient"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc_ext_bench.exe > /dev/null

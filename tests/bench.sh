#!/bin/bash

make

opts=--no-print-directory
cd tests

dune build $opts calc.exe test.exe big_expr.exe hard.exe

echo general tests
dune exec $opts -- ./test.exe --catalan 15 --sequence 10000

echo testing a grammar with cache
dune exec $opts -- ./hard.exe 100000

echo "testing the calculator (with grammar/combinator)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc.exe > /dev/null

echo "testing the calculator (with grammar/combinator, using fammilies for prio)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc_prio.exe > /dev/null

echo "testing the calculator (with grammar/combinator, using fammilies for prio and left factorised)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc_factor.exe > /dev/null

echo "testing the calculator (with ocamlyacc)"
dune exec $opts -- ./big_expr.exe 5 4 4 | time dune exec $opts ./calc_yacc/calc.exe > /dev/null

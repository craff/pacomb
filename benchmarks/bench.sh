#!/bin/bash

make

opts=--no-print-directory
cd benchmarks

echo "testing seq"
dune exec $opts -- ./seq.exe

echo "testing sexp"
dune exec $opts -- ./sexp/sexp.exe

echo "testing calc"
dune exec $opts -- ./calc/calc.exe

cd ../examples

echo "testing catalan"
dune exec $opts -- ./catalan.exe 80 2
dune exec $opts -- ./catalan.exe 60 3

#!/bin/bash

cd tests

dune build calc.exe test.exe big_expr.exe hard.exe

dune exec -- ./hard.exe 100000

dune exec -- ./big_expr.exe 5 4 4 | time dune exec ./calc.exe

dune exec ./test.exe

#!/bin/bash

cd tests

dune build calc.exe test.exe big_expr.exe

time dune exec -- ./big_expr.exe 5 4 4 | time dune exec ./calc.exe

time dune exec ./test.exe

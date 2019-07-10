#!/bin/bash

dune build calc.exe test.exe

time _build/default/calc.exe < big_expr

time _build/default/test.exe

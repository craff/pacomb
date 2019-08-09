
#!/bin/bash

# Sanity checks for source files in directory [src].

ML_FILES=`find lib -name "*.ml" -or -name "*.mli"`
TESTS_FILES=`find tests -name "*.ml" -or -name "*.mli" -or -name "*.mll" -or -name "*.mly"`
FILES="ppx/ppx_pacomb.ml $ML_FILES $TESTS_FILES"

awk 'length>80 {print FILENAME ", line " FNR ": more than 80 characters..."}' $FILES
awk '/.*\s$/   {print FILENAME ", line " FNR ": trailing spaces..."}        ' $FILES
awk '/.*\t.*/  {print FILENAME ", line " FNR ": contains tabs..."}          ' $FILES

# Check for issue number of FIXMEs and TODOs in source files and PML files.

awk '/FIXME:? [^#]/ {print FILENAME ", line " FNR ": FIXME without issue number"}' $FILES
awk '/TODO:? [^#]/  {print FILENAME ", line " FNR ": TODO without issue number"}'  $FILES

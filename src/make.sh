#!/bin/sh

SRC=src
BIN=bin
OCAMLC=ocamlopt
CPP=/usr/bin/g++
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

SRC=src/

FLAGS="-g -dtypes -I $SRC"

export OCAMLRUNPARAM=b

function build()
{
    echo "Building..." &&\
    mkdir -p $BIN &&\
    $OCAMLC $FLAGS -c $SRC/global.ml &&\
    $OCAMLC $FLAGS -c $SRC/ast.ml &&\
    $OCAMLYACC -v $SRC/parser.mly && \
	$OCAMLLEX $SRC/lexer.mll && \
	$OCAMLC $FLAGS -c $SRC/parser.mli && \
	$OCAMLC $FLAGS -c $SRC/lexer.ml && \
	$OCAMLC $FLAGS -c $SRC/parser.ml && \
	$OCAMLC $FLAGS -c $SRC/testtyp.ml && \
	$OCAMLC $FLAGS -c $SRC/refchk.ml && \
	$OCAMLC $FLAGS -c $SRC/typchk.ml && \
	$OCAMLC $FLAGS -c $SRC/cktlib.ml && \
	$OCAMLC $FLAGS -c $SRC/gmwimpl.ml && \
	$OCAMLC $FLAGS -c $SRC/gencircuit.ml && \
	$OCAMLC $FLAGS -c $SRC/opsem.ml && \
	$OCAMLC $FLAGS -c $SRC/main.ml &&\
    \
	$OCAMLC $FLAGS -o $BIN/wysteria unix.cmxa str.cmxa global.cmx ast.cmx lexer.cmx parser.cmx testtyp.cmx refchk.cmx typchk.cmx cktlib.cmx gmwimpl.cmx gencircuit.cmx opsem.cmx main.cmx \
	&& \
	echo "- - - - - - - - - - - - - - - - - -" && \
    echo "Wysteria interpreter successfully built.\n"
}

if [ $# -eq 0 ]
then
    build
elif [ "$1" == "clean" ]
then
    rm -f median optmedian med.wy config* in_* out_* sh* circuit_*.txt src/*.cmi src/*.cmx src/*.cmo src/*.o src/*.annot *.smt perf_* otm_* ot_*
else
    echo "Usage: ./make.sh [clean]+"
fi

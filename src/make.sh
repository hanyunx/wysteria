#!/bin/sh

LEXER=wylexer
PARSER=wyparser
EMBED=false

SRC=src
BIN=bin

if [ "$EMBED" = true ]
then
    OCAMLC=ocamlc
    OBJS="unix.cma str.cma global.cmo sysproc.cmo ast.cmo $LEXER.cmo $PARSER.cmo testtyp.cmo refchk.cmo typchk.cmo cktlib.cmo gmwimpl.cmo gencircuit.cmo opsem.cmo driver.cmo"
else
    OCAMLC=ocamlopt
    OBJS="unix.cmxa str.cmxa global.cmx sysproc.cmx ast.cmx $LEXER.cmx $PARSER.cmx parsehelp.cmx testtyp.cmx refchk.cmx typchk.cmx cktlib.cmx gmwimpl.cmx gencircuit.cmx opsem.cmx driver.cmx main.cmx"
fi

CPP=/usr/bin/g++
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
MKOCAMLTOP=ocamlmktop

SRC=src/

FLAGS="-g -dtypes -I $SRC"

export OCAMLRUNPARAM=b

build ()
{
    echo "Building..." &&\
    mkdir -p $BIN &&\
    $OCAMLC $FLAGS -c $SRC/global.ml &&\
    $OCAMLC $FLAGS -c $SRC/sysproc.ml &&\
    $OCAMLC $FLAGS -c $SRC/ast.ml &&\
    $OCAMLYACC -v $SRC/$PARSER.mly && \
	$OCAMLLEX $SRC/$LEXER.mll && \
	$OCAMLC $FLAGS -c $SRC/$PARSER.mli && \
	$OCAMLC $FLAGS -c $SRC/$LEXER.ml && \
	$OCAMLC $FLAGS -c $SRC/$PARSER.ml && \
	$OCAMLC $FLAGS -c $SRC/parsehelp.ml && \
	$OCAMLC $FLAGS -c $SRC/testtyp.ml && \
	$OCAMLC $FLAGS -c $SRC/refchk.ml && \
	$OCAMLC $FLAGS -c $SRC/typchk.ml && \
	$OCAMLC $FLAGS -c $SRC/cktlib.ml && \
	$OCAMLC $FLAGS -c $SRC/gmwimpl.ml && \
	$OCAMLC $FLAGS -c $SRC/gencircuit.ml && \
	$OCAMLC $FLAGS -c $SRC/opsem.ml && \
	$OCAMLC $FLAGS -c $SRC/driver.ml &&\
	$OCAMLC $FLAGS -c $SRC/main.ml &&\
	$OCAMLC $FLAGS -o $BIN/wysteria $OBJS \
	&& \
	echo "- - - - - - - - - - - - - - - - - -" && \
    echo "Wysteria interpreter successfully built.\n"
    # $CPP -o median examples/genmedian.cpp && \
    # $CPP -o optmedian examples/genoptmedian.cpp && \
    # $CPP -o psigen examples/psigen.cpp && \
    # $CPP -o psiinpgen examples/genpsiinp.cpp && \
    # $CPP -o p2pinpgen examples/genp2pinps.cpp && \
    # echo "Median generators compiled.\n"
}

mktop ()
{
    $MKOCAMLTOP -o $BIN/wtop -I $SRC $OBJS && \
    echo "Wysteria top-level successfully built.\n"
}

if [ $# -eq 0 ]
then
    build
    if [ "$EMBED" = true ]
    then
	mktop
    fi
elif [ "$1" == "clean" ]
then
    rm -f median optmedian med.wy config* in_* out_* sh* circuit_*.txt src/*.cmi src/*.cmx src/*.cmo src/*.o src/*.annot *.smt perf_* otm_* ot_* src/$PARSER.ml src/$LEXER.ml src/$PARSER.mli $PARSER.output $BIN/wysteria $BIN/wtop
else
    echo "Usage: ./make.sh [clean]+"
fi

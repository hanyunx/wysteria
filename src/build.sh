#!/bin/sh

MAIN=$PWD

echo "Building GMW"

pushd ../gmw &> /dev/null
make

if [ $? = 0 ]
then
    cp mpc.exe $MAIN
else
    echo "Error compiling GMW"
    exit 1
fi

echo "Successfully built GMW"
popd &> /dev/null


echo "Building Z3"

pushd ../z3 &> /dev/null
autoconf
./configure
python scripts/mk_make.py
pushd build &> /dev/null
make

if [ $? = 0 ]
then
    cp z3 $MAIN
else
    echo "Error compiling Z3"
    exit 1
fi

echo "Successfully built Z3"
popd &> /dev/null
popd &> /dev/null

echo "Building Wysteria"
./make.sh

if [ $? = 0 ]
then
    echo "Successfully built Wysteria"
else
    echo "Error building Wysteria"
    exit 1
fi

exit 0

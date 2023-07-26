#!/bin/bash

set -eou pipefail

i=3;
j=$#;

TC=$1
shift
RUNTIME=$1
shift

echo TC=$TC
echo RUNTIME=$RUNTIME

echo PWD: `pwd`

while [ $i -le $j ]
do
    tig=$1

    ./$TC $tig

    # this / means this only works on non-windows. oh well.
    asm_name=`dirname $tig`/`basename $tig .tig`.s

    gcc -fPIE $RUNTIME $asm_name

    ./a.out

    i=$((i+1))
    shift
done

#!/bin/bash

set -eou pipefail

i=3;
j=$#;

TC=$1
shift
RUNTIME=$1
shift

echo "Starting e2e test"
echo TC=$TC
echo RUNTIME=$RUNTIME
echo PWD: `pwd`

while [ $i -le $j ]
do
    tig=$1
    echo "Compiling tiger program with tc: "$tig

    ./$TC $tig

    # this / means this only works on non-windows. oh well.
    asm_name=`dirname $tig`/`basename $tig .tig`.s

    echo "Linking with runtime: "$asm_name
    gcc -fPIE $RUNTIME $asm_name

    echo "Invoking the compiled tiger program with timeout..."
    result=0
    # || trick from https://stackoverflow.com/questions/26675681/how-to-check-the-exit-status-using-an-if-statement
    sh -c "timeout 5s ./a.out" || result=$?

    if [ $result -ne 0 ]
    then
        echo "tiger program failed, got status "$result
        exit $result
    fi
    echo "Succeeded for :"$tig

    i=$((i+1))
    shift
done

echo "All successful"
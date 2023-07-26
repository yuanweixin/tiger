#!/bin/bash

set -eou pipefail

i=3;
j=$#;

TC=$1
shift
RUNTIME=$1
shift

echo $TC
echo $RUNTIME

while [ $i -le $j ]
do
    tig=$1
    i=$((i+1))
    echo $tig
    shift
done

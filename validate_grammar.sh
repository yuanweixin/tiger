FILES=`ls tests/tiger_programs/parsing/good`
for f in $FILES; do 
    if nimbleparse -q -y grmtools src/tiger.l src/tiger.y tests/tiger_programs/parsing/good/$f &> /dev/null; then
        echo $f is good 
    else
        echo $f expected to parse but did not
    fi
done

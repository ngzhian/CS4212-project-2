# ./main "testcases/proc" # fails

TESTCASES=./testcases/*
for f in $TESTCASES
do
    if [ "$f" != "./testcases/proc" ]
    then
        # echo "$f: `./main $f`"
        printf "%-30s `./main $f`\n" "$f"
    fi
done

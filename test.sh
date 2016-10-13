# ./main "testcases/proc" # fails

TESTCASES=./testcases/*
for f in $TESTCASES
do
    # echo "$f: `./main $f`"
    printf "%-30s `./main $f`\n" "$f"
done

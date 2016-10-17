# ./test.sh
# to run parsing tests
# ./test.sh -t
# to run parsing tests and type checks

if [[ "$1" == "-t" ]]
then
    TESTCASES=./testcases/typecheck/*
    echo "Typechecking..."
else
    TESTCASES=./testcases/parsing/*
    echo "Check parsing..."
fi

for f in $TESTCASES
do
    # echo "$f: `./main $f`"
    printf "%-50s `./main $1 $f`\n" "$f"
done

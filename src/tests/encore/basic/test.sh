#!/bin/bash

function run_test {
    program=$1
    expected=$program.out
    ./$program | cmp $expected
}

passed=0
failed=0
failed_list=()

for prog in "$@"
do
    if run_test $prog
    then
        ((passed++))
    else
        ((failed++))
        failed_list+=($prog)
    fi
done

total=$(($passed + $failed))

echo "    " Tests passed: $passed"/"$total
echo "    " Tests failed: $failed"/"$total
for prog in ${failed_list[@]}
do
    echo "        " $prog
done

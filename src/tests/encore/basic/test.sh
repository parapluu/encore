#!/bin/bash

ROOT_PATH=../../../..
ENCOREC=${ROOT_PATH}/release/encorec

function run_test {
    # the function looks for either a file called "testname.out" or a
    # script called "testname.chk".
    #
    # In the case of an .out file, the output of the executable has to
    # match exactly; in the case of a .chk file, the output of
    # compiling and (if successful) running the executable will be
    # piped into that script. The test is successful iff the script
    # returns normally (exit 0).

    program=$1
    source=$1.enc
    # compile and, if successful, run the program:
    output=$(${ENCOREC} $source && ./$program)
    checking_script=./$program.chk
    expected=$program.out
    if [ -x "$checking_script" ] ; then
        if [ -e $expected ] ; then
            echo "ERROR: both $checking_script and $expected exist, don't know which to use"
            false
        else
            echo "$output" | ./$checking_script ||
                (echo "ERROR: test $program's checking script failed.";
                 false)
        fi
    else
        if [ -e "$expected" ]; then
            # echo "$output" | cmp $expected ||
            ./$program | cmp $expected ||
                (echo "ERROR: test $program failed with output:";
                 echo "vvvvvvvvvvvvvvvvvvvv"
                 echo "$output"
                 echo "^^^^^^^^^^^^^^^^^^^^";
                 false)
        else
            echo "ERROR: incomplete test <$1>: neither checking script $checking_script, nor output file $expected is available"
            exit 1
        fi
    fi
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

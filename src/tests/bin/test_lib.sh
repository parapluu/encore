#! /usr/bin/env bash

readonly ROOT_PATH=$(pwd)/../..
readonly ENCOREC=${ROOT_PATH}/release/encorec

############################################################
#
# Returns whether a test is disabled/ignored. For this, one of the regexes in
# IGNORED_FILES.grep must match the test name.
#
# Arguments:
#  - Name of the test (no file extension), relative to current working
#    directory.
#
# Returns:
#  true  -- test is enabled
#  false -- test is disabled
#
############################################################
function test_enabled() {
    ! grep --invert-match "^#" < IGNORED_FILES.grep | grep $1"$" > /dev/null
}

############################################################
#
# Checks whether there is exactly one test file (either .out, .fail, .err, or .chk)
# for the given program.
#
# Arguments:
#  The name of the test (no file extension), relative to the current working
#  directory.
#
# Returns:
#  true  -- have exactly one spec file
#  false -- have more or less than one spec file
#
############################################################
function have_one_spec_file() {
    local NAME=$1

    if [   -e "${NAME}.out" -a ! -e "${NAME}.chk" -a ! -e "${NAME}.err" -a ! -e "${NAME}.fail" -o \
         ! -e "${NAME}.out" -a   -e "${NAME}.chk" -a ! -e "${NAME}.err" -a ! -e "${NAME}.fail" -o \
         ! -e "${NAME}.out" -a ! -e "${NAME}.chk" -a   -e "${NAME}.err" -a ! -e "${NAME}.fail" -o \
         ! -e "${NAME}.out" -a ! -e "${NAME}.chk" -a ! -e "${NAME}.err" -a   -e "${NAME}.fail"    \
       ]; then
        true
    else
        false
    fi
}

############################################################
#
# Compiles a test, echoes the output.
#
# Arguments:
#
#  The name of the test (no file extension), relative to the current working
#  directory.
#
# Returns:
#  None
#
############################################################
function compile() {
    readonly local TEST=$1
    readonly local DIR=$( dirname  ${TEST})
    readonly local NAME=$(basename ${TEST})
    readonly local CWD=$( pwd)
    readonly local FLAGS=${@:2}

    # cd to the directory and compile there as a workaround to #439
    cd ${DIR}
    ${ENCOREC} ${NAME}.enc ${FLAGS}
    cd ${CWD}
}

############################################################
#
# Runs a test, echoes the output.
#
# Arguments:
#
#  The name of the test (no file extension), relative to the current working
#  directory.
#
# Returns:
#  the return value of running the test
#
############################################################
function run() {
    readonly local TEST=$1
    readonly local DIR=$( dirname  ${TEST})
    readonly local NAME=$(basename ${TEST})
    readonly local CWD=$( pwd)

    local CMD="./${NAME}"
    if [ -e "${TEST}.run" ]; then
        CMD=$(cat "${TEST}.run")
    fi

    cd ${DIR}
    ${CMD}
    local RET=$?
    rm -f "${NAME}"
    cd ${CWD}
    return ${RET}
}

############################################################
#
# Checks whether there is at least one spec file (either .out, .fail, .err, or .chk)
# for the given program.
#
# Arguments:
#   - The name of the test (no file extension), relative to the current working
#     directory.
#
# Returns:
#  true  -- there is at least one spec file
#  false -- there is none
#
############################################################
function have_at_least_one_spec_file() {
    local TEST=$1
    if [ -e "${TEST}.out"  -o \
         -e "${TEST}.fail" -o \
         -e "${TEST}.err" -o \
         -e "${TEST}.chk"     \
       ]; then
        true
    else
        false
    fi
}

############################################################
#
#
# 1. Compiles the test file.
#
# 2. Makes sure that compilation fails.
#
# 3. Ensures that every line in .../.../testname.fail is contained in the
#    compiler output.
#
# If these conditions are not met, there is a line starting with "ERROR:" in the
# output.
#
# Arguments:
#  - The test name (no file extension)
#
# Returns:
#  None
#
############################################################
function run_fail_test() {
    local TEST=$1
    local FLAGS=$@
    local COMPILE_OUT=$(compile ${TEST} ${FLAGS})
    if [ -e ${TEST} ]; then
        echo "ERROR: Test should not compile."
    fi

    if [ ! -s ${TEST}.fail ]; then
      #file is empty
      echo "ERROR: ${TEST}.fail is empty!"
    fi

    # read .fail file line by line and verify that each line is in the compiler
    # output:
    cat ${TEST}.fail | grep -v "^$" | while read LINE; do
        if ! (echo "${COMPILE_OUT}" | grep "${LINE}" > /dev/null); then
            echo "ERROR: Line not contained in error output: '${LINE}'"
        fi
    done
}

############################################################
#
# 1. Compiles the test file.
#
# 2. Makes sure that compilation succeeds.
#
# 3. Ensures that .../.../testname.out exactly matches the output of the
#    executable.
#
# If these conditions are not met, there is a line starting with "ERROR:" in the
# output.
#
# Arguments:
#  - The test name (no file extension)
#
# Returns:
#  None
#
############################################################
function run_out_test() {
  local TEST=$1
  local FLAGS=$@
  echo ${TEST}
  local COMPILE_OUT=$(compile ${TEST} ${FLAGS})
  echo -e "$COMPILE_OUT"
  if [ ! -e ${TEST} ]; then
      echo "ERROR: ${TEST}.enc should compile."
      return
  fi

  local OUTPUT=$(run ${TEST})

  # echo once without the newline at the end of the output and once with the
  # newline to give the .out files a bit of flexibility:
  if (echo -n "${OUTPUT}" | cmp -s ${TEST}.out); then
      return
  fi
  if (echo    "${OUTPUT}" | cmp -s ${TEST}.out); then
      return
  fi

  echo "ERROR: test ${TEST} failed with output:";
  echo "vvv OUTPUT vvvvvvvvvvvvvvvvv"
  echo "$OUTPUT"
  echo "vvv EXPECTED vvvvvvvvvvvvvvv"
  cat ${TEST}.out
  echo ""
  if which diff>/dev/null; then
      echo "vvv DIFF vvvvvvvvvvvvvvvvvvv"
      echo "$OUTPUT" | diff ${TEST}.out -
  fi
  echo "^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
}

############################################################
#
# 1. Compiles the test file.
#
# 2. Makes sure that compilation succeeds.
#
# 3. Runs the test, and expects the test to fail, i.e., return a non-zero exit status
#
# 3. Ensures that .../.../testname.err exactly matches the output of
#    executable, where stderr was redirected to stdout. NOTE that it may
#    not be safe to mix and match stdout and stderr output because of how
#    these are buffered. Ideally, write all output to the same stream
#
# If these conditions are not met, there is a line starting with "ERROR:" in the
# output.
#
# Arguments:
#  - The test name (no file extension)
#
# Returns:
#  None
#
############################################################
function run_err_test() {
  local TEST=$1
  local FLAGS=$@
  echo ${TEST}
  local COMPILE_ERR=$(compile ${TEST} ${FLAGS})
  echo -e "$COMPILE_ERR"
  if [ ! -e ${TEST} ]; then
      echo "ERROR: ${TEST}.enc should compile."
      return
  fi

  local OUTPUT
  if OUTPUT=$(run ${TEST} 2>&1); then
      echo "ERROR: ${TEST} did not fail at run-time, as expected"
      return
  fi
  readonly OUTPUT

  # echo once without the newline at the end of the output and once with the
  # newline to give the .out files a bit of flexibility:
  if (echo -n "${OUTPUT}" | cmp -s ${TEST}.err); then
      return
  fi
  if (echo    "${OUTPUT}" | cmp -s ${TEST}.err); then
      return
  fi

  echo "ERROR: test ${TEST} failed with output:";
  echo "vvv OUTPUT vvvvvvvvvvvvvvvvv"
  echo "$OUTPUT"
  echo "vvv EXPECTED vvvvvvvvvvvvvvv"
  cat ${TEST}.err
  echo ""
  if which diff>/dev/null; then
      echo "vvv DIFF vvvvvvvvvvvvvvvvvvv"
      echo "$OUTPUT" | diff ${TEST}.err -
  fi
  echo "^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
}

############################################################
#
# 1. Compiles the test file.
#
# 2. Makes sure that compilation succeeds.
#
# 3. Pipes the output into the .../.../testname.chk script. The test succeeds
#    iff the .chk script exits normally (exit 0, not exit 1).
#
# If these conditions are not met, there is a line starting with "ERROR:" in the
# output.
#
# Arguments:
#  - The test name (no file extension)
#
# Returns:
#  None
#
############################################################
function run_chk_test() {
    echo "ERROR: chk tests not implemented"
}

############################################################
#
# Looks for EITHER:
#  - a file called "testname.out",   OR
#  - a file called "testname.fail",  OR
#  - a file called "testname.err",   OR
#  - a script called "testname.chk".
#
# In the case of an .out file, the output of the executable has to match the
# file's content exactly; in the case of a.fail file, compilation has to fail,
# and the compiler output has to contain every line of the file; in the case of
# a .chk file, the output of compiling and running the executable will be piped
# into that script as standard input. The test is successful iff the script
# exits normally (exit 0, not exit 1).
#
# Arguments:
#
#  The name of the test (no file extension), relative to the current working
#  directory.
#
# Returns:
#  None
#
############################################################
function run_test() {
    local TEST=$1
    local PROGRAM=${TEST}.enc

    if [ ! -f ${PROGRAM} ]; then
        echo "ERROR: test ${PROGRAM} does not exist."
    fi

    if ! have_one_spec_file ${TEST}; then
        echo "ERROR: have several specifications (.out/.fail/.err/.chk) for test $TEST"
        return $(false)
    fi

    # just compile program:

    if [ -e ${TEST}.flags ]; then
        FLAGS=$(cat ${TEST}.flags)
    fi

    if [ -e ${TEST}.fail ]; then
        echo "running fail test..."
        run_fail_test ${TEST} ${FLAGS}
        return $?
    fi

    if [ -e ${TEST}.out ]; then
        echo "running out test..."
        run_out_test ${TEST} ${FLAGS}
        return $?
    fi

    if [ -e ${TEST}.err ]; then
        echo "running err test..."
        run_err_test ${TEST} ${FLAGS}
        return $?
    fi

    if [ -e ${TEST}.chk ]; then
        echo "running chk test..."
        run_chk_test ${TEST} ${FLAGS}
        return $?
    fi

    echo -n "ERROR: BUG: should not reach here,"
    echo    " exactly one of the cases above should match"

    return $(false)
}

############################################################
#
# Executes all tests in a given directory, non-recursively.
#
# Arguments:
#
#  - Test directory, relative path to the current working directory.
#  - Temp directory to which the function may write logs.
#
# Returns:
#  None
#
############################################################
function run_test_suite() {
    local readonly REL_PATH=$1
    local readonly TMP_DIR=$2

    local SUCC=0
    local TOTL=0

    if [ -z ${REL_PATH} ]; then
        echo "ERROR: Need relative path to test suite (directory containing tests)"
        exit 1
    fi

    if [ -z ${TMP_DIR} ]; then
        echo "ERROR: Need temporary directory (got none)"
        exit 1
    fi

    if ! test_enabled ${REL_PATH}; then
        continue
    fi
    echo ">>> RUNNING TEST_SUITE ${REL_PATH}"

    if [ -e ${REL_PATH}/Makefile ]; then
        echo "    Found Makefile in ${REL_PATH}. Running!"
        make --directory ${REL_PATH} clean | sed "s/\(.*\)/     make clean | \1/"
        make --directory ${REL_PATH} -k    | sed "s/\(.*\)/           make | \1/"
    fi

    for ENC_FILE in "${REL_PATH}"/*.enc; do
      local TEST_NAME=$(echo ${ENC_FILE} | sed "s/\.enc$//" | sed "s/\.\///")

      if ! test_enabled ${TEST_NAME}; then
        continue
      fi
      echo " - $(basename ${TEST_NAME})..."

      if have_at_least_one_spec_file ${TEST_NAME}; then
        TOTL=$((TOTL+1))

        local TEST_OUT=$(run_test ${TEST_NAME})
        if (echo ${TEST_OUT} | grep ERROR > /dev/null); then
            local REPORT_FILE="${TMP_DIR}/${TEST_NAME}.FAILED"
          echo "    ERROR: test failed"
          mkdir -p "$(dirname "${REPORT_FILE}")"

          echo -e "${TEST_OUT}" > ${REPORT_FILE}
        else
            SUCC=$((SUCC+1))
        fi
      else
          echo "   Warning: .enc file present but missing .out, .fail, .err or .chk file."
          echo "   Consider adding '${TEST_NAME}.enc' to INGORED_FILE.grep to"
          echo "   suppress this warning."
      fi

      # remove the test executable (if it has been created):
      rm -f ${TEST_NAME} 2> /dev/null
    done

    echo -e "${REL_PATH}:\t${SUCC}/${TOTL} tests passed" >> /${TMP_DIR}/test_counts.log
}

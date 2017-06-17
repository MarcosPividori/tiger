RED='\033[0;31m'
NC='\033[0m'

export LIBRARY_PATH=../build:$LIBRARY_PATH

for TEST in $(find . -name *.tig)
do
  # Execute the compilation tests.
  grep "EXPECTED-COMP:" $TEST | while read -r line; do
    CHECK=`echo $line | sed -n 's/[^"]*"\([^"]*\)".*/\1/p'`
    echo -n "TEST-COMP $TEST: "
    if ../build/tiger $TEST 2>&1 | grep -q "$CHECK"
      then echo "PASS"
      else echo "${RED}FAIL (Expected: \"$CHECK\")${NC}"
    fi
  done

  # If necessary, build the executable for the run tests.
  if grep -q "EXPECTED-RUN:" $TEST && ! grep -q "EXPECTED-COMP:" $TEST
  then ../build/tiger $TEST > /dev/null 2>&1; fi

  # Execute the run tests.
  grep "EXPECTED-RUN:" $TEST | while read -r line; do
    RUN=`echo $line | sed -n 's/[^"]*"\([^"]*\)".*/\1/p'`
    CHECK=`echo $line | sed -n 's/[^"]*"[^"]*"\s*=>\s*"\([^"]*\)".*/\1/p'`
    echo -n "TEST-RUN $TEST: "
    if /bin/sh -c "$RUN" 2>&1 | grep -q "$CHECK"
      then echo "PASS"
      else echo "${RED}FAIL (Cmd: \"$RUN\" Expected: \"$CHECK\")${NC}"
    fi
  done
done

rm -f ./a.out

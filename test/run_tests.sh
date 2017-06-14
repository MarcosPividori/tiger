RED='\033[0;31m'
NC='\033[0m'

export LIBRARY_PATH=../build:$LIBRARY_PATH

for TEST in $(find . -name *.tig)
do
  grep "EXPECTED-COMP:" $TEST | while read -r line; do
    CHECK=`echo $line | sed -n 's/[^"]*"\([^"]*\)".*/\1/p'`
    echo -n "TEST-COMP $TEST: "
    if ../build/tiger $TEST 2>&1 | grep -q "$CHECK"
      then echo "PASS"
      else echo "${RED}FAIL (Expected: \"$CHECK\")${NC}"
    fi
  done

  if grep -q "EXPECTED-RUN:" $TEST
  then ../build/tiger $TEST > /dev/null 2>&1; fi

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

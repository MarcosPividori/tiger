RED='\033[0;31m'
NC='\033[0m'
for TEST in $(find . -name *.tig)
do
  if grep -q "EXPECTED:" $TEST
  then
    CHECK=`sed -n 's/\/\*\s*EXPECTED:\s*"\([^"]*\)".*/\1/p' $TEST`
    echo -n "TEST $TEST: "
    if ../build/tiger $TEST 2>&1 | grep -q "$CHECK"
      then echo "PASS"
      else echo "${RED}FAIL (Expected: \"$CHECK\")${NC}"
    fi
  fi
done

for TEST in $(find . -name *.tig)
do
  if grep -q "EXPECTED:" $TEST
  then
    CHECK=`sed -n 's/\/\*\s*EXPECTED:\s*"\([^"]*\)".*/\1/p' $TEST`
    if ../build/tiger $TEST 2>&1 | grep -q "$CHECK"
      then echo "TEST $TEST: PASS"
      else echo "TEST $TEST: FAIL (Expected: \"$CHECK\")"
    fi
  fi
done

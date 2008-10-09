function failed() {
  echo "*** FAILED $1";
  failed="$failed $1";
}

function compile_test() {
  base=$1
  f=test/$base.res
  o=test/$base.out
  if [ -e $f ] 
  then
    gcc a.out.c
    ./a.out > $o
    diff -u $f $o
  else
    gcc -fsyntax-only a.out.c
  fi
}

if [ "$#" -gt 0 ]; then
    tests="$@"
else
    tests=`cat test/basic_tests.txt`;
fi

#LOG_CMD="| tee"
#LOG_CMD=" > "

for f in $tests
do 
    base=`basename $f .c`
    echo $f 1>&2
    ( ./zl test/$f > test/$base.log && compile_test $base ) \
    || failed $f
done

if [ -n "$failed" ]; then 
    echo FAILED TESTS: $failed 1>&2
else
    echo ALL TEST PASSED 1>&2
fi


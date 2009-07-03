function failed() {
  echo "*** FAILED $1";
  failed="$failed $1";
}

function compile_test() {
  base=$1
  f=test/$base.res
  o=test/$base.out
  cp a.out.zls test/$base.out.zls
  if [ -e $f ] 
  then
    zls test/$base.out.zls || return 1
    ./a.out > $o
    diff -u $f $o
  else
    zls -fsyntax-only test/$base.out.zls
  fi
  if [ $? -ne 0 ]; then return 1; fi
  zls -S test/$base.out.zls
  mv $base.out.s test/$base.out.zls.s
  ./zl -s test/$base.out.zls > test/$base.log
  mv a.out.zls test/$base.out2.zls
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


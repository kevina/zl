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
    gcc a.out.c || return 1
    ./a.out > $o
    diff -u $f $o
    zls a.out.zls || return 1
    ./a.out > $o
    diff -u $f $o
  else
    gcc -fsyntax-only a.out.c
    zls -fsyntax-only a.out.zls
  fi
  if [ $? -ne 0 ]; then return 1; fi
  zls -S a.out.c
  cp a.out.s test/$base.out.c.s
  zls -S a.out.zls
  cp a.out.s test/$base.out.zls.s
  diff -I "\.file" -u test/$base.out.c.s test/$base.out.zls.s
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
    cp a.out.c test/$base.out.c
    cp a.out.zls test/$base.out.zls
done

if [ -n "$failed" ]; then 
    echo FAILED TESTS: $failed 1>&2
else
    echo ALL TEST PASSED 1>&2
fi


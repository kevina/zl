function failed() {
  echo "*** FAILED $1";
  failed="$failed $1";
}

for f in `cat test/basic_tests.txt`
do 
    echo $f 1>&2
    ( ./zl test/$f && gcc -fsyntax-only a.out.c ) \
    || failed $f
done

echo FAILED TESTS: $failed

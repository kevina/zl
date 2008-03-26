for f in `cat test/basic_tests.txt`
do 
    echo $f 1>&2
    ./zl test/$f # > /dev/null
    gcc -fsyntax-only a.out.c
done

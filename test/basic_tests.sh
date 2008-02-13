for f in `cat test/basic_tests.txt`
do 
    echo $f
    ./zl < test/$f # > /dev/null
done

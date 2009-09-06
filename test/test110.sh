set -e

cp test110v1.zl test110c.zl

$ZL test110c.zl >> test110.log
$ZL test110main.zl >> test110.log

$ZLS -c test110c.zls test110main.zls

$ZLS test110c.o test110main.o

./a.out > test110.out

cp test110v2.zl test110c.zl

$ZL test110c.zl >> test110.log
$ZLS -c test110c.zls

$ZLS test110c.o test110main.o
./a.out >> test110.out

set -e

cp test109v1.zl test109c.zl

$ZL test109c.zl >> test109.log
$ZL test109main.zl >> test109.log

$ZLS -c test109c.zls test109main.zls

$ZLS test109c.o test109main.o

./a.out > test109.out

cp test109v2.zl test109c.zl

$ZL test109c.zl >> test109.log
$ZLS -c test109c.zls

$ZLS test109c.o test109main.o
./a.out >> test109.out

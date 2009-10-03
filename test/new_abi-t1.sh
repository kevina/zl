set -e

cp new_abi-t1-v1.zl new_abi-t1-c.zl

$ZL new_abi-t1-c.zl >> new_abi-t1.log
$ZL new_abi-t1-main.zl >> new_abi-t1.log

$ZLS -c new_abi-t1-c.zls new_abi-t1-main.zls

$ZLS new_abi-t1-c.o new_abi-t1-main.o

./a.out > new_abi-t1.out

cp new_abi-t1-v2.zl new_abi-t1-c.zl

$ZL new_abi-t1-c.zl >> new_abi-t1.log
$ZLS -c new_abi-t1-c.zls

$ZLS new_abi-t1-c.o new_abi-t1-main.o
./a.out >> new_abi-t1.out

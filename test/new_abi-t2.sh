set -e

cp new_abi-t2-v1.zl new_abi-t2-c.zl

$ZL new_abi-t2-c.zl >> new_abi-t2.log
$ZL new_abi-t2-main.zl >> new_abi-t2.log

$ZLS -c new_abi-t2-c.zls new_abi-t2-main.zls

$ZLS new_abi-t2-c.o new_abi-t2-main.o

./a.out > new_abi-t2.out
echo "---" >> new_abi-t2.out

cp new_abi-t2-v2.zl new_abi-t2-c.zl

$ZL new_abi-t2-c.zl >> new_abi-t2.log
$ZLS -c new_abi-t2-c.zls

$ZLS new_abi-t2-c.o new_abi-t2-main.o
./a.out >> new_abi-t2.out

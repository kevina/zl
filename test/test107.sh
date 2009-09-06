set -e

$ZL test107a.zl > test107.log
$ZL test107b.zl >> test107.log

$ZLS test107a.zls test107b.zls

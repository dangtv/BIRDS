#!/bin/sh

RESULTS="results"
LOGFILE=benchmarks.log

function print_log() {

	local message=$1

	echo `date +"%Y-%m-%d %H:%M:%S"` "["`date +%s`"] : $message" >> $RESULTS/$LOGFILE;

}

mkdir -p $RESULTS
# rm $RESULTS/$LOGFILE || true

echo "==> verifying datalog programs";
print_log "==> verifying datalog programs";

# bash putbxdebug.sh $RESULTS
# bash putbxdebug.inc.sh $RESULTS

bash putbx.sh $RESULTS
bash putbx.inc.sh $RESULTS

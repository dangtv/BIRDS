#!/bin/bash

# HOST="localhost"
# USER=postgres
# PORT=5432
# PASSWD=12345678
# DB="simple_employee"

RESULTS=$1

LOGFILE=putbx.log
RESULTSFILE=putbx_time.csv

function print_log() {

	local message=$1

	echo `date +"%Y-%m-%d %H:%M:%S"` "["`date +%s`"] : $message" >> $RESULTS/$LOGFILE;

}

mkdir -p $RESULTS
mkdir -p $RESULTS/gensql
mkdir -p $RESULTS/compile

# rm $RESULTS/$LOGFILE || true
# rm $RESULTS/$RESULTSFILE || true

for FILE in $(cat dl_list)
do
    print_log "==> running on $FILE.dl" 
    echo "==> running on $FILE.dl" 
    mkdir -p $RESULTS/compile/$FILE
    mkdir -p $RESULTS/gensql/$FILE
    /usr/bin/time -a -f "$FILE , %e" -o $RESULTS/$RESULTSFILE birds -f $FILE.dl -o $RESULTS/gensql/$FILE.sql -v -u > $RESULTS/compile/$FILE.log 2>> $RESULTS/compile/$FILE.err
    sleep 10
done


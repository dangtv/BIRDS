#!/bin/bash

HOST="localhost"
USER=postgres
PORT=5432
PASSWD=12345678
DB="experiments"

# 

rm viewupdate/1.csv

for DBSIZE in 1 10 100 1000 10000 100000 500000 1000000 1500000 2000000 2500000 3000000
do
    bash load-gendb.sh $DBSIZE
    PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -f gensql/luxuryitems.sql
    PGPASSWORD=12345678 /usr/bin/time -a -f "$DBSIZE , %e" -o viewupdate/1.csv timeout 120 psql -U $USER -h $HOST -p $PORT -d $DB -f viewupdate/1.sql
    sleep 2
done

rm viewupdate/1.inc.csv

for DBSIZE in 1 10 100 1000 10000 100000 500000 1000000 1500000 2000000 2500000 3000000
do
    bash load-gendb.sh $DBSIZE
    PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -f gensql/luxuryitems.inc.sql
    PGPASSWORD=12345678 /usr/bin/time -a -f "$DBSIZE , %e" -o viewupdate/1.inc.csv timeout 120 psql -U $USER -h $HOST -p $PORT -d $DB -f viewupdate/1.sql
    sleep 2
done

rm viewupdate/2.csv

for DBSIZE in 1 10 100 1000 10000 100000 500000 1000000 1500000 2000000 2500000 3000000
do
    bash load-gendb.sh $DBSIZE
    PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -f gensql/officeinfo.sql
    PGPASSWORD=12345678 /usr/bin/time -a -f "$DBSIZE , %e" -o viewupdate/2.csv timeout 120 psql -U $USER -h $HOST -p $PORT -d $DB -f viewupdate/2.sql
    sleep 2
done

rm viewupdate/2.inc.csv

for DBSIZE in 1 10 100 1000 10000 100000 500000 1000000 1500000 2000000 2500000 3000000
do
    bash load-gendb.sh $DBSIZE
    PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -f gensql/officeinfo.inc.sql
    PGPASSWORD=12345678 /usr/bin/time -a -f "$DBSIZE , %e" -o viewupdate/2.inc.csv timeout 120 psql -U $USER -h $HOST -p $PORT -d $DB -f viewupdate/2.sql
    sleep 2
done
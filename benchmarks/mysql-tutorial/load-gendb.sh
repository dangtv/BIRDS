#!/bin/bash

HOST="localhost"
USER=postgres
PORT=5432
PASSWD=12345678
DB="experiments"

DBSIZE=$1

cp dbgen/items-$DBSIZE.csv /tmp/
cp dbgen/offices-$DBSIZE.csv /tmp/

service postgresql restart
cd db 
bash init_db.sh
cd ..

PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM items;"

PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "COPY items FROM '/tmp/items-$DBSIZE.csv' WITH (FORMAT csv, DELIMITER ',');"

PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM payments;"
PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM orderdetails;"
PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM orders;"
PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM customers;"
PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM employees;"
PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "DELETE FROM offices;"

PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -c "COPY offices FROM '/tmp/offices-$DBSIZE.csv' WITH (FORMAT csv, DELIMITER ',');"

rm /tmp/items-$DBSIZE.csv 
rm /tmp/offices-$DBSIZE.csv
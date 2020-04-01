#!/bin/bash

HOST="localhost"
USER=postgres
PORT=5432
PASSWD=12345678
DB="vldb20_demo"

echo "initializing database";

PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -f create_db.sql;

PGPASSWORD=12345678 psql -U $USER -h $HOST -p $PORT -d $DB -f customer.sql;
#!/bin/bash
set -e
service postgresql start
# psql -U postgres -c "alter user postgres with password '12345678'"
# createuser -U postgres --superuser dejima
# psql -U postgres -c "alter user dejima with password '12345678'"
python /usr/local/lib/python2.7/dist-packages/pgadmin4/pgAdmin4.py
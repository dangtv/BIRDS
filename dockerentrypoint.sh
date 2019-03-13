#!/bin/bash
set -e
service postgresql start
sudo -u postgres psql -c "alter user postgres with password '12345678'"
cd /root/.pgadmin4
/root/.pgadmin4/bin/python /root/.pgadmin4/lib/python2.7/site-packages/pgadmin4/pgAdmin4.py
exec "$@";
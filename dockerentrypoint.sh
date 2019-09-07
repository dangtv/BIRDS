#!/bin/bash
set -e
service postgresql start
psql -U postgres -c "alter user postgres with password '12345678'"
createuser -U postgres --superuser dejima || true
psql -U postgres -c "alter user dejima with password '12345678'"
cd /usr/lib/birds/webui/server && node server.js --dir ../db --port 3010 --base-url ''
# birds
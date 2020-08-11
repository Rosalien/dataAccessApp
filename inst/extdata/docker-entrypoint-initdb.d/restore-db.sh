#!/bin/bash
set -e

echo "-----CREATE PGPASS-----"
echo "localhost:5432:sno:snouser:sno001" >> ~/.pgpass
export PGPASSFILE="~/.pgpass"
chmod 600 ~/.pgpass

echo "-----PG_RESTORE-----"
pg_restore --clean --dbname sno -h localhost -p 5432 -U snouser snotest.dump
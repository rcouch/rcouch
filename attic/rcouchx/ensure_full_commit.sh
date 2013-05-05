#!/bin/sh
# Runs POST /db/_ensure_full_commit
# on every CouchDB database found in $1 on server $2
# Usage:
#   ./ensure_full_commit.sh /path/to/couchdb/databases http://127.0.0.1:5984

DB_PATH=$1
URI=$2

usage()
{
  echo "Usage:"
  echo "  ./ensure_full_commit.sh /path/to/couchdb/databases http://127.0.0.1:5984"
}

# set defaults
if [ -z "$DB_PATH" ]; then
  usage
  exit 1
fi

if [ -z "$URI" ]; then
  usage
  exit 2
fi

CWD=`pwd`

cd "$DB_PATH"
# for each database
for db in `find . -name "*.couch"`; do 
  echo $db;
  sane_db=`echo "$db" | sed -e 's/^\.\/*//' \
                      | sed -e 's/\.couch//' \
                      | sed -e 's/\//%2f/'`
  # run _ensure_full_commit
  curl -X POST "$URI/$sane_db/_ensure_full_commit" \
   -H "Content-Type: application/json"
done

cd "$CWD"

# done

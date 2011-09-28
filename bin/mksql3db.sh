#!/bin/bash
# Automatically create sqlite3 db from GTFS files
# Creates DB of given name in current dir from all files
# rm the sqlite3/GRT.db, then cd to the data/GRT-?? dir, and run
# ../../bin/mksql3db.sh ../sqlite3/GRT.db *

MYNAME=$(basename $0)
SQ3=/usr/local/android-sdk-linux_x86/tools/sqlite3

function usage {
    echo "Usage: $MYNAME dbname file [file ...]"
    exit 1
}

function error {
    echo "$MYNAME: $@" 1>&2
    exit 1
}

test $# -lt 2 && usage

DB=$1
shift

tmpfile=$(mktemp)
trap "rm -f $tmpfile" 0

# Create the necessary metadata table
$SQ3 $DB <<-EOT
  CREATE TABLE "android_metadata" ("locale" TEXT DEFAULT 'en_US');
  INSERT INTO "android_metadata" VALUES ('en_US');
EOT

# Pipe everything to sqlite3
while [ $# -gt 0 ]
do
    file=$1
    table=$(echo `basename $1` | sed -e 's/\..*//')
    columns=$(head -1 $file)
    cat $file | sed -e1d > $tmpfile
    (
	echo "create table $table($columns);"
	echo ".separator ,"
	echo ".import $tmpfile $table"
    ) | $SQ3 $DB
    shift
done

echo "$0: warning: make sure you bump the db version in the code (DB_VERSION)."
version=$(grep 'int DB_VERSION = ' ../../Android/src/net/kw/shrdlu/grtgtfs/DatabaseHelper.java)
versionN=0
if [ -z "$version" ]
then
	echo "DB_VERSION not found: using 0"
else
	echo "$version"
	versionN=$(echo "$version" | \
		sed -e 's/.* int DB_VERSION = //' \
		    -e 's/\([0-9]*\);.*/\1/')
	if [ -n "$versionN" ]
	then
		echo "Using version $versionN"
	else
		echo "Failed to parse version: using 0"
	fi
fi

$SQ3 $DB <<-EOT
  create index stops_stop_id on stops ( stop_id );
  create index routes_route_id on routes ( route_id );
  create index trips_trip_id on trips ( trip_id );
  create index stop_times_stop_id on stop_times ( stop_id );
  create index trips_route_id on trips ( route_id );
  create index shapes_shape_id on shapes ( shape_id );
  create index calendar_service_id on calendar ( service_id );
  create index calendar_dates_service_id on calendar_dates ( service_id );
  PRAGMA user_version = $versionN;
  vacuum;
EOT

split -d -b 1m $DB $DB.

exit 0

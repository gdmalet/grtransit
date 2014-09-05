#!/bin/bash
# Automatically create sqlite3 db from GTFS files
# Creates DB of given name in current dir from all files.
# rm the sqlite3/GRT.db, then cd to the data/GRT-?? dir, and run
# ../../bin/mksql3db.sh ../sqlite3/GRT.db *

MYNAME=$(basename $0)
#SQ3=/usr/local/android-sdk-linux/tools/sqlite3
SQ3=/usr/bin/sqlite3

function usage {
    echo "Usage: $MYNAME dbname file [file ...]"
    exit 1
}

function warn {
    echo "$MYNAME: $@" 1>&2
}

function error {
    warn "$@"
    exit 1
}

function getdbversion {
    db=$1

    test -s "$db" || { warn "no existing db; using version 1"; return 1; }

    local version=$(echo 'PRAGMA user_version;' | sqlite3 "$db")
    test -z "$version" -o "$version" -lt 0 -o "$version" -gt 999 && {
	warn "strange version \"$version\"; using version 1"
	return 1
    }

    return $version
}

test $# -lt 2 && usage

DB=$1
shift

getdbversion $DB
version=$(expr $? + 1)
echo "using database version $version"

tmpfile=$(mktemp)
trap "rm -f $tmpfile $DB.new" 0
rm -f $DB.new

# Create the necessary metadata table
$SQ3 $DB.new <<-EOT
  CREATE TABLE "android_metadata" ("locale" TEXT DEFAULT 'en_US');
  INSERT INTO "android_metadata" VALUES ('en_US');
EOT

# Pipe everything to sqlite3
while [ $# -gt 0 ]
do
    file=$1
    echo $file
    table=$(echo `basename $1` | sed -e 's/\..*//')
    columns=$(head -1 $file)
    cat $file | sed -e1d > $tmpfile
    (
	echo "create table $table($columns);"
	echo ".separator ,"
	echo ".import $tmpfile $table"
    ) | $SQ3 $DB.new
    shift
done

$SQ3 $DB.new <<-EOT
  create index stops_stop_id on stops ( stop_id );
  create index routes_route_id on routes ( route_id );
  create index trips_trip_id on trips ( trip_id );
  create index stop_times_stop_id on stop_times ( stop_id );
  create index trips_route_id on trips ( route_id );
  create index shapes_shape_id on shapes ( shape_id );
  create index calendar_service_id on calendar ( service_id );
  create index calendar_dates_service_id on calendar_dates ( service_id );
  PRAGMA user_version = $version;
  vacuum;
EOT

mv $DB $DB.old
mv $DB.new $DB
gzip -9v -c $DB > $DB-$version.gz

md5=$(md5sum $DB | cut -f1 -d' ')
size=$(stat -c "%s" $DB-$version.gz)
sizem=$(echo "1k $size 512+ 1024/1024/p" | dc)
echo "$version $sizem $md5" > $DB-$version.version

ls -l $DB $DB-$version.gz $DB-$version.version
cat $DB-$version.version

exit 0

#!/bin/bash
# Check for new versions of the GRT database, and upload if so.

dir=~/src/GRT-GTFS/data/sqlite3
url="http://www.regionofwaterloo.ca/opendatadownloads/GRT_GTFS.zip"
file=GRT_GTFS.zip

cd $dir || { echo "can't cd to $dir" 1>&2; exit 42; }

trap "rm -f $file.new" 0

wget -q -O $file.new "$url"
test -s $file.new || { echo "download of $file failed" 1>&2; exit 42; }
cmp -s $file.new $file && exit 0

echo "New database $file"

mv $file $file.old
mv $file.new $file

name=GRT-transit-$(date "+%Y%m%d")
test -e ../$name && { echo "output $name already exists" 1>&2; exit 42; }

mkdir ../$name
cd ../$name
unzip $OLDPWD/$file
cd -
tar cvf ../$name.tar ../$name
bzip2 -9v ../$name.tar

../../bin/mksql3db.sh GRT.db ../$name/*

scp -p GRT.db.gz balekaor@baleka.org:www/gdmalet/android/grtransit/GRT.db.gz.dbg
scp -p GRT.db.version balekaor@baleka.org:www/gdmalet/android/grtransit/GRT.db.version.dbg

exit 0

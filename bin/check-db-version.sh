#!/bin/bash
# Check for new versions of the GRT database, and upload if so.

dir=~/src/grtransit/data/sqlite3
#dest=/var/www/html/gdmalet/android/grtransit/
dest=~/src/grtransit/www/
url="http://www.regionofwaterloo.ca/opendatadownloads/"
file=GRT_Daily_GTFS.zip

cd $dir || { echo "can't cd to $dir" 1>&2; exit 42; }

trap "rm -f $file.new .webcache.new" 0

# See if anything changed on the web page
lynx -dump "$url" | grep GRT | grep -v http:// | sed -e 's/ \[[1-9][0-9]\]GRT/ GRT/' > .webcache.new
test -s .webcache.new || { echo "download of $url failed" 1>&2; exit 42; }
cmp -s .webcache.new .webcache && exit 0

# Now we play silly buggers trying to find what file to download.
# If there's no simple match, look for something with the year in it.
# These are output in priority order.
newfile=$(diff .webcache .webcache.new | egrep '^> ' | \
    awk -v year=$(date +%Y) '{
	if ($NF == "GRT_Daily_GTFS.zip")
	    print "1", $NF;
	if ($NF == "GRT_Merged_GTFS.zip")
	    print "2", $NF;
	if ($NF == "GRT_GTFS.zip")
	    print "3", $NF;
	m="GRT_GTFS.*" year ".zip";
	if (match($NF, m))
	    {print "4", $NF}
}' | sort -n | head -1 | cut -d' ' -f2)

numfiles=0
test -n "$newfile" && numfiles=$(set $newfile; echo $#)
if test -z "$newfile" -o $numfiles -ne 1
then
    exec 1>&2	# send to stderr
    echo "Unable to determine what file to download..."
    echo -e "\nOld cache:"
    cat .webcache
    echo -e "\nNew cache:"
    cat .webcache.new
    mv .webcache.new .webcache.new_saved
    exit 42
fi

diff -c .webcache .webcache.new
mv .webcache.new .webcache

echo "Downloading $newfile to $file"
wget -q -O $file.new "$url$newfile"
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

# Remove quotation marks around text fields. This will make a horrible
# mess if there's a comma inside such a field, as that's the field delimiter,
# so make a weak attempt at catching that.
regex='"[^"]*,[^"]*"'
egrep -q "$regex" * && {
	echo "Failed stripping quotation marks; something matches \$regex" 1>&2
	egrep "$regex" * 1>&2
	exit 42
}
for file in *.txt
do
	# Strip leading & trailing double quotation marks
	sed -e 's/\(^\|,\)"/\1/' -e 's/"\(,\|$\)/\1/' $file > .foo
	cmp -s .foo $file || { mv $file $file.orig; mv .foo $file; }
done
rm -f .foo

# Sometimes there is no calendar table, so make an empty one if so.
test -f calendar.txt || echo "service_id,monday,tuesday,wednesday,thursday,friday,saturday,sunday,start_date,end_date" > calendar.txt

cd -
tar cvf ../$name.tar ../$name
bzip2 -9v ../$name.tar

../../bin/mksql3db.sh GRT.db ../$name/*.txt

# Don't need uncompressed files anymore
rm -r ../$name

new=$(ls -tr GRT.db-*.version | tail -1)
cp -p ${new} ${new/%version/gz} $dest

# Make links to the new files so they are available for download immediately.
cd $dest && {
    rm -f GRT.db.version GRT.db.gz 
    ln -s ${new} GRT.db.version
    ln -s ${new/%version/gz} GRT.db.gz
}

exit 0

#!/usr/bin/bash
# zenith_range.sh
# script to calculate zenith shift within a timerange
# vesak@iki.fi 1.1.2016 CC BY 2.0
# 29.2.2016 added param t for time

# usage zenith_range.sh latitude longitude date (YYYY-MM-DD) timespan (hh-hh) increment (mins) zone
# timespan NN-NN
# increment NN (mins)
# zone deviation from UTC (hours)

# set path to zenith program (part of LibRadtran-2.0 package)

ZENPATH="/cygdrive/c/Users/Titta/libRadtran/libRadtran-2.0/bin"
ZENPROG="$ZENPATH/zenith"

# set work dir

DIR="/home/Titta"

# set path for uvspec_template.inp file

INPTEMPLATE="$DIR/uvspec_template.inp"
INPFILENAME=$(basename "$INPTEMPLATE")

# set path and name to result file 

RESULTPATH="$DIR/results"


# read user input

if [ "$#" != "6" ]; then
	echo -e "Usage of the script:\n$0 latitude longitude date timespan increment zone"
	echo -e "Example:\n$0 41.000 10.000 2014-12-06 10-20 10 2"
	echo -e "Timespan in hours (0-24). Increment in minutes (0-30)"
	echo -e "Zone = hours from UTC (2 or 3 hits Finland)"
	exit 1
else
	echo "Starting the script"
fi

if [ ! -f "$INPTEMPLATE" ]; then
	echo "$INPTEMPLATE does not exists - create inp w/o zenith angle"
	exit 1
fi

# parse lat and longitude for file naming

lat=$1
long=$2

# parse date
longdate=$3
IFS=- read year month day <<< $3

# parse timespan
IFS=- read starthour endhour <<< $4

# parse increment
baseincrement=$5

# parse zone
zone=$6

# declare zenangle array and initiate anglecounter
declare -a zenangle
anglecounter=0

while [ $starthour -le $endhour ]; do
	increment=0
	while [ $increment -lt "60" ]; do
		read zenangle[anglecounter] <<< $($ZENPROG -a $1 -o $2 -y $year $day $month $starthour $increment -t$zone)
		let "anglecounter += 1"
		let "increment += $baseincrement"
		if [ $increment == "0" ]; then 
			#no increments per hour
			break
		fi
		if [ $starthour -eq "24" ]; then
			#end of day, no more increments
			break
		fi
	done
	let "starthour += 1"
done

DESTINATION=$RESULTPATH/$lat.$long.results.$longdate.dat

if [ -e "$DESTINATION" ]; then
	echo "$DESTINATION already exists - delete this if you don't need it"
	exit 1
else
	$(touch $DESTINATION)
fi

for singleangle in "${zenangle[@]}"; do
	read -r zentime zenith _ <<< $singleangle
	rtime=$(echo $zentime| tr : _)
	cp $INPTEMPLATE $RESULTPATH/.
	echo "sza $zenith" >> $RESULTPATH/$INPFILENAME
	$($ZENPATH/uvspec < $RESULTPATH/$INPFILENAME > $RESULTPATH/$zenith.tmp)
	$(awk '{print $1,A,B,$2,$3}' A=$longdate B=$rtime $RESULTPATH/$zenith.tmp > $RESULTPATH/tmp.tmp)
	cat $RESULTPATH/tmp.tmp >> $RESULTPATH/$lat.$long.results.$longdate.dat
	rm $RESULTPATH/$INPFILENAME
	rm $RESULTPATH/$zenith.tmp
	rm $RESULTPATH/tmp.tmp
done




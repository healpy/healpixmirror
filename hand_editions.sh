#! /bin/sh

#---------------
updateCopyrightDate () {

    dir=${here}
    instring='Copyright (C) 1997-2010'
    outstring='Copyright (C) 1997-2013'
    #tmpfile='/tmp/tmp_updatecopyright.txt'

    echo "In                $dir"
    echo "Changing          $instring -> $outstring"

    inlist=`find $dir -type f | grep -v \\.svn | grep -v java | grep -v cxx | grep -vi bk | grep -v $0 | xargs grep "$instring" | awk -F: '{print $1}'`

    for file in $inlist ; do
	ls -l $file
	grep "$instring" $file
	sed -i "s|$instring|$outstring|" ${file}
	grep "$outstring" $file
    done
}
#---------------
updateHEALPixWebSite () {

    dir=${here}
    instring='healpix.jpl.nasa.gov'
    outstring='healpix.sourceforge.net'
    #tmpfile='/tmp/tmp_updatecopyright.txt'

    echo "In                $dir"
    echo "Changing          $instring -> $outstring"

    inlist=`find $dir -type f | grep -v \\.svn | grep -v java | grep -v cxx | grep -vi bk |grep -v $0 |  xargs grep "$instring" | awk -F: '{print $1}'`

    for file in $inlist ; do
	ls -l $file
	grep "$instring" $file
	sed -i "s|$instring|$outstring|" ${file}
	grep "$outstring" $file
    done
}
#---------------

updateCopyrightDate
updateHEALPixWebSite

exit



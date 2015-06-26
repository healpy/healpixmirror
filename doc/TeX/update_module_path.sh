#! /bin/csh

#set verbose
set strins = ('src/f90/mod/alm'   \
'src/f90/mod/coord'   \
'src/f90/mod/extension'   \
'src/f90/mod/fitstools'   \
'src/f90/mod/head'   \
'src/f90/mod/misc'   \
'src/f90/mod/mpi'   \
'src/f90/mod/paramfile'   \
'src/f90/mod/pix'   \
'src/f90/mod/rngmod'   \
'src/f90/mod/statistics'   \
'src/f90/mod/udgrade'   )

set strouts = ('\modAlmTools' \
'\modCoordVConvert' \
'\modExtension' \
'\modFitstools' \
'\modHeadFits' \
'\modMiscUtils' \
'\modMpiAlmTools' \
'\modParamfileIo' \
'\modPixTools' \
'\modRngmod' \
'\modStatistics' \
'\modUdgradeNr' )

set i=0
foreach strout ( ${strouts} )
@ i++
    echo
    echo
    echo "$strins[$i] -> $strouts[$i]"
    #set list = (`grep '$strins[$i]' *tex | grep -v subrout | awk -F: '{print $1}'`)
    set str    = "$strins[$i]"
    set strout = "$strouts[$i]"
    # echo "$str"
    set list = (`grep "{$str" *tex| grep -v subrout | awk -F: '{print $1}'` )
    #echo $list
    foreach file ($list)
	set new = "/tmp/"$file
	echo $file $new
	#grep "$str" $file 
	cat $file | sed "s|{$str\(.*\)|{\$strout}|g" >! ${new}
	#cat $file | sed "s|{$str\(.*\)|{\$strout}|g" | grep '{\\mod'
	mv $file $file".bk"
	#sdiff -s $file $new
	mv $new $file
    end
end


exit

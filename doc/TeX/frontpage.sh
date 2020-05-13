#! /bin/sh

if [ "$#" = "0" ]; then
    list='intro install idl subroutines facilities csub top'
else
    list=$@
fi
template='fig/cover_template.svg'

for llprefix in ${list} ; do
    svgout="/tmp/${llprefix}.svg"
    pngout="fig/cover_${llprefix}.png"
    jpgout="fig/cover_${llprefix}.jpg"
    htmlfile="html/${llprefix}.htm"
    [ "${llprefix}" = "top" ] && htmlfile="html/intro.htm"
    echo ${htmlfile} ${svgout}  ${pngout} ${jpgout}
    cp -f ${template} ${svgout}

    version=`grep 'Version [0-9]\.[0-9][0-9],'  ${htmlfile} | grep -v address`
    words=`grep 'H1 ALIGN' ${htmlfile} | sed -e "s|<H1 ALIGN=CENTER>||" -e "s|<b>||" -e "s|</b>||" -e "s|</H1>||"`
    [ "${llprefix}" = "top" ] && words='HEALPix Documentation Anthology'
    nw=`echo $words | wc -w`
    #echo "VERSION = $version"

    sed -i.bak -e "s|Comment||"    -e "s|Release|${version}|" ${svgout}
    #sed -i.bak -e 's|pagecolor="#ffffff"|pagecolor="#ffaaaa"|' ${svgout}

    keys=(_k0_ _k1_ _k2_ _k3_ _k4_) # bashism ! https://opensource.com/article/18/5/you-dont-know-bash-intro-bash-arrays
    let "i = 5 - $nw"         # bashism ! https://ryanstutorials.net/bash-scripting-tutorial/bash-arithmetic.php
    for w in ${words} ; do
	#if [ "$w" != "User" ] ; then
	    #echo "${keys[$i]} -> ${w}"
	    sed -i.bak -e "s|${keys[$i]}|${w}|" ${svgout}
	    let i++
	#fi
    done
    sed -i.bak -e "s|${keys[0]}||" -e "s|${keys[1]}||" -e "s|${keys[3]}||" -e "s|${keys[4]}||"  ${svgout} # empty unused keys

    inkscape --export-filename=${pngout} ${svgout}
    gm convert ${pngout} ${jpgout}
    #open ${pngout}
    open ${jpgout}
done


exit


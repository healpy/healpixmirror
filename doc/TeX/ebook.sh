#! /bin/sh

if [ "$#" = "0" ]; then
    list='intro install idl subroutines facilities csub'
else
    list=$@
fi


cssfile='/tmp/ebook.css'
# H2: default (1.5) reduced
cat > ${cssfile} <<EOF
caption { font-size: 8pt; }
caption.bottom { font-size: 8pt; }
small { font-size: 8pt; }
h2 { font-size: 1.25em;  }
table { font-size: 9pt; }
EOF

#cd html
texdir='/Users/hivon/healpix_svn/doc/TeX/'
htmldir=${texdir}'html'
epubdir=${texdir}'epub/'
wrktop='/tmp/'
srfile=${texdir}'ebook_sr.txt'
#here=`pwd -L`
today=`date +%Y-%m-%d` # yyyy-mm-dd
#cover=${texdir}'fig/drawing_Healpix_0002.png'
epubversion="2"

mkdir -p ${epubdir}

for llprefix in ${list} ; do
    lprefix=`echo ${llprefix} | sed -e 's|intro|intro_|' -e 's|idl|idl_|' -e 's|subroutines|sub_|' -e 's|facilities|fac_|' -e 's|csub|csub_|'` 
    sprefix=`echo ${lprefix} | sed 's|_||g'`
    maxlevel=1
    dofootnotes=1
    extra_sr=""
    basefontsize=10
    #fontsizemapping="6,6,6,10,10,10,13,20"
    cover="${texdir}fig/cover_${llprefix}.jpg"
    [ "${sprefix}" = "install" ] && maxlevel=0
    [ "${sprefix}" = "csub" ]    && dofootnotes=0
    [ "${sprefix}" = "intro" ]   && dofootnotes=0
    #echo "${prefix} \t${maxlevel} \t${dofootnotes}"
    #echo $llprefix $lprefix $sprefix

    outfile=${epubdir}${llprefix}.epub
    workdir=${wrktop}${sprefix}
    echo "======================="
    echo "Processing ${llprefix} \t ${lprefix} \t ${sprefix} ..."
    mkdir -p ${workdir}
    \rm -f ${workdir}/*.png ${workdir}/*.htm ${workdir}/*.css ${workdir}/*.pl ${workdir}/*.epub 
    cd ${htmldir} #------
    cp -pf ${sprefix}*.png  ${sprefix}*.htm ${sprefix}*.css ${sprefix}*.pl        ${workdir}
    #cp -pf *ball.png                                                ${workdir}
    cp -pf crossref.png                                             ${workdir}
    cp -pf merge*png plot*png planck*png moll*png outline_earth.png ${workdir}
    cp -pf error_der*png new_dir_tree.png quad_tree.png intro*png   ${workdir}
    # cp ${texdir}/fig/healpix.png ${workdir}/healpix_cover.png
    cd ${workdir} #-----
    #nchapters=`ls -1 ${sprefix}*.htm | wc -l`
    nchapters=`grep "HREF=\"${sprefix}" ${llprefix}.htm | grep -vE 'TABLE|STYLESHEET|footnode' | wc -l`
    \rm -f ${lprefix}About_this_document.htm
    \rm -f ${lprefix}TABLE_CONTENTS.htm
    if [ "${sprefix}" = "install" ]; then
	sed "/<DL>/,/<\/DL>/!d" ${lprefix}footnode.htm  > ${lprefix}fninsert.txt # extract
	sed -i.bak "/<\/ADDRESS>/ r ${lprefix}fninsert.txt" ${sprefix}.htm # insert
	sed -i.bak2 "/<!--Table of Child-Links-->/,/<!--End of Table of Child-Links-->/d" ${sprefix}.htm # remove
	let "nchapters = ${nchapters} / 2 - 5"
    fi
    if [ "${dofootnotes}" = "1" ]; then
	cp -p ${lprefix}footnode.htm ${lprefix}About_this_document.htm
    fi
    let "nc1 = ${nchapters} + 0"
    let "nc2 = ${nchapters} + 0"
    #echo ${extra_sr}
    ebook-convert ${llprefix}.htm ${outfile} \
	--epub-version ${epubversion} \
	--breadth-first \
	--max-levels ${maxlevel} \
	--authors 'HEALPix Team' \
	--pubdate ${today} \
	--language 'English' \
	--comments 'Healpix/documentation' \
	--base-font-size ${basefontsize} \
	--page-breaks-before "//*[name()='h1']" \
	--max-toc-links ${nc2} \
	--toc-threshold ${nc1} \
	--search-replace ${srfile} \
	--extra-css=${cssfile} \
	--cover=${cover} \
	--no-default-epub-cover
    cd ${epubdir}
done
ls -lrt ${epubdir}*.epub
if [ "$#" = "1" ]; then
    #open ${outfile} &
    ebook-viewer ${outfile} &
fi

exit

#	--toc-filter '[A-Z]*=|/[A-Z]*' \

##--font-size-mapping ${fontsizemapping} \

# ebook-convert install.htm install.epub --breadth-first --max-levels 0 --authors 'HEALPix Team' --base-font-size 10 --search-replace /Users/hivon/healpix_svn/doc/TeX/ebook_sr.txt --page-breaks-before "//*[name()='h1']"

# ebook-convert intro.htm intro.epub --breadth-first --max-levels 1 --authors 'Me&You' --base-font-size 10 --search-replace /Users/hivon/healpix_svn/doc/TeX/ebook_sr.txt --page-breaks-before "//*[name()='h1']"



#    --cover 'healpix_cover.png' \
# --sr1-search '(?s)<DIV CLASS="navigation">\s*(.*?)</DIV>' --sr1-replace '' --sr2-search '(?s)<A ID="CHILD_LINKS"><STRONG>Subsections</STRONG></A>(.*?)<UL CLASS="ChildLinks">(.*?)</UL>' --sr2-replace ''
# --sr2-search '(?s)<UL CLASS="ChildLinks">\s*(.*?)</UL>' --sr2-replace '' --sr3-search '<A ID="CHILD_LINKS"><STRONG>Subsections</STRONG></A>' --sr3-replace '' 
# (?s)<H1><A ID=\s*About this document\s*.(.*?)The translation was initiated on \s*
# --page-breaks-before "<H1><"
#  --dont-split-on-page-breaks
# //*[name()='h1' or name()='h2']


# <SPAN CLASS="arabic">\d+</SPAN>
# 99

# (?s)<A ID="CHILD_LINKS"><STRONG>Subsections</STRONG></A>(.*?)<UL CLASS="ChildLinks">(.*?)</UL>
# 

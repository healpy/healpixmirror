#! /bin/sh

if [ "$#" = "0" ]; then
    list='intro install idl subroutines facilities csub'
else
    list=$@
fi

#cd html
texdir='/Users/hivon/healpix_svn/doc/TeX/'
htmldir=${texdir}'html'
epubdir=${texdir}'epub/'
wrktop='/tmp/'
srfile=${texdir}'ebook_sr.txt'
#here=`pwd -L`
today=`date +%Y-%m-%d` # yyyy-mm-dd

mkdir -p ${epubdir}

for llprefix in ${list} ; do
    lprefix=`echo ${llprefix} | sed -e 's|intro|intro_|' -e 's|idl|idl_|' -e 's|subroutines|sub_|' -e 's|facilities|fac_|' -e 's|csub|csub_|'` 
    sprefix=`echo ${lprefix} | sed 's|_||g'`
    maxlevel=1
    dofootnotes=1
    extra_sr=""
    basefontsize=10
    fontsizemapping="6,6,6,10,10,10,13,20"
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
    cp -pf *ball.png crossref.png                                   ${workdir}
    cp -pf merge*png plot*png planck*png moll*png outline_earth.png ${workdir}
    cp -pf error_der*png new_dir_tree.png quad_tree.png intro*png   ${workdir}
    # cp ${texdir}/fig/healpix.png ${workdir}/healpix_cover.png
    cd ${workdir} #-----
    \rm -f ${lprefix}About_this_document.htm
    \rm -f ${lprefix}TABLE_CONTENTS.htm
    if [ "${sprefix}" = "install" ]; then
	sed "/<DL>/,/<\/DL>/!d" ${lprefix}footnode.htm  > ${lprefix}fninsert.txt # extract
	sed -i.bak "/<\/ADDRESS>/ r ${lprefix}fninsert.txt" ${sprefix}.htm # insert
	sed -i.bak2 "/<!--Table of Child-Links-->/,/<!--End of Table of Child-Links-->/d" ${sprefix}.htm # remove
    fi
    if [ "${dofootnotes}" = "1" ]; then
	cp -p ${lprefix}footnode.htm ${lprefix}About_this_document.htm
    fi
    #echo ${extra_sr}
    ebook-convert ${llprefix}.htm ${outfile} \
	--breadth-first \
	--max-levels ${maxlevel} \
	--authors 'HEALPix Team' \
	--no-default-epub-cover \
	--pubdate ${today} \
	--language 'English' \
	--comments 'Healpix/IDL' \
	--base-font-size ${basefontsize} \
	--page-breaks-before "//*[name()='h1']" \
	--max-toc-links 0 \
	--search-replace ${srfile}
    cd ${epubdir}
done
ls -lrt ${epubdir}*.epub
if [ "$#" = "1" ]; then
    open ${outfile} &
fi

exit


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

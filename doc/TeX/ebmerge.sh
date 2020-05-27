#! /bin/sh

#####list='intro install idl subroutines facilities csub'
list='intro install facilities subroutines idl csub'
#list='install'

name='ebmerge'
finalname='HEALPixDocumentation'
# texdir='/Users/hivon/healpix_svn/doc/TeX/'
texdir=`/bin/pwd`/
htmldir=${texdir}'html'
epubdir=${texdir}'epub/'
wrktop='/tmp/'
wrkdir=${wrktop}${name}
f_manifest=${wrkdir}/manifest.txt
f_spine=${wrkdir}/spine.txt
f_toc1=${wrkdir}/toc.txt
f_toc=${wrkdir}/toc.ncx
f_content=${wrkdir}/content.opf
f_c1=${wrkdir}/ct.txt
f_cover=${wrkdir}/cover.xhtml
myuuid=`uuidgen  | tr '[A-Z]' '[a-z]'`
mydate=`date -u "+%Y-%m-%dT%T%z" | sed -e "s|0000|00:00|g"`
echo ${myuuid} ${mydate}

mkdir -p ${wrkdir}
\rm -rf ${wrkdir}/*
\rm -f ${f_toc} ${f_content}
\rm -f ${f_manifest} ${f_spine} ${f_toc1} ${f_c1}
cd ${wrkdir}
let "idbook = 0"

sdir='main'
cd ${wrkdir}
mkdir -p ${sdir}
cd ${sdir}
cp ${texdir}'main_epub.html' main.html
ebook-convert main.html main.epub --no-default-epub-cover --authors 'HEALPix Team' --language 'English'
rm main.html
unzip main.epub
rm main.epub

for llprefix in ${list} ; do
    lprefix=`echo ${llprefix} | sed -e 's|intro|intro_|' -e 's|idl|idl_|' -e 's|subroutines|sub_|' -e 's|facilities|fac_|' -e 's|csub|csub_|'` 
    sprefix=`echo ${lprefix} | sed 's|_||g'`
    sdir=${sprefix}
    cd ${wrkdir}
    mkdir -p ${sdir}
    cd ${sdir}
    pwd
    \cp -p ${epubdir}${llprefix}.epub .
    unzip -q ${llprefix}.epub
    \rm ${llprefix}.epub

    for f in `ls *.htm` ; do
	sed -i.bak -e "s|href=\"\./intro_|href=\"\.\./intro/intro_|g" \
		   -e "s|href=\"intro|href=\"\.\./intro/intro|g" \
		   -e "s|href=\"\./install|href=\"\.\./install/install|g" \
		   -e "s|href=\"install|href=\"\.\./install/install|g" \
		   -e "s|href=\"\./idl_|href=\"\.\./idl/idl_|g" \
		   -e "s|href=\"idl|href=\"\.\./idl/idl|g" \
		   -e "s|href=\"\./sub_|href=\"\.\./sub/sub_|g" \
		   -e "s|href=\"sub|href=\"\.\./sub/sub|g" \
		   -e "s|href=\"\./fac_|href=\"\.\./fac/fac_|g" \
		   -e "s|href=\"fac|href=\"\.\./fac/fac|g" \
		   -e "s|href=\"\./csub_|href=\"\.\./csub/csub_|g" \
		   -e "s|href=\"csub|href=\"\.\./csub/csub|g" \
		   -e "s|installfootnode\.htm|install\.htm|g" \
		   $f
##### -e "s|width=\"50\">|width=\"200\">|g" \
    done
    #grep 'href=\".*.htm' *.htm
    \rm *.htm.bak

    sed '/<manifest>/,/<\/manifest>/!d' content.opf | grep -v manifest |\
	sed -e "s|href=\"|href=\"${sprefix}/|g"  \
	    -e "s|id=\"|id=\"${sprefix}|g" >> ${f_manifest}

    echo "    <item href=\"${sprefix}/content.opf\" id=\"${sprefix}rootfile\" media-type=\"origrootfile/xml\"/>" >> ${f_c1}

    sed '/<spine/,/<\/spine>/!d'       content.opf | grep -v spine |\
	sed -e "s|idref=\"|idref=\"${sprefix}|g" >> ${f_spine}

    echo "   <navPoint id=\"book00${idbook}\" playOrder=\"0\">" >> ${f_toc1}
    sed '/<docTitle>/,/<\/navMap>/!d'    toc.ncx | grep -v "navMap" |\
	sed -e "s|src=\"|src=\"${sprefix}/|g" \
	    -e "s|id=\"|id=\"${sprefix}|g" \
	    -e "s|<docTitle>|<navLabel>|g" \
	    -e "s|</docTitle>|</navLabel>\|\|    <content src=\"${sprefix}/titlepage.xhtml\"/>|g" |\
	tr -s "||" "\n" >> ${f_toc1}
    echo "  </navPoint>" >> ${f_toc1}

    let "idbook ++"
done

cd ${wrkdir}
# cover.jpg
# cover.xhtml
# toc.ncx
cp -pr intro/META-INF .
cp -pr intro/mimetype .

# -----------------------------
echo "creating ${f_content}"
# -----------------------------
cat <<EOF > ${f_content}
<?xml version="1.0" encoding="UTF-8"?>
<package xmlns="http://www.idpf.org/2007/opf" unique-identifier="uuid_id" version="2.0">
 <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:title>HEALPix Documentation Anthology</dc:title>
    <dc:description>&lt;p&gt;HEALPix Documentation Anthology containing:&lt;/p&gt;Introduction to HEALPix&lt;br/&gt;Installing HEALPix&lt;br/&gt;HEALPix/IDL subroutines&lt;br/&gt;HEALPix/F90 subroutines&lt;br/&gt;HEALPix/F90 facilities&lt;br/&gt;HEALPix/C subroutines</dc:description>
    <dc:creator opf:role="aut" opf:file-as="HEALPix Team">HEALPix Team</dc:creator>
    <dc:date>${mydate}</dc:date>
    <dc:language>en</dc:language>
    <dc:identifier id="uuid_id" opf:scheme="uuid">${myuuid}</dc:identifier>
    <meta name="timestamp" content="${mydate}"/>
    <dc:creator opf:file-as="Unknown" opf:role="aut">HEALPix Team</dc:creator>
    <meta name="cover" content="coverimageid"/>
 </metadata>
EOF

echo " <manifest>"  >> ${f_content}
echo '    <item href="cover.xhtml" id="cover" media-type="application/xhtml+xml"/>' >> ${f_content}
echo '    <item href="cover.jpg" id="coverimageid" media-type="image/jpeg"/>'       >> ${f_content}
echo '    <item href="main/main.html" id="mainhtml" media-type="application/xhtml+xml"/>' >> ${f_content}
cat ${f_manifest}  >> ${f_content}
echo '    <item href="toc.ncx" id="ncx" media-type="application/x-dtbncx+xml"/>'    >> ${f_content}
cat ${f_c1}        >> ${f_content}
echo '    <item href="main/main.html" id="mainhtml2" media-type="application/xhtml+xml"/>' >> ${f_content}
echo " </manifest>" >> ${f_content}
#
echo ' <spine toc="ncx">'               >> ${f_content} # epub 2
echo '    <itemref idref="cover"/>'     >> ${f_content}
echo '    <itemref idref="mainhtml"/>'  >> ${f_content}
cat ${f_spine}                          >> ${f_content}
echo '    <itemref idref="mainhtml2"/>' >> ${f_content}
echo " </spine>"                        >> ${f_content}
#
echo " <guide>"     >> ${f_content}
echo '    <reference href="cover.xhtml" title="Cover" type="cover"/>' >> ${f_content}
echo " </guide>"   >> ${f_content}
#
echo "</package>"  >> ${f_content}
\rm -f ${f_manifest} ${f_spine} ${f_c1}


# -----------------------------
echo "creating ${f_toc}"
# -----------------------------
cat <<EOF > ${f_toc}
<?xml version="1.0" encoding="utf-8"?>
<ncx version="2005-1" xmlns="http://www.daisy.org/z3986/2005/ncx/">
   <head>
      <meta content="${myuuid}" name="dtb:uid"/>
      <meta content="2" name="dtb:depth"/>
      <meta content="0" name="dtb:totalPageCount"/>
      <meta content="0" name="dtb:maxPageNumber"/>
   </head>
   <docTitle>
      <text>HEALPix Documentation</text>
   </docTitle>
   <navMap>
EOF
#cat ${f_toc1}       >> ${f_toc} # use single running playOrder
cat ${f_toc1} | nl -b pplay -s ' ' -n ln |\
    sed  -e 's|\(.*\)<navPoint id="\(.*\)" playOrder=\".*\">|       <navPoint id="\2" playOrder="\1">|g' \
	 -e 's|    \">|\">|g' -e 's|   \">|\">|g' -e 's|  \">|\">|g' -e 's| \">|\">|g' >> ${f_toc}
echo "   </navMap>" >> ${f_toc}
echo "</ncx>"       >> ${f_toc}
\rm -f ${f_toc1}

# -----------------------------
echo "creating ${f_cover}"
# -----------------------------
cp ${texdir}fig/cover_top.jpg ./cover.jpg
cat <<EOF > ${f_cover}
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en"><head><title>Cover</title><style type="text/css" title="override_css">
@page {padding: 0pt; margin:0pt}
body { text-align: center; padding:0pt; margin: 0pt; }
div { margin: 0pt; padding: 0pt; }
</style></head><body><div>
<img src="cover.jpg" alt="cover"/>
</div></body></html>
EOF

# -----------------------------
echo "packaging"
# -----------------------------
\rm -rf ${name}.epub
#zip -q ${name}.epub * */*
zip -qrX "${name}.epub" mimetype $(ls|xargs echo|sed 's/mimetype//g') -x *.DS_Store
# https://ebooks.stackexchange.com/questions/257/how-to-repack-an-epub-file-from-command-line
\ls -lrt
cp -pr ${name}.epub ${epubdir}/${finalname}.epub
#/usr/local/bin/ebook-viewer ${name}.epub
#/usr/local/bin/ebook-viewer ${epubdir}/${finalname}.epub

exit


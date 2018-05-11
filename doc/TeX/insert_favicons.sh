#!/bin/sh
# insert in <head> path to favicons
#
# Changes:
#   14-Sep-2017: created by EH

# Get the directory path or filename
if [ -z "$1" ]; then
  cat <<EOF
Insert favicon(s) in html header
Usage: $0 <directory_with_problematic_HTML_files OR file_name_radix>
EOF
  exit
fi

trigger='<META NAME="Generator" CONTENT="LaTeX2HTML v2002-2-1">'
tmpfile='/tmp/html_favicons_edit.txt'
insert_file='/tmp/insert_tmp.html'

# Fix each HTML file by inserting insert_file after trigger
cat > ${insert_file} <<EOF
   <link rel='apple-touch-icon' sizes='180x180' href='images/favicons/apple-touch-icon.png?v=2017'>
   <link rel='icon' type='image/png' sizes='32x32' href='images/favicons/favicon-32x32.png?v=2017'>
   <link rel='icon' type='image/png' sizes='16x16' href='images/favicons/favicon-16x16.png?v=2017'>
   <link rel='manifest' href='images/favicons/manifest.json?v=2017'>
   <link rel='mask-icon' href='images/favicons/safari-pinned-tab.svg?v=2017' color='#5bbad5'>
   <link rel='shortcut icon' href='images/favicons/favicon.ico?v=2017'>
   <meta name='apple-mobile-web-app-title' content='HEALPix'>
   <meta name='application-name' content='HEALPix'>
   <meta name='msapplication-config' content='images/favicons/browserconfig.xml?v=2017'>
   <meta name='theme-color' content='#ffffff'>
EOF


for d in $* ; do
    echo "--------- $d ---------"
    for file in `ls -1 ${d}*.htm` ; do
	found=`grep -i favicons ${file} | wc -l`
	if [ ${found} == 0 ] ; then
	    # echo "editing HTML ${file}"
	    sed "/${trigger}/ r ${insert_file}" ${file} > ${tmpfile}
	    mv ${tmpfile} ${file}
	else
	    echo "       SKIP              ${file}"
	fi
    done
done


exit


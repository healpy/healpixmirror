#!/bin/sh
# improve the appearance of html page on small (mobile) screens
# by adding in the header
# <meta name="viewport" content="width=device-width, initial-scale=1">
#
# Changes:
#   24-Mar-2015: created by EH
# 
# is now redundant with 2018's latex2html (2018-11-21)

# Get the directory path or filename
if [ -z "$1" ]; then
  cat <<EOF
Fix the appearance of HTML files on small (mobile) screens
Usage: $0 <directory_with_problematic_HTML_files OR file_name_radix>
EOF
  exit
fi



# Fix each HTML file by putting magic line before trigger
magic='<meta name="viewport" content="width=device-width, initial-scale=1">'
trigger='<META NAME="Generator" CONTENT="LaTeX2HTML'
tmpfile='/tmp/html_edit.txt'
for d in $* ; do
echo "--------- $d ---------"
for file in `ls -1 ${d}*.htm` ; do
found=`grep -i viewport ${file} | wc -l`
if [ ${found} == 0 ] ; then
    echo "editing  ($0)  HTML ${file}"
    cat ${file} | sed "s/${trigger}/${magic}\\
&/g" > ${tmpfile}
    mv ${tmpfile} ${file}
# else
    #echo "       SKIP  ($0)            ${file}"
fi
done
done


exit


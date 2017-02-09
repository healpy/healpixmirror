#!/bin/sh
# improve the appearance of html page on small (mobile) screens
# by adding in the header
# <meta name="viewport" content="width=device-width, initial-scale=1">
#
# Changes:
#   24-Mar-2015: created by EH

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
    # echo "editing HTML ${file}"
    cat ${file} | sed "s/${trigger}/${magic}\n&/g" > ${tmpfile}
    mv ${tmpfile} ${file}
else
    echo "       SKIP              ${file}"
fi
done
done


exit


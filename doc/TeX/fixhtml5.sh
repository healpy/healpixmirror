#!/bin/sh
# be more html5 compliant
#
# Changes:
#   23-June-2020: EH

# Get the directory path or filename
if [ -z "$1" ]; then
  cat <<EOF
Try to be more HTML5 compliant
Usage: $0 <directory_with_problematic_HTML_files OR file_name_radix>
EOF
  exit
fi



# Fix each HTML file
tmpfile='/tmp/html5_edit.txt'
for d in $* ; do
    echo "--------- $d ---------"
    for file in `ls -1 ${d}*.htm` ; do
	found=`grep WIDTH= ${file} | wc -l`
	if [ ${found} != 0 ] ; then
            # echo "editing HTML ${file}"
	    cat ${file} | sed -e 's|WIDTH=\([0-9][0-9]*\)|style="width:\1px"|g' \
		-e 's|style=" WIDTH:"100%";"|style="width:100%"|g' > ${tmpfile}
	    mv ${tmpfile} ${file}
#	else
#	    echo "       SKIP    ($0)          ${file}"
	fi
    done
done


exit

